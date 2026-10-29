# =====================================================================
# 03_ortho_spike.R : WP3 orthopaedic-layer feasibility spike (gate G1)
# =====================================================================
# Stages (run in order; every stage is resumable and writes only to
# v1/data_clean/ortho, v1/logs/wp3_*, and CACHE/spike):
#   idx     HEAD + GET each DISTINCT issuer index URL (92), classify failures,
#           extract plan/provider/formulary file URLs
#   plans   size plan files (HEAD), fetch the plan files that are small enough,
#           map plan_id -> issuer id, check against the PUF
#   size    HEAD every provider file (gzip and identity), 1-byte range probe
#           when needed -> provider_file_inventory.csv
#   sample  download the first ~20 MB of 5 provider files, inspect the schema
#   nppes   re-test NPPES (listing page, monthly zips, weekly, one API query)
#   eval    issuer_sources.csv, size model, G1 criteria -> logs/wp3_g1_numbers.csv
# Run: Rscript "<root>/v1/R/03_ortho_spike.R" <stage>
#
# Politeness: at most 8 concurrent connections in total and at most one at a
# time per host (work is grouped by host), a plain User-Agent, no e-mail
# address anywhere, no retries with other client identities. A 403 is
# recorded as a 403, never worked around.
# Keep this file ASCII.
# =====================================================================
.args <- commandArgs(trailingOnly = FALSE)
.f <- sub("^--file=", "", grep("^--file=", .args, value = TRUE)[1])
.here <- dirname(normalizePath(gsub("~+~", " ", .f, fixed = TRUE)))
source(file.path(.here, "00_setup.R"))
ensure_pkgs(c("jsonlite", "data.table", "parallel", "yaml"))
suppressPackageStartupMessages(library(data.table))

CFG <- load_config()
G1 <- CFG$gates$g1
FLOOR_GB <- CFG$gates$free_disk_floor_gb
UA <- "Mozilla/5.0 (compatible; public-data-feasibility-check)"
MAXPAR <- 8L
SPIKE <- file.path(P$cache, "spike")
D_IDX <- file.path(SPIKE, "index")
D_PLN <- file.path(SPIKE, "plans")
D_SMP <- file.path(SPIKE, "samples")
D_ST  <- file.path(SPIKE, "state")
D_NP  <- file.path(SPIKE, "nppes")
for (d in c(SPIKE, D_IDX, D_PLN, D_SMP, D_ST, D_NP)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
dir.create(P$clean_ortho, recursive = TRUE, showWarnings = FALSE)
OUT <- P$clean_ortho
LOGF <- function(stage) file.path(P$logs, paste0("wp3_", stage, ".log"))
`%||%` <- function(a, b) if (is.null(a)) b else a
now_utc <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# ---------------------------------------------------------------------
# shared helpers
# ---------------------------------------------------------------------
free_gb <- function() {
  o <- system2("df", c("-k", shQuote(path.expand("~"))), stdout = TRUE)
  as.numeric(strsplit(trimws(o[length(o)]), "\\s+")[[1]][4]) / 1024^2
}
disk_guard <- function(need_gb = 0, what = "download") {
  f <- free_gb()
  if (f - need_gb < FLOOR_GB) {
    stop(sprintf("DISK FLOOR: free %.2f GB, need %.2f GB for %s, floor %.1f GB. Stopping.",
                 f, need_gb, what, FLOOR_GB), call. = FALSE)
  }
  f
}

read_hdr <- function(hf) {
  if (!file.exists(hf)) return(list(status = NA_integer_, h = list(), n_hops = 0L))
  L <- readLines(hf, warn = FALSE)
  L <- sub("\r$", "", L)
  starts <- grep("^HTTP/", L)
  if (!length(starts)) return(list(status = NA_integer_, h = list(), n_hops = 0L))
  last <- L[starts[length(starts)]:length(L)]
  st <- suppressWarnings(as.integer(strsplit(last[1], " ")[[1]][2]))
  kv <- last[-1]
  kv <- kv[grepl(":", kv, fixed = TRUE)]
  nm <- tolower(trimws(sub(":.*$", "", kv)))
  val <- trimws(sub("^[^:]*:", "", kv))
  h <- as.list(val); names(h) <- nm
  list(status = st, h = h, n_hops = length(starts))
}

# One curl call. mode "head": -I. mode "get": body to out_file.
# Returns a one-row list. extra = additional curl args (character vector).
do_curl <- function(url, mode = c("head", "get"), out_file = NULL, extra = character(),
                    max_time = 20, accept_enc = "gzip", write_out_only = FALSE) {
  mode <- match.arg(mode)
  hf <- tempfile("hdr_")
  on.exit(unlink(hf), add = TRUE)
  wfmt <- paste0("CURLINFO_MARK:%{http_code}\t%{url_effective}\t%{content_type}\t%{num_redirects}\t",
                 "%{size_download}\t%{time_total}\t%{exitcode}\t%{errormsg}\t%{speed_download}")
  a <- c("-sS", "-L", "--max-time", as.character(max_time), "--connect-timeout", "15",
         "-A", shQuote(UA), "-D", shQuote(hf), "-w", shQuote(wfmt))
  if (mode == "get" && identical(accept_enc, "gzip")) {
    a <- c(a, "--compressed")
  } else if (!is.null(accept_enc)) {
    a <- c(a, "-H", shQuote(paste0("Accept-Encoding: ", accept_enc)))
  }
  if (mode == "head") a <- c(a, "-I", "-o", "/dev/null") else a <- c(a, "-o", shQuote(out_file))
  a <- c(a, extra, shQuote(url))
  out <- suppressWarnings(system2("curl", a, stdout = TRUE, stderr = TRUE))
  w <- out[grepl("CURLINFO_MARK:", out, fixed = TRUE)]
  info <- if (length(w)) strsplit(sub(".*CURLINFO_MARK:", "", w[length(w)]), "\t", fixed = TRUE)[[1]] else character(0)
  info <- c(info, rep("", 9 - length(info)))
  hd <- read_hdr(hf)
  list(http = suppressWarnings(as.integer(info[1])), final_url = info[2], ctype = info[3],
       redirects = suppressWarnings(as.integer(info[4])), size_dl = suppressWarnings(as.numeric(info[5])),
       time_s = suppressWarnings(as.numeric(info[6])), exit = suppressWarnings(as.integer(info[7])),
       err = info[8], speed = suppressWarnings(as.numeric(info[9])), hdr = hd$h, hdr_status = hd$status,
       stderr = paste(out[!grepl("CURLINFO_MARK:", out, fixed = TRUE)], collapse = " "))
}

fail_reason <- function(r, body_kind = NA_character_) {
  e <- r$exit
  if (!is.na(e) && e != 0L) {
    return(switch(as.character(e),
      "6" = "DNS", "7" = "connection refused", "28" = "timeout",
      "35" = "TLS", "51" = "TLS", "58" = "TLS", "59" = "TLS", "60" = "TLS", "77" = "TLS", "83" = "TLS",
      "56" = "connection reset", "52" = "empty reply", "55" = "send error", "47" = "too many redirects",
      "63" = "file too large", "18" = "partial transfer", "23" = "write error",
      paste0("curl exit ", e)))
  }
  h <- r$http
  if (is.na(h) || h == 0L) return("no HTTP response")
  if (h >= 400L) return(paste0("HTTP ", h))
  if (!is.na(body_kind) && body_kind != "json") return(body_kind)
  NA_character_
}

# classify a downloaded body: "json" | "HTML landing page" | "not JSON" | "empty"
body_kind <- function(path) {
  if (!file.exists(path) || file.size(path) == 0) return("empty")
  hd <- readBin(path, "raw", n = 4096)
  txt <- tryCatch(rawToChar(hd[hd != as.raw(0)]), error = function(e) "")
  t <- sub("^[\\s\\xEF\\xBB\\xBF]+", "", txt, perl = TRUE, useBytes = TRUE)
  if (grepl("^\\s*<", t, perl = TRUE, useBytes = TRUE)) return("HTML landing page")
  if (grepl("^\\s*[\\[{]", t, perl = TRUE, useBytes = TRUE)) return("json")
  "not JSON"
}

host_of <- function(u) sub("^https?://([^/:]+).*$", "\\1", u)

# run one function per host group: hosts in parallel (<= MAXPAR), URLs of one host in sequence
run_by_host <- function(urls, fun, pause = 0.25) {
  hosts <- host_of(urls)
  idx <- split(seq_along(urls), hosts)
  # largest groups first so the tail is short
  idx <- idx[order(-lengths(idx))]
  res <- parallel::mclapply(idx, function(ii) {
    lapply(ii, function(i) { r <- tryCatch(fun(urls[i], i), error = function(e) list(.error = conditionMessage(e), i = i)); Sys.sleep(pause); r })
  }, mc.cores = min(MAXPAR, length(idx)), mc.preschedule = FALSE)
  out <- vector("list", length(urls))
  for (g in seq_along(idx)) for (k in seq_along(idx[[g]])) out[[idx[[g]][k]]] <- res[[g]][[k]]
  out
}

read_mr <- function() {
  mr <- fread(file.path(P$clean_cms, "mr_urls.csv"), colClasses = "character")
  mr[in_individual_qhp_tab == "TRUE"]
}
read_vol <- function() fread(file.path(P$clean_cms, "issuer_volume.csv"), colClasses = c(issuer_id = "character", state = "character", issuer_name = "character"))

# index_url -> stable id
index_table <- function() {
  mr <- read_mr()
  u <- mr[, .(n_issuers = .N, issuer_ids = paste(sort(issuer_id), collapse = ";")), by = mr_index_url]
  u <- u[order(-n_issuers, mr_index_url)]
  u[, idx_id := sprintf("I%03d", seq_len(.N))]
  u
}

# ---------------------------------------------------------------------
# stage idx
# ---------------------------------------------------------------------
stage_idx <- function() {
  lf <- LOGF("idx")
  lg <- function(...) log_msg(..., file = lf)
  it <- index_table()
  lg("distinct index URLs among Individual QHP issuers: ", nrow(it), "; issuers: ", sum(it$n_issuers))
  disk_guard(0.1, "index files")
  one <- function(url, i) {
    id <- it$idx_id[i]
    body <- file.path(D_IDX, paste0(id, ".body"))
    hd <- do_curl(url, "head", max_time = 20)
    Sys.sleep(0.2)
    g <- do_curl(url, "get", out_file = body, max_time = 60, extra = c("--max-filesize", "30000000"))
    bk <- if (!is.na(g$exit) && g$exit == 0L && !is.na(g$http) && g$http < 400L) body_kind(body) else NA_character_
    pj <- NULL
    nprov <- nplan <- nform <- NA_integer_
    if (identical(bk, "json")) {
      pj <- tryCatch(jsonlite::fromJSON(body, simplifyVector = FALSE), error = function(e) NULL)
      if (is.null(pj)) bk <- "not JSON"
    }
    if (identical(bk, "json")) {
      f <- file.path(D_IDX, paste0(id, ".json"))
      file.rename(body, f)
      pu <- unlist(pj$provider_urls); lu <- unlist(pj$plan_urls); fu <- unlist(pj$formulary_urls)
      nprov <- length(pu); nplan <- length(lu); nform <- length(fu)
      writeLines(c(if (length(pu)) paste0("provider\t", pu), if (length(lu)) paste0("plan\t", lu)),
                 file.path(D_IDX, paste0(id, ".urls.tsv")))
    }
    fr <- fail_reason(g, bk)
    if (is.na(fr) && identical(bk, "json") && (nprov == 0L || nplan == 0L)) fr <- "JSON without provider_urls/plan_urls"
    list(idx_id = id, index_url = url, host = host_of(url),
         head_http = hd$http, head_exit = hd$exit, head_final_url = hd$final_url, head_ctype = hd$ctype,
         head_redirects = hd$redirects, head_content_length = hd$hdr[["content-length"]] %||% NA_character_,
         get_http = g$http, get_exit = g$exit, get_err = g$err, get_final_url = g$final_url, get_ctype = g$ctype,
         get_redirects = g$redirects, get_bytes = g$size_dl, body_kind = bk %||% NA_character_,
         n_provider_urls = nprov, n_plan_urls = nplan, n_formulary_urls = nform,
         failure_reason = fr, retrieved_utc = now_utc())
  }
  res <- run_by_host(it$mr_index_url, one)
  bad <- vapply(res, function(r) !is.null(r$.error), logical(1))
  if (any(bad)) lg("R-level errors: ", sum(bad), " first: ", res[[which(bad)[1]]]$.error)
  dt <- rbindlist(lapply(res[!bad], function(r) as.data.table(lapply(r, function(x) if (length(x) == 0) NA else x))), fill = TRUE)
  dt <- merge(it[, .(idx_id, n_issuers, issuer_ids)], dt, by = "idx_id")
  fwrite(dt, file.path(P$logs, "wp3_index_probe.csv"))
  lg("index probe done: ok=", sum(is.na(dt$failure_reason)), " failed=", sum(!is.na(dt$failure_reason)))
  print(dt[, .N, by = .(reason = ifelse(is.na(failure_reason), "OK", failure_reason))][order(-N)])
  invisible(dt)
}

# ---------------------------------------------------------------------
# sizing helpers
# ---------------------------------------------------------------------
# Range probe that can never download more than a few KB: the body goes to a pipe that is closed
# after 1 byte, so a server that ignores Range and streams the whole file is cut off at once.
# (An earlier version of this function wrote the body to /dev/null with --max-time 25 and
# downloaded up to 25 s of data from servers that ignore Range; that was replaced.)
range_probe <- function(url, max_time = 20) {
  hf <- tempfile("hdr_")
  cmd <- paste("curl -sS -L --max-time", max_time, "--connect-timeout 15 -A", shQuote(UA), "-H 'Accept-Encoding: identity' -r 0-0 -D", shQuote(hf),
               shQuote(url), "2>/dev/null | head -c 1 > /dev/null")
  suppressWarnings(system(cmd))
  hd <- read_hdr(hf); unlink(hf)
  list(http = hd$status, hdr = hd$h)
}

# Size one URL without downloading it.
#   1. HEAD asking for identity -> Content-Length = UNCOMPRESSED stream size
#   2. if that is missing, zero or the HEAD failed: 1-byte Range probe with identity
#      (Content-Range total); if the server ignores Range but sends 200 with a
#      Content-Length, that length is used
#   3. HEAD asking for gzip -> does the server compress, and is there a real
#      (non-chunked) compressed Content-Length? Only a length that is smaller than the
#      uncompressed size and at least 1 KB is kept as bytes_gzip_head; servers that
#      answer a gzip HEAD with a stub length (for example 20 bytes) are flagged.
# One 429/503 answer is retried once after Retry-After (at most 30 s).
size_one <- function(url) {
  retry <- function(fn) {
    r <- fn()
    if (!is.na(r$http) && r$http %in% c(429L, 503L)) {
      ra <- suppressWarnings(as.numeric(r$hdr[["retry-after"]]))
      Sys.sleep(min(30, if (is.na(ra)) 5 else ra))
      r <- fn()
    }
    r
  }
  num <- function(x) if (is.null(x) || !nzchar(x)) NA_real_ else suppressWarnings(as.numeric(x))
  is2 <- function(r) !is.na(r$http) && r$http >= 200L && r$http < 300L && !is.na(r$exit) && r$exit == 0L
  b_id <- NA_real_; src <- NA_character_; ar <- ""; last <- NULL
  h1 <- retry(function() do_curl(url, "head", max_time = 25, accept_enc = "identity"))
  last <- h1
  ar <- h1$hdr[["accept-ranges"]] %||% ""
  cl1 <- num(h1$hdr[["content-length"]])
  enc_id <- h1$hdr[["content-encoding"]] %||% ""
  if (is2(h1) && !is.na(cl1) && cl1 > 0) { b_id <- cl1; src <- "head_identity_content_length" }
  if (is.na(b_id)) {
    Sys.sleep(0.15)
    g <- range_probe(url)
    last <- list(http = g$http, exit = 0L, final_url = url, ctype = g$hdr[["content-type"]] %||% "", hdr = g$hdr)
    cr <- g$hdr[["content-range"]] %||% ""
    tot <- suppressWarnings(as.numeric(sub("^.*/", "", cr)))
    if (!is.na(g$http) && g$http == 206L && !is.na(tot)) {
      b_id <- tot; src <- "range_probe_content_range"; ar <- "bytes"
    } else if (!is.na(g$http) && g$http == 200L) {
      cl <- num(g$hdr[["content-length"]])
      if (!is.na(cl) && cl > 1) { b_id <- cl; src <- "get_content_length_range_ignored" }
    }
  }
  # gzip behaviour
  Sys.sleep(0.15)
  h2 <- retry(function() do_curl(url, "head", max_time = 25, accept_enc = "gzip"))
  enc <- h2$hdr[["content-encoding"]] %||% ""
  cl2 <- num(h2$hdr[["content-length"]])
  gz_flag <- "none"
  b_gz <- NA_real_
  if (grepl("gzip|br|deflate", enc, ignore.case = TRUE)) {
    if (is.na(cl2)) gz_flag <- "gzip_chunked_no_length"
    else if (cl2 < 1024 || (!is.na(b_id) && cl2 >= b_id)) gz_flag <- "gzip_stub_or_equal_length"
    else { gz_flag <- "gzip_with_length"; b_gz <- cl2 }
  }
  fr <- if (!is.na(b_id)) NA_character_ else fail_reason(last)
  if (is.na(b_id) && is.na(fr)) fr <- "no size obtainable (HTTP 200, no Content-Length, Range ignored)"
  list(url = url, http = last$http, http_head_identity = h1$http, http_head_gzip = h2$http, exit = last$exit,
       final_url = last$final_url, ctype = last$ctype,
       content_encoding = if (nzchar(enc)) enc else enc_id, gzip_behaviour = gz_flag, accept_ranges = ar,
       last_modified = h1$hdr[["last-modified"]] %||% NA_character_,
       bytes_identity = b_id, bytes_source = src, bytes_gzip_head = b_gz,
       failure_reason = fr, retrieved_utc = now_utc())
}

size_many <- function(urls, tag) {
  lf <- LOGF(tag)
  lg <- function(...) log_msg(..., file = lf)
  urls <- unique(urls)
  lg(tag, ": sizing ", length(urls), " distinct URLs on ", length(unique(host_of(urls))), " hosts")
  res <- run_by_host(urls, function(u, i) size_one(u), pause = 0.2)
  bad <- vapply(res, function(r) !is.null(r$.error), logical(1))
  if (any(bad)) lg("R-level errors: ", sum(bad), " first: ", res[[which(bad)[1]]]$.error)
  dt <- rbindlist(lapply(res[!bad], function(r) as.data.table(lapply(r, function(x) if (length(x) == 0) NA else x))), fill = TRUE)
  dt
}

stage_plans <- function() {
  lg <- function(...) log_msg(..., file = LOGF("plans"))
  lg("HEAD-only stage (no body downloaded); free disk GB now: ", round(free_gb(), 2))
  probe <- fread(file.path(P$logs, "wp3_index_probe.csv"), colClasses = c(idx_id = "character", issuer_ids = "character"))
  ok <- probe[is.na(failure_reason) | failure_reason == ""]
  pl <- rbindlist(lapply(ok$idx_id, function(id) {
    x <- fread(file.path(D_IDX, paste0(id, ".urls.tsv")), header = FALSE, sep = "\t", col.names = c("kind", "url"))
    x[kind == "plan"][, idx_id := id]
  }))
  lg("plan URL rows ", nrow(pl), " distinct ", uniqueN(pl$url))
  sz <- size_many(pl$url, "plans")
  fwrite(sz, file.path(P$logs, "wp3_plan_file_sizes.csv"))
  print(sz[, .(n = .N, with_size = sum(!is.na(bytes_identity)), MB_identity = round(sum(bytes_identity, na.rm = TRUE) / 1e6, 1),
               MB_max = round(max(bytes_identity, na.rm = TRUE) / 1e6, 1))])
  print(sz[, .N, by = .(reason = failure_reason)][order(-N)])
  invisible(sz)
}

stage_size <- function() {
  lg <- function(...) log_msg(..., file = LOGF("size"))
  lg("HEAD-only stage (no body downloaded; range probes cut at 1 byte); free disk GB now: ", round(free_gb(), 2))
  probe <- fread(file.path(P$logs, "wp3_index_probe.csv"), colClasses = c(idx_id = "character", issuer_ids = "character"))
  ok <- probe[is.na(failure_reason) | failure_reason == ""]
  pv <- rbindlist(lapply(ok$idx_id, function(id) {
    x <- fread(file.path(D_IDX, paste0(id, ".urls.tsv")), header = FALSE, sep = "\t", col.names = c("kind", "url"))
    x[kind == "provider"][, idx_id := id]
  }))
  lg("provider URL rows ", nrow(pv), " distinct ", uniqueN(pv$url))
  sz <- size_many(pv$url, "size")
  fwrite(sz, file.path(P$logs, "wp3_provider_file_sizes.csv"))
  print(sz[, .(n = .N, with_size = sum(!is.na(bytes_identity)), GB_identity = round(sum(bytes_identity, na.rm = TRUE) / 1e9, 2))])
  print(sz[, .N, by = .(reason = failure_reason)][order(-N)])
  print(sz[, .N, by = .(gz = gzip_behaviour, src = bytes_source)][order(-N)])
  invisible(sz)
}

# ---------------------------------------------------------------------
# stage plans2: fetch the small plan files and map plan_id -> issuer
# ---------------------------------------------------------------------
stage_plans2 <- function() {
  lg <- function(...) log_msg(..., file = LOGF("plans2"))
  psz <- fread(file.path(P$logs, "wp3_plan_file_sizes.csv"), integer64 = "double")
  disk_guard(0.2, "plan files")
  cap <- 30e6
  todo <- psz[is.na(bytes_identity) | bytes_identity <= cap]
  lg("fetching ", nrow(todo), " plan files (<= 30 MB or size unknown, capped at 30 MB each) of ", nrow(psz))
  one <- function(url, i) {
    f <- file.path(D_PLN, sprintf("P%04d.json", match(url, psz$url)))
    r <- do_curl(url, "get", out_file = f, max_time = 120, extra = c("--max-filesize", format(cap, scientific = FALSE)))
    list(url = url, file = f, http = r$http, exit = r$exit, bytes = if (file.exists(f)) file.size(f) else NA_real_,
         body_kind = if (!is.na(r$exit) && r$exit == 0L && !is.na(r$http) && r$http < 400L) body_kind(f) else NA_character_,
         failure_reason = fail_reason(r, if (!is.na(r$exit) && r$exit == 0L && !is.na(r$http) && r$http < 400L) body_kind(f) else NA_character_))
  }
  res <- run_by_host(todo$url, one, pause = 0.2)
  dt <- rbindlist(lapply(res, function(r) as.data.table(lapply(r, function(x) if (length(x) == 0) NA else x))), fill = TRUE)
  # extract plan_id (+ plan_id_type, years) from each retrievable plan file with jq
  rows <- list()
  for (k in seq_len(nrow(dt))) {
    if (!is.na(dt$failure_reason[k]) || !identical(dt$body_kind[k], "json")) next
    o <- suppressWarnings(system2("jq", c("-r", shQuote("if type==\"array\" then .[] else (.plans // .)[]? end | [(.plan_id|tostring), (.plan_id_type // \"\"), ((.years // [])|map(tostring)|join(\"|\"))] | @tsv"), shQuote(dt$file[k])), stdout = TRUE, stderr = TRUE))
    o <- o[grepl("\t", o, fixed = TRUE)]
    if (length(o)) {
      x <- fread(text = o, header = FALSE, sep = "\t", col.names = c("plan_id", "plan_id_type", "years"), colClasses = "character")
      x[, plan_url := dt$url[k]]
      rows[[length(rows) + 1L]] <- x
    }
  }
  pm <- rbindlist(rows, fill = TRUE)
  fwrite(dt, file.path(P$logs, "wp3_plan_file_fetch.csv"))
  fwrite(pm, file.path(P$logs, "wp3_plan_ids_from_plan_files.csv"))
  lg("plan files fetched ok=", sum(is.na(dt$failure_reason)), " failed=", sum(!is.na(dt$failure_reason)), "; plan rows=", nrow(pm), " distinct plan_id=", uniqueN(pm$plan_id))
  print(dt[, .N, by = .(reason = failure_reason)][order(-N)])
  invisible(pm)
}

# ---------------------------------------------------------------------
# stage sample
# ---------------------------------------------------------------------
SAMPLE_BYTES <- 20971520  # 20 MiB

pick_files <- function() {
  set.seed(CFG$study$seed)
  sz <- fread(file.path(P$logs, "wp3_provider_file_sizes.csv"), integer64 = "double")
  probe <- fread(file.path(P$logs, "wp3_index_probe.csv"), colClasses = c(idx_id = "character", issuer_ids = "character"))
  ok <- probe[is.na(failure_reason) | failure_reason == ""]
  pv <- rbindlist(lapply(ok$idx_id, function(id) {
    x <- fread(file.path(D_IDX, paste0(id, ".urls.tsv")), header = FALSE, sep = "\t", col.names = c("kind", "url"))
    x[kind == "provider"][, idx_id := id]
  }))
  pv <- merge(pv, ok[, .(idx_id, issuer_ids)], by = "idx_id")
  pv <- merge(pv, sz[, .(url, bytes_identity, http, failure_reason)], by = "url", all.x = TRUE)
  # retrievable = HTTP 2xx on the sizing call; the size may be unknown (servers that ignore Range and send no Content-Length)
  pool <- pv[!is.na(http) & http >= 200 & http < 300]
  pool <- unique(pool, by = "url")
  # the micro-probe tells which files hold INDIVIDUAL clinician records in their first bytes; the two issuer-anchored
  # picks are restricted to such files (a facility-only file cannot say anything about the specialty field)
  fpq <- fread(file.path(P$logs, "wp3_provider_file_probe.csv"), select = c("provider_url", "n_individual"))
  pool <- merge(pool, fpq, by.x = "url", by.y = "provider_url", all.x = TRUE)
  pool[, has_ind := !is.na(n_individual) & n_individual > 0]
  vol <- read_vol()
  mr <- read_mr()
  it <- index_table()
  # issuer -> idx_id of retrievable index
  imap <- rbindlist(lapply(seq_len(nrow(ok)), function(i) data.table(issuer_id = strsplit(ok$issuer_ids[i], ";", fixed = TRUE)[[1]], idx_id = ok$idx_id[i])))
  rep <- merge(vol[has_reported_in_network_count == TRUE, .(issuer_id, in_network_claims_received)], imap, by = "issuer_id")
  rep <- rep[order(-in_network_claims_received)]
  big_issuer <- rep$issuer_id[1]
  med_issuer <- rep$issuer_id[ceiling(nrow(rep) / 2)]
  cand <- function(iss) pool[idx_id == imap[issuer_id == iss, idx_id] & has_ind & (is.na(bytes_identity) | bytes_identity >= 1e6)]
  chosen <- list()
  add <- function(role, row, iss) chosen[[length(chosen) + 1L]] <<- cbind(role = role, anchor_issuer = iss, row[, .(url, idx_id, issuer_ids, bytes_identity)])
  cb <- cand(big_issuer); add("large-claims issuer", cb[sample(.N, 1)], big_issuer)
  cm <- cand(med_issuer); cm <- cm[!url %in% rbindlist(chosen)$url]; add("median-claims issuer", cm[sample(.N, 1)], med_issuer)
  sm <- pool[!url %in% rbindlist(chosen)$url & !is.na(bytes_identity) & bytes_identity > 0][order(bytes_identity)][1]
  add("smallest retrievable file", sm, strsplit(sm$issuer_ids, ";", fixed = TRUE)[[1]][1])
  rest <- pool[!url %in% rbindlist(chosen)$url & (is.na(bytes_identity) | bytes_identity >= 1e6)]
  for (k in 1:2) {
    rest <- rest[!url %in% rbindlist(chosen)$url]
    r <- rest[sample(.N, 1)]
    add(paste0("random ", k), r, strsplit(r$issuer_ids, ";", fixed = TRUE)[[1]][1])
  }
  out <- rbindlist(chosen)
  out[, `:=`(big_issuer = big_issuer, med_issuer = med_issuer)]
  out
}

JQ_REC <- paste(
  'fromstream(1|truncate_stream(inputs)) as $r |',
  '( ($r.plans // []) | map((.plan_id // null) | tostring) ) as $pids |',
  '( "R\\t" + ([ (($r.npi // null)|tostring), ($r.type // ""), ',
  '   ((($r.specialty // $r.speciality // null)) | if .==null then "<<NULL>>" elif type=="array" then (map(tostring)|join("|")) else tostring end),',
  '   (($r.plans // [])|length|tostring), ($pids|unique|length|tostring), ($pids|map(length)|unique|map(tostring)|join(",")),',
  '   (($r.plans // [])|map(.plan_id_type // "")|unique|join(",")), ($r | keys | join(",")),',
  '   (($r.facility_type // null)|if .==null then "" elif type=="array" then (map(tostring)|join("|")) else tostring end),',
  '   (($r.addresses // [])|map(.state // "")|unique|join(","))',
  ' ] | @tsv) ), ( $pids|unique[] | "P\\t" + . )', sep = " ")
REC_COLS <- c("npi", "type", "specialty", "n_plan_entries", "n_unique_plans", "plan_id_lens", "plan_id_types", "keys", "facility_type", "states")

# Download the first nbytes of url into f (identity encoding, Range first; if the server ignores Range the
# stream is cut at nbytes). Returns timing and what the server did.
fetch_prefix <- function(u, f, nbytes) {
  hf <- tempfile("hdr_")
  t0 <- proc.time()[["elapsed"]]
  cmd <- paste("curl -sS -L --max-time 240 --connect-timeout 15 -A", shQuote(UA), "-H 'Accept-Encoding: identity'",
               "-r", paste0("0-", nbytes - 1), "-D", shQuote(hf), shQuote(u), "2>/dev/null | head -c", nbytes, ">", shQuote(f))
  suppressWarnings(system(cmd))
  t1 <- proc.time()[["elapsed"]]
  hd <- read_hdr(hf); unlink(hf)
  got <- if (file.exists(f)) file.size(f) else 0
  list(http = hd$status, bytes = got, secs = t1 - t0,
       range_honored = identical(hd$status, 206L), content_encoding = hd$h[["content-encoding"]] %||% "",
       content_range = hd$h[["content-range"]] %||% "", content_type = hd$h[["content-type"]] %||% "")
}

# Inspect a downloaded prefix: record summaries via jq --stream (a truncated last record is dropped)
inspect_prefix <- function(f, outdir, tag) {
  raw <- readBin(f, "raw", n = 4000)
  gz <- length(raw) >= 2 && raw[1] == as.raw(0x1f) && raw[2] == as.raw(0x8b)
  if (gz) {
    dec <- paste0(f, ".dec")
    system(paste("gzip -dc", shQuote(f), ">", shQuote(dec), "2>/dev/null"))
    f <- dec
    raw <- readBin(f, "raw", n = 4000)
  }
  txt <- rawToChar(raw[raw != as.raw(0)])
  fc <- substr(sub("^[\\s\\xEF\\xBB\\xBF]+", "", txt, perl = TRUE, useBytes = TRUE), 1, 1)
  head_txt <- substr(gsub("[\r\n]+", " ", txt), 1, 300)
  rec_f <- file.path(outdir, paste0(tag, ".rec.tsv"))
  pid_f <- file.path(outdir, paste0(tag, ".planids.txt"))
  err_f <- file.path(outdir, paste0(tag, ".jq.err"))
  if (fc != "[") return(list(first_char = fc, head = head_txt, parsed = FALSE, gz_encoded_body = gz))
  awk <- paste0("awk -F'\\t' '/^R\\t/{print substr($0,3) > \"", rec_f, "\"} /^P\\t/{c[$2]++} END{for (k in c) print k \"\\t\" c[k] > \"", pid_f, "\"}'")
  file.create(rec_f); file.create(pid_f)
  system(paste0("jq -r --stream -n ", shQuote(JQ_REC), " ", shQuote(f), " 2> ", shQuote(err_f), " | ", awk))
  rec <- NULL; pids <- NULL
  if (file.size(rec_f) > 0) {
    rec <- fread(rec_f, header = FALSE, sep = "\t", quote = "", fill = TRUE, colClasses = "character", col.names = REC_COLS)
    pids <- fread(pid_f, header = FALSE, sep = "\t", quote = "", colClasses = c("character", "integer"), col.names = c("plan_id", "n_records"))
  }
  list(first_char = fc, head = head_txt, parsed = TRUE, gz_encoded_body = gz, jq_err = paste(readLines(err_f, warn = FALSE), collapse = " "),
       rec = rec, pids = pids)
}

# Stream the first nbytes of url through head -c into jq: NO raw JSON is written to disk (the volume was
# below the 6 GB free-space floor during this task, so everything that can be a pipe is a pipe). Only
# the record summaries (small TSV) and, optionally, a short excerpt of the first bytes are kept.
stream_inspect <- function(u, nbytes, tag, outdir, excerpt_bytes = 0, compressed = FALSE) {
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  hf <- tempfile("hdr_")
  first <- file.path(outdir, paste0(tag, ".first.bin"))
  cnt <- tempfile("cnt_"); gzc <- tempfile("gzc_")
  rec_f <- file.path(outdir, paste0(tag, ".rec.tsv"))
  pid_f <- file.path(outdir, paste0(tag, ".planids.txt"))
  err_f <- file.path(outdir, paste0(tag, ".jq.err"))
  for (x in c(rec_f, pid_f, first)) file.create(x)
  awk <- paste0("awk -F'\\t' '/^R\\t/{print substr($0,3) > \"", rec_f, "\"} /^P\\t/{c[$2]++} END{for (k in c) print k \"\\t\" c[k] > \"", pid_f, "\"}'")
  keep <- max(4000L, as.integer(excerpt_bytes))
  script <- paste0(
    "curl -sS -L --max-time 240 --connect-timeout 15 -A ", shQuote(UA), if (compressed) " --compressed" else " -H 'Accept-Encoding: identity'", " -r 0-", nbytes - 1,
    " -D ", shQuote(hf), " ", shQuote(u), " 2>/dev/null",
    " | head -c ", nbytes,
    " | tee >( { head -c ", keep, " > ", shQuote(first), "; cat > /dev/null; } ) >(wc -c > ", shQuote(cnt), ") >(gzip -c | wc -c > ", shQuote(gzc), ")",
    " | jq -r --stream -n ", shQuote(JQ_REC), " 2> ", shQuote(err_f),
    " | ", awk)
  t0 <- proc.time()[["elapsed"]]
  suppressWarnings(system2("bash", c("-c", shQuote(script))))
  secs <- proc.time()[["elapsed"]] - t0
  for (w in 1:20) { if (file.exists(cnt) && nzchar(readLines(cnt, warn = FALSE)[1] %||% "")) break; Sys.sleep(0.25) }
  got <- suppressWarnings(as.numeric(trimws(readLines(cnt, warn = FALSE)[1])))
  for (w in 1:20) { if (file.exists(gzc) && nzchar(readLines(gzc, warn = FALSE)[1] %||% "")) break; Sys.sleep(0.25) }
  gzb <- suppressWarnings(as.numeric(trimws(readLines(gzc, warn = FALSE)[1])))
  hd <- read_hdr(hf); unlink(c(hf, cnt, gzc))
  raw <- readBin(first, "raw", n = 4000)
  gz <- length(raw) >= 2 && raw[1] == as.raw(0x1f) && raw[2] == as.raw(0x8b)
  txt <- iconv(rawToChar(raw[raw != as.raw(0)]), from = "latin1", to = "ASCII", sub = "?")
  fc <- substr(sub("^[\\s\\xEF\\xBB\\xBF]+", "", txt, perl = TRUE, useBytes = TRUE), 1, 1)
  rec <- NULL; pids <- NULL
  if (file.size(rec_f) > 0) {
    rec <- fread(rec_f, header = FALSE, sep = "\t", quote = "", fill = TRUE, colClasses = "character", col.names = REC_COLS)
    pids <- fread(pid_f, header = FALSE, sep = "\t", quote = "", colClasses = c("character", "integer"), col.names = c("plan_id", "n_records"))
  }
  if (excerpt_bytes <= 0) unlink(first) else if (file.size(first) > excerpt_bytes) {
    # keep the excerpt only as large as asked
    system(paste("head -c", as.integer(excerpt_bytes), shQuote(first), ">", shQuote(paste0(first, ".t")), "&& mv", shQuote(paste0(first, ".t")), shQuote(first)))
  }
  list(http = hd$status, bytes = got, secs = secs, local_gzip_bytes = gzb, range_honored = identical(hd$status, 206L),
       content_encoding = hd$h[["content-encoding"]] %||% "", content_range = hd$h[["content-range"]] %||% "",
       content_type = hd$h[["content-type"]] %||% "", first_char = fc, head = substr(gsub("[\r\n]+", " ", txt), 1, 300),
       gz_encoded_body = gz, parsed = (fc == "["), jq_err = paste(readLines(err_f, warn = FALSE), collapse = " "), rec = rec, pids = pids)
}

# NUCC code set (retrieved from nucc.org, checked against the WP1 SHA-256)
NUCC_SHA <- "0615cce1dac2fae75e8a0497657fd3ad81f9505c829fb6129a6551583f4fe0fb"
norm_txt <- function(x) {
  x <- tolower(x)
  x <- gsub("orthopedic", "orthopaedic", x, fixed = TRUE)
  x <- gsub("[^a-z0-9 ]+", " ", x)
  trimws(gsub("\\s+", " ", x))
}
load_nucc <- function() {
  f <- file.path(SPIKE, "nucc_taxonomy_261.csv")
  if (!file.exists(f)) {
    r <- do_curl("https://nucc.org/images/stories/CSV/nucc_taxonomy_261.csv", "get", out_file = f, max_time = 60)
    if (!identical(r$http, 200L)) stop("NUCC download failed: http ", r$http)
  }
  sha <- sha256_file(f)[[1]]
  x <- fread(f, colClasses = "character", encoding = "UTF-8")
  setnames(x, make.names(names(x)))
  list(x = x, sha = sha, sha_ok = identical(sha, NUCC_SHA))
}
nucc_lookup <- function(nu) {
  x <- nu$x
  keys <- unique(c(norm_txt(x$Classification), norm_txt(x$Specialization), norm_txt(x$Display.Name),
                   norm_txt(paste(x$Classification, x$Specialization)), tolower(x$Code)))
  keys[nzchar(keys)]
}
ORTHO_CODES <- c("207X00000X", "207XS0114X", "207XX0004X", "207XS0106X", "207XS0117X", "207XX0801X", "207XP3100X", "207XX0005X")
specialty_flags <- function(sp, nu_keys) {
  # sp: specialty strings joined with "|" ; returns logical vectors
  parts <- strsplit(sp, "|", fixed = TRUE)
  nonempty <- vapply(parts, function(p) any(nzchar(trimws(p)) & p != "<<NULL>>"), logical(1))
  mappable <- vapply(parts, function(p) any(norm_txt(p) %in% nu_keys), logical(1))
  ortho_text <- vapply(parts, function(p) any(grepl("orthop", norm_txt(p), fixed = TRUE)), logical(1))
  list(nonempty = nonempty, mappable = mappable, ortho_text = ortho_text)
}

summarise_records <- function(ins, puf_ids, nu_keys) {
  rec <- ins$rec; pids <- ins$pids
  if (is.null(rec) || !nrow(rec)) return(list(n_records = 0L))
  ind <- rec[type == "INDIVIDUAL"]; fac <- rec[type == "FACILITY"]
  sf <- if (nrow(ind)) specialty_flags(ind$specialty, nu_keys) else list(nonempty = logical(0), mappable = logical(0), ortho_text = logical(0))
  lens <- unlist(strsplit(rec$plan_id_lens[nzchar(rec$plan_id_lens)], ",", fixed = TRUE))
  list(n_records = nrow(rec), n_individual = nrow(ind), n_facility = nrow(fac), n_other_type = nrow(rec) - nrow(ind) - nrow(fac),
       npi_10digit = mean(grepl("^[0-9]{10}$", rec$npi)), n_npi_not10 = sum(!grepl("^[0-9]{10}$", rec$npi)),
       has_plans = mean(as.integer(rec$n_plan_entries) > 0), mean_plan_entries = mean(as.integer(rec$n_plan_entries)),
       plan_id_len_values = paste(sort(unique(lens)), collapse = ","), plan_id_types = paste(sort(unique(unlist(strsplit(rec$plan_id_types, ",", fixed = TRUE)))), collapse = ","),
       n_distinct_plan_ids = nrow(pids), plan_id_match_puf = if (nrow(pids)) mean(pids$plan_id %in% puf_ids) else NA_real_,
       plan_id_match_puf_entries = if (nrow(pids)) sum(pids$n_records[pids$plan_id %in% puf_ids]) / sum(pids$n_records) else NA_real_,
       n_puf_plans_seen = sum(unique(pids$plan_id) %in% puf_ids),
       ind_specialty_nonempty = if (nrow(ind)) mean(sf$nonempty) else NA_real_,
       ind_specialty_mappable = if (nrow(ind)) mean(sf$mappable) else NA_real_,
       ind_specialty_ortho_text = if (nrow(ind)) sum(sf$ortho_text) else NA_integer_,
       n_ind_ortho_text = if (nrow(ind)) sum(sf$ortho_text) else NA_integer_,
       fac_has_facility_type = if (nrow(fac)) mean(nzchar(fac$facility_type)) else NA_real_,
       key_sets = paste(head(names(sort(table(rec$keys), decreasing = TRUE)), 3), collapse = " || "))
}

stage_sample <- function() {
  lg <- function(...) log_msg(..., file = LOGF("sample"))
  pk <- pick_files()
  lg("sample selection (seed ", CFG$study$seed, "):")
  print(pk[, .(role, anchor_issuer, url, MB = round(bytes_identity / 1e6, 1))])
  fwrite(pk, file.path(P$logs, "wp3_sample_selection.csv"))
  plans <- fread(file.path(P$clean_cms, "plans.csv"), colClasses = c(plan_id = "character", issuer_id = "character"), select = c("plan_id", "issuer_id"))
  puf_ids <- unique(plans$plan_id)
  nu <- load_nucc(); nu_keys <- nucc_lookup(nu)
  lg("NUCC file sha256 ", nu$sha, " matches WP1: ", nu$sha_ok, "; free disk GB ", round(free_gb(), 2), " (samples are streamed through a pipe, nothing large is written)")
  rows <- list(); top_spec <- list()
  for (k in seq_len(nrow(pk))) {
    u <- pk$url[k]; iss <- pk$anchor_issuer[k]
    d <- file.path(D_SMP, iss)
    tag <- sprintf("S%d_%s", k, gsub("[^A-Za-z0-9]+", "_", pk$role[k]))
    lg("[", k, "/5] ", pk$role[k], " issuer ", iss, " ", u, " (", round(pk$bytes_identity[k] / 1e6, 1), " MB listed)")
    si <- stream_inspect(u, SAMPLE_BYTES, tag, d, excerpt_bytes = 30000)
    thr <- si$bytes / max(si$secs, 0.001) / 1e6
    lg("  http ", si$http, " bytes ", si$bytes, " range_honored ", si$range_honored, " in ", round(si$secs, 1), " s = ", round(thr, 2), " MB/s (includes jq parse time)")
    # compression: gzip request, raw (undecoded) wire bytes for the first 4 MiB, piped (no disk)
    hf2 <- tempfile("hdr_")
    wire_dec <- suppressWarnings(system2("bash", c("-c", shQuote(paste0(
      "curl -sS -L --raw --max-time 120 --connect-timeout 15 -A ", shQuote(UA), " -H 'Accept-Encoding: gzip' -D ", shQuote(hf2), " ", shQuote(u),
      " 2>/dev/null | head -c 4194304 | tee >(wc -c > ", shQuote(paste0(hf2, ".wire")), ") | gzip -dc 2>/dev/null | wc -c"))), stdout = TRUE))
    Sys.sleep(0.5)
    wire <- suppressWarnings(as.numeric(trimws(readLines(paste0(hf2, ".wire"), warn = FALSE)[1])))
    hd2 <- read_hdr(hf2); enc2 <- hd2$h[["content-encoding"]] %||% ""
    decoded <- if (grepl("gzip", enc2)) suppressWarnings(as.numeric(trimws(wire_dec[1]))) else wire
    unlink(c(hf2, paste0(hf2, ".wire")))
    # pure network throughput: the same 20 MiB window piped to wc (no parsing), timed
    tn0 <- proc.time()[["elapsed"]]
    net_bytes <- suppressWarnings(as.numeric(trimws(system2("bash", c("-c", shQuote(paste0(
      "curl -sS -L --max-time 240 --connect-timeout 15 -A ", shQuote(UA), " -H 'Accept-Encoding: identity' -r 0-", SAMPLE_BYTES - 1, " ",
      shQuote(u), " 2>/dev/null | head -c ", SAMPLE_BYTES, " | wc -c"))), stdout = TRUE)[1])))
    net_secs <- proc.time()[["elapsed"]] - tn0
    sm <- if (isTRUE(si$parsed)) summarise_records(si, puf_ids, nu_keys) else list(n_records = 0L)
    lg("  network-only: ", net_bytes, " bytes in ", round(net_secs, 1), " s = ", round(net_bytes / max(net_secs, 0.001) / 1e6, 2), " MB/s")
    lg("  records ", sm$n_records, "; first char '", si$first_char, "'; jq note: ", substr(si$jq_err %||% "", 1, 100))
    rows[[k]] <- data.table(sample_no = k, role = pk$role[k], anchor_issuer = iss, provider_url = u, listed_bytes = pk$bytes_identity[k],
      http = si$http, range_honored = si$range_honored, prefix_bytes = si$bytes, seconds_with_parse = round(si$secs, 2), MB_per_s_with_parse = round(thr, 2),
      net_only_bytes = net_bytes, net_only_seconds = round(net_secs, 2), net_only_MB_per_s = round(net_bytes / max(net_secs, 0.001) / 1e6, 2),
      content_type = si$content_type, gzip_requested_encoding = enc2, wire_bytes_4MiB_window = wire, decoded_bytes_of_window = decoded,
      wire_to_decoded_ratio = round(decoded / max(wire, 1), 2), local_gzip_ratio_on_prefix = round(si$bytes / max(si$local_gzip_bytes, 1), 2),
      first_char = si$first_char, parsed = isTRUE(si$parsed), as.data.table(lapply(sm, function(x) if (length(x) == 0) NA else x)))
    if (!is.null(si$rec)) {
      ind <- si$rec[type == "INDIVIDUAL"]
      if (nrow(ind)) top_spec[[k]] <- cbind(sample_no = k, ind[, .N, by = specialty][order(-N)][1:min(.N, 15)])
      fwrite(si$rec[1:min(.N, 200), !c("npi"), with = FALSE], file.path(d, paste0(tag, ".rec_head_no_npi.csv")))
      writeLines(si$rec[type == "INDIVIDUAL" & grepl("^[0-9]{10}$", npi), npi][1], file.path(D_ST, paste0("sample_npi_", k, ".txt")))
    }
    Sys.sleep(1)
  }
  dt <- rbindlist(rows, fill = TRUE)
  fwrite(dt, file.path(P$logs, "wp3_sample_schema.csv"))
  if (length(top_spec)) fwrite(rbindlist(top_spec), file.path(P$logs, "wp3_sample_top_specialties.csv"))
  print(t(dt[, !c("provider_url")]))
  invisible(dt)
}

# micro-probe: first 256 KiB of EVERY distinct provider file (type mix, schema and plan-ID prefixes per file)
stage_probe <- function() {
  lg <- function(...) log_msg(..., file = LOGF("probe"))
  probe <- fread(file.path(P$logs, "wp3_index_probe.csv"), colClasses = c(idx_id = "character", issuer_ids = "character"))
  ok <- probe[is.na(failure_reason) | failure_reason == ""]
  pv <- rbindlist(lapply(ok$idx_id, function(id) {
    x <- fread(file.path(D_IDX, paste0(id, ".urls.tsv")), header = FALSE, sep = "\t", col.names = c("kind", "url"))
    x[kind == "provider"][, idx_id := id]
  }))
  pick <- unique(pv[, .(url)])
  pick[, fid := sprintf("F%04d", seq_len(.N))]
  plans <- fread(file.path(P$clean_cms, "plans.csv"), colClasses = c(plan_id = "character", issuer_id = "character"), select = c("plan_id", "issuer_id"))
  puf_ids <- unique(plans$plan_id)
  nu <- load_nucc(); nu_keys <- nucc_lookup(nu)
  d <- file.path(SPIKE, "probe"); dir.create(d, showWarnings = FALSE)
  lg("free disk GB now: ", round(free_gb(), 2), " (probe bodies are streamed through a pipe; nothing large is written)")
  lg("micro-probing ", nrow(pick), " distinct provider files, 256 KiB each (4 MiB retry when no complete record fits)")
  one <- function(url, i) {
    tag <- pick$fid[i]
    si <- stream_inspect(url, 262144, tag, d, excerpt_bytes = 0)
    tier <- "256 KiB"
    if (is.na(si$http)) { Sys.sleep(2); si <- stream_inspect(url, 262144, tag, d, excerpt_bytes = 0) }
    if (isTRUE(si$gz_encoded_body)) { si <- stream_inspect(url, 262144, tag, d, excerpt_bytes = 0, compressed = TRUE); tier <- "256 KiB (curl --compressed: server gzips even when identity is requested)" }
    if (isTRUE(si$parsed) && (is.null(si$rec) || nrow(si$rec) == 0L)) {
      # no complete record in the first 256 KiB (records can hold hundreds of plan IDs): retry once with 4 MiB
      Sys.sleep(0.3)
      si <- stream_inspect(url, 4194304, tag, d, excerpt_bytes = 0)
      tier <- "4 MiB"
    }
    sm <- if (isTRUE(si$parsed)) summarise_records(si, puf_ids, nu_keys) else list(n_records = 0L)
    pid_issuers <- if (!is.null(si$pids) && nrow(si$pids)) paste(sort(unique(substr(si$pids$plan_id, 1, 5))), collapse = ";") else NA_character_
    unlink(file.path(d, paste0(tag, c(".rec.tsv", ".planids.txt", ".jq.err", ".first.bin"))))
    c(list(fid = tag, provider_url = url, prefix_tier = tier, http = si$http, prefix_bytes = si$bytes, range_honored = si$range_honored,
           first_char = si$first_char, first_bytes = if (!isTRUE(si$parsed)) si$head else NA_character_,
           parsed = isTRUE(si$parsed), gz_encoded_body = isTRUE(si$gz_encoded_body), plan_issuer_prefixes = pid_issuers),
      sm)
  }
  res <- run_by_host(pick$url, one, pause = 0.2)
  bad <- vapply(res, function(r) !is.null(r$.error), logical(1))
  if (any(bad)) lg("R-level errors: ", sum(bad), " first: ", res[[which(bad)[1]]]$.error)
  dt <- rbindlist(lapply(res[!bad], function(r) as.data.table(lapply(r, function(x) if (length(x) == 0) NA else x))), fill = TRUE)
  fwrite(dt, file.path(P$logs, "wp3_provider_file_probe.csv"))
  lg("microprobe: files ", nrow(dt), "; parsed with records ", sum(dt$n_records > 0, na.rm = TRUE))
  print(dt[, .(n = .N, with_records = sum(n_records > 0, na.rm = TRUE), has_individual = sum(n_individual > 0, na.rm = TRUE),
               has_facility = sum(n_facility > 0, na.rm = TRUE))])
  invisible(dt)
}

# ---------------------------------------------------------------------
# stage nppes
# ---------------------------------------------------------------------
stage_nppes <- function() {
  lg <- function(...) log_msg(..., file = LOGF("nppes"))
  res <- list()
  rec <- function(test, url, r, note = "") {
    res[[length(res) + 1L]] <<- data.table(test = test, url = url, http = r$http, exit = r$exit,
      content_length = r$hdr[["content-length"]] %||% NA_character_, last_modified = r$hdr[["last-modified"]] %||% NA_character_,
      content_type = r$ctype, final_url = r$final_url, note = note, tested_utc = now_utc())
  }
  base <- "https://download.cms.gov/nppes/"
  # 1. live listing page
  lp <- file.path(D_NP, "NPI_Files_live.html")
  r <- do_curl(paste0(base, "NPI_Files.html"), "get", out_file = lp, max_time = 60, accept_enc = "gzip")
  txt <- if (file.exists(lp)) paste(readLines(lp, warn = FALSE), collapse = " ") else ""
  plain <- trimws(gsub("\\s+", " ", gsub("<[^>]+>", " ", txt)))
  zips <- unique(regmatches(txt, gregexpr("NPPES_[A-Za-z0-9_\\-]*\\.zip", txt))[[1]])
  rec("listing page (GET)", paste0(base, "NPI_Files.html"), r, paste0("body text: ", substr(plain, 1, 160), " | zip names listed: ", length(zips)))
  lg("listing page http ", r$http, " last-modified ", r$hdr[["last-modified"]] %||% "NA", " zip names listed ", length(zips), " text: ", substr(plain, 1, 120))
  # 2. monthly zips, latest three months (and the deactivation report named in the last archived listing)
  heads <- c("NPPES_Data_Dissemination_October_2026_V2.zip", "NPPES_Data_Dissemination_September_2026_V2.zip",
             "NPPES_Data_Dissemination_August_2026_V2.zip", "NPPES_Deactivated_NPI_Report_101226_V2.zip",
             "NPPES_Data_Dissemination_101226_101826_Weekly_V2.zip", "NPPES_Data_Dissemination_101926_102526_Weekly_V2.zip")
  lab <- c("monthly Oct 2026 V2", "monthly Sep 2026 V2", "monthly Aug 2026 V2", "deactivation report 101226 V2", "weekly 101226-101826 V2", "weekly 101926-102526 V2")
  for (i in seq_along(heads)) {
    r <- do_curl(paste0(base, heads[i]), "head", max_time = 30, accept_enc = "identity")
    rec(paste0("HEAD ", lab[i]), paste0(base, heads[i]), r)
    lg("HEAD ", heads[i], " -> ", r$http, " content-length ", r$hdr[["content-length"]] %||% "NA")
    Sys.sleep(0.5)
  }
  # 3. one NPI Registry API smoke query (single NPI, not bulk). NPI taken from the sampled provider records.
  npi <- NA_character_
  for (k in 1:5) {
    f <- file.path(D_ST, paste0("sample_npi_", k, ".txt"))
    if (file.exists(f)) { x <- readLines(f, warn = FALSE)[1]; if (!is.na(x) && grepl("^[0-9]{10}$", x)) { npi <- x; break } }
  }
  if (is.na(npi)) npi <- "1234567893"
  ap <- file.path(D_NP, "npi_registry_smoke.json")
  r <- do_curl(paste0("https://npiregistry.cms.hhs.gov/api/?version=2.1&number=", npi), "get", out_file = ap, max_time = 30)
  note <- ""
  if (file.exists(ap) && file.size(ap) > 0 && identical(body_kind(ap), "json")) {
    j <- tryCatch(jsonlite::fromJSON(ap, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.null(j)) {
      rc <- j$result_count %||% NA
      tx <- tryCatch(vapply(j$results[[1]]$taxonomies, function(t) paste0(t$code, if (isTRUE(t$primary)) "*" else ""), character(1)), error = function(e) character(0))
      note <- paste0("result_count=", rc, "; keys=", paste(names(j), collapse = ","), "; taxonomy codes (primary starred) present: ", length(tx) > 0)
      writeLines(note, file.path(D_NP, "npi_registry_smoke_note.txt"))
    }
  } else note <- paste0("body kind: ", body_kind(ap))
  rec("NPI Registry API single-NPI query", "https://npiregistry.cms.hhs.gov/api/?version=2.1&number=<one sampled NPI>", r, note)
  lg("NPI Registry API http ", r$http, " ", note)
  dt <- rbindlist(res)
  fwrite(dt, file.path(P$logs, "wp3_nppes_tests.csv"))
  print(dt[, .(test, http, content_length, last_modified, note = substr(note, 1, 70))])
  # 4. stream-pipeline test only if a monthly zip is reachable
  mz <- dt[grepl("^HEAD monthly", test) & http == 200L]
  if (nrow(mz)) {
    sz <- as.numeric(mz$content_length[1])
    lg("monthly zip reachable: ", mz$url[1], " size ", sz)
    fgb <- free_gb()
    if (!is.na(sz) && sz <= 1.5e9 && fgb - sz / 1e9 >= FLOOR_GB + 1) {
      zf <- file.path(D_NP, "nppes_monthly.zip")
      r <- do_curl(mz$url[1], "get", out_file = zf, max_time = 1800, accept_enc = "identity")
      lg("downloaded ", file.size(zf), " bytes at ", round(r$speed / 1e6, 2), " MB/s")
      lst <- system(paste("unzip -l", shQuote(zf)), intern = TRUE)
      writeLines(lst, file.path(P$logs, "wp3_nppes_zip_listing.txt"))
      dat <- grep("npidata_pfile_[0-9]+-[0-9]+\\.csv$", lst, value = TRUE)
      nm <- sub("^.*\\s(npidata_pfile_[0-9\\-]+\\.csv)$", "\\1", dat[1])
      pre <- file.path(D_NP, "npidata_prefix.csv")
      system(paste("unzip -p", shQuote(zf), shQuote(nm), "2>/dev/null | head -c 104857600 >", shQuote(pre)))
      hdr <- readLines(pre, n = 1, warn = FALSE)
      writeLines(hdr, file.path(P$logs, "wp3_nppes_header.txt"))
      lg("data file ", nm, "; prefix bytes ", file.size(pre), "; header fields ", length(strsplit(hdr, '","', fixed = TRUE)[[1]]))
      unlink(zf)
    } else lg("monthly zip reachable but disk guard or size cap prevented the download test")
  } else lg("no monthly zip reachable: NPPES bulk file UNAVAILABLE at test time")
  invisible(dt)
}

# ---------------------------------------------------------------------
# stage eval : inventory, issuer sources, size model, G1 numbers
# ---------------------------------------------------------------------
stage_eval <- function() {
  lg <- function(...) log_msg(..., file = LOGF("eval"))
  vol <- read_vol()
  mr <- read_mr()
  probe <- fread(file.path(P$logs, "wp3_index_probe.csv"), colClasses = c(idx_id = "character", issuer_ids = "character"))
  sz <- fread(file.path(P$logs, "wp3_provider_file_sizes.csv"), integer64 = "double")
  fp <- fread(file.path(P$logs, "wp3_provider_file_probe.csv"), colClasses = c(plan_issuer_prefixes = "character"))
  planmap <- fread(file.path(P$logs, "wp3_plan_ids_from_plan_files.csv"), colClasses = "character")
  plans <- fread(file.path(P$clean_cms, "plans.csv"), colClasses = c(plan_id = "character", issuer_id = "character"))
  plans[, computable := !is.na(plan_in_network_denial_rate)]
  plans[, has_enroll := !is.na(avg_monthly_enrollment) & avg_monthly_enrollment_marker == "numeric"]
  # ---- index -> provider URL rows ---------------------------------------
  okidx <- probe[is.na(failure_reason) | failure_reason == ""]
  pv <- rbindlist(lapply(okidx$idx_id, function(id) {
    x <- fread(file.path(D_IDX, paste0(id, ".urls.tsv")), header = FALSE, sep = "\t", col.names = c("kind", "url"))
    x[kind == "provider"][, idx_id := id]
  }))
  setnames(pv, "url", "provider_url")
  szs <- copy(sz); setnames(szs, "url", "provider_url")
  fps <- fp[, .(provider_url, probe_http = http, probe_prefix_bytes = prefix_bytes, probe_range_honored = range_honored,
                probe_parsed = parsed, n_records, n_individual, n_facility, n_other_type, npi_10digit, has_plans, plan_id_len_values,
                plan_issuer_prefixes, ind_specialty_nonempty, ind_specialty_mappable)]
  inv <- merge(pv[, !"kind"], szs, by = "provider_url", all.x = TRUE)
  inv <- merge(inv, fps, by = "provider_url", all.x = TRUE)
  inv <- merge(inv, probe[, .(idx_id, index_url, n_issuers, issuer_ids)], by = "idx_id")
  inv[, host := host_of(provider_url)]
  ok_http <- function(h) !is.na(h) & h >= 200 & h < 300
  inv[, status := fifelse(!is.na(bytes_identity), "ok", fifelse(ok_http(http) | ok_http(probe_http), "ok_size_unknown", paste0("failed: ", failure_reason)))]
  inv[, count_bytes_once := !duplicated(provider_url)]
  inv[, content_class := fifelse(is.na(n_records) | n_records == 0, "unknown (no complete record in prefix)",
                         fifelse(n_individual > 0 & (n_facility > 0 | n_other_type > 0), "mixed (individual plus facility or group)",
                         fifelse(n_individual > 0, "individual only", "facility or group only")))]
  inv_out <- inv[, .(index_url, provider_url, status, bytes = bytes_identity, bytes_source, content_encoding,
                     retrieved_utc, n_issuers_covered = n_issuers, issuer_ids,
                     idx_id, host, http_status = http, gzip_behaviour, bytes_gzip_head, accept_ranges,
                     count_bytes_once, content_class_from_prefix = content_class, plan_issuer_prefixes_in_prefix = plan_issuer_prefixes)]
  fwrite(inv_out, file.path(OUT, "provider_file_inventory.csv"))
  lg("inventory rows ", nrow(inv_out), "; distinct provider URLs ", uniqueN(inv_out$provider_url))
  # ---- per-index summary --------------------------------------------------
  inv[, file_parse_ok := (!is.na(n_records) & n_records > 0 & npi_10digit > 0 & has_plans > 0) | (!is.na(probe_prefix_bytes) & probe_prefix_bytes < 2000 & ok_http(probe_http))]
  inv[, file_retrievable := status %in% c("ok", "ok_size_unknown") & ok_http(probe_http)]
  ix <- inv[, .(n_provider_files = .N, n_files_retrievable = sum(file_retrievable), n_files_parse_ok = sum(file_parse_ok),
                n_files_with_individuals = sum(n_individual > 0, na.rm = TRUE),
                n_files_sized = sum(status == "ok"), bytes_known = sum(bytes_identity, na.rm = TRUE),
                n_files_size_unknown = sum(status == "ok_size_unknown")), by = idx_id]
  # ---- plan files per index -----------------------------------------------
  planurls <- rbindlist(lapply(okidx$idx_id, function(id) {
    x <- fread(file.path(D_IDX, paste0(id, ".urls.tsv")), header = FALSE, sep = "\t", col.names = c("kind", "url"))
    x[kind == "plan"][, idx_id := id]
  }))
  pm <- merge(planurls[, .(idx_id, plan_url = url)], planmap, by = "plan_url", allow.cartesian = TRUE)
  idx_plan_ids <- unique(pm[, .(idx_id, plan_id)])
  fetched <- fread(file.path(P$logs, "wp3_plan_file_fetch.csv"), colClasses = c(url = "character"))
  pf_ok <- merge(planurls, fetched[is.na(failure_reason), .(url)], by = "url")
  ix_pl <- merge(planurls[, .(n_plan_files = .N), by = idx_id], pf_ok[, .(n_plan_files_ok = .N), by = idx_id], by = "idx_id", all.x = TRUE)
  ix_pl[is.na(n_plan_files_ok), n_plan_files_ok := 0L]
  # ---- issuer table --------------------------------------------------------
  isr <- merge(vol[, .(issuer_id, state, issuer_name, in_network_claims_received, has_reported_in_network_count)],
               mr[, .(issuer_id, index_url = mr_index_url)], by = "issuer_id", all.x = TRUE)
  isr <- merge(isr, probe[, .(index_url, idx_id, index_failure = failure_reason, n_issuers_sharing_index = n_issuers)], by = "index_url", all.x = TRUE)
  isr[, index_status := fifelse(is.na(index_url), "no MR URL in CMS PUF", fifelse(is.na(index_failure) | index_failure == "", "ok", paste0("failed: ", index_failure)))]
  isr <- merge(isr, ix, by = "idx_id", all.x = TRUE)
  isr <- merge(isr, ix_pl, by = "idx_id", all.x = TRUE)
  # issuer id seen in the plan-ID prefixes of any probed file of its index
  seen <- inv[!is.na(plan_issuer_prefixes), .(prefixes = paste(unique(unlist(strsplit(plan_issuer_prefixes, ";", fixed = TRUE))), collapse = ";")), by = idx_id]
  isr <- merge(isr, seen, by = "idx_id", all.x = TRUE)
  isr[, issuer_in_provider_prefixes := !is.na(prefixes) & mapply(function(i, p) i %in% strsplit(p, ";", fixed = TRUE)[[1]], issuer_id, prefixes)]
  cp <- plans[computable == TRUE, .(n_computable_plans = .N, n_computable_with_enrollment = sum(has_enroll),
                                    computable_plan_claims = sum(plan_claims_received_in_network, na.rm = TRUE)), by = issuer_id]
  isr <- merge(isr, cp, by = "issuer_id", all.x = TRUE)
  for (j in c("n_computable_plans", "n_computable_with_enrollment", "computable_plan_claims")) set(isr, which(is.na(isr[[j]])), j, 0)
  isr[, has_computable_plan := n_computable_plans > 0]
  cm <- merge(plans[computable == TRUE, .(plan_id, issuer_id, has_enroll, claims = plan_claims_received_in_network)], isr[, .(issuer_id, idx_id)], by = "issuer_id")
  cm[, in_plan_file := paste(idx_id, plan_id) %in% idx_plan_ids[, paste(idx_id, plan_id)]]
  isr <- merge(isr, cm[, .(n_computable_plans_in_plan_file = sum(in_plan_file), n_computable_enroll_in_plan_file = sum(in_plan_file & has_enroll)), by = issuer_id], by = "issuer_id", all.x = TRUE)
  for (j in c("n_computable_plans_in_plan_file", "n_computable_enroll_in_plan_file")) set(isr, which(is.na(isr[[j]])), j, 0L)
  # qualification (the numerator of the G1 coverage share)
  #   strict : index ok, EVERY listed provider file retrievable, at least one file parse-confirmed (npi + plans[].plan_id)
  #   relaxed: index ok, at least one provider file retrievable and parse-confirmed
  isr[, any_parse := !is.na(n_files_parse_ok) & n_files_parse_ok > 0]
  isr[, qual_strict := index_status == "ok" & !is.na(n_provider_files) & n_provider_files > 0 & n_files_retrievable == n_provider_files & any_parse]
  isr[, qual_relaxed := index_status == "ok" & !is.na(n_files_retrievable) & n_files_retrievable > 0 & any_parse]
  # bytes without double counting: the whole index is shared equally between the issuers that use it
  idxb <- inv[count_bytes_once == TRUE, .(index_bytes_known = sum(bytes_identity, na.rm = TRUE)), by = idx_id]
  isr <- merge(isr, idxb, by = "idx_id", all.x = TRUE)
  isr[, total_bytes := index_bytes_known / n_issuers_sharing_index]
  isr[, total_bytes_is_lower_bound := !is.na(n_files_size_unknown) & n_files_size_unknown > 0]
  out_is <- isr[, .(issuer_id, state, issuer_name, in_network_claims_received, has_reported_in_network_count, has_computable_plan,
                    n_computable_plans, n_computable_with_enrollment, computable_plan_claims, index_url, index_status,
                    n_provider_files, n_files_retrievable, n_files_size_unknown, total_bytes, total_bytes_is_lower_bound, index_bytes_known, n_issuers_sharing_index,
                    n_files_parse_ok, n_files_with_individuals, schema_issuer_id_in_provider_plan_ids = issuer_in_provider_prefixes,
                    n_computable_plans_in_plan_file, n_plan_files, n_plan_files_ok,
                    retrievable_and_parses_strict = qual_strict, retrievable_and_parses_relaxed = qual_relaxed)]
  setorder(out_is, -in_network_claims_received, na.last = TRUE)
  fwrite(out_is, file.path(OUT, "issuer_sources.csv"))
  lg("issuer_sources rows ", nrow(out_is))
  saveRDS(list(isr = isr, inv = inv, plans = plans, cm = cm, vol = vol), file.path(D_ST, "eval_objects.rds"))
  invisible(out_is)
}

# ---------------------------------------------------------------------
# stage g1 : the numbers behind the verdict (reads everything the other stages wrote)
# ---------------------------------------------------------------------
stage_g1 <- function() {
  lg <- function(...) log_msg(..., file = LOGF("g1"))
  ob <- readRDS(file.path(D_ST, "eval_objects.rds"))
  isr <- ob$isr; inv <- ob$inv; plans <- ob$plans; cm <- ob$cm; vol <- ob$vol
  smp <- fread(file.path(P$logs, "wp3_sample_schema.csv"), colClasses = c(anchor_issuer = "character", plan_id_len_values = "character", plan_id_types = "character"))
  np <- fread(file.path(P$logs, "wp3_nppes_tests.csv"), colClasses = c(content_length = "character"))
  fpr <- fread(file.path(P$logs, "wp3_provider_file_probe.csv"), colClasses = c(plan_issuer_prefixes = "character"))
  K <- list()
  add <- function(key, value, note = "") { K[[length(K) + 1L]] <<- data.table(k = key, value = as.character(value), note = note); invisible(NULL) }
  pct <- function(x) round(100 * x, 2)
  # ---------------- (a) coverage ----------------
  rep <- isr[has_reported_in_network_count == TRUE]
  tot_claims <- sum(rep$in_network_claims_received)
  add("issuers_in_individual_qhp_population", nrow(isr)); add("issuers_with_mr_url", sum(!is.na(isr$index_url)))
  add("distinct_index_urls", uniqueN(isr$index_url[!is.na(isr$index_url)]))
  add("issuers_reporting_in_network_count", nrow(rep)); add("in_network_claims_reporting_issuers", tot_claims)
  for (nm in c("strict", "relaxed")) {
    q <- if (nm == "strict") rep$qual_strict else rep$qual_relaxed
    add(paste0("cov_issuer_weighted_", nm), sum(rep$in_network_claims_received[q]) / tot_claims, "share of 451.2M in-network claims held by qualifying issuers")
    add(paste0("cov_issuer_unweighted_", nm), mean(q), "share of the 159 reporting issuers")
    add(paste0("n_issuers_qualifying_all185_", nm), sum(if (nm == "strict") isr$qual_strict else isr$qual_relaxed))
  }
  coh <- merge(plans[computable == TRUE, .(plan_id, issuer_id, claims = plan_claims_received_in_network, has_enroll)],
               isr[, .(issuer_id, qual_strict, qual_relaxed, index_status)], by = "issuer_id")
  add("cohort_plans", nrow(coh)); add("cohort_issuers", uniqueN(coh$issuer_id)); add("cohort_claims", sum(coh$claims))
  for (nm in c("strict", "relaxed")) {
    q <- if (nm == "strict") coh$qual_strict else coh$qual_relaxed
    add(paste0("cov_cohort_plans_", nm), mean(q)); add(paste0("cov_cohort_claims_", nm), sum(coh$claims[q]) / sum(coh$claims))
    add(paste0("cohort_issuers_qualifying_", nm), uniqueN(coh$issuer_id[q]))
  }
  # why issuers do not qualify (strict), among all 185
  why <- isr[qual_strict == FALSE | is.na(qual_strict), .(n = .N, claims = sum(in_network_claims_received, na.rm = TRUE)), by = .(reason = fifelse(index_status != "ok", index_status, "index ok but not every file retrievable or no parse"))]
  print(why[order(-claims)])
  # ---------------- (b) record content ----------------
  ind_w <- function(dt, col) { w <- dt$n_individual; sum(dt[[col]] * w, na.rm = TRUE) / sum(w[!is.na(dt[[col]])]) }
  add("sample_files", nrow(smp)); add("sample_records_total", sum(smp$n_records, na.rm = TRUE)); add("sample_individual_records", sum(smp$n_individual, na.rm = TRUE))
  add("sample_facility_records", sum(smp$n_facility, na.rm = TRUE))
  add("sample_npi_10digit_share", sum(smp$npi_10digit * smp$n_records, na.rm = TRUE) / sum(smp$n_records, na.rm = TRUE))
  add("sample_npi_not10_count", sum(smp$n_npi_not10, na.rm = TRUE))
  add("sample_has_plans_share", sum(smp$has_plans * smp$n_records, na.rm = TRUE) / sum(smp$n_records, na.rm = TRUE))
  add("sample_specialty_nonempty_individuals", ind_w(smp, "ind_specialty_nonempty")); add("sample_specialty_mappable_individuals", ind_w(smp, "ind_specialty_mappable"))
  add("sample_plan_id_lengths", paste(sort(unique(unlist(strsplit(smp$plan_id_len_values[!is.na(smp$plan_id_len_values)], ",", fixed = TRUE)))), collapse = ","))
  add("sample_plan_id_match_puf_distinct_ids", paste(round(smp$plan_id_match_puf, 3), collapse = ";"), "per sample, share of distinct plan_ids in the prefix that are PUF plan IDs")
  add("sample_plan_id_match_puf_entries_pooled", sum(smp$plan_id_match_puf_entries * smp$n_records, na.rm = TRUE) / sum(smp$n_records, na.rm = TRUE), "record-weighted share of (record, plan) entries whose plan_id is in the PUF")
  fi <- fpr[n_records > 0]
  add("allfiles_probed", nrow(fpr)); add("allfiles_with_records_in_prefix", nrow(fi))
  add("allfiles_individual_records", sum(fi$n_individual, na.rm = TRUE)); add("allfiles_facility_records", sum(fi$n_facility, na.rm = TRUE))
  add("allfiles_specialty_nonempty_individuals", ind_w(fi, "ind_specialty_nonempty")); add("allfiles_specialty_mappable_individuals", ind_w(fi, "ind_specialty_mappable"))
  add("allfiles_npi_10digit_share", sum(fi$npi_10digit * fi$n_records, na.rm = TRUE) / sum(fi$n_records, na.rm = TRUE))
  add("allfiles_files_with_individuals", sum(fi$n_individual > 0, na.rm = TRUE)); add("allfiles_files_no_individual_in_prefix", sum(fi$n_individual == 0, na.rm = TRUE))
  mz <- np[grepl("^HEAD monthly", test)]
  nppes_reach <- any(mz$http == 200L, na.rm = TRUE)
  add("nppes_monthly_reachable", nppes_reach); add("nppes_monthly_http", paste(mz$http, collapse = ","))
  add("nppes_api_http", np[grepl("API", test), http][1])
  # ---------------- (c) volume ----------------
  u <- inv[count_bytes_once == TRUE]
  u[, compresses := grepl("^gzip", gzip_behaviour)]
  ratio <- suppressWarnings(median(smp$wire_to_decoded_ratio[grepl("gzip", smp$gzip_requested_encoding)], na.rm = TRUE))
  ratio_src <- "median wire/decoded ratio of the sampled files whose server returned Content-Encoding gzip"
  if (!is.finite(ratio)) { ratio <- suppressWarnings(median(smp$local_gzip_ratio_on_prefix, na.rm = TRUE)); ratio_src <- "median local gzip ratio of the 20 MiB sample prefixes (no sampled server compressed)" }
  M <- median(u$bytes_identity, na.rm = TRUE); P90 <- as.numeric(quantile(u$bytes_identity, 0.9, na.rm = TRUE))
  add("provider_urls_distinct", nrow(u)); add("provider_urls_sized", sum(!is.na(u$bytes_identity))); add("provider_urls_size_unknown", sum(is.na(u$bytes_identity)))
  add("bytes_known_identity_GB", sum(u$bytes_identity, na.rm = TRUE) / 1e9, "uncompressed stream size, files with a known size")
  add("bytes_known_noncompressing_servers_GB", sum(u$bytes_identity[!u$compresses], na.rm = TRUE) / 1e9, "HARD LOWER BOUND of transfer: these servers do not gzip, transfer equals uncompressed size")
  add("n_files_noncompressing_known", sum(!u$compresses & !is.na(u$bytes_identity)))
  add("bytes_known_compressing_servers_GB", sum(u$bytes_identity[u$compresses], na.rm = TRUE) / 1e9)
  add("median_known_file_MB", M / 1e6); add("p90_known_file_MB", P90 / 1e6)
  add("gzip_ratio_used", ratio, ratio_src)
  vb <- function(d) {
    kn <- d[!is.na(bytes_identity)]; un <- d[is.na(bytes_identity)]
    list(low = sum(kn$bytes_identity[!kn$compresses]) + sum(kn$bytes_identity[kn$compresses]) / max(ratio, 1),
         central = sum(ifelse(kn$compresses, kn$bytes_identity / ratio, kn$bytes_identity)) + sum(ifelse(un$compresses, M / ratio, M)),
         high = sum(kn$bytes_identity) + nrow(un) * P90, n = nrow(d), n_unknown = nrow(un))
  }
  A <- vb(u)
  B <- vb(u[content_class != "facility or group only"])
  for (nm in c("low", "central", "high")) {
    add(paste0("transfer_GB_all_listed_files_", nm), A[[nm]] / 1e9,
        c(low = "scope A, every listed file of every retrievable issuer; known sizes only, compressing servers at the measured ratio, unknown-size files counted as zero",
          central = "scope A; unknown-size files at the median known size, compressing servers at the measured ratio",
          high = "scope A; no compression anywhere, unknown-size files at the 90th percentile known size")[[nm]])
    add(paste0("transfer_GB_individual_bearing_files_", nm), B[[nm]] / 1e9,
        if (nm == "central") "scope B, files whose prefix holds at least one INDIVIDUAL record (prefix-based, WP4 must verify); same assumptions as scope A" else "scope B, same assumptions as scope A")
  }
  add("scopeB_files", B$n); add("scopeB_files_unknown_size", B$n_unknown); add("scopeA_files_unknown_size", A$n_unknown)
  add("facility_or_group_only_files_by_prefix", sum(u$content_class == "facility or group only"))
  add("unknown_size_files_by_host", paste(u[is.na(bytes_identity), .N, by = host][order(-N)][, paste0(host, "=", N)], collapse = "; "))
  hb <- u[, .(GB = sum(ifelse(is.na(bytes_identity), M, bytes_identity)) / 1e9), by = host][order(-GB)]
  add("largest_host_GB_uncompressed", hb$GB[1], hb$host[1])
  # throughput
  thr <- smp$net_only_MB_per_s[smp$net_only_bytes >= 1e6]   # the 11 KB sample says nothing about throughput
  add("sample_net_throughput_MB_s_each", paste(thr, collapse = ";")); add("sample_net_throughput_MB_s_median", median(thr, na.rm = TRUE))
  sp <- median(thr, na.rm = TRUE)
  wall_h <- max(A$central / (MAXPAR * sp * 1e6), (hb$GB[1] * 1e9 / ifelse(u$compresses[match(hb$host[1], u$host)], ratio, 1)) / (sp * 1e6)) / 3600
  add("wall_clock_hours_slow_case", A$central / (MAXPAR * min(thr) * 1e6) / 3600, "same volume at the slowest sampled large-file speed on all 8 streams")
  add("wall_clock_hours_estimate", wall_h, "max of total/(8 streams x median MB/s) and the largest host serially (one connection per host)")
  # peak disk (model, not a measurement)
  nppes_gb <- 1.106
  add("peak_disk_GB_model", nppes_gb + 1.0 + 0.5, "NPPES monthly zip 1.106 GB (archived listing) + at most 1 GB of kept intermediates (design cap: only individual records, aggregated to NPI x plan set) + 0.5 GB tmp; provider JSON is streamed and never stored")
  free_now <- free_gb()
  add("free_disk_GB_now", free_now); add("free_disk_GB_min_observed", suppressWarnings(min(fread(file.path(P$logs, "wp3_disk_trend.csv"))$free_gb)))
  add("free_disk_GB_projected_min", free_now - (nppes_gb + 1.5))
  # ---------------- (d) yield ----------------
  y <- merge(cm, isr[, .(issuer_id, qual_strict, qual_relaxed, state)], by = "issuer_id")
  for (nm in c("strict", "relaxed")) {
    q <- if (nm == "strict") y$qual_strict else y$qual_relaxed
    z <- y[q & in_plan_file == TRUE]
    add(paste0("yield_plans_", nm), nrow(z), "computable plans of qualifying issuers whose plan_id is in the issuer's plan file(s)")
    add(paste0("yield_issuer_states_", nm), uniqueN(z$issuer_id)); add(paste0("yield_states_", nm), uniqueN(z$state))
    ze <- z[has_enroll == TRUE]
    add(paste0("yield_plans_enrollment_", nm), nrow(ze)); add(paste0("yield_issuer_states_enrollment_", nm), uniqueN(ze$issuer_id))
  }
  add("cohort_plans_with_enrollment", sum(coh$has_enroll)); add("cohort_issuers_with_enrollment", uniqueN(coh$issuer_id[coh$has_enroll]))
  add("cohort_plans_in_any_plan_file", sum(cm$in_plan_file)); add("cohort_plans_no_pa_network_id", sum(is.na(plans[computable == TRUE, pa_network_id]) | plans[computable == TRUE, pa_network_id] == ""))
  # ---------------- orthopaedic strings in the 5 deep samples ----------------
  # reads the kept record summaries once, saves the result, then deletes them; reruns read the saved summary
  osum <- file.path(P$logs, "wp3_sample_ortho_summary.csv")
  rfs <- list.files(D_SMP, pattern = "\\.rec\\.tsv$", recursive = TRUE, full.names = TRUE)
  if (length(rfs)) {
    rr <- rbindlist(lapply(rfs, function(f) fread(f, header = FALSE, sep = "\t", quote = "", fill = TRUE, colClasses = "character", col.names = REC_COLS)))
    ind <- rr[type == "INDIVIDUAL"]
    ind[, ortho := grepl("orthopaedic", norm_txt(specialty), fixed = TRUE)]
    ind[, co_label := grepl("physician assistant|nurse|aprn|crna|surgical assistance", norm_txt(specialty))]
    ind[, single_label := !grepl("|", specialty, fixed = TRUE)]
    ind[, sub_text := grepl("hand|sports|pediatric orthopaedic|spine|trauma|foot|reconstruct", norm_txt(specialty))]
    o <- list(c("sample_ortho_text_individuals", sum(ind$ortho), "individual records whose specialty text contains orthopedic or orthopaedic"),
              c("sample_ortho_text_share", mean(ind$ortho), ""),
              c("sample_ortho_text_single_label", sum(ind$ortho & ind$single_label), ""),
              c("sample_ortho_text_with_PA_NP_co_label", sum(ind$ortho & ind$co_label), "physician assistant, nurse, APRN, CRNA or surgical assistance in the same specialty array"),
              c("sample_ortho_text_with_subspecialty_text", sum(ind$ortho & ind$sub_text), "hand, sports, pediatric orthopaedic, spine, trauma, foot, reconstruct"),
              c("sample_ortho_distinct_strings", uniqueN(ind[ortho == TRUE, specialty]), ""),
              c("sample_multi_label_share_individuals", mean(!ind$single_label), "specialty array with more than one value"))
    od <- rbindlist(lapply(o, function(x) data.table(key = x[1], value = x[2], note = x[3])))
    fwrite(od, osum)
    fwrite(ind[ortho == TRUE, .(n_records = .N), by = specialty][order(-n_records)], file.path(P$logs, "wp3_sample_ortho_strings.csv"))
    unlink(c(rfs, list.files(D_SMP, pattern = "\\.planids\\.txt$|\\.jq\\.err$", recursive = TRUE, full.names = TRUE)))
  }
  if (file.exists(osum)) { od <- fread(osum, colClasses = "character"); for (r in seq_len(nrow(od))) add(od$key[r], od$value[r], od$note[r]) }
  K <- rbindlist(K); setnames(K, "k", "key")
  fwrite(K, file.path(P$logs, "wp3_g1_numbers.csv"))
  print(K, nrows = 200)
  invisible(K)
}

# one retry (same client, after a pause) of the micro-probe rows that returned no record
stage_reprobe <- function() {
  lg <- function(...) log_msg(..., file = LOGF("probe"))
  f <- file.path(P$logs, "wp3_provider_file_probe.csv")
  fp <- fread(f, colClasses = c(plan_issuer_prefixes = "character", first_bytes = "character"))
  bad <- which(is.na(fp$n_records) | fp$n_records == 0)
  lg("reprobe: ", length(bad), " file(s) with no record in the first pass")
  if (!length(bad)) return(invisible(NULL))
  plans <- fread(file.path(P$clean_cms, "plans.csv"), colClasses = c(plan_id = "character", issuer_id = "character"), select = c("plan_id", "issuer_id"))
  puf_ids <- unique(plans$plan_id)
  nu <- load_nucc(); nu_keys <- nucc_lookup(nu)
  d <- file.path(SPIKE, "probe"); dir.create(d, showWarnings = FALSE)
  if (!"retry_note" %in% names(fp)) fp[, retry_note := NA_character_]
  for (i in bad) {
    Sys.sleep(5)
    si <- stream_inspect(fp$provider_url[i], 262144, "retry", d, excerpt_bytes = 0)
    if (isTRUE(si$gz_encoded_body)) si <- stream_inspect(fp$provider_url[i], 262144, "retry", d, excerpt_bytes = 0, compressed = TRUE)
    if (isTRUE(si$parsed) && (is.null(si$rec) || nrow(si$rec) == 0L)) si <- stream_inspect(fp$provider_url[i], 4194304, "retry", d, excerpt_bytes = 0)
    sm <- if (isTRUE(si$parsed)) summarise_records(si, puf_ids, nu_keys) else list(n_records = 0L)
    lg("  retry ", fp$provider_url[i], " -> http ", si$http, " records ", sm$n_records)
    unlink(file.path(d, paste0("retry", c(".rec.tsv", ".planids.txt", ".jq.err", ".first.bin"))))
    if (sm$n_records > 0) {
      pid_issuers <- if (!is.null(si$pids) && nrow(si$pids)) paste(sort(unique(substr(si$pids$plan_id, 1, 5))), collapse = ";") else NA_character_
      new <- as.data.table(c(list(fid = fp$fid[i], provider_url = fp$provider_url[i], prefix_tier = "256 KiB (retry after first-pass failure)", http = si$http,
                                  prefix_bytes = si$bytes, range_honored = si$range_honored, first_char = si$first_char, first_bytes = NA_character_,
                                  parsed = TRUE, gz_encoded_body = isTRUE(si$gz_encoded_body), plan_issuer_prefixes = pid_issuers), sm))
      for (cn in names(new)) if (cn %in% names(fp)) set(fp, i, cn, new[[cn]][1])
      set(fp, i, "retry_note", "first pass returned no HTTP response; retried once after 5 s")
    } else set(fp, i, "retry_note", "first pass and retry both returned no record")
  }
  fwrite(fp, f)
  lg("reprobe done; files with records: ", sum(fp$n_records > 0, na.rm = TRUE), " of ", nrow(fp))
}

stage_size1 <- function() {
  u <- commandArgs(trailingOnly = TRUE)[2]
  t0 <- proc.time()[["elapsed"]]
  r <- size_one(u)
  cat("size_one elapsed s:", round(proc.time()[["elapsed"]] - t0, 1), "\n"); str(r)
  f <- tempfile()
  t0 <- proc.time()[["elapsed"]]
  fp <- fetch_prefix(u, f, 262144)
  cat("fetch_prefix 256 KiB elapsed s:", round(proc.time()[["elapsed"]] - t0, 1), "\n"); str(fp)
  unlink(f)
}
# ---------------------------------------------------------------------
stages <- list(idx = stage_idx, plans = stage_plans, size = stage_size, plans2 = stage_plans2, sample = stage_sample, probe = stage_probe, nppes = stage_nppes, eval = stage_eval, g1 = stage_g1, reprobe = stage_reprobe, size1 = stage_size1)
a <- commandArgs(trailingOnly = TRUE)
if (length(a) < 1 || !(a[1] %in% names(stages))) {
  stop("usage: Rscript 03_ortho_spike.R <", paste(names(stages), collapse = "|"), ">", call. = FALSE)
}
stages[[a[1]]]()
