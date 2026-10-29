# =====================================================================
# 01_download_cms.R : WP2 step 1. Discover and download the CMS PY2026 files.
# =====================================================================
# 1. curl the CMS Exchange PUF page, list every candidate link (all years), show
#    the PY2026 choice, and check every URL in config/cms_sources.yaml is present
#    under the "2026 Exchange PUF Datasets" block. Stops if one is missing.
# 2. Download each file with curl to v1/data_raw/cms_py2026/ (zips and PDFs),
#    save HTTP headers, write provenance/manifest.csv, extract zip members
#    (members over 5 MB go to the cache, not to v1), hash everything, chmod a-w.
# 3. Re-runnable: a raw file that already exists is NOT downloaded again; its
#    hash must match the manifest or the script stops.
#
# Outputs
#   v1/data_raw/cms_py2026/*.zip, *.pdf, extracted/<zip stem>/<member>
#   v1/data_raw/kff_2024/2024-kff-transparency-data-working-file.xlsx
#   v1/data_raw/provenance/manifest.csv   file, source_url, retrieved_utc, http_status, bytes, sha256
#   v1/data_raw/provenance/http_headers.csv   file, last_modified, content_type, content_length, etag
#   v1/logs/wp2_download.log, v1/logs/wp2_candidates.csv
# File names in the manifest are relative to v1/data_raw; a member extracted to the
# cache is written as "CACHE/<relative path under the cache>".
# =====================================================================
.args <- commandArgs(trailingOnly = FALSE)
.f <- sub("^--file=", "", grep("^--file=", .args, value = TRUE)[1])
.here <- dirname(normalizePath(gsub("~+~", " ", .f, fixed = TRUE)))
source(file.path(.here, "00_setup.R"))
source(file.path(.here, "ledger.R"))
ensure_pkgs(c("digest", "yaml", "readr"))

LOG <- file.path(P$logs, "wp2_download.log")
lg <- function(..., level = "INFO") log_msg(..., level = level, file = LOG)
UA <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15"
CURL <- "/usr/bin/curl"
FREE_FLOOR_GB <- load_config()$gates$free_disk_floor_gb

cfg <- load_config()$cms_sources
raw_dir <- file.path(P$data_raw, cfg$download_dir)
ext_dir <- file.path(raw_dir, "extracted")
cache_ext <- file.path(P$cache, cfg$download_dir, "extracted")
prov_dir <- P$provenance
for (d in c(raw_dir, ext_dir, cache_ext, prov_dir)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

free_gb <- function() {
  o <- system2("/bin/df", shQuote(c("-k", P$cache)), stdout = TRUE)
  as.numeric(strsplit(trimws(o[length(o)]), "[[:space:]]+")[[1]][4]) / 1024^2
}
utc_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

curl_get <- function(url, out, hdr) {
  args <- c("-sS", "-L", "--fail-with-body", "--max-time", "180", "-A", UA,
            "-D", hdr, "-o", out, "-w", "%{http_code}", url)
  r <- suppressWarnings(system2(CURL, shQuote(args), stdout = TRUE, stderr = TRUE))
  code <- suppressWarnings(as.integer(sub(".*?([0-9]{3})$", "\\1", paste(r, collapse = ""))))
  list(code = code, raw = r)
}

last_header_block <- function(hdr_file) {
  x <- readLines(hdr_file, warn = FALSE)
  starts <- grep("^HTTP/", x)
  x[starts[length(starts)]:length(x)]
}
get_h <- function(blk, name) {
  m <- grep(paste0("^", name, ":"), blk, ignore.case = TRUE, value = TRUE)
  if (!length(m)) return(NA_character_)
  trimws(sub("^[^:]+:", "", m[length(m)]))
}

# ---- step 1: discover on the live page ---------------------------------
lg("free disk GB at start: ", round(free_gb(), 1))
page_html <- file.path(P$cache, "cms_puf_page.html")
page_hdr <- file.path(P$cache, "cms_puf_page.hdr")
r <- curl_get(cfg$page_url, page_html, page_hdr)
if (!identical(r$code, 200L)) stop("CMS PUF page not retrievable: HTTP ", r$code)
html <- paste(readLines(page_html, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
lg("fetched ", cfg$page_url, " HTTP ", r$code, ", ", nchar(html), " characters")

hrefs <- regmatches(html, gregexpr('href="[^"]+"', html))[[1]]
hrefs <- sub('^href="', "", sub('"$', "", hrefs))
zips <- unique(grep("download\\.cms\\.gov/marketplace-puf/[0-9]{4}/[a-z-]+\\.zip$", hrefs, value = TRUE))
cand <- data.frame(
  year = as.integer(sub(".*/marketplace-puf/([0-9]{4})/.*", "\\1", zips)),
  dataset = sub(".*/([a-z-]+)\\.zip$", "\\1", zips),
  url = zips, stringsAsFactors = FALSE
)
cand <- cand[order(cand$dataset, cand$year), ]

# "Updated <date>" label that precedes each 2026 link in the page text
upd <- vapply(cand$url, function(u) {
  pos <- regexpr(u, html, fixed = TRUE)
  if (pos < 0) return(NA_character_)
  seg <- substr(html, max(1, pos - 700), pos)
  seg <- gsub("<[^>]+>", " ", seg)
  seg <- gsub("&nbsp;", " ", seg, fixed = TRUE)
  m <- regmatches(seg, gregexpr("\\(Updated [A-Za-z]+ [0-9]{1,2}, [0-9]{4}\\)", seg))[[1]]
  if (length(m)) sub("^\\(Updated (.*)\\)$", "\\1", m[length(m)]) else NA_character_
}, character(1), USE.NAMES = FALSE)
cand$page_updated <- ifelse(cand$year == cfg$plan_year, upd, NA_character_)
cand$chosen <- cand$year == cfg$plan_year & cand$url %in% vapply(cfg$files, function(z) z$url, character(1))
readr::write_csv(cand, file.path(P$logs, "wp2_candidates.csv"))

tic <- cand[cand$dataset == "transparency-in-coverage-puf", ]
lg("Transparency in Coverage PUF candidates on the page (", nrow(tic), " years): ",
   paste0(tic$year, collapse = ", "))
lg("CHOSEN for plan year ", cfg$plan_year, " (data year ", cfg$data_year, "): ",
   tic$url[tic$year == cfg$plan_year], " (page says Updated ", tic$page_updated[tic$year == cfg$plan_year], ")")
for (i in which(cand$year == cfg$plan_year)) {
  lg("  PY", cand$year[i], " ", cand$dataset[i], " -> ", cand$url[i], " | Updated ", cand$page_updated[i],
     " | in config: ", cand$chosen[i])
}

for (z in c(cfg$files, cfg$benchmark_files)) {
  if (z$kind == "zip") {
    if (!(z$url %in% cand$url[cand$year == cfg$plan_year])) {
      stop("Config URL not found under the PY", cfg$plan_year, " block of the live page: ", z$url)
    }
    if (!grepl(paste0("/marketplace-puf/", cfg$plan_year, "/"), z$url, fixed = TRUE)) {
      stop("Wrong-year URL in config: ", z$url)
    }
  } else if (z$kind == "pdf") {
    path <- sub("^https://www\\.cms\\.gov", "", z$url)
    if (!any(hrefs == path) || !grepl("py26", z$url)) stop("PDF link not on the live page or not a py26 file: ", z$url)
  }
}
lg("all configured CMS URLs are present on the live page under PY", cfg$plan_year)

# ---- step 2: download ---------------------------------------------------
man_path <- file.path(prov_dir, "manifest.csv")
hdr_path <- file.path(prov_dir, "http_headers.csv")
MAN_COLS <- c("file", "source_url", "retrieved_utc", "http_status", "bytes", "sha256")
old <- if (file.exists(man_path)) utils::read.csv(man_path, colClasses = "character", stringsAsFactors = FALSE) else
  as.data.frame(setNames(replicate(length(MAN_COLS), character(0), simplify = FALSE), MAN_COLS))
oldh <- if (file.exists(hdr_path)) utils::read.csv(hdr_path, colClasses = "character", stringsAsFactors = FALSE) else
  data.frame(file = character(0), last_modified = character(0), content_type = character(0),
             content_length = character(0), etag = character(0), stringsAsFactors = FALSE)
man <- list(); hdrs <- list()

abs_of <- function(rel) if (startsWith(rel, "CACHE/")) file.path(P$cache, sub("^CACHE/", "", rel)) else file.path(P$data_raw, rel)

download_one <- function(url, dest_rel) {
  dest <- abs_of(dest_rel)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(dest)) {
    prev <- old[old$file == dest_rel, ]
    h <- sha256_file(dest)[[1]]
    if (!nrow(prev) || !identical(prev$sha256[1], h)) {
      stop("Existing raw file has no manifest row or a different hash (raw data must stay unmodified): ", dest_rel)
    }
    lg("SKIP (already downloaded, hash matches manifest): ", dest_rel)
    man[[dest_rel]] <<- prev[1, MAN_COLS]
    ph <- oldh[oldh$file == dest_rel, ]
    if (nrow(ph)) hdrs[[dest_rel]] <<- ph[1, ]
    return(invisible(dest))
  }
  if (free_gb() < FREE_FLOOR_GB + 1) stop("Free disk below floor, aborting: ", round(free_gb(), 1), " GB")
  tmp <- paste0(dest, ".part"); hf <- paste0(dest, ".hdr.tmp")
  t0 <- utc_now()
  r <- curl_get(url, tmp, hf)
  if (!identical(r$code, 200L)) { unlink(c(tmp, hf)); stop("Download failed HTTP ", r$code, ": ", url) }
  blk <- last_header_block(hf)
  file.rename(tmp, dest)
  man[[dest_rel]] <<- data.frame(file = dest_rel, source_url = url, retrieved_utc = t0, http_status = as.character(r$code),
                                 bytes = as.character(file.size(dest)), sha256 = sha256_file(dest)[[1]],
                                 stringsAsFactors = FALSE)
  hdrs[[dest_rel]] <<- data.frame(file = dest_rel, last_modified = get_h(blk, "last-modified"),
                                  content_type = get_h(blk, "content-type"), content_length = get_h(blk, "content-length"),
                                  etag = get_h(blk, "etag"), stringsAsFactors = FALSE)
  unlink(hf)
  lg("downloaded ", dest_rel, " HTTP ", r$code, " ", file.size(dest), " bytes, Last-Modified ", hdrs[[dest_rel]]$last_modified)
  invisible(dest)
}

for (z in cfg$files) {
  rel <- file.path(cfg$download_dir, basename(z$url))
  download_one(z$url, rel)
}
for (z in cfg$benchmark_files) {
  download_one(z$url, file.path(z$download_dir, basename(z$url)))
}

# ---- extract zip members, hash them ------------------------------------
LIMIT <- cfg$max_file_mb_in_v1 * 1024^2
for (z in cfg$files) {
  if (z$kind != "zip") next
  zpath <- file.path(raw_dir, basename(z$url))
  stem <- sub("\\.zip$", "", basename(z$url))
  lst <- system2("/usr/bin/unzip", shQuote(c("-Z", "-l", zpath)), stdout = TRUE)  # zipinfo long listing
  mem_lines <- grep("^-", lst, value = TRUE)
  mem_names <- vapply(strsplit(mem_lines, "[[:space:]]+"), function(p) paste(p[10:length(p)], collapse = " "), character(1))
  mem_sizes <- vapply(strsplit(mem_lines, "[[:space:]]+"), function(p) as.numeric(p[4]), numeric(1))
  if (!(z$member %in% mem_names) || length(mem_names) != 1L) {
    stop("Unexpected members in ", basename(zpath), ": ", paste(mem_names, collapse = "; "))
  }
  to_cache <- mem_sizes[1] > LIMIT
  rel <- if (to_cache) file.path("CACHE", cfg$download_dir, "extracted", stem, z$member) else
    file.path(cfg$download_dir, "extracted", stem, z$member)
  dest <- abs_of(rel)
  if (file.exists(dest)) {
    prev <- old[old$file == rel, ]
    if (!nrow(prev) || !identical(prev$sha256[1], sha256_file(dest)[[1]])) {
      stop("Extracted file exists with no manifest row or a different hash: ", rel)
    }
    man[[rel]] <- prev[1, MAN_COLS]
    lg("SKIP extracted (hash matches manifest): ", rel)
    next
  }
  if (free_gb() < FREE_FLOOR_GB + 1) stop("Free disk below floor, aborting")
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  rc <- system2("/usr/bin/unzip", shQuote(c("-o", "-q", zpath, z$member, "-d", dirname(dest))))
  if (rc != 0) stop("unzip failed for ", zpath)
  if (!file.exists(dest)) stop("extracted member not found: ", dest)
  man[[rel]] <- data.frame(file = rel, source_url = paste0(z$url, "#", z$member), retrieved_utc = utc_now(),
                           http_status = "from_zip", bytes = as.character(file.size(dest)),
                           sha256 = sha256_file(dest)[[1]], stringsAsFactors = FALSE)
  lg("extracted ", rel, " (", file.size(dest), " bytes", if (to_cache) ", over the 5 MB limit so kept in the cache" else "", ")")
}

man_df <- do.call(rbind, man)
man_df <- man_df[order(man_df$file), MAN_COLS]
rownames(man_df) <- NULL
readr::write_csv(man_df, man_path)
hdr_df <- do.call(rbind, hdrs)
if (!is.null(hdr_df)) { hdr_df <- hdr_df[order(hdr_df$file), ]; rownames(hdr_df) <- NULL; readr::write_csv(hdr_df, hdr_path) }
lg("manifest rows: ", nrow(man_df), " -> ", man_path)

# ---- immutability ------------------------------------------------------
raw_files <- list.files(P$data_raw, recursive = TRUE, full.names = TRUE, all.files = FALSE)
raw_files <- raw_files[!grepl("/provenance/", raw_files) & !grepl("\\.(part|tmp)$", raw_files)]
for (f in raw_files) Sys.chmod(f, mode = "0444")
cf <- file.path(P$cache, sub("^CACHE/", "", man_df$file[startsWith(man_df$file, "CACHE/")]))
for (f in cf) Sys.chmod(f, mode = "0444")
lg("chmod a-w on ", length(raw_files) + length(cf), " raw files")
lg("free disk GB at end: ", round(free_gb(), 1))
lg("DONE")
