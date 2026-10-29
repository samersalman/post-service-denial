# =====================================================================
# helpers_docx.R : placeholder renderer (subset)
# =====================================================================
# This repository ships only the text-rendering part of the original helper: number
# formatting from the numbers ledger, template placeholder rendering and word counts,
# which the figure and table scripts use to check their caption text. The manuscript
# builder (Word document assembly) is not part of this repository.
# Source 00_setup.R and ledger.R first. Requires: jsonlite, readr.
#   fmt_ledger_value(value_num, value, fmt, key = "")  formats int, mil1, pct0, pct1, n1, n2, n3, p, str
#   read_refs(path)                                     reads a references JSON into id, pmid, vancouver
#   render_template(path, ledger, refs, section = NULL, cite_state = NULL)
#   word_count(rendered)
#   new_cite_state()
# Keep this file ASCII.
# =====================================================================

.HD_FONT <- "Times New Roman"
.HD_FMTS <- c("int", "mil1", "pct0", "pct1", "n1", "n2", "n3", "p", "str")
.HD_NS <- c(
  w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main",
  wp = "http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
)
.hd_emdash <- function() intToUtf8(0x2014)
.hd_minus <- function() intToUtf8(0x2212)

# ---- number formatting -------------------------------------------------
.hd_round_half_up <- function(x, digits) {
  z <- abs(x) * 10^digits
  sign(x) * floor(z + 0.5 + 1e-9) / 10^digits
}

.hd_fixed <- function(x, digits, big = FALSE) {
  r <- .hd_round_half_up(x, digits)
  if (r == 0) r <- 0
  s <- formatC(abs(r), format = "f", digits = digits, big.mark = if (big) "," else "")
  if (r < 0) paste0(.hd_minus(), s) else s
}

fmt_ledger_value <- function(value_num, value, fmt, key = "") {
  who <- if (nzchar(key)) paste0("key '", key, "': ") else ""
  if (!is.character(fmt) || length(fmt) != 1L || is.na(fmt) || !(fmt %in% .HD_FMTS)) {
    stop(who, "unknown format '", paste(fmt, collapse = ","), "' (allowed: ", paste(.HD_FMTS, collapse = ", "), ")", call. = FALSE)
  }
  if (identical(fmt, "str")) {
    if (is.null(value) || length(value) != 1L || is.na(value) || !nzchar(value)) stop(who, "empty or NA value for fmt str", call. = FALSE)
    return(as.character(value))
  }
  if (is.null(value_num) || length(value_num) != 1L || is.na(value_num) || !is.finite(value_num)) {
    stop(who, "value '", paste(value, collapse = ""), "' is not a number but fmt '", fmt, "' needs one", call. = FALSE)
  }
  x <- value_num
  switch(fmt,
    int = .hd_fixed(x, 0, big = TRUE),
    mil1 = .hd_fixed(x / 1e6, 1),
    pct0 = , pct1 = {
      if (abs(x) > 10) stop(who, "value ", x, " is too large for a proportion (expected 0-1; is it a count or already a percent?)", call. = FALSE)
      paste0(.hd_fixed(x * 100, if (fmt == "pct0") 0 else 1), "%")
    },
    n1 = .hd_fixed(x, 1),
    n2 = .hd_fixed(x, 2),
    n3 = .hd_fixed(x, 3),
    p = {
      if (x < 0 || x > 1) stop(who, "P value ", x, " is outside 0-1", call. = FALSE)
      if (x < 0.001) {
        "<.001"
      } else {
        r <- .hd_round_half_up(x, 3)
        if (r >= 1) ">.999" else sub("^0", "", sprintf("%.3f", r))
      }
    }
  )
}

# ---- references --------------------------------------------------------
.hd_ref_tibble <- function(id, pmid, vancouver) {
  data.frame(id = as.character(id), pmid = as.character(pmid), vancouver = as.character(vancouver),
             stringsAsFactors = FALSE)
}

read_refs <- function(path) {
  if (!file.exists(path)) stop("read_refs(): file not found: ", path, call. = FALSE)
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("package 'jsonlite' is required")
  j <- jsonlite::read_json(path, simplifyVector = FALSE)
  arr <- if (!is.null(names(j))) {
    if (!is.null(j$references)) j$references else if (!is.null(j$candidates)) j$candidates else
      stop("read_refs(): object has neither 'references' nor 'candidates'", call. = FALSE)
  } else j
  get1 <- function(e, k) { v <- e[[k]]; if (is.null(v) || !length(v)) NA_character_ else as.character(v[[1]]) }
  .hd_ref_tibble(vapply(arr, get1, character(1), k = "id"),
                 vapply(arr, get1, character(1), k = "pmid"),
                 vapply(arr, get1, character(1), k = "vancouver"))
}

.hd_as_refs <- function(refs) {
  if (is.character(refs) && length(refs) == 1L) refs <- read_refs(refs)
  if (!is.data.frame(refs) || !all(c("id", "pmid", "vancouver") %in% names(refs))) {
    stop("refs must be a data.frame with columns id, pmid, vancouver (or a path to the JSON)", call. = FALSE)
  }
  refs <- .hd_ref_tibble(refs$id, refs$pmid, refs$vancouver)
  bad <- is.na(refs$id) | !nzchar(refs$id) | is.na(refs$vancouver) | !nzchar(trimws(refs$vancouver)) |
    is.na(refs$pmid) | !grepl("^[0-9]+$", refs$pmid)
  if (any(bad)) {
    stop("refs: entry ", paste(which(bad), collapse = ","), " lacks an id, a vancouver string or a numeric PMID", call. = FALSE)
  }
  if (anyDuplicated(refs$id)) stop("refs: duplicated id(s): ", paste(unique(refs$id[duplicated(refs$id)]), collapse = ", "), call. = FALSE)
  refs
}

.hd_ref_line <- function(vancouver, pmid) {
  v <- trimws(vancouver)
  if (!grepl("PMID", v, fixed = TRUE)) {
    if (!grepl("[.]$", v)) v <- paste0(v, ".")
    v <- paste0(v, " PMID: ", pmid)
  }
  v
}

new_cite_state <- function() {
  e <- new.env(parent = emptyenv())
  e$ids <- character(0)
  e
}

# 2 consecutive numbers stay a comma list; 3 or more consecutive become a range
.hd_collapse_cites <- function(nums) {
  n <- sort(unique(as.integer(nums)))
  out <- character(0)
  i <- 1L
  while (i <= length(n)) {
    j <- i
    while (j < length(n) && n[j + 1L] == n[j] + 1L) j <- j + 1L
    if (j - i >= 2L) {
      out <- c(out, paste0(n[i], "-", n[j]))
    } else {
      out <- c(out, as.character(n[i:j]))
    }
    i <- j + 1L
  }
  paste(out, collapse = ",")
}

# ---- ledger access -----------------------------------------------------
.hd_as_ledger <- function(ledger) {
  if (is.character(ledger) && length(ledger) == 1L) ledger <- ledger_read_merged(ledger)
  if (!is.data.frame(ledger) || !all(c("key", "value", "value_num") %in% names(ledger))) {
    stop("ledger must be the data.frame from ledger_read_merged() (or a path to the merged CSV)", call. = FALSE)
  }
  ledger
}

# ---- text rendering ----------------------------------------------------
.hd_ph_re <- "\\{\\{[^{}]*\\}\\}"
.hd_ph_inner_re <- "^\\s*([a-z][a-z0-9_]*)\\s*(\\|\\s*([A-Za-z0-9_]+)\\s*)?$"

.hd_check_text_chars <- function(text, where) {
  if (grepl(.hd_emdash(), text, fixed = TRUE)) stop(where, ": contains U+2014 (em-dash)", call. = FALSE)
  if (grepl("[\\x01-\\x08\\x0B\\x0C\\x0E-\\x1F]", text, perl = TRUE)) stop(where, ": contains a control character", call. = FALSE)
  invisible(TRUE)
}

# returns list(text, trace) ; trace rows carry paragraph_index and section
.hd_render_text <- function(text, ledger, refs, cite, section, par_index, where, allow_ph = TRUE) {
  .hd_check_text_chars(text, where)
  text <- gsub("[ \t]+", " ", text)
  tr <- NULL
  m <- gregexpr(.hd_ph_re, text)
  ph <- regmatches(text, m)[[1]]
  if (length(ph)) {
    if (!allow_ph) stop(where, ": placeholders are not allowed here: ", ph[1], call. = FALSE)
    rend <- character(length(ph)); keys <- character(length(ph)); fmts <- character(length(ph))
    for (i in seq_along(ph)) {
      inner <- sub("^\\{\\{(.*)\\}\\}$", "\\1", ph[i])
      mm <- regmatches(inner, regexec(.hd_ph_inner_re, inner))[[1]]
      if (!length(mm)) stop(where, ": malformed placeholder ", ph[i], " (use {{key}} or {{key|fmt}}, key lowercase snake_case)", call. = FALSE)
      key <- mm[2]
      fmt <- if (nzchar(mm[4])) mm[4] else NA_character_
      idx <- match(key, ledger$key)
      if (is.na(idx)) stop(where, ": key not in ledger: ", key, call. = FALSE)
      if (is.na(fmt)) {
        if (!is.na(ledger$value_num[idx])) {
          stop(where, ": {{", key, "}} has a numeric value and needs a format, e.g. {{", key, "|int}} ({{", key,
               "|str}} prints the stored text)", call. = FALSE)
        }
        fmt <- "str"
      }
      rend[i] <- tryCatch(fmt_ledger_value(ledger$value_num[idx], ledger$value[idx], fmt, key),
                          error = function(e) stop(where, ": ", conditionMessage(e), call. = FALSE))
      keys[i] <- key; fmts[i] <- fmt
    }
    regmatches(text, m) <- list(rend)
    tr <- data.frame(paragraph_index = par_index, section = section, key = keys, fmt = fmts, rendered = rend,
                     stringsAsFactors = FALSE)
  }
  # citations: a run of adjacent [@...] groups becomes one bracket
  cre <- "\\s*(\\[@[^\\]]*\\])+"
  cm <- gregexpr(cre, text, perl = TRUE)
  cg <- regmatches(text, cm)[[1]]
  if (length(cg)) {
    cites <- character(length(cg))
    for (i in seq_along(cg)) {
      groups <- regmatches(cg[i], gregexpr("\\[@[^\\]]*\\]", cg[i], perl = TRUE))[[1]]
      ids <- unlist(lapply(groups, function(g) {
        inner <- sub("^\\[(.*)\\]$", "\\1", g)
        parts <- trimws(strsplit(inner, "[;,]")[[1]])
        parts <- parts[nzchar(parts)]
        if (!length(parts) || !all(grepl("^@[A-Za-z0-9_.:-]+$", parts))) {
          stop(where, ": malformed citation ", g, " (use [@refid] or [@id1; @id2])", call. = FALSE)
        }
        sub("^@", "", parts)
      }))
      unk <- setdiff(ids, refs$id)
      if (length(unk)) {
        stop(where, ": refid not in refs: ", paste(unk, collapse = ", "), " (known: ",
             paste(utils::head(refs$id, 12), collapse = ", "), if (nrow(refs) > 12) ", ...", ")", call. = FALSE)
      }
      nums <- integer(0)
      for (id in ids) {
        k <- match(id, cite$ids)
        if (is.na(k)) { cite$ids <- c(cite$ids, id); k <- length(cite$ids) }
        nums <- c(nums, k)
      }
      cites[i] <- paste0("[", .hd_collapse_cites(nums), "]")
    }
    regmatches(text, cm) <- list(cites)
    text <- gsub("(\\[[0-9]+(?:[,-][0-9]+)*\\])(?=[A-Za-z0-9])", "\\1 ", text, perl = TRUE)
  }
  text <- trimws(text)
  if (grepl("{{", text, fixed = TRUE) || grepl("}}", text, fixed = TRUE) || grepl("[@", text, fixed = TRUE)) {
    stop(where, ": unresolved placeholder or citation left in output: ", substr(text, 1, 120), call. = FALSE)
  }
  list(text = text, trace = tr)
}

.hd_parse_template <- function(path) {
  if (!file.exists(path)) stop("template not found: ", path, call. = FALSE)
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines <- sub("\r$", "", lines)
  if (length(lines)) lines[1] <- sub(intToUtf8(0xFEFF), "", lines[1], fixed = TRUE)
  items <- list(); cur <- character(0)
  flush <- function() {
    if (length(cur)) items[[length(items) + 1L]] <<- list(type = "para", text = paste(trimws(cur), collapse = " "))
    cur <<- character(0)
  }
  for (k in seq_along(lines)) {
    ln <- lines[k]
    if (!nzchar(trimws(ln))) { flush(); next }
    if (grepl("^\\s*#", ln)) stop(basename(path), " line ", k, ": a line starting with # is not allowed (use @@ for a sub-heading)", call. = FALSE)
    if (grepl("^@@", ln)) {
      flush()
      txt <- trimws(sub("^@@", "", ln))
      if (!nzchar(txt)) stop(basename(path), " line ", k, ": empty sub-heading after @@", call. = FALSE)
      items[[length(items) + 1L]] <- list(type = "heading", text = txt)
      next
    }
    cur <- c(cur, ln)
  }
  flush()
  items
}

render_template <- function(path, ledger, refs, section = NULL, cite_state = NULL) {
  ledger <- .hd_as_ledger(ledger)
  refs <- .hd_as_refs(refs)
  if (is.null(cite_state)) cite_state <- new_cite_state()
  if (is.null(section)) section <- tools::toTitleCase(sub("\\.[^.]*$", "", basename(path)))
  items <- .hd_parse_template(path)
  if (!length(items)) stop(basename(path), ": template is empty", call. = FALSE)
  types <- character(length(items)); texts <- character(length(items)); pidx <- rep(NA_integer_, length(items))
  traces <- list(); n_body <- 0L
  for (i in seq_along(items)) {
    it <- items[[i]]
    if (it$type == "para") n_body <- n_body + 1L
    where <- sprintf("%s paragraph %d", basename(path), i)
    r <- .hd_render_text(it$text, ledger, refs, cite_state, section, if (it$type == "para") n_body else NA_integer_,
                         where, allow_ph = (it$type == "para"))
    if (it$type == "heading" && grepl("\\[[0-9]", r$text)) stop(where, ": citations are not allowed in sub-headings", call. = FALSE)
    types[i] <- it$type; texts[i] <- r$text
    if (it$type == "para") pidx[i] <- n_body
    if (!is.null(r$trace)) traces[[length(traces) + 1L]] <- r$trace
  }
  trace <- if (length(traces)) do.call(rbind, traces) else
    data.frame(paragraph_index = integer(0), section = character(0), key = character(0), fmt = character(0),
               rendered = character(0), stringsAsFactors = FALSE)
  rownames(trace) <- NULL
  list(paragraphs = data.frame(type = types, text = texts, paragraph_index = pidx, stringsAsFactors = FALSE),
       trace = trace, cited = cite_state$ids, section = section)
}

# ---- word count --------------------------------------------------------
.hd_body_texts <- function(x) {
  if (is.character(x)) return(x)
  if (is.data.frame(x)) {
    if (!all(c("type", "text") %in% names(x))) stop("word_count(): data.frame needs columns type and text", call. = FALSE)
    return(x$text[x$type == "para"])
  }
  if (is.list(x) && !is.null(x$paragraphs)) return(.hd_body_texts(x$paragraphs))
  if (is.list(x)) return(unlist(lapply(x, .hd_body_texts), use.names = FALSE))
  stop("word_count(): unsupported input", call. = FALSE)
}

.HD_CITE_STRIP_RE <- "\\s*\\[[0-9]+([,-][0-9]+)*\\]"

word_count <- function(rendered) {
  txt <- .hd_body_texts(rendered)
  count <- function(x) {
    tok <- unlist(strsplit(trimws(x), "\\s+"))
    sum(nzchar(tok))
  }
  c(without_citations = count(gsub(.HD_CITE_STRIP_RE, "", txt)), with_citations = count(txt))
}
