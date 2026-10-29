# =====================================================================
# ledger.R : numbers ledger helper (WP0; merged-ledger schema pinned in F1)
# =====================================================================
# Every number reported in the manuscript passes through the ledger.
# Source 00_setup.R first (this file only needs P at call time, and only
# when a path/dir argument is left at its default).
#
# ---- 1. Part files (one per producing script) ------------------------
# V1/results/ledger_parts/ledger_<part>.csv (CSV, UTF-8). Columns:
#   key,value,unit,description,source_script
#   key           lowercase snake_case, stage prefix by convention:
#                 cohort_ kff_ att_ hetero_ reason_ ortho_ model_ sens_ design_
#                 (a key without one of these prefixes gives a warning)
#   value         numeric stored UNFORMATTED (proportions 0-1, counts as
#                 integers, P-values raw) or non-empty text. Never NA.
#                 Forbidden values (error at write AND at read): the strings
#                 "NA", "NaN", "Inf", "-Inf" in any case (also +Inf and
#                 Infinity), and ID-like strings with a leading zero and more
#                 than one digit, such as "0123" or "007" (ledger_get() turns
#                 anything that parses as a number into a number, so "0123"
#                 would silently become 123). Store such an ID as text that does
#                 not parse as a number (for example "ID 0123"), or do not put
#                 it in the ledger. "0", "0.5" and "0.05" are fine.
#   unit          free text, e.g. "claims", "proportion", "count", "P value"
#   description   one line, plain words, no em-dash
#   source_script name of the script that produced it
#
# ---- 2. Merged ledger (one file, the reader's input) -----------------
# V1/results/numbers_ledger.csv, written ONLY by ledger_write_merged() (called by
# R/08_merge_ledger.R) and read by the renderer and verifier through
# ledger_read_merged(). Columns, in this order, all text:
#   key,value,unit,description,source_script,part
# i.e. the part-file columns plus part (the part name; "design" for rows that
# come from config/design_constants.csv). value is written as TEXT exactly as
# stored in the part file; numeric parsing happens at read time (value_num).
# Keys are unique across the whole file. This schema is defined once, here:
# .LEDGER_MERGED_COLS.
#
# ---- 3. API ----------------------------------------------------------
#   ledger_add(part, key, value, unit = "", description = "", source_script = "")
#       collect one row in memory (environment) under <part>. Only the shape of
#       the arguments is checked here; content checks happen at write time.
#   ledger_write(part, dir = NULL)
#       validate and write <dir>/ledger_<part>.csv (dir defaults to
#       P$ledger_parts). Errors, listing every problem, on: duplicate key
#       within the part, non-snake_case key, NA / non-finite / empty value,
#       NA/NaN/Inf text, leading-zero ID-like text, bad part name, U+2014 in
#       text fields. Returns the path invisibly.
#   ledger_reset(part = NULL)
#       forget collected rows (one part, or all). Call at the top of a script
#       that is re-run in one session.
#   ledger_read_all(dir = NULL, design_csv = NULL)
#       read every ledger_*.csv in dir (default P$ledger_parts) plus, if the
#       file exists, config/design_constants.csv (default
#       file.path(P$config, "design_constants.csv")) as part "design". Errors
#       on a duplicate key across parts, an empty value, NA/NaN/Inf text or a
#       leading-zero ID-like value (in any part, design included). Returns a
#       data.frame with columns key, value (character, exactly as stored),
#       value_num (numeric or NA when the value is not a number), unit,
#       description, source_script, part.
#   ledger_write_merged(path = file.path(P$results, "numbers_ledger.csv"),
#                       dir = NULL, design_csv = NULL)
#       calls ledger_read_all(dir, design_csv) (so every check above, and the
#       duplicate-key error, apply), then writes the merged file (section 2).
#       Returns the path invisibly.
#   ledger_read_merged(path = file.path(P$results, "numbers_ledger.csv"))
#       read the merged file; checks that the columns are exactly the six above,
#       and re-applies the key, value and duplicate checks. Returns the same
#       structure as ledger_read_all (key, value, value_num, unit, description,
#       source_script, part).
#   ledger_get(ledger, key)
#       one value from the output of ledger_read_all() or ledger_read_merged():
#       numeric if the stored value parses as a number, otherwise character.
#       Errors on a missing key.
#
# Keep this file ASCII.
# =====================================================================

.ledger_env <- new.env(parent = emptyenv())
.ledger_env$parts <- list()

.LEDGER_COLS <- c("key", "value", "unit", "description", "source_script")
.LEDGER_MERGED_COLS <- c(.LEDGER_COLS, "part")
.LEDGER_PREFIXES <- c("cohort_", "kff_", "att_", "hetero_", "reason_", "ortho_",
                      "model_", "sens_", "design_")
.LEDGER_KEY_RE <- "^[a-z][a-z0-9]*(_[a-z0-9]+)*$"
.LEDGER_PART_RE <- "^[a-z][a-z0-9_]*$"

.ledger_emdash <- function() intToUtf8(0x2014)

ledger_add <- function(part, key, value, unit = "", description = "", source_script = "") {
  if (!is.character(part) || length(part) != 1L || is.na(part)) stop("ledger_add(): part must be one string", call. = FALSE)
  if (!is.character(key) || length(key) != 1L || is.na(key)) stop("ledger_add(): key must be one string", call. = FALSE)
  if (length(value) != 1L) stop("ledger_add(): value for '", key, "' must have length 1", call. = FALSE)
  if (is.factor(value)) value <- as.character(value)
  if (!(is.numeric(value) || is.character(value) || is.logical(value))) {
    stop("ledger_add(): value for '", key, "' must be numeric or character", call. = FALSE)
  }
  for (a in list(unit, description, source_script)) {
    if (!is.character(a) || length(a) != 1L) stop("ledger_add(): unit, description, source_script must be single strings", call. = FALSE)
  }
  row <- list(key = key, value = value, unit = unit, description = description, source_script = source_script)
  .ledger_env$parts[[part]] <- c(.ledger_env$parts[[part]], list(row))
  invisible(NULL)
}

ledger_reset <- function(part = NULL) {
  if (is.null(part)) .ledger_env$parts <- list() else .ledger_env$parts[[part]] <- NULL
  invisible(NULL)
}

.ledger_value_string <- function(v) {
  if (is.numeric(v)) sprintf("%.15g", v) else as.character(v)
}

# Content checks on stored value text (character vector, no NA). Returns two
# logical vectors:
#   nonvalue      "NA", "NaN", "Inf", "-Inf" in any case (and anything else that
#                 parses to a non-finite number, e.g. "+Inf", "Infinity")
#   leading_zero  parses as a finite number but starts with a zero followed by
#                 another digit ("0123", "007", "-01"), i.e. an ID-like string
#                 that ledger_get() would silently turn into a number
.ledger_value_issues <- function(v) {
  t <- trimws(v)
  num <- suppressWarnings(as.numeric(t))
  list(
    nonvalue = tolower(t) %in% c("na", "nan", "inf", "-inf") | (!is.na(num) & !is.finite(num)),
    leading_zero = !is.na(num) & is.finite(num) & grepl("^[+-]?0[0-9]", t)
  )
}

# Returns character vector of problems (empty = valid) for a data frame with
# the ledger columns (value as stored in R, so NA is detectable).
.ledger_problems <- function(keys, values, texts) {
  prob <- character(0)
  bad_key <- is.na(keys) | !grepl(.LEDGER_KEY_RE, keys)
  if (any(bad_key)) prob <- c(prob, paste0("key not lowercase snake_case: ", paste(sQuote(keys[bad_key], q = FALSE), collapse = ", ")))
  dups <- unique(keys[duplicated(keys)])
  if (length(dups)) prob <- c(prob, paste0("duplicate key(s): ", paste(dups, collapse = ", ")))
  bad_val <- vapply(values, function(v) {
    if (is.logical(v)) return(TRUE)
    if (is.na(v)) return(TRUE)
    if (is.numeric(v)) return(!is.finite(v))
    !nzchar(trimws(v))
  }, logical(1))
  if (any(bad_val)) prob <- c(prob, paste0("NA, non-finite, logical or empty value for key(s): ", paste(keys[bad_val], collapse = ", ")))
  is_chr <- !bad_val & vapply(values, is.character, logical(1))
  if (any(is_chr)) {
    vi <- .ledger_value_issues(vapply(values[is_chr], function(v) v, character(1)))
    if (any(vi$nonvalue)) prob <- c(prob, paste0("value is the text NA, NaN, Inf or -Inf for key(s): ", paste(keys[is_chr][vi$nonvalue], collapse = ", ")))
    if (any(vi$leading_zero)) prob <- c(prob, paste0("ID-like value with a leading zero (ledger_get would turn it into a number) for key(s): ", paste(keys[is_chr][vi$leading_zero], collapse = ", ")))
  }
  if (any(grepl(.ledger_emdash(), texts, fixed = TRUE))) prob <- c(prob, "U+2014 (em-dash) found in a value, unit, description or source_script")
  prob
}

ledger_write <- function(part, dir = NULL) {
  if (!is.character(part) || length(part) != 1L || !grepl(.LEDGER_PART_RE, part)) {
    stop("ledger_write(): part must be lowercase letters, digits, underscores (got ", deparse(part), ")", call. = FALSE)
  }
  rows <- .ledger_env$parts[[part]]
  if (is.null(rows) || !length(rows)) stop("ledger_write(): nothing collected for part '", part, "'", call. = FALSE)
  keys <- vapply(rows, function(r) r$key, character(1))
  values <- lapply(rows, function(r) r$value)
  texts <- unlist(lapply(rows, function(r) c(if (is.character(r$value)) r$value, r$unit, r$description, r$source_script)))
  prob <- .ledger_problems(keys, values, texts)
  if (length(prob)) {
    stop("ledger_write('", part, "') failed:\n  - ", paste(prob, collapse = "\n  - "), call. = FALSE)
  }
  odd <- keys[!vapply(keys, function(k) any(startsWith(k, .LEDGER_PREFIXES)), logical(1))]
  if (length(odd)) warning("ledger part '", part, "': key(s) without a standard stage prefix (",
                           paste(.LEDGER_PREFIXES, collapse = " "), "): ", paste(odd, collapse = ", "), call. = FALSE)
  df <- data.frame(
    key = keys,
    value = vapply(values, .ledger_value_string, character(1)),
    unit = vapply(rows, function(r) r$unit, character(1)),
    description = vapply(rows, function(r) r$description, character(1)),
    source_script = vapply(rows, function(r) r$source_script, character(1)),
    stringsAsFactors = FALSE
  )
  if (is.null(dir)) dir <- P$ledger_parts
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, paste0("ledger_", part, ".csv"))
  if (!requireNamespace("readr", quietly = TRUE)) stop("package 'readr' is required")
  readr::write_csv(df, path)
  message("ledger_write: ", nrow(df), " rows -> ", path)
  invisible(path)
}

.ledger_read_part <- function(path, part) {
  d <- utils::read.csv(path, colClasses = "character", na.strings = character(0),
                       stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  if (!all(c("key", "value") %in% names(d))) stop("ledger file lacks key/value columns: ", path, call. = FALSE)
  for (cn in .LEDGER_COLS) if (!cn %in% names(d)) d[[cn]] <- ""
  d <- d[, .LEDGER_COLS, drop = FALSE]
  d$part <- rep(part, nrow(d))
  d
}

# Shared read-side validation. all: data.frame with the six merged columns,
# all character. Returns it with value_num added, in the reader column order.
.ledger_validate_table <- function(all, who) {
  bad <- all$key[is.na(all$key) | !grepl(.LEDGER_KEY_RE, all$key)]
  if (length(bad)) stop(who, ": key not lowercase snake_case: ", paste(unique(bad), collapse = ", "), call. = FALSE)
  empty <- all$key[!nzchar(trimws(all$value))]
  if (length(empty)) stop(who, ": empty value for key(s): ", paste(unique(empty), collapse = ", "), call. = FALSE)
  vi <- .ledger_value_issues(all$value)
  if (any(vi$nonvalue)) {
    stop(who, ": value is the text NA, NaN, Inf or -Inf for key(s): ",
         paste(all$key[vi$nonvalue], collapse = ", "), call. = FALSE)
  }
  if (any(vi$leading_zero)) {
    stop(who, ": ID-like value with a leading zero (ledger_get would turn it into a number) for key(s): ",
         paste(all$key[vi$leading_zero], collapse = ", "), call. = FALSE)
  }
  dup <- unique(all$key[duplicated(all$key)])
  if (length(dup)) {
    where <- vapply(dup, function(k) paste(all$part[all$key == k], collapse = "+"), character(1))
    stop(who, ": duplicate key(s) across parts: ",
         paste0(dup, " (", where, ")", collapse = ", "), call. = FALSE)
  }
  all$value_num <- suppressWarnings(as.numeric(all$value))
  all[, c("key", "value", "value_num", "unit", "description", "source_script", "part")]
}

ledger_read_all <- function(dir = NULL, design_csv = NULL) {
  if (is.null(dir)) dir <- P$ledger_parts
  if (is.null(design_csv)) design_csv <- file.path(P$config, "design_constants.csv")
  files <- sort(list.files(dir, pattern = "^ledger_.+\\.csv$", full.names = TRUE))
  parts <- lapply(files, function(f) .ledger_read_part(f, sub("^ledger_(.+)\\.csv$", "\\1", basename(f))))
  if (file.exists(design_csv)) parts <- c(parts, list(.ledger_read_part(design_csv, "design")))
  if (!length(parts)) stop("ledger_read_all(): no ledger parts found in ", dir, call. = FALSE)
  all <- do.call(rbind, parts)
  rownames(all) <- NULL
  .ledger_validate_table(all, "ledger_read_all()")
}

ledger_write_merged <- function(path = file.path(P$results, "numbers_ledger.csv"), dir = NULL, design_csv = NULL) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("ledger_write_merged(): path must be one file path", call. = FALSE)
  }
  L <- ledger_read_all(dir = dir, design_csv = design_csv)
  out <- data.frame(
    key = L$key, value = L$value, unit = L$unit, description = L$description,
    source_script = L$source_script, part = L$part, stringsAsFactors = FALSE
  )
  out <- out[, .LEDGER_MERGED_COLS, drop = FALSE]
  if (!requireNamespace("readr", quietly = TRUE)) stop("package 'readr' is required")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(out, path)
  message("ledger_write_merged: ", nrow(out), " rows from ", length(unique(out$part)), " parts -> ", path)
  invisible(path)
}

ledger_read_merged <- function(path = file.path(P$results, "numbers_ledger.csv")) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !file.exists(path)) {
    stop("ledger_read_merged(): file not found: ", paste(path, collapse = " "), call. = FALSE)
  }
  d <- utils::read.csv(path, colClasses = "character", na.strings = character(0),
                       stringsAsFactors = FALSE, fileEncoding = "UTF-8", check.names = FALSE)
  if (anyDuplicated(names(d)) || !setequal(names(d), .LEDGER_MERGED_COLS)) {
    stop("ledger_read_merged(): columns must be exactly ", paste(.LEDGER_MERGED_COLS, collapse = ","),
         " (found: ", paste(names(d), collapse = ","), ")", call. = FALSE)
  }
  if (!nrow(d)) stop("ledger_read_merged(): no rows in ", path, call. = FALSE)
  d <- d[, .LEDGER_MERGED_COLS, drop = FALSE]
  if (any(!nzchar(trimws(d$part)))) stop("ledger_read_merged(): empty part for key(s): ",
                                         paste(d$key[!nzchar(trimws(d$part))], collapse = ", "), call. = FALSE)
  .ledger_validate_table(d, "ledger_read_merged()")
}

ledger_get <- function(ledger, key) {
  i <- match(key, ledger$key)
  if (is.na(i)) stop("ledger_get(): key not in ledger: ", key, call. = FALSE)
  if (!is.na(ledger$value_num[i])) ledger$value_num[i] else ledger$value[i]
}
