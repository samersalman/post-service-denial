# =====================================================================
# 08_merge_ledger.R : WP6b. Merge every ledger part into v1/results/numbers_ledger.csv
# =====================================================================
# Calls ledger_write_merged() with its defaults (path = results/numbers_ledger.csv, parts from
# results/ledger_parts, config/design_constants.csv as part "design"), then reads the merged file back with
# ledger_read_merged() as a self-check. Errors on a duplicate key across parts, an empty value, the text NA,
# NaN or Inf, or an ID-like value with a leading zero (those checks live in ledger.R).
# Requires the parts kff, desc, figdesc, ortho, models and design. If two parts collide on a key, fix the key in
# the part that you own; never edit another task's part.
# Run: cd v1 ; Rscript R/08_merge_ledger.R     Log: v1/logs/wp6b_merge.log
# Keep this file ASCII.
# =====================================================================

if (!exists("P", inherits = TRUE) || !exists("find_root", inherits = TRUE)) {
  .args <- commandArgs(trailingOnly = FALSE)
  .f <- sub("^--file=", "", grep("^--file=", .args, value = TRUE)[1])
  .here <- if (is.na(.f)) getwd() else dirname(normalizePath(gsub("~+~", " ", .f, fixed = TRUE)))
  .setup <- file.path(.here, "00_setup.R")
  if (!file.exists(.setup)) .setup <- file.path(dirname(.here), "R", "00_setup.R")
  if (!file.exists(.setup)) .setup <- file.path(getwd(), "R", "00_setup.R")
  source(.setup)
}
if (!exists("ledger_write_merged", inherits = TRUE)) source(file.path(P$R, "ledger.R"))

LOGFILE <- getOption("psd_merge_logfile", file.path(P$logs, "wp6b_merge.log"))
lg <- function(...) log_msg(..., file = LOGFILE)

REQUIRED_PARTS <- c("kff", "desc", "figdesc", "ortho", "models", "design")

merge_main <- function() {
  lg("08_merge_ledger.R start")
  have <- sub("^ledger_(.+)\\.csv$", "\\1", list.files(P$ledger_parts, pattern = "^ledger_.+\\.csv$"))
  lg("ledger part files found: ", paste(sort(have), collapse = ", "))
  miss <- setdiff(setdiff(REQUIRED_PARTS, "design"), have)
  if (length(miss)) stop("08_merge_ledger.R: missing ledger part(s): ", paste(miss, collapse = ", "), call. = FALSE)
  if (!file.exists(file.path(P$config, "design_constants.csv"))) stop("config/design_constants.csv is missing", call. = FALSE)
  path <- ledger_write_merged()
  L <- ledger_read_merged()
  parts <- table(L$part)
  lg("merged ledger written to ", path, " ; rows ", nrow(L), " ; parts ", paste(names(parts), as.integer(parts), sep = "=", collapse = ", "))
  if (!all(REQUIRED_PARTS %in% names(parts))) stop("merged ledger lacks part(s): ", paste(setdiff(REQUIRED_PARTS, names(parts)), collapse = ", "), call. = FALSE)
  stopifnot(!anyDuplicated(L$key))
  lg("self-check with ledger_read_merged() passed: ", nrow(L), " unique keys, ", sum(!is.na(L$value_num)), " numeric")
  invisible(L)
}

if (!isTRUE(getOption("psd_merge_no_run"))) invisible(merge_main())
