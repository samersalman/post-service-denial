# =====================================================================
# 00_setup.R : shared setup for the post-service denial package (WP0)
# =====================================================================
# Source this first in every script.
#
# Bootstrap snippet (put at the top of each script in v1/R; it finds this
# file even from the spaced project path, because Rscript writes spaces in
# --file= as "~+~"):
#
#   .args <- commandArgs(trailingOnly = FALSE)
#   .f <- sub("^--file=", "", grep("^--file=", .args, value = TRUE)[1])
#   .here <- dirname(normalizePath(gsub("~+~", " ", .f, fixed = TRUE)))
#   source(file.path(.here, "00_setup.R"))
#
# (a script in v1/tests uses file.path(dirname(.here), "R", "00_setup.R"))
#
# Provides
#   find_root()            project root (the folder that contains v1/)
#   P                      named list of absolute paths (see below)
#   load_config(dir)       merge every config/*.yaml; error on key collision
#   sha256_file(path)      SHA-256 hex digest(s) of file(s)
#   log_msg(..., level, file)  timestamped message to console + log file
#   ensure_pkgs(pkgs)      check only; never installs
#   assert_ascii_source(f) error if the file has any non-ASCII byte
#   decode_rscript_path(f) turn "~+~" back into spaces
#   script_path()          decoded path of the running Rscript (NA if none)
#   U8                     list of non-ASCII characters built at runtime:
#                          U8$en_dash U8$minus U8$times U8$le U8$ge
#
# P names: root v1 R config data_raw provenance clean_cms clean_ortho
#          results ledger_parts figures fig_captions tables tab_captions
#          manuscript templates qa logs tests cache
#
# Keep this file ASCII. Build special characters with intToUtf8().
# =====================================================================

decode_rscript_path <- function(f) gsub("~+~", " ", f, fixed = TRUE)

script_path <- function() {
  a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (!length(a)) return(NA_character_)
  decode_rscript_path(sub("^--file=", "", a[1]))
}

.psd_walk_up <- function(start) {
  d <- normalizePath(start, winslash = "/", mustWork = FALSE)
  repeat {
    if (file.exists(file.path(d, "v1", "config", "study_config.yaml"))) return(d)
    parent <- dirname(d)
    if (identical(parent, d)) return(NULL)
    d <- parent
  }
}

# Order of attempts: file(s) currently being source()d, then the Rscript
# --file= path (decoded), then getwd(). No hard-coded absolute fallback.
find_root <- function() {
  starts <- character(0)
  for (i in seq_len(sys.nframe())) {
    of <- get0("ofile", envir = sys.frame(i), inherits = FALSE)
    if (is.character(of) && length(of) == 1L && !is.na(of) && nzchar(of)) {
      starts <- c(starts, dirname(of))
    }
  }
  sp <- script_path()
  if (!is.na(sp)) starts <- c(starts, dirname(sp))
  starts <- c(starts, getwd())
  for (s in starts) {
    r <- .psd_walk_up(s)
    if (!is.null(r)) return(r)
  }
  stop("find_root(): could not find a folder containing v1/config/study_config.yaml, ",
       "starting from: ", paste(unique(starts), collapse = " | "), call. = FALSE)
}

.psd_build_paths <- function(root) {
  v1 <- file.path(root, "v1")
  p <- list(
    root = root,
    v1 = v1,
    R = file.path(v1, "R"),
    config = file.path(v1, "config"),
    data_raw = file.path(v1, "data_raw"),
    provenance = file.path(v1, "data_raw", "provenance"),
    clean_cms = file.path(v1, "data_clean", "cms"),
    clean_ortho = file.path(v1, "data_clean", "ortho"),
    results = file.path(v1, "results"),
    ledger_parts = file.path(v1, "results", "ledger_parts"),
    figures = file.path(v1, "figures"),
    fig_captions = file.path(v1, "figures", "captions"),
    tables = file.path(v1, "tables"),
    tab_captions = file.path(v1, "tables", "captions"),
    manuscript = file.path(v1, "manuscript"),
    templates = file.path(v1, "manuscript", "templates"),
    qa = file.path(v1, "qa"),
    logs = file.path(v1, "logs"),
    tests = file.path(v1, "tests"),
    cache = path.expand("~/.cache/post-service-denials")
  )
  if (!dir.exists(p$cache)) dir.create(p$cache, recursive = TRUE, showWarnings = FALSE)
  p
}

P <- .psd_build_paths(find_root())

# ---- config ---------------------------------------------------------
# Merges every *.yaml / *.yml in the config folder (alphabetical). Each file
# must be a mapping. Two files defining the same top-level key is an error.
# The merged list carries attr(, "sources"): top-level key -> file name.
load_config <- function(dir = P$config) {
  if (!requireNamespace("yaml", quietly = TRUE)) stop("package 'yaml' is required")
  files <- sort(list.files(dir, pattern = "\\.ya?ml$", full.names = TRUE))
  cfg <- list()
  src <- character(0)
  for (f in files) {
    x <- yaml::read_yaml(f)
    if (is.null(x)) next
    if (!is.list(x) || is.null(names(x)) || any(!nzchar(names(x)))) {
      stop("load_config(): ", basename(f), " is not a YAML mapping with named keys", call. = FALSE)
    }
    if (anyDuplicated(names(x))) {
      stop("load_config(): duplicate top-level key(s) inside ", basename(f), ": ",
           paste(unique(names(x)[duplicated(names(x))]), collapse = ", "), call. = FALSE)
    }
    dup <- intersect(names(x), names(cfg))
    if (length(dup)) {
      stop("load_config(): key collision, ", paste(dup, collapse = ", "), " defined in both ",
           paste(unique(src[dup]), collapse = "/"), " and ", basename(f), call. = FALSE)
    }
    cfg <- c(cfg, x)
    src[names(x)] <- basename(f)
  }
  attr(cfg, "sources") <- src
  cfg
}

# ---- hashing, logging, package and source checks ---------------------
sha256_file <- function(path) {
  if (!requireNamespace("digest", quietly = TRUE)) stop("package 'digest' is required")
  miss <- path[!file.exists(path)]
  if (length(miss)) stop("sha256_file(): file not found: ", paste(miss, collapse = ", "), call. = FALSE)
  out <- vapply(path, function(f) digest::digest(file = f, algo = "sha256", serialize = FALSE),
                character(1), USE.NAMES = FALSE)
  names(out) <- basename(path)
  out
}

# Writes "YYYY-MM-DD HH:MM:SS [LEVEL] text" to stdout and appends to a log
# file. Default log file: v1/logs/<running script name>.log, or
# v1/logs/interactive.log when not run through Rscript.
log_msg <- function(..., level = "INFO", file = NULL) {
  line <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " [", level, "] ", paste0(..., collapse = ""))
  cat(line, "\n", sep = "")
  if (is.null(file)) {
    sp <- script_path()
    stem <- if (is.na(sp)) "interactive" else sub("\\.[Rr]$", "", basename(sp))
    file <- file.path(P$logs, paste0(stem, ".log"))
  }
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  cat(line, "\n", sep = "", file = file, append = TRUE)
  invisible(line)
}

# Check only. Installing inside a pipeline script is not allowed.
ensure_pkgs <- function(pkgs) {
  ok <- vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
  if (!all(ok)) {
    stop("Missing R package(s): ", paste(pkgs[!ok], collapse = ", "),
         ". Install them outside the pipeline, then rerun.", call. = FALSE)
  }
  invisible(TRUE)
}

assert_ascii_source <- function(file) {
  if (!file.exists(file)) stop("assert_ascii_source(): file not found: ", file, call. = FALSE)
  lines <- readLines(file, warn = FALSE, encoding = "bytes")
  bad <- which(grepl("[^\\x01-\\x7F]", lines, perl = TRUE, useBytes = TRUE))
  if (length(bad)) {
    stop("Non-ASCII byte(s) in ", file, " at line(s): ",
         paste(utils::head(bad, 20), collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

# ---- characters that must not appear as literals in R sources --------
U8 <- list(
  en_dash = intToUtf8(0x2013),
  minus   = intToUtf8(0x2212),
  times   = intToUtf8(0x00D7),
  le      = intToUtf8(0x2264),
  ge      = intToUtf8(0x2265)
)
