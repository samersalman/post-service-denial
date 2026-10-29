# =====================================================================
# helpers_tables.R : table objects and flextable builder (S1)
# =====================================================================
# Source 00_setup.R first. Requires: flextable, officer.
#
# THE TABLE OBJECT (what WP7a / WP7b save, what build_manuscript reads)
#   V1/tables/tableN.csv   human-readable headers, the cell text exactly as shown
#   V1/tables/tableN.rds   a plain data.frame (all cells character) with attributes
#       widths_in   numeric, one per column, inches; sum <= 6.5 (portrait) or 9 (landscape)
#       landscape   logical (default FALSE); TRUE puts the table on a landscape page
#       footnote    character vector or NULL; each element is one footnote paragraph
#                   under the table ({{key|fmt}} placeholders allowed, they are
#                   rendered from the ledger and traced). Footnotes may instead (or
#                   also) go in V1/tables/captions/tableN.txt lines 2..n.
#       align       NULL or character vector (left/center/right), one per column
#       bold_rows   NULL or integer row numbers of the body to set in bold
#                   (for example sub-group header rows)
#   Use save_table() to write both files; it checks everything below.
#
# RULES (hard errors)
#   - header (column name) must not contain an underscore, be empty or duplicated;
#   - no cell may be NA (use "" for an empty cell, "N/A" or a marker where meant);
#   - numeric columns must hold whole numbers (they are shown with thousands
#     separators); pre-format decimals as text with sprintf() so rounding is explicit;
#   - cells, headers and footnotes may not contain U+2014 (em-dash);
#   - cells may not contain an underscore (reviewer-facing text) unless
#     allow_underscore_cells = TRUE;
#   - column widths sum to at most 6.5 in (portrait) or 9 in (landscape).
#
# API
#   make_ft(df, widths_in, footnote = NULL, landscape = FALSE, font_size = 9,
#           align = NULL, bold_rows = NULL, font_family = "Times New Roman",
#           allow_underscore_cells = FALSE)
#       flextable: Times New Roman 9 pt, three-line table (top rule 1 pt, header rule
#       0.5 pt, bottom rule 1 pt, no vertical lines), header repeats on each page,
#       text wraps, fixed layout with the column widths written into the table grid
#       (w:tblGrid) so Word honours them. Numeric-looking columns are right-aligned,
#       the first column and text columns left-aligned, header aligned like its
#       column. footnote (optional) becomes footer lines inside the table object.
#   save_table(df, name, widths_in, footnote = NULL, landscape = FALSE, align = NULL,
#              bold_rows = NULL, outdir = NULL, allow_underscore_cells = FALSE)
#       validate, then write <outdir>/<name>.csv and <name>.rds (outdir defaults to
#       P$tables). Returns both paths invisibly.
#   read_table_rds(path)
#       read a tableN.rds and check the attributes; returns the data.frame.
#   table_checks(df, widths_in, landscape, ...)
#       the validation used above; returns the character data.frame or stops.
#
# Keep this file ASCII.
# =====================================================================

.ht_emdash <- function() intToUtf8(0x2014)
.ht_minus <- function() intToUtf8(0x2212)
.HT_MAX_W <- c(portrait = 6.5, landscape = 9)

# regex for a cell that looks like a number: 1,234  19.0%  -3.2  <.001  12.3 (11.0 to 13.5)
.ht_numlike_re <- function() {
  paste0("^[<>~]?[-+", .ht_minus(), "]?[.]?[0-9][0-9,.]*(%| ?[(\\[].*[)\\]])?$")
}
.ht_markers <- function() c("*", "**", "***", "N/A", "NA", "-", intToUtf8(0x2013))

.ht_col_numlike <- function(x) {
  x <- trimws(x)
  x <- x[nzchar(x) & !(x %in% .ht_markers())]
  length(x) > 0 && all(grepl(.ht_numlike_re(), x))
}

# Validate and convert to an all-character data.frame. Does not touch attributes.
table_checks <- function(df, widths_in, landscape = FALSE, footnote = NULL,
                         allow_underscore_cells = FALSE) {
  if (!is.data.frame(df) || ncol(df) < 1L || nrow(df) < 1L) {
    stop("table: df must be a data.frame with at least one row and one column", call. = FALSE)
  }
  nm <- names(df)
  if (is.null(nm) || anyNA(nm) || any(!nzchar(trimws(nm)))) stop("table: every column needs a non-empty header", call. = FALSE)
  if (anyDuplicated(nm)) stop("table: duplicated header(s): ", paste(unique(nm[duplicated(nm)]), collapse = ", "), call. = FALSE)
  bad <- nm[grepl("_", nm, fixed = TRUE)]
  if (length(bad)) {
    stop("table: header contains an underscore (use plain words with spaces): ",
         paste(bad, collapse = ", "), call. = FALSE)
  }
  if (!is.numeric(widths_in) || length(widths_in) != ncol(df) || anyNA(widths_in) || any(widths_in <= 0)) {
    stop("table: widths_in must be positive numbers, one per column (", ncol(df), " expected)", call. = FALSE)
  }
  lim <- if (isTRUE(landscape)) .HT_MAX_W[["landscape"]] else .HT_MAX_W[["portrait"]]
  if (sum(widths_in) > lim + 1e-6) {
    stop(sprintf("table: widths sum to %.3f in, more than the %.1f in allowed for a %s table",
                 sum(widths_in), lim, if (isTRUE(landscape)) "landscape" else "portrait"), call. = FALSE)
  }
  out <- df
  for (j in seq_along(out)) {
    x <- out[[j]]
    if (is.factor(x)) x <- as.character(x)
    if (is.numeric(x)) {
      if (anyNA(x)) stop("table: NA in numeric column '", nm[j], "'", call. = FALSE)
      if (any(abs(x - round(x)) > 1e-9)) {
        stop("table: numeric column '", nm[j], "' has non-integer values; pre-format decimals as text (sprintf) ",
             "so the rounding is explicit", call. = FALSE)
      }
      x <- formatC(x, format = "d", big.mark = ",")
    } else if (!is.character(x)) {
      stop("table: column '", nm[j], "' must be character, factor or whole-number numeric", call. = FALSE)
    }
    if (anyNA(x)) {
      stop("table: NA in column '", nm[j], "' at row(s) ", paste(utils::head(which(is.na(x)), 10), collapse = ", "),
           " (use an empty string or an explicit marker)", call. = FALSE)
    }
    out[[j]] <- x
  }
  allt <- c(nm, unlist(out, use.names = FALSE), footnote)
  if (any(grepl(.ht_emdash(), allt, fixed = TRUE))) stop("table: U+2014 (em-dash) found in headers, cells or footnote", call. = FALSE)
  if (!isTRUE(allow_underscore_cells)) {
    u <- grepl("_", unlist(out, use.names = FALSE), fixed = TRUE)
    if (any(u)) {
      stop("table: a cell contains an underscore (reviewer-facing text must use plain words): ",
           paste(utils::head(unique(unlist(out, use.names = FALSE)[u]), 5), collapse = " | "), call. = FALSE)
    }
  }
  out
}

make_ft <- function(df, widths_in, footnote = NULL, landscape = FALSE, font_size = 9,
                    align = NULL, bold_rows = NULL, font_family = "Times New Roman",
                    allow_underscore_cells = FALSE) {
  if (!requireNamespace("flextable", quietly = TRUE) || !requireNamespace("officer", quietly = TRUE)) {
    stop("make_ft(): packages flextable and officer are required", call. = FALSE)
  }
  if (!is.null(footnote)) {
    if (!is.character(footnote) || anyNA(footnote)) stop("make_ft(): footnote must be a character vector", call. = FALSE)
    footnote <- footnote[nzchar(trimws(footnote))]
    if (!length(footnote)) footnote <- NULL
  }
  d <- table_checks(df, widths_in, landscape, footnote, allow_underscore_cells)
  nc <- ncol(d)
  nm <- names(d)
  # alignment: first column and text columns left, number-like columns right
  if (is.null(align)) {
    align <- vapply(seq_len(nc), function(j) if (j > 1L && .ht_col_numlike(d[[j]])) "right" else "left", character(1))
  } else {
    if (!is.character(align) || length(align) != nc || !all(align %in% c("left", "center", "right"))) {
      stop("make_ft(): align must be left/center/right, one per column", call. = FALSE)
    }
  }
  if (!is.null(bold_rows)) {
    if (!is.numeric(bold_rows) || any(bold_rows < 1 | bold_rows > nrow(d))) stop("make_ft(): bold_rows out of range", call. = FALSE)
  }

  ft <- flextable::flextable(d)
  ft <- flextable::set_table_properties(ft, layout = "fixed", align = "left")
  for (j in seq_len(nc)) ft <- flextable::width(ft, j = j, width = widths_in[j], unit = "in")
  if (!is.null(footnote)) ft <- flextable::add_footer_lines(ft, values = footnote)
  ft <- flextable::font(ft, fontname = font_family, part = "all")
  ft <- flextable::fontsize(ft, size = font_size, part = "all")
  ft <- flextable::color(ft, color = "black", part = "all")
  ft <- flextable::bold(ft, bold = FALSE, part = "all")
  ft <- flextable::bold(ft, bold = TRUE, part = "header")
  if (!is.null(bold_rows)) ft <- flextable::bold(ft, i = bold_rows, bold = TRUE, part = "body")
  ft <- flextable::line_spacing(ft, space = 1, part = "all")
  ft <- flextable::padding(ft, padding.top = 1.5, padding.bottom = 1.5, padding.left = 3, padding.right = 3, part = "all")
  ft <- flextable::valign(ft, valign = "top", part = "body")
  ft <- flextable::valign(ft, valign = "bottom", part = "header")
  for (j in seq_len(nc)) {
    ft <- flextable::align(ft, j = j, align = align[j], part = "header")
    ft <- flextable::align(ft, j = j, align = align[j], part = "body")
  }
  if (!is.null(footnote)) ft <- flextable::align(ft, align = "left", part = "footer")
  # three-line table: remove everything, then top, header and bottom rules only
  ft <- flextable::border_remove(ft)
  ft <- flextable::hline_top(ft, border = officer::fp_border(color = "black", width = 1), part = "header")
  ft <- flextable::hline_bottom(ft, border = officer::fp_border(color = "black", width = 0.5), part = "header")
  ft <- flextable::hline_bottom(ft, border = officer::fp_border(color = "black", width = 1), part = "body")
  attr(ft, "psd_landscape") <- isTRUE(landscape)
  attr(ft, "psd_widths_in") <- widths_in
  attr(ft, "psd_font_size") <- font_size
  ft
}

.ht_attrs <- function(df, widths_in, footnote, landscape, align, bold_rows) {
  attr(df, "widths_in") <- widths_in
  attr(df, "landscape") <- isTRUE(landscape)
  attr(df, "footnote") <- footnote
  attr(df, "align") <- align
  attr(df, "bold_rows") <- bold_rows
  df
}

save_table <- function(df, name, widths_in, footnote = NULL, landscape = FALSE, align = NULL,
                       bold_rows = NULL, outdir = NULL, allow_underscore_cells = FALSE) {
  if (!is.character(name) || length(name) != 1L || !grepl("^[A-Za-z0-9_.-]+$", name)) {
    stop("save_table(): name must be a plain file stem such as table1", call. = FALSE)
  }
  if (is.null(outdir)) outdir <- P$tables
  if (!is.null(footnote) && (!is.character(footnote) || anyNA(footnote))) stop("save_table(): footnote must be character", call. = FALSE)
  d <- table_checks(df, widths_in, landscape, footnote, allow_underscore_cells)
  # make_ft is the strictest consumer: build it once so a bad object fails here, not in the build
  invisible(make_ft(d, widths_in, footnote = NULL, landscape = landscape, align = align,
                    bold_rows = bold_rows, allow_underscore_cells = allow_underscore_cells))
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  csv <- file.path(outdir, paste0(name, ".csv"))
  rds <- file.path(outdir, paste0(name, ".rds"))
  if (!requireNamespace("readr", quietly = TRUE)) stop("package 'readr' is required")
  readr::write_csv(d, csv)
  saveRDS(.ht_attrs(d, widths_in, footnote, landscape, align, bold_rows), rds)
  invisible(c(csv = csv, rds = rds))
}

read_table_rds <- function(path) {
  if (!file.exists(path)) stop("read_table_rds(): file not found: ", path, call. = FALSE)
  d <- readRDS(path)
  if (!is.data.frame(d)) stop("read_table_rds(): ", basename(path), " is not a data.frame", call. = FALSE)
  w <- attr(d, "widths_in")
  if (is.null(w)) stop("read_table_rds(): ", basename(path), " has no widths_in attribute (write it with save_table())", call. = FALSE)
  lscape <- isTRUE(attr(d, "landscape"))
  table_checks(d, w, lscape, attr(d, "footnote"))
  d
}
