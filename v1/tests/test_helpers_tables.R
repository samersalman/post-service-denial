# =====================================================================
# test_helpers_tables.R : S1 tests for v1/R/helpers_tables.R (plain R)
# Run:  Rscript "<abs path>/v1/tests/test_helpers_tables.R"
# Outputs go to PSD_TEST_OUT (default: a temp folder), never into v1.
# =====================================================================
.args <- commandArgs(trailingOnly = FALSE)
.f <- sub("^--file=", "", grep("^--file=", .args, value = TRUE)[1])
.self <- normalizePath(gsub("~+~", " ", .f, fixed = TRUE))
.here <- dirname(.self)
RDIR <- file.path(dirname(.here), "R")
source(file.path(RDIR, "00_setup.R"))
source(file.path(RDIR, "helpers_tables.R"))
suppressMessages({library(flextable); library(officer); library(xml2)})

OUT <- Sys.getenv("PSD_TEST_OUT", unset = "")
if (!nzchar(OUT)) OUT <- file.path(tempdir(), "s1_tables_test")
OUT <- file.path(OUT, "tables_test")
unlink(OUT, recursive = TRUE)
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

RES <- data.frame(test = character(), pass = logical(), note = character(), stringsAsFactors = FALSE)
check <- function(name, cond, note = "") {
  ok <- isTRUE(cond)
  RES[nrow(RES) + 1L, ] <<- list(name, ok, note)
  cat(sprintf("[%s] %s%s\n", if (ok) "PASS" else "FAIL", name, if (nzchar(note)) paste0("  (", note, ")") else ""))
}
raises <- function(expr, pattern = NULL) {
  r <- tryCatch({ force(expr); NULL }, error = function(e) conditionMessage(e))
  if (is.null(r)) return(FALSE)
  is.null(pattern) || grepl(pattern, r, fixed = TRUE)
}
W <- c(w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main")
doc_xml <- function(docx) {
  d <- tempfile("tt_"); dir.create(d)
  utils::unzip(docx, files = "word/document.xml", exdir = d)
  read_xml(file.path(d, "word", "document.xml"))
}
em <- intToUtf8(0x2014)

# listing of v1/tables, v1/figures, v1/manuscript before the tests (nothing may be added)
snap <- function() sort(unlist(lapply(c(P$tables, P$figures, P$manuscript), list.files, recursive = TRUE, all.files = TRUE, full.names = TRUE)))
v1_before <- snap()

# ---- toy tables ---------------------------------------------------------
df_a <- data.frame(`Plan type` = c("EPO", "HMO", "PPO"), `Claims received` = c(1234, 5678, 91011),
                   `Denial rate` = c("19.0%", "12.5%", "<0.1%"), Note = c("a", "b", "c"), check.names = FALSE)
w_a <- c(2.0, 1.5, 1.5, 1.5)
ft <- make_ft(df_a, w_a, footnote = c("Note one.", "Note two."))
check("make_ft returns a flextable", inherits(ft, "flextable"))

docx <- file.path(OUT, "t1.docx")
x <- read_docx()
x <- body_add_flextable(x, ft, align = "left")
print(x, target = docx)
dx <- doc_xml(docx)
q <- function(xp) xml_find_all(dx, xp, W)

# ---- fonts and sizes ----------------------------------------------------
runs <- q("//w:tbl//w:r")
check("table has runs", length(runs) > 10, length(runs))
check("every table run is Times New Roman", length(q("//w:tbl//w:r[not(w:rPr/w:rFonts[@w:ascii='Times New Roman' and @w:hAnsi='Times New Roman' and @w:cs='Times New Roman'])]")) == 0)
check("every table run is 9 pt (sz 18)", length(q("//w:tbl//w:r[not(w:rPr/w:sz[@w:val='18'])]")) == 0)
ft11 <- make_ft(df_a, w_a, font_size = 11)
x11 <- body_add_flextable(read_docx(), ft11); d11 <- file.path(OUT, "t11.docx"); print(x11, target = d11)
check("font_size argument changes the size", length(xml_find_all(doc_xml(d11), "//w:tbl//w:r[not(w:rPr/w:sz[@w:val='22'])]", W)) == 0)

# ---- grid, layout, header repeat ---------------------------------------
gc <- as.integer(xml_attr(q("//w:tbl/w:tblGrid/w:gridCol"), "w:w", W))
check("tblGrid widths equal widths_in * 1440", identical(gc, as.integer(round(w_a * 1440))), paste(gc, collapse = ","))
check("grid columns are not all equal", length(unique(gc)) > 1)
check("layout is fixed", length(q("//w:tbl/w:tblPr/w:tblLayout[@w:type='fixed']")) == 1)
check("header row repeats (w:tblHeader)", length(q("//w:tbl/w:tr/w:trPr/w:tblHeader")) >= 1)

# ---- three-line table ----------------------------------------------------
side <- q("//w:tc/w:tcPr/w:tcBorders/w:left[@w:val!='none'] | //w:tc/w:tcPr/w:tcBorders/w:right[@w:val!='none']")
check("no vertical lines", length(side) == 0)
rows <- q("//w:tbl/w:tr")
n_body <- 3L
hdr_top <- xml_find_all(rows[[1]], "./w:tc/w:tcPr/w:tcBorders/w:top[@w:val='single']", W)
check("top rule on the header (1 pt)", length(hdr_top) == ncol(df_a) && all(xml_attr(hdr_top, "w:sz", W) == "8"))
hdr_bot <- xml_find_all(rows[[1]], "./w:tc/w:tcPr/w:tcBorders/w:bottom[@w:val='single']", W)
check("header rule (0.5 pt)", length(hdr_bot) == ncol(df_a) && all(xml_attr(hdr_bot, "w:sz", W) == "4"))
last_body <- rows[[1 + n_body]]
lb <- xml_find_all(last_body, "./w:tc/w:tcPr/w:tcBorders/w:bottom[@w:val='single']", W)
check("bottom rule under the last body row (1 pt)", length(lb) == ncol(df_a) && all(xml_attr(lb, "w:sz", W) == "8"))
mid <- xml_find_all(rows[[3]], "./w:tc/w:tcPr/w:tcBorders/w:top[@w:val='single'] | ./w:tc/w:tcPr/w:tcBorders/w:bottom[@w:val='single']", W)
check("no rules between body rows", length(mid) == 0)
ftxt <- paste(xml_text(q("//w:tbl//w:t")), collapse = "|")
check("footnote lines are inside the table object", grepl("Note one.", ftxt, fixed = TRUE) && grepl("Note two.", ftxt, fixed = TRUE))

# ---- number formatting and alignment ------------------------------------
check("whole numbers get thousands separators", grepl("91,011", ftxt, fixed = TRUE) && grepl("1,234", ftxt, fixed = TRUE))
jc_of <- function(cell_text) {
  p <- q(sprintf("//w:tbl//w:tc[.//w:t[text()='%s']]/w:p/w:pPr/w:jc", cell_text))
  if (!length(p)) NA_character_ else xml_attr(p[[1]], "w:val", W)
}
check("number columns right-aligned, text columns left", identical(jc_of("1,234"), "right") && identical(jc_of("19.0%"), "right") &&
        identical(jc_of("EPO"), "left") && identical(jc_of("a"), "left"),
      paste(jc_of("1,234"), jc_of("19.0%"), jc_of("EPO"), jc_of("a")))
check("header aligned like its column", identical(jc_of("Claims received"), "right") && identical(jc_of("Plan type"), "left"))
ft_al <- make_ft(df_a, w_a, align = c("left", "center", "center", "right"))
xa <- body_add_flextable(read_docx(), ft_al); da <- file.path(OUT, "talign.docx"); print(xa, target = da)
jca <- xml_attr(xml_find_all(doc_xml(da), "//w:tbl/w:tr[2]/w:tc/w:p/w:pPr/w:jc", W), "w:val", W)
check("align argument overrides", identical(jca, c("left", "center", "center", "right")), paste(jca, collapse = ","))
ft_b <- make_ft(df_a, w_a, bold_rows = 2)
xb <- body_add_flextable(read_docx(), ft_b); db <- file.path(OUT, "tbold.docx"); print(xb, target = db)
bold_rows <- xml_find_all(doc_xml(db), "//w:tbl/w:tr[3]/w:tc//w:r[w:rPr/w:b[not(@w:val='false')]]", W)
check("bold_rows makes that body row bold", length(bold_rows) == ncol(df_a), length(bold_rows))

# ---- hard errors --------------------------------------------------------
bad <- df_a; names(bad)[2] <- "claims_received"
check("underscore in a header is an error", raises(make_ft(bad, w_a), "underscore"))
dup <- df_a; names(dup)[2] <- "Plan type"
check("duplicated header is an error", raises(make_ft(dup, w_a), "duplicated"))
check("widths over 6.5 in (portrait) is an error", raises(make_ft(df_a, c(2, 2, 2, 1)), "6.5"))
check("widths of 6.5 in exactly is accepted", inherits(make_ft(df_a, c(2, 1.5, 1.5, 1.5)), "flextable"))
check("landscape accepts 9 in", inherits(make_ft(df_a, c(3, 2, 2, 2), landscape = TRUE), "flextable"))
check("landscape rejects more than 9 in", raises(make_ft(df_a, c(3, 3, 2, 1.5), landscape = TRUE), "9.0"))
check("wrong number of widths is an error", raises(make_ft(df_a, c(2, 2)), "one per column"))
na_df <- df_a; na_df$Note[2] <- NA
check("NA cell is an error", raises(make_ft(na_df, w_a), "NA in column"))
frac <- df_a; frac$`Claims received` <- c(1.5, 2, 3)
check("non-integer numeric column is an error", raises(make_ft(frac, w_a), "non-integer"))
emd <- df_a; emd$Note[1] <- paste0("x", em, "y")
check("U+2014 in a cell is an error", raises(make_ft(emd, w_a), "U+2014"))
check("U+2014 in a footnote is an error", raises(make_ft(df_a, w_a, footnote = paste0("a", em, "b")), "U+2014"))
under <- df_a; under$Note[1] <- "in_network"
check("underscore in a cell is an error", raises(make_ft(under, w_a), "underscore"))
check("allow_underscore_cells = TRUE lets it pass", inherits(make_ft(under, w_a, allow_underscore_cells = TRUE), "flextable"))
check("empty data.frame is an error", raises(make_ft(df_a[0, ], w_a), "at least one row"))

# ---- save_table / read_table_rds ----------------------------------------
paths <- save_table(df_a, "tableX", widths_in = w_a, footnote = c("Foot one."), landscape = FALSE, align = NULL,
                    bold_rows = 1L, outdir = OUT)
check("save_table writes csv and rds", all(file.exists(paths)) && grepl("tableX.csv$", paths[["csv"]]) && grepl("tableX.rds$", paths[["rds"]]))
csv <- utils::read.csv(paths[["csv"]], check.names = FALSE, colClasses = "character")
check("csv has the human-readable headers and the cell text", identical(names(csv), names(df_a)) && csv$`Claims received`[3] == "91,011")
rd <- read_table_rds(paths[["rds"]])
check("rds keeps widths, footnote, landscape, bold_rows",
      identical(attr(rd, "widths_in"), w_a) && identical(attr(rd, "footnote"), "Foot one.") &&
        identical(attr(rd, "landscape"), FALSE) && identical(attr(rd, "bold_rows"), 1L))
check("rds cells are character", all(vapply(rd, is.character, logical(1))))
check("save_table rejects an underscore header and writes nothing",
      raises(save_table(bad, "tableBad", widths_in = w_a, outdir = OUT), "underscore") && !file.exists(file.path(OUT, "tableBad.csv")))
check("save_table rejects widths over the limit", raises(save_table(df_a, "tableBad2", widths_in = c(3, 3, 3, 3), outdir = OUT), "6.5"))
check("read_table_rds errors without widths_in",
      { f <- file.path(OUT, "nowidth.rds"); saveRDS(df_a, f); raises(read_table_rds(f), "widths_in") })
check("save_table rejects a path-like name", raises(save_table(df_a, "a/b", widths_in = w_a, outdir = OUT), "plain file stem"))

# ---- render check: column widths survive a real layout engine ------------
soffice <- Sys.which("soffice")
if (nzchar(soffice) && nzchar(Sys.which("pdftotext"))) {
  wide <- data.frame(Reason = c("Short", "Other"), Description = c(paste(rep("long text that wraps", 8), collapse = " "), "x"),
                     Share = c("1.0%", "2.0%"), check.names = FALSE)
  x3 <- body_add_flextable(read_docx(), make_ft(wide, c(1.0, 4.0, 1.5)), align = "left")
  d3 <- file.path(OUT, "twide.docx"); print(x3, target = d3)
  out <- suppressWarnings(system2(soffice, c("--headless", "--convert-to", "pdf", "--outdir", shQuote(OUT), shQuote(d3)), stdout = TRUE, stderr = TRUE))
  pdf <- file.path(OUT, "twide.pdf")
  check("LibreOffice converted the table docx", file.exists(pdf))
  if (file.exists(pdf)) {
    bb <- suppressWarnings(system2("pdftotext", c("-bbox", shQuote(pdf), "-"), stdout = TRUE, stderr = TRUE))
    wl <- regmatches(bb, regexec('<word xMin="([0-9.]+)" yMin="([0-9.]+)" xMax="([0-9.]+)" yMax="([0-9.]+)">(.*)</word>', bb))
    wl <- wl[lengths(wl) == 6]
    wd <- data.frame(x0 = as.numeric(vapply(wl, `[`, "", 2)), x1 = as.numeric(vapply(wl, `[`, "", 4)), t = vapply(wl, `[`, "", 6),
                     stringsAsFactors = FALSE)
    xr <- wd$x0[wd$t == "Reason"][1]; xd <- wd$x0[wd$t == "Description"][1]
    xs_right <- wd$x1[wd$t == "Share"][1]
    left_margin <- 72
    check("rendered Description column starts 1.0 in after the first column", abs((xd - xr) / 72 - 1.0) < 0.06,
          sprintf("%.2f in", (xd - xr) / 72))
    check("rendered right edge of the right-aligned Share header is at the table edge (6.5 in)",
          abs((xs_right - left_margin) / 72 - (6.5 - 3 / 72)) < 0.06, sprintf("%.2f in", (xs_right - left_margin) / 72))
    check("no rendered word crosses the page margins", all(wd$x0 >= 71) && all(wd$x1 <= 541), sprintf("x %.1f to %.1f pt", min(wd$x0), max(wd$x1)))
    desc_words <- wd[wd$x0 >= xd - 1 & wd$x0 < left_margin + 5.0 * 72 - 5 & wd$t %in% strsplit(paste(rep("long text that wraps", 8), collapse = " "), " ")[[1]], ]
    check("long cell wraps within its 4.0 in column and fills it",
          nrow(desc_words) >= 20 && max(desc_words$x1) < left_margin + 5.0 * 72 && max(desc_words$x1) > left_margin + 4.2 * 72,
          sprintf("rightmost %.1f pt, column edge %.1f pt", max(desc_words$x1), left_margin + 5.0 * 72))
  }
} else {
  cat("[NOTE] soffice or pdftotext not found: render checks skipped\n")
}

# ---- hygiene ------------------------------------------------------------
check("helpers_tables.R is ASCII", isTRUE(assert_ascii_source(file.path(RDIR, "helpers_tables.R"))))
check("test_helpers_tables.R is ASCII", isTRUE(assert_ascii_source(.self)))
check("tests added nothing to v1/tables, v1/figures, v1/manuscript", identical(snap(), v1_before), paste(setdiff(snap(), v1_before), collapse = ","))

cat("\n==================== SUMMARY ====================\n")
cat(sprintf("%d tests, %d passed, %d failed\n", nrow(RES), sum(RES$pass), sum(!RES$pass)))
if (any(!RES$pass)) {
  cat("FAILED:\n")
  cat(paste0("  - ", RES$test[!RES$pass]), sep = "\n")
  quit(save = "no", status = 1)
}
cat("ALL PASS\n")
