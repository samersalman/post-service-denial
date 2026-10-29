# =====================================================================
# theme_pub.R : shared publication theme, colours, figure export (WP0)
# =====================================================================
# Source 00_setup.R first (save_fig's default outdir is P$figures).
# Requires: ggplot2 (>= 4.0), systemfonts, ragg, grid. patchwork is optional.
#
# API
#   theme_pub(base_size = 6, grid = FALSE)
#       Arial; axis text base_size - 0.5 (5.5), axis title / strip / legend
#       title base_size (6), legend text base_size - 0.5, panel tag 8 pt bold;
#       axis lines 0.5 pt, ticks 0.25 pt; no gridlines (grid = TRUE adds a very
#       faint major y grid); title, subtitle and caption are element_blank()
#       so a baked-in title cannot appear; legend on top; white plot background.
#       Geoms inherit Arial and base_size - 0.5 text through theme(geom = ...),
#       so geom_text() with no size argument is 5.5 pt.
#   pub_colors      named Okabe-Ito vector, FIXED meanings across all figures:
#                   in_network, out_of_network, orthopaedic, control, neutral
#   pub_palette8    the eight Okabe-Ito colours (named) for other categories
#                   (Okabe-Ito is a colourblind-safe palette)
#   pub_labels      named vector of human-readable legend labels for pub_colors:
#                   in_network = "In-network", out_of_network = "Out-of-network",
#                   orthopaedic = "Orthopaedic", control = "Control", neutral = "Neutral"
#   scale_colour_pub(..., labels = pub_labels), scale_fill_pub(..., labels = pub_labels)
#                   manual scales using pub_colors; data levels must be the
#                   pub_colors names; legends show the pub_labels text, never the
#                   raw names with underscores. Override through the dots: a named
#                   character vector replaces only the names it gives and keeps the
#                   defaults for the rest (labels = c(in_network = "Within network"));
#                   labels = ggplot2::waiver() shows the raw level names; a function
#                   or an unnamed vector is passed to ggplot2 unchanged.
#   pt_to_mm(pt)    font size in points -> geom size argument (mm)
#   pt_to_lw(pt)    line width in points -> ggplot linewidth argument
#   panel_letter(letter)
#                   add with +; lowercase bold 8 pt tag at top left
#   save_fig(plot, name, width_mm = c(89, 183), height_mm, outdir = P$figures)
#                   writes <name>.pdf (cairo_pdf, Arial embedded), <name>.png and
#                   <name>.tiff (ragg, LZW), both 600 dpi. width_mm must be 89 or
#                   183 (+/- 0.5; the default vector means 89); height_mm is
#                   required and at most 247. Returns c(pdf=, png=, tiff=).
#   min_font_pt(plot, detail = FALSE)
#                   smallest font size (points) over every text element of the
#                   resolved theme and every text/label geom (size read from the
#                   built layer data, so mapped sizes count). For patchwork objects
#                   every sub-plot is included. detail = TRUE returns the table.
#                   Limit: text drawn by annotation_custom() or raw grobs is not seen.
#
# Keep this file ASCII. Build special characters with intToUtf8() (see U8 in 00_setup.R).
# =====================================================================

if (!requireNamespace("ggplot2", quietly = TRUE)) stop("package 'ggplot2' is required")
if (!requireNamespace("systemfonts", quietly = TRUE)) stop("package 'systemfonts' is required")
if (!requireNamespace("ragg", quietly = TRUE)) stop("package 'ragg' is required")
if (utils::packageVersion("ggplot2") < "4.0.0") stop("theme_pub.R needs ggplot2 >= 4.0.0 (element_geom)")

# ---- Arial: resolve through systemfonts, never substitute --------------
.PUB_FAMILY <- "Arial"

.pub_resolve_arial <- function() {
  one <- function(weight) {
    m <- systemfonts::match_fonts(.PUB_FAMILY, italic = FALSE, weight = weight)
    list(path = m$path[1], index = m$index[1])
  }
  reg <- one("normal")
  bld <- one("bold")
  chk <- function(x, expect) {
    ok <- !is.na(x$path) && file.exists(x$path) && identical(basename(x$path), expect)
    if (!ok) {
      stop("Arial did not resolve through systemfonts (wanted ", expect, ", got '", x$path,
           "'). Stopping rather than substituting Helvetica or another font. ",
           "Expected /System/Library/Fonts/Supplemental/", expect, call. = FALSE)
    }
  }
  chk(reg, "Arial.ttf")
  chk(bld, "Arial Bold.ttf")
  list(regular = reg$path, bold = bld$path)
}
PUB_FONT_FILES <- .pub_resolve_arial()

# ---- colours -----------------------------------------------------------
# Okabe and Ito (2008) colourblind-safe palette.
pub_palette8 <- c(
  black = "#000000", orange = "#E69F00", sky_blue = "#56B4E9", bluish_green = "#009E73",
  yellow = "#F0E442", blue = "#0072B2", vermillion = "#D55E00", reddish_purple = "#CC79A7"
)

# Fixed meanings. Do not reassign per figure.
pub_colors <- c(
  in_network      = unname(pub_palette8["blue"]),
  out_of_network  = unname(pub_palette8["vermillion"]),
  orthopaedic     = unname(pub_palette8["bluish_green"]),
  control         = unname(pub_palette8["orange"]),
  neutral         = "#595959"
)
# Checked with colorspace (WP0, informational): minimum pairwise CIELAB (dE76) distance
# between the five colours is 33.4 normal vision, 18.4 deutan, 24.9 protan, 16.5 tritan.
# A mid grey (#7F7F7F) fell to 9.6 under deutan against orthopaedic, hence the darker grey.
# In greyscale blue (in_network) and #595959 are close in luminance, so use direct labels or
# line types as well as colour wherever those two share a panel.

# Human-readable legend labels (names match pub_colors). Machine names stay
# snake_case in the data; reviewers never see them.
pub_labels <- c(
  in_network      = "In-network",
  out_of_network  = "Out-of-network",
  orthopaedic     = "Orthopaedic",
  control         = "Control",
  neutral         = "Neutral"
)

.pub_merge_labels <- function(labels) {
  if (is.character(labels) && !is.null(names(labels)) && all(nzchar(names(labels)))) {
    return(c(labels, pub_labels[setdiff(names(pub_labels), names(labels))]))
  }
  labels
}

scale_colour_pub <- function(..., labels = pub_labels) {
  ggplot2::scale_colour_manual(values = pub_colors, labels = .pub_merge_labels(labels), ...)
}
scale_fill_pub <- function(..., labels = pub_labels) {
  ggplot2::scale_fill_manual(values = pub_colors, labels = .pub_merge_labels(labels), ...)
}

# ---- unit helpers ------------------------------------------------------
pt_to_mm <- function(pt) pt / ggplot2::.pt
# ggplot linewidth 1 = .pt R lwd units, and 1 lwd = 1/96 inch = 0.75 pt.
pt_to_lw <- function(pt) pt / (ggplot2::.pt * 0.75)

# ---- theme -------------------------------------------------------------
theme_pub <- function(base_size = 6, grid = FALSE) {
  if (base_size < 5.5 || base_size > 7) {
    warning("theme_pub(): base_size ", base_size, " puts some text outside the 5-7 pt spec", call. = FALSE)
  }
  fam <- .PUB_FAMILY
  txt <- ggplot2::element_text
  ln <- ggplot2::element_line
  th <- ggplot2::theme_classic(base_size = base_size, base_family = fam) +
    ggplot2::theme(
      text = txt(family = fam, size = base_size, colour = "black"),
      axis.text = txt(size = base_size - 0.5, colour = "black"),
      axis.title = txt(size = base_size, colour = "black"),
      axis.line = ln(colour = "black", linewidth = pt_to_lw(0.5)),
      axis.ticks = ln(colour = "black", linewidth = pt_to_lw(0.25)),
      axis.ticks.length = grid::unit(1.5, "pt"),
      strip.text = txt(size = base_size, face = "plain", colour = "black"),
      strip.background = ggplot2::element_blank(),
      legend.text = txt(size = base_size - 0.5, colour = "black"),
      legend.title = txt(size = base_size, colour = "black"),
      legend.position = "top",
      legend.key = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      legend.margin = ggplot2::margin(0, 0, 0, 0, "pt"),
      panel.background = ggplot2::element_rect(fill = NA, colour = NA),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank(),
      plot.caption = ggplot2::element_blank(),
      plot.tag = txt(family = fam, face = "bold", size = 8, colour = "black"),
      plot.tag.position = "topleft",
      plot.margin = ggplot2::margin(3, 3, 3, 3, "pt"),
      geom = ggplot2::element_geom(family = fam, fontsize = base_size - 0.5)
    )
  if (isTRUE(grid)) {
    th <- th + ggplot2::theme(
      panel.grid.major.y = ln(colour = "grey92", linewidth = pt_to_lw(0.25))
    )
  }
  th
}

panel_letter <- function(letter) {
  if (!is.character(letter) || length(letter) != 1L || nchar(letter) != 1L || is.na(letter)) {
    stop("panel_letter(): letter must be a single character", call. = FALSE)
  }
  list(
    ggplot2::labs(tag = tolower(letter)),
    ggplot2::theme(
      plot.tag = ggplot2::element_text(family = .PUB_FAMILY, face = "bold", size = 8, colour = "black"),
      plot.tag.position = "topleft"
    )
  )
}

# ---- export ------------------------------------------------------------
save_fig <- function(plot, name, width_mm = c(89, 183), height_mm, outdir = P$figures) {
  if (identical(as.numeric(width_mm), c(89, 183))) width_mm <- 89
  if (!is.numeric(width_mm) || length(width_mm) != 1L || is.na(width_mm)) {
    stop("save_fig(): width_mm must be a single number, 89 or 183", call. = FALSE)
  }
  if (!any(abs(width_mm - c(89, 183)) <= 0.5)) {
    stop("save_fig(): width_mm must be 89 or 183 (+/- 0.5), got ", width_mm, call. = FALSE)
  }
  if (missing(height_mm) || !is.numeric(height_mm) || length(height_mm) != 1L || is.na(height_mm)) {
    stop("save_fig(): height_mm is required (one number)", call. = FALSE)
  }
  if (height_mm <= 0 || height_mm > 247) {
    stop("save_fig(): height_mm must be in (0, 247], got ", height_mm, call. = FALSE)
  }
  if (!is.character(name) || length(name) != 1L || !grepl("^[A-Za-z0-9_.-]+$", name)) {
    stop("save_fig(): name must be a plain file stem (letters, digits, _ . -)", call. = FALSE)
  }
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  paths <- c(pdf = file.path(outdir, paste0(name, ".pdf")),
             png = file.path(outdir, paste0(name, ".png")),
             tiff = file.path(outdir, paste0(name, ".tiff")))
  w_in <- width_mm / 25.4
  h_in <- height_mm / 25.4
  draw <- function(open_dev) {
    open_dev()
    ok <- FALSE
    on.exit(if (!ok) grDevices::dev.off(), add = TRUE)
    print(plot)
    grDevices::dev.off()
    ok <- TRUE
  }
  draw(function() grDevices::cairo_pdf(paths[["pdf"]], width = w_in, height = h_in, family = .PUB_FAMILY))
  # whole pixels at 600 dpi (89 mm = 2102 px, 183 mm = 4323 px), so the width is within 0.5 px of exact
  w_px <- round(w_in * 600)
  h_px <- round(h_in * 600)
  draw(function() ragg::agg_png(paths[["png"]], width = w_px, height = h_px, units = "px",
                                res = 600, background = "white"))
  draw(function() ragg::agg_tiff(paths[["tiff"]], width = w_px, height = h_px, units = "px",
                                 res = 600, background = "white", compression = "lzw"))
  paths
}

# ---- font-size audit ---------------------------------------------------
.pub_theme_sizes <- function(p, label) {
  th <- ggplot2::theme_get() + p$theme
  rows <- list()
  for (nm in names(th)) {
    if (!inherits(th[[nm]], "element_text")) next
    r <- tryCatch(ggplot2::calc_element(nm, th), error = function(e) NULL)
    if (is.null(r) || !inherits(r, "element_text")) next
    sz <- r$size
    if (is.numeric(sz) && length(sz) == 1L && is.finite(sz)) {
      rows[[length(rows) + 1L]] <- data.frame(source = paste0(label, " theme:", nm), pt = sz, stringsAsFactors = FALSE)
    }
  }
  if (length(rows)) do.call(rbind, rows) else data.frame(source = character(), pt = numeric())
}

.pub_layer_sizes <- function(p, label) {
  b <- ggplot2::ggplot_build(p)
  rows <- list()
  for (i in seq_along(p$layers)) {
    g <- p$layers[[i]]$geom
    if (!any(grepl("Text|Label", class(g)))) next
    d <- b$data[[i]]
    if (!"size" %in% names(d)) next
    pt <- unique(d$size[is.finite(d$size)] * ggplot2::.pt)
    if (length(pt)) {
      rows[[length(rows) + 1L]] <- data.frame(source = paste0(label, " layer ", i, ":", class(g)[1]), pt = pt, stringsAsFactors = FALSE)
    }
  }
  if (length(rows)) do.call(rbind, rows) else data.frame(source = character(), pt = numeric())
}

.pub_annotation_sizes <- function(p, label) {
  th <- p$patches$annotation$theme
  rows <- list()
  if (!is.null(th)) {
    for (nm in names(th)) {
      el <- th[[nm]]
      if (inherits(el, "element_text") && is.numeric(el$size) && length(el$size) == 1L && is.finite(el$size)) {
        rows[[length(rows) + 1L]] <- data.frame(source = paste0(label, " annotation:", nm), pt = el$size, stringsAsFactors = FALSE)
      }
    }
  }
  if (length(rows)) do.call(rbind, rows) else data.frame(source = character(), pt = numeric())
}

.pub_font_table <- function(p, label = "plot") {
  out <- rbind(.pub_theme_sizes(p, label), .pub_layer_sizes(p, label))
  if (inherits(p, "patchwork")) {
    out <- rbind(out, .pub_annotation_sizes(p, label))
    subs <- p$patches$plots
    for (j in seq_along(subs)) out <- rbind(out, .pub_font_table(subs[[j]], paste0(label, "/", j)))
  }
  out
}

min_font_pt <- function(plot, detail = FALSE) {
  tab <- .pub_font_table(plot)
  if (!nrow(tab)) stop("min_font_pt(): no text sizes found", call. = FALSE)
  if (isTRUE(detail)) return(tab[order(tab$pt), , drop = FALSE])
  min(tab$pt)
}
