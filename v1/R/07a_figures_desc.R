# =====================================================================
# 07a_figures_desc.R : WP7a. Descriptive figures and table.
#   Fig 1  attrition ladder (claim lines and appeal requests, 2024 national totals)
#   Fig 2  (a) funnel plot of issuer in-network denial rates, (b) state tile grid
#   Table 2 denial-reason mix by plan type and metal level
# =====================================================================
# Inputs (read only): v1/results/desc_attrition.csv, desc_attrition_ratios.csv,
#   desc_issuer_funnel.csv, desc_funnel_limits_grid.csv, desc_funnel_summary.csv,
#   desc_state_rates.csv, desc_reason_mix.csv, desc_oon_exclusions.csv,
#   ledger_parts/ledger_kff.csv and ledger_desc.csv, data_clean/cms/issuer_state.csv
#   (one cross-check of the out-of-network total).
# Outputs: v1/figures/fig1.* fig2.* (pdf, png, tiff), figures/captions/fig1.txt fig2.txt,
#   tables/table2.csv .rds, tables/captions/table2.txt, config/state_tile_grid.csv,
#   results/ledger_parts/ledger_figdesc.csv (keys figd_), logs/wp7a_*.
# Run: Rscript "<root>/v1/R/07a_figures_desc.R"
#
# Decisions recorded here (also in the WP7a handoff notes):
#  - Fig 1 plots the published national totals (desc_attrition.csv = att_ ledger keys),
#    which include the 2 issuers whose out-of-network denied count exceeds received. The
#    caption gives the out-of-network denied total without them (figd_ key). Ratios per 1,000
#    denied claim lines use the matched issuer sets of desc_attrition_ratios.csv.
#  - Fig 2b shows the claims-weighted pooled state rate (desc_state_rates.csv pooled_denial_rate).
#  - Captions are written by this script; numbers in them are {{key|fmt}} placeholders only.
#  - The ledger is read from a scratch copy of the kff, desc and figdesc parts only, so a
#    ledger_ortho.csv written by the parallel harvest is never opened.
#
# Keep this file ASCII. Special characters come from intToUtf8().
# =====================================================================
if (!exists("P", inherits = TRUE)) {
  .args <- commandArgs(trailingOnly = FALSE)
  .f <- sub("^--file=", "", grep("^--file=", .args, value = TRUE)[1])
  .here <- dirname(normalizePath(gsub("~+~", " ", .f, fixed = TRUE)))
  source(file.path(.here, "00_setup.R"))
}
if (!exists("ledger_add", inherits = TRUE)) source(file.path(P$R, "ledger.R"))
if (!exists("theme_pub", inherits = TRUE)) source(file.path(P$R, "theme_pub.R"))
if (!exists("save_table", inherits = TRUE)) source(file.path(P$R, "helpers_tables.R"))
if (!exists("render_template", inherits = TRUE)) source(file.path(P$R, "helpers_docx.R"))
ensure_pkgs(c("ggplot2", "patchwork", "scales", "readr"))
suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
})

SCRIPT <- "07a_figures_desc.R"
PART <- "figdesc"
LOG <- file.path(P$logs, "wp7a_run.log")
lg <- function(..., level = "INFO") log_msg(..., level = level, file = LOG)
DAGGER <- intToUtf8(0x2020)
TXT_MM <- pt_to_mm(5.5)          # every in-plot text label is 5.5 pt
FAM <- "Arial"

# ---- input readers -------------------------------------------------------
.CC <- c(issuer_id = "character", state = "character", ids = "character")
rd_res <- function(f, dir = P$results) {
  path <- file.path(dir, f)
  hdr <- names(utils::read.csv(path, nrows = 1, check.names = FALSE))
  utils::read.csv(path, colClasses = .CC[names(.CC) %in% hdr], na.strings = c("", "NA"),
                  stringsAsFactors = FALSE, check.names = FALSE)
}

# Ledger from a scratch copy of the allowed parts (kff, desc, figdesc) only.
read_ledger_safe <- function() {
  d <- file.path(tempdir(), "wp7a_ledger_copy")
  unlink(d, recursive = TRUE)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  for (p in c("kff", "desc", "figdesc")) {
    f <- file.path(P$ledger_parts, paste0("ledger_", p, ".csv"))
    if (file.exists(f)) file.copy(f, file.path(d, basename(f)), overwrite = TRUE)
  }
  ledger_read_all(dir = d)
}
lget <- function(L, key) ledger_get(L, key)

# ---- number text helpers (built from data, never typed) ---------------------
fmt_count <- function(x) {
  vapply(as.numeric(x), function(v) {
    if (v >= 1e8) paste(formatC(v / 1e6, format = "f", digits = 0), "million")
    else if (v >= 1e7) paste(formatC(v / 1e6, format = "f", digits = 1), "million")
    else if (v >= 1e6) paste(formatC(v / 1e6, format = "f", digits = 2), "million")
    else formatC(v, format = "d", big.mark = ",")
  }, character(1))
}
fmt_ratio <- function(r) {
  vapply(as.numeric(r), function(v) {
    if (v >= 100) formatC(v, format = "f", digits = 0)
    else if (v >= 1) formatC(v, format = "f", digits = 2)
    else formatC(v, format = "f", digits = 3)
  }, character(1))
}
fmt_pct0 <- function(p) paste0(formatC(100 * p, format = "f", digits = 0), "%")
pretty_net <- c(in_network = "in-network", out_of_network = "out-of-network")

# =====================================================================
# FIG 1 : attrition ladder
# =====================================================================
FIG1_ROWS <- data.frame(
  panel = c(rep("a", 6), rep("b", 4)),
  measure = c(rep(c("claims_received", "claims_denied", "claims_resubmitted"), each = 2),
              "internal_appeals_filed", "internal_appeals_overturned",
              "external_appeals_filed", "external_appeals_overturned"),
  network = c(rep(c("in_network", "out_of_network"), 3), rep("not_split", 4)),
  ypos = c(7.35, 6.35, 5.0, 4.0, 2.65, 1.65, 3.35, 2.35, 1.0, 0.0),
  ratio_id = c(NA, NA, NA, NA,
               "resubmitted_per1000_denied_in_network", "resubmitted_per1000_denied_out_of_network",
               "internal_appeals_filed_per1000_denied_in_network",
               "internal_appeals_overturned_per1000_denied_in_network",
               "external_appeals_filed_per1000_denied_in_network",
               "external_appeals_overturned_per1000_denied_in_network"),
  stringsAsFactors = FALSE
)
FIG1_BASE <- c(a = 1e6, b = 1e3)

fig1_ledger_key <- function(measure, network) {
  if (network == "not_split") paste0("att_", measure) else paste0("att_", measure, "_", network)
}

# Returns the plotted data frame; stops if any plotted number differs from desc_attrition.csv
# or from the att_ ledger keys.
fig1_data <- function(att, ratios, L) {
  d <- FIG1_ROWS
  i <- match(paste(d$measure, d$network), paste(att$measure, att$network))
  if (anyNA(i)) stop("fig1_data(): missing measure/network row in desc_attrition.csv")
  d$total <- att$total[i]
  d$unit <- att$unit[i]
  d$label <- att$label[i]
  for (k in seq_len(nrow(d))) {
    lk <- fig1_ledger_key(d$measure[k], d$network[k])
    if (!isTRUE(all.equal(d$total[k], lget(L, lk), tolerance = 0))) {
      stop("fig1_data(): ", lk, " differs between desc_attrition.csv (", d$total[k], ") and the ledger (", lget(L, lk), ")")
    }
  }
  d$ylab <- ifelse(d$network == "not_split", d$label, paste0(d$label, " (", pretty_net[d$network], ")"))
  d$fill <- ifelse(d$network == "not_split", "neutral", d$network)
  d$base <- unname(FIG1_BASE[d$panel])
  d$vlab <- fmt_count(d$total)
  d$ratio <- NA_real_; d$ratio_n <- NA_integer_; d$rlab <- NA_character_
  for (k in which(!is.na(d$ratio_id))) {
    j <- match(d$ratio_id[k], ratios$ratio_id)
    if (is.na(j)) stop("fig1_data(): ratio_id not found: ", d$ratio_id[k])
    d$ratio[k] <- ratios$ratio[j]
    d$ratio_n[k] <- ratios$n_issuers_matched[j]
    kr <- paste0("att_ratio_", d$ratio_id[k])
    if (!isTRUE(all.equal(d$ratio[k], lget(L, kr), tolerance = 1e-9))) stop("fig1_data(): ratio differs from ledger key ", kr)
    if (!isTRUE(all.equal(as.numeric(d$ratio_n[k]), lget(L, paste0(kr, "_issuers")), tolerance = 0))) {
      stop("fig1_data(): matched issuer count differs from ledger key ", kr, "_issuers")
    }
    sl <- ratios$scale_label[j]
    if (d$network[k] == "not_split") sl <- sub("denied claim lines", "in-network denied claim lines", sl, fixed = TRUE)
    d$rlab[k] <- paste0(fmt_ratio(d$ratio[k]), " ", sl, " (", d$ratio_n[k], " issuers)")
  }
  d$has_ratio <- !is.na(d$rlab)
  d[order(d$panel, -d$ypos), ]
}

fig1_panel <- function(dd, brks, brk_labs, xmax, xlab, letter) {
  dd <- dd[order(dd$ypos), ]
  ggplot(dd) +
    geom_rect(aes(xmin = base, xmax = total, ymin = ypos - 0.28, ymax = ypos + 0.28, fill = fill)) +
    geom_text(aes(x = total * 1.12, y = ypos + ifelse(has_ratio, 0.18, 0), label = vlab),
              hjust = 0, vjust = 0.5, size = TXT_MM, family = FAM) +
    geom_text(data = dd[dd$has_ratio, ], aes(x = total * 1.12, y = ypos - 0.2, label = rlab),
              hjust = 0, vjust = 0.5, size = TXT_MM, family = FAM, colour = "grey25") +
    scale_fill_manual(values = pub_colors, guide = "none") +
    scale_x_log10(breaks = brks, labels = brk_labs, expand = c(0, 0)) +
    scale_y_continuous(breaks = dd$ypos, labels = dd$ylab, expand = expansion(add = 0.5)) +
    coord_cartesian(xlim = c(dd$base[1], xmax)) +
    labs(x = xlab, y = NULL) +
    theme_pub() +
    panel_letter(letter)
}

make_fig1 <- function(d) {
  da <- d[d$panel == "a", ]
  db <- d[d$panel == "b", ]
  pa <- fig1_panel(da, brks = c(1e6, 1e7, 1e8, 1e9),
                   brk_labs = c("1 million", "10 million", "100 million", "1 billion"),
                   xmax = 10^9.15, xlab = "Claim lines, 2024 (logarithmic scale)", letter = "a")
  pb <- fig1_panel(db, brks = c(1e3, 1e4, 1e5, 1e6),
                   brk_labs = c("1,000", "10,000", "100,000", "1 million"),
                   xmax = 1e7, xlab = "Appeal requests, 2024 (logarithmic scale)", letter = "b")
  list(a = pa, b = pb, fig = (pa / pb) + plot_layout(heights = c(6.7, 4.35)))
}

# =====================================================================
# FIG 2 : (a) funnel plot, (b) state tile grid
# =====================================================================
TILE_STATE <- c("AK", "ME", "VT", "NH",
                "WA", "ID", "MT", "ND", "MN", "IL", "WI", "MI", "NY", "RI", "MA",
                "OR", "NV", "WY", "SD", "IA", "IN", "OH", "PA", "NJ", "CT",
                "CA", "UT", "CO", "NE", "MO", "KY", "WV", "VA", "MD", "DE",
                "AZ", "NM", "KS", "AR", "TN", "NC", "SC", "DC",
                "OK", "LA", "MS", "AL", "GA",
                "HI", "TX", "FL")
TILE_ROW <- c(1, 1, 2, 2, rep(3, 11), rep(4, 10), rep(5, 10), rep(6, 8), rep(7, 5), 8, 8, 8)
TILE_COL <- c(1, 11, 10, 11, 1:11, 1:10, 1:10, 2:9, 4:8, 1, 4, 9)

build_tile_grid <- function() {
  nm <- c(stats::setNames(datasets::state.name, datasets::state.abb), DC = "District of Columbia")
  g <- data.frame(state = TILE_STATE, state_name = unname(nm[TILE_STATE]),
                  row = as.integer(TILE_ROW), col = as.integer(TILE_COL), stringsAsFactors = FALSE)
  g[order(g$row, g$col), ]
}

# Validation used by the script and the test. Returns a character vector of problems.
validate_tile_grid <- function(g, puf_states) {
  prob <- character(0)
  if (nrow(g) != 51L) prob <- c(prob, paste("rows:", nrow(g), "not 51"))
  if (anyDuplicated(g$state)) prob <- c(prob, "duplicate state code")
  if (anyDuplicated(paste(g$row, g$col))) prob <- c(prob, "two states share a cell")
  if (anyNA(g$state_name) || anyNA(g$row) || anyNA(g$col)) prob <- c(prob, "NA in grid")
  if (!all(c(datasets::state.abb, "DC") %in% g$state)) prob <- c(prob, "not all 50 states plus DC present")
  miss <- setdiff(puf_states, g$state)
  if (length(miss)) prob <- c(prob, paste("PUF states missing from grid:", paste(miss, collapse = ",")))
  at <- function(s) g[g$state == s, c("row", "col")]
  if (!(at("ME")$row == min(g$row) && at("ME")$col == max(g$col))) prob <- c(prob, "ME is not top right")
  if (!(at("AK")$row == min(g$row) && at("AK")$col == min(g$col))) prob <- c(prob, "AK is not top left")
  if (!(at("HI")$row == max(g$row) && at("HI")$col == min(g$col))) prob <- c(prob, "HI is not bottom left")
  prob
}

# Geographic neighbours that must sit within 2 tiles (Chebyshev) of each other.
TILE_NEIGHBOURS <- list(
  c("WA", "OR"), c("OR", "CA"), c("CA", "NV"), c("CA", "AZ"), c("NV", "UT"), c("UT", "CO"), c("CO", "NM"),
  c("AZ", "NM"), c("NM", "TX"), c("TX", "OK"), c("TX", "LA"), c("OK", "KS"), c("KS", "NE"), c("NE", "SD"),
  c("SD", "ND"), c("ND", "MN"), c("MN", "IA"), c("IA", "MO"), c("MO", "AR"), c("AR", "LA"), c("LA", "MS"),
  c("MS", "AL"), c("AL", "GA"), c("GA", "FL"), c("GA", "SC"), c("SC", "NC"), c("NC", "VA"), c("VA", "WV"),
  c("WV", "KY"), c("KY", "TN"), c("TN", "NC"), c("OH", "PA"), c("PA", "NY"), c("NY", "VT"), c("VT", "NH"),
  c("NH", "ME"), c("NH", "MA"), c("MA", "RI"), c("RI", "CT"), c("CT", "NY"), c("NJ", "PA"), c("MD", "DE"),
  c("MD", "VA"), c("MD", "DC"), c("IL", "IN"), c("IN", "OH"), c("MI", "OH"), c("WI", "MI"), c("WI", "IL"),
  c("ID", "MT"), c("MT", "WY"), c("WY", "CO"), c("WA", "ID"), c("OR", "ID"))

tile_neighbour_gaps <- function(g) {
  vapply(TILE_NEIGHBOURS, function(p) {
    a <- g[g$state == p[1], ]; b <- g[g$state == p[2], ]
    max(abs(a$row - b$row), abs(a$col - b$col))
  }, numeric(1))
}

# ---- Fig 2a ------------------------------------------------------------
FUNNEL_LEVELS <- c(l95 = 0.95, l998 = 0.998)
N_FUNNEL_LABELS <- 4L
# Hand-placed label anchors (data coordinates) so that labels sit in empty parts of the plot and do not
# cover points. The labelled issuers are the N_FUNNEL_LABELS farthest from the pooled rate in units of
# the additive limit width; the script stops if that rule picks different issuers.
FUNNEL_LABEL_POS <- data.frame(
  issuer_id = c("34826", "60536", "56707", "31195"),
  x = c(4.2e3, 3.0e5, 3.0e5, 1.7e6),
  y = c(0.372, 0.011, 0.033, 0.015),
  hjust = c(0, 1, 1, 0),
  wrap = c(22, 99, 99, 99),
  stringsAsFactors = FALSE)

fig2a_data <- function(fun, grid, summ, L) {
  fun$status <- ifelse(fun$outside_998_adjusted, "outside", "within")
  p0 <- summ$pooled_rate
  tau2 <- summ$tau2_winsorised
  if (!isTRUE(all.equal(p0, lget(L, "hetero_funnel_pooled_rate"), tolerance = 1e-9))) stop("pooled rate differs from ledger")
  if (!isTRUE(all.equal(tau2, lget(L, "hetero_funnel_tau2"), tolerance = 1e-9))) stop("tau2 differs from ledger")
  if (nrow(fun) != lget(L, "hetero_funnel_issuers")) stop("issuer count differs from ledger")
  if (sum(fun$outside_998_adjusted) != lget(L, "hetero_funnel_outside_998_adjusted")) stop("outside count differs from ledger")
  zc <- stats::qnorm(1 - (1 - FUNNEL_LEVELS) / 2)
  if (!isTRUE(all.equal(unname(zc), c(lget(L, "hetero_funnel_zcrit_95"), lget(L, "hetero_funnel_zcrit_998")), tolerance = 1e-9))) {
    stop("funnel levels do not match the ledger critical values")
  }
  fun$z_adj <- (fun$rate - p0) / sqrt(fun$se_binomial^2 + tau2)
  fun$label <- paste0(fun$issuer_name, " (", fun$state, ")")
  ext <- fun[order(-abs(fun$z_adj)), ][seq_len(N_FUNNEL_LABELS), ]
  if (!setequal(ext$issuer_id, FUNNEL_LABEL_POS$issuer_id)) {
    stop("fig2a_data(): the most extreme issuers are ", paste(sort(ext$issuer_id), collapse = ","),
         " but the label placement table holds ", paste(sort(FUNNEL_LABEL_POS$issuer_id), collapse = ","))
  }
  ext <- merge(ext, FUNNEL_LABEL_POS, by = "issuer_id", sort = FALSE)
  ext$label_wrapped <- mapply(function(s, w) paste(strwrap(s, width = w), collapse = "\n"), ext$label, ext$wrap)
  ext$seg_x <- ifelse(ext$hjust == 0, ext$x / 1.08, ext$x * 1.08)
  cl <- function(x) pmin(1, pmax(0, x))
  grid$l95_lo <- cl(grid$limit_95_lower_adjusted); grid$l95_hi <- cl(grid$limit_95_upper_adjusted)
  grid$l998_lo <- cl(grid$limit_998_lower_adjusted); grid$l998_hi <- cl(grid$limit_998_upper_adjusted)
  lev <- paste0(formatC(100 * FUNNEL_LEVELS, format = "fg", digits = 3), "%")
  end <- grid[which.max(grid$received), ]
  ends <- data.frame(
    y = c(end$l998_hi, end$l95_hi, p0, end$l95_lo, end$l998_lo),
    label = c(paste(lev[2], "limit"), paste(lev[1], "limit"),
              paste0("Pooled rate\n", formatC(100 * p0, format = "f", digits = 1), "%"),
              paste(lev[1], "limit"), paste(lev[2], "limit")),
    stringsAsFactors = FALSE)
  list(fun = fun, ext = ext, grid = grid, ends = ends, p0 = p0, x_end = end$received, lev = lev)
}

make_fig2a <- function(d) {
  fun <- d$fun
  n_out <- sum(fun$status == "outside"); n_in <- sum(fun$status == "within")
  fun$status <- factor(fun$status, levels = c("outside", "within"))
  lab_out <- paste0("Outside the ", d$lev[2], " limits (", n_out, " issuers)")
  lab_in <- paste0("Within the ", d$lev[2], " limits (", n_in, " issuers)")
  lw <- pt_to_lw(0.5)
  ggplot() +
    geom_line(data = d$grid, aes(received, l95_lo), linetype = "22", linewidth = lw, colour = "black") +
    geom_line(data = d$grid, aes(received, l95_hi), linetype = "22", linewidth = lw, colour = "black") +
    geom_line(data = d$grid, aes(received, l998_lo), linewidth = lw, colour = "black") +
    geom_line(data = d$grid, aes(received, l998_hi), linewidth = lw, colour = "black") +
    geom_hline(yintercept = d$p0, linewidth = lw, colour = "grey45") +
    geom_point(data = fun, aes(received, rate, fill = status), shape = 21, size = 1.3, stroke = 0.3,
               colour = pub_colors[["in_network"]]) +
    geom_segment(data = d$ext, aes(x = seg_x, y = y, xend = received, yend = rate),
                 linewidth = pt_to_lw(0.25), colour = "grey30") +
    geom_text(data = d$ext, aes(x = x, y = y, label = label_wrapped, hjust = hjust), vjust = 0.5,
              size = TXT_MM, family = FAM, lineheight = 0.9, colour = "black") +
    geom_text(data = d$ends, aes(x = d$x_end * 1.2, y = y, label = label), hjust = 0, vjust = 0.5,
              size = TXT_MM, family = FAM, colour = "black", lineheight = 0.9) +
    scale_fill_manual(values = c(outside = pub_colors[["in_network"]], within = "white"),
                      labels = c(outside = lab_out, within = lab_in), name = NULL) +
    scale_x_log10(breaks = c(1e3, 1e4, 1e5, 1e6, 1e7),
                  labels = c("1,000", "10,000", "100,000", "1 million", "10 million")) +
    scale_y_continuous(labels = function(x) paste0(formatC(100 * x, format = "f", digits = 0), "%"),
                       breaks = seq(0, 0.4, 0.1)) +
    coord_cartesian(xlim = c(150, 8e7), ylim = c(0, 0.4), expand = FALSE, clip = "off") +
    labs(x = "In-network claim lines received (logarithmic scale)", y = "In-network denial rate") +
    theme_pub() +
    theme(plot.margin = margin(3, 34, 3, 3, "pt")) +
    panel_letter("a")
}

# ---- Fig 2b ------------------------------------------------------------
TILE_LOW <- "#E3EFF7"
TILE_HIGH <- "#0072B2"

fig2b_data <- function(grid, st, L) {
  d <- merge(grid, st, by = "state", all.x = TRUE, sort = FALSE)
  d$has_data <- !is.na(d$pooled_denial_rate)
  d$single <- d$has_data & d$n_issuers == 1
  d$pct <- ifelse(d$has_data, fmt_pct0(d$pooled_denial_rate), NA_character_)
  d$tile_label <- ifelse(d$has_data, paste0(d$state, ifelse(d$single, DAGGER, ""), "\n", d$pct), d$state)
  if (sum(d$has_data) != lget(L, "hetero_states")) stop("number of states with data differs from ledger")
  if (!isTRUE(all.equal(min(d$pooled_denial_rate, na.rm = TRUE), lget(L, "hetero_state_pooled_rate_min"), tolerance = 1e-9))) stop("state min differs from ledger")
  if (!isTRUE(all.equal(max(d$pooled_denial_rate, na.rm = TRUE), lget(L, "hetero_state_pooled_rate_max"), tolerance = 1e-9))) stop("state max differs from ledger")
  if (sum(d$single) != lget(L, "hetero_states_single_issuer")) stop("single-issuer state count differs from ledger")
  rng <- range(d$pooled_denial_rate, na.rm = TRUE)
  pal <- scales::seq_gradient_pal(TILE_LOW, TILE_HIGH)
  fill_hex <- pal(scales::rescale(d$pooled_denial_rate, from = c(0.05, 0.30)))
  rgbm <- grDevices::col2rgb(fill_hex) / 255
  lin <- function(u) ifelse(u <= 0.03928, u / 12.92, ((u + 0.055) / 1.055)^2.4)
  lum <- 0.2126 * lin(rgbm[1, ]) + 0.7152 * lin(rgbm[2, ]) + 0.0722 * lin(rgbm[3, ])
  d$txt_col <- ifelse(!d$has_data, "grey55", ifelse(lum < 0.30, "white", "black"))
  d$x <- d$col
  d$y <- max(d$row) + 1 - d$row
  d
}

make_fig2b <- function(d) {
  has <- d[d$has_data, ]; no <- d[!d$has_data, ]
  ggplot() +
    geom_tile(data = no, aes(x, y), width = 0.94, height = 0.94, fill = "white", colour = "grey70",
              linewidth = pt_to_lw(0.4)) +
    geom_tile(data = has, aes(x, y, fill = pooled_denial_rate), width = 0.94, height = 0.94, colour = NA) +
    geom_text(data = no, aes(x, y, label = tile_label), size = TXT_MM, family = FAM, colour = "grey55",
              lineheight = 0.85) +
    geom_text(data = has, aes(x, y, label = tile_label, colour = txt_col), size = TXT_MM, family = FAM,
              lineheight = 0.85) +
    scale_colour_identity() +
    scale_fill_gradient(low = TILE_LOW, high = TILE_HIGH, limits = c(0.05, 0.30),
                        breaks = seq(0.05, 0.30, 0.05),
                        labels = function(x) paste0(formatC(100 * x, format = "f", digits = 0), "%"),
                        name = "In-network denial rate, claim-weighted",
                        guide = guide_colourbar(direction = "horizontal", title.position = "top",
                                                theme = theme(legend.key.width = unit(46, "mm"),
                                                              legend.key.height = unit(2.5, "mm")),
                                                ticks.colour = "black", ticks.linewidth = 0.3)) +
    scale_x_continuous(expand = expansion(add = 0.1)) +
    scale_y_continuous(expand = expansion(add = 0.1)) +
    coord_fixed() +
    theme_pub() +
    theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
          axis.title = element_blank(), legend.position = "inside", legend.position.inside = c(0.46, 0.86),
          legend.justification = "center", legend.title.position = "top",
          legend.title = element_text(size = 6, hjust = 0.5)) +
    panel_letter("b")
}

make_fig2 <- function(d2a, d2b) {
  pa <- make_fig2a(d2a); pb <- make_fig2b(d2b)
  list(a = pa, b = pb, fig = (pa | pb) + plot_layout(widths = c(90, 93)))
}

# =====================================================================
# TABLE 2 : denial-reason mix
# =====================================================================
T2_STRATA <- c("All plans", "EPO", "HMO", "POS", "PPO", "Bronze", "Silver", "Gold", "Platinum", "Catastrophic")
T2_SMALL <- 100L
T2_WIDTHS <- c(2.55, 0.62, 0.5, 0.5, 0.5, 0.5, 0.62, 0.6, 0.5, 0.7, 0.9)

# Returns list(df = display data.frame, shares = numeric matrix reasons x strata, totals = tally totals)
table2_data <- function(mix, L) {
  mix <- mix[mix$stratum %in% T2_STRATA, ]
  reasons <- unique(mix$reason[mix$stratum == "All plans"])
  if (length(reasons) != 10L) stop("table2_data(): expected 10 reasons, found ", length(reasons))
  labs <- mix$label[match(reasons, mix$reason)]
  sh <- matrix(NA_real_, 10, length(T2_STRATA), dimnames = list(reasons, T2_STRATA))
  tot <- stats::setNames(rep(NA_real_, length(T2_STRATA)), T2_STRATA)
  npl <- stats::setNames(rep(NA_real_, length(T2_STRATA)), T2_STRATA)
  for (s in T2_STRATA) {
    m <- mix[mix$stratum == s, ]
    sh[, s] <- m$share_of_ten_reason_total[match(reasons, m$reason)]
    tot[s] <- unique(m$ten_reason_total)
    npl[s] <- unique(m$n_plans)
  }
  if (anyNA(sh)) stop("table2_data(): NA share (a stratum is missing a reason)")
  if (!isTRUE(all.equal(unname(tot["All plans"]), lget(L, "reason_ten_reason_total_all"), tolerance = 0))) stop("tally total differs from ledger")
  for (r in reasons) {
    key <- paste0("reason_share_", sub("^plan_reason_", "", r), "_all")
    if (!isTRUE(all.equal(unname(sh[r, "All plans"]), lget(L, key), tolerance = 1e-9))) stop("All plans share differs from ledger key ", key)
  }
  small <- names(npl)[npl < T2_SMALL]
  if (!setequal(small, c("Platinum", "Catastrophic"))) {
    stop("table2_data(): strata with fewer than ", T2_SMALL, " plans are ", paste(small, collapse = ","),
         ", but the footnote names Platinum and Catastrophic")
  }
  cell <- function(x) formatC(100 * x, format = "f", digits = 1)
  df <- data.frame(`Denial reason` = c(labs, "Total reason tallies, millions"), check.names = FALSE, stringsAsFactors = FALSE)
  for (s in T2_STRATA) df[[s]] <- c(cell(sh[, s]), formatC(tot[s] / 1e6, format = "f", digits = 2))
  list(df = df, shares = sh, totals = tot, n_plans = npl)
}

# =====================================================================
# CAPTIONS (placeholders only; the docx builder numbers figures and tables)
# =====================================================================
CAP_FIG1 <- c(
  "Claim lines and appeal requests reported by Individual market qualified health plan issuers, 2024, national totals",
  paste0("(a) Claim lines received, denied and resubmitted, in-network and out-of-network. (b) Appeal requests filed and overturned; ",
         "appeals are not split by network and are counted in appeal requests, not claim lines, so the panels use different units and different axes. ",
         "The bars are not a pathway: resubmitted claim lines are all lines resubmitted regardless of outcome and are not a subset of denied lines, ",
         "and appeals are those completed in 2024. Bars are on a logarithmic axis and start at the left edge of each panel, so bar length is not proportional to the count; read the labels."),
  paste0("Totals add the numeric values that issuers reported and are lower bounds: of {{att_issuers_in_puf|int}} issuers in the file, ",
         "{{att_issuers_na_issuer_level|int}} have no issuer-level data (new to the Exchange), and suppressed cells are left out, not imputed ",
         "(for example, {{att_external_appeals_filed_issuers_suppressed|int}} issuers have a suppressed count of external appeals filed). ",
         "Out-of-network totals are descriptive only and are as published: they include {{att_oon_anomalous_issuers|int}} issuers whose out-of-network denied count exceeds claims received, ",
         "without which out-of-network denied claim lines fall from {{att_claims_denied_out_of_network|mil1}} million to {{figd_oon_denied_excl_anomalous|mil1}} million."),
  paste0("Smaller text gives the ratio per thousand denied claim lines, with the number of issuers in the matched set that reports both the numerator and the denominator. ",
         "It is a ratio, not a share. The denominator is in-network denied claim lines, except for out-of-network resubmissions, which use out-of-network denied claim lines (without the anomalous issuers).")
)

CAP_FIG2 <- c(
  "Variation in in-network denial rates across issuers and states, 2024",
  paste0("(a) Funnel plot of the in-network denial rate against in-network claim lines received, each point is an issuer ({{hetero_funnel_issuers|int}} issuers with reported counts). ",
         "The gray line is the pooled rate ({{hetero_funnel_pooled_rate|pct1}}). Dashed and solid black curves are the {{figd_funnel_level_95|pct0}} and {{figd_funnel_level_998|pct1}} limits, widened by an additive between-issuer variance term ",
         "(standard deviation {{figd_funnel_tau_pp|n1}} percentage points) and clipped to the possible range of a rate; they are analyst choices described in the Methods, not published thresholds. ",
         "Filled points lie outside the {{figd_funnel_level_998|pct1}} limits: {{hetero_funnel_outside_998_adjusted|int}} of {{hetero_funnel_issuers|int}} issuers ",
         "({{hetero_funnel_above_998_adjusted|int}} above, {{hetero_funnel_below_998_adjusted|int}} below). Without the additive term, {{hetero_funnel_outside_998_unadjusted|int}} issuers would lie outside. ",
         "The count depends on the Winsorizing choice ({{hetero_funnel_winsor_probability|pct0}} of z scores in each tail): without Winsorizing the additive standard deviation is {{figd_funnel_tau_unwins_pp|n1}} percentage points ",
         "and {{hetero_funnel_outside_998_adjusted_unwinsorised|int}} issuers lie outside the {{figd_funnel_level_998|pct1}} limits. The {{figd_n_labelled|int}} labeled issuers are the farthest from the pooled rate in units of the limit width; the label gives the issuer and its state as a postal code."),
  paste0("(b) State tile grid of the claim-weighted pooled in-network denial rate: denied claim lines divided by claim lines received, summed over the issuers with reported counts in each state. ",
         "It is the share of claim lines denied in the state, not the average of issuer rates, which differs in some states. ",
         "State rates range from {{hetero_state_pooled_rate_min|pct1}} ({{figd_state_lowest_name|str}}) to {{hetero_state_pooled_rate_max|pct1}} ({{figd_state_highest_name|str}}) across {{hetero_states|int}} states. ",
         "These are the states in which issuers filed Individual market data in the CMS file; all {{att_issuers_in_puf|int}} issuers in the file are on the federal platform ",
         "({{figd_issuers_ffe|int}} on the federally facilitated exchange, {{figd_issuers_spe|int}} on state partnership exchanges and {{figd_issuers_sbefp|int}} on state-based exchanges using the federal platform). ",
         "Blank outlined tiles are states with no issuer in the file. A dagger marks the state whose rate rests on a single issuer, {{figd_single_issuer_state_name|str}}; tiles show postal codes.")
)

CAP_TAB2 <- c(
  "Denial reasons recorded by plan type and metal level, Individual market qualified health plans, 2024",
  "Cells are the percent of the total of the denial-reason tallies in each column, taken from reported plan-reason cells over the {{cohort_all_plans_computable|int}} cohort plans (plans with a computable in-network denial proportion); the shares in a column add to the whole apart from rounding. The last row is the column total of reason tallies in millions.",
  "EPO (exclusive provider organization), HMO (health maintenance organization), POS (point of service) and PPO (preferred provider organization) are plan types; Bronze, Silver, Gold, Platinum and Catastrophic are metal levels.",
  "Reasons are tallies of denial reasons recorded over the life of claims, not unique denied claims, and they do not sum to the number of denied claims.",
  "Suppressed (**) cells are missing and are left out of every tally: {{reason_suppressed_cells_all|int}} of {{reason_cells_all|int}} plan-reason cells were suppressed, so shares rest on reported cells only.",
  "The cohort plans include {{cohort_all_plans_platinum|int}} Platinum, {{cohort_all_plans_catastrophic|int}} Catastrophic and {{cohort_all_plans_pos|int}} POS plans; shares for strata with fewer than {{figd_small_stratum_threshold|int}} plans (Platinum and Catastrophic) are imprecise.",
  "Without the out-of-network provider reason, other reason is {{reason_share_other_of_nine_all|pct1}} of the all-plans total."
)

# Render every caption line through the real renderer; returns the rendered text (stops on any failure).
render_caption_check <- function(lines, L) {
  tf <- tempfile(fileext = ".txt")
  on.exit(unlink(tf))
  writeLines(paste(lines, collapse = "\n\n"), tf)
  refs <- data.frame(id = character(0), pmid = character(0), vancouver = character(0), stringsAsFactors = FALSE)
  render_template(tf, L, refs, section = "caption")
}

# =====================================================================
# MAIN
# =====================================================================
run_main <- function() {
  lg("07a_figures_desc.R start")
  ledger_reset()
  att <- rd_res("desc_attrition.csv"); ratios <- rd_res("desc_attrition_ratios.csv")
  fun <- rd_res("desc_issuer_funnel.csv"); grid <- rd_res("desc_funnel_limits_grid.csv")
  summ <- rd_res("desc_funnel_summary.csv"); st <- rd_res("desc_state_rates.csv")
  mix <- rd_res("desc_reason_mix.csv"); oon <- rd_res("desc_oon_exclusions.csv")

  # ---- ledger part figdesc: numbers no existing key holds ----
  iss <- utils::read.csv(file.path(P$clean_cms, "issuer_state.csv"), colClasses = c(issuer_id = "character"),
                         stringsAsFactors = FALSE, na.strings = "")
  anom <- strsplit(oon$ids[oon$scope == "issuers"], ";", fixed = TRUE)[[1]]
  ok <- iss$issuer_claims_denied_out_of_network_marker == "numeric" & !(iss$issuer_id %in% anom)
  oon_denied_excl <- sum(as.numeric(iss$issuer_claims_denied_out_of_network[ok]))
  rr <- ratios[ratios$ratio_id == "denial_rate_out_of_network", ]
  if (!isTRUE(all.equal(oon_denied_excl, rr$numerator_sum, tolerance = 0))) {
    stop("out-of-network denied total without the anomalous issuers differs between issuer_state.csv (", oon_denied_excl,
         ") and desc_attrition_ratios.csv (", rr$numerator_sum, ")")
  }
  ledger_add(PART, "figd_oon_denied_excl_anomalous", oon_denied_excl, "claim lines",
             "Out-of-network claim lines denied, reported cells, leaving out the 2 issuers whose denied count exceeds received (Fig 1 caption)", SCRIPT)
  ledger_add(PART, "figd_funnel_tau_pp", 100 * summ$tau_winsorised, "percentage points",
             "Additive between-issuer standard deviation of the funnel limits, Winsorised, in percentage points of the denial rate", SCRIPT)
  ledger_add(PART, "figd_funnel_tau_unwins_pp", 100 * sqrt(summ$tau2_unwinsorised), "percentage points",
             "Same without Winsorising the z scores (sensitivity), in percentage points", SCRIPT)
  ledger_add(PART, "figd_funnel_level_95", FUNNEL_LEVELS[["l95"]], "proportion", "Inner funnel limit level (Fig 2 caption)", SCRIPT)
  ledger_add(PART, "figd_funnel_level_998", FUNNEL_LEVELS[["l998"]], "proportion", "Outer funnel limit level (Fig 2 caption)", SCRIPT)
  tg0 <- build_tile_grid()
  nm_state <- function(code) tg0$state_name[match(code, tg0$state)]
  ledger_add(PART, "figd_state_lowest_name", nm_state(st$state[which.min(st$pooled_denial_rate)]), "state name",
             "Full name of the state with the lowest pooled in-network denial rate (Fig 2 caption; the tiles keep postal codes)", SCRIPT)
  ledger_add(PART, "figd_state_highest_name", nm_state(st$state[which.max(st$pooled_denial_rate)]), "state name",
             "Full name of the state with the highest pooled in-network denial rate (Fig 2 caption)", SCRIPT)
  ledger_add(PART, "figd_single_issuer_state_name", nm_state(st$state[st$n_issuers == 1]), "state name",
             "Full name of the state whose pooled rate rests on a single issuer (dagger in Fig 2b)", SCRIPT)
  xt <- table(factor(iss$exchange_type, levels = c("FFE", "SPE", "SBE-FP")))
  if (sum(xt) != nrow(iss) || !all(iss$state %in% st$state)) stop("exchange_type has a value other than FFE, SPE, SBE-FP, or an issuer sits outside the 30 states")
  ledger_add(PART, "figd_issuers_ffe", unname(xt[["FFE"]]), "issuers", "Issuers in the file on the federally facilitated exchange (exchange_type FFE; Fig 2 caption)", SCRIPT)
  ledger_add(PART, "figd_issuers_spe", unname(xt[["SPE"]]), "issuers", "Issuers in the file on a state partnership exchange (exchange_type SPE)", SCRIPT)
  ledger_add(PART, "figd_issuers_sbefp", unname(xt[["SBE-FP"]]), "issuers", "Issuers in the file on a state-based exchange using the federal platform (exchange_type SBE-FP)", SCRIPT)
  ledger_add(PART, "figd_n_labelled", N_FUNNEL_LABELS, "issuers", "Issuers labelled by name in Fig 2a", SCRIPT)
  ledger_add(PART, "figd_small_stratum_threshold", T2_SMALL, "plans",
             "Strata with fewer plans than this get the imprecision note in Table 2", SCRIPT)
  ledger_write(PART)
  L <- read_ledger_safe()
  lg("ledger read from a scratch copy of kff, desc and figdesc: ", nrow(L), " keys")
  if (!identical(lget(L, "hetero_state_lowest"), st$state[which.min(st$pooled_denial_rate)]) ||
      !identical(lget(L, "hetero_state_highest"), st$state[which.max(st$pooled_denial_rate)])) stop("lowest or highest state differs from the ledger")
  if (sum(xt) != lget(L, "att_issuers_in_puf")) stop("issuers by exchange type do not add to att_issuers_in_puf")

  # ---- tile grid ----
  grid_tile <- build_tile_grid()
  pr <- validate_tile_grid(grid_tile, st$state)
  if (length(pr)) stop("tile grid invalid: ", paste(pr, collapse = "; "))
  readr::write_csv(grid_tile, file.path(P$config, "state_tile_grid.csv"))
  gaps <- tile_neighbour_gaps(grid_tile)
  lg("state_tile_grid.csv written: 51 cells; ", sum(gaps > 2), " of ", length(gaps), " neighbour pairs farther than 2 tiles apart")

  # ---- Fig 1 ----
  d1 <- fig1_data(att, ratios, L)
  readr::write_csv(d1[, c("panel", "measure", "network", "total", "vlab", "ratio", "ratio_n", "rlab")],
                   file.path(P$logs, "wp7a_fig1_plotted.csv"))
  f1 <- make_fig1(d1)
  lg("Fig 1 min font (pt): ", round(min_font_pt(f1$fig), 2))
  h1 <- 106
  p1 <- save_fig(f1$fig, "fig1", width_mm = 183, height_mm = h1)
  lg("Fig 1 saved: ", paste(basename(p1), collapse = ", "))

  # ---- Fig 2 ----
  d2a <- fig2a_data(fun, grid, summ, L)
  d2b <- fig2b_data(grid_tile, st, L)
  readr::write_csv(d2a$ext[, c("issuer_id", "state", "issuer_name", "received", "rate", "z_adj")],
                   file.path(P$logs, "wp7a_fig2a_labelled.csv"))
  f2 <- make_fig2(d2a, d2b)
  lg("Fig 2 min font (pt): ", round(min_font_pt(f2$fig), 2))
  h2 <- 78
  p2 <- save_fig(f2$fig, "fig2", width_mm = 183, height_mm = h2)
  lg("Fig 2 saved: ", paste(basename(p2), collapse = ", "))

  # ---- captions ----
  dir.create(P$fig_captions, showWarnings = FALSE, recursive = TRUE)
  dir.create(P$tab_captions, showWarnings = FALSE, recursive = TRUE)
  for (cp in list(list(f = file.path(P$fig_captions, "fig1.txt"), x = CAP_FIG1),
                  list(f = file.path(P$fig_captions, "fig2.txt"), x = CAP_FIG2),
                  list(f = file.path(P$tab_captions, "table2.txt"), x = CAP_TAB2))) {
    render_caption_check(cp$x, L)
    writeLines(cp$x, cp$f)
  }
  lg("captions written; every placeholder resolves through render_template")

  # ---- Table 2 ----
  t2 <- table2_data(mix, L)
  save_table(t2$df, "table2", T2_WIDTHS, footnote = NULL, landscape = TRUE,
             align = c("left", rep("right", 10)), bold_rows = nrow(t2$df))
  lg("Table 2 saved: ", nrow(t2$df), " rows, ", ncol(t2$df), " columns, widths sum ", sum(T2_WIDTHS), " in")
  lg("07a_figures_desc.R done")
  invisible(list(fig1 = f1, fig2 = f2, table2 = t2, ledger = L))
}

if (!isTRUE(getOption("psd_figdesc_no_run"))) {
  res <- run_main()
}
