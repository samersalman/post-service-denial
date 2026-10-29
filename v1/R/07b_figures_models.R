# =====================================================================
# 07b_figures_models.R : WP7b. Fig 3, Table 1, Table 3, Supplementary Table 1.
#   Fig 3   average marginal predicted in-network denial probability against log2 density
#           (orthopaedic curve, dermatology negative-control curve, 8 equal-count bins)
#   Table 1 cohort flow and characteristics of the analytic sample
#   Table 3 specification matrix (primary row plus sens_a to sens_l)
#   Supplementary Table 1 (sens_m to sens_t)
# =====================================================================
# Draw and tabulate what WP6b produced; nothing is re-estimated or recomputed from raw data.
# Inputs (read only): v1/results/model_fig3_curve.csv, model_fig3_bins.csv, model_sensitivity_rows.csv,
#   model_table1_ladder.csv, model_table1_data.csv, model_cohort_flow.csv, model_sens_leave_one_out.csv
#   (cross-check only), the ledger parts via ledger_read_all() (what a re-merge would give) and
#   numbers_ledger.csv (consistency check only).
# Outputs: v1/figures/fig3.pdf .png .tiff, figures/captions/fig3.txt, tables/table1 table3 tableS1 (.csv .rds),
#   tables/captions/table1.txt table3.txt tableS1.txt, results/ledger_parts/ledger_figmodels.csv (keys figm_),
#   logs/wp7b_*.
# Run: cd v1 ; Rscript R/07b_figures_models.R
#
# Decisions recorded here (also in the handoff notes):
#  - Every table cell number is formatted with fmt_ledger_value() (round half up, same rules as the prose):
#    odds ratios and limits with 3 decimals, percentages with 1 decimal, counts with thousands separators.
#  - Table 1 shows the median of the surgeon count only: model_table1_data.csv carries no quartiles for it.
#  - Captions and footnotes carry no typed number: every number is a {{key|fmt}} placeholder.
#  - Supplementary Table 1 has a Detail column instead of the Holm column (no supplement row is in the Holm
#    family); the sens_s row shows the full-sample estimate in the OR column and the leave-out range in Detail.
#
# Keep this file ASCII. Special characters come from intToUtf8().
# =====================================================================
if (!exists("P", inherits = TRUE)) {
  .args <- commandArgs(trailingOnly = FALSE)
  .f <- sub("^--file=", "", grep("^--file=", .args, value = TRUE)[1])
  .here <- if (is.na(.f)) getwd() else dirname(normalizePath(gsub("~+~", " ", .f, fixed = TRUE)))
  .setup <- file.path(.here, "00_setup.R")
  if (!file.exists(.setup)) .setup <- file.path(dirname(.here), "R", "00_setup.R")
  if (!file.exists(.setup)) .setup <- file.path(getwd(), "R", "00_setup.R")
  source(.setup)
}
if (!exists("ledger_add", inherits = TRUE)) source(file.path(P$R, "ledger.R"))
if (!exists("theme_pub", inherits = TRUE)) source(file.path(P$R, "theme_pub.R"))
if (!exists("save_table", inherits = TRUE)) source(file.path(P$R, "helpers_tables.R"))
if (!exists("render_template", inherits = TRUE)) source(file.path(P$R, "helpers_docx.R"))
# reuse the WP7a helpers (rd_res, render_caption_check, TXT_MM, FAM) without running that script
options(psd_figdesc_no_run = TRUE)
source(file.path(P$R, "07a_figures_desc.R"))
options(psd_figdesc_no_run = NULL)
ensure_pkgs(c("ggplot2", "scales", "readr"))
suppressPackageStartupMessages(library(ggplot2))

SCRIPT <- "07b_figures_models.R"
PART <- "figmodels"
LOG <- file.path(P$logs, "wp7b_run.log")
lg <- function(..., level = "INFO") log_msg(..., level = level, file = LOG)
NB <- intToUtf8(0x00A0)            # non-breaking space, used to indent table rows
INDENT <- strrep(NB, 3)

# ---- formatting helpers (same rounding as the prose: fmt_ledger_value) -----------------
fl <- function(L, key, fmt) {
  i <- match(key, L$key)
  if (is.na(i)) stop("ledger key missing: ", key, call. = FALSE)
  fmt_ledger_value(L$value_num[i], L$value[i], fmt, key)
}
fv <- function(x, fmt) fmt_ledger_value(x, as.character(x), fmt)
or_ci <- function(or, lo, hi) paste0(fv(or, "n3"), " (", fv(lo, "n3"), " to ", fv(hi, "n3"), ")")
mil2 <- function(x) if (x < 5000) "<0.01" else fv(x / 1e6, "n2")        # millions, 2 decimals; tiny counts as <0.01
med_iqr <- function(q25, med, q75) paste0(fv(med, "int"), " (", fv(q25, "int"), " to ", fv(q75, "int"), ")")
same <- function(a, b, tol = 1e-9) isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = tol, check.attributes = FALSE))
chk_ledger <- function(L, key, x, tol = 1e-9) {
  v <- L$value_num[match(key, L$key)]
  if (is.na(v) || !same(v, x, tol)) stop("CSV value ", format(x, digits = 15), " differs from ledger key ", key, " (", v, ")", call. = FALSE)
  invisible(TRUE)
}

# =====================================================================
# LEDGER PART figmodels (numbers used by the Fig 3 caption and not already ledger keys)
# =====================================================================
f3_extra_keys <- function(cv, bn, L0) {
  ctrl <- cv[cv$curve == "control", ]
  ledger_reset(PART)
  lo_q <- ledger_get(L0, "design_fig3_lower_quantile"); hi_q <- ledger_get(L0, "design_fig3_upper_quantile")
  ledger_add(PART, "figm_curve_lower_percentile", round(100 * lo_q, 8), "percentile",
             "Lower percentile of log2 density where the Fig 3 curves start (design_fig3_lower_quantile times 100)", SCRIPT)
  ledger_add(PART, "figm_curve_upper_percentile", round(100 * hi_q, 8), "percentile",
             "Upper percentile of log2 density where the Fig 3 curves end (design_fig3_upper_quantile times 100)", SCRIPT)
  ledger_add(PART, "figm_derm_density_p05", min(ctrl$density_per_10000), "dermatology clinicians per 10,000 reported enrollees",
             "Dermatology density at the start of the negative-control curve in Fig 3 (its own lower percentile)", SCRIPT)
  ledger_add(PART, "figm_derm_density_p95", max(ctrl$density_per_10000), "dermatology clinicians per 10,000 reported enrollees",
             "Dermatology density at the end of the negative-control curve in Fig 3 (its own upper percentile)", SCRIPT)
  # constant probabilities that fit inside both bands over each curve's own plotted range
  lo_all <- max(tapply(cv$ci_low, cv$curve, max)); hi_all <- min(tapply(cv$ci_high, cv$curve, min))
  if (!(lo_all < hi_all)) stop("f3_extra_keys(): no constant probability fits inside both bands; the caption sentence about a horizontal line must be dropped")
  ledger_add(PART, "figm_flat_line_lower", lo_all, "probability",
             "Lowest constant probability that stays inside both Fig 3 bands over each curve's plotted range (largest lower band limit of the two curves)", SCRIPT)
  ledger_add(PART, "figm_flat_line_upper", hi_all, "probability",
             "Highest constant probability that stays inside both Fig 3 bands over each curve's plotted range (smallest upper band limit of the two curves)", SCRIPT)
  b1 <- bn[bn$bin == 1, ]
  tot <- sum(bn$claims_received)
  ledger_add(PART, "figm_bin1_claims", b1$claims_received, "claim lines",
             "In-network claim lines received by the analytic plans in bin 1 (lowest density) of Fig 3", SCRIPT)
  ledger_add(PART, "figm_bin1_claims_share", b1$claims_received / tot, "proportion",
             "Share of the analytic in-network claim lines that fall in bin 1 of Fig 3 (bin 1 claim lines over the sum over the 8 bins)", SCRIPT)
  table_extra_keys(L0)
  ledger_write(PART)
}

# numbers used only in the table footnotes and not already ledger keys; every one is arithmetic on ledger keys or a read of a small
# validation summary file (npi_match_summary.csv, the strata table of qa/g3_npi_match.md), never a recomputation from claim data
table_extra_keys <- function(L0) {
  gv <- function(k) ledger_get(L0, k)
  nc <- gv("cohort_all_plans_excluded_received_suppressed") + gv("cohort_all_plans_excluded_denied_suppressed") + gv("cohort_all_plans_excluded_received_zero")
  if (nc != gv("cohort_all_plans_plan_level_data") - gv("cohort_all_plans_computable")) stop("table_extra_keys(): the three exclusion keys do not add up to the plans without a computable proportion")
  if (gv("cohort_all_plan_rows") - gv("cohort_all_plans_na_plan_level") - nc != gv("cohort_all_plans_computable")) stop("table_extra_keys(): the pre-cohort flow does not add up to the cohort")
  ledger_add(PART, "figm_cohort_plans_not_computable", nc, "plans",
             "Plans with plan-level counts reported but no computable in-network denial proportion (received suppressed, denied suppressed or received zero; sum of three cohort_all_ keys)", SCRIPT)
  ledger_add(PART, "figm_cohort_share_of_issuer_claims", gv("cohort_flow_cohort_claims") / gv("att_claims_received_in_network"), "proportion",
             "Share of the issuer-level in-network claim lines (att_claims_received_in_network) that the cohort plans hold at plan level (cohort_flow_cohort_claims)", SCRIPT)
  ledger_add(PART, "figm_lost_share_of_exposure_claims", gv("cohort_flow_lost_complete_case_claims") / gv("cohort_flow_exposure_ok_claims"), "proportion",
             "Share of the in-network claim lines of plans with exposure available that sit in the plans lost to the complete-case rule (the plans added back in the bounded-enrollment rows)", SCRIPT)
  ledger_add(PART, "figm_ophth_positive_plans", gv("cohort_flow_analytic_plans") - gv("design_zero_count_ophth_analytic"), "plans",
             "Analytic plans with a positive ophthalmology clinician count (analytic plans minus plans with a zero count); the basis of the ophthalmology raw Spearman correlation", SCRIPT)
  # BCBS Florida shards: total from the issuer source table, harvested count cross-checked against the harvest manifest
  isrc <- utils::read.csv(file.path(P$clean_ortho, "issuer_sources.csv"), colClasses = c(issuer_id = "character"), stringsAsFactors = FALSE, na.strings = "")
  fl_row <- isrc[grepl("^Blue Cross and Blue Shield of Florida", isrc$issuer_name), ]
  if (nrow(fl_row) != 1L) stop("table_extra_keys(): expected one Blue Cross and Blue Shield of Florida row in issuer_sources.csv")
  man <- utils::read.csv(file.path(P$clean_ortho, "harvest_manifest.csv"), stringsAsFactors = FALSE, na.strings = "")
  ok_n <- sum(man$host == "www.bcbsfl.com" & man$status == "ok")
  if (fl_row$n_provider_files - gv("ortho_files_not_run") != ok_n) stop("table_extra_keys(): shard counts disagree (", fl_row$n_provider_files, " files, ", gv("ortho_files_not_run"), " not run, ", ok_n, " harvested)")
  nr <- man[man$host == "www.bcbsfl.com" & man$status == "not_run", ]
  n_ind <- sum(nr$content_class == "individual only"); n_fg <- sum(nr$content_class == "facility or group only")
  if (n_ind + n_fg != gv("ortho_files_not_run") || nrow(nr) != gv("ortho_files_not_run")) stop("table_extra_keys(): not-run shard classes (", n_ind, " individual, ", n_fg, " facility or group) do not add up to ortho_files_not_run")
  ledger_add(PART, "figm_incomplete_issuer_not_run_individual", n_ind, "provider files",
             "Provider-file shards of the incomplete-directory insurer (BCBS Florida) not run, classed by file name as individual-clinician files (harvest_manifest content_class)", SCRIPT)
  ledger_add(PART, "figm_incomplete_issuer_not_run_facility_group", n_fg, "provider files",
             "Provider-file shards of the incomplete-directory insurer (BCBS Florida) not run, classed by file name as facility or group files (harvest_manifest content_class)", SCRIPT)
  ledger_add(PART, "figm_incomplete_issuer_files_total", fl_row$n_provider_files, "provider files",
             "Provider-file shards listed for the incomplete-directory insurer (BCBS Florida); ortho_files_not_run of them were not retrieved and the rest were harvested", SCRIPT)
  # validation strata that the ortho ledger part does not carry
  sm <- utils::read.csv(file.path(P$clean_ortho, "npi_match_summary.csv"), stringsAsFactors = FALSE)
  g <- function(it) as.numeric(sm$value[sm$item == it])
  for (z in list(c("derm_n", "pp_derm_207n_n", "Dermatology-text NPIs sampled and found active"),
                 c("derm_x", "pp_derm_207n_x", "Dermatology-text NPIs holding a 207N code"),
                 c("ophth_n", "pp_ophth_207w_n", "Ophthalmology-text NPIs sampled and found active"),
                 c("ophth_x", "pp_ophth_207w_x", "Ophthalmology-text NPIs holding a 207W code"))) {
    ledger_add(PART, paste0("figm_g3_", z[1]), g(z[2]), "NPIs", z[3], SCRIPT)
  }
  if (!same(g("pp_derm_207n_x") / g("pp_derm_207n_n"), gv("ortho_ppv_derm")) || !same(g("pp_ophth_207w_x") / g("pp_ophth_207w_n"), gv("ortho_ppv_ophth"))) stop("table_extra_keys(): control PPV differs from the ledger")
  md <- readLines(file.path(P$qa, "g3_npi_match.md"), warn = FALSE)
  grab <- function(stratum) {
    l <- grep(paste0("^- ", stratum, ": sampled "), md, value = TRUE)
    if (length(l) != 1L) stop("table_extra_keys(): stratum line not found in g3_npi_match.md: ", stratum)
    as.numeric(c(sub(".*sampled ([0-9]+),.*", "\\1", l), sub(".*found active ([0-9]+),.*", "\\1", l)))
  }
  c1 <- grab("C1"); c2 <- grab("C2")
  if (c1[1] + c2[1] != gv("ortho_g3_unflagged_sampled") || c1[2] + c2[2] != gv("ortho_g3_unflagged_found_active")) stop("table_extra_keys(): C1 and C2 do not add up to the unflagged ledger keys")
  ledger_add(PART, "figm_g3_random_unflagged_n", c1[1], "NPIs", "Randomly drawn unflagged NPIs sampled (stratum C1 of qa/g3_npi_match.md; C1 plus C2 equals ortho_g3_unflagged_sampled)", SCRIPT)
  ledger_add(PART, "figm_g3_ambiguous_unflagged_n", c2[1], "NPIs", "Unflagged NPIs with an ambiguous term sampled (stratum C2 of qa/g3_npi_match.md)", SCRIPT)
}

# =====================================================================
# FIG 3
# =====================================================================
F3_W <- 89
F3_H <- 78
F3_XLIM <- c(9.9, 20.15)
F3_YLIM <- c(0.05, 0.30)
F3_DENS <- c(1e3, 1e4, 1e5, 1e6)

f3_data <- function(L) {
  cv <- rd_res("model_fig3_curve.csv")
  bn <- rd_res("model_fig3_bins.csv")
  if (!setequal(unique(cv$curve), c("orthopaedic", "control"))) stop("f3_data(): curve names changed")
  if (!all(table(cv$curve) == ledger_get(L, "model_fig3_grid_points"))) stop("f3_data(): grid size differs from the ledger")
  if (nrow(bn) != ledger_get(L, "model_fig3_bins")) stop("f3_data(): bin count differs from the ledger")
  if (sum(bn$n_plans) != ledger_get(L, "cohort_flow_analytic_plans")) stop("f3_data(): bin plans do not add up to the analytic sample")
  if (sum(bn$claims_received) != ledger_get(L, "cohort_flow_analytic_claims")) stop("f3_data(): bin claim lines do not add up to the analytic sample")
  if (!all(bn$n_plans[1:7] == ledger_get(L, "model_fig3_bin_size")) || bn$n_plans[8] != ledger_get(L, "model_fig3_bin_last")) stop("f3_data(): bin sizes differ from the ledger")
  o <- cv[cv$curve == "orthopaedic", ]; o <- o[order(o$exposure_log2), ]
  cc <- cv[cv$curve == "control", ]; cc <- cc[order(cc$exposure_log2), ]
  for (nm in c("p05", "p95")) {
    r <- if (nm == "p05") o[1, ] else o[nrow(o), ]
    chk_ledger(L, paste0("model_pred_", nm), r$predicted_probability)
    chk_ledger(L, paste0("model_pred_", nm, "_lo"), r$ci_low)
    chk_ledger(L, paste0("model_pred_", nm, "_hi"), r$ci_high)
    chk_ledger(L, paste0("model_density_", nm), r$density_per_10000)
  }
  if (!same(2^o$exposure_log2, o$density_per_10000, 1e-9) || !same(2^cc$exposure_log2, cc$density_per_10000, 1e-9)) stop("f3_data(): density is not 2^exposure_log2")
  if (any(o$ci_low > o$predicted_probability) || any(o$ci_high < o$predicted_probability)) stop("f3_data(): band does not contain the curve")
  list(ortho = o, ctrl = cc, bins = bn)
}

f3_labels <- function(d) {
  # hand-placed direct labels in the right margin; y positions follow the data ends
  oe <- d$ortho[nrow(d$ortho), ]; ce <- d$ctrl[nrow(d$ctrl), ]; b8 <- d$bins[d$bins$bin == 8, ]
  xl <- F3_XLIM[2] + 0.45
  data.frame(
    id = c("control", "bins", "orthopaedic"),
    label = c("Dermatology\n(negative\ncontrol)", "Unadjusted\nbins\n(orthopaedic\ndensity)", "Orthopaedic\nsurgeons"),
    x = xl,
    y = c(0.232, 0.178, 0.112),
    px = c(ce$exposure_log2, b8$exposure_median_log2, oe$exposure_log2),
    py = c(ce$predicted_probability, b8$denial_proportion, oe$predicted_probability),
    stringsAsFactors = FALSE)
}

make_fig3 <- function(d) {
  lw <- pt_to_lw(0.75)
  lab <- f3_labels(d)
  dens_lab <- formatC(F3_DENS, format = "d", big.mark = ",")
  ggplot() +
    geom_ribbon(data = d$ctrl, aes(x = exposure_log2, ymin = ci_low, ymax = ci_high),
                fill = pub_colors[["control"]], alpha = 0.22) +
    geom_ribbon(data = d$ortho, aes(x = exposure_log2, ymin = ci_low, ymax = ci_high),
                fill = pub_colors[["orthopaedic"]], alpha = 0.22) +
    geom_line(data = d$ctrl, aes(x = exposure_log2, y = predicted_probability),
              colour = pub_colors[["control"]], linetype = "22", linewidth = lw) +
    geom_line(data = d$ortho, aes(x = exposure_log2, y = predicted_probability),
              colour = pub_colors[["orthopaedic"]], linetype = "solid", linewidth = lw) +
    geom_point(data = d$bins, aes(x = exposure_median_log2, y = denial_proportion, size = claims_received),
               shape = 21, fill = pub_colors[["neutral"]], colour = "white", stroke = 0.3) +
    geom_segment(data = lab, aes(x = x - 0.08, y = y, xend = px, yend = py),
                 linewidth = pt_to_lw(0.25), colour = "grey35") +
    geom_text(data = lab, aes(x = x, y = y, label = label), hjust = 0, vjust = 0.5,
              size = TXT_MM, family = FAM, lineheight = 0.9, colour = "black") +
    scale_size_area(max_size = 4.6, guide = "none") +
    scale_x_continuous(breaks = log2(F3_DENS), labels = dens_lab, minor_breaks = 10:20,
                       guide = guide_axis(minor.ticks = TRUE)) +
    scale_y_continuous(breaks = seq(0.05, 0.30, 0.05),
                       labels = function(x) formatC(100 * x, format = "f", digits = 0)) +
    coord_cartesian(xlim = F3_XLIM, ylim = F3_YLIM, expand = FALSE, clip = "off") +
    labs(x = "Listed clinicians per 10,000 reported enrollees\n(own specialty, logarithmic scale)",
         y = "Predicted in-network denial probability (%)") +
    theme_pub() +
    theme(legend.position = "none", plot.margin = margin(3, 16, 3, 3, "mm"),
          axis.minor.ticks.length.x.bottom = grid::unit(0.8, "pt"),
          axis.ticks.length = grid::unit(1.5, "pt"))
}

# =====================================================================
# CAPTIONS (placeholders only)
# =====================================================================
CAP_FIG3 <- c(
  "Predicted in-network denial probability by listed orthopaedic surgeon density, with a dermatology negative control and unadjusted binned plan data",
  paste0("Solid line: average marginal prediction from the primary model (issuer fixed effects, adjusted for plan type, metal level and log2 reported enrollment). ",
         "The exposure was set to each value on a grid for every analytic plan, with the other covariates and the issuer effect left at their observed values, ",
         "and the predicted probabilities were averaged on the response scale; the model is linear in log2 density. ",
         "The shaded band is the {{design_ci_level|pct0}} interval from the issuer-clustered covariance (delta method). ",
         "The curve covers the {{figm_curve_lower_percentile|int}}th to {{figm_curve_upper_percentile|int}}th percentile of orthopaedic density ",
         "({{model_density_p05|int}} to {{model_density_p95|int}} listed surgeons per {{design_density_per|int}} reported enrollees)."),
  paste0("Dashed line: negative control, the same model with dermatology clinician density as the exposure, over its own ",
         "{{figm_curve_lower_percentile|int}}th to {{figm_curve_upper_percentile|int}}th percentile of dermatology density ",
         "({{figm_derm_density_p05|int}} to {{figm_derm_density_p95|int}} listed clinicians per {{design_density_per|int}} reported enrollees), with its {{design_ci_level|pct0}} band. ",
         "Each curve is plotted against its own density, so the horizontal ranges differ. Dermatology density is strongly correlated with orthopaedic density (Table 3). ",
         "Over the plotted range, a horizontal line at any constant probability from {{figm_flat_line_lower|pct1}} to {{figm_flat_line_upper|pct1}} lies inside both bands."),
  paste0("Points: {{model_fig3_bins|int}} equal-count bins of the {{cohort_flow_analytic_plans|int}} analytic plans ranked by orthopaedic density ",
         "({{model_fig3_bin_size|int}} plans per bin and {{model_fig3_bin_last|int}} in the last bin), plotted at the median log2 density of each bin. ",
         "Each point is the unadjusted claim-weighted denial proportion of the bin (denied divided by received in-network claim lines, summed over its plans), ",
         "and its area is proportional to the claim lines received in the bin. ",
         "The lowest-density bin holds {{figm_bin1_claims_share|pct1}} of the {{cohort_flow_analytic_claims|mil1}} million analytic claim lines. ",
         "The points are not adjusted for plan type, metal level, enrollment or issuer, so they need not lie on the curve."),
  paste0("Minor ticks mark doublings of density. The horizontal axis mostly reflects plan size: {{design_within_issuer_share_enrollment|pct0}} of the within-issuer variation of log2 density is variation in reported enrollment. ",
         "Density is the number of orthopaedic surgeons listed in the plan's directory network per {{design_density_per|int}} reported enrollees; it is a relative measure, not a population density, ",
         "and the curve keeps each plan's own reported enrollment. The outcome is the in-network denial proportion across all specialties, not orthopaedic claims.")
)

CAP_TAB1 <- c(
  "Cohort flow and characteristics of the analytic sample, Individual market qualified health plans",
  paste0("Block A follows the Individual market file from its plan rows to the analytic sample: plans (percent of cohort plans from the cohort row down), ",
         "issuers, states and in-network claim lines received by the plans in each row (2024 dates of service, millions). ",
         "Issuer and state counts are not additive across rows because an issuer can have plans in several rows."),
  paste0("Cohort denotes the eligible plans, those with a computable in-network denial proportion; the design is cross-sectional. ",
         "All eligible plans were included; no sample size calculation was made."),
  paste0("Of the {{cohort_all_plans_plan_level_data|int}} plans with plan-level counts, {{figm_cohort_plans_not_computable|int}} have no computable denial proportion: ",
         "received count suppressed ({{cohort_all_plans_excluded_received_suppressed|int}}), denied count suppressed ({{cohort_all_plans_excluded_denied_suppressed|int}}) or no claim lines received ({{cohort_all_plans_excluded_received_zero|int}}). ",
         "The file reports plan-level counts only for plans still offered in 2026 (survivorship), so issuer-level totals exceed plan-level totals: ",
         "the {{cohort_flow_cohort_plans|int}} cohort plans hold {{cohort_flow_cohort_claims|mil1}} million of the {{att_claims_received_in_network|mil1}} million in-network claim lines reported at issuer level ({{figm_cohort_share_of_issuer_claims|pct1}})."),
  paste0("Exposure status was ok when the plan's directory network was found and listed individual clinicians; the {{cohort_flow_excluded_exposure_status_plans|int}} cohort plans that lack this are excluded for exposure status. ",
         "Together with the {{cohort_flow_lost_complete_case_plans|int}} plans lost to the complete-case rule they are the {{cohort_flow_excluded_not_analytic_plans|int}} cohort plans outside the analytic sample."),
  paste0("The {{cohort_flow_issuer_incomplete_plans|int}} plans with incomplete provider files belong to {{cohort_flow_issuer_incomplete_issuers|int}} issuers of one insurer (Blue Cross and Blue Shield (BCBS) of Florida), whose directory was only partly retrieved: ",
         "{{ortho_files_not_run|int}} of its {{figm_incomplete_issuer_files_total|int}} provider-file shards were not retrieved because of host speed and the transfer limit ",
         "({{figm_incomplete_issuer_not_run_individual|int}} individual-clinician files and {{figm_incomplete_issuer_not_run_facility_group|int}} facility or group files, classed by file name). ",
         "This is a limit of the harvest and not a property of the issuer's data. These plans hold {{cohort_flow_issuer_incomplete_claims_share|pct1}} of the cohort's in-network claim lines."),
  paste0("Complete case: the primary analysis uses only plans with a numeric reported average monthly enrollment, so the {{cohort_flow_lost_complete_case_plans|int}} plans with exposure available ",
         "whose enrollment is suppressed are lost. Reported enrollment is a plan-reported figure of uncertain scale, so density is a relative measure per {{design_density_per|int}} reported enrollees."),
  paste0("Blocks B and C describe the analytic sample (percent of analytic plans). Density is listed orthopaedic surgeons per {{design_density_per|int}} reported enrollees; ",
         "medians are over plans, with the interquartile range (first to third quartile) in parentheses; the surgeon-count column shows medians only. ",
         "The models pool Catastrophic plans with Bronze and Platinum plans with Gold; a level with fewer than {{design_metal_pool_min_plans|int}} analytic plans is pooled."),
  "EPO, exclusive provider organization; HMO, health maintenance organization; POS, point of service; PPO, preferred provider organization.",
  "Source: Centers for Medicare and Medicaid Services Marketplace Transparency in Coverage public use file for plan year 2026 (claims of 2024) and issuer provider directories (machine-readable provider files, October 2026)."
)

CAP_TAB3 <- c(
  "Specification matrix: odds ratio of in-network claim denial per doubling of orthopaedic surgeon density, primary model and sensitivity analyses",
  paste0("Odds ratios are per doubling of orthopaedic surgeon density (listed surgeons per {{design_density_per|int}} reported enrollees), except the surgeon share row, which is per doubling of the share of listed clinicians who are orthopaedic surgeons. ",
         "The primary model is a quasi-binomial logistic regression of denied on received in-network claim lines with issuer fixed effects, adjusted for log2 reported enrollment, plan type and metal level; ",
         "{{design_ci_level|pct0}} confidence intervals use issuer-clustered robust standard errors and a normal quantile. ",
         "The surgeon share, APC-inclusive and negative control rows use the same model type with their own exposure. ",
         "In the row labels, plan size means reported average monthly enrollment; the labels are fixed by the frozen analysis plan."),
  paste0("Mixed model: logistic mixed model with random intercepts for state, issuer within state and plan, fitted with nAGQ = {{model_glmm_nagq|int}}, as the analysis plan allows; ",
         "nAGQ = {{model_glmm_nagq|int}} is the fast approximation of the likelihood used by the pre-specified fit. ",
         "Its interval is a model-based Wald interval (standard errors are not clustered), the model also uses between-issuer contrasts, and the row is outside the Holm family. ",
         "A Laplace fit ended with a gradient convergence message and gave {{model_glmm_laplace_or|n3}} ({{model_glmm_laplace_lo|n3}} to {{model_glmm_laplace_hi|n3}})."),
  "Issuer with the largest share of identifying variation left out: issuer {{design_leverage_top1_issuer|str}}, which carries {{design_leverage_top1_share|pct1}} of the residual variation of the exposure.",
  paste0("Holm-adjusted P values apply to the surgeon share and APC-inclusive density rows only (Holm family of {{design_holm_family_size|int}}); no other row is adjusted. ",
         "The surgeon share row uses all {{cohort_flow_exposure_ok_plans|int}} plans with exposure available because it needs no reported enrollment. ",
         "APC, advanced practice clinician (a physician assistant, nurse practitioner or advanced practice registered nurse with an orthopaedic label)."),
  paste0("Negative control rows use dermatology or ophthalmology clinician density in the same model. The ophthalmology density adds half the smallest positive density ",
         "({{design_zero_offset_ophth|n1}} per {{design_density_per|int}} reported enrollees) before the log, because {{design_zero_count_ophth_analytic|int}} analytic plans list no ophthalmology clinician. ",
         "Their log2 densities correlate with orthopaedic density (raw Spearman correlation {{design_spearman_ortho_derm|n3}} for dermatology and {{design_spearman_ortho_ophth|n3}} for ophthalmology, ",
         "the latter on the {{figm_ophth_positive_plans|int}} plans with a positive count; within-issuer partial correlation on all {{cohort_flow_analytic_plans|int}} plans after removing log2 reported enrollment, plan type, metal level and issuer effects ",
         "{{design_partial_corr_derm|n3}} and {{design_partial_corr_ophth|n3}}), largely through the shared enrollment denominator."),
  paste0("Outcome counts were suppressed for {{sens_suppout_total_suppressed|int}} plans. The rows with bounded suppressed outcome counts add back {{sens_i_n_added|int}} plans (lower bound) and {{sens_j_n_added|int}} plans (upper bound); ",
         "a plan is added back only if its other count and its reported enrollment are numeric, and it takes the clinician counts of plans with exposure available of the same issuer and network identifier when those plans agree. ",
         "The {{sens_suppout_qualifying_plans|int}} qualifying plans come from {{sens_suppout_issuers_with_added_plans|int}} issuers ({{sens_suppout_plans_largest_issuer|int}} from issuer {{sens_suppout_largest_issuer_id|str}}), ",
         "and a qualifying plan whose received count is suppressed cannot be added at the lower bound because it leaves no denominator. ",
         "These rows are near-null by construction and are not stress tests of the outcome definition."),
  paste0("The rows with bounded suppressed enrollment add back the {{sens_k_n_added|int}} plans with exposure available whose reported enrollment is suppressed, replacing it by the bound in both the exposure and the enrollment term; ",
         "these plans hold only {{figm_lost_share_of_exposure_claims|pct1}} of the in-network claim lines of plans with exposure available, so these rows are weak tests. ",
         "Reported average monthly enrollment is a plan-reported figure of uncertain scale, so density is a relative measure; the rows without adjustment for plan size, with the surgeon share and with bounded enrollment address this limitation."),
  paste0("No crude, subgroup or interaction model was prespecified; Figure 3 shows the unadjusted binned data. ",
         "The leave-one-issuer-out and conservative t interval rows are in Supplementary Table 2.")
)

CAP_TABS1 <- c(
  "Specialty text crosswalk used to flag orthopaedic surgeons, orthopaedic advanced practice clinicians and the negative-control specialties in the provider directories",
  paste0("Provider directories carry specialty only as free text, so each individual clinician record was classified by the rules in this table, which are described in plain words. ",
         "Facility and group records were ignored. The rules were fixed before any outcome was joined; refinements made from the specialty vocabulary alone, such as the abbreviation Ortho Surg and the exclusion of dental orthodontics and dentofacial orthopedics, are logged in the project record."),
  paste0("Validation: NPIs listed in cohort plans were sampled by stratum and checked against NPI Registry taxonomy codes through the registry's public interface: ",
         "{{ortho_g3_surgeon_n|int}} surgeon-text, {{ortho_g3_apc_n|int}} advanced-practice-clinician-text, {{figm_g3_random_unflagged_n|int}} randomly drawn unflagged, ",
         "{{figm_g3_ambiguous_unflagged_n|int}} unflagged with an ambiguous term (surgery, sports, hand, spine, trauma, musculoskeletal or joint), ",
         "{{figm_g3_derm_n|int}} dermatology-text and {{figm_g3_ophth_n|int}} ophthalmology-text NPIs ({{ortho_g3_sampled|int}} in all, of which {{ortho_g3_found_active|int}} were found as active individuals)."),
  paste0("Results: {{ortho_g3_surgeon_with_207x|int}} of {{ortho_g3_surgeon_n|int}} surgeon-text NPIs ({{ortho_ppv_surgeon|pct1}}; Wilson limits {{ortho_ppv_surgeon_lo|pct1}} to {{ortho_ppv_surgeon_hi|pct1}}) held a 207X orthopaedic taxonomy code, ",
         "against {{ortho_g3_apc_with_207x|int}} of {{ortho_g3_apc_n|int}} advanced-practice-clinician-text NPIs ({{ortho_ppv_apc|pct0}}; {{ortho_ppv_apc_lo|pct1}} to {{ortho_ppv_apc_hi|pct1}}). ",
         "Dermatology text: {{figm_g3_derm_x|int}} of {{figm_g3_derm_n|int}} ({{ortho_ppv_derm|pct0}}; {{ortho_ppv_derm_lo|pct1}} to {{ortho_ppv_derm_hi|pct1}}) held a 207N code. ",
         "Ophthalmology text: {{figm_g3_ophth_x|int}} of {{figm_g3_ophth_n|int}} ({{ortho_ppv_ophth|pct0}}; {{ortho_ppv_ophth_lo|pct1}} to {{ortho_ppv_ophth_hi|pct1}}) held a 207W code. ",
         "Of the {{ortho_g3_unflagged_sampled|int}} sampled unflagged NPIs, {{ortho_g3_unflagged_found_active|int}} were found as active individuals, and {{ortho_g3_unflagged_with_207x|int}} of them held a 207X code."),
  paste0("Sensitivity of the surgeon-text flag for a 207X code: {{ortho_sensitivity_surgeon|pct0}} (interval {{ortho_sensitivity_surgeon_lo|pct0}} to {{ortho_sensitivity_surgeon_hi|pct0}}), ",
         "weighted back to the size of each sampled stratum, with a Jeffreys-prior Monte Carlo interval because no event ({{ortho_g3_unflagged_with_207x|int}}) was seen among the {{ortho_g3_unflagged_found_active|int}} unflagged NPIs found as active. ",
         "The point estimate is therefore prior-dependent and a rough magnitude only; the interval should be read, not the point value."),
  "NPI, National Provider Identifier; APC, advanced practice clinician; 207X, 207N and 207W, taxonomy code families of the National Uniform Claim Committee for orthopaedic surgery, dermatology and ophthalmology."
)

CAP_TABS2 <- c(
  "Supplementary sensitivity analyses: odds ratio of in-network claim denial per doubling of orthopaedic surgeon density",
  paste0("Odds ratios are per doubling of orthopaedic surgeon density and use the primary model of Table 3 unless the row says otherwise; ",
         "{{design_ci_level|pct0}} confidence intervals use issuer-clustered robust standard errors and a normal quantile. No row is in the Holm family."),
  paste0("Issuer-level analysis: the analytic plans are aggregated to issuers; quasi-binomial logistic regression of summed denied on summed received claim lines, ",
         "with the claim-weighted mean log2 density and mean log2 reported enrollment as the only predictors and no plan type, metal level or state term."),
  paste0("Suppressed outcome counts set to {{design_suppression_mid|n1}} adds back plans from the same {{sens_suppout_issuers_with_added_plans|int}} issuers as in Table 3 ({{sens_suppout_plans_largest_issuer|int}} from issuer {{sens_suppout_largest_issuer_id|str}}) ",
         "and is near-null by construction. Suppressed enrollment set to {{design_suppression_mid|n1}} replaces the suppressed enrollment by the bound in both the exposure and the enrollment term."),
  paste0("Leave-one-issuer-out: the primary model refitted without each of the {{sens_s_n_leaveouts|int}} informative issuers in turn. ",
         "The odds ratio and interval in the row are the full-sample primary estimate; the detail column gives the smallest and largest leave-out estimates, the issuers whose removal gives them ",
         "and the number of leave-outs whose {{design_ci_level|pct0}} interval excludes the null value of the odds ratio."),
  paste0("Conservative t interval: the primary estimate and clustered covariance with a t quantile on {{sens_t_df|int}} degrees of freedom; ",
         "the primary confidence interval remains the normal-quantile interval.")
)

# =====================================================================
# TABLE 1
# =====================================================================
T1_LADDER_CODES <- c("cohort", "excluded_exposure_status", "issuer_unreachable", "issuer_incomplete", "plan_not_in_directory",
                     "no_individuals", "exposure_ok", "lost_complete_case", "analytic")
T1_STRATA <- data.frame(
  dimension = c(rep("plan_type", 4), rep("metal_level", 5)),
  level = c("EPO", "HMO", "POS", "PPO", "Bronze", "Silver", "Gold", "Platinum", "Catastrophic"),
  stringsAsFactors = FALSE)
T1_HEAD <- c("Characteristic", "Plans (%)", "Issuers", "States", "In-network claim lines received (millions)",
             "Median (IQR) reported enrollment", "Median listed orthopaedic surgeons",
             "Median (IQR) density per 10,000 reported enrollees")
T1_WIDTHS <- c(1.38, 0.95, 0.50, 0.42, 0.62, 0.92, 0.78, 0.93)
T1_BLOCK_A <- "A. Cohort flow (% of cohort plans)"
T1_BLOCK_B <- "B. By plan type (% of analytic plans)"
T1_BLOCK_C <- "C. By metal level (% of analytic plans)"

table1_data <- function(L) {
  lad <- rd_res("model_table1_ladder.csv"); dat <- rd_res("model_table1_data.csv"); flw <- rd_res("model_cohort_flow.csv")
  lad <- lad[order(lad$row_order), ]
  if (!identical(lad$rung_code, T1_LADDER_CODES)) stop("table1_data(): ladder rungs or their order changed: ", paste(lad$rung_code, collapse = ","))
  blank <- function() setNames(as.list(rep("", length(T1_HEAD))), T1_HEAD)
  rows <- list(); bold <- integer(0)
  add <- function(r, is_bold = FALSE) { rows[[length(rows) + 1L]] <<- as.data.frame(r, check.names = FALSE, stringsAsFactors = FALSE); if (is_bold) bold <<- c(bold, length(rows)) }
  hdr <- function(txt) { r <- blank(); r[[1]] <- txt; add(r, TRUE) }
  hdr(T1_BLOCK_A)
  gl <- function(k) ledger_get(L, k)
  if (gl("cohort_all_plan_rows") - gl("cohort_all_plans_na_plan_level") - gl("figm_cohort_plans_not_computable") != lad$n_plans[lad$rung_code == "cohort"]) stop("table1_data(): the pre-cohort rows do not add up to the cohort row")
  if (gl("cohort_all_issuers_in_puf") != gl("att_issuers_in_puf") || gl("cohort_all_plans_computable") != lad$n_plans[lad$rung_code == "cohort"]) stop("table1_data(): pre-cohort keys disagree with other ledger keys")
  r <- blank(); r[[1]] <- "Individual market plan rows"; r[[2]] <- fl(L, "cohort_all_plan_rows", "int")
  r[[3]] <- fl(L, "cohort_all_issuers_in_puf", "int"); r[[4]] <- fl(L, "kff_states_individual_qhp", "int"); add(r)
  r <- blank(); r[[1]] <- paste0(INDENT, "No plan-level counts (N/A)"); r[[2]] <- fl(L, "cohort_all_plans_na_plan_level", "int"); add(r)
  r <- blank(); r[[1]] <- paste0(INDENT, "No computable proportion"); r[[2]] <- fl(L, "figm_cohort_plans_not_computable", "int"); add(r)
  ov <- dat[dat$section == "Overall", ]
  for (k in seq_len(nrow(lad))) {
    x <- lad[k, ]
    for (m in c("plans", "issuers", "states", "claims", "claims_share", "plans_share")) {
      chk_ledger(L, paste0("cohort_flow_", x$rung_code, "_", m),
                 switch(m, plans = x$n_plans, issuers = x$n_issuers, states = x$n_states, claims = x$claim_lines_received,
                        claims_share = x$share_of_cohort_claim_lines, plans_share = x$share_of_cohort_plans))
    }
    r <- blank()
    r[[1]] <- paste0(if (x$indent == 1) INDENT else "", x$rung_label)
    r[[2]] <- paste0(fv(x$n_plans, "int"), " (", fv(x$share_of_cohort_plans, "pct1"), ")")
    r[[3]] <- fv(x$n_issuers, "int"); r[[4]] <- fv(x$n_states, "int"); r[[5]] <- mil2(x$claim_lines_received)
    if (x$rung_code == "analytic") {
      for (m in c("enrollment_q25", "enrollment_median", "enrollment_q75", "surgeon_count_median", "density_q25", "density_median", "density_q75")) {
        chk_ledger(L, paste0("cohort_flow_analytic_", m), ov[[m]])
      }
      r[[6]] <- med_iqr(ov$enrollment_q25, ov$enrollment_median, ov$enrollment_q75)
      r[[7]] <- fv(ov$surgeon_count_median, "int")
      r[[8]] <- med_iqr(ov$density_q25, ov$density_median, ov$density_q75)
    }
    add(r)
  }
  strat_rows <- function(dim) {
    for (lv in T1_STRATA$level[T1_STRATA$dimension == dim]) {
      x <- dat[dat$dimension == dim & dat$level == lv, ]
      f <- flw[flw$dimension == dim & flw$level == lv & flw$rung_code == "analytic", ]
      if (nrow(x) != 1L || nrow(f) != 1L) stop("table1_data(): stratum not found: ", dim, " ", lv)
      pre <- paste0("cohort_flow_", if (dim == "plan_type") "type_" else "metal_", tolower(lv), "_")
      for (m in c("plans_cohort", "plans_exposure_ok", "plans_analytic", "issuers_cohort", "issuers_analytic", "claim_lines_cohort", "claim_lines_analytic",
                  "share_of_analytic_plans", "density_q25", "density_median", "density_q75", "surgeon_count_median",
                  "enrollment_q25", "enrollment_median", "enrollment_q75")) chk_ledger(L, paste0(pre, m), x[[m]])
      if (!same(f$n_plans, x$plans_analytic) || !same(f$n_issuers, x$issuers_analytic) || !same(f$claim_lines_received, x$claim_lines_analytic)) {
        stop("table1_data(): model_cohort_flow.csv and model_table1_data.csv disagree for ", lv)
      }
      r <- blank()
      r[[1]] <- paste0(INDENT, lv)
      r[[2]] <- paste0(fv(x$plans_analytic, "int"), " (", fv(x$share_of_analytic_plans, "pct1"), ")")
      r[[3]] <- fv(x$issuers_analytic, "int"); r[[4]] <- fv(f$n_states, "int"); r[[5]] <- mil2(x$claim_lines_analytic)
      r[[6]] <- med_iqr(x$enrollment_q25, x$enrollment_median, x$enrollment_q75)
      r[[7]] <- fv(x$surgeon_count_median, "int")
      r[[8]] <- med_iqr(x$density_q25, x$density_median, x$density_q75)
      add(r)
    }
  }
  hdr(T1_BLOCK_B); strat_rows("plan_type")
  hdr(T1_BLOCK_C); strat_rows("metal_level")
  df <- do.call(rbind, rows)
  names(df) <- T1_HEAD
  rownames(df) <- NULL
  list(df = df, bold = bold, lad = lad, dat = dat)
}

# =====================================================================
# TABLE 3 and SUPPLEMENTARY TABLE 1
# =====================================================================
# display wording: "exposure available" instead of the frozen label's "exposure-ok" (F8); nothing else in a label changes
disp_label <- function(x) sub("all exposure-ok plans", "all plans with exposure available", x, fixed = TRUE)
T3_CODES <- c("primary", paste0("sens_", letters[1:12]))
TS2_CODES <- paste0("sens_", letters[13:20])
T3_HEAD <- c("Analysis", "Odds ratio per doubling (95% CI)", "Plans", "Issuers", "Holm-adjusted P")
T3_WIDTHS <- c(2.95, 1.55, 0.6, 0.6, 0.8)
TS2_HEAD <- c("Analysis", "Odds ratio per doubling (95% CI)", "Plans", "Issuers", "Detail")
TS2_WIDTHS <- c(1.6, 1.45, 0.5, 0.55, 2.4)

sens_rows <- function(L) {
  s <- rd_res("model_sensitivity_rows.csv")
  s <- s[order(s$order), ]
  if (!identical(s$row_code, c(T3_CODES, TS2_CODES))) stop("sens_rows(): row codes or their order changed")
  # every CSV estimate must equal its ledger key (the sens_s row repeats the primary by design)
  for (k in seq_len(nrow(s))) {
    cd <- s$row_code[k]
    pre <- if (cd == "primary") "model_primary_" else paste0(cd, "_")
    chk_ledger(L, paste0(pre, "or"), s$odds_ratio_per_doubling[k])
    chk_ledger(L, paste0(pre, "lo"), s$ci_low[k])
    chk_ledger(L, paste0(pre, "hi"), s$ci_high[k])
    chk_ledger(L, paste0(pre, "n_plans"), s$n_plans[k])
    chk_ledger(L, paste0(pre, "n_issuers"), s$n_issuers[k])
  }
  s
}

table3_data <- function(L, s) {
  x <- s[s$row_code %in% T3_CODES, ]
  holm <- ifelse(is.na(x$p_holm), "", vapply(ifelse(is.na(x$p_holm), 0, x$p_holm), function(p) fv(p, "p"), character(1)))
  if (!identical(x$row_code[nzchar(holm)], c("sens_e", "sens_f"))) stop("table3_data(): Holm P present on rows other than sens_e and sens_f")
  for (cd in c("sens_e", "sens_f")) chk_ledger(L, paste0(cd, "_p_holm"), x$p_holm[x$row_code == cd])
  df <- data.frame(
    disp_label(x$label),
    mapply(or_ci, x$odds_ratio_per_doubling, x$ci_low, x$ci_high),
    vapply(x$n_plans, fv, character(1), fmt = "int"),
    vapply(x$n_issuers, fv, character(1), fmt = "int"),
    holm, stringsAsFactors = FALSE)
  names(df) <- T3_HEAD
  rownames(df) <- NULL
  df
}

tableS2_data <- function(L, s) {
  x <- s[s$row_code %in% TS2_CODES, ]
  det <- character(nrow(x)); names(det) <- x$row_code
  det["sens_m"] <- paste0("Standard errors clustered on ", fl(L, "sens_m_n_clusters", "int"), " states")
  det["sens_n"] <- paste0(fl(L, "sens_n_n_added", "int"), " plans added back")
  det["sens_o"] <- paste0(fl(L, "sens_o_n_added", "int"), " plans added back")
  det["sens_p"] <- paste0(fl(L, "sens_p_n_small_issuers", "int"), " issuers (", fl(L, "sens_p_n_small_plans", "int"), " plans) left out")
  det["sens_q"] <- paste0(fl(L, "cohort_flow_metal_catastrophic_plans_analytic", "int"), " Catastrophic plans left out")
  det["sens_r"] <- paste0("Issuer ", fl(L, "design_largest_issuer_id", "str"), " left out")
  sr <- x[x$row_code == "sens_s", ]
  for (kk in c("or_min", "or_max")) chk_ledger(L, paste0("sens_s_", kk), if (kk == "or_min") sr$leave_out_or_min else sr$leave_out_or_max)
  chk_ledger(L, "sens_s_issuer_min", sr$leave_out_issuer_min); chk_ledger(L, "sens_s_issuer_max", sr$leave_out_issuer_max)
  chk_ledger(L, "sens_s_n_leaveouts", sr$leave_outs); chk_ledger(L, "sens_s_n_excl1", sr$leave_outs_excluding_one)
  det["sens_s"] <- paste0(
    "Full-sample estimate shown. Smallest and largest of ", fl(L, "sens_s_n_leaveouts", "int"), " leave-outs: ",
    or_ci(ledger_get(L, "sens_s_or_min"), ledger_get(L, "sens_s_or_min_lo"), ledger_get(L, "sens_s_or_min_hi")),
    " without issuer ", fl(L, "sens_s_issuer_min", "str"), "; ",
    or_ci(ledger_get(L, "sens_s_or_max"), ledger_get(L, "sens_s_or_max_lo"), ledger_get(L, "sens_s_or_max_hi")),
    " without issuer ", fl(L, "sens_s_issuer_max", "str"), ". Leave-out intervals that exclude the null: ",
    fl(L, "sens_s_n_excl1", "int"), " of ", fl(L, "sens_s_n_leaveouts", "int"))
  st <- x[x$row_code == "sens_t", ]
  if (!same(st$ci_low, ledger_get(L, "sens_t_lo")) || !same(st$ci_high, ledger_get(L, "sens_t_hi"))) stop("tableS2_data(): sens_t interval differs from the ledger")
  det["sens_t"] <- paste0("t distribution with ", fl(L, "sens_t_df", "int"), " degrees of freedom; z interval ",
                          fl(L, "sens_t_z_lo", "n3"), " to ", fl(L, "sens_t_z_hi", "n3"))
  df <- data.frame(
    x$label,
    mapply(or_ci, x$odds_ratio_per_doubling, x$ci_low, x$ci_high),
    vapply(x$n_plans, fv, character(1), fmt = "int"),
    vapply(x$n_issuers, fv, character(1), fmt = "int"),
    unname(det[x$row_code]), stringsAsFactors = FALSE)
  names(df) <- TS2_HEAD
  rownames(df) <- NULL
  df
}

# =====================================================================
# SUPPLEMENTARY TABLE 1 : the specialty text crosswalk in plain words (RECORD 7.1)
# =====================================================================
TS1_HEAD <- c("Class", "Text included", "Text excluded", "Reason")
TS1_WIDTHS <- c(1.3, 2.9, 2.9, 1.9)
CW_APC <- "a physician assistant, nurse practitioner, advanced practice registered nurse, nurse or surgical-assistant label"
CW_PLAIN <- list(
  R01_ortho_surgeon = c("Orthopaedic surgeon (primary exposure)",
    "A specialty label that contains orthopaedic or orthopedic, or the abbreviation ortho surg, for example Orthopedic Surgery, Orthopaedic Surgery of the Spine, Orthopedic Sports Medicine and Pediatric Orthopaedic Surgery",
    paste0("Labels that name a non-physician, equipment or dental role (physical or occupational therapy, athletic training, chiropractic, massage, podiatry, technologist, technician, appliance, shoe, footwear, brace, prosthesis, orthotic, aide, durable medical equipment, supplier, orthodontics, dentofacial orthopedics, non-surgical); records that also carry ", CW_APC),
    "Physician-level orthopaedic text only; records with a non-physician co-label are counted in the next row"),
  R02_ortho_apc = c("Orthopaedic advanced practice clinician (sensitivity analysis)",
    paste0("Orthopaedic text as in the row above, with ", CW_APC, " in the same record"),
    "The same non-physician, equipment and dental labels as in the row above; records without such a co-label (they count as surgeons)",
    "Counted separately from surgeons and used only in the APC-inclusive sensitivity analysis"),
  R03_derm = c("Dermatology (negative control)",
    "Labels that contain dermatology, derm as a whole word or Mohs, including dermatopathology, Mohs surgery, and pediatric and procedural dermatology",
    paste0("Records that also carry ", CW_APC, "; esthetic and cosmetology labels"),
    "Negative-control specialty (taxonomy family 207N); physician level, mirroring the orthopaedic surgeon rule"),
  R04_ophth = c("Ophthalmology (negative control)",
    "Labels that contain ophthalmology (including the misspelling ophthamology and the abbreviation ophth), oculoplastic or retina specialist",
    paste0("Records that also carry ", CW_APC, "; ophthalmic technician, technologist, assistant, photographer and medical labels; optician, optometry and orthoptist labels"),
    "Negative-control specialty (taxonomy family 207W); optometry is not ophthalmology and ophthalmic technicians and assistants are not physicians"),
  R10_orthodontics = c("Not counted", "Labels that contain orthodontics", "None", "Dental specialty, not orthopaedic"),
  R11_orthotics_prosthetics = c("Not counted", "Labels that contain orthotic, prosthetic or orthoptist", "None", "Orthotists, prosthetists and orthoptists are not orthopaedic surgeons"),
  R12_chiropractic = c("Not counted", "Labels that contain chiropractic", "None", "Chiropractic is not orthopaedic surgery"),
  R13_sports_medicine_alone = c("Not counted", "A label that is exactly Sports Medicine", "None",
    "Sports Medicine alone is open to non-surgeons (family and emergency medicine, physiatry) and is not counted without orthopaedic text"),
  R14_optometry = c("Not counted", "Labels that contain optometry", "None", "Optometry is not ophthalmology"),
  R15_physician_assistant_alone = c("Not counted", "A label that is exactly Physician Assistant (or PA, PA-C)", "None", "Carries no orthopaedic text"),
  R16_podiatry = c("Not counted", "Labels that contain podiatry", "None", "Podiatry is a separate taxonomy family, not the orthopaedic family 207X"),
  R17_therapy_and_training = c("Not counted", "Labels that contain physical therapy, occupational therapy or athletic training", "None",
    "Therapists and trainers are not surgeons, including orthopaedic-certified therapists"),
  R18_hand_surgery_alone = c("Not counted", "A label that is exactly Hand Surgery (or Surgery, Hand)", "None",
    "Hand surgery alone is shared by plastic, orthopaedic and general surgeons; it is counted only when orthopaedic text is also present")
)

tableS1_data <- function() {
  cw <- utils::read.csv(file.path(P$config, "specialty_text_crosswalk.csv"), stringsAsFactors = FALSE)
  if (!identical(cw$rule_id, names(CW_PLAIN))) stop("tableS1_data(): the crosswalk rules or their order changed; rewrite CW_PLAIN: ", paste(cw$rule_id, collapse = ","))
  fam <- c(ortho_surgeon = "Orthopaedic surgeon", ortho_apc = "Orthopaedic advanced practice clinician", derm = "Dermatology", ophth = "Ophthalmology", other = "Not counted")
  if (!all(startsWith(vapply(CW_PLAIN, `[`, character(1), 1), fam[cw$family]))) stop("tableS1_data(): a plain-word class does not match the crosswalk family")
  df <- as.data.frame(do.call(rbind, CW_PLAIN), stringsAsFactors = FALSE)
  names(df) <- TS1_HEAD
  rownames(df) <- NULL
  df
}

# =====================================================================
# MAIN
# =====================================================================
run_models_main <- function() {
  lg("07b_figures_models.R start")
  ledger_reset()
  cv <- rd_res("model_fig3_curve.csv"); bn <- rd_res("model_fig3_bins.csv")
  L0 <- ledger_read_all()
  # consistency: every key of the merged ledger is unchanged in the parts
  Lm <- ledger_read_merged()
  i <- match(Lm$key, L0$key)
  if (anyNA(i) || !identical(Lm$value, L0$value[i])) stop("numbers_ledger.csv differs from the ledger parts; re-run 08_merge_ledger.R first")
  f3_extra_keys(cv, bn, L0)
  L <- ledger_read_all()
  lg("ledger (parts + design): ", nrow(L), " keys; numbers_ledger.csv has ", nrow(Lm), " (", nrow(L) - nrow(Lm), " new figm_ keys)")

  # ---- Fig 3 ----
  d <- f3_data(L)
  p3 <- make_fig3(d)
  lg("Fig 3 min font (pt): ", round(min_font_pt(p3), 2))
  paths <- save_fig(p3, "fig3", width_mm = F3_W, height_mm = F3_H)
  lg("Fig 3 saved: ", paste(basename(paths), collapse = ", "))

  # ---- captions ----
  dir.create(P$fig_captions, showWarnings = FALSE, recursive = TRUE)
  dir.create(P$tab_captions, showWarnings = FALSE, recursive = TRUE)
  caps <- list(list(f = file.path(P$fig_captions, "fig3.txt"), x = CAP_FIG3),
               list(f = file.path(P$tab_captions, "table1.txt"), x = CAP_TAB1),
               list(f = file.path(P$tab_captions, "table3.txt"), x = CAP_TAB3),
               list(f = file.path(P$tab_captions, "tableS1.txt"), x = CAP_TABS1),
               list(f = file.path(P$tab_captions, "tableS2.txt"), x = CAP_TABS2))
  for (cp in caps) {
    render_caption_check(cp$x, L)
    writeLines(cp$x, cp$f)
  }
  lg("captions written; every placeholder resolves through render_template")

  # ---- tables ----
  t1 <- table1_data(L)
  save_table(t1$df, "table1", T1_WIDTHS, footnote = NULL, landscape = FALSE, align = c("left", rep("right", 7)), bold_rows = t1$bold)
  lg("Table 1 saved: ", nrow(t1$df), " rows, ", ncol(t1$df), " columns, widths sum ", sum(T1_WIDTHS), " in")
  s <- sens_rows(L)
  t3 <- table3_data(L, s)
  save_table(t3, "table3", T3_WIDTHS, footnote = NULL, landscape = FALSE, align = c("left", rep("right", 4)), bold_rows = 1L)
  lg("Table 3 saved: ", nrow(t3), " rows, widths sum ", sum(T3_WIDTHS), " in")
  ts2 <- tableS2_data(L, s)
  save_table(ts2, "tableS2", TS2_WIDTHS, footnote = NULL, landscape = FALSE, align = c("left", "right", "right", "right", "left"))
  lg("Supplementary Table 2 saved: ", nrow(ts2), " rows, widths sum ", sum(TS2_WIDTHS), " in")
  ts1 <- tableS1_data()
  save_table(ts1, "tableS1", TS1_WIDTHS, footnote = NULL, landscape = TRUE, align = rep("left", 4))
  lg("Supplementary Table 1 (crosswalk) saved: ", nrow(ts1), " rows, widths sum ", sum(TS1_WIDTHS), " in, landscape")
  lg("07b_figures_models.R done")
  invisible(list(fig3 = p3, t1 = t1, t3 = t3, ts1 = ts1, ts2 = ts2, ledger = L))
}

if (!isTRUE(getOption("psd_figmodels_no_run"))) {
  res <- run_models_main()
}
