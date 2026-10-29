# =====================================================================
# 05b_sap_checks.R : WP5. Outcome-blind diagnostics that justify the frozen SAP
# =====================================================================
# OUTCOME BLINDNESS (binding). This script looks at exposure and plan structure only.
# It reads: v1/data_clean/ortho/plan_exposure.csv (cohort membership: its rows are the
# 2,527 cohort plans), v1/data_clean/cms/plans.csv through ONE explicit column selection
# (PLAN_COLS below, nine columns), v1/config (codebook, crosswalk, taxonomy, YAML),
# v1/results/ledger_parts/ledger_ortho.csv (measurement-error numbers) and two WP4 logs.
# Nothing else. No outcome column is selected, loaded, tabulated or joined. If an outcome
# column is ever needed, stop and report BLOCKED.
#
# README (one line per output; all in v1/results unless stated)
#   sap_check_ladder.csv            sample ladder: plans, issuers, states, issuer-states per rung
#   sap_check_ladder_strata.csv     the same ladder by plan type and metal level
#   sap_check_included_excluded.csv structural comparison, analytic sample vs excluded cohort plans
#   sap_check_exposure_dist.csv     exposure distributions (min, p5, p25, median, p75, p95, max, zeros)
#   sap_check_within_issuer.csv     informative-issuer count K and related one-line facts
#   sap_check_within_issuer_distinct.csv  distinct surgeon-count values and network IDs per issuer
#   sap_check_informative_issuers_top10.csv  the informative issuers with most plans
#   sap_check_included_excluded_enrollment.csv  enrollment of included vs excluded plans
#   sap_check_enrollment_markers.csv  enrollment marker counts by exposure status
#   sap_check_variance_decomp.csv   within-issuer variance of log2 density by source
#   sap_check_correlations.csv      exposure vs log2 enrollment (Spearman)
#   sap_check_exposure_by_group.csv exposure medians by plan type and metal level
#   sap_check_state_distinct.csv    distinct exposure values within each state
#   sap_check_enrollment_by_issuer.csv  enrollment availability per issuer
#   sap_check_enrollment_lost_top10.csv the 10 issuers losing most plans to the complete-case rule
#   sap_check_structure.csv         plans per issuer, issuers per state, largest issuer share
#   sap_check_nppes.csv             HTTP HEAD result for the NPPES monthly zip
#   sap_check_identifying_variation.csv  each issuer's share of the residual variation of the primary exposure (F5)
#   sap_check_identifying_summary.csv    effective number of issuers, top shares, leave-out checks (F5)
#   sap_check_partial_correlations.csv   partial and raw correlations of orthopaedic with control densities (F5)
#   sap_check_donor_availability.csv     exposure-side donor counts for plans outside the cohort (F5)
#   v1/config/design_constants.csv  machine-readable design constants (written here, once, before the freeze)
#   v1/logs/sap_checks.log          run log
# Run: cd v1 ; Rscript R/05b_sap_checks.R   (about 10 seconds; needs network only for the HEAD request)
#
# FREEZE GUARD. If v1/logs/sap_hash.txt exists the script does not rewrite
# design_constants.csv; it recomputes the constants and reports any mismatch instead.
#
# Definitions fixed here (and in the SAP)
#   analytic sample      exposure_status ok AND numeric reported average monthly enrollment
#   enrollment-free set  exposure_status ok (no enrollment restriction)
#   density              count / enrollment * 10000 (per 10,000 reported enrollees)
#   informative issuer   at least 2 analytic plans AND at least 2 distinct n_ortho_surgeon_text values
#   primary model rule   K >= 30 -> issuer_fe, otherwise state_fe
# Keep this file ASCII.
# =====================================================================

.args <- commandArgs(trailingOnly = FALSE)
.f <- sub("^--file=", "", grep("^--file=", .args, value = TRUE)[1])
.here <- if (is.na(.f)) getwd() else dirname(normalizePath(gsub("~+~", " ", .f, fixed = TRUE)))
.setup <- file.path(.here, "00_setup.R")
if (!file.exists(.setup)) .setup <- file.path(dirname(.here), "R", "00_setup.R")   # when sourced from v1/tests
source(.setup)
suppressPackageStartupMessages(library(data.table))

LOGF <- file.path(P$logs, "sap_checks.log")
lg <- function(...) log_msg(..., file = LOGF)

# ---- the only columns this script may read from plans.csv -------------------------------
PLAN_COLS <- c("plan_id", "issuer_id", "state", "plan_type", "metal_level",
               "avg_monthly_enrollment", "avg_monthly_enrollment_marker",
               "pa_network_id", "pa_service_area_id")

INFORMATIVE_THRESHOLD <- 30L   # K at or above this: issuer fixed effects primary
DENSITY_PER <- 10000
QPROBS <- c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)
N_SENS_TABLE3 <- 12L           # sensitivity rows in Table 3 (the SAP lists them)
N_SENS_SUPPLEMENT <- 8L        # extra pre-specified rows reported in the Supplement only
SENS_ROW_CODES <- paste0("sens_", letters[1:12])
SUPP_ROW_CODES <- paste0("sens_", letters[13:20])
METAL_POOL_MIN <- 20L          # metal levels with fewer analytic plans are pooled with the adjacent level

# ---- readers ----------------------------------------------------------------------------
read_plans_allowed <- function(path = file.path(P$clean_cms, "plans.csv")) {
  cb <- fread(file.path(P$config, "variable_codebook.csv"), select = "variable")
  miss <- setdiff(PLAN_COLS, cb$variable)
  if (length(miss)) stop("column(s) not in the codebook: ", paste(miss, collapse = ", "))
  x <- fread(path, select = PLAN_COLS, colClasses = "character", na.strings = c("", "NA"))
  stopifnot(identical(names(x), PLAN_COLS) || setequal(names(x), PLAN_COLS))
  x
}

read_exposure <- function(path = file.path(P$clean_ortho, "plan_exposure.csv")) {
  fread(path, colClasses = list(character = c("plan_id", "issuer_id", "state", "exposure_status")),
        na.strings = c("", "NA"))
}

# ---- rule and helpers (sourced by the tests) --------------------------------------------
primary_model_rule <- function(k, threshold = INFORMATIVE_THRESHOLD) {
  if (is.na(k)) stop("K is missing")
  if (k >= threshold) "issuer_fe" else "state_fe"
}

# per-issuer table: n plans, n distinct surgeon counts, informative flag (data.table input)
issuer_info <- function(dt, count_col = "n_ortho_surgeon_text") {
  dt[, .(n_plans = .N,
         n_distinct_count = uniqueN(get(count_col)),
         n_distinct_network = uniqueN(pa_network_id, na.rm = TRUE),
         n_network_missing = sum(is.na(pa_network_id))), by = issuer_id][
    , informative := n_plans >= 2L & n_distinct_count >= 2L][]
}

qrow <- function(x) {
  q <- as.numeric(quantile(x, QPROBS, type = 7, na.rm = TRUE))
  c(n = sum(!is.na(x)), n_zero = sum(x == 0, na.rm = TRUE), min = q[1], p5 = q[2], p25 = q[3],
    median = q[4], p75 = q[5], p95 = q[6], max = q[7])
}

# within-group variance decomposition of log2 density = log2 count - log2 enrollment + const
within_decomp <- function(dt, grp, label) {
  d <- copy(dt)
  d[, n_g := .N, by = grp]
  d <- d[n_g >= 2L]
  d[, `:=`(a = lc - mean(lc), e = le - mean(le)), by = grp]
  n <- nrow(d); g <- uniqueN(d[[grp]])
  ss_a <- sum(d$a^2); ss_e <- sum(d$e^2); ss_ae <- sum(d$a * d$e)
  ss_d <- ss_a + ss_e - 2 * ss_ae
  data.table(scope = label, n_plans = n, n_groups = g, df = n - g,
             var_log2_count = ss_a / (n - g), var_log2_enrollment = ss_e / (n - g),
             cov_count_enrollment = ss_ae / (n - g), var_log2_density = ss_d / (n - g),
             share_from_count = ss_a / ss_d, share_from_enrollment = ss_e / ss_d,
             share_from_covariance = -2 * ss_ae / ss_d,
             within_corr_count_enrollment = ss_ae / sqrt(ss_a * ss_e))
}

fmt15 <- function(x) {
  if (is.character(x)) return(x)
  if (is.na(x)) stop("NA design constant")
  if (x == round(x) && abs(x) < 1e15) return(format(x, scientific = FALSE, trim = TRUE))
  format(signif(x, 15), scientific = FALSE, trim = TRUE, digits = 15)
}

# ---- NPPES HEAD check -------------------------------------------------------------------
nppes_head <- function(url) {
  out <- suppressWarnings(system2("curl",
    c("-sS", "-I", "-L", "--max-time", "30", "-A", shQuote("Mozilla/5.0"), "-o", "/dev/null",
      "-w", shQuote("%{http_code}"), shQuote(url)), stdout = TRUE, stderr = TRUE))
  code <- tail(out[grepl("^[0-9]{3}$", out)], 1)
  if (!length(code)) code <- "000"
  list(code = as.integer(code), raw = paste(out, collapse = " | "))
}

main <- function() {
  lg("05b_sap_checks.R start; outcome-blind (exposure and structure only)")
  cfg <- load_config()
  res <- function(name) file.path(P$results, name)
  wr <- function(x, name) { fwrite(x, res(name), na = ""); lg("wrote ", name, " (", nrow(x), " rows)") }

  # ---- g. NPPES first, so a 200 stops the run before anything else ----------------------
  st <- fread(file.path(P$logs, "wp4_nppes_start.csv"))
  zips <- st[grepl("^HEAD", test), url]
  nrows <- list()
  for (u in zips) {
    h <- nppes_head(u)
    nrows[[length(nrows) + 1L]] <- data.table(url = u, http = h$code,
      tested_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), curl_output = h$raw)
  }
  nppes <- rbindlist(nrows)
  nppes[, reachable := http == 200L]
  wr(nppes, "sap_check_nppes.csv")
  lg("NPPES HEAD status: ", paste(nppes$http, collapse = ", "))
  if (any(nppes$reachable)) stop("BLOCKED: NPPES monthly zip responds with 200. The primary exposure source would change; the lead decides.")
  nppes_http <- max(nppes$http)

  # ---- load the allowed inputs ------------------------------------------------------------
  ex <- read_exposure()
  pl <- read_plans_allowed()
  stopifnot(nrow(ex) == uniqueN(ex$plan_id), nrow(pl) == uniqueN(pl$plan_id))
  coh <- merge(ex, pl[, .(plan_id, issuer_id_p = issuer_id, state_p = state, plan_type, metal_level,
                          enroll_raw = avg_monthly_enrollment, enroll_marker = avg_monthly_enrollment_marker,
                          pa_network_id, pa_service_area_id)], by = "plan_id", all.x = TRUE)
  stopifnot(nrow(coh) == nrow(ex), !anyNA(coh$issuer_id_p),
            all(coh$issuer_id == coh$issuer_id_p), all(coh$state == coh$state_p))
  coh[, enrollment := ifelse(enroll_marker == "numeric", suppressWarnings(as.numeric(enroll_raw)), NA_real_)]
  stopifnot(!anyNA(coh$enrollment[coh$enroll_marker == "numeric"]))
  coh[, `:=`(issuer_state = paste(state, issuer_id, sep = "_"),
             ok = exposure_status == "ok",
             has_enroll = !is.na(enrollment) & enrollment > 0)]
  coh[, analytic := ok & has_enroll]
  stopifnot(!anyNA(coh$n_ortho_surgeon_text[coh$ok]), !anyNA(coh$n_individual_npis[coh$ok]))
  lg("cohort plans ", nrow(coh), "; ok ", sum(coh$ok), "; numeric enrollment ", sum(coh$has_enroll),
     "; analytic ", sum(coh$analytic))

  # exposures
  coh[, `:=`(density = n_ortho_surgeon_text / enrollment * DENSITY_PER,
             density_apc = (n_ortho_surgeon_text + n_ortho_apc_text) / enrollment * DENSITY_PER,
             density_derm = n_derm_text / enrollment * DENSITY_PER,
             density_ophth = n_ophth_text / enrollment * DENSITY_PER,
             share = n_ortho_surgeon_text / n_individual_npis)]
  coh[, log2_density := log2(density)]
  A <- coh[analytic == TRUE]
  O <- coh[ok == TRUE]

  # ---- a. ladder ---------------------------------------------------------------------------
  cnt <- function(d) data.table(n_plans = nrow(d), n_issuers = uniqueN(d$issuer_id),
                                n_states = uniqueN(d$state), n_issuer_states = uniqueN(d$issuer_state))
  lad <- rbindlist(list(
    cbind(data.table(rung = 1L, rung_code = "cohort_computable", label = "Cohort: plans with a computable outcome"), cnt(coh)),
    cbind(data.table(rung = 2L, rung_code = "exposure_ok", label = "Exposure status ok (enrollment-free exposure set)"), cnt(O)),
    cbind(data.table(rung = 3L, rung_code = "analytic", label = "Analytic sample: exposure ok and numeric reported enrollment"), cnt(A)),
    cbind(data.table(rung = NA_integer_, rung_code = "numeric_enrollment_any_status", label = "Cohort plans with numeric enrollment, any exposure status"), cnt(coh[has_enroll == TRUE])),
    cbind(data.table(rung = NA_integer_, rung_code = "ok_without_enrollment", label = "Exposure ok but enrollment not numeric (lost to complete case)"), cnt(O[has_enroll == FALSE]))
  ))
  for (s in sort(unique(coh$exposure_status))) {
    if (s == "ok") next
    lad <- rbind(lad, cbind(data.table(rung = NA_integer_, rung_code = paste0("excluded_", s),
      label = paste0("Excluded from exposure analyses: ", s)), cnt(coh[exposure_status == s])))
  }
  lad[, share_of_cohort_plans := n_plans / nrow(coh)]
  wr(lad, "sap_check_ladder.csv")

  strata <- list()
  for (dim in c("plan_type", "metal_level")) {
    for (lv in sort(unique(coh[[dim]]))) {
      s1 <- coh[get(dim) == lv]; s2 <- s1[ok == TRUE]; s3 <- s1[analytic == TRUE]
      strata[[length(strata) + 1L]] <- data.table(dimension = dim, level = lv,
        plans_cohort = nrow(s1), plans_ok = nrow(s2), plans_analytic = nrow(s3),
        issuers_cohort = uniqueN(s1$issuer_id), issuers_ok = uniqueN(s2$issuer_id), issuers_analytic = uniqueN(s3$issuer_id),
        states_cohort = uniqueN(s1$state), states_ok = uniqueN(s2$state), states_analytic = uniqueN(s3$state),
        issuer_states_analytic = uniqueN(s3$issuer_state),
        share_ok = nrow(s2) / nrow(s1), share_analytic = nrow(s3) / nrow(s1))
    }
  }
  wr(rbindlist(strata), "sap_check_ladder_strata.csv")

  # structural comparison: analytic vs excluded cohort plans
  ie <- list()
  for (dim in c("plan_type", "metal_level", "state")) {
    for (lv in sort(unique(coh[[dim]]))) {
      ni <- sum(coh$analytic & coh[[dim]] == lv); ne <- sum(!coh$analytic & coh[[dim]] == lv)
      ie[[length(ie) + 1L]] <- data.table(dimension = dim, level = lv, n_included = ni,
        pct_included = ni / sum(coh$analytic), n_excluded = ne, pct_excluded = ne / sum(!coh$analytic))
    }
  }
  ie <- rbindlist(ie)
  wr(ie, "sap_check_included_excluded.csv")
  qe <- function(x) { x <- x[!is.na(x)]; if (!length(x)) rep(NA_real_, 3) else as.numeric(quantile(x, c(.25, .5, .75), type = 7)) }
  ge <- function(lbl, d) { q <- qe(d$enrollment)
    data.table(group = lbl, n_plans = nrow(d), n_numeric_enrollment = sum(d$has_enroll),
               share_numeric_enrollment = mean(d$has_enroll), enrollment_p25 = q[1], enrollment_median = q[2], enrollment_p75 = q[3]) }
  wr(rbindlist(list(ge("analytic sample", coh[analytic == TRUE]), ge("excluded cohort plans", coh[analytic == FALSE]),
                    ge("exposure ok without numeric enrollment", coh[ok == TRUE & has_enroll == FALSE]),
                    ge("not exposure ok", coh[ok == FALSE]))), "sap_check_included_excluded_enrollment.csv")
  mk <- coh[, .N, by = .(exposure_ok = ok, enrollment_marker = enroll_marker)][order(-exposure_ok, enrollment_marker)]
  wr(mk, "sap_check_enrollment_markers.csv")
  n_ok_star2 <- sum(coh$ok & coh$enroll_marker == "star2")

  # ---- b. exposure distributions ---------------------------------------------------------
  ed <- list()
  add_ed <- function(metric, sample, x) ed[[length(ed) + 1L]] <<- as.data.table(as.list(c(metric = metric, sample = sample, qrow(x))))
  add_ed("surgeon_text_count", "analytic", A$n_ortho_surgeon_text)
  add_ed("surgeon_text_count", "exposure_ok", O$n_ortho_surgeon_text)
  add_ed("apc_text_count", "analytic", A$n_ortho_apc_text)
  add_ed("derm_text_count", "analytic", A$n_derm_text)
  add_ed("ophth_text_count", "analytic", A$n_ophth_text)
  add_ed("listed_individual_npis", "analytic", A$n_individual_npis)
  add_ed("reported_enrollment", "analytic", A$enrollment)
  add_ed("density_per_10000", "analytic", A$density)
  add_ed("log2_density", "analytic", A$log2_density)
  add_ed("apc_inclusive_density_per_10000", "analytic", A$density_apc)
  add_ed("log2_apc_inclusive_density", "analytic", log2(A$density_apc))
  add_ed("derm_density_per_10000", "analytic", A$density_derm)
  add_ed("ophth_density_per_10000", "analytic", A$density_ophth)
  add_ed("surgeon_share_of_listed", "exposure_ok", O$share)
  add_ed("surgeon_share_of_listed", "analytic", A$share)
  add_ed("log2_surgeon_share", "exposure_ok", log2(O$share))
  ed <- rbindlist(ed)
  for (cc in setdiff(names(ed), c("metric", "sample"))) ed[[cc]] <- as.numeric(ed[[cc]])
  wr(ed, "sap_check_exposure_dist.csv")
  zero_any <- c(ortho = sum(A$n_ortho_surgeon_text == 0), apc_inclusive = sum(A$density_apc == 0),
                derm = sum(A$n_derm_text == 0), ophth = sum(A$n_ophth_text == 0),
                share = sum(O$n_ortho_surgeon_text == 0))
  lg("exact zeros in the analytic sample: ", paste(names(zero_any), zero_any, sep = "=", collapse = ", "))
  # zero rule: offset c = half the smallest positive density, per exposure, only if an exact zero exists
  zero_offset <- function(x) if (any(x == 0)) 0.5 * min(x[x > 0]) else 0
  off_ortho <- zero_offset(A$density); off_derm <- zero_offset(A$density_derm); off_ophth <- zero_offset(A$density_ophth)
  lg("zero offsets: ortho ", off_ortho, ", derm ", off_derm, ", ophth ", off_ophth)
  metal_n <- A[, .N, by = metal_level]
  metal_pooled <- metal_n[N < METAL_POOL_MIN, metal_level]
  lg("metal levels pooled (fewer than ", METAL_POOL_MIN, " analytic plans): ", paste(sort(metal_pooled), collapse = ", "))

  # ---- c. within-issuer information -------------------------------------------------------
  ii <- issuer_info(A)
  K <- sum(ii$informative)
  inf_ids <- ii[informative == TRUE, issuer_id]
  io <- issuer_info(O)
  K_free <- sum(io$informative)
  rule_model <- primary_model_rule(K)
  wi <- data.table(metric = c(
    "analytic_issuers", "issuers_with_ge2_analytic_plans", "informative_issuers_K",
    "plans_in_informative_issuers", "issuers_ge2_plans_identical_counts", "issuers_ge2_plans_single_network_id",
    "issuers_ge2_plans_with_missing_network_id", "primary_model_by_rule",
    "enrollment_free_issuers", "enrollment_free_issuers_ge2_plans", "enrollment_free_informative_issuers",
    "informative_issuers_states", "analytic_states", "states_with_ge2_issuers_analytic"),
    value = c(nrow(ii), sum(ii$n_plans >= 2L), K,
              sum(ii[informative == TRUE, n_plans]), sum(ii$n_plans >= 2L & ii$n_distinct_count == 1L),
              sum(ii$n_plans >= 2L & ii$n_distinct_network == 1L), sum(ii$n_plans >= 2L & ii$n_network_missing > 0L),
              rule_model, nrow(io), sum(io$n_plans >= 2L), K_free,
              uniqueN(A[issuer_id %in% inf_ids, state]), uniqueN(A$state),
              sum(A[, .(n = uniqueN(issuer_id)), by = state]$n >= 2L)))
  wr(wi, "sap_check_within_issuer.csv")
  lg("K informative issuers = ", K, " -> primary model by rule: ", rule_model)

  # which informative issuers carry the within-issuer information (largest first)
  iis <- merge(ii[informative == TRUE], unique(A[, .(issuer_id, state)]), by = "issuer_id")
  iis[, share_of_informative_plans := n_plans / sum(n_plans)]
  setorder(iis, -n_plans)
  iis[, cumulative_share := cumsum(share_of_informative_plans)]
  wr(iis[1:min(10L, .N), .(issuer_id, state, n_plans, n_distinct_count, n_distinct_network, share_of_informative_plans, cumulative_share)],
     "sap_check_informative_issuers_top10.csv")
  top_inf_share <- iis$share_of_informative_plans[1]
  top_inf_id <- iis$issuer_id[1]
  lg("largest informative issuer ", top_inf_id, " holds ", round(top_inf_share, 3), " of informative-issuer plans")
  m <- ii[n_plans >= 2L]
  dist_count <- m[, .N, by = .(n_distinct = pmin(n_distinct_count, 5L))][order(n_distinct)][, variable := "distinct_surgeon_counts_per_issuer"]
  dist_net <- m[, .N, by = .(n_distinct = pmin(n_distinct_network, 5L))][order(n_distinct)][, variable := "distinct_network_ids_per_issuer"]
  cross <- m[, .N, by = .(n_distinct = paste0("counts ", pmin(n_distinct_count, 3L), " / networks ", pmin(n_distinct_network, 3L)))][
    , variable := "cross: distinct counts / distinct network IDs (capped at 3)"]
  dd <- rbindlist(list(dist_count, dist_net, cross), use.names = TRUE)
  setnames(dd, "N", "n_issuers")
  dd[, note := "n_distinct value 5 means 5 or more; cross rows cap at 3; issuers with at least 2 analytic plans only"]
  setcolorder(dd, c("variable", "n_distinct", "n_issuers", "note"))
  wr(dd, "sap_check_within_issuer_distinct.csv")

  A[, `:=`(lc = log2(n_ortho_surgeon_text), le = log2(enrollment))]
  vd <- rbindlist(list(
    within_decomp(A, "issuer_id", "within issuer, issuers with at least 2 analytic plans"),
    within_decomp(A[issuer_id %in% inf_ids], "issuer_id", "within issuer, informative issuers only"),
    within_decomp(A, "state", "within state, states with at least 2 analytic plans")))
  # total and between shares of log2 density
  tot_ss <- sum((A$log2_density - mean(A$log2_density))^2)
  w_iss <- vd[1, var_log2_density * df]
  w_st <- vd[3, var_log2_density * df]
  vd[, share_of_total_variance_within := c(w_iss / tot_ss, vd[2, var_log2_density * df] / tot_ss, w_st / tot_ss)]
  vd[, note := c("share_of_total_variance_within uses all analytic plans in the denominator (singleton issuers contribute zero within)", "", "")]
  wr(vd, "sap_check_variance_decomp.csv")
  lg("within-issuer variance of log2 density: from count ", round(vd$share_from_count[1], 3),
     ", from enrollment ", round(vd$share_from_enrollment[1], 3), ", covariance ", round(vd$share_from_covariance[1], 3))

  # ---- c2. identifying variation of the primary exposure (exposure only; F5) ---------------
  # regress the primary exposure on log2 enrollment, plan type, metal group and issuer effects (plain lm,
  # analytic sample); an issuer's share of the residual sum of squares is its share of the identifying variation
  big <- A[, .N, by = issuer_id][order(-N, issuer_id)][1]
  to_metal3 <- function(x) fifelse(x %in% c("Bronze", "Catastrophic"), "Bronze", fifelse(x %in% c("Gold", "Platinum"), "Gold", x))
  A[, `:=`(lenroll = log2(enrollment), metal3 = to_metal3(metal_level), expo_ortho = log2(density),
           expo_derm = log2(density_derm + off_derm), expo_ophth = log2(density_ophth + off_ophth))]
  stopifnot(all(A$metal3 %in% c("Bronze", "Silver", "Gold")), all(is.finite(A$expo_ortho)),
            all(is.finite(A$expo_derm)), all(is.finite(A$expo_ophth)))
  resid_lm <- function(d, yname) {
    f <- as.formula(paste(yname, "~ lenroll + plan_type + metal3 + factor(issuer_id)"))
    as.numeric(resid(lm(f, data = d)))
  }
  shares_of <- function(d) {
    d <- copy(d); set(d, j = "res", value = resid_lm(d, "expo_ortho"))
    z <- d[, .(n_plans = .N, rss = sum(res^2)), by = issuer_id]
    z[, share := rss / sum(rss)]
    setorder(z, -share, issuer_id)
    z
  }
  iv <- shares_of(A)
  n_eff <- 1 / sum(iv$share^2)
  iv <- merge(iv, unique(A[, .(issuer_id, state)]), by = "issuer_id")
  iv <- merge(iv, ii[, .(issuer_id, n_distinct_count, informative)], by = "issuer_id")
  setorder(iv, -share, issuer_id)
  iv[, `:=`(rank = seq_len(.N), cumulative_share = cumsum(share))]
  wr(iv[, .(rank, issuer_id, state, n_plans, n_distinct_count, informative, share_of_residual_ss = share, cumulative_share)],
     "sap_check_identifying_variation.csv")
  top3 <- iv[1:3]
  lo1 <- shares_of(A[issuer_id != iv$issuer_id[1]])
  lo2 <- shares_of(A[!(issuer_id %in% iv$issuer_id[1:2])])
  lo3 <- shares_of(A[!(issuer_id %in% iv$issuer_id[1:3])])
  lop <- shares_of(A[issuer_id != big$issuer_id])
  k_after <- function(drop) sum(ii[!(issuer_id %in% drop), informative])
  isum <- data.table(metric = c("analytic_issuers", "issuers_with_positive_residual_ss", "effective_number_of_issuers_inverse_herfindahl",
      "top1_issuer", "top1_share", "top2_issuer", "top2_share", "top3_issuer", "top3_share", "top3_cumulative_share",
      "k_informative_issuers", "k_after_dropping_top1", "k_after_dropping_top2", "k_after_dropping_top3",
      "effective_issuers_after_dropping_top1", "top_share_after_dropping_top1",
      "effective_issuers_after_dropping_top2", "effective_issuers_after_dropping_top3",
      "largest_issuer_by_plan_count", "largest_issuer_by_plan_count_share_of_residual_ss", "effective_issuers_after_dropping_largest_by_plans",
      "top_share_after_dropping_largest_by_plans", "sd_of_residual_exposure_doublings", "sd_of_log2_density_doublings"),
    value = c(nrow(iv), sum(iv$rss > 1e-12), n_eff, top3$issuer_id[1], top3$share[1], top3$issuer_id[2], top3$share[2],
      top3$issuer_id[3], top3$share[3], sum(top3$share), K, k_after(iv$issuer_id[1]), k_after(iv$issuer_id[1:2]), k_after(iv$issuer_id[1:3]),
      1 / sum(lo1$share^2), lo1$share[1], 1 / sum(lo2$share^2), 1 / sum(lo3$share^2), big$issuer_id,
      iv[issuer_id == big$issuer_id, share], 1 / sum(lop$share^2), lop$share[1],
      sd(resid_lm(A, "expo_ortho")), sd(A$expo_ortho)))
  isum[, note := ""]
  isum[metric == "effective_number_of_issuers_inverse_herfindahl", note := "unweighted by claims; a claims-weighted figure is likely lower and is not computed in this outcome-blind step"]
  wr(isum, "sap_check_identifying_summary.csv")
  lg("identifying variation: top3 ", paste(top3$issuer_id, round(top3$share, 3), collapse = "; "), "; effective issuers ", round(n_eff, 2))
  id_top <- top3$issuer_id

  # ---- c3. partial correlations of the orthopaedic with the control exposures (exposure only; F5) ----
  set(A, j = "e_ortho", value = resid_lm(A, "expo_ortho"))
  pc_derm <- cor(A$e_ortho, resid_lm(A, "expo_derm"))
  pc_ophth <- cor(A$e_ortho, resid_lm(A, "expo_ophth"))
  Ap <- A[n_ophth_text > 0]
  pc_ophth_pos <- cor(resid_lm(Ap, "expo_ortho"), resid_lm(Ap, "expo_ophth"))
  sp_derm <- cor(A$expo_ortho, A$expo_derm, method = "spearman")
  Ap2 <- A[is.finite(log2(density_ophth))]
  sp_ophth <- cor(Ap2$expo_ortho, log2(Ap2$density_ophth), method = "spearman")
  pcor <- data.table(pair = c("orthopaedic vs dermatology density", "orthopaedic vs ophthalmology density (frozen control definition, offset)",
                              "orthopaedic vs ophthalmology density (plans with a positive ophthalmology count)"),
                     n_plans = c(nrow(A), nrow(A), nrow(Ap)),
                     partial_correlation = c(pc_derm, pc_ophth, pc_ophth_pos),
                     raw_spearman_log2_density = c(sp_derm, cor(A$expo_ortho, A$expo_ophth, method = "spearman"), sp_ophth),
                     note = c("residuals after log2 enrollment, plan type, metal group, issuer effects; equals the partial correlation of log2 counts",
                              "raw Spearman uses the offset definition", "raw Spearman uses plans with finite log2 density"))
  wr(pcor, "sap_check_partial_correlations.csv")
  lg("partial correlations: derm ", round(pc_derm, 3), ", ophth ", round(pc_ophth, 3), " (positive-count plans ", round(pc_ophth_pos, 3), ")")

  # ---- c4. donor availability for plans outside the cohort (exposure side only; F5) -------
  nc <- pl[!(plan_id %in% coh$plan_id)]
  nc[, num_enroll := avg_monthly_enrollment_marker == "numeric"]
  donors <- coh[ok == TRUE & !is.na(pa_network_id), .(n_donor_plans = .N,
                agree = uniqueN(paste(n_ortho_surgeon_text, n_ortho_apc_text, n_derm_text, n_ophth_text, n_individual_npis)) == 1L),
                by = .(issuer_id, pa_network_id)]
  nc <- merge(nc, donors, by = c("issuer_id", "pa_network_id"), all.x = TRUE)
  nc[, has_donor := !is.na(agree) & agree]
  don <- data.table(metric = c("plans_outside_cohort", "outside_cohort_missing_network_id", "outside_cohort_with_agreeing_donor",
                               "outside_cohort_with_donor_and_numeric_enrollment"),
                    value = c(nrow(nc), sum(is.na(nc$pa_network_id)), sum(nc$has_donor), sum(nc$has_donor & nc$num_enroll)),
                    note = c("rows of the plan table that are not cohort plans", "", "upper bound for the plans addable in 9.4; the 21 suppressed-outcome plans are a subset",
                             "upper bound; the outcome-side count is determined by WP6b"))
  wr(don, "sap_check_donor_availability.csv")

  # ---- d. correlations and group medians ------------------------------------------------
  sp <- function(x, y, label, sample) { ok <- is.finite(x) & is.finite(y)
    data.table(pair = label, sample = sample, method = "spearman", n = sum(ok), rho = cor(x[ok], y[ok], method = "spearman")) }
  A[, le_raw := log2(enrollment)]
  cors <- rbindlist(list(
    sp(A$log2_density, A$le_raw, "log2 density vs log2 reported enrollment", "analytic"),
    sp(log2(A$n_ortho_surgeon_text), A$le_raw, "log2 surgeon count vs log2 reported enrollment", "analytic"),
    sp(log2(A$n_derm_text), A$le_raw, "log2 derm count vs log2 reported enrollment", "analytic"),
    sp(log2(A$density_derm), A$le_raw, "log2 derm density vs log2 reported enrollment", "analytic"),
    sp(log2(A$density_ophth), A$le_raw, "log2 ophth density vs log2 reported enrollment", "analytic"),
    sp(log2(A$density), log2(A$density_derm), "log2 ortho density vs log2 derm density", "analytic"),
    sp(log2(A$density), log2(A$density_ophth), "log2 ortho density vs log2 ophth density", "analytic"),
    sp(log2(A$density), log2(A$density_apc), "log2 ortho density vs log2 APC-inclusive density", "analytic"),
    sp(log2(A$n_ortho_surgeon_text), log2(A$n_individual_npis), "log2 surgeon count vs log2 listed individual NPIs", "analytic"),
    sp(A$log2_density, log2(A$share), "log2 ortho density vs log2 surgeon share", "analytic")))
  # same, within the plan type and metal level groups as ordinal ranks (Spearman vs rank code)
  A[, `:=`(metal_rank = match(metal_level, c("Catastrophic", "Bronze", "Silver", "Gold", "Platinum")))]
  cors <- rbind(cors, sp(A$log2_density, A$metal_rank, "log2 density vs metal level rank (Catastrophic<Bronze<Silver<Gold<Platinum)", "analytic"), fill = TRUE)
  wr(cors, "sap_check_correlations.csv")

  gm <- list()
  for (dim in c("plan_type", "metal_level")) {
    for (lv in sort(unique(A[[dim]]))) {
      s <- A[get(dim) == lv]
      gm[[length(gm) + 1L]] <- data.table(dimension = dim, level = lv, n_plans = nrow(s),
        median_surgeon_count = median(s$n_ortho_surgeon_text), median_enrollment = median(s$enrollment),
        median_density = median(s$density), median_log2_density = median(s$log2_density),
        p25_log2_density = quantile(s$log2_density, .25, names = FALSE), p75_log2_density = quantile(s$log2_density, .75, names = FALSE),
        median_surgeon_share = median(s$share))
    }
  }
  wr(rbindlist(gm), "sap_check_exposure_by_group.csv")

  sd_ <- A[, .(n_plans = .N, n_issuers = uniqueN(issuer_id), n_distinct_density = uniqueN(round(density, 6)),
               n_distinct_surgeon_count = uniqueN(n_ortho_surgeon_text),
               median_density = median(density)), by = state][order(state)]
  wr(sd_, "sap_check_state_distinct.csv")

  # ---- e. enrollment availability by issuer ---------------------------------------------
  eb <- coh[, .(state = state[1], n_cohort_plans = .N, n_numeric_enrollment = sum(has_enroll),
                n_ok_plans = sum(ok), n_ok_lost_to_complete_case = sum(ok & !has_enroll),
                n_analytic = sum(analytic)), by = issuer_id]
  eb[, share_numeric_enrollment := n_numeric_enrollment / n_cohort_plans]
  setorder(eb, -n_ok_lost_to_complete_case, -n_cohort_plans, issuer_id)
  wr(eb, "sap_check_enrollment_by_issuer.csv")
  top <- eb[1:10]
  top[, share_of_all_ok_plans_lost := n_ok_lost_to_complete_case / sum(eb$n_ok_lost_to_complete_case)]
  wr(top, "sap_check_enrollment_lost_top10.csv")

  # ---- f. structure ------------------------------------------------------------------------
  ppi <- A[, .N, by = issuer_id]
  ips <- A[, .(n = uniqueN(issuer_id)), by = state]
  big <- ppi[order(-N)][1]
  ppi_ok <- O[, .N, by = issuer_id]
  stc <- rbindlist(list(
    data.table(metric = paste0("analytic_plans_per_issuer_", c("min", "p25", "median", "p75", "max")), value = as.numeric(quantile(ppi$N, c(0, .25, .5, .75, 1), type = 7))),
    data.table(metric = paste0("analytic_issuers_per_state_", c("min", "p25", "median", "p75", "max")), value = as.numeric(quantile(ips$n, c(0, .25, .5, .75, 1), type = 7))),
    data.table(metric = paste0("ok_plans_per_issuer_", c("min", "median", "max")), value = as.numeric(quantile(ppi_ok$N, c(0, .5, 1), type = 7))),
    data.table(metric = c("analytic_issuers_with_1_plan", "analytic_issuers_with_ge2_plans", "largest_issuer_analytic_plans",
                          "largest_issuer_share_of_analytic_plans", "largest_state_share_of_analytic_plans",
                          "analytic_states_with_1_issuer", "analytic_distinct_service_area_ids", "analytic_distinct_network_ids_nonmissing",
                          "analytic_plans_missing_network_id"),
               value = c(sum(ppi$N == 1L), sum(ppi$N >= 2L), big$N, big$N / nrow(A),
                         max(A[, .N, by = state]$N) / nrow(A), sum(ips$n == 1L),
                         uniqueN(A$pa_service_area_id, na.rm = TRUE), uniqueN(A$pa_network_id, na.rm = TRUE),
                         sum(is.na(A$pa_network_id))))))
  stc[, note := ""]
  stc[metric == "largest_issuer_analytic_plans", note := paste0("issuer id ", big$issuer_id, " (public HIOS identifier)")]
  wr(stc, "sap_check_structure.csv")

  # ---- hashes of frozen inputs, measurement-error numbers, design constants ---------------
  cw <- sha256_file(file.path(P$config, "specialty_text_crosswalk.csv"))
  tx <- sha256_file(file.path(P$config, "ortho_taxonomy.csv"))
  hl <- readLines(file.path(P$logs, "wp4_crosswalk_hash.txt"), warn = FALSE)
  stopifnot(any(grepl(cw, hl, fixed = TRUE)), any(grepl(tx, hl, fixed = TRUE)))
  led <- fread(file.path(P$ledger_parts, "ledger_ortho.csv"), select = c("key", "value"), colClasses = "character")
  lv <- function(k) { v <- led[key == k, value]; if (length(v) != 1L) stop("ledger_ortho key missing: ", k); as.numeric(v) }

  cs <- load_config()
  consts <- list(
    list("design_alpha", cs$inference$alpha, "probability", "Two-sided significance level for the single primary hypothesis"),
    list("design_ci_level", cs$inference$ci_level, "proportion", "Confidence level for every interval"),
    list("design_seed", cs$study$seed, "integer", "Random seed (study_config.yaml)"),
    list("design_holm_family_size", 2L, "count", "Holm family: surgeon share of listed clinicians and APC-inclusive density; primary not adjusted"),
    list("design_informative_issuer_k", K, "count", "Informative issuers in the analytic sample (at least 2 plans and at least 2 distinct surgeon counts)"),
    list("design_informative_issuer_threshold", INFORMATIVE_THRESHOLD, "count", "K at or above this selects issuer fixed effects as the primary model"),
    list("design_primary_model", rule_model, "text", "Primary model selected by the decision rule: issuer_fe or state_fe"),
    list("design_density_per", DENSITY_PER, "enrollees", "Density denominator: surgeons per 10,000 reported average monthly enrollees"),
    list("design_zero_offset_needed", as.integer(zero_any[["ortho"]] > 0L), "indicator", "1 if the primary exposure has an exact zero in the analytic sample"),
    list("design_zero_offset", off_ortho, "density units", "Offset added before log2 for the primary exposure (0 when no exact zero exists)"),
    list("design_zero_offset_needed_controls", as.integer(zero_any[["derm"]] > 0L | zero_any[["ophth"]] > 0L), "indicator", "1 if a negative-control exposure has an exact zero in the analytic sample"),
    list("design_zero_offset_derm", off_derm, "density units", "Offset for the dermatology control density (0 when no exact zero exists)"),
    list("design_zero_offset_ophth", off_ophth, "density units", "Offset for the ophthalmology control density: half the smallest positive density in the analytic sample (5 exact zeros)"),
    list("design_zero_count_ophth_analytic", zero_any[["ophth"]], "plans", "Analytic plans with an exact zero ophthalmology count"),
    list("design_primary_adjusts_enrollment", 1L, "indicator", "1 if the primary model adjusts for log2 reported enrollment (decided from the exposure diagnostics)"),
    list("design_metal_pool_min_plans", METAL_POOL_MIN, "plans", "Metal levels with fewer analytic plans are pooled with the adjacent level"),
    list("design_metal_pooled_levels", paste(sort(metal_pooled), collapse = ";"), "text", "Metal levels pooled (Catastrophic into Bronze, Platinum into Gold)"),
    list("design_suppression_lower", 1, "claim lines or enrollees", "Lower bound for a ** cell"),
    list("design_suppression_mid", 5.5, "claim lines or enrollees", "Midpoint bound for a ** cell (Supplement)"),
    list("design_suppression_upper", 10, "claim lines or enrollees", "Upper bound for a ** cell"),
    list("design_control_reading_ratio", 0.5, "ratio", "Negative-control reading rule: same sign and at least this fraction of the orthopaedic estimate"),
    list("design_fig3_bins", 8L, "count", "Equal-count bins of plans for the raw data in Fig 3"),
    list("design_fig3_lower_quantile", 0.05, "probability", "Lower quantile of log2 density for the Fig 3 curve"),
    list("design_fig3_upper_quantile", 0.95, "probability", "Upper quantile of log2 density for the Fig 3 curve"),
    list("design_min_claims_exclusion", 10000, "claim lines", "Sensitivity: exclude issuers with fewer in-network claim lines than this"),
    list("design_n_sensitivity_rows", N_SENS_TABLE3, "count", "Sensitivity rows in Table 3 (plus the primary row)"),
    list("design_n_supplement_rows", N_SENS_SUPPLEMENT, "count", "Pre-specified rows reported in the Supplement only"),
    list("design_sens_row_codes", paste(SENS_ROW_CODES, collapse = ";"), "text", "Table 3 row codes in display order"),
    list("design_supplement_row_codes", paste(SUPP_ROW_CODES, collapse = ";"), "text", "Supplement-only row codes in display order"),
    list("design_ok_plans_enrollment_star2", n_ok_star2, "plans", "Exposure-ok plans whose enrollment is the ** marker (added back in the enrollment-bound rows)"),
    list("design_density_median_analytic", median(A$density), "surgeons per 10,000 reported enrollees", "Median primary density, analytic sample"),
    list("design_reported_enrollment_median", median(A$enrollment), "enrollees", "Median reported average monthly enrollment, analytic sample"),
    list("design_largest_issuer_share", big$N / nrow(A), "proportion", "Largest issuer's share of analytic plans"),
    list("design_largest_issuer_id", big$issuer_id, "issuer id", "Issuer with the most analytic plans (sens_r leaves it out)"),
    list("design_leverage_top1_issuer", top3$issuer_id[1], "issuer id", "Issuer with the largest share of the identifying variation (sens_d leaves it out)"),
    list("design_leverage_top1_share", top3$share[1], "proportion", "Top-1 issuer's share of the residual sum of squares of the primary exposure"),
    list("design_leverage_top2_issuer", top3$issuer_id[2], "issuer id", "Issuer with the second largest share"),
    list("design_leverage_top2_share", top3$share[2], "proportion", "Top-2 share"),
    list("design_leverage_top3_issuer", top3$issuer_id[3], "issuer id", "Issuer with the third largest share"),
    list("design_leverage_top3_share", top3$share[3], "proportion", "Top-3 share"),
    list("design_effective_issuers", n_eff, "issuers", "Effective number of issuers, inverse Herfindahl of the shares (unweighted by claims)"),
    list("design_t_interval_df", K - 1L, "degrees of freedom", "Degrees of freedom of the conservative t interval (K - 1)"),
    list("design_partial_corr_derm", pc_derm, "correlation", "Partial correlation of orthopaedic and dermatology log2 density after log2 enrollment, plan type, metal group and issuer effects"),
    list("design_partial_corr_ophth", pc_ophth, "correlation", "Partial correlation with ophthalmology log2 density (frozen offset definition, all analytic plans)"),
    list("design_partial_corr_ophth_positive", pc_ophth_pos, "correlation", "Same, restricted to plans with a positive ophthalmology count"),
    list("design_spearman_ortho_derm", sp_derm, "correlation", "Raw Spearman correlation of orthopaedic and dermatology log2 density"),
    list("design_spearman_ortho_ophth", sp_ophth, "correlation", "Raw Spearman correlation with ophthalmology log2 density (plans with a positive count)"),
    list("design_noncohort_plans", nrow(nc), "plans", "Plans in the plan table outside the cohort (exposure-side count only)"),
    list("design_noncohort_with_donor", sum(nc$has_donor), "plans", "Outside-cohort plans with an agreeing donor network (upper bound for 9.4)"),
    list("design_noncohort_with_donor_numeric_enrollment", sum(nc$has_donor & nc$num_enroll), "plans", "Same, with numeric enrollment (upper bound for 9.4)"),
    list("design_largest_informative_issuer_share", top_inf_share, "proportion", "Largest informative issuer's share of plans in informative issuers"),
    list("design_spearman_density_enrollment", cors$rho[1], "correlation", "Spearman correlation of log2 density with log2 reported enrollment, analytic sample"),
    list("design_cohort_plans", nrow(coh), "plans", "Cohort plans (computable outcome), rows of plan_exposure.csv"),
    list("design_exposure_ok_plans", nrow(O), "plans", "Plans with exposure_status ok"),
    list("design_exposure_ok_issuers", uniqueN(O$issuer_id), "issuers", "Issuers with at least one exposure-ok plan"),
    list("design_exposure_ok_states", uniqueN(O$state), "states", "States with at least one exposure-ok plan"),
    list("design_enrollment_free_plans", nrow(O), "plans", "Plans in the enrollment-free exposure set (exposure ok)"),
    list("design_analytic_plans", nrow(A), "plans", "Analytic sample: exposure ok and numeric reported enrollment"),
    list("design_analytic_issuers", uniqueN(A$issuer_id), "issuers", "Issuers in the analytic sample"),
    list("design_analytic_states", uniqueN(A$state), "states", "States in the analytic sample"),
    list("design_analytic_issuer_states", uniqueN(A$issuer_state), "issuer-states", "Issuer-states in the analytic sample"),
    list("design_issuers_ge2_plans", sum(ii$n_plans >= 2L), "issuers", "Analytic issuers with at least 2 plans"),
    list("design_informative_issuer_plans", sum(ii[informative == TRUE, n_plans]), "plans", "Analytic plans in informative issuers"),
    list("design_k_enrollment_free", K_free, "count", "Informative issuers in the enrollment-free set (diagnostic only)"),
    list("design_within_issuer_share_count", vd$share_from_count[1], "proportion", "Share of within-issuer variance of log2 density carried by log2 surgeon count"),
    list("design_within_issuer_share_enrollment", vd$share_from_enrollment[1], "proportion", "Share carried by log2 reported enrollment"),
    list("design_within_issuer_share_covariance", vd$share_from_covariance[1], "proportion", "Share carried by the covariance term"),
    list("design_nppes_http_status", nppes_http, "HTTP code", "Highest status of the NPPES monthly zip HEAD requests at the freeze"),
    list("design_crosswalk_sha256", cw[[1]], "text", "SHA-256 of config/specialty_text_crosswalk.csv"),
    list("design_taxonomy_sha256", tx[[1]], "text", "SHA-256 of config/ortho_taxonomy.csv"),
    list("design_ppv_surgeon_text", lv("ortho_ppv_surgeon"), "proportion", "PPV of the surgeon-text flag for any 207X code (ledger_ortho)"),
    list("design_ppv_surgeon_text_lo", lv("ortho_ppv_surgeon_lo"), "proportion", "Wilson lower limit"),
    list("design_ppv_surgeon_text_hi", lv("ortho_ppv_surgeon_hi"), "proportion", "Wilson upper limit"),
    list("design_ppv_derm", lv("ortho_ppv_derm"), "proportion", "PPV of the dermatology-text flag for any 207N code"),
    list("design_ppv_derm_lo", lv("ortho_ppv_derm_lo"), "proportion", "Wilson lower limit"),
    list("design_ppv_derm_hi", lv("ortho_ppv_derm_hi"), "proportion", "Wilson upper limit"),
    list("design_ppv_ophth", lv("ortho_ppv_ophth"), "proportion", "PPV of the ophthalmology-text flag for any 207W code"),
    list("design_ppv_ophth_lo", lv("ortho_ppv_ophth_lo"), "proportion", "Wilson lower limit"),
    list("design_ppv_ophth_hi", lv("ortho_ppv_ophth_hi"), "proportion", "Wilson upper limit"))
  dc <- rbindlist(lapply(consts, function(z) as.data.table(list(key = z[[1]], value = fmt15(z[[2]]), unit = z[[3]],
                                                        description = z[[4]], source_script = "05b_sap_checks.R"))))
  stopifnot(!anyDuplicated(dc$key), all(grepl("^design_[a-z0-9_]+$", dc$key)), !any(grepl("NA|NaN|Inf", dc$value)))
  out <- file.path(P$config, "design_constants.csv")
  hashf <- file.path(P$logs, "sap_hash.txt")
  if (file.exists(hashf)) {
    if (!file.exists(out)) stop("freeze guard: sap_hash.txt exists but design_constants.csv is missing")
    old <- fread(out, colClasses = "character", na.strings = NULL)
    mm <- merge(dc[, .(key, new = value)], old[, .(key, old = value)], by = "key", all = TRUE)
    bad <- mm[is.na(new) | is.na(old) | new != old]
    if (nrow(bad)) { print(bad); stop("freeze guard: recomputed design constants differ from the frozen file") }
    lg("freeze guard: sap_hash.txt exists; recomputed constants identical to the frozen file; design_constants.csv not rewritten")
  } else {
    fwrite(dc, out, na = "")
    lg("wrote design_constants.csv (", nrow(dc), " keys)")
  }
  lg("05b_sap_checks.R done")
  invisible(list(K = K, rule = rule_model))
}

if (!isTRUE(getOption("psd_sap_no_run"))) main()
