# =====================================================================
# test_desc.R : WP6a tests (plain R). Run: Rscript "<root>/v1/tests/test_desc.R"
# Exit status 1 if any check fails. Nothing here is tuned to pass; a failing check is reported as it is.
# Run 05_descriptives.R first (it writes the files checked here).
# NOT tested, on purpose: any relation between the ten denial-reason tallies and denied claims (reasons are
# tallies, not unique claims). Reason shares are only checked to sum to 1 within a stratum.
# Simulation tolerances were fixed from sampling theory before running (stated at each check).
# =====================================================================
.args <- commandArgs(trailingOnly = FALSE)
.f <- sub("^--file=", "", grep("^--file=", .args, value = TRUE)[1])
.here <- dirname(normalizePath(gsub("~+~", " ", .f, fixed = TRUE)))
source(file.path(dirname(.here), "R", "00_setup.R"))
source(file.path(dirname(.here), "R", "ledger.R"))
suppressPackageStartupMessages(library(lme4))
options(psd_desc_no_run = TRUE)
SCRIPT_PATH <- file.path(P$R, "05_descriptives.R")
source(SCRIPT_PATH)
options(psd_desc_no_run = NULL)

N_PASS <- 0L; N_FAIL <- 0L; FAILS <- character(0)
check <- function(name, expr) {
  res <- tryCatch({ r <- force(expr); if (isTRUE(r)) "ok" else paste0("FALSE", if (is.character(r)) paste0(": ", paste(utils::head(r, 3), collapse = " | ")) else "") },
                  error = function(e) paste0("ERROR: ", conditionMessage(e)))
  if (identical(res, "ok")) { N_PASS <<- N_PASS + 1L; cat("PASS  ", name, "\n", sep = "") }
  else { N_FAIL <<- N_FAIL + 1L; FAILS <<- c(FAILS, name); cat("FAIL  ", name, "  ->  ", res, "\n", sep = "") }
  invisible(NULL)
}
near <- function(a, b, tol = 1e-9) all(abs(a - b) <= tol * pmax(1, abs(b)))

CC <- c(issuer_id = "character", plan_id = "character", state = "character", level = "character", stratum = "character", ids = "character")
rd_path <- function(path) {
  hdr <- names(utils::read.csv(path, nrows = 1, check.names = FALSE))
  utils::read.csv(path, colClasses = CC[names(CC) %in% hdr], na.strings = c("", "NA"), stringsAsFactors = FALSE, check.names = FALSE)
}
rd <- function(f) rd_path(file.path(P$results, f))
rc <- function(f) rd_path(file.path(P$clean_cms, f))
iss <- rc("issuer_state.csv"); pl <- rc("plans.csv"); vol <- rc("issuer_volume.csv")
kff <- utils::read.csv(file.path(P$ledger_parts, "ledger_kff.csv"), colClasses = "character", stringsAsFactors = FALSE)
kffv <- function(k) as.numeric(kff$value[match(k, kff$key)])
numcol <- function(df, col) { v <- suppressWarnings(as.numeric(df[[col]])); v[df[[paste0(col, "_marker")]] != "numeric"] <- NA_real_; v }

OUTF <- c("desc_attrition.csv", "desc_attrition_ratios.csv", "desc_oon_exclusions.csv", "desc_issuer_funnel.csv",
          "desc_funnel_summary.csv", "desc_funnel_limits_grid.csv", "desc_funnel_method.txt", "desc_state_rates.csv",
          "desc_reason_mix.csv", "desc_reason_mix_wide.csv", "desc_cohort_flow.csv", "desc_plan_characteristics.csv",
          "desc_variance_partition.csv")

# ---- 0. files and hygiene -------------------------------------------------------
check("all WP6a output files and the ledger part exist", all(file.exists(file.path(P$results, OUTF))) &&
        file.exists(file.path(P$ledger_parts, "ledger_desc.csv")) && file.exists(file.path(P$logs, "wp6a_ledger_sources.csv")))
.nonascii <- function(f) { x <- readLines(f, warn = FALSE, encoding = "bytes"); any(grepl("[^\\x01-\\x7F]", x, perl = TRUE, useBytes = TRUE)) }
check("05_descriptives.R and test_desc.R are ASCII only", !.nonascii(SCRIPT_PATH) && !.nonascii(file.path(P$tests, "test_desc.R")))
EMD <- intToUtf8(0x2014)
chk_files <- c(file.path(P$results, OUTF), file.path(P$ledger_parts, "ledger_desc.csv"), file.path(P$logs, "wp6a_ledger_sources.csv"),
               SCRIPT_PATH, file.path(P$tests, "test_desc.R"), list.files(P$logs, pattern = "^wp6a_", full.names = TRUE))
check("no em-dash (U+2014) in any WP6a file", !any(vapply(chk_files, function(f) any(grepl(EMD, readLines(f, warn = FALSE, encoding = "UTF-8"), fixed = TRUE)), logical(1))))
check("script code never touches the orthopaedic layer (no ortho reference outside comment lines)", {
  code <- readLines(SCRIPT_PATH, warn = FALSE); code <- code[!grepl("^\\s*#", code)]
  !any(grepl("ortho", code, ignore.case = TRUE)) })
check("run log never mentions the orthopaedic layer", !any(grepl("ortho", readLines(file.path(P$logs, "wp6a_run.log"), warn = FALSE), ignore.case = TRUE)))
check("no iCloud conflict copies of WP6a files", {
  all_f <- list.files(file.path(P$v1), recursive = TRUE, full.names = FALSE, all.files = TRUE)
  bad <- all_f[grepl(" [0-9]+(\\.[A-Za-z0-9]+)?$", all_f) & grepl("desc_|ledger_desc|wp6a_|05_descriptives|test_desc", all_f)]
  if (length(bad)) bad else TRUE })
check("issuer_id and plan_id are written as character (no decimals, exact match to the source ids)", {
  fu <- readLines(file.path(P$results, "desc_issuer_funnel.csv"), n = 6)
  fn <- rd("desc_issuer_funnel.csv")
  is.character(fn$issuer_id) && !any(grepl("\\.", fn$issuer_id)) && all(fn$issuer_id %in% iss$issuer_id) && all(grepl("^[0-9]{5}$", fn$issuer_id)) })

# ---- 1. used rows are valid ------------------------------------------------------
fun <- rd("desc_issuer_funnel.csv")
check("funnel: in-network denied <= received, rate in [0,1], rate = denied / received, every row", {
  all(fun$denied <= fun$received) && all(fun$rate >= 0 & fun$rate <= 1) && near(fun$rate, fun$denied / fun$received, 1e-12) })
rn <- numcol(pl, "plan_claims_received_in_network"); dn <- numcol(pl, "plan_claims_denied_in_network")
is_comp <- !is.na(rn) & !is.na(dn) & rn > 0 & dn <= rn
check("computable plans (independent rule): denied <= received and rate in [0,1] for every used plan", {
  all(dn[is_comp] <= rn[is_comp]) && all(dn[is_comp] / rn[is_comp] >= 0 & dn[is_comp] / rn[is_comp] <= 1) })
check("independent computable-plan count equals the parsed plan_in_network_denial_rate count (2,527)", sum(is_comp) == sum(!is.na(pl$plan_in_network_denial_rate)) && sum(is_comp) == 2527L)
check("no in-network plan or issuer row has denied above received in the source", !any(dn > rn, na.rm = TRUE) &&
        !any(numcol(iss, "issuer_claims_denied_in_network") > numcol(iss, "issuer_claims_received_in_network"), na.rm = TRUE))

# ---- 2. attrition: ties and independent recomputation ----------------------------------
att <- rd("desc_attrition.csv")
AK <- c(claims_received_in_network = "att_claims_received_in_network", claims_received_out_of_network = "att_claims_received_out_of_network",
        claims_denied_in_network = "att_claims_denied_in_network", claims_denied_out_of_network = "att_claims_denied_out_of_network",
        claims_resubmitted_in_network = "att_claims_resubmitted_in_network", claims_resubmitted_out_of_network = "att_claims_resubmitted_out_of_network",
        internal_appeals_filed_not_split = "att_internal_appeals_filed", internal_appeals_overturned_not_split = "att_internal_appeals_overturned",
        external_appeals_filed_not_split = "att_external_appeals_filed", external_appeals_overturned_not_split = "att_external_appeals_overturned")
att$id <- paste(att$measure, att$network, sep = "_")
check("G5: attrition totals equal the att_ keys in ledger_kff.csv (10 of 10)", {
  ok <- vapply(names(AK), function(i) { r <- att[att$id == i, ]; nrow(r) == 1L && r$total == kffv(AK[[i]]) }, logical(1)); if (all(ok)) TRUE else names(ok)[!ok] })
check("attrition issuers_reported and issuers_suppressed equal the att_ keys (10 of 10)", {
  ok <- vapply(names(AK), function(i) { r <- att[att$id == i, ]; r$issuers_reported == kffv(paste0(AK[[i]], "_issuers_reported")) && r$issuers_suppressed == kffv(paste0(AK[[i]], "_issuers_suppressed")) }, logical(1)); all(ok) })
check("attrition issuers_na equals att_issuers_na_issuer_level for every non-combined row", all(att$issuers_na[att$network != "combined"] == kffv("att_issuers_na_issuer_level")))
check("attrition totals recomputed independently from issuer_state.csv (different code path)", {
  cols <- c("issuer_claims_received_in_network", "issuer_claims_received_out_of_network", "issuer_claims_denied_in_network", "issuer_claims_denied_out_of_network",
            "issuer_claims_resubmitted_in_network", "issuer_claims_resubmitted_out_of_network", "issuer_internal_appeals_filed", "issuer_internal_appeals_overturned",
            "issuer_external_appeals_filed", "issuer_external_appeals_overturned")
  tot <- vapply(cols, function(cn) sum(numcol(iss, cn), na.rm = TRUE), numeric(1))
  isTRUE(all.equal(unname(tot), att$total[match(names(AK), att$id)], tolerance = 0)) })
check("combined rows equal in-network plus out-of-network totals", {
  all(vapply(c("claims_received", "claims_denied", "claims_resubmitted"), function(m)
    att$total[att$measure == m & att$network == "combined"] == att$total[att$measure == m & att$network == "in_network"] + att$total[att$measure == m & att$network == "out_of_network"], logical(1))) })
check("in-network received and denied totals agree with issuer_volume.csv (a separately parsed table)", {
  vr <- vol$in_network_claims_received; vd <- vol$in_network_claims_denied
  sum(vr[vol$in_network_claims_received_marker == "numeric"]) == att$total[att$id == "claims_received_in_network"] &&
    sum(vd[vol$in_network_claims_denied_marker == "numeric"]) == att$total[att$id == "claims_denied_in_network"] })

oon_bad <- iss$issuer_id[!is.na(numcol(iss, "issuer_claims_denied_out_of_network")) & !is.na(numcol(iss, "issuer_claims_received_out_of_network")) &
                           numcol(iss, "issuer_claims_denied_out_of_network") > numcol(iss, "issuer_claims_received_out_of_network")]
check("pinned anomaly found independently: out-of-network denied > received for exactly issuers 73836 and 37160", setequal(oon_bad, c("73836", "37160")))
rat <- rd("desc_attrition_ratios.csv")
SP <- data.frame(
  id = c("resubmitted_per1000_denied_in_network", "internal_appeals_filed_per1000_denied_in_network", "internal_appeals_overturned_per1000_denied_in_network",
         "external_appeals_filed_per1000_denied_in_network", "external_appeals_overturned_per1000_denied_in_network",
         "resubmitted_per1000_denied_combined", "internal_appeals_filed_per1000_denied_combined", "internal_appeals_overturned_per1000_denied_combined",
         "external_appeals_filed_per1000_denied_combined", "external_appeals_overturned_per1000_denied_combined",
         "resubmitted_per1000_denied_out_of_network", "denial_rate_in_network", "denial_rate_out_of_network", "denial_rate_combined",
         "internal_overturned_per1000_internal_filed", "external_overturned_per1000_external_filed"),
  num = c("rsi", "iaf", "iao", "eaf", "eao", "rsi+rso", "iaf", "iao", "eaf", "eao", "rso", "dni", "dno", "dni+dno", "iao", "eao"),
  den = c(rep("dni", 5), rep("dni+dno", 5), "dno", "rci", "rco", "rci+rco", "iaf", "eaf"),
  scale = c(rep(1000, 11), 1, 1, 1, 1000, 1000),
  excl = c(rep(FALSE, 5), rep(TRUE, 6), FALSE, TRUE, TRUE, FALSE, FALSE), stringsAsFactors = FALSE)
CM <- c(rsi = "issuer_claims_resubmitted_in_network", rso = "issuer_claims_resubmitted_out_of_network", iaf = "issuer_internal_appeals_filed",
        iao = "issuer_internal_appeals_overturned", eaf = "issuer_external_appeals_filed", eao = "issuer_external_appeals_overturned",
        dni = "issuer_claims_denied_in_network", dno = "issuer_claims_denied_out_of_network", rci = "issuer_claims_received_in_network",
        rco = "issuer_claims_received_out_of_network")
expr_sum <- function(df, s) Reduce(`+`, lapply(strsplit(s, "+", fixed = TRUE)[[1]], function(t) numcol(df, CM[[t]])))
check("ratio table lists exactly the 16 ratios in the independent specification", setequal(rat$ratio_id, SP$id) && nrow(rat) == 16L)
check("matched-set ratios, issuer counts and sums recomputed independently (16 of 16)", {
  ok <- vapply(seq_len(nrow(SP)), function(i) {
    d <- if (SP$excl[i]) iss[!(iss$issuer_id %in% oon_bad), ] else iss
    nn <- expr_sum(d, SP$num[i]); dd <- expr_sum(d, SP$den[i]); m <- !is.na(nn) & !is.na(dd)
    r <- rat[rat$ratio_id == SP$id[i], ]
    r$n_issuers_matched == sum(m) && near(r$numerator_sum, sum(nn[m]), 0) && near(r$denominator_sum, sum(dd[m]), 0) &&
      near(r$ratio, SP$scale[i] * sum(nn[m]) / sum(dd[m]), 1e-12) && r$scale == SP$scale[i] }, logical(1))
  if (all(ok)) TRUE else SP$id[!ok] })
check("matched issuer set never exceeds the fewest issuers reporting any cell it uses", {
  ok <- vapply(seq_len(nrow(SP)), function(i) {
    cells_used <- unique(unlist(lapply(c(SP$num[i], SP$den[i]), function(s) CM[strsplit(s, "+", fixed = TRUE)[[1]]])))
    rat$n_issuers_matched[rat$ratio_id == SP$id[i]] <= min(vapply(cells_used, function(cn) sum(iss[[paste0(cn, "_marker")]] == "numeric"), numeric(1))) }, logical(1)); all(ok) })
check("resubmitted is never presented as a share of denied (ratios are per 1,000 and not capped)", {
  r <- rat[rat$ratio_id == "resubmitted_per1000_denied_in_network", ]; r$scale == 1000 && grepl("per 1,000", r$scale_label) })
check("the 2 anomalous out-of-network issuers are left out of every out-of-network or combined rate and counted", {
  ex <- rat[SP$excl[match(rat$ratio_id, SP$id)], ]; all(ex$n_issuers_excluded_anomalous == 2L) && all(rat$n_issuers_excluded_anomalous[!SP$excl[match(rat$ratio_id, SP$id)]] == 0L) })
oo <- rd("desc_oon_exclusions.csv")
check("out-of-network exclusions: 2 issuers (73836, 37160), 30 plans, 6 issuers holding them", {
  setequal(strsplit(oo$ids[oo$scope == "issuers"], ";")[[1]], c("73836", "37160")) && oo$n_excluded[oo$scope == "plans"] == 30L &&
    oo$n_excluded[oo$scope == "issuers_holding_the_plans"] == 6L && sum(pl$out_of_network_denied_exceeds_received %in% TRUE) == 30L })

# ---- 3. funnel on the real data ----------------------------------------------------
fs <- rd("desc_funnel_summary.csv")
mr <- iss$issuer_claims_received_in_network_marker; md <- iss$issuer_claims_denied_in_network_marker
check("funnel population: 158 issuers; 26 N/A and 1 suppressed denied excluded and counted; 157 over 1,000 claims", {
  nrow(fun) == 158L && fs$n_issuers_na == 26L && fs$n_issuers_denied_suppressed == 1L && fs$n_issuers_funnel == 158L && fs$n_issuers_over_1000 == 157L &&
    sum(mr == "na") == 26L && sum(mr == "numeric" & md == "star2") == 1L && sum(mr == "numeric" & md == "numeric") == 158L && !anyDuplicated(fun$issuer_id) })
check("funnel rows are exactly the issuers with both cells numeric", setequal(fun$issuer_id, iss$issuer_id[mr == "numeric" & md == "numeric"]))
check("pooled rate = sum denied / sum received; se_binomial and z recomputed", {
  p0 <- sum(fun$denied) / sum(fun$received); se <- sqrt(p0 * (1 - p0) / fun$received)
  near(fs$pooled_rate, p0, 1e-12) && near(fun$se_binomial, se, 1e-12) && near(fun$z, (fun$denied / fun$received - p0) / se, 1e-9) })
check("Winsorising: z_winsorised equals z clipped at its 10th and 90th percentiles; about 10 percent clipped in each tail", {
  q <- quantile(fun$z, c(0.1, 0.9), type = 7, names = FALSE)
  near(fun$z_winsorised, pmin(pmax(fun$z, q[1]), q[2]), 1e-12) && sum(fun$z < q[1]) %in% 14:18 && sum(fun$z > q[2]) %in% 14:18 })
check("tau2, phi and Q recomputed by separately written code", {
  n <- fun$received; y <- fun$denied; I <- length(n); p0 <- sum(y) / sum(n); s2 <- p0 * (1 - p0) / n
  zz <- (y / n - p0) / sqrt(s2); lo <- quantile(zz, 0.1, type = 7, names = FALSE); hi <- quantile(zz, 0.9, type = 7, names = FALSE)
  zw <- ifelse(zz < lo, lo, ifelse(zz > hi, hi, zz)); Q <- sum(zw^2); S1 <- sum(1 / s2); S2 <- sum(1 / s2^2)
  t2 <- max(0, (Q - (I - 1)) / (S1 - S2 / S1))
  near(fs$tau2_winsorised, t2, 1e-9) && near(fs$q_statistic, Q, 1e-9) && near(fs$phi_winsorised, Q / I, 1e-9) && fs$tau2_winsorised >= 0 })
check("limits recomputed: unadjusted p0 +/- zc se, adjusted p0 +/- zc sqrt(se^2 + tau2); adjusted at least as wide", {
  p0 <- fs$pooled_rate; t2 <- fs$tau2_winsorised; z95 <- qnorm(0.975); z998 <- qnorm(0.999)
  near(fun$limit_95_upper_unadjusted, p0 + z95 * fun$se_binomial, 1e-12) && near(fun$limit_998_lower_unadjusted, p0 - z998 * fun$se_binomial, 1e-12) &&
    near(fun$limit_95_upper_adjusted, p0 + z95 * sqrt(fun$se_binomial^2 + t2), 1e-12) && near(fun$limit_998_lower_adjusted, p0 - z998 * sqrt(fun$se_binomial^2 + t2), 1e-12) &&
    all(fun$limit_95_upper_adjusted >= fun$limit_95_upper_unadjusted) && all(fun$limit_998_lower_adjusted <= fun$limit_998_lower_unadjusted) &&
    all(fun$limit_998_upper_unadjusted >= fun$limit_95_upper_unadjusted) && all(fun$limit_998_upper_adjusted >= fun$limit_95_upper_adjusted) })
check("flags agree with the limits and the summary counts agree with the flags", {
  f1 <- fun$rate < fun$limit_95_lower_unadjusted | fun$rate > fun$limit_95_upper_unadjusted
  f2 <- fun$rate < fun$limit_998_lower_adjusted | fun$rate > fun$limit_998_upper_adjusted
  f3 <- fun$rate < fun$limit_95_lower_adjusted | fun$rate > fun$limit_95_upper_adjusted
  f4 <- fun$rate < fun$limit_998_lower_unadjusted | fun$rate > fun$limit_998_upper_unadjusted
  identical(f1, fun$outside_95_unadjusted) && identical(f2, fun$outside_998_adjusted) && identical(f3, fun$outside_95_adjusted) && identical(f4, fun$outside_998_unadjusted) &&
    fs$outside_95_unadjusted == sum(f1) && fs$outside_998_adjusted == sum(f2) && fs$outside_95_adjusted == sum(f3) && fs$outside_998_unadjusted == sum(f4) &&
    fs$above_998_adjusted + fs$below_998_adjusted == fs$outside_998_adjusted && fs$outside_998_adjusted <= fs$outside_95_adjusted })
check("limits grid reproduces funnel_limits() for the pooled rate and tau2", {
  g <- rd("desc_funnel_limits_grid.csv"); L <- funnel_limits(fs$pooled_rate, g$received, 0.998, fs$tau2_winsorised)
  near(g$limit_998_lower_adjusted, L$lower, 1e-12) && near(g$limit_998_upper_adjusted, L$upper, 1e-12) && nrow(g) == 200L && min(g$received) < min(fun$received) && max(g$received) > max(fun$received) })
check("funnel method file states the formulas and does not attribute the analyst choices to the source papers", {
  m <- readLines(file.path(P$results, "desc_funnel_method.txt"), warn = FALSE)
  any(grepl("tau2 = max", m, fixed = TRUE)) && any(grepl("analyst choices", m, fixed = TRUE)) && any(grepl("1.9600", m, fixed = TRUE)) && any(grepl("3.0902", m, fixed = TRUE)) })

# ---- 4. funnel code on simulated truth ---------------------------------------------
sim_issuers <- function(I, p0, tau, meanlog, sdlog, contam = 0, contam_dev = 0.3, constant_n = NULL) {
  n <- if (is.null(constant_n)) pmax(round(exp(rnorm(I, meanlog, sdlog))), 50) else rep(constant_n, I)
  th <- p0 + rnorm(I, 0, tau)
  if (contam > 0) { k <- sample.int(I, contam); th[k] <- th[k] + sample(c(-1, 1), contam, replace = TRUE) * contam_dev }
  th <- pmin(pmax(th, 0.001), 0.999)
  list(n = n, y = rbinom(I, n, th))
}
set.seed(20261028)
qW <- qnorm(0.9); cW <- (2 * pnorm(qW) - 1 - 2 * qW * dnorm(qW)) + qW^2 * 2 * (1 - pnorm(qW))
check("simulation, no overdispersion: share outside the unadjusted 95 percent limits is about 5 percent (band 4.6 to 5.4, +/- 3 SE of 30,000 points)", {
  s <- replicate(200, { d <- sim_issuers(150, 0.19, 0, 10, 1.5); ff <- funnel_fit(d$n, d$y); L <- funnel_limits(ff$p0, d$n, 0.95, 0); mean(ff$p < L$lower | ff$p > L$upper) })
  m <- mean(s); cat("      (observed ", sprintf("%.4f", m), ")\n", sep = ""); m > 0.046 && m < 0.054 })
check("simulation, no overdispersion: share outside the unadjusted 99.8 percent limits is about 0.2 percent (band 0.12 to 0.30)", {
  s <- replicate(200, { d <- sim_issuers(150, 0.19, 0, 10, 1.5); ff <- funnel_fit(d$n, d$y); L <- funnel_limits(ff$p0, d$n, 0.998, 0); mean(ff$p < L$lower | ff$p > L$upper) })
  m <- mean(s); cat("      (observed ", sprintf("%.5f", m), ")\n", sep = ""); m > 0.0012 && m < 0.0030 })
check("simulation, no overdispersion: tau2 is 0 (Winsorised, in at least 95 percent of runs) and the adjusted limits then equal the unadjusted ones", {
  t2 <- replicate(200, { d <- sim_issuers(150, 0.19, 0, 10, 1.5); funnel_fit(d$n, d$y)$tau2 })
  d <- sim_issuers(150, 0.19, 0, 10, 1.5); ff <- funnel_fit(d$n, d$y)
  mean(t2 == 0) >= 0.95 && ff$tau2 == 0 && identical(funnel_limits(ff$p0, d$n, 0.95, ff$tau2), funnel_limits(ff$p0, d$n, 0.95, 0)) })
TAU <- 0.03
check("simulation, known tau2 = 0.0009: the unwinsorised estimate is unbiased (mean ratio 0.92 to 1.08 over 200 runs)", {
  r <- replicate(200, { d <- sim_issuers(150, 0.19, TAU, 11, 1); funnel_fit(d$n, d$y, winsor_p = 0)$tau2 / TAU^2 })
  m <- mean(r); cat("      (observed ", sprintf("%.3f", m), ")\n", sep = ""); m > 0.92 && m < 1.08 })
check("simulation, known tau2, equal issuer sizes: the 10 percent Winsorised estimate is below truth by the analytic factor for clipping a normal at its 10th/90th percentiles (0.679 +/- 0.06)", {
  r <- replicate(200, { d <- sim_issuers(150, 0.19, TAU, 11, 1, constant_n = 60000); funnel_fit(d$n, d$y, winsor_p = 0.1)$tau2 / TAU^2 })
  m <- mean(r); cat("      (analytic factor ", sprintf("%.4f", cW), ", observed ", sprintf("%.3f", m), ")\n", sep = ""); abs(m - cW) < 0.06 })
check("simulation, known tau2: additive limits with the unwinsorised tau2 contain about the nominal share (95 percent: 0.935 to 0.965; 99.8 percent: 0.9955 to 0.9995)", {
  s <- replicate(200, { d <- sim_issuers(150, 0.19, TAU, 11, 1); ff <- funnel_fit(d$n, d$y, winsor_p = 0)
    L1 <- funnel_limits(ff$p0, d$n, 0.95, ff$tau2); L2 <- funnel_limits(ff$p0, d$n, 0.998, ff$tau2)
    c(mean(ff$p >= L1$lower & ff$p <= L1$upper), mean(ff$p >= L2$lower & ff$p <= L2$upper)) })
  m <- rowMeans(s); cat("      (observed ", paste(sprintf("%.4f", m), collapse = " and "), ")\n", sep = ""); m[1] > 0.935 && m[1] < 0.965 && m[2] > 0.9955 && m[2] < 0.9995 })
check("simulation, known tau2, equal issuer sizes: Winsorised additive limits are narrower, and the share outside 95 percent matches the analytic prediction 2(1 - pnorm(1.96 sqrt(factor))) +/- 0.02", {
  s <- replicate(200, { d <- sim_issuers(150, 0.19, TAU, 11, 1, constant_n = 60000); ff <- funnel_fit(d$n, d$y, winsor_p = 0.1); L <- funnel_limits(ff$p0, d$n, 0.95, ff$tau2); mean(ff$p < L$lower | ff$p > L$upper) })
  pred <- 2 * (1 - pnorm(1.96 * sqrt(cW))); m <- mean(s); cat("      (predicted ", sprintf("%.4f", pred), ", observed ", sprintf("%.4f", m), ")\n", sep = ""); abs(m - pred) < 0.02 && m > 0.05 })
check("simulation, outliers: 6 contaminated issuers (+/- 0.3) raise the unwinsorised tau2 more than 3-fold but the Winsorised tau2 less than 1.5-fold (median over 100 runs)", {
  rr <- replicate(100, {
    a <- sim_issuers(150, 0.19, TAU, 11, 1); b <- a; k <- sample.int(150, 6)
    b$y[k] <- rbinom(6, b$n[k], pmin(pmax(a$y[k] / a$n[k] + sample(c(-1, 1), 6, TRUE) * 0.3, 0.001), 0.999))
    c(funnel_fit(b$n, b$y, 0)$tau2 / funnel_fit(a$n, a$y, 0)$tau2, funnel_fit(b$n, b$y, 0.1)$tau2 / funnel_fit(a$n, a$y, 0.1)$tau2) })
  m <- apply(rr, 1, median); cat("      (median ratios ", paste(sprintf("%.2f", m), collapse = " and "), ")\n", sep = ""); m[1] > 3 && m[2] < 1.5 })
check("funnel helpers: critical values, symmetry, bad input refused", {
  ff <- funnel_fit(c(100, 1000, 1e4, 1e5), c(20, 190, 1900, 19000)); L <- funnel_limits(ff$p0, c(100, 1000), 0.95, 0.001)
  near(funnel_zcrit(0.95), 1.959964, 1e-6) && near(funnel_zcrit(0.998), 3.090232, 1e-6) && near(ff$p0 - L$lower, L$upper - ff$p0, 1e-12) &&
    inherits(try(funnel_fit(c(10, 20, 30), c(11, 2, 3)), silent = TRUE), "try-error") && inherits(try(funnel_fit(c(10, 20), c(1, 2)), silent = TRUE), "try-error") })

# ---- 5. state rates ---------------------------------------------------------------
sr <- rd("desc_state_rates.csv")
check("state rates: pooled rate, mean issuer rate and counts recomputed from the funnel rows", {
  ok <- vapply(seq_len(nrow(sr)), function(i) { a <- fun[fun$state == sr$state[i], ]
    sr$n_issuers[i] == nrow(a) && sr$n_claims[i] == sum(a$received) && sr$n_denied[i] == sum(a$denied) &&
      near(sr$pooled_denial_rate[i], sum(a$denied) / sum(a$received), 1e-12) && near(sr$mean_issuer_rate[i], mean(a$denied / a$received), 1e-12) &&
      sr$n_issuers_in_puf[i] == sum(iss$state == sr$state[i]) }, logical(1)); all(ok) })
check("state rates: 30 states; issuer counts sum to 158 funnel issuers and 185 in the file; claims sum to the funnel total; denied <= claims; rates in [0,1]", {
  nrow(sr) == 30L && sum(sr$n_issuers) == 158L && sum(sr$n_issuers_in_puf) == 185L && sum(sr$n_claims) == sum(fun$received) && all(sr$n_denied <= sr$n_claims) &&
    all(sr$pooled_denial_rate >= 0 & sr$pooled_denial_rate <= 1) && all(sr$mean_issuer_rate >= 0 & sr$mean_issuer_rate <= 1) })

# ---- 6. cohort flow and plan characteristics ---------------------------------------
fl <- rd("desc_cohort_flow.csv"); pc <- rd("desc_plan_characteristics.csv")
check("cohort flow closes: 4,159 - 1,600 - 5 - 11 - 16 = 2,527; counts equal the independent classification of plans.csv", {
  na_pl <- pl$plan_claims_received_in_network_marker == "na"; r_sup <- pl$plan_claims_received_in_network_marker == "star2"
  r0 <- !is.na(rn) & rn == 0; d_sup <- !is.na(rn) & rn > 0 & pl$plan_claims_denied_in_network_marker == "star2"
  fl$n_plans_remaining[1] == 4159L && fl$n_plans_excluded_at_step[2] == sum(na_pl) && fl$n_plans_excluded_at_step[3] == sum(r_sup) &&
    fl$n_plans_excluded_at_step[4] == sum(r0) && fl$n_plans_excluded_at_step[5] == sum(d_sup) &&
    fl$n_plans_remaining[6] == sum(is_comp) && 4159L - sum(fl$n_plans_excluded_at_step) == fl$n_plans_remaining[6] &&
    identical(as.integer(fl$n_plans_excluded_at_step[2:5]), c(1600L, 5L, 11L, 16L)) })
check("cohort flow: 149 issuers and 30 states in the computable set; claims in the computable set recomputed", {
  cp <- pl[is_comp, ]; fl$n_issuers_remaining[6] == length(unique(cp$issuer_id)) && fl$n_issuers_remaining[6] == 149L && fl$n_states_remaining[6] == length(unique(cp$state)) &&
    fl$in_network_claims_received_remaining[6] == sum(rn[is_comp]) })
check("plan characteristics: plan types, metal levels, states and enrollment availability each partition the 2,527 plans", {
  s <- function(dim) sum(pc$n_computable_plans[pc$dimension == dim])
  s("plan_type") == 2527L && s("metal_level") == 2527L && s("state") == 2527L && s("enrollment_availability") == 2527L && pc$n_computable_plans[pc$dimension == "all"] == 2527L })
check("plan characteristics recomputed independently from plans.csv (plan type, metal level, issuers, enrollment markers)", {
  cp <- pl[is_comp, ]
  g1 <- vapply(c("EPO", "HMO", "POS", "PPO"), function(x) sum(cp$plan_type == x) == pc$n_computable_plans[pc$dimension == "plan_type" & pc$level == x] &&
                 length(unique(cp$issuer_id[cp$plan_type == x])) == pc$n_issuers[pc$dimension == "plan_type" & pc$level == x], logical(1))
  g2 <- vapply(c("Bronze", "Silver", "Gold", "Platinum", "Catastrophic"), function(x) sum(cp$metal_level == x) == pc$n_computable_plans[pc$dimension == "metal_level" & pc$level == x] &&
                 length(unique(cp$issuer_id[cp$metal_level == x])) == pc$n_issuers[pc$dimension == "metal_level" & pc$level == x], logical(1))
  em <- table(factor(cp$avg_monthly_enrollment_marker, levels = c("numeric", "star2", "star1", "na")))
  g3 <- c(em[["numeric"]] == pc$n_computable_plans[pc$level == "numeric"], em[["star2"]] == pc$n_computable_plans[pc$level == "suppressed_star2"],
          em[["star1"]] == pc$n_computable_plans[pc$level == "unavailable_star1"], em[["na"]] == pc$n_computable_plans[pc$level == "not_applicable"])
  all(g1) && all(g2) && all(g3) && em[["numeric"]] == 1942L })
check("plan characteristics: in-network denied <= received in every stratum and pooled rates in [0,1]", {
  all(pc$in_network_claims_denied <= pc$in_network_claims_received) && all(pc$pooled_in_network_denial_rate >= 0 & pc$pooled_in_network_denial_rate <= 1, na.rm = TRUE) })

# ---- 7. reason mix ----------------------------------------------------------------
rm_l <- rd("desc_reason_mix.csv"); rm_w <- rd("desc_reason_mix_wide.csv")
rmap <- utils::read.csv(file.path(P$config, "denial_reason_map.csv"), stringsAsFactors = FALSE)
cp <- pl[is_comp, ]
strat <- unique(rm_l[, c("stratum_type", "stratum")])
check("reason mix: shares of the ten-reason total sum to 1 within every stratum (10 strata)", {
  nrow(strat) == 10L && all(vapply(seq_len(nrow(strat)), function(i) { a <- rm_l[rm_l$stratum_type == strat$stratum_type[i] & rm_l$stratum == strat$stratum[i], ]
    nrow(a) == 10L && abs(sum(a$share_of_ten_reason_total) - 1) < 1e-12 }, logical(1))) })
check("reason mix: shares of the nine-reason total (without out-of-network provider) sum to 1 within every stratum", {
  all(vapply(seq_len(nrow(strat)), function(i) { a <- rm_l[rm_l$stratum_type == strat$stratum_type[i] & rm_l$stratum == strat$stratum[i], ]
    abs(sum(a$share_of_nine_reason_total, na.rm = TRUE) - 1) < 1e-12 && is.na(a$share_of_nine_reason_total[a$reason == "plan_reason_out_of_network_provider"]) }, logical(1))) })
check("reason mix wide: shares sum to 1 in every row and match the long file", {
  sc <- grep("^share_", names(rm_w), value = TRUE); sc <- setdiff(sc, "share_other_of_nine")
  length(sc) == 10L && all(abs(rowSums(rm_w[, sc]) - 1) < 1e-12) &&
    near(rm_w$share_other, vapply(seq_len(nrow(rm_w)), function(i) rm_l$share_of_ten_reason_total[rm_l$stratum == rm_w$stratum[i] & rm_l$stratum_type == rm_w$stratum_type[i] & rm_l$is_other], numeric(1)), 1e-12) })
check("reason mix: tallies and suppressed-cell counts recomputed independently from plans.csv for every stratum and reason", {
  ok <- logical(0)
  for (i in seq_len(nrow(strat))) {
    d <- switch(strat$stratum_type[i], all = cp, plan_type = cp[cp$plan_type == strat$stratum[i], ], metal_level = cp[cp$metal_level == strat$stratum[i], ])
    for (v in rmap$variable) {
      x <- numcol(d, v); r <- rm_l[rm_l$stratum_type == strat$stratum_type[i] & rm_l$stratum == strat$stratum[i] & rm_l$reason == v, ]
      ok <- c(ok, r$tally_sum == sum(x, na.rm = TRUE) && r$n_cells_suppressed == sum(d[[paste0(v, "_marker")]] == "star2") &&
                r$n_cells_reported + r$n_cells_suppressed + r$n_cells_other_missing == nrow(d) && r$n_plans == nrow(d) && r$n_cells_reported == sum(!is.na(x)))
    } }
  all(ok) })
check("reason mix: the label column carries the human-readable labels from denial_reason_map.csv", {
  a <- rm_l[rm_l$stratum == "All plans", ]; identical(a$reason, rmap$variable) && identical(a$label, rmap$label) && any(a$label == "Other reason not listed") && !any(grepl("_", a$label)) })
check("reason mix: All plans tallies equal the sum over plan types and the sum over metal levels (strata partition the plans)", {
  all(vapply(rmap$variable, function(v) { t_all <- rm_l$tally_sum[rm_l$stratum == "All plans" & rm_l$reason == v]
    t_pt <- sum(rm_l$tally_sum[rm_l$stratum_type == "plan_type" & rm_l$reason == v]); t_ml <- sum(rm_l$tally_sum[rm_l$stratum_type == "metal_level" & rm_l$reason == v])
    t_all == t_pt && t_all == t_ml }, logical(1))) })
check("reason mix: suppressed (**) cells are never added to a tally (a plan with a ** cell contributes nothing to that reason)", {
  v <- "plan_reason_medical_necessity_behavioral_health"; x <- numcol(cp, v); sum(is.na(x)) == sum(cp[[paste0(v, "_marker")]] == "star2") && sum(is.na(x)) > 0 })

# ---- 8. variance partition ------------------------------------------------------------
vp <- rd("desc_variance_partition.csv")
check("vpc_table algebra: shares sum to 1, among-levels shares sum to 1, known case (1,1,1) gives 1/(3 + pi^2/3), MOR formula", {
  t <- vpc_table(1, 1, 1); u <- vpc_table(0.1, 0.2, 0.3)
  abs(sum(t$vpc_latent) - 1) < 1e-12 && abs(sum(t$vpc_among_levels, na.rm = TRUE) - 1) < 1e-12 && near(t$vpc_latent[1], 1 / (3 + pi^2 / 3), 1e-12) && near(t$vpc_among_levels[1:3], rep(1 / 3, 3), 1e-12) &&
    near(u$median_odds_ratio[2], exp(sqrt(2 * 0.2) * qnorm(0.75)), 1e-12) && near(u$variance[4], pi^2 / 3, 1e-12) && is.na(u$median_odds_ratio[4]) && is.na(u$vpc_among_levels[4]) })
check("variance partition file: 4 components, latent VPCs sum to 1, among-levels VPCs of state, issuer, plan sum to 1", {
  identical(vp$component, c("state", "issuer_within_state", "plan", "claim_residual")) && abs(sum(vp$vpc_latent) - 1) < 1e-9 && abs(sum(vp$vpc_among_levels, na.rm = TRUE) - 1) < 1e-9 && near(vp$variance[4], pi^2 / 3, 1e-12) })
check("variance partition: model converged cleanly, singularity recorded, 2,527 plans / 149 issuers / 30 states, exclusions 16 + 5 + 11 counted", {
  v <- vp[1, ]; isTRUE(as.logical(v$converged_clean)) && is.logical(v$singular) && v$n_plans == 2527L && v$n_issuers == 149L && v$n_states == 30L &&
    v$plans_excluded_denied_suppressed == 16L && v$plans_excluded_received_suppressed == 5L && v$plans_excluded_received_zero == 11L && v$bootstrap_usable > 0.9 * v$bootstrap_nsim })
check("variance partition: independent refit (state + state:issuer + plan, default optimizer) reproduces the three variances and singularity", {
  cp <- pl[is_comp, ]
  d <- data.frame(denied = numcol(cp, "plan_claims_denied_in_network"), failed = numcol(cp, "plan_claims_received_in_network") - numcol(cp, "plan_claims_denied_in_network"),
                  state = factor(cp$state), issuer = factor(cp$issuer_id), plan = factor(cp$plan_id))
  m <- suppressWarnings(glmer(cbind(denied, failed) ~ 1 + (1 | state) + (1 | state:issuer) + (1 | plan), data = d, family = binomial))
  v <- as.data.frame(VarCorr(m)); g <- function(n) v$vcov[v$grp == n]
  near(g("state"), vp$variance[1], 5e-4) && near(g("state:issuer"), vp$variance[2], 5e-4) && near(g("plan"), vp$variance[3], 5e-4) && identical(isSingular(m, tol = 1e-4), vp$singular[1]) })
check("variance partition: bootstrap intervals bracket the estimates and are ordered", {
  a <- vp[1:3, ]; all(a$variance_ci_lower <= a$variance & a$variance <= a$variance_ci_upper) && all(a$vpc_among_levels_ci_lower <= a$vpc_among_levels & a$vpc_among_levels <= a$vpc_among_levels_ci_upper) &&
    all(a$median_odds_ratio_ci_lower <= a$median_odds_ratio & a$median_odds_ratio <= a$median_odds_ratio_ci_upper) })
check("simulation, three-level binomial truth (0.10, 0.15, 0.10): vp_fit recovers the variances (state +/-40 percent, issuer +/-17, plan +/-8; about 3 SE each)", {
  set.seed(20261028); G <- 200; J <- 5; K <- 3
  st <- rep(seq_len(G), each = J * K); is <- rep(seq_len(G * J), each = K)
  b0 <- -1.5 + rnorm(G, 0, sqrt(0.10))[st] + rnorm(G * J, 0, sqrt(0.15))[is] + rnorm(G * J * K, 0, sqrt(0.10))
  n <- round(exp(rnorm(G * J * K, 11, 0.7))); y <- rbinom(length(n), n, plogis(b0))
  d <- data.frame(denied = y, failed = n - y, state = factor(st), issuer = factor(is), plan = factor(seq_along(n)))
  f <- vp_fit(d); v <- vp_variances(f$fit); cat("      (estimates ", paste(sprintf("%.4f", v), collapse = ", "), ")\n", sep = "")
  abs(v[["state"]] / 0.10 - 1) < 0.40 && abs(v[["issuer"]] / 0.15 - 1) < 0.17 && abs(v[["plan"]] / 0.10 - 1) < 0.08 })
check("vp_fit fallback: a crippled first attempt is rejected and the next attempt is used; if every attempt is crippled it stops", {
  set.seed(1); G <- 40; st <- rep(seq_len(G), each = 6); is <- rep(seq_len(G * 2), each = 3)
  b0 <- -1.5 + rnorm(G, 0, 0.3)[st] + rnorm(G * 2, 0, 0.4)[is] + rnorm(G * 6, 0, 0.3); n <- round(exp(rnorm(G * 6, 9, 0.5))); y <- rbinom(length(n), n, plogis(b0))
  d <- data.frame(denied = y, failed = n - y, state = factor(st), issuer = factor(is), plan = factor(seq_along(n)))
  bad <- list(name = "crippled", nAGQ = 1L, optimizer = "bobyqa", maxfun = 5)
  r <- vp_fit(d, attempts = list(bad, VP_ATTEMPTS[[1]]))
  stopped <- inherits(try(vp_fit(d, attempts = list(bad)), silent = TRUE), "try-error")
  r$attempt_index == 2L && r$attempt$name == "laplace_bobyqa" && stopped })

# ---- 9. ledger ---------------------------------------------------------------------------
lp <- file.path(P$ledger_parts, "ledger_desc.csv")
LD <- .ledger_read_part(lp, "desc")
SRCM <- utils::read.csv(file.path(P$logs, "wp6a_ledger_sources.csv"), colClasses = "character", na.strings = character(0), stringsAsFactors = FALSE)
check("ledger part passes the shared validators (snake_case keys, no duplicates, no NA / Inf / empty values, no leading-zero ids)", {
  r <- try(.ledger_validate_table(LD, "test_desc"), silent = TRUE); !inherits(r, "try-error") && nrow(LD) > 150L })
check("ledger: no NA, no marker strings, every value is a finite number or a short state code", {
  v <- LD$value; num <- suppressWarnings(as.numeric(v))
  !anyNA(v) && !any(v %in% c("*", "**", "***", "N/A", "NA", "")) && all(is.finite(num[!is.na(num)])) && all(grepl("^[A-Z]{2}$", v[is.na(num)])) })
check("ledger: only the stage prefixes att_, hetero_, reason_ and cohort_all_ are used; cohort keys are all cohort_all_", {
  all(grepl("^(att_|hetero_|reason_|cohort_all_)", LD$key)) && !any(grepl("^cohort_", LD$key) & !grepl("^cohort_all_", LD$key)) })
check("ledger: no key duplicates a key in ledger_kff.csv (the national att_ totals are not repeated here)", !any(LD$key %in% kff$key))
check("ledger: sources map lists exactly the ledger keys", setequal(SRCM$key, LD$key) && !anyDuplicated(SRCM$key))
check("ledger: every cell-type value equals the CSV value it came from", {
  cellr <- SRCM[SRCM$kind == "cell", ]
  ok <- vapply(seq_len(nrow(cellr)), function(i) {
    d <- rd(cellr$csv[i]); sel <- rep(TRUE, nrow(d))
    if (nzchar(cellr$where[i])) for (w in strsplit(cellr$where[i], ";", fixed = TRUE)[[1]]) { kv <- strsplit(w, "=", fixed = TRUE)[[1]]; sel <- sel & as.character(d[[kv[1]]]) == kv[2] }
    if (sum(sel) != 1L) return(FALSE)
    v <- d[[cellr$column[i]]][sel]; if (is.logical(v)) v <- as.numeric(v)
    lv_ <- LD$value[LD$key == cellr$key[i]]
    if (is.numeric(v)) near(as.numeric(lv_), v, 1e-12) else identical(lv_, as.character(v)) }, logical(1))
  if (all(ok)) TRUE else cellr$key[!ok] })
check("ledger: the aggregate-type values equal independent recomputations", {
  g <- function(k) as.numeric(LD$value[LD$key == k])
  srx <- rd("desc_state_rates.csv"); rml <- rd("desc_reason_mix.csv")
  g("hetero_states") == nrow(srx) && LD$value[LD$key == "hetero_state_lowest"] == srx$state[which.min(srx$n_denied / srx$n_claims)] &&
    LD$value[LD$key == "hetero_state_highest"] == srx$state[which.max(srx$n_denied / srx$n_claims)] &&
    g("hetero_states_single_issuer") == sum(srx$n_issuers == 1L) &&
    g("reason_suppressed_cells_all") == sum(vapply(rmap$variable, function(v) sum(cp[[paste0(v, "_marker")]] == "star2"), numeric(1))) &&
    g("reason_cells_all") == 10L * nrow(cp) })
check("ledger: spot checks against independent numbers (funnel counts, tau2, variance partition, cohort, reasons, ratios)", {
  g <- function(k) as.numeric(LD$value[LD$key == k])
  g("hetero_funnel_issuers") == nrow(fun) && g("hetero_funnel_outside_998_adjusted") == sum(fun$outside_998_adjusted) && g("hetero_funnel_outside_998_unadjusted") == sum(fun$outside_998_unadjusted) &&
    near(g("hetero_funnel_tau2"), fs$tau2_winsorised, 1e-12) && near(g("hetero_vp_var_issuer"), vp$variance[2], 1e-12) && near(g("hetero_vp_vpc_among_issuer"), vp$variance[2] / sum(vp$variance[1:3]), 1e-9) &&
    g("cohort_all_plans_computable") == sum(is_comp) && g("cohort_all_plans_excluded_denied_suppressed") == 16 && near(g("reason_share_other_all"), rm_l$share_of_ten_reason_total[rm_l$stratum == "All plans" & rm_l$is_other], 1e-12) &&
    near(g("att_ratio_denial_rate_in_network"), sum(fun$denied) / sum(fun$received), 1e-12) && g("att_oon_anomalous_issuers") == 2 && g("att_oon_anomalous_plans") == 30 })
check("ledger: keys needed by Fig 1, Fig 2, Table 1, Table 2 and the heterogeneity sentences all exist", {
  need <- c("att_claims_received_combined", "att_claims_denied_combined", "att_ratio_internal_appeals_filed_per1000_denied_in_network", "att_ratio_external_appeals_filed_per1000_denied_in_network",
            "att_ratio_resubmitted_per1000_denied_in_network", "hetero_funnel_tau2", "hetero_funnel_pooled_rate", "hetero_funnel_outside_998_adjusted", "hetero_funnel_outside_998_unadjusted",
            "hetero_vp_vpc_among_state", "hetero_vp_vpc_among_issuer", "hetero_vp_vpc_among_plan", "hetero_vp_vpc_latent_issuer", "cohort_all_plans_computable", "cohort_all_issuers_computable",
            "reason_share_other_all", "reason_suppressed_cells_all", "reason_ten_reason_total_all", "cohort_all_plans_enrollment_numeric", "cohort_all_plans_epo", "cohort_all_plans_bronze")
  if (all(need %in% LD$key)) TRUE else setdiff(need, LD$key) })

cat("\n=====\n"); cat(sprintf("test_desc.R: %d passed, %d failed\n", N_PASS, N_FAIL))
if (N_FAIL > 0L) { cat("FAILED:\n"); cat(paste0("  - ", FAILS), sep = "\n"); quit(status = 1) }
