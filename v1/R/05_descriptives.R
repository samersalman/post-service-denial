# =====================================================================
# 05_descriptives.R : WP6a. Descriptive analyses on the CMS tables only.
# =====================================================================
# This script never reads, loads or joins the directory-derived (orthopaedic)
# layer. Inputs: the cleaned CMS tables (issuer_state.csv, plans.csv), the denial
# reason map, study_config.yaml (read directly), and ledger_kff.csv (tie-out only).
# No imputation: a suppressed or N/A cell is missing and is counted, never filled.
#
# Outputs (v1/results): desc_attrition.csv, desc_attrition_ratios.csv,
#   desc_oon_exclusions.csv, desc_issuer_funnel.csv, desc_funnel_summary.csv,
#   desc_funnel_limits_grid.csv, desc_funnel_method.txt, desc_state_rates.csv,
#   desc_reason_mix.csv, desc_reason_mix_wide.csv, desc_cohort_flow.csv,
#   desc_plan_characteristics.csv, desc_variance_partition.csv,
#   ledger_parts/ledger_desc.csv ; v1/logs/wp6a_run.log, wp6a_ledger_sources.csv
# Run: Rscript "<root>/v1/R/05_descriptives.R"  (about 3 minutes, mostly the bootstrap)
#
# ---- FUNNEL (issuer level, in-network, proportion scale) -------------
# Analyst implementation of a Spiegelhalter-style funnel plot with an additive
# random-effects overdispersion term. The source papers were verified at abstract
# level only, so the limit levels (95 and 99.8 percent), the 10 percent Winsorisation
# and the formulas below are analyst choices; none is attributed to the papers.
# For issuer i of I issuers with n_i in-network claim lines received and y_i denied
# (both reported numeric cells):
#   p_i  = y_i / n_i
#   p0   = sum(y) / sum(n)                      pooled rate: target and centre line
#   se_i = sqrt(p0 (1 - p0) / n_i)              se_binomial (null standard error)
#   z_i  = (p_i - p0) / se_i
#   zw_i = min(max(z_i, q10), q90)              Winsorised z: q10 and q90 are the 10th and
#                                               90th percentiles of the z_i (stats::quantile,
#                                               type 7), so 10 percent in each tail is clipped
#   w_i  = 1 / se_i^2 ;  Q = sum(zw_i^2) ;  phi = Q / I          (multiplicative factor)
#   tau2 = max(0, (Q - (I - 1)) / (sum(w) - sum(w^2) / sum(w)))  (additive; DerSimonian-Laird form)
# Limits at level L, critical value zc = qnorm(1 - (1 - L) / 2) (1.96 and 3.09):
#   without overdispersion term:  p0 +/- zc * se_i
#   with additive term:           p0 +/- zc * sqrt(se_i^2 + tau2)
#   multiplicative (counts only): p0 +/- zc * se_i * sqrt(max(1, phi))
# An issuer is outside when p_i is strictly below the lower or above the upper limit.
# Limits are written unclipped; plotting code clips them to [0, 1].
# Winsorising caps the largest |z| (dominated by the biggest issuers) but the weights
# in the denominator are not capped, so tau2 is conservative (small) and more issuers
# are flagged than with the unwinsorised estimate. The unwinsorised tau2 is therefore
# also written (sensitivity), with the counts it would give.
#
# ---- VARIANCE PARTITION (plan level) -----------------------------------
# lme4::glmer, null model on the computable plans (both in-network counts numeric,
# received above 0): cbind(denied, received - denied) ~ 1 + (1 | state) +
# (1 | issuer within state) + (1 | plan), logit link. The plan-level intercept is an
# observation-level random effect that absorbs overdispersion. Convention (latent
# response, as described for the variance partition coefficient and median odds ratio
# in the candidate reference austin2017, verified at abstract level only, so the exact
# formulas are stated here and not attributed): level-1 (claim) residual variance on
# the latent scale is pi^2 / 3, and
#   vpc_latent(level)        = s2_level / (s2_state + s2_issuer + s2_plan + pi^2 / 3)
#   vpc_among_levels(level)  = s2_level / (s2_state + s2_issuer + s2_plan)
#   median odds ratio(level) = exp(sqrt(2 * s2_level) * qnorm(0.75))
# Fit order (the documented fallback): Laplace (nAGQ = 1) with bobyqa; if that does not
# converge cleanly, nAGQ = 0 with bobyqa; then Laplace with Nelder-Mead. The plan-level
# term is never dropped automatically; if all three fail the script stops. Convergence
# is accepted only when the optimizer code is 0, lme4 reports no convergence message and
# no warning was raised. Singularity is checked with lme4::isSingular and reported.
# Interval estimates: parametric bootstrap (lme4::bootMer, percentile intervals).
#
# ---- OTHER CONVENTIONS ---------------------------------------------------
# Out-of-network denied exceeds received in the CMS file for 2 issuers and 30 plans
# (pinned anomaly). Those issuers are left out of every out-of-network or combined RATE
# computed here (the raw national totals include them, as the att_ ledger keys do).
# Reasons are tallies, not unique claims; nothing here relates them to denied claims.
# =====================================================================
if (!exists("P", inherits = TRUE)) {
  .args <- commandArgs(trailingOnly = FALSE)
  .f <- sub("^--file=", "", grep("^--file=", .args, value = TRUE)[1])
  .here <- dirname(normalizePath(gsub("~+~", " ", .f, fixed = TRUE)))
  source(file.path(.here, "00_setup.R"))
}
if (!exists("ledger_add", inherits = TRUE)) source(file.path(P$R, "ledger.R"))
ensure_pkgs(c("lme4", "readr", "yaml"))

SCRIPT <- "05_descriptives.R"
PART <- "desc"
LOG <- file.path(P$logs, "wp6a_run.log")
OUT <- P$results
lg <- function(..., level = "INFO") log_msg(..., level = level, file = LOG)

# ---- pure functions (also used by tests/test_desc.R) --------------------
funnel_fit <- function(n, y, winsor_p = 0.1) {
  n <- as.numeric(n); y <- as.numeric(y)
  if (length(n) != length(y) || length(n) < 3L) stop("funnel_fit(): n and y need the same length of at least 3")
  if (any(!is.finite(n)) || any(!is.finite(y)) || any(n <= 0) || any(y < 0) || any(y > n)) {
    stop("funnel_fit(): need finite n > 0 and 0 <= y <= n")
  }
  I <- length(n)
  p <- y / n
  p0 <- sum(y) / sum(n)
  if (!(p0 > 0 && p0 < 1)) stop("funnel_fit(): pooled rate must lie strictly between 0 and 1")
  se <- sqrt(p0 * (1 - p0) / n)
  z <- (p - p0) / se
  if (winsor_p > 0) {
    q <- stats::quantile(z, c(winsor_p, 1 - winsor_p), type = 7, names = FALSE)
    zw <- pmin(pmax(z, q[1]), q[2])
  } else {
    q <- c(NA_real_, NA_real_)
    zw <- z
  }
  w <- 1 / se^2
  Q <- sum(zw^2)
  denom <- sum(w) - sum(w^2) / sum(w)
  list(I = I, p = p, p0 = p0, se = se, z = z, zw = zw, q = q, w = w, Q = Q,
       phi = Q / I, denom = denom, tau2 = max(0, (Q - (I - 1)) / denom), winsor_p = winsor_p)
}

funnel_zcrit <- function(level) stats::qnorm(1 - (1 - level) / 2)

funnel_limits <- function(p0, n, level, tau2 = 0) {
  half <- funnel_zcrit(level) * sqrt(p0 * (1 - p0) / n + tau2)
  data.frame(lower = p0 - half, upper = p0 + half)
}

vpc_table <- function(v_state, v_issuer, v_plan) {
  v_claim <- pi^2 / 3
  v <- c(v_state, v_issuer, v_plan, v_claim)
  among <- v_state + v_issuer + v_plan
  data.frame(
    component = c("state", "issuer_within_state", "plan", "claim_residual"),
    variance = v,
    vpc_latent = v / sum(v),
    vpc_among_levels = c(v[1:3] / among, NA_real_),
    median_odds_ratio = c(exp(sqrt(2 * v[1:3]) * stats::qnorm(0.75)), NA_real_),
    stringsAsFactors = FALSE
  )
}

VP_ATTEMPTS <- list(
  list(name = "laplace_bobyqa", nAGQ = 1L, optimizer = "bobyqa", maxfun = 2e5),
  list(name = "pirls_nagq0_bobyqa", nAGQ = 0L, optimizer = "bobyqa", maxfun = 2e5),
  list(name = "laplace_nelder_mead", nAGQ = 1L, optimizer = "Nelder_Mead", maxfun = 4e5)
)
VP_FORMULA <- cbind(denied, failed) ~ 1 + (1 | state) + (1 | issuer) + (1 | plan)

vp_variances <- function(m) {
  v <- as.data.frame(lme4::VarCorr(m))
  out <- v$vcov[match(c("state", "issuer", "plan"), v$grp)]
  if (anyNA(out)) stop("vp_variances(): a random-effect group is missing from the fit")
  stats::setNames(out, c("state", "issuer", "plan"))
}

# dat: data.frame with denied, failed (= received - denied), state, issuer, plan (factors)
vp_fit_one <- function(dat, a) {
  warns <- character(0)
  # the call is built with its arguments inlined so that bootMer can refit from fit@call later
  fit <- tryCatch(
    withCallingHandlers(
      eval(bquote(lme4::glmer(.(VP_FORMULA), data = .(dat), family = stats::binomial, nAGQ = .(a$nAGQ),
                              control = lme4::glmerControl(optimizer = .(a$optimizer), optCtrl = list(maxfun = .(a$maxfun)))))),
      warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") }),
    error = function(e) e)
  if (inherits(fit, "error")) return(list(fit = NULL, ok = FALSE, msg = paste("error:", conditionMessage(fit))))
  conv <- fit@optinfo$conv
  msgs <- c(warns, unlist(conv$lme4$messages))
  ok <- identical(as.integer(conv$opt), 0L) && !length(conv$lme4) && !length(warns)
  list(fit = fit, ok = ok, msg = if (length(msgs)) paste(msgs, collapse = " | ") else "")
}

vp_fit <- function(dat, attempts = VP_ATTEMPTS) {
  log <- character(0)
  for (i in seq_along(attempts)) {
    r <- vp_fit_one(dat, attempts[[i]])
    log <- c(log, paste0(attempts[[i]]$name, ": ", if (r$ok) "clean" else paste0("NOT clean (", r$msg, ")")))
    if (r$ok) return(list(fit = r$fit, attempt = attempts[[i]], attempt_index = i, log = log))
  }
  stop("vp_fit(): the full three-level model did not converge cleanly in any attempt: ", paste(log, collapse = " ; "))
}

# ---- run everything unless a test asked only for the functions ------------
if (!isTRUE(getOption("psd_desc_no_run"))) {

lg("=== 05_descriptives.R start ===")
cfg <- yaml::read_yaml(file.path(P$config, "study_config.yaml"))
SEED <- as.integer(cfg$study$seed)
LEVELS <- as.numeric(unlist(cfg$inference$funnel_ci_levels))
if (!identical(LEVELS, c(0.95, 0.998))) stop("config inference$funnel_ci_levels is not 0.95 and 0.998: ", paste(LEVELS, collapse = ", "))
set.seed(SEED)

read_clean <- function(f) {
  path <- file.path(P$clean_cms, f)
  hdr <- names(utils::read.csv(path, nrows = 1, check.names = FALSE))
  cc <- c(issuer_id = "character", plan_id = "character", state = "character")
  cc <- cc[names(cc) %in% hdr]
  utils::read.csv(path, colClasses = cc, na.strings = c("", "NA"), stringsAsFactors = FALSE, check.names = FALSE)
}
cells <- function(df, col) {
  m <- df[[paste0(col, "_marker")]]
  v <- as.numeric(df[[col]])
  if (anyNA(m)) stop("cells(): missing marker for ", col)
  if (any(!is.na(v) & m != "numeric")) stop("cells(): a non-numeric marker carries a value in ", col)
  if (any(is.na(v) & m == "numeric")) stop("cells(): a numeric marker has no value in ", col)
  v
}
mk <- function(df, col) {
  m <- df[[paste0(col, "_marker")]]
  c(numeric = sum(m == "numeric"), star1 = sum(m == "star1"), star2 = sum(m == "star2"),
    star3 = sum(m == "star3"), na = sum(m == "na"))
}
wcsv <- function(df, file) {
  readr::write_csv(df, file.path(OUT, file), na = "")
  lg("wrote ", file, " (", nrow(df), " rows)")
  invisible(df)
}

iss <- read_clean("issuer_state.csv")
pl <- read_clean("plans.csv")
if (nrow(iss) != 185L || anyDuplicated(iss$issuer_id)) stop("issuer_state.csv: expected 185 unique issuers")
if (nrow(pl) != 4159L || anyDuplicated(pl$plan_id)) stop("plans.csv: expected 4,159 unique plans")
if (!all(is.character(iss$issuer_id), is.character(pl$plan_id), is.character(pl$issuer_id))) stop("ids must be character")
kff <- utils::read.csv(file.path(P$ledger_parts, "ledger_kff.csv"), colClasses = "character", stringsAsFactors = FALSE)
kffv <- function(k) { i <- match(k, kff$key); if (is.na(i)) stop("ledger_kff.csv lacks key ", k); as.numeric(kff$value[i]) }

# ======================================================================
# 1. ATTRITION LADDER (national totals, 2024, Individual QHP)
# ======================================================================
ATT <- data.frame(
  measure = c("claims_received", "claims_received", "claims_denied", "claims_denied",
              "claims_resubmitted", "claims_resubmitted",
              "internal_appeals_filed", "internal_appeals_overturned",
              "external_appeals_filed", "external_appeals_overturned"),
  network = c("in_network", "out_of_network", "in_network", "out_of_network", "in_network", "out_of_network",
              "not_split", "not_split", "not_split", "not_split"),
  col = c("issuer_claims_received_in_network", "issuer_claims_received_out_of_network",
          "issuer_claims_denied_in_network", "issuer_claims_denied_out_of_network",
          "issuer_claims_resubmitted_in_network", "issuer_claims_resubmitted_out_of_network",
          "issuer_internal_appeals_filed", "issuer_internal_appeals_overturned",
          "issuer_external_appeals_filed", "issuer_external_appeals_overturned"),
  label = c("Claim lines received", "Claim lines received", "Claim lines denied", "Claim lines denied",
            "Claim lines resubmitted", "Claim lines resubmitted",
            "Internal appeals filed", "Internal appeals overturned",
            "External appeals filed", "External appeals overturned"),
  unit = c(rep("claim lines", 6), rep("appeal requests", 4)),
  stringsAsFactors = FALSE)
ATT$att_key <- ifelse(ATT$network == "not_split", paste0("att_", ATT$measure),
                      paste0("att_", ATT$measure, "_", ATT$network))
anom_iss <- sort(iss$issuer_id[iss$out_of_network_denied_exceeds_received %in% TRUE])
anom_pl <- pl[pl$out_of_network_denied_exceeds_received %in% TRUE, , drop = FALSE]
lg("out-of-network anomaly: ", length(anom_iss), " issuers (", paste(anom_iss, collapse = ", "), "), ",
   nrow(anom_pl), " plans over ", length(unique(anom_pl$issuer_id)), " issuers")

att_rows <- lapply(seq_len(nrow(ATT)), function(i) {
  v <- cells(iss, ATT$col[i]); m <- mk(iss, ATT$col[i])
  data.frame(measure = ATT$measure[i], network = ATT$network[i], label = ATT$label[i], unit = ATT$unit[i],
             total = sum(v, na.rm = TRUE), issuers_reported = m[["numeric"]], issuers_suppressed = m[["star2"]],
             issuers_na = m[["na"]], issuers_other_missing = m[["star1"]] + m[["star3"]],
             issuers_in_file = nrow(iss), note = "", stringsAsFactors = FALSE)
})
att <- do.call(rbind, att_rows)
# tie-out to ledger_kff.csv att_ keys (assertion: the script stops if any differ)
tie <- do.call(rbind, lapply(seq_len(nrow(ATT)), function(i) {
  k <- ATT$att_key[i]; r <- att[i, ]
  data.frame(key = k,
             total_ok = isTRUE(all.equal(r$total, kffv(k), tolerance = 0)),
             reported_ok = r$issuers_reported == kffv(paste0(k, "_issuers_reported")),
             suppressed_ok = r$issuers_suppressed == kffv(paste0(k, "_issuers_suppressed")),
             stringsAsFactors = FALSE)
}))
if (kffv("att_issuers_na_issuer_level") != att$issuers_na[1]) stop("att_issuers_na_issuer_level does not tie")
if (!all(unlist(tie[, -1]))) stop("attrition totals do not equal the att_ keys in ledger_kff.csv: ", paste(tie$key[!apply(tie[, -1], 1, all)], collapse = ", "))
lg("attrition tie-out to ledger_kff.csv att_ keys: ", nrow(tie), " of ", nrow(tie), " measures tie (total, issuers reported, issuers suppressed)")
att$note[att$network == "out_of_network" & att$measure == "claims_denied"] <-
  "Includes the 2 anomalous issuers whose denied count exceeds received, as published"
att$note[att$network == "not_split"] <- "Appeals are not split by network; not a subset of in-network or out-of-network lines"
att$note[att$measure == "claims_resubmitted"] <- "Resubmitted is not a subset of denied (all resubmitted claims regardless of outcome)"
comb <- do.call(rbind, lapply(c("claims_received", "claims_denied", "claims_resubmitted"), function(ms) {
  ci <- ATT$col[ATT$measure == ms & ATT$network == "in_network"]
  co <- ATT$col[ATT$measure == ms & ATT$network == "out_of_network"]
  mi <- iss[[paste0(ci, "_marker")]]; mo <- iss[[paste0(co, "_marker")]]
  data.frame(measure = ms, network = "combined", label = ATT$label[match(ms, ATT$measure)], unit = "claim lines",
             total = att$total[att$measure == ms & att$network == "in_network"] + att$total[att$measure == ms & att$network == "out_of_network"],
             issuers_reported = sum(mi == "numeric" & mo == "numeric"),
             issuers_suppressed = sum(mi == "star2" | mo == "star2"),
             issuers_na = sum(mi == "na" | mo == "na"),
             issuers_other_missing = sum(mi %in% c("star1", "star3") | mo %in% c("star1", "star3")),
             issuers_in_file = nrow(iss),
             note = "In-network total plus out-of-network total; issuers_reported counts issuers with both cells reported",
             stringsAsFactors = FALSE)
}))
att_all <- rbind(att, comb)
ord <- c("claims_received", "claims_denied", "claims_resubmitted", "internal_appeals_filed",
         "internal_appeals_overturned", "external_appeals_filed", "external_appeals_overturned")
att_all <- att_all[order(match(att_all$measure, ord), match(att_all$network, c("in_network", "out_of_network", "combined", "not_split"))), ]
rownames(att_all) <- NULL
wcsv(att_all, "desc_attrition.csv")

# ratios on matched issuer sets (issuers that reported every numerator and denominator cell)
mcol <- function(ms, nw) ATT$col[ATT$measure == ms & ATT$network == nw]
RAT <- data.frame(
  ratio_id = c("resubmitted_per1000_denied_in_network", "internal_appeals_filed_per1000_denied_in_network",
               "internal_appeals_overturned_per1000_denied_in_network", "external_appeals_filed_per1000_denied_in_network",
               "external_appeals_overturned_per1000_denied_in_network",
               "resubmitted_per1000_denied_combined", "internal_appeals_filed_per1000_denied_combined",
               "internal_appeals_overturned_per1000_denied_combined", "external_appeals_filed_per1000_denied_combined",
               "external_appeals_overturned_per1000_denied_combined",
               "resubmitted_per1000_denied_out_of_network",
               "denial_rate_in_network", "denial_rate_out_of_network", "denial_rate_combined",
               "internal_overturned_per1000_internal_filed", "external_overturned_per1000_external_filed"),
  num = c(mcol("claims_resubmitted", "in_network"), mcol("internal_appeals_filed", "not_split"),
          mcol("internal_appeals_overturned", "not_split"), mcol("external_appeals_filed", "not_split"),
          mcol("external_appeals_overturned", "not_split"),
          paste(mcol("claims_resubmitted", "in_network"), mcol("claims_resubmitted", "out_of_network"), sep = ";"),
          mcol("internal_appeals_filed", "not_split"), mcol("internal_appeals_overturned", "not_split"),
          mcol("external_appeals_filed", "not_split"), mcol("external_appeals_overturned", "not_split"),
          mcol("claims_resubmitted", "out_of_network"),
          mcol("claims_denied", "in_network"), mcol("claims_denied", "out_of_network"),
          paste(mcol("claims_denied", "in_network"), mcol("claims_denied", "out_of_network"), sep = ";"),
          mcol("internal_appeals_overturned", "not_split"), mcol("external_appeals_overturned", "not_split")),
  den = c(rep(mcol("claims_denied", "in_network"), 5),
          rep(paste(mcol("claims_denied", "in_network"), mcol("claims_denied", "out_of_network"), sep = ";"), 5),
          mcol("claims_denied", "out_of_network"),
          mcol("claims_received", "in_network"), mcol("claims_received", "out_of_network"),
          paste(mcol("claims_received", "in_network"), mcol("claims_received", "out_of_network"), sep = ";"),
          mcol("internal_appeals_filed", "not_split"), mcol("external_appeals_filed", "not_split")),
  network = c(rep("in_network", 5), rep("combined", 5), "out_of_network", "in_network", "out_of_network", "combined",
              "not_split", "not_split"),
  numerator_label = c("Claim lines resubmitted (in-network)", "Internal appeals filed", "Internal appeals overturned",
                      "External appeals filed", "External appeals overturned",
                      "Claim lines resubmitted (in-network plus out-of-network)", "Internal appeals filed",
                      "Internal appeals overturned", "External appeals filed", "External appeals overturned",
                      "Claim lines resubmitted (out-of-network)",
                      "Claim lines denied (in-network)", "Claim lines denied (out-of-network)",
                      "Claim lines denied (in-network plus out-of-network)",
                      "Internal appeals overturned", "External appeals overturned"),
  denominator_label = c(rep("Claim lines denied (in-network)", 5),
                        rep("Claim lines denied (in-network plus out-of-network)", 5),
                        "Claim lines denied (out-of-network)",
                        "Claim lines received (in-network)", "Claim lines received (out-of-network)",
                        "Claim lines received (in-network plus out-of-network)",
                        "Internal appeals filed", "External appeals filed"),
  scale = c(rep(1000, 11), 1, 1, 1, 1000, 1000),
  scale_label = c(rep("per 1,000 denied claim lines", 11), rep("proportion", 3), "per 1,000 internal appeals filed",
                  "per 1,000 external appeals filed"),
  excl_oon = c(rep(FALSE, 5), rep(TRUE, 6), FALSE, TRUE, TRUE, FALSE, FALSE),
  stringsAsFactors = FALSE)
rat_rows <- lapply(seq_len(nrow(RAT)), function(i) {
  nc <- strsplit(RAT$num[i], ";", fixed = TRUE)[[1]]; dc <- strsplit(RAT$den[i], ";", fixed = TRUE)[[1]]
  d <- iss
  if (RAT$excl_oon[i]) d <- d[!(d$issuer_id %in% anom_iss), , drop = FALSE]
  ok <- rep(TRUE, nrow(d))
  for (cn in c(nc, dc)) ok <- ok & d[[paste0(cn, "_marker")]] == "numeric"
  mm <- d[ok, , drop = FALSE]
  ns <- sum(Reduce(`+`, lapply(nc, function(cn) as.numeric(mm[[cn]]))))
  ds <- sum(Reduce(`+`, lapply(dc, function(cn) as.numeric(mm[[cn]]))))
  data.frame(ratio_id = RAT$ratio_id[i], network = RAT$network[i], numerator = RAT$numerator_label[i],
             denominator = RAT$denominator_label[i], scale_label = RAT$scale_label[i], scale = RAT$scale[i],
             n_issuers_matched = nrow(mm),
             n_issuers_excluded_anomalous = if (RAT$excl_oon[i]) sum(iss$issuer_id %in% anom_iss) else 0L,
             numerator_sum = ns, denominator_sum = ds, ratio = RAT$scale[i] * ns / ds, stringsAsFactors = FALSE)
})
ratios <- do.call(rbind, rat_rows)
wcsv(ratios, "desc_attrition_ratios.csv")

oonx <- data.frame(
  scope = c("issuers", "plans", "issuers_holding_the_plans"),
  n_excluded = c(length(anom_iss), nrow(anom_pl), length(unique(anom_pl$issuer_id))),
  ids = c(paste(anom_iss, collapse = ";"), "", paste(sort(unique(anom_pl$issuer_id)), collapse = ";")),
  description = c("Issuers whose out-of-network denied count exceeds received (left out of out-of-network and combined rates)",
                  "Plans whose out-of-network denied count exceeds received (flagged; no plan-level out-of-network rate is used)",
                  "Issuers that hold those plans"),
  stringsAsFactors = FALSE)
wcsv(oonx, "desc_oon_exclusions.csv")

# ======================================================================
# 2. ISSUER FUNNEL (in-network denial rate vs claim volume)
# ======================================================================
rin <- "issuer_claims_received_in_network"; din <- "issuer_claims_denied_in_network"
mr <- iss[[paste0(rin, "_marker")]]; md <- iss[[paste0(din, "_marker")]]
n_na <- sum(mr == "na"); n_denied_supp <- sum(mr == "numeric" & md == "star2")
if (any(mr == "numeric" & !(md %in% c("numeric", "star2")))) stop("unexpected denied marker among issuers with a numeric received count")
use <- mr == "numeric" & md == "numeric"
f <- iss[use, , drop = FALSE]
n_i <- as.numeric(f[[rin]]); y_i <- as.numeric(f[[din]])
if (any(y_i > n_i)) stop("in-network denied exceeds received for a funnel issuer")
ff <- funnel_fit(n_i, y_i, winsor_p = 0.1)
fu <- funnel_fit(n_i, y_i, winsor_p = 0)
zc95 <- funnel_zcrit(0.95); zc998 <- funnel_zcrit(0.998)
L95u <- funnel_limits(ff$p0, n_i, 0.95, 0); L998u <- funnel_limits(ff$p0, n_i, 0.998, 0)
L95a <- funnel_limits(ff$p0, n_i, 0.95, ff$tau2); L998a <- funnel_limits(ff$p0, n_i, 0.998, ff$tau2)
outside <- function(p, L) p < L$lower | p > L$upper
fun <- data.frame(
  issuer_id = f$issuer_id, state = f$state, issuer_name = f$issuer_name, received = n_i, denied = y_i, rate = ff$p,
  se_binomial = ff$se, z = ff$z, z_winsorised = ff$zw,
  limit_95_lower_unadjusted = L95u$lower, limit_95_upper_unadjusted = L95u$upper,
  limit_998_lower_unadjusted = L998u$lower, limit_998_upper_unadjusted = L998u$upper,
  limit_95_lower_adjusted = L95a$lower, limit_95_upper_adjusted = L95a$upper,
  limit_998_lower_adjusted = L998a$lower, limit_998_upper_adjusted = L998a$upper,
  outside_95_unadjusted = outside(ff$p, L95u), outside_998_unadjusted = outside(ff$p, L998u),
  outside_95_adjusted = outside(ff$p, L95a), outside_998_adjusted = outside(ff$p, L998a),
  direction_998_adjusted = ifelse(ff$p > L998a$upper, "above", ifelse(ff$p < L998a$lower, "below", "within")),
  over_1000_claims = n_i > 1000, stringsAsFactors = FALSE)
fun <- fun[order(-fun$received), ]; rownames(fun) <- NULL
wcsv(fun, "desc_issuer_funnel.csv")

# sensitivity: additive limits with the unwinsorised tau2; multiplicative counts
L95s <- funnel_limits(ff$p0, n_i, 0.95, fu$tau2); L998s <- funnel_limits(ff$p0, n_i, 0.998, fu$tau2)
mult_out <- function(zc) sum(abs(ff$z) > zc * sqrt(max(1, ff$phi)))
q <- stats::quantile(ff$p, c(0.25, 0.5, 0.75), type = 7, names = FALSE)
fsum <- data.frame(
  n_issuers_in_puf = nrow(iss), n_issuers_na = n_na, n_issuers_received_numeric = sum(mr == "numeric"),
  n_issuers_denied_suppressed = n_denied_supp, n_issuers_funnel = ff$I, n_issuers_over_1000 = sum(n_i > 1000),
  total_received = sum(n_i), total_denied = sum(y_i), pooled_rate = ff$p0,
  issuer_rate_min = min(ff$p), issuer_rate_p25 = q[1], issuer_rate_median = q[2], issuer_rate_p75 = q[3], issuer_rate_max = max(ff$p),
  winsor_probability = ff$winsor_p, winsor_z_lower = ff$q[1], winsor_z_upper = ff$q[2],
  q_statistic = ff$Q, phi_winsorised = ff$phi, tau2_winsorised = ff$tau2, tau_winsorised = sqrt(ff$tau2),
  phi_unwinsorised = fu$phi, tau2_unwinsorised = fu$tau2,
  z_crit_95 = zc95, z_crit_998 = zc998,
  outside_95_unadjusted = sum(fun$outside_95_unadjusted), outside_998_unadjusted = sum(fun$outside_998_unadjusted),
  outside_95_adjusted = sum(fun$outside_95_adjusted), outside_998_adjusted = sum(fun$outside_998_adjusted),
  above_998_adjusted = sum(fun$direction_998_adjusted == "above"), below_998_adjusted = sum(fun$direction_998_adjusted == "below"),
  outside_95_multiplicative = mult_out(zc95), outside_998_multiplicative = mult_out(zc998),
  outside_95_adjusted_unwinsorised_tau2 = sum(outside(ff$p, L95s)), outside_998_adjusted_unwinsorised_tau2 = sum(outside(ff$p, L998s)),
  outside_998_adjusted_over_1000 = sum(fun$outside_998_adjusted & fun$over_1000_claims),
  stringsAsFactors = FALSE)
wcsv(fsum, "desc_funnel_summary.csv")
ngrid <- exp(seq(log(min(n_i) * 0.8), log(max(n_i) * 1.25), length.out = 200))
G95u <- funnel_limits(ff$p0, ngrid, 0.95, 0); G998u <- funnel_limits(ff$p0, ngrid, 0.998, 0)
G95a <- funnel_limits(ff$p0, ngrid, 0.95, ff$tau2); G998a <- funnel_limits(ff$p0, ngrid, 0.998, ff$tau2)
grid <- data.frame(received = ngrid, pooled_rate = ff$p0,
  limit_95_lower_unadjusted = G95u$lower, limit_95_upper_unadjusted = G95u$upper,
  limit_998_lower_unadjusted = G998u$lower, limit_998_upper_unadjusted = G998u$upper,
  limit_95_lower_adjusted = G95a$lower, limit_95_upper_adjusted = G95a$upper,
  limit_998_lower_adjusted = G998a$lower, limit_998_upper_adjusted = G998a$upper)
wcsv(grid, "desc_funnel_limits_grid.csv")
lg("funnel: ", ff$I, " issuers; pooled rate ", sprintf("%.5f", ff$p0), "; tau2 ", sprintf("%.6g", ff$tau2),
   " (tau ", sprintf("%.4f", sqrt(ff$tau2)), "); phi ", sprintf("%.1f", ff$phi),
   "; outside 99.8 pct unadjusted ", fsum$outside_998_unadjusted, ", adjusted ", fsum$outside_998_adjusted,
   "; outside 95 pct unadjusted ", fsum$outside_95_unadjusted, ", adjusted ", fsum$outside_95_adjusted)

fm <- c(
  "FUNNEL PLOT METHOD (written by 05_descriptives.R; numbers are from this run)",
  "",
  "Population: Individual QHP issuers (2024 claims, PY2026 Transparency PUF) with a reported numeric in-network claims received count AND a reported numeric in-network claims denied count.",
  sprintf("Issuers in file %d; excluded: %d with N/A issuer-level claims (new to the Exchange) and %d with a suppressed (**) in-network denied count; plotted: %d (%d of them with more than 1,000 in-network claims).",
          nrow(iss), n_na, n_denied_supp, ff$I, sum(n_i > 1000)),
  "No imputation: an excluded issuer has no value. Each issuer is in exactly one state, so issuer-state equals issuer.",
  "",
  "Notation: n_i = in-network claim lines received, y_i = denied, p_i = y_i / n_i, I = number of issuers.",
  "  p0   = sum(y) / sum(n)                       pooled rate (target and centre line)",
  "  se_i = sqrt(p0 (1 - p0) / n_i)               column se_binomial",
  "  z_i  = (p_i - p0) / se_i                     column z",
  "  zw_i = min(max(z_i, q10), q90)               column z_winsorised; q10 and q90 are the 10th and 90th percentiles of z (stats::quantile type 7)",
  "  w_i  = 1 / se_i^2 ;  Q = sum(zw_i^2) ;  phi = Q / I                        multiplicative factor",
  "  tau2 = max(0, (Q - (I - 1)) / (sum(w) - sum(w^2) / sum(w)))                 additive overdispersion (DerSimonian-Laird form)",
  "Limits at level L with zc = qnorm(1 - (1 - L) / 2):",
  "  without overdispersion term: p0 +/- zc * se_i",
  "  with additive term:          p0 +/- zc * sqrt(se_i^2 + tau2)",
  sprintf("  zc = %.4f (95 percent) and %.4f (99.8 percent). Outside means strictly below the lower or above the upper limit. Limits are unclipped.", zc95, zc998),
  "",
  "Provenance: a Spiegelhalter-style funnel plot (candidate references spiegelhalter2005 and spiegelhalter2005qshc, both verified at abstract level only).",
  "The 95 and 99.8 percent levels, the 10 percent Winsorisation and the exact formulas above are analyst choices and must not be attributed to those papers.",
  "",
  "RESULTS",
  sprintf("  pooled rate p0 = %.5f ; issuer rates range %.4f to %.4f (median %.4f)", ff$p0, min(ff$p), max(ff$p), q[2]),
  sprintf("  Winsorising bounds on z: %.2f to %.2f ; Q = %.1f ; phi (multiplicative factor) = %.1f", ff$q[1], ff$q[2], ff$Q, ff$phi),
  sprintf("  tau2 (additive) = %.6g, tau = %.4f on the proportion scale (%.2f percentage points)", ff$tau2, sqrt(ff$tau2), 100 * sqrt(ff$tau2)),
  sprintf("  outside the 95 percent limits: %d without, %d with the additive term (multiplicative: %d)", fsum$outside_95_unadjusted, fsum$outside_95_adjusted, fsum$outside_95_multiplicative),
  sprintf("  outside the 99.8 percent limits: %d without, %d with the additive term (%d above, %d below; multiplicative: %d)", fsum$outside_998_unadjusted, fsum$outside_998_adjusted, fsum$above_998_adjusted, fsum$below_998_adjusted, fsum$outside_998_multiplicative),
  "",
  "SENSITIVITY (not the primary result)",
  sprintf("  Without Winsorising, tau2 = %.6g (tau = %.4f) and phi = %.1f; outside the additive limits: %d at 95 percent, %d at 99.8 percent.",
          fu$tau2, sqrt(fu$tau2), fu$phi, fsum$outside_95_adjusted_unwinsorised_tau2, fsum$outside_998_adjusted_unwinsorised_tau2),
  "  Winsorising clips the largest z (the biggest issuers) but the weights are not clipped, so the Winsorised tau2 is the smaller one and flags more issuers.",
  "  The additive term is on the proportion scale and is one common value for all issuers; issuers with very large volumes dominate the weights.",
  "")
writeLines(fm, file.path(OUT, "desc_funnel_method.txt"))
lg("wrote desc_funnel_method.txt")

# ======================================================================
# 3. STATE RATES (Fig 2b)
# ======================================================================
st <- sort(unique(iss$state))
sr <- do.call(rbind, lapply(st, function(s) {
  a <- fun[fun$state == s, , drop = FALSE]
  data.frame(state = s, n_issuers_in_puf = sum(iss$state == s), n_issuers = nrow(a),
             n_issuers_excluded = sum(iss$state == s) - nrow(a),
             n_claims = sum(a$received), n_denied = sum(a$denied),
             pooled_denial_rate = if (nrow(a)) sum(a$denied) / sum(a$received) else NA_real_,
             mean_issuer_rate = if (nrow(a)) mean(a$rate) else NA_real_,
             min_issuer_rate = if (nrow(a)) min(a$rate) else NA_real_,
             max_issuer_rate = if (nrow(a)) max(a$rate) else NA_real_,
             stringsAsFactors = FALSE)
}))
if (anyNA(sr$pooled_denial_rate)) stop("a state has no issuer with both in-network counts")
wcsv(sr, "desc_state_rates.csv")

# ======================================================================
# 4. COHORT FLOW AND PLAN CHARACTERISTICS (computable plans)
# ======================================================================
pr <- "plan_claims_received_in_network"; pd <- "plan_claims_denied_in_network"
mrp <- pl[[paste0(pr, "_marker")]]; mdp <- pl[[paste0(pd, "_marker")]]
rv <- cells(pl, pr); dv <- cells(pl, pd)
cls <- ifelse(mrp == "na", "na_plan_level",
       ifelse(mrp == "star2", "received_suppressed",
       ifelse(mrp != "numeric", "received_other_missing",
       ifelse(rv == 0, "received_zero",
       ifelse(mdp != "numeric", "denied_not_reported",
       ifelse(dv > rv, "denied_exceeds_received", "computable"))))))
if (any(cls %in% c("received_other_missing", "denied_exceeds_received"))) stop("unexpected plan class: ", paste(unique(cls), collapse = ", "))
if (any(cls == "denied_not_reported" & mdp != "star2")) stop("a denied marker other than ** among plans with a positive received count")
comp <- pl[cls == "computable", , drop = FALSE]
if (nrow(comp) != sum(!is.na(pl$plan_in_network_denial_rate)) || !all(comp$plan_id %in% pl$plan_id[!is.na(pl$plan_in_network_denial_rate)])) {
  stop("computable set differs from the parsed plan_in_network_denial_rate")
}
if (sum(cls != "na_plan_level") != sum(pl$plan_level_reported %in% TRUE)) stop("plan-level class differs from plan_level_reported")
cnt <- table(factor(cls, levels = c("na_plan_level", "received_suppressed", "received_zero", "denied_not_reported", "computable")))
n_iss_of <- function(d) length(unique(d$issuer_id))
flow <- data.frame(
  step_id = c("all_plan_rows", "plan_level_na", "received_suppressed", "received_zero", "denied_suppressed", "computable"),
  description = c("Plan rows in the Individual QHP tab",
                  "Excluded: plan-level in-network claims are N/A (plan or issuer new to the Exchange)",
                  "Excluded: in-network claims received suppressed (**)",
                  "Excluded: in-network claims received equal to 0",
                  "Excluded: in-network claims denied suppressed (**) with received above 0",
                  "Plans with a computable in-network denial proportion"),
  n_plans_excluded_at_step = c(0L, cnt[["na_plan_level"]], cnt[["received_suppressed"]], cnt[["received_zero"]], cnt[["denied_not_reported"]], 0L),
  n_plans_remaining = c(nrow(pl), nrow(pl) - cnt[["na_plan_level"]],
                        nrow(pl) - cnt[["na_plan_level"]] - cnt[["received_suppressed"]],
                        nrow(pl) - cnt[["na_plan_level"]] - cnt[["received_suppressed"]] - cnt[["received_zero"]],
                        nrow(comp), nrow(comp)),
  n_issuers_remaining = c(n_iss_of(pl), n_iss_of(pl[cls != "na_plan_level", ]),
                          n_iss_of(pl[!(cls %in% c("na_plan_level", "received_suppressed")), ]),
                          n_iss_of(pl[!(cls %in% c("na_plan_level", "received_suppressed", "received_zero")), ]),
                          n_iss_of(comp), n_iss_of(comp)),
  stringsAsFactors = FALSE)
flow$n_states_remaining <- c(length(unique(pl$state)), length(unique(pl$state[cls != "na_plan_level"])),
                             length(unique(pl$state[!(cls %in% c("na_plan_level", "received_suppressed"))])),
                             length(unique(pl$state[!(cls %in% c("na_plan_level", "received_suppressed", "received_zero"))])),
                             length(unique(comp$state)), length(unique(comp$state)))
flow$in_network_claims_received_remaining <- c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, sum(as.numeric(comp[[pr]])))
wcsv(flow, "desc_cohort_flow.csv")
if (flow$n_plans_remaining[5] != nrow(comp)) stop("cohort flow does not close")

ENR <- c(numeric = "Reported (numeric)", star2 = "Suppressed (**)", star1 = "Not available (*)", na = "N/A")
ENRID <- c(numeric = "numeric", star2 = "suppressed_star2", star1 = "unavailable_star1", na = "not_applicable")
pc_row <- function(dimension, level, label, d) {
  rcv <- sum(as.numeric(d[[pr]])); dnd <- sum(as.numeric(d[[pd]]))
  data.frame(dimension = dimension, level = level, level_label = label, n_computable_plans = nrow(d),
             share_of_computable_plans = nrow(d) / nrow(comp), n_issuers = n_iss_of(d), n_states = length(unique(d$state)),
             in_network_claims_received = rcv, in_network_claims_denied = dnd,
             pooled_in_network_denial_rate = if (nrow(d) && rcv > 0) dnd / rcv else NA_real_,
             n_plans_enrollment_numeric = sum(d$avg_monthly_enrollment_marker == "numeric"),
             stringsAsFactors = FALSE)
}
ptypes <- c("EPO", "HMO", "POS", "PPO"); metals <- c("Bronze", "Silver", "Gold", "Platinum", "Catastrophic")
if (!setequal(unique(comp$plan_type), ptypes) || !setequal(unique(comp$metal_level), metals)) stop("unexpected plan type or metal level values")
pcs <- rbind(
  pc_row("all", "all", "All computable plans", comp),
  do.call(rbind, lapply(ptypes, function(x) pc_row("plan_type", x, x, comp[comp$plan_type == x, , drop = FALSE]))),
  do.call(rbind, lapply(metals, function(x) pc_row("metal_level", x, x, comp[comp$metal_level == x, , drop = FALSE]))),
  do.call(rbind, lapply(sort(unique(comp$state)), function(x) pc_row("state", x, x, comp[comp$state == x, , drop = FALSE]))),
  do.call(rbind, lapply(names(ENR), function(k) pc_row("enrollment_availability", ENRID[[k]], ENR[[k]], comp[comp$avg_monthly_enrollment_marker == k, , drop = FALSE]))))
rownames(pcs) <- NULL
wcsv(pcs, "desc_plan_characteristics.csv")

# ======================================================================
# 5. REASON MIX (ten denial-reason tallies, computable plans)
# ======================================================================
rmap <- utils::read.csv(file.path(P$config, "denial_reason_map.csv"), stringsAsFactors = FALSE)
RV <- rmap$variable; RL <- rmap$label
if (length(RV) != 10L || !all(RV %in% names(comp))) stop("denial_reason_map.csv does not list the ten plan_reason_ columns")
OON_REASON <- "plan_reason_out_of_network_provider"
strata <- rbind(data.frame(stratum_type = "all", stratum = "All plans", stringsAsFactors = FALSE),
                data.frame(stratum_type = "plan_type", stratum = ptypes, stringsAsFactors = FALSE),
                data.frame(stratum_type = "metal_level", stratum = metals, stringsAsFactors = FALSE))
sub_of <- function(st_type, st) switch(st_type, all = comp, plan_type = comp[comp$plan_type == st, , drop = FALSE],
                                       metal_level = comp[comp$metal_level == st, , drop = FALSE])
rm_long <- do.call(rbind, lapply(seq_len(nrow(strata)), function(i) {
  d <- sub_of(strata$stratum_type[i], strata$stratum[i])
  tl <- vapply(RV, function(v) sum(cells(d, v), na.rm = TRUE), numeric(1))
  ten <- sum(tl); nine <- sum(tl[RV != OON_REASON])
  do.call(rbind, lapply(seq_along(RV), function(j) {
    m <- mk(d, RV[j])
    data.frame(stratum_type = strata$stratum_type[i], stratum = strata$stratum[i], reason = RV[j], label = RL[j],
               n_plans = nrow(d), n_cells_reported = m[["numeric"]], n_cells_suppressed = m[["star2"]],
               n_cells_other_missing = m[["star1"]] + m[["star3"]] + m[["na"]],
               tally_sum = tl[[j]], ten_reason_total = ten, share_of_ten_reason_total = tl[[j]] / ten,
               nine_reason_total = nine,
               share_of_nine_reason_total = if (RV[j] == OON_REASON) NA_real_ else tl[[j]] / nine,
               is_other = RV[j] == "plan_reason_other", stringsAsFactors = FALSE)
  }))
}))
rownames(rm_long) <- NULL
wcsv(rm_long, "desc_reason_mix.csv")
sid <- sub("^plan_reason_", "", RV)
rm_wide <- do.call(rbind, lapply(seq_len(nrow(strata)), function(i) {
  a <- rm_long[rm_long$stratum_type == strata$stratum_type[i] & rm_long$stratum == strata$stratum[i], , drop = FALSE]
  a <- a[match(RV, a$reason), ]
  w <- data.frame(stratum_type = strata$stratum_type[i], stratum = strata$stratum[i], n_plans = a$n_plans[1],
                  ten_reason_total = a$ten_reason_total[1], stringsAsFactors = FALSE)
  for (j in seq_along(RV)) w[[paste0("share_", sid[j])]] <- a$share_of_ten_reason_total[j]
  w$share_other_of_nine <- a$share_of_nine_reason_total[a$reason == "plan_reason_other"]
  for (j in seq_along(RV)) w[[paste0("tally_", sid[j])]] <- a$tally_sum[j]
  for (j in seq_along(RV)) w[[paste0("suppressed_", sid[j])]] <- a$n_cells_suppressed[j]
  w
}))
wcsv(rm_wide, "desc_reason_mix_wide.csv")

# ======================================================================
# 6. VARIANCE PARTITION (null three-level binomial GLMM)
# ======================================================================
vdat <- data.frame(denied = as.numeric(comp[[pd]]), failed = as.numeric(comp[[pr]]) - as.numeric(comp[[pd]]),
                   state = factor(comp$state), issuer = factor(paste(comp$state, comp$issuer_id, sep = "_")),
                   plan = factor(comp$plan_id), stringsAsFactors = FALSE)
if (any(vdat$failed < 0)) stop("denied exceeds received in the variance-partition data")
if (nlevels(vdat$issuer) != length(unique(comp$issuer_id))) stop("an issuer appears in more than one state")
lg("variance partition: fitting on ", nrow(vdat), " plans, ", nlevels(vdat$issuer), " issuers, ", nlevels(vdat$state), " states")
vf <- vp_fit(vdat)
fit <- vf$fit
for (s in vf$log) lg("fit attempt ", s)
vv <- vp_variances(fit)
tab <- vpc_table(vv[["state"]], vv[["issuer"]], vv[["plan"]])
singular <- lme4::isSingular(fit, tol = 1e-4)
grad <- fit@optinfo$derivs$gradient
alt <- vp_fit_one(vdat, if (vf$attempt_index == 1L) VP_ATTEMPTS[[3]] else VP_ATTEMPTS[[1]])
alt_diff <- if (!is.null(alt$fit)) max(abs(vp_variances(alt$fit) - vv)) else NA_real_
lg("convergence cross-check (", if (vf$attempt_index == 1L) VP_ATTEMPTS[[3]]$name else VP_ATTEMPTS[[1]]$name,
   "): clean=", alt$ok, ", max abs difference in the three variances = ", signif(alt_diff, 3))
nsim <- 500L
lg("parametric bootstrap, nsim = ", nsim, " (about 2 to 3 minutes)")
boot <- lme4::bootMer(fit, vp_variances, nsim = nsim, seed = SEED, use.u = FALSE, type = "parametric")
bt <- boot$t; colnames(bt) <- c("state", "issuer", "plan")
bok <- stats::complete.cases(bt) & is.finite(rowSums(bt))
bt <- bt[bok, , drop = FALSE]
lg("bootstrap: ", nrow(bt), " of ", nsim, " replicates usable")
bvpc <- lapply(seq_len(nrow(bt)), function(r) vpc_table(bt[r, 1], bt[r, 2], bt[r, 3]))
pick <- function(col, i) vapply(bvpc, function(x) x[[col]][i], numeric(1))
ci <- function(x) stats::quantile(x, c(0.025, 0.975), type = 7, names = FALSE)
comp_key <- c("state", "issuer", "plan")
tab$variance_ci_lower <- tab$variance_ci_upper <- NA_real_
tab$vpc_latent_ci_lower <- tab$vpc_latent_ci_upper <- NA_real_
tab$vpc_among_levels_ci_lower <- tab$vpc_among_levels_ci_upper <- NA_real_
tab$median_odds_ratio_ci_lower <- tab$median_odds_ratio_ci_upper <- NA_real_
for (i in 1:3) {
  a <- ci(bt[, comp_key[i]]); tab$variance_ci_lower[i] <- a[1]; tab$variance_ci_upper[i] <- a[2]
  a <- ci(pick("vpc_latent", i)); tab$vpc_latent_ci_lower[i] <- a[1]; tab$vpc_latent_ci_upper[i] <- a[2]
  a <- ci(pick("vpc_among_levels", i)); tab$vpc_among_levels_ci_lower[i] <- a[1]; tab$vpc_among_levels_ci_upper[i] <- a[2]
  a <- ci(pick("median_odds_ratio", i)); tab$median_odds_ratio_ci_lower[i] <- a[1]; tab$median_odds_ratio_ci_upper[i] <- a[2]
}
a <- ci(pick("vpc_latent", 4)); tab$vpc_latent_ci_lower[4] <- a[1]; tab$vpc_latent_ci_upper[4] <- a[2]
tab$label <- c("State", "Issuer within state", "Plan (observation level)", "Claim line (latent residual, pi^2/3)")
tab$sd <- sqrt(tab$variance)
b0 <- as.numeric(lme4::fixef(fit)[1])
tab$n_plans <- nrow(vdat); tab$n_issuers <- nlevels(vdat$issuer); tab$n_states <- nlevels(vdat$state)
tab$n_agq <- vf$attempt$nAGQ; tab$optimizer <- vf$attempt$optimizer; tab$attempt_used <- vf$attempt$name
tab$converged_clean <- TRUE; tab$singular <- singular
tab$max_abs_gradient <- max(abs(grad)); tab$log_likelihood <- as.numeric(stats::logLik(fit)); tab$aic <- stats::AIC(fit)
tab$intercept_logit <- b0; tab$intercept_probability <- stats::plogis(b0)
tab$max_abs_variance_diff_alt_fit <- alt_diff
tab$bootstrap_nsim <- nsim; tab$bootstrap_usable <- nrow(bt)
tab$plans_excluded_denied_suppressed <- cnt[["denied_not_reported"]]
tab$plans_excluded_received_suppressed <- cnt[["received_suppressed"]]
tab$plans_excluded_received_zero <- cnt[["received_zero"]]
tab <- tab[, c("component", "label", "variance", "sd", "variance_ci_lower", "variance_ci_upper",
               "vpc_latent", "vpc_latent_ci_lower", "vpc_latent_ci_upper",
               "vpc_among_levels", "vpc_among_levels_ci_lower", "vpc_among_levels_ci_upper",
               "median_odds_ratio", "median_odds_ratio_ci_lower", "median_odds_ratio_ci_upper",
               "n_plans", "n_issuers", "n_states", "n_agq", "optimizer", "attempt_used", "converged_clean", "singular",
               "max_abs_gradient", "log_likelihood", "aic", "intercept_logit", "intercept_probability",
               "max_abs_variance_diff_alt_fit", "bootstrap_nsim", "bootstrap_usable",
               "plans_excluded_denied_suppressed", "plans_excluded_received_suppressed", "plans_excluded_received_zero")]
wcsv(tab, "desc_variance_partition.csv")
lg("variance components: state ", signif(vv[["state"]], 4), ", issuer ", signif(vv[["issuer"]], 4), ", plan ", signif(vv[["plan"]], 4),
   "; singular=", singular, "; among-levels VPC state/issuer/plan = ",
   paste(sprintf("%.3f", tab$vpc_among_levels[1:3]), collapse = " / "))

# ======================================================================
# 7. LEDGER (every value is read back from the CSV it came from)
# ======================================================================
ledger_reset(PART)
RB <- new.env()
rb <- function(file) {
  if (is.null(RB[[file]])) RB[[file]] <- read_back_csv(file)
  RB[[file]]
}
read_back_csv <- function(file) {
  path <- file.path(OUT, file)
  hdr <- names(utils::read.csv(path, nrows = 1, check.names = FALSE))
  cc <- c(issuer_id = "character", plan_id = "character", state = "character", level = "character",
          stratum = "character", ids = "character")
  cc <- cc[names(cc) %in% hdr]
  utils::read.csv(path, colClasses = cc, na.strings = c("", "NA"), stringsAsFactors = FALSE, check.names = FALSE)
}
SRC <- data.frame(key = character(0), csv = character(0), kind = character(0), where = character(0), column = character(0), stringsAsFactors = FALSE)
KEYS <- character(0)
note_src <- function(key, file, kind, where, col) {
  SRC[nrow(SRC) + 1L, ] <<- list(key, file, kind, where, col)
  KEYS <<- c(KEYS, key)
}
# lv(): one ledger key whose value is a single cell of a CSV read back from disk
lv <- function(key, file, col, where = NULL, unit = "", desc = "") {
  d <- rb(file)
  sel <- rep(TRUE, nrow(d))
  for (nm in names(where)) sel <- sel & as.character(d[[nm]]) == as.character(where[[nm]])
  if (sum(sel) != 1L) stop("lv(): ", key, " matched ", sum(sel), " rows in ", file)
  v <- d[[col]][sel]
  if (is.na(v)) stop("lv(): NA value for ", key)
  if (is.logical(v)) v <- as.integer(v)
  ledger_add(PART, key, v, unit, desc, SCRIPT)
  note_src(key, file, "cell", paste(names(where), unlist(where), sep = "=", collapse = ";"), col)
  invisible(NULL)
}
# la(): one ledger key that is a simple aggregate of a read-back CSV (rule is named in the sources map)
la <- function(key, file, rule, column, value, unit, desc) {
  if (length(value) != 1L || is.na(value)) stop("la(): bad value for ", key)
  ledger_add(PART, key, value, unit, desc, SCRIPT)
  note_src(key, file, "aggregate", rule, column)
}
chk_new <- function() {
  ks <- KEYS
  if (any(ks %in% kff$key)) stop("ledger keys collide with ledger_kff.csv: ", paste(ks[ks %in% kff$key], collapse = ", "))
}

# attrition: combined totals (the in/out/not_split totals already exist as att_ keys in ledger_kff.csv)
for (ms in c("claims_received", "claims_denied", "claims_resubmitted")) {
  lv(paste0("att_", ms, "_combined"), "desc_attrition.csv", "total", list(measure = ms, network = "combined"), "claim lines",
     paste0("2024 national total, in-network plus out-of-network (", sub("_", " ", ms), "), sum of reported issuer-level cells; lower bound"))
  lv(paste0("att_", ms, "_combined_issuers_reported"), "desc_attrition.csv", "issuers_reported", list(measure = ms, network = "combined"), "issuers",
     paste0("Issuers with both the in-network and out-of-network cell reported (", sub("_", " ", ms), ")"))
}
for (i in seq_len(nrow(ratios))) {
  id <- ratios$ratio_id[i]
  lv(paste0("att_ratio_", id), "desc_attrition_ratios.csv", "ratio", list(ratio_id = id), ratios$scale_label[i],
     paste0(ratios$numerator[i], " over ", tolower(substr(ratios$denominator[i], 1, 1)), substr(ratios$denominator[i], 2, nchar(ratios$denominator[i])),
            ", matched issuer set (issuers reporting every cell used)", if (ratios$n_issuers_excluded_anomalous[i] > 0) "; the 2 anomalous out-of-network issuers left out" else ""))
  lv(paste0("att_ratio_", id, "_issuers"), "desc_attrition_ratios.csv", "n_issuers_matched", list(ratio_id = id), "issuers",
     paste0("Matched issuers behind ratio ", id))
}
lv("att_oon_anomalous_issuers", "desc_oon_exclusions.csv", "n_excluded", list(scope = "issuers"), "issuers",
   "Issuers with out-of-network denied above received, left out of out-of-network and combined rates")
lv("att_oon_anomalous_plans", "desc_oon_exclusions.csv", "n_excluded", list(scope = "plans"), "plans",
   "Plans with out-of-network denied above received (flagged, no out-of-network plan rate used)")
lv("att_oon_anomalous_plan_issuers", "desc_oon_exclusions.csv", "n_excluded", list(scope = "issuers_holding_the_plans"), "issuers",
   "Issuers holding the anomalous out-of-network plans")

# funnel and state heterogeneity
FS <- function(key, col, unit, desc) lv(key, "desc_funnel_summary.csv", col, NULL, unit, desc)
FS("hetero_funnel_issuers", "n_issuers_funnel", "issuers", "Issuers with reported in-network received and denied counts (funnel plot)")
FS("hetero_funnel_issuers_na", "n_issuers_na", "issuers", "Issuers excluded from the funnel because issuer-level claims are N/A")
FS("hetero_funnel_issuers_denied_suppressed", "n_issuers_denied_suppressed", "issuers", "Issuers excluded from the funnel because the in-network denied count is suppressed")
FS("hetero_funnel_issuers_over_1000", "n_issuers_over_1000", "issuers", "Funnel issuers with more than 1,000 in-network claim lines")
FS("hetero_funnel_claims_received", "total_received", "claim lines", "In-network claim lines received by the funnel issuers")
FS("hetero_funnel_claims_denied", "total_denied", "claim lines", "In-network claim lines denied by the funnel issuers")
FS("hetero_funnel_pooled_rate", "pooled_rate", "proportion", "Pooled in-network denial rate of the funnel issuers (sum denied over sum received)")
FS("hetero_issuer_rate_min", "issuer_rate_min", "proportion", "Lowest issuer in-network denial rate among funnel issuers")
FS("hetero_issuer_rate_p25", "issuer_rate_p25", "proportion", "25th percentile of issuer in-network denial rates (unweighted)")
FS("hetero_issuer_rate_median", "issuer_rate_median", "proportion", "Median issuer in-network denial rate (unweighted)")
FS("hetero_issuer_rate_p75", "issuer_rate_p75", "proportion", "75th percentile of issuer in-network denial rates (unweighted)")
FS("hetero_issuer_rate_max", "issuer_rate_max", "proportion", "Highest issuer in-network denial rate among funnel issuers")
FS("hetero_funnel_tau2", "tau2_winsorised", "proportion scale squared", "Additive overdispersion tau-squared from 10 percent Winsorised z-scores")
FS("hetero_funnel_tau", "tau_winsorised", "proportion", "Square root of the additive overdispersion tau-squared")
FS("hetero_funnel_phi", "phi_winsorised", "factor", "Multiplicative overdispersion factor from 10 percent Winsorised z-scores")
FS("hetero_funnel_tau2_unwinsorised", "tau2_unwinsorised", "proportion scale squared", "Additive tau-squared without Winsorising (sensitivity)")
FS("hetero_funnel_winsor_probability", "winsor_probability", "proportion", "Share of z-scores Winsorised in each tail")
FS("hetero_funnel_zcrit_95", "z_crit_95", "z", "Critical z for the 95 percent funnel limits")
FS("hetero_funnel_zcrit_998", "z_crit_998", "z", "Critical z for the 99.8 percent funnel limits")
FS("hetero_funnel_outside_95_unadjusted", "outside_95_unadjusted", "issuers", "Issuers outside the 95 percent limits without the overdispersion term")
FS("hetero_funnel_outside_998_unadjusted", "outside_998_unadjusted", "issuers", "Issuers outside the 99.8 percent limits without the overdispersion term")
FS("hetero_funnel_outside_95_adjusted", "outside_95_adjusted", "issuers", "Issuers outside the 95 percent limits with the additive overdispersion term")
FS("hetero_funnel_outside_998_adjusted", "outside_998_adjusted", "issuers", "Issuers outside the 99.8 percent limits with the additive overdispersion term")
FS("hetero_funnel_above_998_adjusted", "above_998_adjusted", "issuers", "Issuers above the adjusted 99.8 percent upper limit")
FS("hetero_funnel_below_998_adjusted", "below_998_adjusted", "issuers", "Issuers below the adjusted 99.8 percent lower limit")
FS("hetero_funnel_outside_95_multiplicative", "outside_95_multiplicative", "issuers", "Issuers outside the 95 percent limits with the multiplicative factor")
FS("hetero_funnel_outside_998_multiplicative", "outside_998_multiplicative", "issuers", "Issuers outside the 99.8 percent limits with the multiplicative factor")
FS("hetero_funnel_outside_998_adjusted_unwinsorised", "outside_998_adjusted_unwinsorised_tau2", "issuers", "Issuers outside the 99.8 percent limits using the unwinsorised tau-squared (sensitivity)")
FS("hetero_funnel_outside_95_adjusted_unwinsorised", "outside_95_adjusted_unwinsorised_tau2", "issuers", "Issuers outside the 95 percent limits using the unwinsorised tau-squared (sensitivity)")
FS("hetero_funnel_outside_998_adjusted_over_1000", "outside_998_adjusted_over_1000", "issuers", "Issuers with more than 1,000 claim lines outside the adjusted 99.8 percent limits")
srd <- rb("desc_state_rates.csv")
imin <- srd$state[which.min(srd$pooled_denial_rate)]; imax <- srd$state[which.max(srd$pooled_denial_rate)]
la("hetero_states", "desc_state_rates.csv", "rows", "state", nrow(srd), "states", "States with at least one funnel issuer (rows of desc_state_rates.csv)")
lv("hetero_state_pooled_rate_min", "desc_state_rates.csv", "pooled_denial_rate", list(state = imin), "proportion", "Lowest state pooled in-network denial rate")
lv("hetero_state_pooled_rate_max", "desc_state_rates.csv", "pooled_denial_rate", list(state = imax), "proportion", "Highest state pooled in-network denial rate")
la("hetero_state_lowest", "desc_state_rates.csv", "state at which.min(pooled_denial_rate)", "state", imin, "state code", "State with the lowest pooled in-network denial rate")
la("hetero_state_highest", "desc_state_rates.csv", "state at which.max(pooled_denial_rate)", "state", imax, "state code", "State with the highest pooled in-network denial rate")
la("hetero_states_single_issuer", "desc_state_rates.csv", "count of rows with n_issuers == 1", "n_issuers", sum(srd$n_issuers == 1L), "states", "States whose pooled rate rests on a single issuer")

# variance partition
VK <- c(state = "state", issuer = "issuer_within_state", plan = "plan", claim = "claim_residual")
VP <- function(key, col, comp_name, unit, desc) lv(key, "desc_variance_partition.csv", col, list(component = comp_name), unit, desc)
for (k in names(VK)) {
  cn <- VK[[k]]; nm <- if (k == "claim") "claim line (latent residual)" else k
  VP(paste0("hetero_vp_var_", k), "variance", cn, "latent logit variance", paste0("Variance component, ", nm))
  VP(paste0("hetero_vp_vpc_latent_", k), "vpc_latent", cn, "proportion", paste0("Latent-scale variance partition coefficient, ", nm, " (includes the claim-line residual pi^2/3)"))
  VP(paste0("hetero_vp_vpc_latent_", k, "_ci_lower"), "vpc_latent_ci_lower", cn, "proportion", paste0("Lower 95 percent bootstrap limit, latent VPC, ", nm))
  VP(paste0("hetero_vp_vpc_latent_", k, "_ci_upper"), "vpc_latent_ci_upper", cn, "proportion", paste0("Upper 95 percent bootstrap limit, latent VPC, ", nm))
  if (k != "claim") {
    VP(paste0("hetero_vp_sd_", k), "sd", cn, "logit scale", paste0("Standard deviation of the random intercept, ", nm))
    VP(paste0("hetero_vp_var_", k, "_ci_lower"), "variance_ci_lower", cn, "latent logit variance", paste0("Lower 95 percent bootstrap limit, variance, ", nm))
    VP(paste0("hetero_vp_var_", k, "_ci_upper"), "variance_ci_upper", cn, "latent logit variance", paste0("Upper 95 percent bootstrap limit, variance, ", nm))
    VP(paste0("hetero_vp_vpc_among_", k), "vpc_among_levels", cn, "proportion", paste0("Share of the between-claim-group variance (state, issuer, plan) at this level, ", nm))
    VP(paste0("hetero_vp_vpc_among_", k, "_ci_lower"), "vpc_among_levels_ci_lower", cn, "proportion", paste0("Lower 95 percent bootstrap limit, among-levels share, ", nm))
    VP(paste0("hetero_vp_vpc_among_", k, "_ci_upper"), "vpc_among_levels_ci_upper", cn, "proportion", paste0("Upper 95 percent bootstrap limit, among-levels share, ", nm))
    VP(paste0("hetero_vp_mor_", k), "median_odds_ratio", cn, "odds ratio", paste0("Median odds ratio, ", nm))
    VP(paste0("hetero_vp_mor_", k, "_ci_lower"), "median_odds_ratio_ci_lower", cn, "odds ratio", paste0("Lower 95 percent bootstrap limit, median odds ratio, ", nm))
    VP(paste0("hetero_vp_mor_", k, "_ci_upper"), "median_odds_ratio_ci_upper", cn, "odds ratio", paste0("Upper 95 percent bootstrap limit, median odds ratio, ", nm))
  }
}
VP("hetero_vp_plans", "n_plans", "state", "plans", "Plans in the variance partition model (computable in-network proportion)")
VP("hetero_vp_issuers", "n_issuers", "state", "issuers", "Issuers in the variance partition model")
VP("hetero_vp_states", "n_states", "state", "states", "States in the variance partition model")
VP("hetero_vp_nagq", "n_agq", "state", "count", "Adaptive Gauss-Hermite points used (1 = Laplace, 0 = fixed-effects PIRLS step)")
VP("hetero_vp_converged_clean", "converged_clean", "state", "0 or 1", "1 if the model converged with no optimizer or lme4 message and no warning")
VP("hetero_vp_singular", "singular", "state", "0 or 1", "1 if lme4 reports a singular fit (a variance at the boundary)")
VP("hetero_vp_intercept_logit", "intercept_logit", "state", "logit", "Fixed intercept of the null model")
VP("hetero_vp_intercept_probability", "intercept_probability", "state", "proportion", "Inverse logit of the fixed intercept")
VP("hetero_vp_excluded_denied_suppressed", "plans_excluded_denied_suppressed", "state", "plans", "Plans excluded because the in-network denied count is suppressed (received above 0)")
VP("hetero_vp_excluded_received_suppressed", "plans_excluded_received_suppressed", "state", "plans", "Plans excluded because the in-network received count is suppressed")
VP("hetero_vp_excluded_received_zero", "plans_excluded_received_zero", "state", "plans", "Plans excluded because the in-network received count is 0")
VP("hetero_vp_bootstrap_nsim", "bootstrap_nsim", "state", "replicates", "Parametric bootstrap replicates requested")
VP("hetero_vp_bootstrap_usable", "bootstrap_usable", "state", "replicates", "Parametric bootstrap replicates usable")

# cohort flow and plan characteristics (cohort_all_)
CF <- function(key, step, col, unit, desc) lv(key, "desc_cohort_flow.csv", col, list(step_id = step), unit, desc)
CF("cohort_all_plan_rows", "all_plan_rows", "n_plans_remaining", "plans", "Plan rows in the Individual QHP tab")
CF("cohort_all_issuers_in_puf", "all_plan_rows", "n_issuers_remaining", "issuers", "Issuers in the Individual QHP tab")
CF("cohort_all_plans_na_plan_level", "plan_level_na", "n_plans_excluded_at_step", "plans", "Plans whose plan-level in-network claims are N/A")
CF("cohort_all_plans_plan_level_data", "plan_level_na", "n_plans_remaining", "plans", "Plans with plan-level in-network claims reported (numeric or suppressed)")
CF("cohort_all_issuers_plan_level_data", "plan_level_na", "n_issuers_remaining", "issuers", "Issuers holding at least one plan with plan-level data")
CF("cohort_all_plans_excluded_received_suppressed", "received_suppressed", "n_plans_excluded_at_step", "plans", "Plans excluded: in-network received count suppressed (**)")
CF("cohort_all_plans_excluded_received_zero", "received_zero", "n_plans_excluded_at_step", "plans", "Plans excluded: in-network received count equal to 0")
CF("cohort_all_plans_excluded_denied_suppressed", "denied_suppressed", "n_plans_excluded_at_step", "plans", "Plans excluded: in-network denied count suppressed (**) with received above 0")
CF("cohort_all_plans_computable", "computable", "n_plans_remaining", "plans", "Plans with a computable in-network denial proportion")
CF("cohort_all_issuers_computable", "computable", "n_issuers_remaining", "issuers", "Issuers holding at least one computable plan")
CF("cohort_all_states_computable", "computable", "n_states_remaining", "states", "States with at least one computable plan")
CF("cohort_all_claims_received_computable", "computable", "in_network_claims_received_remaining", "claim lines", "In-network claim lines received by the computable plans")
PCK <- function(key, dim, lev, col, unit, desc) lv(key, "desc_plan_characteristics.csv", col, list(dimension = dim, level = lev), unit, desc)
for (x in ptypes) {
  PCK(paste0("cohort_all_plans_", tolower(x)), "plan_type", x, "n_computable_plans", "plans", paste0("Computable plans, plan type ", x))
  PCK(paste0("cohort_all_issuers_", tolower(x)), "plan_type", x, "n_issuers", "issuers", paste0("Issuers with at least one computable ", x, " plan"))
}
for (x in metals) {
  PCK(paste0("cohort_all_plans_", tolower(x)), "metal_level", x, "n_computable_plans", "plans", paste0("Computable plans, metal level ", x))
  PCK(paste0("cohort_all_issuers_", tolower(x)), "metal_level", x, "n_issuers", "issuers", paste0("Issuers with at least one computable ", x, " plan"))
}
for (k in names(ENR)) {
  PCK(paste0("cohort_all_plans_enrollment_", ENRID[[k]]), "enrollment_availability", ENRID[[k]], "n_computable_plans", "plans",
      paste0("Computable plans with average monthly enrollment: ", ENR[[k]]))
}
PCK("cohort_all_share_enrollment_numeric", "enrollment_availability", "numeric", "share_of_computable_plans", "proportion", "Share of computable plans with numeric average monthly enrollment")

# reason mix
RMK <- function(key, st_type, st, rs, col, unit, desc) lv(key, "desc_reason_mix.csv", col, list(stratum_type = st_type, stratum = st, reason = rs), unit, desc)
for (j in seq_along(RV)) {
  RMK(paste0("reason_share_", sid[j], "_all"), "all", "All plans", RV[j], "share_of_ten_reason_total", "proportion",
      paste0("Share of the ten-reason tally total, ", RL[j], ", all computable plans"))
  RMK(paste0("reason_suppressed_cells_", sid[j], "_all"), "all", "All plans", RV[j], "n_cells_suppressed", "plan cells",
      paste0("Plans with a suppressed (**) tally, ", RL[j], ", all computable plans"))
}
RMK("reason_ten_reason_total_all", "all", "All plans", "plan_reason_other", "ten_reason_total", "reason tallies", "Sum of the ten reported reason tallies (tallies, not unique claims), all computable plans")
RMK("reason_nine_reason_total_all", "all", "All plans", "plan_reason_other", "nine_reason_total", "reason tallies", "Sum of the nine reported tallies excluding out-of-network provider (KFF comparison basis)")
RMK("reason_plans", "all", "All plans", "plan_reason_other", "n_plans", "plans", "Computable plans behind the reason mix")
RMK("reason_share_other_of_nine_all", "all", "All plans", "plan_reason_other", "share_of_nine_reason_total", "proportion", "Other-reason share of the nine-reason total (excluding out-of-network provider), all computable plans")
for (x in ptypes) RMK(paste0("reason_share_other_", tolower(x)), "plan_type", x, "plan_reason_other", "share_of_ten_reason_total", "proportion", paste0("Other-reason share of the ten-reason total, plan type ", x))
for (x in metals) RMK(paste0("reason_share_other_", tolower(x)), "metal_level", x, "plan_reason_other", "share_of_ten_reason_total", "proportion", paste0("Other-reason share of the ten-reason total, metal level ", x))
rml <- rb("desc_reason_mix.csv")
la("reason_suppressed_cells_all", "desc_reason_mix.csv", "sum over stratum_type == all", "n_cells_suppressed", sum(rml$n_cells_suppressed[rml$stratum_type == "all"]), "plan cells", "Suppressed (**) reason cells over the ten reasons, all computable plans (cells, not plans)")
la("reason_cells_all", "desc_reason_mix.csv", "sum over stratum_type == all", "n_plans", sum(rml$n_plans[rml$stratum_type == "all"]), "plan cells", "Reason cells in the ten-reason table, all computable plans (plans times ten)")

chk_new()
ledger_write(PART)
utils::write.csv(SRC, file.path(P$logs, "wp6a_ledger_sources.csv"), row.names = FALSE)
lg("ledger_desc.csv written with ", nrow(SRC), " keys; sources map in logs/wp6a_ledger_sources.csv")
lg("=== 05_descriptives.R done ===")
}
