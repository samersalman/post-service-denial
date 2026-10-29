# =====================================================================
# 06_models.R : WP6b. Implements the FROZEN statistical analysis plan (v1/config/sap_frozen.md)
# =====================================================================
# Reads  : v1/data_clean/ortho/plan_exposure.csv (cohort of 2,527 plans, exposure counts)
#          v1/data_clean/cms/plans.csv, issuer_state.csv (outcome, enrollment, plan attributes)
#          v1/config/design_constants.csv, v1/logs/sap_hash.txt (frozen; never written here)
# Writes : v1/results/model_analytic_data.csv        one row per cohort plan with the analysis variables
#          v1/results/model_outcome_bound_plans.csv  plans added back in rows sens_i, sens_j, sens_n
#          v1/results/model_cohort_flow.csv          ladder with claim lines per rung, strata, selection check (SAP 2.6)
#          v1/results/model_table1_data.csv          analytic-sample characteristics by plan type and metal level
#          v1/results/model_table1_ladder.csv        the Table 1 sample ladder (overall rows of model_cohort_flow.csv, display order)
#          v1/results/model_estimates.csv            long format, primary first, then the SAP row codes
#          v1/results/model_sensitivity_rows.csv     Table 3 rows (primary + sens_a..sens_l) and the Supplement rows
#          v1/results/model_sens_leave_one_out.csv   the 33 leave-one-issuer-out refits (sens_s)
#          v1/results/model_fig3_curve.csv, model_fig3_bins.csv   Fig 3 data
#          v1/results/model_deviations.csv           departures from the SAP text (header only if none)
#          v1/results/ledger_parts/ledger_models.csv numbers ledger part "models"
#          v1/logs/wp6b_models.log
# Run    : cd v1 ; Rscript R/06_models.R     (about 1-2 minutes)
#
# Binding rules honoured here
#   - The first thing the script does is recompute the SHA-256 of the four frozen files and stop (BLOCKED) if any
#     differs from v1/logs/sap_hash.txt. It never writes to config/ or to the hash file.
#   - The PRIMARY model is fitted first (immediately after the analytic data are built and checked against the
#     design constants) and its estimate is logged before any other model is fitted. Nothing is changed afterwards.
#   - Only fallbacks that the SAP names (7.5: state fixed effects, then the mixed model; 8.1: nAGQ = 0) are used.
#     Every departure is recorded in model_deviations.csv (columns item, sap_text, what_was_done, reason).
#   - Out-of-network is not modelled. Plan claims received is never an exposure denominator (it is the binomial
#     denominator of the outcome only). NA exposure counts are never treated as zero.
# Seeds: design_seed (nothing here is random; set for completeness).
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
if (!exists("ledger_add", inherits = TRUE)) source(file.path(P$R, "ledger.R"))
suppressPackageStartupMessages(library(data.table))
ensure_pkgs(c("data.table", "sandwich", "lme4", "digest", "readr"))

LOGFILE <- getOption("psd_models_logfile", file.path(P$logs, "wp6b_models.log"))
lg <- function(...) log_msg(..., file = LOGFILE)
SRC <- "06_models.R"

# ---- frozen constants --------------------------------------------------------------------
read_design <- function(path = file.path(P$config, "design_constants.csv")) {
  d <- utils::read.csv(path, colClasses = "character", na.strings = NULL, stringsAsFactors = FALSE)
  stats::setNames(d$value, d$key)
}
DK <- read_design()
dnum <- function(k) {
  if (is.na(DK[k])) stop("design constant missing: ", k, call. = FALSE)
  as.numeric(DK[[k]])
}
DENS_PER <- dnum("design_density_per")
OFFSET_OPHTH <- dnum("design_zero_offset_ophth")
CI_LEVEL <- dnum("design_ci_level")
CRIT_Z <- stats::qnorm(1 - (1 - CI_LEVEL) / 2)
DF_T <- dnum("design_t_interval_df")
SEED <- dnum("design_seed")

ROW_LABELS <- c(
  primary = "Primary model (issuer fixed effects, adjusted for plan size)",
  sens_a = "Density without adjustment for plan size",
  sens_b = "State fixed effects instead of issuer fixed effects",
  sens_c = "Mixed model",
  sens_d = "Issuer with the largest share of identifying variation left out",
  sens_e = "Surgeon share of listed clinicians, all exposure-ok plans",
  sens_f = "APC-inclusive density",
  sens_g = "Negative control: dermatology density",
  sens_h = "Negative control: ophthalmology density",
  sens_i = "Suppressed outcome counts set to 0",
  sens_j = "Suppressed outcome counts set to 10",
  sens_k = "Suppressed enrollment set to 1",
  sens_l = "Suppressed enrollment set to 10",
  sens_m = "Issuer-level analysis",
  sens_n = "Suppressed outcome counts set to 5.5",
  sens_o = "Suppressed enrollment set to 5.5",
  sens_p = "Issuers with fewer than 10,000 in-network claim lines excluded",
  sens_q = "Catastrophic plans excluded",
  sens_r = "Issuer with the most plans left out",
  sens_s = "Leave-one-issuer-out over the informative issuers",
  sens_t = "Conservative t interval"
)
TABLE3_CODES <- strsplit(DK[["design_sens_row_codes"]], ";", fixed = TRUE)[[1]]
SUPP_CODES <- strsplit(DK[["design_supplement_row_codes"]], ";", fixed = TRUE)[[1]]
ALL_CODES <- c("primary", TABLE3_CODES, SUPP_CODES)

COVARS <- c("lenroll", "plan_type", "metal3")          # SAP 7.2 (besides the exposure and the fixed effects)
COVARS_NOENR <- c("plan_type", "metal3")               # sens_a, sens_e
PLAN_LEVELS <- c("HMO", "EPO", "POS", "PPO")           # reference HMO
METAL3_LEVELS <- c("Bronze", "Silver", "Gold")         # reference Bronze; Catastrophic -> Bronze, Platinum -> Gold

# ---- 0. frozen-file hash check -----------------------------------------------------------
check_sap_hash <- function(v1 = P$v1, hash_file = file.path(v1, "logs", "sap_hash.txt")) {
  hl <- readLines(hash_file, warn = FALSE)
  hl <- hl[grepl("^[0-9a-f]{64}  ", hl)]
  if (length(hl) != 4L) stop("BLOCKED: sap_hash.txt does not hold four hash lines", call. = FALSE)
  want <- sub("^[0-9a-f]{64}  ", "", hl)
  got <- unname(sha256_file(file.path(v1, want)))
  stored <- sub("^([0-9a-f]{64}).*$", "\\1", hl)
  bad <- want[got != stored]
  if (length(bad)) stop("BLOCKED: SAP hash mismatch for ", paste(bad, collapse = ", "), call. = FALSE)
  data.frame(file = want, sha256 = got, stringsAsFactors = FALSE)
}

# ---- decision rule (SAP 7.4) -------------------------------------------------------------
primary_model_rule <- function(k, threshold = dnum("design_informative_issuer_threshold")) {
  if (length(k) != 1L || is.na(k)) stop("K is missing", call. = FALSE)
  if (k >= threshold) "issuer_fe" else "state_fe"
}
count_informative <- function(a) {
  tab <- tapply(a$n_ortho_surgeon_text, a$issuer_id, function(x) c(length(x), length(unique(x))))
  sum(vapply(tab, function(z) z[1] >= 2L && z[2] >= 2L, logical(1)))
}
informative_ids <- function(a) {
  tab <- tapply(a$n_ortho_surgeon_text, a$issuer_id, function(x) c(length(x), length(unique(x))))
  sort(names(tab)[vapply(tab, function(z) z[1] >= 2L && z[2] >= 2L, logical(1))])
}

# ---- exposure variables (SAP 17.1) -------------------------------------------------------
add_exposures <- function(d, enrollment = d$enrollment) {
  d$lenroll <- log2(enrollment)
  d$expo_ortho <- log2(d$n_ortho_surgeon_text / enrollment * DENS_PER)
  d$expo_apc <- log2((d$n_ortho_surgeon_text + d$n_ortho_apc_text) / enrollment * DENS_PER)
  d$expo_derm <- log2(d$n_derm_text / enrollment * DENS_PER)
  d$expo_ophth <- log2(d$n_ophth_text / enrollment * DENS_PER + OFFSET_OPHTH)
  d$expo_share <- log2(d$n_ortho_surgeon_text / d$n_individual_npis)
  d
}

# ---- 1. build the analysis table --------------------------------------------------------
PLAN_SELECT <- c("plan_id", "issuer_id", "state", "plan_type", "metal_level",
                 "plan_claims_received_in_network", "plan_claims_received_in_network_marker",
                 "plan_claims_denied_in_network", "plan_claims_denied_in_network_marker",
                 "avg_monthly_enrollment", "avg_monthly_enrollment_marker", "pa_network_id")

num_if <- function(x, marker) ifelse(marker == "numeric", suppressWarnings(as.numeric(x)), NA_real_)

read_inputs <- function() {
  ex <- fread(file.path(P$clean_ortho, "plan_exposure.csv"),
              colClasses = list(character = c("plan_id", "issuer_id", "state", "exposure_status")),
              na.strings = c("", "NA"))
  pl <- fread(file.path(P$clean_cms, "plans.csv"), select = PLAN_SELECT, colClasses = "character",
              na.strings = c("", "NA"))
  iv <- fread(file.path(P$clean_cms, "issuer_state.csv"),
              select = c("issuer_id", "state", "issuer_claims_received_in_network", "issuer_claims_received_in_network_marker"),
              colClasses = "character", na.strings = c("", "NA"))
  list(ex = ex, pl = pl, iv = iv)
}

prep_plans <- function(pl) {
  pl[, `:=`(received = num_if(plan_claims_received_in_network, plan_claims_received_in_network_marker),
            denied = num_if(plan_claims_denied_in_network, plan_claims_denied_in_network_marker),
            enrollment_marker = avg_monthly_enrollment_marker,
            enrollment = num_if(avg_monthly_enrollment, avg_monthly_enrollment_marker))]
  pl[, metal3 := fcase(metal_level %in% c("Bronze", "Catastrophic"), "Bronze",
                       metal_level == "Silver", "Silver",
                       metal_level %in% c("Gold", "Platinum"), "Gold", default = NA_character_)]
  pl
}

build_cohort <- function(inp) {
  ex <- inp$ex; pl <- prep_plans(copy(inp$pl)); iv <- inp$iv
  stopifnot(nrow(ex) == uniqueN(ex$plan_id), nrow(pl) == uniqueN(pl$plan_id))
  m <- merge(ex, pl[, .(plan_id, issuer_id_p = issuer_id, state_p = state, plan_type, metal_level, metal3,
                        received, denied, enrollment_marker, enrollment, pa_network_id)],
             by = "plan_id", all.x = TRUE, sort = FALSE)
  if (nrow(m) != nrow(ex) || anyNA(m$issuer_id_p) || any(m$issuer_id != m$issuer_id_p) || any(m$state != m$state_p)) {
    stop("BLOCKED: cohort plans do not join one-to-one to plans.csv with matching issuer and state", call. = FALSE)
  }
  m[, c("issuer_id_p", "state_p") := NULL]
  if (anyNA(m$received) || anyNA(m$denied) || any(m$received <= 0) || any(m$denied > m$received) || any(m$denied < 0)) {
    stop("BLOCKED: a cohort plan lacks a computable in-network denial proportion", call. = FALSE)
  }
  if (anyNA(m$plan_type) || anyNA(m$metal3)) stop("BLOCKED: plan type or metal level missing for a cohort plan", call. = FALSE)
  iv[, issuer_in_network_claims := num_if(issuer_claims_received_in_network, issuer_claims_received_in_network_marker)]
  m <- merge(m, iv[, .(issuer_id, state, issuer_in_network_claims)], by = c("issuer_id", "state"), all.x = TRUE, sort = FALSE)
  m <- m[match(ex$plan_id, m$plan_id)]
  m[, state_issuer := paste(state, issuer_id, sep = "_")]
  m[, has_enroll := !is.na(enrollment) & enrollment > 0]
  m[, `:=`(in_exposure_ok = exposure_status == "ok")]
  m[, in_analytic := in_exposure_ok & has_enroll]
  m[, in_enrollment_star2_ok := in_exposure_ok & enrollment_marker == "star2"]
  setDF(m)
  m$plan_type <- factor(m$plan_type, levels = PLAN_LEVELS)
  m$metal3 <- factor(m$metal3, levels = METAL3_LEVELS)
  if (anyNA(m$plan_type) || anyNA(m$metal3)) stop("BLOCKED: unexpected plan type or metal level", call. = FALSE)
  # counts are kept only for exposure-ok plans: the other statuses are excluded from every exposure model (SAP 2.7), and the
  # four listed-without-individuals plans carry a count of 0 that must never be used as an exposure
  cnt_cols <- c("n_individual_npis", "n_ortho_surgeon_text", "n_ortho_apc_text", "n_derm_text", "n_ophth_text")
  for (cn in cnt_cols) m[[cn]][!m$in_exposure_ok] <- NA
  m <- add_exposures(m)
  # exposures exist only where the counts exist and enrollment is numeric
  bad <- !(m$in_analytic) & !is.na(m$expo_ortho) & is.na(m$enrollment)
  stopifnot(!any(bad))
  m$informative_issuer <- FALSE
  m$informative_issuer[m$in_analytic] <- m$issuer_id[m$in_analytic] %in% informative_ids(m[m$in_analytic, ])
  m
}

check_design <- function(D) {
  A <- D[D$in_analytic, ]
  chk <- list(
    cohort_plans = c(nrow(D), dnum("design_cohort_plans")),
    exposure_ok_plans = c(sum(D$in_exposure_ok), dnum("design_exposure_ok_plans")),
    exposure_ok_issuers = c(length(unique(D$issuer_id[D$in_exposure_ok])), dnum("design_exposure_ok_issuers")),
    analytic_plans = c(nrow(A), dnum("design_analytic_plans")),
    analytic_issuers = c(length(unique(A$issuer_id)), dnum("design_analytic_issuers")),
    analytic_states = c(length(unique(A$state)), dnum("design_analytic_states")),
    analytic_issuer_states = c(length(unique(A$state_issuer)), dnum("design_analytic_issuer_states")),
    ok_plans_enrollment_star2 = c(sum(D$in_enrollment_star2_ok), dnum("design_ok_plans_enrollment_star2")),
    informative_issuers_k = c(count_informative(A), dnum("design_informative_issuer_k")),
    issuers_ge2_plans = c(sum(table(A$issuer_id) >= 2L), dnum("design_issuers_ge2_plans")),
    informative_issuer_plans = c(sum(D$informative_issuer), dnum("design_informative_issuer_plans")),
    ophth_zero_analytic = c(sum(A$n_ophth_text == 0), dnum("design_zero_count_ophth_analytic")),
    largest_issuer_plans_share = c(max(table(A$issuer_id)) / nrow(A), dnum("design_largest_issuer_share"))
  )
  df <- data.frame(item = names(chk), computed = vapply(chk, `[`, numeric(1), 1), frozen = vapply(chk, `[`, numeric(1), 2),
                   stringsAsFactors = FALSE)
  df$match <- abs(df$computed - df$frozen) < 1e-9
  zero_ok <- min(A$n_ortho_surgeon_text) > 0 && min(A$n_derm_text) > 0 && min(D$n_ortho_surgeon_text[D$in_exposure_ok]) > 0
  list(table = df, zero_ok = zero_ok)
}

# ---- 2. model functions ------------------------------------------------------------------
fit_stats <- function(beta, se, method = "z", dfree = NA_real_) {
  crit <- if (method == "z") CRIT_Z else stats::qt(1 - (1 - CI_LEVEL) / 2, dfree)
  stat <- beta / se
  p <- if (method == "z") 2 * stats::pnorm(-abs(stat)) else 2 * stats::pt(-abs(stat), dfree)
  c(or = exp(beta), lo = exp(beta - crit * se), hi = exp(beta + crit * se), p = p)
}

# Quasi-binomial logit, grouped counts cbind(denied, received - denied), clustered HC1 covariance (SAP 7.1, 7.3).
# Aliased columns are removed and the model refitted BEFORE vcovCL (SAP 7.3); an aliased exposure = failure.
fit_qb <- function(d, expo, covars, fe, cluster, label = "") {
  d <- as.data.frame(d)
  out <- list(label = label, ok = FALSE, converged = FALSE, expo = expo, n_plans = nrow(d),
              n_issuers = length(unique(d$issuer_id)), n_clusters = length(unique(d[[cluster]])),
              removed = character(0), warnings = character(0), message = "", fo_class = "qb")
  need <- unique(c("denied", "received", expo, covars, fe, cluster))
  if (anyNA(d[, need, drop = FALSE])) { out$message <- "NA in model variables"; return(out) }
  if (any(d$denied > d$received) || any(d$received <= 0)) { out$message <- "denied above received or received not positive"; return(out) }
  for (v in names(d)) if (is.factor(d[[v]])) d[[v]] <- droplevels(d[[v]])
  rhs <- c(expo, covars, if (!is.null(fe)) paste0("factor(", fe, ")"))
  f <- stats::as.formula(paste("~", paste(rhs, collapse = " + ")))
  X <- stats::model.matrix(f, data = d)
  nm <- colnames(X)[-1]
  Z <- X[, -1, drop = FALSE]
  colnames(Z) <- sprintf("v%03d", seq_along(nm))
  expo_col <- colnames(Z)[match(expo, nm)]
  # the exposure is identified only if it adds a dimension to the other columns (intercept, covariates, fixed effects);
  # glm flags a LATER column as aliased, so an exposure that lies in the span of the fixed effects would otherwise pass
  # silently. An aliased exposure counts as non-convergence (SAP 7.3, 7.5).
  tolq <- min(1e-07, 1e-08 / 1000)
  ei <- match(expo, colnames(X))
  if (qr(X, tol = tolq)$rank <= qr(X[, -ei, drop = FALSE], tol = tolq)$rank) {
    out$message <- "exposure coefficient aliased (the exposure lies in the span of the other model columns)"
    return(out)
  }
  warns <- character(0)
  fitit <- function(Z) {
    dd <- data.frame(denied = d$denied, failed = d$received - d$denied, Z, check.names = FALSE)
    form <- stats::as.formula(paste0("cbind(denied, failed) ~ ", paste(colnames(Z), collapse = " + ")))
    withCallingHandlers(stats::glm(form, data = dd, family = stats::quasibinomial()),
      warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") })
  }
  fit <- fitit(Z)
  al <- is.na(stats::coef(fit))[colnames(Z)]
  if (any(al)) {
    out$removed <- nm[al]
    if (al[[expo_col]]) {
      out$message <- paste0("exposure coefficient aliased; removed: ", paste(out$removed, collapse = "; "))
      out$warnings <- warns
      return(out)
    }
    Z <- Z[, !al, drop = FALSE]
    fit <- fitit(Z)
    if (anyNA(stats::coef(fit))) { out$message <- "aliased columns remain after refit"; return(out) }
  }
  V <- sandwich::vcovCL(fit, cluster = d[[cluster]], type = "HC1")
  beta <- stats::coef(fit)
  e <- match(expo_col, names(beta))
  Xk <- cbind(1, Z)
  se <- sqrt(V[e, e])
  out <- utils::modifyList(out, list(
    ok = is.finite(beta[e]) && is.finite(se) && se > 0, beta = unname(beta[e]), se = unname(se),
    converged = isTRUE(fit$converged) && !isTRUE(fit$boundary) && is.finite(se) && se > 0,
    warnings = warns, X = Xk, coef = beta, V = V, e = e, fit = fit, d = d))
  if (!isTRUE(out$converged)) out$message <- paste0("glm converged = ", fit$converged, ", boundary = ", fit$boundary)
  out
}

GLMM_ATTEMPTS <- list(
  list(name = "laplace_bobyqa", nAGQ = 1L, optimizer = "bobyqa", maxfun = 2e5, center = FALSE),
  list(name = "pirls_nagq0_bobyqa", nAGQ = 0L, optimizer = "bobyqa", maxfun = 2e5, center = FALSE),
  list(name = "laplace_nelder_mead", nAGQ = 1L, optimizer = "Nelder_Mead", maxfun = 4e5, center = FALSE),
  list(name = "laplace_bobyqa_centered", nAGQ = 1L, optimizer = "bobyqa", maxfun = 2e5, center = TRUE)
)

glmm_one <- function(dd, form, a) {
  warns <- character(0)
  ctl <- lme4::glmerControl(optimizer = a$optimizer, optCtrl = list(maxfun = a$maxfun))
  fit <- tryCatch(
    withCallingHandlers(lme4::glmer(form, data = dd, family = stats::binomial, nAGQ = a$nAGQ, control = ctl),
      warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") }),
    error = function(e) e)
  if (inherits(fit, "error")) return(list(fit = NULL, ok = FALSE, msg = paste("error:", conditionMessage(fit)), warns = warns))
  conv <- fit@optinfo$conv
  msgs <- unique(c(warns, unlist(conv$lme4$messages)))
  # a boundary (singular) fit is reported through isSingular() and is not a convergence failure by itself
  other <- msgs[!grepl("boundary (singular) fit", msgs, fixed = TRUE)]
  ok <- identical(as.integer(conv$opt), 0L) && !length(other)
  list(fit = fit, ok = ok, msg = if (length(msgs)) paste(msgs, collapse = " | ") else "", warns = warns)
}

# Secondary mixed model (SAP 8.1): same exposure and covariates, no fixed effects for issuer or state,
# random intercepts for state, issuer within state, and an observation-level plan effect.
fit_glmm <- function(d, expo, covars = COVARS, attempts = GLMM_ATTEMPTS, label = "") {
  d <- as.data.frame(d)
  for (v in names(d)) if (is.factor(d[[v]])) d[[v]] <- droplevels(d[[v]])
  out <- list(label = label, ok = FALSE, converged = FALSE, expo = expo, n_plans = nrow(d),
              n_issuers = length(unique(d$issuer_id)), n_clusters = length(unique(d$state_issuer)),
              removed = character(0), warnings = character(0), message = "", fo_class = "glmm", log = character(0))
  dd <- data.frame(denied = d$denied, failed = d$received - d$denied,
                   state = factor(d$state), state_issuer = factor(d$state_issuer), plan_id = factor(d$plan_id),
                   d[, c(expo, setdiff(covars, c("plan_type", "metal3"))), drop = FALSE],
                   plan_type = d$plan_type, metal3 = d$metal3, check.names = FALSE)
  rhs <- c(expo, covars, "(1 | state)", "(1 | state_issuer)", "(1 | plan_id)")
  form <- stats::as.formula(paste("cbind(denied, failed) ~", paste(rhs, collapse = " + ")))
  first_fit <- NULL
  out$attempt_table <- data.frame()
  for (i in seq_along(attempts)) {
    a <- attempts[[i]]
    d2 <- dd
    if (isTRUE(a$center)) for (v in intersect(c(expo, "lenroll"), names(d2))) d2[[v]] <- d2[[v]] - mean(d2[[v]])
    r <- glmm_one(d2, form, a)
    out$log <- c(out$log, paste0(a$name, ": ", if (is.null(r$fit)) r$msg else if (r$ok) "clean" else paste0("NOT clean (", r$msg, ")")))
    if (!is.null(r$fit)) {
      cfa <- summary(r$fit)$coefficients
      out$attempt_table <- rbind(out$attempt_table, data.frame(attempt = a$name, index = i, clean = isTRUE(r$ok),
        beta = unname(cfa[expo, "Estimate"]), se = unname(cfa[expo, "Std. Error"]),
        msg = { z <- strsplit(r$msg, "\n", fixed = TRUE)[[1]]; if (length(z)) z[1] else "" }, stringsAsFactors = FALSE))
    }
    if (!is.null(r$fit) && is.null(first_fit)) first_fit <- list(r = r, a = a, i = i)
    if (r$ok) { first_fit <- list(r = r, a = a, i = i); break }
  }
  if (is.null(first_fit)) { out$message <- paste(out$log, collapse = " ; "); return(out) }
  r <- first_fit$r; fit <- r$fit; a <- first_fit$a
  cf <- summary(fit)$coefficients
  vc <- as.data.frame(lme4::VarCorr(fit))
  var_of <- function(g) { x <- vc$vcov[vc$grp == g]; if (length(x)) x[1] else NA_real_ }
  out <- utils::modifyList(out, list(
    ok = TRUE, converged = isTRUE(r$ok), beta = unname(cf[expo, "Estimate"]), se = unname(cf[expo, "Std. Error"]),
    attempt = a$name, attempt_index = first_fit$i, nAGQ = a$nAGQ, optimizer = a$optimizer, centered = isTRUE(a$center),
    singular = lme4::isSingular(fit), opt_code = as.integer(fit@optinfo$conv$opt), messages = r$msg,
    var_state = var_of("state"), var_issuer = var_of("state_issuer"), var_plan = var_of("plan_id"),
    message = if (isTRUE(r$ok)) "" else paste("not clean:", r$msg)))
  out
}

# One plain-words sentence on how the mixed-model fit was reached (used in the row notes and the ledger descriptions).
glmm_context_note <- function(g) {
  if (!isTRUE(g$ok)) return("the mixed model did not produce a fit")
  at <- g$attempt_table
  first <- if (nrow(at) && at$index[1] == 1L && g$attempt_index != 1L) {
    sa <- fit_stats(at$beta[1], at$se[1], "z")
    sprintf("the first attempt (Laplace, nAGQ 1) ended with a convergence message (%s) and gave OR %.4f (%.3f to %.3f)", at$msg[1], sa[["or"]], sa[["lo"]], sa[["hi"]])
  } else "the first attempt of the predeclared ladder gave the reported fit"
  paste0(first, "; the reported fit is attempt ", g$attempt, " (nAGQ ", g$nAGQ, ", ", g$optimizer, ")", if (g$nAGQ == 0) " per SAP 8.1" else "", ", ",
         if (isTRUE(g$converged)) "clean" else "NOT clean", ", ", if (isTRUE(g$singular)) "singular" else "not singular",
         "; standard errors are model-based, not clustered; the model also uses between-issuer contrasts; it is outside the Holm family")
}

# ---- Fig 3: average marginal prediction on the response scale with a delta-method band --------
amp_point <- function(fo, g) {
  X <- fo$X; b <- fo$coef; e <- fo$e
  eta <- as.vector(X %*% b) + (g - X[, e]) * b[e]
  mu <- stats::plogis(eta); w <- mu * (1 - mu)
  grad <- colMeans(w * X)
  grad[e] <- g * mean(w)
  list(p = mean(mu), grad = grad, se = sqrt(drop(t(grad) %*% fo$V %*% grad)))
}
amp_curve <- function(fo, grid) {
  r <- lapply(grid, function(g) { z <- amp_point(fo, g); c(p = z$p, se = z$se) })
  r <- do.call(rbind, r)
  data.frame(exposure_log2 = grid, predicted_probability = r[, "p"], se = r[, "se"],
             ci_low = r[, "p"] - CRIT_Z * r[, "se"], ci_high = r[, "p"] + CRIT_Z * r[, "se"])
}
amp_diff <- function(fo, g1, g2) {
  a <- amp_point(fo, g1); b <- amp_point(fo, g2)
  gr <- b$grad - a$grad
  se <- sqrt(drop(t(gr) %*% fo$V %*% gr))
  est <- b$p - a$p
  c(diff = est, se = se, lo = est - CRIT_Z * se, hi = est + CRIT_Z * se)
}

make_bins <- function(d, nb = dnum("design_fig3_bins")) {
  o <- order(d$expo_ortho, d$plan_id, method = "radix")
  d <- d[o, ]
  n <- nrow(d); sz <- n %/% nb
  d$bin <- pmin(ceiling(seq_len(n) / sz), nb)
  do.call(rbind, lapply(seq_len(nb), function(b) {
    x <- d[d$bin == b, ]
    data.frame(bin = b, n_plans = nrow(x), exposure_median_log2 = stats::median(x$expo_ortho),
               exposure_min_log2 = min(x$expo_ortho), exposure_max_log2 = max(x$expo_ortho),
               density_median_per_10000 = 2^stats::median(x$expo_ortho),
               denied = sum(x$denied), claims_received = sum(x$received),
               denial_proportion = sum(x$denied) / sum(x$received))
  }))
}

# ---- outcome-suppression candidates (SAP 9.4) ---------------------------------------------
build_outcome_bound_plans <- function(inp, D) {
  pl <- prep_plans(copy(inp$pl)); ex <- inp$ex
  out <- pl[!plan_id %in% ex$plan_id]
  out[, `:=`(rec_m = plan_claims_received_in_network_marker, den_m = plan_claims_denied_in_network_marker)]
  supp <- out[rec_m == "star2" | den_m == "star2"]
  n_total <- nrow(supp)
  both <- supp[rec_m == "star2" & den_m == "star2"]
  one <- supp[!(rec_m == "star2" & den_m == "star2")]
  one[, side := ifelse(den_m == "star2", "denied", "received")]
  # donors: exposure-ok cohort plans of the same issuer (and state) with the same non-missing network id
  dn <- merge(ex[exposure_status == "ok", .(plan_id, issuer_id, state, n_individual_npis, n_ortho_surgeon_text,
                                             n_ortho_apc_text, n_derm_text, n_ophth_text)],
              pl[, .(plan_id, pa_network_id)], by = "plan_id")
  dn <- dn[!is.na(pa_network_id)]
  dn[, tuple := paste(n_ortho_surgeon_text, n_ortho_apc_text, n_derm_text, n_ophth_text, n_individual_npis, sep = "|")]
  dg <- dn[, .(n_donor_plans = .N, n_tuples = uniqueN(tuple), n_ortho_surgeon_text = n_ortho_surgeon_text[1],
               n_ortho_apc_text = n_ortho_apc_text[1], n_derm_text = n_derm_text[1], n_ophth_text = n_ophth_text[1],
               n_individual_npis = n_individual_npis[1]), by = .(issuer_id, state, pa_network_id)]
  dg <- dg[n_tuples == 1L]
  one <- merge(one, dg, by = c("issuer_id", "state", "pa_network_id"), all.x = TRUE, sort = FALSE)
  one[, `:=`(has_donor = !is.na(n_donor_plans), has_enroll = !is.na(enrollment) & enrollment > 0)]
  funnel <- c(total_suppressed = n_total, both_suppressed = nrow(both), single_suppressed = nrow(one),
              single_with_donor = sum(one$has_donor), single_with_numeric_enrollment = sum(one$has_enroll),
              single_with_donor_and_enrollment = sum(one$has_donor & one$has_enroll))
  cand <- one[has_donor & has_enroll]
  rows <- list()
  for (b in c(0, dnum("design_suppression_mid"), dnum("design_suppression_upper"))) {
    if (!nrow(cand)) break
    x <- copy(cand)
    x[, `:=`(denied_b = ifelse(side == "denied", b, denied), received_b = ifelse(side == "received", b, received))]
    x <- x[!is.na(denied_b) & !is.na(received_b) & received_b > 0 & denied_b <= received_b]
    if (nrow(x)) {
      x[, bound := b]
      rows[[length(rows) + 1L]] <- x
    }
  }
  add <- if (length(rows)) rbindlist(rows) else data.table()
  if (nrow(add)) {
    add <- add[, .(plan_id, bound, suppressed_side = side, issuer_id, state, state_issuer = paste(state, issuer_id, sep = "_"),
                   plan_type, metal_level, metal3, enrollment, denied = denied_b, received = received_b,
                   n_donor_plans, donor_network_id = pa_network_id, n_ortho_surgeon_text, n_ortho_apc_text, n_derm_text,
                   n_ophth_text, n_individual_npis)]
    setDF(add)
    add$plan_type <- factor(add$plan_type, levels = PLAN_LEVELS)
    add$metal3 <- factor(add$metal3, levels = METAL3_LEVELS)
    add <- add_exposures(add)
  } else add <- as.data.frame(add)
  list(add = add, funnel = funnel)
}

restore_types <- function(D) {
  D$plan_type <- factor(D$plan_type, levels = PLAN_LEVELS)
  D$metal3 <- factor(D$metal3, levels = METAL3_LEVELS)
  for (v in c("plan_id", "issuer_id", "state", "state_issuer", "exposure_status")) if (v %in% names(D)) D[[v]] <- as.character(D[[v]])
  D
}

# ---- row-level result records ---------------------------------------------------------------
est_row <- function(code, exposure, model, fo, method = "z", se_type = "cluster HC1 (issuer)", notes = "",
                    n_plans = NULL, n_issuers = NULL, n_clusters = NULL) {
  r <- data.frame(row_code = code, label = unname(ROW_LABELS[code]), exposure = exposure, model = model,
                  n_plans = if (is.null(n_plans)) fo$n_plans else n_plans,
                  n_issuers = if (is.null(n_issuers)) fo$n_issuers else n_issuers,
                  n_clusters = if (is.null(n_clusters)) fo$n_clusters else n_clusters,
                  log_odds = NA_real_, se_cluster = NA_real_, odds_ratio_per_doubling = NA_real_,
                  ci_low = NA_real_, ci_high = NA_real_, p_value = NA_real_, p_holm = NA_real_,
                  converged = FALSE, notes = notes, se_type = se_type,
                  ci_method = if (method == "z") "z" else paste0("t(", DF_T, ")"),
                  stringsAsFactors = FALSE)
  if (isTRUE(fo$ok)) {
    s <- fit_stats(fo$beta, fo$se, method, DF_T)
    r$log_odds <- fo$beta; r$se_cluster <- if (identical(fo$fo_class, "qb")) fo$se else NA_real_
    r$odds_ratio_per_doubling <- s[["or"]]; r$ci_low <- s[["lo"]]; r$ci_high <- s[["hi"]]; r$p_value <- s[["p"]]
    r$converged <- isTRUE(fo$converged)
  }
  extra <- c(if (length(fo$removed)) paste0("aliased columns removed and refitted: ", paste(fo$removed, collapse = "; ")),
             if (length(fo$warnings)) paste0("warnings: ", paste(unique(fo$warnings), collapse = " | ")),
             if (nzchar(fo$message)) fo$message)
  if (length(extra)) r$notes <- paste(c(if (nzchar(notes)) notes, extra), collapse = " ; ")
  r
}

DEVIATIONS <- list()
add_deviation <- function(item, sap_text, what_was_done, reason) {
  DEVIATIONS[[length(DEVIATIONS) + 1L]] <<- data.frame(item = item, sap_text = sap_text, what_was_done = what_was_done,
                                                       reason = reason, stringsAsFactors = FALSE)
}

safe_row <- function(code, expr_fn) {
  tryCatch(expr_fn(), error = function(e) {
    lg("row ", code, " FAILED: ", conditionMessage(e), level = "WARN")
    list(ok = FALSE, converged = FALSE, message = paste("error:", conditionMessage(e)), removed = character(0),
         warnings = character(0), n_plans = NA_integer_, n_issuers = NA_integer_, n_clusters = NA_integer_, fo_class = "qb")
  })
}

# Primary model with the fallback order of SAP 7.5: the model chosen by the decision rule; if it does not converge or
# the exposure is aliased, state fixed effects; then the mixed model of Section 8. Covariates are never dropped.
fit_primary <- function(A, rule, say = function(...) invisible(NULL)) {
  fe_primary <- c(issuer_fe = "issuer_id", state_fe = "state")[[rule]]
  fo_p <- fit_qb(A, "expo_ortho", COVARS, fe_primary, "issuer_id", "primary")
  used <- rule
  if (!(isTRUE(fo_p$ok) && isTRUE(fo_p$converged))) {
    say("PRIMARY FAILED: ", fo_p$message, " ; applying SAP 7.5 fallbacks in order", level = "WARN")
    add_deviation("primary model", "SAP 7.1 and 7.4: quasibinomial glm with the fixed effects chosen by the decision rule",
                  "fallback per SAP 7.5 triggered", paste("primary did not converge or exposure aliased:", fo_p$message))
    if (rule == "issuer_fe") {
      fo2 <- fit_qb(A, "expo_ortho", COVARS, "state", "issuer_id", "primary (state fixed effects fallback)")
      if (isTRUE(fo2$ok) && isTRUE(fo2$converged)) { fo_p <- fo2; used <- "state_fe" }
    }
    if (used == rule) {
      fo3 <- fit_glmm(A, "expo_ortho", COVARS, label = "primary (mixed model fallback)")
      if (isTRUE(fo3$ok)) { fo_p <- fo3; used <- "mixed" }
      else stop("BLOCKED: primary, state fixed effects and mixed model all failed", call. = FALSE)
    }
  }
  list(fo = fo_p, used = used)
}

# ---- 3. run every model of the SAP -----------------------------------------------------------
run_models <- function(D, ADD, inp_info = NULL, verbose = TRUE) {
  say <- function(...) if (verbose) lg(...)
  A <- D[D$in_analytic, ]
  K <- count_informative(A)
  rule <- primary_model_rule(K)
  if (!identical(rule, DK[["design_primary_model"]])) {
    stop("BLOCKED: the decision rule gives ", rule, " but the frozen design constant says ", DK[["design_primary_model"]], call. = FALSE)
  }
  fe_primary <- c(issuer_fe = "issuer_id", state_fe = "state")[[rule]]
  say("decision rule: K = ", K, " informative issuers (threshold ", dnum("design_informative_issuer_threshold"),
      ") -> primary model ", rule)
  est <- list(); res <- list()

  # ---- PRIMARY, fitted first ----------------------------------------------------------------
  pf <- fit_primary(A, rule, say)
  fo_p <- pf$fo; primary_model_used <- pf$used
  s <- fit_stats(fo_p$beta, fo_p$se, "z")
  say("PRIMARY (fitted first, before any other model; specification not changed afterwards): model ", primary_model_used,
      " ; OR per doubling ", sprintf("%.4f", s[["or"]]), " (95% CI ", sprintf("%.4f", s[["lo"]]), " to ", sprintf("%.4f", s[["hi"]]),
      ") ; P ", signif(s[["p"]], 4), " ; n plans ", fo_p$n_plans, " ; issuers ", fo_p$n_issuers, " ; clusters ", fo_p$n_clusters,
      " ; converged ", fo_p$converged)
  model_str <- c(issuer_fe = "quasibinomial logit, issuer fixed effects, HC1 clustered by issuer",
                 state_fe = "quasibinomial logit, state fixed effects, HC1 clustered by issuer",
                 mixed = "mixed model (fallback)")[[primary_model_used]]
  est$primary <- est_row("primary", "expo_ortho", model_str, fo_p,
                         notes = paste0("K = ", K, " informative issuers; effective number of issuers ",
                                        sprintf("%.1f", dnum("design_effective_issuers")), " (identifying variation concentrated)"),
                         se_type = if (identical(fo_p$fo_class, "qb")) "cluster HC1 (issuer)" else "model-based Wald (glmer)")
  res$primary <- fo_p

  qb_issuer <- function(d, expo, covars, label) fit_qb(d, expo, covars, fe_primary, "issuer_id", label)
  qb_str <- model_str

  # ---- Table 3 rows -------------------------------------------------------------------------
  fo <- safe_row("sens_a", function() qb_issuer(A, "expo_ortho", COVARS_NOENR, "sens_a"))
  est$sens_a <- est_row("sens_a", "expo_ortho", qb_str, fo, notes = "no log2 enrollment term"); res$sens_a <- fo

  fo <- safe_row("sens_b", function() fit_qb(A, "expo_ortho", COVARS, "state", "issuer_id", "sens_b"))
  est$sens_b <- est_row("sens_b", "expo_ortho", "quasibinomial logit, state fixed effects, HC1 clustered by issuer", fo); res$sens_b <- fo

  fo <- safe_row("sens_c", function() fit_glmm(A, "expo_ortho", COVARS, label = "sens_c"))
  gl_note <- if (isTRUE(fo$ok)) paste0("glmer binomial, random intercepts state, issuer within state, plan; attempt ", fo$attempt,
                                       " (nAGQ ", fo$nAGQ, ", ", fo$optimizer, if (fo$centered) ", predictors centered" else "",
                                       "); singular ", fo$singular, "; Wald CI from model-based SE; ", glmm_context_note(fo)) else ""
  est$sens_c <- est_row("sens_c", "expo_ortho", "mixed model: glmer binomial, random intercepts for state, issuer within state, plan",
                        fo, se_type = "model-based Wald (glmer)", notes = gl_note); res$sens_c <- fo
  if (isTRUE(fo$ok) && isTRUE(fo$centered)) {
    add_deviation("sens_c mixed model", "SAP 8.1: same exposure and covariates; nAGQ = 0 if needed",
                  "fitted with log2 exposure and log2 enrollment centered at their sample means (same model, reparametrised; the slope per doubling is unchanged)",
                  "the three predeclared attempts on the uncentered variables (Laplace bobyqa, nAGQ 0, Nelder-Mead) did not all end clean; centering is a numerical-conditioning step")
  }

  lo1 <- as.character(DK[["design_leverage_top1_issuer"]])
  fo <- safe_row("sens_d", function() qb_issuer(A[A$issuer_id != lo1, ], "expo_ortho", COVARS, "sens_d"))
  est$sens_d <- est_row("sens_d", "expo_ortho", qb_str, fo, notes = paste0("issuer ", lo1, " left out")); res$sens_d <- fo

  O <- D[D$in_exposure_ok, ]
  fo <- safe_row("sens_e", function() qb_issuer(O, "expo_share", COVARS_NOENR, "sens_e"))
  est$sens_e <- est_row("sens_e", "expo_share", qb_str, fo, notes = "all exposure-ok plans, no enrollment term, Holm family of 2"); res$sens_e <- fo

  fo <- safe_row("sens_f", function() qb_issuer(A, "expo_apc", COVARS, "sens_f"))
  est$sens_f <- est_row("sens_f", "expo_apc", qb_str, fo, notes = "surgeon plus APC text count per 10,000 reported enrollees; Holm family of 2"); res$sens_f <- fo

  fo <- safe_row("sens_g", function() qb_issuer(A, "expo_derm", COVARS, "sens_g"))
  est$sens_g <- est_row("sens_g", "expo_derm", qb_str, fo,
                        notes = paste0("partial correlation with orthopaedic density ", sprintf("%.3f", dnum("design_partial_corr_derm")),
                                       " (design_partial_corr_derm); raw Spearman ", sprintf("%.3f", dnum("design_spearman_ortho_derm")))); res$sens_g <- fo

  fo <- safe_row("sens_h", function() qb_issuer(A, "expo_ophth", COVARS, "sens_h"))
  est$sens_h <- est_row("sens_h", "expo_ophth", qb_str, fo,
                        notes = paste0("offset ", signif(OFFSET_OPHTH, 6), " added inside the log; partial correlation with orthopaedic density ",
                                       sprintf("%.3f", dnum("design_partial_corr_ophth")), " (design_partial_corr_ophth); raw Spearman ",
                                       sprintf("%.3f", dnum("design_spearman_ortho_ophth")))); res$sens_h <- fo

  # outcome-suppression rows (SAP 9.4): analytic plans plus the added-back plans
  cols_add <- c("plan_id", "issuer_id", "state", "state_issuer", "plan_type", "metal3", "denied", "received",
                "lenroll", "expo_ortho", "n_ortho_surgeon_text")
  for (cc in list(c("sens_i", 0), c("sens_j", 10), c("sens_n", 5.5))) {
    code <- cc[1]; b <- as.numeric(cc[2])
    ab <- if (nrow(ADD)) ADD[ADD$bound == b, ] else ADD
    n_add <- if (nrow(ADD)) nrow(ab) else 0L
    dd <- rbind(A[, cols_add], if (n_add) ab[, cols_add])
    fo <- safe_row(code, function() qb_issuer(dd, "expo_ortho", COVARS, code))
    est[[code]] <- est_row(code, "expo_ortho", qb_str, fo,
                           notes = paste0(n_add, " plans added back (suppressed outcome count set to ", b, "); near-null by construction"))
    fo$n_added <- n_add; res[[code]] <- fo
  }
  # enrollment-suppression rows (SAP 9.5): the 494 exposure-ok plans with a "**" enrollment get the bound
  for (cc in list(c("sens_k", 1), c("sens_l", 10), c("sens_o", 5.5))) {
    code <- cc[1]; b <- as.numeric(cc[2])
    Ob <- O
    enr <- ifelse(Ob$enrollment_marker == "star2", b, Ob$enrollment)
    Ob <- add_exposures(Ob, enr)
    fo <- safe_row(code, function() qb_issuer(Ob, "expo_ortho", COVARS, code))
    n_add <- sum(Ob$enrollment_marker == "star2")
    est[[code]] <- est_row(code, "expo_ortho", qb_str, fo, notes = paste0(n_add, " plans added back (enrollment set to ", b, " in the exposure and the enrollment term)"))
    fo$n_added <- n_add; res[[code]] <- fo
  }

  # ---- Supplement rows ---------------------------------------------------------------------------
  # sens_m: issuer-level, quasi-binomial on issuer totals, no glm weights, claim-weighted means form the two predictors, HC1 by state
  ag <- do.call(rbind, lapply(split(A, A$issuer_id), function(x) data.frame(
    issuer_id = x$issuer_id[1], state = x$state[1], state_issuer = x$state_issuer[1],
    denied = sum(x$denied), received = sum(x$received),
    expo_ortho = stats::weighted.mean(x$expo_ortho, x$received), lenroll = stats::weighted.mean(x$lenroll, x$received),
    n_plans_issuer = nrow(x), stringsAsFactors = FALSE)))
  fo <- safe_row("sens_m", function() fit_qb(ag, "expo_ortho", "lenroll", NULL, "state", "sens_m"))
  est$sens_m <- est_row("sens_m", "expo_ortho", "issuer-level quasibinomial logit on issuer totals, no glm weights, HC1 clustered by state", fo,
                        se_type = "cluster HC1 (state)", n_plans = nrow(A), n_issuers = nrow(ag),
                        notes = "weights (plan claim lines received) only form the claim-weighted mean log2 density and mean log2 enrollment"); res$sens_m <- fo

  fo <- safe_row("sens_p", function() {
    small <- unique(A$issuer_id[!is.na(A$issuer_in_network_claims) & A$issuer_in_network_claims < dnum("design_min_claims_exclusion")])
    r <- qb_issuer(A[!A$issuer_id %in% small, ], "expo_ortho", COVARS, "sens_p")
    r$n_small_issuers <- length(small); r$n_small_plans <- sum(A$issuer_id %in% small); r })
  est$sens_p <- est_row("sens_p", "expo_ortho", qb_str, fo,
                        notes = paste0(fo$n_small_issuers, " issuers (", fo$n_small_plans, " plans) with fewer than ",
                                       format(dnum("design_min_claims_exclusion"), big.mark = ","), " in-network claim lines in the issuer file left out")); res$sens_p <- fo

  fo <- safe_row("sens_q", function() qb_issuer(A[A$metal_level != "Catastrophic", ], "expo_ortho", COVARS, "sens_q"))
  est$sens_q <- est_row("sens_q", "expo_ortho", qb_str, fo, notes = "Catastrophic plans left out"); res$sens_q <- fo

  lo2 <- as.character(DK[["design_largest_issuer_id"]])
  fo <- safe_row("sens_r", function() qb_issuer(A[A$issuer_id != lo2, ], "expo_ortho", COVARS, "sens_r"))
  est$sens_r <- est_row("sens_r", "expo_ortho", qb_str, fo, notes = paste0("issuer ", lo2, " left out")); res$sens_r <- fo

  # sens_s: leave-one-issuer-out over the K informative issuers (same model type; K not recomputed)
  inf <- informative_ids(A)
  loo <- do.call(rbind, lapply(inf, function(id) {
    fo <- safe_row("sens_s", function() qb_issuer(A[A$issuer_id != id, ], "expo_ortho", COVARS, "sens_s"))
    base <- data.frame(issuer_id = id, state = A$state[A$issuer_id == id][1], n_plans_left_out = sum(A$issuer_id == id),
                       n_plans = fo$n_plans, n_issuers = fo$n_issuers, odds_ratio_per_doubling = NA_real_, ci_low = NA_real_,
                       ci_high = NA_real_, p_value = NA_real_, excludes_one = NA, converged = FALSE, notes = fo$message,
                       stringsAsFactors = FALSE)
    if (isTRUE(fo$ok)) {
      s <- fit_stats(fo$beta, fo$se, "z")
      base$odds_ratio_per_doubling <- s[["or"]]; base$ci_low <- s[["lo"]]; base$ci_high <- s[["hi"]]; base$p_value <- s[["p"]]
      base$excludes_one <- s[["lo"]] > 1 || s[["hi"]] < 1; base$converged <- isTRUE(fo$converged)
      if (length(fo$removed)) base$notes <- paste0("aliased columns removed: ", paste(fo$removed, collapse = "; "))
    }
    base
  }))
  est$sens_s <- est_row("sens_s", "expo_ortho", qb_str, fo_p,
                        notes = "Leave-out range in extra columns; OR, CI shown here are the full-sample primary (the SAP asks for the full-sample interval beside the leave-out range); the 33 leave-out estimates are in model_sens_leave_one_out.csv",
                        n_plans = fo_p$n_plans, n_issuers = fo_p$n_issuers)
  res$sens_s <- list(loo = loo, K = K)

  # sens_t: same estimate and HC1 covariance, t interval on K - 1 = 32 df
  if (!identical(DF_T, K - 1)) stop("BLOCKED: design_t_interval_df is not K - 1", call. = FALSE)
  est$sens_t <- est_row("sens_t", "expo_ortho", qb_str, fo_p, method = "t",
                        notes = paste0("t interval on ", DF_T, " degrees of freedom; the z interval is ", sprintf("%.4f", s[["lo"]]), " to ",
                                       sprintf("%.4f", s[["hi"]]), " (the primary CI stays the z interval)"),
                        se_type = if (identical(fo_p$fo_class, "qb")) "cluster HC1 (issuer)" else "model-based Wald (glmer)")
  res$sens_t <- fo_p

  E <- do.call(rbind, est[ALL_CODES])
  rownames(E) <- NULL
  ph <- stats::p.adjust(c(E$p_value[E$row_code == "sens_e"], E$p_value[E$row_code == "sens_f"]), method = "holm")
  E$p_holm[E$row_code == "sens_e"] <- ph[1]; E$p_holm[E$row_code == "sens_f"] <- ph[2]
  list(est = E, res = res, loo = loo, A = A, K = K, rule = rule, primary_model_used = primary_model_used, fe_primary = fe_primary)
}

# ---- cohort flow, Table 1 data, selection check --------------------------------------------------
RUNG_LABELS <- c(
  cohort = "Cohort: plans with a computable in-network denial proportion",
  excluded_exposure_status = "Excluded for exposure status",
  issuer_unreachable = "Issuer provider files not retrievable",
  issuer_incomplete = "Issuer provider files incomplete",
  plan_not_in_directory = "Plan not found in the directory",
  no_individuals = "Plan listed without individual clinicians",
  exposure_ok = "Exposure available (exposure status ok)",
  lost_complete_case = "Lost to the complete-case rule (enrollment suppressed)",
  analytic = "Analytic sample (exposure available and reported enrollment)",
  excluded_not_analytic = "All excluded cohort plans (comparison group, not a ladder step)"
)
rung_index <- function(D, code) {
  switch(code,
    cohort = rep(TRUE, nrow(D)),
    excluded_exposure_status = !D$in_exposure_ok,
    issuer_unreachable = D$exposure_status == "issuer_unreachable",
    issuer_incomplete = D$exposure_status == "issuer_incomplete",
    plan_not_in_directory = D$exposure_status == "plan_not_in_directory",
    no_individuals = D$exposure_status == "no_individuals",
    exposure_ok = D$in_exposure_ok,
    lost_complete_case = D$in_exposure_ok & !D$in_analytic,
    analytic = D$in_analytic,
    excluded_not_analytic = !D$in_analytic)
}
rung_metrics <- function(x) {
  data.frame(n_plans = nrow(x), n_issuers = length(unique(x$issuer_id)), n_states = length(unique(x$state)),
             claim_lines_received = sum(x$received), claim_lines_denied = sum(x$denied),
             denial_proportion = if (nrow(x)) sum(x$denied) / sum(x$received) else NA_real_,
             plan_median_denial_proportion = if (nrow(x)) stats::median(x$denied / x$received) else NA_real_,
             median_claim_lines_per_plan = if (nrow(x)) stats::median(x$received) else NA_real_)
}
STRATA <- function(D) {
  st <- list(list(dimension = "overall", level = "All plans", idx = rep(TRUE, nrow(D))))
  for (lv in PLAN_LEVELS) st[[length(st) + 1L]] <- list(dimension = "plan_type", level = lv, idx = D$plan_type == lv)
  for (lv in sort(unique(D$metal_level))) st[[length(st) + 1L]] <- list(dimension = "metal_level", level = lv, idx = D$metal_level == lv)
  st
}
build_cohort_flow <- function(D) {
  rows <- list()
  for (s in STRATA(D)) {
    Ds <- D[s$idx, ]
    tot_plans <- nrow(Ds); tot_claims <- sum(Ds$received)
    for (code in names(RUNG_LABELS)) {
      x <- Ds[rung_index(Ds, code), ]
      m <- rung_metrics(x)
      rows[[length(rows) + 1L]] <- cbind(data.frame(dimension = s$dimension, level = s$level, rung_code = code,
                                                    rung_label = RUNG_LABELS[[code]], stringsAsFactors = FALSE), m,
                                         share_of_cohort_plans = m$n_plans / tot_plans,
                                         share_of_cohort_claim_lines = m$claim_lines_received / tot_claims)
    }
  }
  do.call(rbind, rows)
}
build_table1 <- function(D) {
  A <- D[D$in_analytic, ]
  q <- function(x) stats::quantile(x, c(0.25, 0.5, 0.75), type = 7, names = FALSE)
  rows <- lapply(STRATA(D), function(s) {
    Ds <- D[s$idx, ]; As <- Ds[Ds$in_analytic, ]
    dq <- q(2^As$expo_ortho); eq <- q(As$enrollment); cq <- q(As$n_ortho_surgeon_text)
    data.frame(section = c(overall = "Overall", plan_type = "Plan type", metal_level = "Metal level")[[s$dimension]],
               dimension = s$dimension, level = s$level,
               plans_cohort = nrow(Ds), plans_exposure_ok = sum(Ds$in_exposure_ok), plans_analytic = nrow(As),
               issuers_cohort = length(unique(Ds$issuer_id)), issuers_analytic = length(unique(As$issuer_id)),
               claim_lines_cohort = sum(Ds$received), claim_lines_analytic = sum(As$received),
               share_of_analytic_plans = nrow(As) / nrow(A),
               density_q25 = dq[1], density_median = dq[2], density_q75 = dq[3],
               surgeon_count_median = cq[2], enrollment_q25 = eq[1], enrollment_median = eq[2], enrollment_q75 = eq[3],
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# ---- ledger ------------------------------------------------------------------------------------
lkey <- function(...) tolower(gsub("[^a-z0-9]+", "_", tolower(paste(..., sep = "_"))))
LADD_SKIPPED <- character(0)
ladd <- function(key, value, unit, desc) {
  if (length(value) != 1L || is.na(value) || (is.numeric(value) && !is.finite(value))) {
    LADD_SKIPPED <<- c(LADD_SKIPPED, key)
    return(invisible(FALSE))
  }
  if (is.logical(value)) value <- as.integer(value)
  ledger_add("models", key, value, unit, desc, SRC)
  invisible(TRUE)
}
id_text <- function(x) {
  x <- as.character(x)
  ifelse(grepl("^0[0-9]", x), paste0("ID ", x), x)
}

fill_ledger <- function(M, flow, tab1, ADD, curve, bins, ctrl, pcts, sel, outcome_funnel, glmm_fo) {
  E <- M$est; A <- M$A; loo <- M$loo
  # primary
  p <- E[E$row_code == "primary", ]
  ladd("model_primary_or", p$odds_ratio_per_doubling, "odds ratio per doubling", "Primary model odds ratio per doubling of orthopaedic surgeon density (log2), 95% z interval")
  ladd("model_primary_lo", p$ci_low, "odds ratio", "Primary model 95% CI lower limit (z)")
  ladd("model_primary_hi", p$ci_high, "odds ratio", "Primary model 95% CI upper limit (z)")
  ladd("model_primary_p", p$p_value, "P value", "Primary model two-sided Wald z P value, clustered HC1 covariance")
  ladd("model_primary_logodds", p$log_odds, "log odds", "Primary model coefficient of log2 density")
  ladd("model_primary_se", p$se_cluster, "log odds", "Primary model cluster-robust (HC1, issuer) standard error")
  ladd("model_primary_n_plans", p$n_plans, "plans", "Plans in the primary model")
  ladd("model_primary_n_issuers", p$n_issuers, "issuers", "Issuers in the primary model")
  ladd("model_primary_n_clusters", p$n_clusters, "clusters", "Clusters (issuers) in the primary model")
  ladd("model_primary_converged", as.integer(p$converged), "indicator", "1 if the primary glm converged with an unaliased exposure coefficient")
  ladd("model_primary_excludes_one", as.integer(p$ci_low > 1 || p$ci_high < 1), "indicator", "1 if the primary 95% CI excludes 1")
  ladd("model_primary_model", M$primary_model_used, "text", "Model type used for the primary estimate (issuer_fe unless a SAP 7.5 fallback was needed)")
  ladd("model_primary_k", M$K, "issuers", "Informative issuers (recomputed in WP6b; equals design_informative_issuer_k)")
  ladd("model_primary_rel_change", p$odds_ratio_per_doubling - 1, "proportion", "Relative change in the odds of denial per doubling of density (OR minus 1)")
  # every Table 3 / Supplement row
  n_recv_supp <- if (nrow(ADD)) sum(unique(ADD[, c("plan_id", "suppressed_side")])$suppressed_side == "received") else 0L
  for (code in setdiff(ALL_CODES, "primary")) {
    r <- E[E$row_code == code, ]
    pre <- code
    dsc <- function(what) {
      if (code == "sens_s") paste0("sens_s row, FULL-SAMPLE primary ", what, " repeated for table alignment, not a leave-out estimate and not to be cited; the leave-out range is in sens_s_or_min, sens_s_or_max (with their _lo, _hi and issuer keys), sens_s_n_leaveouts and sens_s_n_excl1")
      else paste0(ROW_LABELS[[code]], ": ", what)
    }
    gx <- if (code == "sens_c") paste0("; ", glmm_context_note(glmm_fo)) else ""
    ladd(paste0(pre, "_or"), r$odds_ratio_per_doubling, "odds ratio per doubling", paste0(dsc("odds ratio per doubling"), gx))
    ladd(paste0(pre, "_lo"), r$ci_low, "odds ratio", paste0(dsc("95% CI lower limit"), gx))
    ladd(paste0(pre, "_hi"), r$ci_high, "odds ratio", paste0(dsc("95% CI upper limit"), gx))
    ladd(paste0(pre, "_p"), r$p_value, "P value", paste0(dsc("two-sided P value"), gx))
    ladd(paste0(pre, "_n_plans"), r$n_plans, "plans", dsc("plans"))
    ladd(paste0(pre, "_n_issuers"), r$n_issuers, "issuers", dsc("issuers"))
    ladd(paste0(pre, "_converged"), as.integer(r$converged), "indicator", dsc("1 if converged"))
    nadd <- M$res[[code]]$n_added
    if (!is.null(nadd)) {
      extra <- if (code == "sens_i" && n_recv_supp > 0) paste0(" (", n_recv_supp, " qualifying plan with received suppressed cannot be added at bound 0: the denominator would be 0)") else ""
      ladd(paste0(pre, "_n_added"), nadd, "plans", paste0(ROW_LABELS[[code]], ": plans added back to the analytic sample", extra))
    }
  }
  for (code in c("sens_e", "sens_f")) {
    ladd(paste0(code, "_p_holm"), E$p_holm[E$row_code == code], "P value", paste0(ROW_LABELS[[code]], ": Holm-adjusted P value (family of 2)"))
  }
  for (code in c("sens_m")) ladd(paste0(code, "_n_clusters"), E$n_clusters[E$row_code == code], "clusters", "Issuer-level analysis: state clusters")
  ladd("sens_p_n_small_issuers", M$res$sens_p$n_small_issuers, "issuers", "Issuers left out in sens_p (fewer than 10,000 in-network claim lines)")
  ladd("sens_p_n_small_plans", M$res$sens_p$n_small_plans, "plans", "Plans of the issuers left out in sens_p")
  # t interval beside the z interval
  ladd("sens_t_z_lo", E$ci_low[E$row_code == "primary"], "odds ratio", "Primary z interval lower limit, shown beside the t interval")
  ladd("sens_t_z_hi", E$ci_high[E$row_code == "primary"], "odds ratio", "Primary z interval upper limit, shown beside the t interval")
  ladd("sens_t_df", DF_T, "degrees of freedom", "Degrees of freedom of the conservative t interval")
  # leave-one-issuer-out summary
  okl <- loo[!is.na(loo$odds_ratio_per_doubling), ]
  imin <- which.min(okl$odds_ratio_per_doubling); imax <- which.max(okl$odds_ratio_per_doubling)
  ladd("sens_s_n_leaveouts", nrow(loo), "refits", "Leave-one-issuer-out refits (informative issuers)")
  ladd("sens_s_n_converged", sum(loo$converged), "refits", "Leave-out refits that converged")
  ladd("sens_s_n_excl1", sum(okl$excludes_one), "refits", "Leave-outs whose 95% z interval excludes 1")
  ladd("sens_s_or_min", okl$odds_ratio_per_doubling[imin], "odds ratio per doubling", "Smallest leave-one-issuer-out odds ratio per doubling")
  ladd("sens_s_or_min_lo", okl$ci_low[imin], "odds ratio", "Interval lower limit of the leave-out with the smallest odds ratio")
  ladd("sens_s_or_min_hi", okl$ci_high[imin], "odds ratio", "Interval upper limit of the leave-out with the smallest odds ratio")
  ladd("sens_s_issuer_min", id_text(okl$issuer_id[imin]), "issuer id", "Issuer whose removal gives the smallest odds ratio")
  ladd("sens_s_state_min", okl$state[imin], "state", "State of the issuer whose removal gives the smallest odds ratio")
  ladd("sens_s_or_max", okl$odds_ratio_per_doubling[imax], "odds ratio per doubling", "Largest leave-one-issuer-out odds ratio per doubling")
  ladd("sens_s_or_max_lo", okl$ci_low[imax], "odds ratio", "Interval lower limit of the leave-out with the largest odds ratio")
  ladd("sens_s_or_max_hi", okl$ci_high[imax], "odds ratio", "Interval upper limit of the leave-out with the largest odds ratio")
  ladd("sens_s_issuer_max", id_text(okl$issuer_id[imax]), "issuer id", "Issuer whose removal gives the largest odds ratio")
  ladd("sens_s_state_max", okl$state[imax], "state", "State of the issuer whose removal gives the largest odds ratio")
  # outcome-suppression funnel
  for (nm in names(outcome_funnel)) ladd(paste0("sens_suppout_", nm), unname(outcome_funnel[nm]), "plans", paste0("Outcome suppression (SAP 9.4): ", gsub("_", " ", nm)))
  if (nrow(ADD)) {
    qp <- unique(ADD[, c("plan_id", "issuer_id", "suppressed_side")])
    byi <- sort(table(qp$issuer_id), decreasing = TRUE)
    ladd("sens_suppout_qualifying_plans", nrow(qp), "plans", "Outcome suppression (SAP 9.4): plans that pass the donor and numeric-enrollment rules and enter at least one bound row")
    ladd("sens_suppout_issuers_with_added_plans", length(byi), "issuers", "Outcome suppression (SAP 9.4): issuers that contribute the qualifying plans")
    ladd("sens_suppout_plans_largest_issuer", as.integer(byi[1]), "plans", paste0("Outcome suppression (SAP 9.4): qualifying plans from the issuer with the most of them (issuer ", names(byi)[1], ")"))
    ladd("sens_suppout_largest_issuer_id", id_text(names(byi)[1]), "issuer id", "Outcome suppression (SAP 9.4): issuer contributing the most qualifying plans")
    ladd("sens_suppout_denied_suppressed_plans", sum(qp$suppressed_side == "denied"), "plans", "Outcome suppression (SAP 9.4): qualifying plans with the denied count suppressed (added in every bound row)")
    ladd("sens_suppout_received_suppressed_plans", sum(qp$suppressed_side == "received"), "plans", "Outcome suppression (SAP 9.4): qualifying plans with the received count suppressed; at bound 0 the denominator would be 0, so they are not added in sens_i")
  }
  # negative-control reading (SAP 6.2 and 6.4)
  noquote_txt <- if (!(p$ci_low > 1 || p$ci_high < 1)) "; not to be quoted; SAP 6.2 rule not applied because the primary CI includes 1; SAP 6.4 forbids a specificity statement" else ""
  for (nm in names(ctrl)) {
    cc <- ctrl[[nm]]
    ladd(paste0("model_control_", nm, "_ratio"), cc$ratio, "ratio", paste0("Control log-odds divided by the orthopaedic log-odds (", nm, ")", noquote_txt))
    ladd(paste0("model_control_", nm, "_same_sign"), as.integer(cc$same_sign), "indicator", paste0("1 if the ", nm, " control has the same sign as the orthopaedic estimate", noquote_txt))
    ladd(paste0("model_control_", nm, "_at_least_half"), as.integer(cc$at_least_half), "indicator", paste0("1 if the ", nm, " control is at least half as large (design_control_reading_ratio)", noquote_txt))
    ladd(paste0("model_control_", nm, "_reading"), cc$reading, "text", paste0("SAP 6.2 reading of the ", nm, " control (applied only when the primary CI excludes 1)"))
  }
  # GLMM
  g <- glmm_fo
  if (isTRUE(g$ok)) {
    s <- fit_stats(g$beta, g$se, "z")
    ladd("model_glmm_or", s[["or"]], "odds ratio per doubling", paste0("Mixed model odds ratio per doubling (Wald z); ", glmm_context_note(g)))
    ladd("model_glmm_lo", s[["lo"]], "odds ratio", paste0("Mixed model 95% CI lower limit; ", glmm_context_note(g)))
    ladd("model_glmm_hi", s[["hi"]], "odds ratio", paste0("Mixed model 95% CI upper limit; ", glmm_context_note(g)))
    ladd("model_glmm_p", s[["p"]], "P value", paste0("Mixed model two-sided Wald P value, model-based SE; ", glmm_context_note(g)))
    ladd("model_glmm_converged", as.integer(g$converged), "indicator", paste0("1 if the reported mixed model fit ended without a convergence message; ", glmm_context_note(g)))
    ladd("model_glmm_singular", as.integer(g$singular), "indicator", "1 if the mixed model fit is singular")
    ladd("model_glmm_nagq", g$nAGQ, "integer", "nAGQ of the reported mixed model fit")
    ladd("model_glmm_optimizer", g$optimizer, "text", "Optimizer of the reported mixed model fit")
    ladd("model_glmm_attempt", g$attempt_index, "integer", "Index of the attempt in the predeclared ladder that gave the reported fit")
    ladd("model_glmm_opt_code", g$opt_code, "integer", "Optimizer return code (0 = success)")
    ladd("model_glmm_var_state", g$var_state, "variance", "Random-intercept variance, state")
    ladd("model_glmm_var_issuer", g$var_issuer, "variance", "Random-intercept variance, issuer within state")
    ladd("model_glmm_var_plan", g$var_plan, "variance", "Observation-level random-effect variance, plan")
    ladd("model_glmm_n_plans", g$n_plans, "plans", "Plans in the mixed model")
    at <- g$attempt_table
    if (nrow(at) && at$index[1] == 1L && g$attempt_index != 1L) {
      sa <- fit_stats(at$beta[1], at$se[1], "z")
      ladd("model_glmm_laplace_or", sa[["or"]], "odds ratio per doubling", "Mixed model, first attempt (Laplace, nAGQ 1), which ended with a convergence message: odds ratio per doubling")
      ladd("model_glmm_laplace_lo", sa[["lo"]], "odds ratio", "Mixed model first attempt: 95% CI lower limit")
      ladd("model_glmm_laplace_hi", sa[["hi"]], "odds ratio", "Mixed model first attempt: 95% CI upper limit")
      ladd("model_glmm_laplace_clean", as.integer(at$clean[1]), "indicator", "1 if the Laplace attempt ended without a convergence message")
    }
  }
  # predicted probabilities at the exposure percentiles
  for (nm in names(pcts$points)) {
    z <- pcts$points[[nm]]
    ladd(paste0("model_pred_", nm), z$p, "probability", paste0("Predicted in-network denial probability (average marginal) at the ", nm, " percentile of log2 density"))
    ladd(paste0("model_pred_", nm, "_lo"), z$lo, "probability", paste0("Lower 95% limit at the ", nm, " percentile"))
    ladd(paste0("model_pred_", nm, "_hi"), z$hi, "probability", paste0("Upper 95% limit at the ", nm, " percentile"))
    ladd(paste0("model_density_", nm), z$density, "surgeons per 10,000 reported enrollees", paste0("Primary density at the ", nm, " percentile, analytic sample"))
  }
  ladd("model_pred_diff_p95_p05", pcts$diff[["diff"]], "probability", "Predicted probability at the 95th minus the 5th percentile of density (absolute difference)")
  ladd("model_pred_diff_p95_p05_lo", pcts$diff[["lo"]], "probability", "Lower 95% limit of that difference (delta method)")
  ladd("model_pred_diff_p95_p05_hi", pcts$diff[["hi"]], "probability", "Upper 95% limit of that difference (delta method)")
  ladd("model_fig3_bins", nrow(bins), "bins", "Equal-count bins of log2 density in Fig 3")
  ladd("model_fig3_bin_size", bins$n_plans[1], "plans", "Plans in each of the first bins of Fig 3")
  ladd("model_fig3_bin_last", bins$n_plans[nrow(bins)], "plans", "Plans in the last bin of Fig 3 (receives the remainder)")
  ladd("model_fig3_grid_points", nrow(curve[curve$curve == "orthopaedic", ]), "points", "Grid points of each Fig 3 curve")
  # cohort flow: rungs
  ov <- flow[flow$dimension == "overall", ]
  for (i in seq_len(nrow(ov))) {
    r <- ov[i, ]; k <- paste0("cohort_flow_", r$rung_code)
    inc <- r$rung_code == "issuer_incomplete"
    ladd(paste0(k, "_plans"), r$n_plans, "plans", paste0(r$rung_label, ": plans"))
    ladd(paste0(k, "_issuers"), r$n_issuers, "issuers", paste0(r$rung_label, ": issuers"))
    ladd(paste0(k, "_states"), r$n_states, "states", paste0(r$rung_label, ": states"))
    ladd(paste0(k, "_claims"), r$claim_lines_received, "claim lines", paste0(r$rung_label, ": in-network claim lines received"))
    ladd(paste0(k, "_claims_share"), r$share_of_cohort_claim_lines, "proportion",
         if (inc) sprintf("Share of the cohort's in-network claim lines held by the %d incomplete-directory plans (BCBS Florida), %.1f percent; not a denial rate", r$n_plans, 100 * r$share_of_cohort_claim_lines)
         else paste0(r$rung_label, ": share of the cohort's in-network claim lines received"))
    ladd(paste0(k, "_plans_share"), r$share_of_cohort_plans, "proportion", paste0(r$rung_label, ": share of the cohort's plans"))
    ladd(paste0(k, "_denial"), r$denial_proportion, "proportion",
         if (inc) sprintf("Claim-weighted in-network denial proportion of the %d incomplete-directory plans (BCBS Florida)", r$n_plans)
         else paste0(r$rung_label, ": claim-weighted in-network denial proportion"))
    ladd(paste0(k, "_denial_median"), r$plan_median_denial_proportion, "proportion", paste0(r$rung_label, ": median plan-level in-network denial proportion"))
    ladd(paste0(k, "_claims_median"), r$median_claim_lines_per_plan, "claim lines", paste0(r$rung_label, ": median in-network claim lines received per plan"))
  }
  inc_cl <- ov$claim_lines_received[ov$rung_code == "issuer_incomplete"]; exc_cl <- ov$claim_lines_received[ov$rung_code == "excluded_not_analytic"]
  ladd("cohort_flow_issuer_incomplete_share_of_excluded_claims", inc_cl / exc_cl, "proportion",
       sprintf("Share of the in-network claim lines of all %d excluded cohort plans (the selection-check comparison group) held by the %d incomplete-directory plans (BCBS Florida), %.1f percent; not a denial rate",
               ov$n_plans[ov$rung_code == "excluded_not_analytic"], ov$n_plans[ov$rung_code == "issuer_incomplete"], 100 * inc_cl / exc_cl))
  # selection check (SAP 2.6): differences
  for (nm in names(sel)) ladd(paste0("cohort_flow_sel_", nm), sel[[nm]], if (grepl("ratio", nm)) "ratio" else "proportion",
                              paste0("Selection check (descriptive): ", gsub("_", " ", nm)))
  # Table 1 strata
  for (i in seq_len(nrow(tab1))) {
    r <- tab1[i, ]
    if (r$dimension == "overall") next
    k <- lkey("cohort_flow", if (r$dimension == "plan_type") "type" else "metal", r$level)
    unit <- list(plans_cohort = "plans", plans_exposure_ok = "plans", plans_analytic = "plans", issuers_cohort = "issuers",
                 issuers_analytic = "issuers", claim_lines_cohort = "claim lines", claim_lines_analytic = "claim lines",
                 share_of_analytic_plans = "proportion", density_q25 = "density", density_median = "density", density_q75 = "density",
                 surgeon_count_median = "surgeons", enrollment_q25 = "enrollees", enrollment_median = "enrollees", enrollment_q75 = "enrollees")
    for (m in names(unit)) ladd(paste0(k, "_", m), r[[m]], unit[[m]], paste0("Table 1, ", r$dimension, " ", r$level, ": ", gsub("_", " ", m)))
  }
  r <- tab1[tab1$dimension == "overall", ]
  for (m in c("density_q25", "density_median", "density_q75", "surgeon_count_median", "enrollment_q25", "enrollment_median", "enrollment_q75")) {
    ladd(paste0("cohort_flow_analytic_", m), r[[m]], "value", paste0("Analytic sample: ", gsub("_", " ", m)))
  }
}

# ---- main ---------------------------------------------------------------------------------------
main <- function() {
  set.seed(SEED)
  dir.create(dirname(LOGFILE), recursive = TRUE, showWarnings = FALSE)
  lg("06_models.R start (WP6b). R ", R.version.string, " ; sandwich ", as.character(utils::packageVersion("sandwich")),
     " ; lme4 ", as.character(utils::packageVersion("lme4")), " ; data.table ", as.character(utils::packageVersion("data.table")),
     " ; platform ", R.version$platform)
  h <- check_sap_hash()
  lg("SAP hash check PASSED for ", paste(h$file, collapse = ", "), " (sap_frozen.md ", substr(h$sha256[1], 1, 8), ")")
  assert_ascii_source(file.path(P$R, "06_models.R"))
  ledger_reset("models")

  inp <- read_inputs()
  D <- build_cohort(inp)
  ch <- check_design(D)
  for (i in seq_len(nrow(ch$table))) lg("design check ", ch$table$item[i], ": computed ", format(ch$table$computed[i], digits = 10),
                                       " frozen ", format(ch$table$frozen[i], digits = 10), if (ch$table$match[i]) " ok" else " MISMATCH")
  if (!all(ch$table$match)) stop("BLOCKED: the analytic data disagree with the frozen design constants: ",
                                 paste(ch$table$item[!ch$table$match], collapse = ", "), call. = FALSE)
  if (!ch$zero_ok) stop("BLOCKED: an exact zero in the primary or dermatology exposure counts; the SAP says there is none", call. = FALSE)
  bound_funnel <- build_outcome_bound_plans(inp, D)
  ADD <- bound_funnel$add
  lg("outcome suppression (SAP 9.4): ", paste(names(bound_funnel$funnel), bound_funnel$funnel, sep = " = ", collapse = "; "),
     " ; rows in ADD: ", nrow(ADD))

  # analytic data (written before the models so the models can be reproduced from the file)
  keep <- c("plan_id", "issuer_id", "state", "state_issuer", "plan_type", "metal_level", "metal3", "exposure_status",
            "enrollment_marker", "enrollment", "received", "denied", "n_individual_npis", "n_ortho_surgeon_text", "n_ortho_apc_text",
            "n_derm_text", "n_ophth_text", "expo_ortho", "expo_apc", "expo_derm", "expo_ophth", "expo_share", "lenroll",
            "in_exposure_ok", "in_analytic", "in_enrollment_star2_ok", "informative_issuer", "issuer_in_network_claims")
  fwrite(D[, keep], file.path(P$results, "model_analytic_data.csv"), na = "")
  fwrite(ADD, file.path(P$results, "model_outcome_bound_plans.csv"), na = "")
  lg("wrote model_analytic_data.csv (", nrow(D), " rows) and model_outcome_bound_plans.csv (", nrow(ADD), " rows)")

  # ---- the models (primary first inside run_models) ----
  M <- run_models(D, ADD)
  E <- M$est
  for (i in seq_len(nrow(E))) {
    r <- E[i, ]
    lg(sprintf("%-8s OR %.4f (%.4f to %.4f) P %.4g n %d issuers %d clusters %d converged %s | %s", r$row_code,
               r$odds_ratio_per_doubling, r$ci_low, r$ci_high, r$p_value, r$n_plans, r$n_issuers, r$n_clusters, r$converged,
               substr(r$notes, 1, 160)))
  }
  glmm <- M$res$sens_c
  if (isTRUE(glmm$ok)) {
    lg("GLMM: attempts: ", paste(glmm$log, collapse = " ; "))
    at <- glmm$attempt_table
    for (i in seq_len(nrow(at))) {
      sa <- fit_stats(at$beta[i], at$se[i], "z")
      lg("GLMM attempt ", at$attempt[i], ": clean ", at$clean[i], " ; OR per doubling ", sprintf("%.4f", sa[["or"]]), " (", sprintf("%.4f", sa[["lo"]]), " to ", sprintf("%.4f", sa[["hi"]]), ")")
    }
    lg("GLMM: reported attempt ", glmm$attempt, " nAGQ ", glmm$nAGQ, " optimizer ", glmm$optimizer, " opt code ", glmm$opt_code,
       " singular ", glmm$singular, " converged(clean) ", glmm$converged, " messages: ", glmm$messages,
       " ; variances state ", signif(glmm$var_state, 4), " issuer ", signif(glmm$var_issuer, 4), " plan ", signif(glmm$var_plan, 4))
  } else lg("GLMM FAILED in every attempt: ", glmm$message, level = "WARN")
  if (!isTRUE(glmm$converged) && isTRUE(glmm$ok)) {
    add_deviation("sens_c mixed model convergence", "SAP 8.1: lme4::glmer, use nAGQ = 0 if needed",
                  paste0("reported the first available fit; it did not end clean: ", glmm$messages), "no attempt in the predeclared ladder ended without a convergence message")
  }

  # negative-control reading rule (SAP 6.2, applied only if the primary CI excludes 1)
  pr <- E[E$row_code == "primary", ]
  excl <- pr$ci_low > 1 || pr$ci_high < 1
  ctrl <- list()
  for (cc in list(c("derm", "sens_g"), c("ophth", "sens_h"))) {
    r <- E[E$row_code == cc[2], ]
    ratio <- r$log_odds / pr$log_odds
    same <- sign(r$log_odds) == sign(pr$log_odds)
    half <- same && abs(r$log_odds) >= dnum("design_control_reading_ratio") * abs(pr$log_odds)
    reading <- if (!excl) "rule not applied" else if (half) "not specific" else "not reproduced"
    ctrl[[cc[1]]] <- list(ratio = ratio, same_sign = same, at_least_half = half, reading = reading)
    lg("control ", cc[1], ": log-odds ratio to primary ", sprintf("%.3f", ratio), " same sign ", same, " at least half ", half,
       " ; primary CI excludes 1: ", excl, " ; reading: ", reading)
  }

  # ---- Fig 3 data ----
  A <- M$A
  fo_p <- M$res$primary
  q <- stats::quantile(A$expo_ortho, c(dnum("design_fig3_lower_quantile"), dnum("design_fig3_upper_quantile")), type = 7, names = FALSE)
  grid <- seq(q[1], q[2], length.out = 101)
  cur_o <- cbind(curve = "orthopaedic", amp_curve(fo_p, grid), stringsAsFactors = FALSE)
  fo_g <- M$res$sens_g
  qd <- stats::quantile(A$expo_derm, c(dnum("design_fig3_lower_quantile"), dnum("design_fig3_upper_quantile")), type = 7, names = FALSE)
  cur_d <- cbind(curve = "control", amp_curve(fo_g, seq(qd[1], qd[2], length.out = 101)), stringsAsFactors = FALSE)
  curve <- rbind(cur_o, cur_d)
  curve$density_per_10000 <- 2^curve$exposure_log2
  curve <- curve[, c("curve", "exposure_log2", "density_per_10000", "predicted_probability", "se", "ci_low", "ci_high")]
  bins <- make_bins(A)
  fwrite(curve, file.path(P$results, "model_fig3_curve.csv"), na = "")
  fwrite(bins, file.path(P$results, "model_fig3_bins.csv"), na = "")
  pq <- stats::quantile(A$expo_ortho, c(0.05, 0.25, 0.5, 0.75, 0.95), type = 7, names = FALSE)
  names(pq) <- c("p05", "p25", "p50", "p75", "p95")
  pts <- lapply(names(pq), function(nm) { z <- amp_point(fo_p, pq[[nm]]); list(p = z$p, lo = z$p - CRIT_Z * z$se, hi = z$p + CRIT_Z * z$se, density = 2^pq[[nm]]) })
  names(pts) <- names(pq)
  pcts <- list(points = pts, diff = amp_diff(fo_p, pq[["p05"]], pq[["p95"]]))
  lg("Fig 3: grid ", sprintf("%.3f", q[1]), " to ", sprintf("%.3f", q[2]), " (log2); predicted probability at p05 ", sprintf("%.4f", pts$p05$p),
     ", p50 ", sprintf("%.4f", pts$p50$p), ", p95 ", sprintf("%.4f", pts$p95$p), " ; difference p95 - p05 ", sprintf("%.4f", pcts$diff[["diff"]]),
     " (", sprintf("%.4f", pcts$diff[["lo"]]), " to ", sprintf("%.4f", pcts$diff[["hi"]]), ")")
  lg("Fig 3 bins: ", paste(bins$n_plans, collapse = ", "), " plans per bin")

  # ---- cohort flow, selection check, Table 1 ----
  flow <- build_cohort_flow(D)
  ov <- flow[flow$dimension == "overall", ]
  g <- function(code, col) ov[[col]][ov$rung_code == code]
  sel <- list(
    analytic_minus_excluded_denial = g("analytic", "denial_proportion") - g("excluded_not_analytic", "denial_proportion"),
    analytic_minus_excluded_denial_median = g("analytic", "plan_median_denial_proportion") - g("excluded_not_analytic", "plan_median_denial_proportion"),
    ok_minus_unreachable_denial = g("exposure_ok", "denial_proportion") - g("issuer_unreachable", "denial_proportion"),
    ok_minus_unreachable_denial_median = g("exposure_ok", "plan_median_denial_proportion") - g("issuer_unreachable", "plan_median_denial_proportion"),
    analytic_to_excluded_claims_median_ratio = g("analytic", "median_claim_lines_per_plan") / g("excluded_not_analytic", "median_claim_lines_per_plan"),
    analytic_to_excluded_claims_total_ratio = g("analytic", "claim_lines_received") / g("excluded_not_analytic", "claim_lines_received"),
    ok_to_unreachable_claims_median_ratio = g("exposure_ok", "median_claim_lines_per_plan") / g("issuer_unreachable", "median_claim_lines_per_plan")
  )
  fwrite(flow, file.path(P$results, "model_cohort_flow.csv"), na = "")
  tab1 <- build_table1(D)
  fwrite(tab1, file.path(P$results, "model_table1_data.csv"), na = "")
  lad_codes <- c("cohort", "excluded_exposure_status", "issuer_unreachable", "issuer_incomplete", "plan_not_in_directory", "no_individuals",
                 "exposure_ok", "lost_complete_case", "analytic")
  lad <- ov[match(lad_codes, ov$rung_code), c("rung_code", "rung_label", "n_plans", "n_issuers", "n_states", "claim_lines_received",
                                              "share_of_cohort_plans", "share_of_cohort_claim_lines")]
  lad <- cbind(row_order = seq_along(lad_codes), indent = c(0, 0, 1, 1, 1, 1, 0, 1, 0), lad)
  fwrite(lad, file.path(P$results, "model_table1_ladder.csv"), na = "")
  for (code in names(RUNG_LABELS)) {
    lg(sprintf("ladder %-26s plans %5.0f issuers %4.0f states %3.0f claim lines %12.0f denial %.4f (plan median %.4f)", code, g(code, "n_plans"),
               g(code, "n_issuers"), g(code, "n_states"), g(code, "claim_lines_received"), g(code, "denial_proportion"),
               g(code, "plan_median_denial_proportion")))
  }
  lg("selection check: analytic minus excluded claim-weighted denial ", sprintf("%.4f", sel$analytic_minus_excluded_denial),
     " ; ok minus unreachable ", sprintf("%.4f", sel$ok_minus_unreachable_denial))

  # ---- estimate files ----
  fwrite(E, file.path(P$results, "model_estimates.csv"), na = "")
  loo <- M$loo
  fwrite(loo, file.path(P$results, "model_sens_leave_one_out.csv"), na = "")
  okl <- loo[!is.na(loo$odds_ratio_per_doubling), ]
  sens <- E[, c("row_code", "label", "odds_ratio_per_doubling", "ci_low", "ci_high", "n_plans", "n_issuers", "p_value", "p_holm", "converged", "notes")]
  sens$table <- ifelse(sens$row_code == "primary", "Table 3 (primary row)", ifelse(sens$row_code %in% TABLE3_CODES, "Table 3", "Supplement"))
  sens$n_plans_added <- vapply(sens$row_code, function(cd) { z <- M$res[[cd]]$n_added; if (is.null(z)) NA_real_ else z }, numeric(1))
  sens$leave_out_or_min <- NA_real_; sens$leave_out_or_max <- NA_real_; sens$leave_out_issuer_min <- NA_character_
  sens$leave_out_issuer_max <- NA_character_; sens$leave_outs <- NA_real_; sens$leave_outs_excluding_one <- NA_real_
  i_s <- sens$row_code == "sens_s"
  sens$leave_out_or_min[i_s] <- min(okl$odds_ratio_per_doubling); sens$leave_out_or_max[i_s] <- max(okl$odds_ratio_per_doubling)
  sens$leave_out_issuer_min[i_s] <- okl$issuer_id[which.min(okl$odds_ratio_per_doubling)]
  sens$leave_out_issuer_max[i_s] <- okl$issuer_id[which.max(okl$odds_ratio_per_doubling)]
  sens$leave_outs[i_s] <- nrow(loo); sens$leave_outs_excluding_one[i_s] <- sum(okl$excludes_one)
  sens$order <- seq_len(nrow(sens))
  sens <- sens[, c("order", "table", "row_code", "label", "odds_ratio_per_doubling", "ci_low", "ci_high", "n_plans", "n_issuers",
                   "p_value", "p_holm", "n_plans_added", "converged", "leave_out_or_min", "leave_out_or_max", "leave_out_issuer_min",
                   "leave_out_issuer_max", "leave_outs", "leave_outs_excluding_one", "notes")]
  fwrite(sens, file.path(P$results, "model_sensitivity_rows.csv"), na = "")

  # ---- deviations and ledger ----
  dev <- if (length(DEVIATIONS)) do.call(rbind, DEVIATIONS) else data.frame(item = character(), sap_text = character(), what_was_done = character(), reason = character())
  fwrite(dev, file.path(P$results, "model_deviations.csv"), na = "")
  lg("deviations logged: ", nrow(dev))
  fill_ledger(M, flow, tab1, ADD, curve, bins, ctrl, pcts, sel, bound_funnel$funnel, glmm)
  if (length(LADD_SKIPPED)) lg("ledger keys skipped because the value was missing: ", paste(LADD_SKIPPED, collapse = ", "), level = "WARN")
  ledger_write("models")
  lg("06_models.R done")
  invisible(list(M = M, D = D, ADD = ADD))
}

if (!isTRUE(getOption("psd_models_no_run"))) invisible(main())
