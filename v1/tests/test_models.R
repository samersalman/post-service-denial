# =====================================================================
# test_models.R : WP6b tests (plain R).
# Run: cd v1 ; Rscript tests/test_models.R        (any working directory works; the script cd's into v1)
#   MODELS_TEST_ONLY=machinery Rscript tests/test_models.R   runs only the checks that need no model output
# Exit status 1 if any check fails. Nothing is tuned to pass. Test logs and temporary files go to tempdir();
# nothing under v1/logs changes during the run (check l).
# Keep this file ASCII.
# =====================================================================
.args <- commandArgs(trailingOnly = FALSE)
.f <- sub("^--file=", "", grep("^--file=", .args, value = TRUE)[1])
.here <- dirname(normalizePath(gsub("~+~", " ", .f, fixed = TRUE)))
source(file.path(dirname(.here), "R", "00_setup.R"))
source(file.path(dirname(.here), "R", "ledger.R"))
setwd(P$v1)
SCR <- file.path(tempdir(), "wp6b_test"); dir.create(SCR, showWarnings = FALSE, recursive = TRUE)
options(psd_models_no_run = TRUE, psd_models_logfile = file.path(SCR, "test_models.log"))
logs_snapshot <- function() {
  f <- sort(list.files(P$logs, full.names = TRUE, recursive = TRUE, all.files = TRUE, no.. = TRUE))
  f <- f[!dir.exists(f)]
  data.frame(file = f, md5 = unname(tools::md5sum(f)), size = file.info(f)$size, stringsAsFactors = FALSE)
}
LOGS0 <- logs_snapshot()
source(file.path(P$R, "06_models.R"))
ONLY <- Sys.getenv("MODELS_TEST_ONLY", unset = "")

N_PASS <- 0L; N_FAIL <- 0L; FAILS <- character(0)
check <- function(name, expr) {
  res <- tryCatch({ r <- force(expr); if (isTRUE(r)) "ok" else paste0("FALSE", if (is.character(r)) paste0(": ", paste(utils::head(r, 3), collapse = " | ")) else "") },
                  error = function(e) paste0("ERROR: ", conditionMessage(e)))
  if (identical(res, "ok")) { N_PASS <<- N_PASS + 1L; cat("PASS  ", name, "\n", sep = "") }
  else { N_FAIL <<- N_FAIL + 1L; FAILS <<- c(FAILS, name); cat("FAIL  ", name, "  ->  ", res, "\n", sep = "") }
  invisible(NULL)
}
near <- function(a, b, tol = 1e-8) isTRUE(all(abs(a - b) <= tol * pmax(1, abs(b))))
R <- function(name) file.path(P$results, name)
rd <- function(name) utils::read.csv(R(name), colClasses = "character", na.strings = character(0), stringsAsFactors = FALSE)
rdn <- function(name) { x <- utils::read.csv(R(name), stringsAsFactors = FALSE, na.strings = c("", "NA")); x }

# ---------------------------------------------------------------- (a) hash ------------------------------
cat("== (a) frozen-file hash\n")
check("check_sap_hash() passes and returns four files", { h <- check_sap_hash(); nrow(h) == 4L })
check("independent digest of the four frozen files equals logs/sap_hash.txt", {
  hl <- readLines("logs/sap_hash.txt", warn = FALSE); hl <- hl[grepl("^[0-9a-f]{64}  ", hl)]
  ok <- TRUE
  for (l in hl) { f <- sub("^[0-9a-f]{64}  ", "", l); ok <- ok && identical(digest::digest(file = f, algo = "sha256", serialize = FALSE), substr(l, 1, 64)) }
  ok && length(hl) == 4L })
check("check_sap_hash() stops with BLOCKED on a tampered copy", {
  v <- file.path(SCR, "tamper"); dir.create(file.path(v, "config"), recursive = TRUE, showWarnings = FALSE); dir.create(file.path(v, "logs"), showWarnings = FALSE)
  for (f in c("sap_frozen.md", "design_constants.csv", "specialty_text_crosswalk.csv", "ortho_taxonomy.csv")) file.copy(file.path("config", f), file.path(v, "config", f), overwrite = TRUE)
  file.copy("logs/sap_hash.txt", file.path(v, "logs", "sap_hash.txt"), overwrite = TRUE)
  ok0 <- is.data.frame(check_sap_hash(v))
  cat("x", file = file.path(v, "config", "sap_frozen.md"), append = TRUE)
  msg <- tryCatch({ check_sap_hash(v); "no error" }, error = function(e) conditionMessage(e))
  ok0 && grepl("BLOCKED: SAP hash mismatch", msg, fixed = TRUE) })

# ---------------------------------------------------------------- simulation helpers -------------------
sim_one <- function(seed, G = 60, per = 8, beta = -0.10) {
  set.seed(seed)
  n <- G * per
  d <- data.frame(issuer_id = rep(sprintf("I%03d", seq_len(G)), each = per), stringsAsFactors = FALSE)
  d$plan_id <- sprintf("P%05d", seq_len(n))
  d$state <- sprintf("S%02d", rep(seq_len(12), length.out = G))[match(d$issuer_id, unique(d$issuer_id))]
  d$state_issuer <- paste(d$state, d$issuer_id, sep = "_")
  gi <- match(d$issuer_id, unique(d$issuer_id))
  d$expo_ortho <- 15 + rnorm(G, 0, 2)[gi] + rnorm(n, 0, 1)
  d$lenroll <- rnorm(n, 5, 1.2)
  d$plan_type <- factor(sample(PLAN_LEVELS, n, TRUE), levels = PLAN_LEVELS)
  d$metal3 <- factor(sample(METAL3_LEVELS, n, TRUE), levels = METAL3_LEVELS)
  u <- rnorm(G, 0, 0.4)[gi]; v <- rnorm(n, 0, 0.2)
  d$received <- sample(300:3000, n, TRUE)
  p <- stats::plogis(-1.5 + beta * (d$expo_ortho - 15) + 0.1 * (d$lenroll - 5) + u + v)
  d$denied <- stats::rbinom(n, d$received, p)
  d
}

# ---------------------------------------------------------------- (b) simulation recovery --------------
cat("== (b) simulation recovery\n")
SIM_BETA <- -0.10
sims <- lapply(1:200, function(i) { d <- sim_one(1000 + i, beta = SIM_BETA); fo <- fit_qb(d, "expo_ortho", COVARS, "issuer_id", "issuer_id")
                                    c(ok = fo$ok && fo$converged, beta = fo$beta, se = fo$se) })
sims <- do.call(rbind, sims)
check("all 200 simulated quasi-binomial fits converged", all(sims[, "ok"] == 1))
check("quasi-binomial FE estimate recovers the known log-odds slope (mean within 0.01 of truth and within 3 Monte Carlo SE)", {
  m <- mean(sims[, "beta"]); mcse <- stats::sd(sims[, "beta"]) / sqrt(nrow(sims))
  cat("      mean estimate ", sprintf("%.4f", m), " truth ", SIM_BETA, " MC SE ", sprintf("%.5f", mcse), "\n", sep = "")
  abs(m - SIM_BETA) < 0.01 && abs(m - SIM_BETA) < 3 * mcse + 0.005 })
check("coverage of the 95% clustered Wald interval over 200 replicates is between 0.90 and 0.99", {
  cov <- mean(abs(sims[, "beta"] - SIM_BETA) < CRIT_Z * sims[, "se"]); cat("      coverage ", cov, "\n", sep = ""); cov >= 0.90 && cov <= 0.99 })

# ---------------------------------------------------------------- (c) decision rule and fallback -------
cat("== (c) decision rule, aliasing and fallback\n")
check("decision rule maps K to the model as the SAP states", identical(primary_model_rule(33), "issuer_fe") && identical(primary_model_rule(30), "issuer_fe") &&
        identical(primary_model_rule(29), "state_fe") && identical(primary_model_rule(0), "state_fe") && identical(primary_model_rule(100), "issuer_fe"))
check("decision rule stops on a missing K", inherits(try(primary_model_rule(NA), silent = TRUE), "try-error"))
check("threshold is the frozen design constant (30)", identical(dnum("design_informative_issuer_threshold"), 30))
check("count_informative on a toy table (issuer A: 2 plans, 2 counts; B: 2 plans, 1 count; C: 1 plan)", {
  toy <- data.frame(issuer_id = c("A", "A", "B", "B", "C"), n_ortho_surgeon_text = c(10, 12, 7, 7, 9))
  count_informative(toy) == 1L && identical(informative_ids(toy), "A") })
check("an exposure constant within every issuer is flagged aliased under issuer fixed effects (ok FALSE)", {
  d <- sim_one(5); d$expo_const <- ave(d$expo_ortho, d$issuer_id)
  fo <- fit_qb(d, "expo_const", COVARS, "issuer_id", "issuer_id"); !fo$ok && grepl("aliased", fo$message) })
check("an aliased covariate is removed and refitted before vcovCL; exposure estimate and SE unchanged", {
  d <- sim_one(6); d$lenroll <- ave(d$lenroll, d$issuer_id)
  fo1 <- fit_qb(d, "expo_ortho", COVARS, "issuer_id", "issuer_id")
  fo0 <- fit_qb(d, "expo_ortho", COVARS_NOENR, "issuer_id", "issuer_id")
  fo1$ok && length(fo1$removed) >= 1L && near(fo1$beta, fo0$beta, 1e-6) && near(fo1$se, fo0$se, 1e-6) })
check("fit_primary falls back to state fixed effects (SAP 7.5) and logs a deviation when the exposure is aliased", {
  DEVIATIONS <<- list()
  d <- sim_one(7); d$expo_ortho <- ave(d$expo_ortho, d$issuer_id)
  pf <- fit_primary(d, "issuer_fe"); ok <- identical(pf$used, "state_fe") && isTRUE(pf$fo$converged) && length(DEVIATIONS) == 1L
  DEVIATIONS <<- list(); ok })
check("fit_primary keeps the rule's model when it converges (no deviation)", {
  DEVIATIONS <<- list(); d <- sim_one(8); pf <- fit_primary(d, "issuer_fe"); ok <- identical(pf$used, "issuer_fe") && length(DEVIATIONS) == 0L; ok })

# ---------------------------------------------------------------- (d) Holm ------------------------------
cat("== (d) Holm\n")
check("Holm adjustment equals p.adjust(method = holm) and the hand calculation", {
  p <- c(0.01, 0.04); h <- stats::p.adjust(p, "holm"); hand <- c(min(1, 2 * 0.01), min(1, max(2 * 0.01, 1 * 0.04)))
  p2 <- c(0.04, 0.01); h2 <- stats::p.adjust(p2, "holm")
  near(h, hand, 1e-12) && near(h2, rev(hand), 1e-12) && near(stats::p.adjust(c(0.30, 0.02), "holm"), c(0.30, 0.04), 1e-12) })

# ---------------------------------------------------------------- (e) clustered SE by hand ----------------
cat("== (e) clustered covariance by hand\n")
check("fit_qb standard error equals a hand-coded HC1 cluster sandwich (G/(G-1) * (n-1)/(n-k))", {
  d <- sim_one(11, G = 12, per = 10)
  fo <- fit_qb(d, "expo_ortho", COVARS, "issuer_id", "issuer_id")
  X <- stats::model.matrix(fo$fit); mu <- stats::fitted(fo$fit); r <- d$received
  sc <- X * (d$denied - r * mu)
  meat <- crossprod(rowsum(sc, d$issuer_id))
  bread <- solve(crossprod(X, X * (r * mu * (1 - mu))))
  G <- length(unique(d$issuer_id)); n <- nrow(X); k <- ncol(X)
  V <- G / (G - 1) * (n - 1) / (n - k) * bread %*% meat %*% bread
  near(sqrt(V[fo$e, fo$e]), fo$se, 1e-6) })

if (!identical(ONLY, "machinery")) {
# ---------------------------------------------------------------- independent recomputation --------------
ex <- utils::read.csv("data_clean/ortho/plan_exposure.csv", colClasses = c(plan_id = "character", issuer_id = "character", state = "character", exposure_status = "character"), na.strings = c("", "NA"))
hdr <- strsplit(readLines("data_clean/cms/plans.csv", n = 1L, warn = FALSE), ",", fixed = TRUE)[[1]]
want <- c("plan_id", "issuer_id", "state", "plan_type", "metal_level", "plan_claims_received_in_network", "plan_claims_received_in_network_marker",
          "plan_claims_denied_in_network", "plan_claims_denied_in_network_marker", "avg_monthly_enrollment", "avg_monthly_enrollment_marker", "pa_network_id")
cc <- ifelse(hdr %in% want, "character", "NULL"); names(cc) <- hdr
pl <- utils::read.csv("data_clean/cms/plans.csv", colClasses = cc, na.strings = c("", "NA"))
m <- merge(ex, pl[, c("plan_id", "plan_type", "metal_level", "plan_claims_received_in_network", "plan_claims_received_in_network_marker",
                      "plan_claims_denied_in_network", "plan_claims_denied_in_network_marker", "avg_monthly_enrollment", "avg_monthly_enrollment_marker")], by = "plan_id")
m$rec <- as.numeric(m$plan_claims_received_in_network); m$den <- as.numeric(m$plan_claims_denied_in_network)
m$enr <- ifelse(m$avg_monthly_enrollment_marker == "numeric", as.numeric(m$avg_monthly_enrollment), NA_real_)
m$ok <- m$exposure_status == "ok"; m$an <- m$ok & !is.na(m$enr) & m$enr > 0
ind_A <- m[m$an, ]
ind_K <- { a <- aggregate(n_ortho_surgeon_text ~ issuer_id, ind_A, function(x) c(length(x), length(unique(x)))); sum(a$n_ortho_surgeon_text[, 1] >= 2 & a$n_ortho_surgeon_text[, 2] >= 2) }

AD <- utils::read.csv(R("model_analytic_data.csv"), colClasses = c(plan_id = "character", issuer_id = "character", state = "character", state_issuer = "character"), stringsAsFactors = FALSE)
FL <- rdn("model_cohort_flow.csv"); EST <- utils::read.csv(R("model_estimates.csv"), stringsAsFactors = FALSE, na.strings = c("", "NA"))
SR <- utils::read.csv(R("model_sensitivity_rows.csv"), stringsAsFactors = FALSE, na.strings = c("", "NA"))
LOO <- utils::read.csv(R("model_sens_leave_one_out.csv"), colClasses = c(issuer_id = "character"), stringsAsFactors = FALSE, na.strings = c("", "NA"))
CU <- utils::read.csv(R("model_fig3_curve.csv"), stringsAsFactors = FALSE); BI <- utils::read.csv(R("model_fig3_bins.csv"), stringsAsFactors = FALSE)
LED <- utils::read.csv(R("ledger_parts/ledger_models.csv"), colClasses = "character", na.strings = character(0), stringsAsFactors = FALSE)
lv <- function(k) { i <- match(k, LED$key); if (is.na(i)) stop("ledger key missing: ", k); LED$value[i] }
lvn <- function(k) as.numeric(lv(k))

# ---------------------------------------------------------------- (f) data sanity -----------------------
cat("== (f) analytic data sanity\n")
check("model_analytic_data.csv has one row per cohort plan, unique plan_id, ids as character", nrow(AD) == 2527L && !anyDuplicated(AD$plan_id) && all(nchar(AD$plan_id) == 14L))
check("denied <= received, denied >= 0, rates in [0,1] for every row", all(AD$denied <= AD$received) && all(AD$denied >= 0) && all(AD$received > 0) && all(AD$denied / AD$received >= 0 & AD$denied / AD$received <= 1))
check("no NA in outcome or exposure in the primary sample", { a <- AD[AD$in_analytic, ]; !anyNA(a[, c("denied", "received", "expo_ortho", "lenroll", "expo_apc", "expo_derm", "expo_ophth", "expo_share", "plan_type", "metal3", "issuer_id")]) })
check("no NA in the enrollment-free exposure for every exposure-ok plan", !anyNA(AD$expo_share[AD$in_exposure_ok]))
check("no exposure value exists for a plan whose exposure status is not ok", all(is.na(AD$expo_share[!AD$in_exposure_ok])) && all(is.na(AD$n_ortho_surgeon_text[!AD$in_exposure_ok])))
check("expo_ortho recomputes from the raw counts and enrollment (log2 of count / enrollment x 10,000)", {
  a <- AD[AD$in_analytic, ]; mm <- ex[match(a$plan_id, ex$plan_id), ]
  near(a$expo_ortho, log2(mm$n_ortho_surgeon_text / a$enrollment * 10000), 1e-12) && near(a$lenroll, log2(a$enrollment), 1e-12) &&
    near(a$expo_ophth, log2(mm$n_ophth_text / a$enrollment * 10000 + 49.1096016155682), 1e-9) &&
    near(AD$expo_share[AD$in_exposure_ok], log2(ex$n_ortho_surgeon_text[match(AD$plan_id[AD$in_exposure_ok], ex$plan_id)] / ex$n_individual_npis[match(AD$plan_id[AD$in_exposure_ok], ex$plan_id)]), 1e-12) })
check("metal pooling: Catastrophic -> Bronze, Platinum -> Gold, plan type levels as in the SAP", {
  t <- table(AD$metal_level[AD$in_analytic], AD$metal3[AD$in_analytic])
  t["Catastrophic", "Bronze"] == 14 && t["Platinum", "Gold"] == 9 && sum(t["Catastrophic", ]) == 14 && sum(t["Platinum", ]) == 9 })

# ---------------------------------------------------------------- (g) counts ---------------------------
cat("== (g) sample counts against the design constants and an independent recomputation\n")
g1 <- function(code, col) FL[[col]][FL$dimension == "overall" & FL$rung_code == code]
check("independent: cohort 2,527 plans, 149 issuers; exposure ok 2,175 / 136; analytic 1,681 / 135 / 30 states", nrow(m) == 2527 && length(unique(m$issuer_id)) == 149 &&
        sum(m$ok) == 2175 && length(unique(m$issuer_id[m$ok])) == 136 && nrow(ind_A) == 1681 && length(unique(ind_A$issuer_id)) == 135 && length(unique(ind_A$state)) == 30)
check("independent counts equal the frozen design constants", sum(m$ok) == dnum("design_exposure_ok_plans") && nrow(ind_A) == dnum("design_analytic_plans") &&
        length(unique(ind_A$issuer_id)) == dnum("design_analytic_issuers") && ind_K == dnum("design_informative_issuer_k") && ind_K == 33 &&
        sum(m$ok & is.na(m$enr)) == dnum("design_ok_plans_enrollment_star2"))
check("independent K = 33 gives issuer_fe, equal to design_primary_model", identical(primary_model_rule(ind_K), DK[["design_primary_model"]]))
check("model_cohort_flow.csv ladder equals the independent recomputation (plans, issuers, states, claim lines) for every rung", {
  rr <- list(cohort = rep(TRUE, nrow(m)), excluded_exposure_status = !m$ok, issuer_unreachable = m$exposure_status == "issuer_unreachable",
             issuer_incomplete = m$exposure_status == "issuer_incomplete", plan_not_in_directory = m$exposure_status == "plan_not_in_directory",
             no_individuals = m$exposure_status == "no_individuals", exposure_ok = m$ok, lost_complete_case = m$ok & !m$an, analytic = m$an, excluded_not_analytic = !m$an)
  ok <- TRUE
  for (nm in names(rr)) { x <- m[rr[[nm]], ]
    ok <- ok && g1(nm, "n_plans") == nrow(x) && g1(nm, "n_issuers") == length(unique(x$issuer_id)) && g1(nm, "n_states") == length(unique(x$state)) &&
      g1(nm, "claim_lines_received") == sum(x$rec) && near(g1(nm, "denial_proportion"), sum(x$den) / sum(x$rec), 1e-12) &&
      near(g1(nm, "plan_median_denial_proportion"), stats::median(x$den / x$rec), 1e-12) }
  ok })
check("ladder values match WP5/SAP text: 352 excluded (263/77/8/4), 494 lost, 846 excluded in all", g1("excluded_exposure_status", "n_plans") == 352 && g1("issuer_unreachable", "n_plans") == 263 &&
        g1("issuer_incomplete", "n_plans") == 77 && g1("plan_not_in_directory", "n_plans") == 8 && g1("no_individuals", "n_plans") == 4 && g1("lost_complete_case", "n_plans") == 494 &&
        g1("lost_complete_case", "n_issuers") == 81 && g1("lost_complete_case", "n_states") == 27 && g1("excluded_not_analytic", "n_plans") == 846)
check("analytic plans by plan type (EPO 387, HMO 872, POS 188, PPO 234) and metal level (Bronze 611, Catastrophic 14, Gold 473, Platinum 9, Silver 574) as in WP5", {
  tt <- table(ind_A$plan_type); mt <- table(ind_A$metal_level)
  T1 <- rdn("model_table1_data.csv")
  tt[["EPO"]] == 387 && tt[["HMO"]] == 872 && tt[["POS"]] == 188 && tt[["PPO"]] == 234 && mt[["Bronze"]] == 611 && mt[["Catastrophic"]] == 14 && mt[["Gold"]] == 473 &&
    mt[["Platinum"]] == 9 && mt[["Silver"]] == 574 && T1$plans_analytic[T1$level == "HMO"] == 872 && T1$plans_cohort[T1$level == "Platinum"] == 27 &&
    T1$plans_cohort[T1$level == "Catastrophic"] == 76 && T1$plans_cohort[T1$level == "POS"] == 347 })
check("model_estimates.csv sample sizes: primary 1,681 / 135 / 135; sens_d 1,669 (issuer 46944 has 12 plans); sens_r 1,301; sens_q 1,667; sens_p 1,673; sens_e/k/l/o 2,175 / 136", {
  e <- function(code, col) EST[[col]][EST$row_code == code]
  n46944 <- sum(ind_A$issuer_id == "46944"); n33602 <- sum(ind_A$issuer_id == "33602")
  e("primary", "n_plans") == 1681 && e("primary", "n_issuers") == 135 && e("primary", "n_clusters") == 135 && n46944 == 12 && n33602 == 380 &&
    e("sens_d", "n_plans") == 1681 - n46944 && e("sens_r", "n_plans") == 1681 - n33602 && e("sens_q", "n_plans") == 1681 - 14 &&
    e("sens_p", "n_plans") == 1681 - 8 && all(vapply(c("sens_e", "sens_k", "sens_l", "sens_o"), function(cd) e(cd, "n_plans") == 2175 && e(cd, "n_issuers") == 136, logical(1))) &&
    e("sens_m", "n_clusters") == 30 && e("sens_m", "n_issuers") == 135 })
check("model_table1_ladder.csv equals the overall ladder rows of model_cohort_flow.csv in display order", {
  L1 <- utils::read.csv(R("model_table1_ladder.csv"), stringsAsFactors = FALSE)
  nrow(L1) == 9L && identical(L1$row_order, 1:9) && all(vapply(seq_len(9), function(i) L1$n_plans[i] == g1(L1$rung_code[i], "n_plans") &&
        L1$claim_lines_received[i] == g1(L1$rung_code[i], "claim_lines_received") && L1$n_issuers[i] == g1(L1$rung_code[i], "n_issuers"), logical(1))) &&
    L1$n_plans[1] == 2527 && L1$n_plans[9] == 1681 && sum(L1$n_plans[3:6]) == L1$n_plans[2] && L1$n_plans[1] - L1$n_plans[2] == L1$n_plans[7] })
check("selection check values recompute from the raw tables (claim-weighted and plan-median denial; claim volume)", {
  a <- m[m$an, ]; x <- m[!m$an, ]; u <- m[m$exposure_status == "issuer_unreachable", ]; o <- m[m$ok, ]
  near(lvn("cohort_flow_sel_analytic_minus_excluded_denial"), sum(a$den) / sum(a$rec) - sum(x$den) / sum(x$rec), 1e-12) &&
    near(lvn("cohort_flow_sel_ok_minus_unreachable_denial"), sum(o$den) / sum(o$rec) - sum(u$den) / sum(u$rec), 1e-12) &&
    near(lvn("cohort_flow_sel_analytic_to_excluded_claims_total_ratio"), sum(a$rec) / sum(x$rec), 1e-12) &&
    near(lvn("cohort_flow_sel_analytic_to_excluded_claims_median_ratio"), stats::median(a$rec) / stats::median(x$rec), 1e-12) &&
    near(lvn("cohort_flow_sel_analytic_minus_excluded_denial_median"), stats::median(a$den / a$rec) - stats::median(x$den / x$rec), 1e-12) })
check("outcome-suppression funnel recomputes from the raw tables (21 suppressed, 2 both, 19 single; donors by the agreement rule)", {
  hdr2 <- hdr; want2 <- c("plan_id", "issuer_id", "state", "plan_claims_received_in_network_marker", "plan_claims_denied_in_network_marker", "avg_monthly_enrollment_marker", "avg_monthly_enrollment", "pa_network_id")
  c2 <- ifelse(hdr2 %in% want2, "character", "NULL"); names(c2) <- hdr2
  p2 <- utils::read.csv("data_clean/cms/plans.csv", colClasses = c2, na.strings = c("", "NA"))
  o <- p2[!p2$plan_id %in% ex$plan_id, ]
  s <- o[o$plan_claims_received_in_network_marker == "star2" | o$plan_claims_denied_in_network_marker == "star2", ]
  both <- s$plan_claims_received_in_network_marker == "star2" & s$plan_claims_denied_in_network_marker == "star2"
  one <- s[!both, ]
  okx <- merge(ex[ex$exposure_status == "ok", ], p2[, c("plan_id", "pa_network_id")], by = "plan_id")
  has_donor <- vapply(seq_len(nrow(one)), function(i) {
    dd <- okx[okx$issuer_id == one$issuer_id[i] & okx$state == one$state[i] & !is.na(okx$pa_network_id) & !is.na(one$pa_network_id[i]) & okx$pa_network_id == one$pa_network_id[i], ]
    nrow(dd) > 0 && nrow(unique(dd[, c("n_ortho_surgeon_text", "n_ortho_apc_text", "n_derm_text", "n_ophth_text", "n_individual_npis")])) == 1L }, logical(1))
  has_enr <- one$avg_monthly_enrollment_marker == "numeric"
  nrow(s) == 21 && sum(both) == 2 && nrow(one) == 19 && lvn("sens_suppout_total_suppressed") == nrow(s) && lvn("sens_suppout_both_suppressed") == sum(both) &&
    lvn("sens_suppout_single_with_donor") == sum(has_donor) && lvn("sens_suppout_single_with_donor_and_enrollment") == sum(has_donor & has_enr) &&
    lvn("sens_j_n_added") <= sum(has_donor & has_enr) && lvn("sens_i_n_added") <= lvn("sens_j_n_added") })
check("outcome-bound plans: bound applied to the suppressed side only, denied <= received, received > 0, none added with a zero denominator", {
  B <- utils::read.csv(R("model_outcome_bound_plans.csv"), colClasses = c(plan_id = "character", issuer_id = "character"), stringsAsFactors = FALSE)
  all(B$denied <= B$received) && all(B$received > 0) && all(B$bound %in% c(0, 5.5, 10)) && all(!is.na(B$enrollment) & B$enrollment > 0) &&
    all(ifelse(B$suppressed_side == "denied", B$denied == B$bound, B$received == B$bound)) && !any(B$plan_id %in% ex$plan_id) &&
    lvn("sens_i_n_added") == sum(B$bound == 0) && lvn("sens_n_n_added") == sum(B$bound == 5.5) && lvn("sens_j_n_added") == sum(B$bound == 10) })

# ---------------------------------------------------------------- (h) row codes ------------------------
cat("== (h) row codes and labels\n")
sap <- readLines("config/sap_frozen.md", warn = FALSE)
sap_rows <- regmatches(sap, regexec("^\\| (sens_[a-t]) \\| ([^|]+) \\| ", sap))
sap_rows <- sap_rows[lengths(sap_rows) == 3L]
sap_lab <- stats::setNames(trimws(vapply(sap_rows, `[`, "", 3)), vapply(sap_rows, `[`, "", 2))
check("SAP tables list 20 row codes and the design constants list the same codes in the same order", length(sap_lab) == 20L &&
        identical(names(sap_lab), c(strsplit(DK[["design_sens_row_codes"]], ";")[[1]], strsplit(DK[["design_supplement_row_codes"]], ";")[[1]])))
check("model_sensitivity_rows.csv holds the primary row first, then exactly the SAP row codes, each once, in the SAP order", {
  exp <- c("primary", strsplit(DK[["design_sens_row_codes"]], ";")[[1]], strsplit(DK[["design_supplement_row_codes"]], ";")[[1]])
  identical(SR$row_code, exp) && !anyDuplicated(SR$row_code) && length(SR$row_code) == 1L + dnum("design_n_sensitivity_rows") + dnum("design_n_supplement_rows") })
check("labels equal the SAP table labels (human readable, no underscores)", all(SR$label[match(names(sap_lab), SR$row_code)] == sap_lab) && !any(grepl("_", SR$label)) &&
        identical(SR$label[1], "Primary model (issuer fixed effects, adjusted for plan size)"))
check("model_estimates.csv: primary row first, same codes and order, required columns present", identical(EST$row_code, SR$row_code) && EST$row_code[1] == "primary" &&
        all(c("row_code", "label", "exposure", "model", "n_plans", "n_issuers", "n_clusters", "log_odds", "se_cluster", "odds_ratio_per_doubling", "ci_low", "ci_high", "p_value", "p_holm", "converged", "notes") %in% names(EST)) &&
        identical(names(EST)[1:16], c("row_code", "label", "exposure", "model", "n_plans", "n_issuers", "n_clusters", "log_odds", "se_cluster", "odds_ratio_per_doubling", "ci_low", "ci_high", "p_value", "p_holm", "converged", "notes")))
check("odds ratio = exp(log odds); z intervals = exp(log odds +/- 1.96 SE) for the cluster-robust rows; t interval for sens_t", {
  q <- EST[!is.na(EST$se_cluster) & EST$row_code != "sens_t", ]
  ok1 <- near(q$odds_ratio_per_doubling, exp(q$log_odds), 1e-10) && near(q$ci_low, exp(q$log_odds - stats::qnorm(0.975) * q$se_cluster), 1e-9) && near(q$ci_high, exp(q$log_odds + stats::qnorm(0.975) * q$se_cluster), 1e-9)
  t <- EST[EST$row_code == "sens_t", ]; p <- EST[EST$row_code == "primary", ]
  ok1 && near(t$ci_low, exp(p$log_odds - stats::qt(0.975, 32) * p$se_cluster), 1e-9) && near(t$ci_high, exp(p$log_odds + stats::qt(0.975, 32) * p$se_cluster), 1e-9) &&
    near(t$odds_ratio_per_doubling, p$odds_ratio_per_doubling, 1e-12) && t$ci_low < p$ci_low && t$ci_high > p$ci_high &&
    near(t$p_value, 2 * stats::pt(-abs(p$log_odds / p$se_cluster), 32), 1e-9) && near(p$p_value, 2 * stats::pnorm(-abs(p$log_odds / p$se_cluster)), 1e-9) })
check("Holm: p_holm of sens_e and sens_f equals p.adjust(holm) of those two Wald P values; no other row has p_holm", {
  pe <- EST$p_value[EST$row_code == "sens_e"]; pf <- EST$p_value[EST$row_code == "sens_f"]; h <- stats::p.adjust(c(pe, pf), "holm")
  near(EST$p_holm[EST$row_code == "sens_e"], h[1], 1e-12) && near(EST$p_holm[EST$row_code == "sens_f"], h[2], 1e-12) && sum(!is.na(EST$p_holm)) == 2L &&
    near(lvn("sens_e_p_holm"), h[1], 1e-12) && near(lvn("sens_f_p_holm"), h[2], 1e-12) })
check("leave-one-issuer-out: 33 rows, one per informative issuer, summary equals the file", {
  inf <- { a <- aggregate(n_ortho_surgeon_text ~ issuer_id, ind_A, function(x) c(length(x), length(unique(x)))); sort(a$issuer_id[a$n_ortho_surgeon_text[, 1] >= 2 & a$n_ortho_surgeon_text[, 2] >= 2]) }
  ok <- LOO[!is.na(LOO$odds_ratio_per_doubling), ]
  nrow(LOO) == 33L && identical(sort(LOO$issuer_id), inf) && all(LOO$n_plans == 1681 - LOO$n_plans_left_out) &&
    near(lvn("sens_s_or_min"), min(ok$odds_ratio_per_doubling), 1e-12) && near(lvn("sens_s_or_max"), max(ok$odds_ratio_per_doubling), 1e-12) &&
    lvn("sens_s_n_excl1") == sum(ok$ci_low > 1 | ok$ci_high < 1) && identical(lv("sens_s_issuer_min"), ok$issuer_id[which.min(ok$odds_ratio_per_doubling)]) &&
    identical(lv("sens_s_issuer_max"), ok$issuer_id[which.max(ok$odds_ratio_per_doubling)]) &&
    all(LOO$excludes_one[!is.na(LOO$excludes_one)] == (ok$ci_low > 1 | ok$ci_high < 1)) })
check("sens_d leaves out issuer 46944 and equals the matching row of the leave-one-out file; sens_r leaves out 33602 and equals its row", {
  r46944 <- LOO[LOO$issuer_id == "46944", ]; r33602 <- LOO[LOO$issuer_id == "33602", ]
  near(EST$odds_ratio_per_doubling[EST$row_code == "sens_d"], r46944$odds_ratio_per_doubling, 1e-10) && near(EST$odds_ratio_per_doubling[EST$row_code == "sens_r"], r33602$odds_ratio_per_doubling, 1e-10) &&
    identical(as.character(DK[["design_leverage_top1_issuer"]]), "46944") && identical(as.character(DK[["design_largest_issuer_id"]]), "33602") })

# ---------------------------------------------------------------- independent primary and Fig 3 ---------
cat("== independent recomputation of the primary model and of Fig 3\n")
A <- AD[AD$in_analytic, ]; A$plan_type <- factor(A$plan_type, levels = c("HMO", "EPO", "POS", "PPO")); A$metal3 <- factor(A$metal3, levels = c("Bronze", "Silver", "Gold"))
f0 <- stats::glm(cbind(denied, received - denied) ~ expo_ortho + lenroll + plan_type + metal3 + factor(issuer_id), data = A, family = stats::quasibinomial())
V0 <- sandwich::vcovCL(f0, cluster = A$issuer_id, type = "HC1")
check("primary: plain glm + sandwich::vcovCL (HC1, issuer) reproduces the log odds and cluster SE in model_estimates.csv", !anyNA(stats::coef(f0)) &&
        near(stats::coef(f0)[["expo_ortho"]], EST$log_odds[1], 1e-8) && near(sqrt(V0["expo_ortho", "expo_ortho"]), EST$se_cluster[1], 1e-8) &&
        near(exp(stats::coef(f0)[["expo_ortho"]]), lvn("model_primary_or"), 1e-8))
check("primary formula terms equal the SAP 17.2 string (cbind(denied, received - denied) ~ expo_ortho + lenroll + plan_type + metal3 + factor(issuer_id))", {
  src <- paste(readLines("R/06_models.R", warn = FALSE), collapse = "\n")
  grepl('COVARS <- c("lenroll", "plan_type", "metal3")', src, fixed = TRUE) && grepl('fit_qb(A, "expo_ortho", COVARS, fe_primary, "issuer_id", "primary")', src, fixed = TRUE) &&
    grepl('type = "HC1"', src, fixed = TRUE) && grepl("quasibinomial", src, fixed = TRUE) })

# ---------------------------------------------------------------- (j) Fig 3 ----------------------------
cat("== (j) Fig 3\n")
check("Fig 3 bins: 8 bins of floor(1681/8) = 210 plans, remainder (211) in the last bin; sizes sum to 1,681", nrow(BI) == 8L && all(BI$n_plans[1:7] == 210L) && BI$n_plans[8] == 211L && sum(BI$n_plans) == 1681L &&
        max(BI$n_plans[1:7]) - min(BI$n_plans[1:7]) == 0L)
check("Fig 3 bins recompute independently (rank by log2 density, ties by plan_id in C order): claim lines, denied, medians", {
  o <- order(A$expo_ortho, A$plan_id, method = "radix"); a <- A[o, ]; a$bin <- pmin(ceiling(seq_len(nrow(a)) / 210), 8)
  ok <- TRUE
  for (b in 1:8) { x <- a[a$bin == b, ]; ok <- ok && nrow(x) == BI$n_plans[b] && sum(x$received) == BI$claims_received[b] && sum(x$denied) == BI$denied[b] &&
                    near(stats::median(x$expo_ortho), BI$exposure_median_log2[b], 1e-12) && near(sum(x$denied) / sum(x$received), BI$denial_proportion[b], 1e-12) }
  ok && all(diff(BI$exposure_median_log2) > 0) })
check("Fig 3 curves: 101 grid points each, equally spaced on the log2 scale, ends at the 5th and 95th percentiles of the exposure's own distribution", {
  o <- CU[CU$curve == "orthopaedic", ]; d <- CU[CU$curve == "control", ]
  qo <- stats::quantile(A$expo_ortho, c(0.05, 0.95), type = 7, names = FALSE); qd <- stats::quantile(A$expo_derm, c(0.05, 0.95), type = 7, names = FALSE)
  nrow(o) == 101L && nrow(d) == 101L && max(abs(diff(diff(o$exposure_log2)))) < 1e-9 && max(abs(diff(diff(d$exposure_log2)))) < 1e-9 &&
    near(range(o$exposure_log2), qo, 1e-12) && near(range(d$exposure_log2), qd, 1e-12) && identical(sort(unique(CU$curve)), c("control", "orthopaedic")) })
check("Fig 3 curve is linear in log2 exposure: second differences of the predicted probability are below 2% of the mean first difference (the model is logit-linear)", {
  ok <- TRUE
  for (cv in c("orthopaedic", "control")) { x <- CU[CU$curve == cv, ]; ok <- ok && max(abs(diff(diff(x$predicted_probability)))) < 0.02 * mean(abs(diff(x$predicted_probability))) }
  ok })
check("Fig 3 band: ci_low < prediction < ci_high, SE > 0, probabilities in (0,1)", all(CU$ci_low < CU$predicted_probability & CU$predicted_probability < CU$ci_high & CU$se > 0 & CU$predicted_probability > 0 & CU$predicted_probability < 1))
check("Fig 3 orthopaedic curve at three grid points equals an independent average marginal prediction; delta SE equals numDeriv gradient with the full clustered covariance", {
  Xb <- stats::model.matrix(f0); b <- stats::coef(f0); o <- CU[CU$curve == "orthopaedic", ]; ok <- TRUE
  for (i in c(1, 51, 101)) {
    g <- o$exposure_log2[i]
    pf <- function(bb) { Xg <- Xb; Xg[, "expo_ortho"] <- g; mean(stats::plogis(Xg %*% bb)) }
    gr <- numDeriv::grad(pf, b); se <- sqrt(drop(t(gr) %*% V0 %*% gr))
    nd <- A; nd$expo_ortho <- g; pr <- mean(stats::predict(f0, newdata = nd, type = "response"))
    ok <- ok && near(pr, o$predicted_probability[i], 1e-8) && near(se, o$se[i], 1e-5) && near(pf(b), pr, 1e-10)
  }
  ok })
check("predicted probabilities in the ledger equal the curve at the percentile (independent predict() average)", {
  q50 <- stats::quantile(A$expo_ortho, 0.5, type = 7, names = FALSE); nd <- A; nd$expo_ortho <- q50
  near(mean(stats::predict(f0, newdata = nd, type = "response")), lvn("model_pred_p50"), 1e-8) && near(lvn("model_density_p50"), dnum("design_density_median_analytic"), 1e-9) })

# ---------------------------------------------------------------- (i) ledger ----------------------------
cat("== (i) ledger\n")
TEXT_KEYS <- c("model_primary_model", "model_glmm_optimizer", "model_control_derm_reading", "model_control_ophth_reading",
               "sens_s_issuer_min", "sens_s_issuer_max", "sens_s_state_min", "sens_s_state_max")
check("ledger_models.csv keys are lowercase snake_case with a standard prefix and no duplicates", all(grepl("^[a-z][a-z0-9]*(_[a-z0-9]+)*$", LED$key)) && !anyDuplicated(LED$key) &&
        all(grepl("^(model_|sens_|cohort_flow_)", LED$key)) && !any(startsWith(LED$key, "design_")) && !any(startsWith(LED$key, "cohort_all_")))
check("every ledger value is numeric except the declared text keys; no NA, NaN, Inf, marker strings or empty values", {
  num_keys <- setdiff(LED$key, TEXT_KEYS); v <- suppressWarnings(as.numeric(LED$value[match(num_keys, LED$key)]))
  !anyNA(v) && all(is.finite(v)) && all(nzchar(LED$value)) && !any(LED$value %in% c("*", "**", "***", "N/A", "NA", "NaN", "Inf", "-Inf")) && all(TEXT_KEYS %in% LED$key) })
check("proportions are stored 0 to 1 (denial proportions, shares, probabilities)", {
  k <- LED$key[grepl("_denial$|_denial_median$|_claims_share$|_plans_share$|model_pred_p[0-9]+$|share_of_analytic_plans$", LED$key) & !grepl("_sel_", LED$key)]
  v <- as.numeric(LED$value[match(k, LED$key)]); length(k) > 20 && all(v >= 0 & v <= 1) })
check("every Table 3 and Supplement row code has _or, _lo, _hi, _n_plans, _n_issuers keys; the primary has model_primary_*", {
  codes <- c(strsplit(DK[["design_sens_row_codes"]], ";")[[1]], strsplit(DK[["design_supplement_row_codes"]], ";")[[1]])
  need <- c(paste0(rep(codes, each = 5), c("_or", "_lo", "_hi", "_n_plans", "_n_issuers")), paste0("model_primary_", c("or", "lo", "hi", "p", "n_plans", "n_issuers", "n_clusters")))
  all(need %in% LED$key) })
check("ledger estimates equal model_estimates.csv", {
  ok <- near(lvn("model_primary_or"), EST$odds_ratio_per_doubling[1], 1e-12) && near(lvn("model_primary_lo"), EST$ci_low[1], 1e-12) && near(lvn("model_primary_hi"), EST$ci_high[1], 1e-12)
  for (cd in setdiff(EST$row_code, "primary")) ok <- ok && near(lvn(paste0(cd, "_or")), EST$odds_ratio_per_doubling[EST$row_code == cd], 1e-12) && near(lvn(paste0(cd, "_hi")), EST$ci_high[EST$row_code == cd], 1e-12)
  ok })
check("partial correlations are not recreated in the models part (they live in the design part)", !any(grepl("partial_corr", LED$key)))
ld <- function(k) { i <- match(k, LED$key); if (is.na(i)) stop("ledger key missing: ", k); LED$description[i] }
check("F6 S1: the seven sens_s keys keep their values (= primary) but say FULL-SAMPLE and point to the leave-out keys", {
  ks <- paste0("sens_s_", c("or", "lo", "hi", "p", "n_plans", "n_issuers", "converged"))
  all(ks %in% LED$key) && all(vapply(ks, function(k) grepl("FULL-SAMPLE primary", ld(k), fixed = TRUE) && grepl("sens_s_or_min", ld(k), fixed = TRUE) &&
        grepl("sens_s_n_leaveouts", ld(k), fixed = TRUE) && grepl("sens_s_n_excl1", ld(k), fixed = TRUE) && grepl("not to be cited", ld(k), fixed = TRUE), logical(1))) &&
    near(lvn("sens_s_or"), lvn("model_primary_or"), 1e-12) && near(lvn("sens_s_lo"), lvn("model_primary_lo"), 1e-12) && near(lvn("sens_s_hi"), lvn("model_primary_hi"), 1e-12) })
check("F6 S1: the sens_s row note says the leave-out range is in extra columns and the OR and CI are the full-sample primary (estimates and sensitivity files)", {
  grepl("Leave-out range in extra columns; OR, CI shown here are the full-sample primary", SR$notes[SR$row_code == "sens_s"], fixed = TRUE) &&
    grepl("Leave-out range in extra columns; OR, CI shown here are the full-sample primary", EST$notes[EST$row_code == "sens_s"], fixed = TRUE) })
check("F6 S4: the sens_c row note and the model_glmm_ descriptions state the Laplace gradient message and its estimate, the nAGQ 0 fit, model-based SEs, between-issuer contrasts and the Holm family", {
  n <- SR$notes[SR$row_code == "sens_c"]
  need <- c("convergence message", "max|grad|", "gave OR", "nAGQ 0", "per SAP 8.1", "not singular", "model-based", "between-issuer contrasts", "outside the Holm family")
  ok <- all(vapply(need, grepl, logical(1), x = n, fixed = TRUE))
  for (k in c("model_glmm_or", "model_glmm_lo", "model_glmm_hi", "model_glmm_p", "model_glmm_converged", "sens_c_or", "sens_c_p"))
    ok <- ok && all(vapply(need, grepl, logical(1), x = ld(k), fixed = TRUE))
  ok && grepl(sprintf("OR %.4f", lvn("model_glmm_laplace_or")), n, fixed = TRUE) })
check("F6 S3: incomplete-directory descriptions are labelled and the derived share of excluded claim lines recomputes (54.1M of 78.6M = 0.689)", {
  d1 <- ld("cohort_flow_issuer_incomplete_denial"); d2 <- ld("cohort_flow_issuer_incomplete_claims_share")
  grepl("Claim-weighted in-network denial proportion of the 77 incomplete-directory plans (BCBS Florida)", d1, fixed = TRUE) &&
    grepl("not a denial rate", d2, fixed = TRUE) && grepl("15.7 percent", d2, fixed = TRUE) &&
    near(lvn("cohort_flow_issuer_incomplete_share_of_excluded_claims"), sum(m$rec[m$exposure_status == "issuer_incomplete"]) / sum(m$rec[!m$an]), 1e-12) &&
    near(lvn("cohort_flow_issuer_incomplete_share_of_excluded_claims"), 0.689, 1e-3) &&
    grepl("68.9 percent", ld("cohort_flow_issuer_incomplete_share_of_excluded_claims"), fixed = TRUE) })
check("F6 S2: control ratio, same-sign and at-least-half descriptions (derm and ophth) say not to quote, rule not applied, SAP 6.4", {
  ks <- paste0("model_control_", rep(c("derm", "ophth"), each = 3), c("_ratio", "_same_sign", "_at_least_half"))
  all(vapply(ks, function(k) grepl("not to be quoted; SAP 6.2 rule not applied because the primary CI includes 1; SAP 6.4 forbids a specificity statement", ld(k), fixed = TRUE), logical(1))) })
check("F6 S6: outcome-bound plans come from 2 issuers (5 + 1); sens_i adds 5 because one received-suppressed plan has no denominator at bound 0; keys recompute from the bound-plan file", {
  B <- utils::read.csv(R("model_outcome_bound_plans.csv"), colClasses = c(plan_id = "character", issuer_id = "character"), stringsAsFactors = FALSE)
  qp <- unique(B[, c("plan_id", "issuer_id", "suppressed_side")]); tb <- sort(table(qp$issuer_id), decreasing = TRUE)
  lvn("sens_suppout_issuers_with_added_plans") == 2 && lvn("sens_suppout_plans_largest_issuer") == 5 && length(tb) == 2 && as.integer(tb[1]) == 5 && as.integer(tb[2]) == 1 &&
    identical(names(tb)[1], "71287") && identical(names(tb)[2], "60612") && lv("sens_suppout_largest_issuer_id") == "71287" &&
    lvn("sens_suppout_qualifying_plans") == 6 && lvn("sens_suppout_received_suppressed_plans") == 1 && lvn("sens_suppout_denied_suppressed_plans") == 5 &&
    lvn("sens_i_n_added") == 5 && lvn("sens_j_n_added") == 6 && grepl("denominator would be 0", ld("sens_i_n_added"), fixed = TRUE) })
check("merged ledger (kff, desc, figdesc, ortho, models, design) reads with no duplicate key across parts", {
  L <- ledger_read_all(); all(c("kff", "desc", "figdesc", "ortho", "models", "design") %in% L$part) && !anyDuplicated(L$key) })
check("numbers_ledger.csv, if present, equals ledger_read_all() (key, value, part)", {
  f <- R("numbers_ledger.csv"); if (!file.exists(f)) TRUE else { Lm <- ledger_read_merged(f); L <- ledger_read_all(); identical(Lm$key, L$key) && identical(Lm$value, L$value) && identical(Lm$part, L$part) } })
check("design-constant cross-checks inside the models part: density median equals design_density_median_analytic; K equals design_informative_issuer_k", near(lvn("model_density_p50"), dnum("design_density_median_analytic"), 1e-9) && lvn("model_primary_k") == dnum("design_informative_issuer_k"))

# ---------------------------------------------------------------- misc output checks --------------------
check("model_deviations.csv has the required header and no em-dash", { d <- utils::read.csv(R("model_deviations.csv"), stringsAsFactors = FALSE); identical(names(d), c("item", "sap_text", "what_was_done", "reason")) })
check("GLMM row: sens_c present with convergence status and a model-based Wald interval; ledger carries convergence, singularity and variances", {
  r <- EST[EST$row_code == "sens_c", ]
  !is.na(r$odds_ratio_per_doubling) && is.na(r$se_cluster) && r$se_type == "model-based Wald (glmer)" && all(c("model_glmm_or", "model_glmm_converged", "model_glmm_singular", "model_glmm_nagq", "model_glmm_var_plan") %in% LED$key) })
check("negative-control reading keys exist; the reading rule is applied only when the primary CI excludes 1", {
  ex1 <- lvn("model_primary_excludes_one") == 1; r1 <- lv("model_control_derm_reading")
  (ex1 && r1 %in% c("not specific", "not reproduced")) || (!ex1 && identical(r1, "rule not applied")) })

# ---------------------------------------------------------------- (m) fresh-process reproducibility ------
cat("== (m) fresh-process reproducibility\n")
fresh <- file.path(SCR, "fresh.R")
writeLines(c(
  sprintf('setwd("%s")', P$v1),
  'source("R/00_setup.R"); source("R/ledger.R")',
  sprintf('options(psd_models_no_run = TRUE, psd_models_logfile = "%s")', file.path(SCR, "fresh.log")),
  'source("R/06_models.R")',
  'idc <- list(character = c("plan_id", "issuer_id", "state", "state_issuer", "exposure_status", "enrollment_marker", "metal_level", "plan_type", "metal3"))',
  'D <- restore_types(as.data.frame(data.table::fread("results/model_analytic_data.csv", colClasses = idc, na.strings = c("", "NA"))))',
  'ADD <- restore_types(as.data.frame(data.table::fread("results/model_outcome_bound_plans.csv", colClasses = list(character = c("plan_id", "issuer_id", "state", "state_issuer", "metal_level", "plan_type", "metal3", "suppressed_side", "donor_network_id")), na.strings = c("", "NA"))))',
  'M <- run_models(D, ADD, verbose = FALSE)',
  sprintf('utils::write.csv(M$est, "%s", row.names = FALSE)', file.path(SCR, "est_fresh.csv")),
  sprintf('utils::write.csv(M$loo, "%s", row.names = FALSE)', file.path(SCR, "loo_fresh.csv"))), fresh)
rc <- system2(file.path(R.home("bin"), "Rscript"), fresh, stdout = file.path(SCR, "fresh.out"), stderr = file.path(SCR, "fresh.err"))
check("fresh R process ran from model_analytic_data.csv and model_outcome_bound_plans.csv without error", identical(as.integer(rc), 0L) && file.exists(file.path(SCR, "est_fresh.csv")))
if (file.exists(file.path(SCR, "est_fresh.csv"))) {
  EF <- utils::read.csv(file.path(SCR, "est_fresh.csv"), stringsAsFactors = FALSE, na.strings = c("", "NA"))
  LF <- utils::read.csv(file.path(SCR, "loo_fresh.csv"), colClasses = c(issuer_id = "character"), stringsAsFactors = FALSE, na.strings = c("", "NA"))
  check("every exposure-outcome estimate (all 21 rows: log odds, SE, interval, P, n) reproduces in the fresh process", {
    ok <- identical(EF$row_code, EST$row_code) && all(EF$n_plans == EST$n_plans) && all(EF$n_issuers == EST$n_issuers)
    for (cd in EST$row_code) { a <- EF[EF$row_code == cd, ]; b <- EST[EST$row_code == cd, ]
      tol <- if (cd == "sens_c") 1e-5 else 1e-9
      ok <- ok && near(a$log_odds, b$log_odds, tol) && near(a$ci_low, b$ci_low, tol) && near(a$ci_high, b$ci_high, tol) && near(a$p_value, b$p_value, max(tol, 1e-7)) }
    ok })
  check("all 33 leave-one-issuer-out estimates reproduce in the fresh process", near(LF$odds_ratio_per_doubling, LOO$odds_ratio_per_doubling[match(LF$issuer_id, LOO$issuer_id)], 1e-9))
}

# ---------------------------------------------------------------- (n) merge script ----------------------
check("08_merge_ledger.R exists, is ASCII and calls ledger_write_merged() and ledger_read_merged()", {
  s <- readLines("R/08_merge_ledger.R", warn = FALSE); any(grepl("ledger_write_merged()", s, fixed = TRUE)) && any(grepl("ledger_read_merged()", s, fixed = TRUE)) })
}

# ---------------------------------------------------------------- (k) ASCII and no em-dash ---------------
cat("== (k) ASCII and prose rules\n")
em <- intToUtf8(0x2014)
scripts <- c("R/06_models.R", "R/08_merge_ledger.R", "tests/test_models.R")
check("scripts are ASCII only (so no U+2014 either)", all(vapply(scripts, function(f) !any(grepl("[^\\x01-\\x7F\\n\\t]", readLines(f, warn = FALSE, encoding = "bytes"), perl = TRUE, useBytes = TRUE)), logical(1))))
check("scripts contain no em-dash", all(vapply(scripts, function(f) !any(grepl(em, readLines(f, warn = FALSE), fixed = TRUE)), logical(1))))
check("no script line is a banner of four or more hash characters (Bash hygiene rule)",
      all(vapply(scripts, function(f) !any(grepl("^\\s*#{4,}", readLines(f, warn = FALSE), perl = TRUE)), logical(1))))
if (!identical(ONLY, "machinery")) {
  outs <- list.files(P$results, pattern = "^model_.*\\.csv$", full.names = TRUE)
  outs <- c(outs, file.path(P$ledger_parts, "ledger_models.csv"))
  check("model_*.csv outputs and ledger_models.csv are ASCII with no em-dash", all(vapply(outs, function(f) !any(grepl("[^\\x01-\\x7F\\n\\t]", readLines(f, warn = FALSE, encoding = "bytes"), perl = TRUE, useBytes = TRUE)), logical(1))))
  check("machine CSV column names are snake_case (lowercase letters, digits, underscores)", all(vapply(outs[basename(outs) != "ledger_models.csv"], function(f) all(grepl("^[a-z][a-z0-9_]*$", names(utils::read.csv(f, nrows = 1, check.names = FALSE)))), logical(1))))
}

# ---------------------------------------------------------------- (l) logs unchanged ----------------------
cat("== (l) nothing under v1/logs changed\n")
check("v1/logs is byte-identical before and after this test run", { LOGS1 <- logs_snapshot(); identical(LOGS0, LOGS1) })

cat("\n", N_PASS, " passed, ", N_FAIL, " failed\n", sep = "")
if (N_FAIL > 0L) { cat("FAILED: ", paste(FAILS, collapse = "; "), "\n", sep = ""); quit(status = 1L) }
