# =====================================================================
# test_sap.R : WP5 tests (plain R).
# Run: cd v1 ; Rscript tests/test_sap.R     (any working directory works; the script cd's into v1)
# Exit status 1 if any check fails. Nothing is tuned to pass.
# The independent recomputation reads plans.csv ONLY through the nine allowed columns
# (it skips every other column with colClasses "NULL") and never opens an outcome column.
# Keep this file ASCII.
# =====================================================================
.args <- commandArgs(trailingOnly = FALSE)
.f <- sub("^--file=", "", grep("^--file=", .args, value = TRUE)[1])
.here <- dirname(normalizePath(gsub("~+~", " ", .f, fixed = TRUE)))
source(file.path(dirname(.here), "R", "00_setup.R"))
source(file.path(dirname(.here), "R", "ledger.R"))
setwd(P$v1)

N_PASS <- 0L; N_FAIL <- 0L; FAILS <- character(0)
check <- function(name, expr) {
  res <- tryCatch({ r <- force(expr); if (isTRUE(r)) "ok" else paste0("FALSE", if (is.character(r)) paste0(": ", paste(utils::head(r, 3), collapse = " | ")) else "") },
                  error = function(e) paste0("ERROR: ", conditionMessage(e)))
  if (identical(res, "ok")) { N_PASS <<- N_PASS + 1L; cat("PASS  ", name, "\n", sep = "") }
  else { N_FAIL <<- N_FAIL + 1L; FAILS <<- c(FAILS, name); cat("FAIL  ", name, "  ->  ", res, "\n", sep = "") }
  invisible(NULL)
}

SAP <- "config/sap_frozen.md"; DC <- "config/design_constants.csv"; HASH <- "logs/sap_hash.txt"
SCRIPT <- Sys.getenv("SAP_TEST_SCRIPT", unset = "R/05b_sap_checks.R")   # env override is for the mutation checks only
PLAN_COLS <- c("plan_id", "issuer_id", "state", "plan_type", "metal_level", "avg_monthly_enrollment",
               "avg_monthly_enrollment_marker", "pa_network_id", "pa_service_area_id")
sap_txt <- paste(readLines(SAP, warn = FALSE), collapse = "\n")
dc <- read.csv(DC, colClasses = "character", na.strings = NULL)
dk <- setNames(dc$value, dc$key)
num <- function(k) as.numeric(dk[[k]])

# ---- independent recomputation (base R, restricted column read) -------------------------
read_plans_restricted <- function() {
  f <- "data_clean/cms/plans.csv"
  hdr <- strsplit(readLines(f, n = 1L, warn = FALSE), ",", fixed = TRUE)[[1]]
  stopifnot(all(PLAN_COLS %in% hdr))
  cc <- ifelse(hdr %in% PLAN_COLS, "character", "NULL"); names(cc) <- hdr
  read.csv(f, colClasses = cc, na.strings = c("", "NA"))
}
ex <- read.csv("data_clean/ortho/plan_exposure.csv",
               colClasses = c(plan_id = "character", issuer_id = "character", state = "character", exposure_status = "character"),
               na.strings = c("", "NA"))
pl <- read_plans_restricted()
m <- merge(ex, pl[, c("plan_id", "plan_type", "metal_level", "avg_monthly_enrollment", "avg_monthly_enrollment_marker", "pa_network_id")], by = "plan_id", all.x = TRUE)
m$enr <- ifelse(m$avg_monthly_enrollment_marker == "numeric", suppressWarnings(as.numeric(m$avg_monthly_enrollment)), NA_real_)
ok <- m$exposure_status == "ok"; an <- ok & !is.na(m$enr) & m$enr > 0
A <- m[an, ]
agg <- aggregate(n_ortho_surgeon_text ~ issuer_id, data = A, FUN = function(x) c(n = length(x), d = length(unique(x))))
K_ind <- sum(agg$n_ortho_surgeon_text[, "n"] >= 2 & agg$n_ortho_surgeon_text[, "d"] >= 2)

# ---- 1. hash ----------------------------------------------------------------------------
check("hash file exists", file.exists(HASH))
hl <- if (file.exists(HASH)) readLines(HASH, warn = FALSE) else character(0)
want <- c("config/sap_frozen.md", "config/design_constants.csv", "config/specialty_text_crosswalk.csv", "config/ortho_taxonomy.csv")
check("hash file has four hash lines, then frozen_at and frozen_by", length(hl) == 6L &&
        all(grepl("^[0-9a-f]{64}  ", hl[1:4])) && grepl("^frozen_at: [0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(Z|[+-][0-9]{2}:?[0-9]{2})$", hl[5]) &&
        identical(hl[6], "frozen_by: WP5-F5"))
check("hash lines name the four files in order", identical(sub("^[0-9a-f]{64}  ", "", hl[1:4]), want))
check("hashes match the current files", {
  cur <- vapply(want, function(p) digest::digest(file = p, algo = "sha256", serialize = FALSE), character(1))
  identical(unname(cur), sub("^([0-9a-f]{64}).*$", "\\1", hl[1:4]))
})
check("hash history file keeps the superseded first freeze, labelled, with different SAP and constants hashes", {
  hh <- readLines("logs/sap_hash_history.txt", warn = FALSE)
  length(hh) == 7L && grepl("^SUPERSEDED 2026-10-28, never used by any outcome analysis, replaced by the F5 re-freeze because of reviewer R5", hh[1]) &&
    all(grepl("^[0-9a-f]{64}  config/", hh[2:5])) && identical(hh[7], "frozen_by: WP5") &&
    !identical(sub(" .*", "", hh[2]), sub(" .*", "", hl[1])) && !identical(sub(" .*", "", hh[3]), sub(" .*", "", hl[2])) &&
    identical(hh[4:5], hl[3:4])
})
check("crosswalk and taxonomy hashes equal the WP4 frozen hashes and the design keys", {
  w4 <- readLines("logs/wp4_crosswalk_hash.txt", warn = FALSE)
  cw <- sub("^([0-9a-f]{64}).*$", "\\1", hl[3]); tx <- sub("^([0-9a-f]{64}).*$", "\\1", hl[4])
  any(grepl(cw, w4, fixed = TRUE)) && any(grepl(tx, w4, fixed = TRUE)) &&
    identical(dk[["design_crosswalk_sha256"]], cw) && identical(dk[["design_taxonomy_sha256"]], tx)
})
check("sap_frozen.md was not modified after the hash (mtime not later than the hash file)", {
  file.info(SAP)$mtime <= file.info(HASH)$mtime && file.info(DC)$mtime <= file.info(HASH)$mtime
})

# ---- 2. required design keys ------------------------------------------------------------
req <- c("design_alpha", "design_ci_level", "design_seed", "design_holm_family_size", "design_informative_issuer_k",
         "design_informative_issuer_threshold", "design_primary_model", "design_density_per", "design_zero_offset_needed",
         "design_zero_offset", "design_suppression_lower", "design_suppression_mid", "design_suppression_upper",
         "design_control_reading_ratio", "design_fig3_bins", "design_min_claims_exclusion", "design_n_sensitivity_rows",
         "design_analytic_plans", "design_analytic_issuers", "design_analytic_states", "design_enrollment_free_plans",
         "design_crosswalk_sha256", "design_taxonomy_sha256", "design_ppv_surgeon_text", "design_ppv_derm", "design_ppv_ophth",
         "design_partial_corr_derm", "design_partial_corr_ophth", "design_leverage_top1_issuer", "design_leverage_top1_share",
         "design_leverage_top2_issuer", "design_leverage_top3_issuer", "design_effective_issuers", "design_t_interval_df",
         "design_largest_issuer_id", "design_n_supplement_rows", "design_supplement_row_codes", "design_spearman_ortho_derm",
         "design_spearman_ortho_ophth", "design_noncohort_with_donor", "design_noncohort_with_donor_numeric_enrollment")
check("every required design key exists with a non-missing value", all(req %in% dc$key) &&
        all(nzchar(trimws(dk[req]))) && !any(toupper(dk[req]) %in% c("NA", "NAN", "INF")))
check("design keys are unique, lowercase snake_case and all start with design_", !anyDuplicated(dc$key) && all(grepl("^design_[a-z0-9_]+$", dc$key)))
check("design column set is key,value,unit,description,source_script", identical(names(dc), c("key", "value", "unit", "description", "source_script")))
check("design constants read through the ledger reader (no collision with other parts, no empty values)", {
  led <- ledger_read_all(design_csv = file.path(P$config, "design_constants.csv"))
  d <- led[led$part == "design", ]
  nrow(d) == nrow(dc) && !any(duplicated(led$key)) && is.numeric(ledger_get(led, "design_informative_issuer_k"))
})
check("design constants agree with study_config.yaml (alpha, ci level, seed)", {
  cfg <- load_config()
  num("design_alpha") == cfg$inference$alpha && num("design_ci_level") == cfg$inference$ci_level && num("design_seed") == cfg$study$seed
})
check("fixed values: Holm family 2, threshold 30, density per 10000, bounds 1 / 5.5 / 10, ratio 0.5, bins 8, min claims 10000",
      num("design_holm_family_size") == 2 && num("design_informative_issuer_threshold") == 30 && num("design_density_per") == 10000 &&
        num("design_suppression_lower") == 1 && num("design_suppression_mid") == 5.5 && num("design_suppression_upper") == 10 &&
        num("design_control_reading_ratio") == 0.5 && num("design_fig3_bins") == 8 && num("design_min_claims_exclusion") == 10000)
check("zero offset consistent: no zero in the primary exposure, offset 0; ophthalmology offset positive", {
  min(A$n_ortho_surgeon_text) > 0 && num("design_zero_offset_needed") == 0 && num("design_zero_offset") == 0 &&
    num("design_zero_offset_ophth") > 0 && num("design_zero_count_ophth_analytic") == sum(A$n_ophth_text == 0)
})

# ---- 3. K and the decision rule ----------------------------------------------------------
check("K recomputed independently equals design_informative_issuer_k", K_ind == num("design_informative_issuer_k"))
check("plans in informative issuers recomputed equals the design key", {
  inf <- agg$issuer_id[agg$n_ortho_surgeon_text[, "n"] >= 2 & agg$n_ortho_surgeon_text[, "d"] >= 2]
  sum(A$issuer_id %in% inf) == num("design_informative_issuer_plans")
})
source_main <- function() { options(psd_sap_no_run = TRUE); sys.source(SCRIPT, envir = (e <- new.env(parent = globalenv()))); e }
E <- tryCatch(source_main(), error = function(err) { cat("could not source the script: ", conditionMessage(err), "\n"); NULL })
rule_toy <- function(k, th = 30) if (k >= th) "issuer_fe" else "state_fe"
check("decision rule at K = 29, 30, 31 (toy function)", identical(rule_toy(29), "state_fe") && identical(rule_toy(30), "issuer_fe") && identical(rule_toy(31), "issuer_fe"))
check("the script's primary_model_rule agrees with the toy rule at 29, 30, 31 and errors on NA", {
  !is.null(E) && identical(E$primary_model_rule(29), "state_fe") && identical(E$primary_model_rule(30), "issuer_fe") &&
    identical(E$primary_model_rule(31), "issuer_fe") && inherits(try(E$primary_model_rule(NA), silent = TRUE), "try-error")
})
check("the rule maps the computed K to design_primary_model", identical(rule_toy(K_ind), dk[["design_primary_model"]]))
check("SAP states K and the selected model", grepl(paste0("K = ", K_ind), sap_txt, fixed = TRUE) && grepl(dk[["design_primary_model"]], sap_txt, fixed = TRUE))

# ---- 4. analytic sample counts -----------------------------------------------------------
check("cohort = 2527 plans and one row per plan", nrow(ex) == 2527L && !anyDuplicated(ex$plan_id))
check("exposure-ok counts recomputed (plans, issuers, states)", sum(ok) == num("design_exposure_ok_plans") && length(unique(m$issuer_id[ok])) == num("design_exposure_ok_issuers") &&
        length(unique(m$state[ok])) == num("design_exposure_ok_states") && sum(ok) == num("design_enrollment_free_plans"))
check("analytic counts recomputed (plans, issuers, states)", nrow(A) == num("design_analytic_plans") && length(unique(A$issuer_id)) == num("design_analytic_issuers") &&
        length(unique(A$state)) == num("design_analytic_states"))
check("every analytic issuer sits in one state (issuers = issuer-states)", all(tapply(A$state, A$issuer_id, function(s) length(unique(s))) == 1L) &&
        num("design_analytic_issuer_states") == num("design_analytic_issuers"))
check("SAP text carries the ladder numbers (cohort, ok, analytic, lost to enrollment)", {
  f <- function(x) format(x, big.mark = ",", trim = TRUE)
  all(vapply(c(f(nrow(ex)), f(sum(ok)), f(nrow(A)), f(sum(ok & !an))), function(s) grepl(s, sap_txt, fixed = TRUE), logical(1)))
})
check("enrollment among exposure-ok plans is either numeric or the ** marker (star2)", all(m$avg_monthly_enrollment_marker[ok & !an] == "star2") &&
        sum(ok & !an) == num("design_ok_plans_enrollment_star2"))
check("ladder CSV rungs equal the recomputed counts", {
  l <- read.csv("results/sap_check_ladder.csv", na.strings = "")
  l$n_plans[l$rung_code == "cohort_computable"] == 2527 && l$n_plans[l$rung_code == "exposure_ok"] == sum(ok) && l$n_plans[l$rung_code == "analytic"] == nrow(A)
})
check("excluded exposure statuses sum to cohort minus ok", {
  l <- read.csv("results/sap_check_ladder.csv", na.strings = "")
  sum(l$n_plans[grepl("^excluded_", l$rung_code)]) == 2527 - sum(ok)
})
check("SAP words for the three variance shares match the diagnostics file", {
  v <- read.csv("results/sap_check_variance_decomp.csv")[1, ]
  all(vapply(sprintf("%.1f percent", 100 * c(v$share_from_count, v$share_from_enrollment, v$share_from_covariance)),
             function(s) grepl(s, sap_txt, fixed = TRUE), logical(1))) &&
    abs(v$share_from_count - num("design_within_issuer_share_count")) < 1e-9
})
check("variance decomposition shares sum to 1", {
  v <- read.csv("results/sap_check_variance_decomp.csv")
  all(abs(v$share_from_count + v$share_from_enrollment + v$share_from_covariance - 1) < 1e-9)
})

check("SAP section 16 state statement: at least 9 distinct density values per state; distinct surgeon counts 1 to 20", {
  sd <- read.csv("results/sap_check_state_distinct.csv")
  min(sd$n_distinct_density) == 9L && min(sd$n_distinct_surgeon_count) == 1L && max(sd$n_distinct_surgeon_count) == 20L && nrow(sd) == 30L
})
check("SAP section 16 enrollment-loss statement: top 10 issuers hold 314 of the lost plans; 585 cohort plans lack numeric enrollment", {
  e <- read.csv("results/sap_check_enrollment_lost_top10.csv", colClasses = c(issuer_id = "character"))
  sum(e$n_ok_lost_to_complete_case) == 314L && e$issuer_id[1] == "33602" && e$n_ok_lost_to_complete_case[1] == 142L && sum(is.na(m$enr)) + sum(!is.na(m$enr) & m$enr <= 0) + 0L == 2527L - 1942L
})
check("SAP section 16 structure statements: largest issuer share, informative-issuer concentration", {
  top <- read.csv("results/sap_check_informative_issuers_top10.csv", colClasses = c(issuer_id = "character"))
  top$issuer_id[1] == "33602" && top$n_plans[1] == 380L && abs(top$share_of_informative_plans[1] - 0.419) < 0.0005 &&
    abs(num("design_largest_issuer_share") - 380 / nrow(A)) < 1e-12 && grepl("41.9 percent", sap_txt, fixed = TRUE)
})

# ---- 5. sensitivity table ----------------------------------------------------------------
lines <- readLines(SAP, warn = FALSE)
rowline <- grep("^\\| sens_[a-z] \\|", lines, value = TRUE)
codes <- sub("^\\| (sens_[a-z]) \\|.*$", "\\1", rowline)
sect92 <- which(grepl("^9\\.2 ", lines)); sect93 <- which(grepl("^9\\.3 ", lines)); sect94 <- which(grepl("^9\\.4 ", lines))
idx <- grep("^\\| sens_[a-z] \\|", lines)
t3 <- codes[idx > sect92 & idx < sect93]; sup <- codes[idx > sect93 & idx < sect94]
check("row codes are unique across Table 3 and the Supplement", !anyDuplicated(codes) && length(codes) == length(t3) + length(sup))
check("Table 3 has at most 12 sensitivity rows (plus the primary) and the count equals design_n_sensitivity_rows", length(t3) <= 12L && length(t3) == num("design_n_sensitivity_rows"))
check("Supplement row count equals design_n_supplement_rows", length(sup) == num("design_n_supplement_rows"))
check("row codes in display order equal the design key lists", identical(paste(t3, collapse = ";"), dk[["design_sens_row_codes"]]) &&
        identical(paste(sup, collapse = ";"), dk[["design_supplement_row_codes"]]))
check("row codes are consecutive letters from sens_a", identical(c(t3, sup), paste0("sens_", letters[seq_along(c(t3, sup))])))
check("every table row has exactly three cells (code, label, specification)", all(lengths(strsplit(sub("^\\| ", "", sub(" \\|$", "", rowline)), " \\| ")) == 3L))

# ---- 5b. F5 additions: identifying variation, partial correlations, donors, wording ---------------
A$lenroll <- log2(A$enr)
A$metal3 <- ifelse(A$metal_level %in% c("Bronze", "Catastrophic"), "Bronze", ifelse(A$metal_level %in% c("Gold", "Platinum"), "Gold", "Silver"))
A$dens <- A$n_ortho_surgeon_text / A$enr * 10000
A$expo <- log2(A$dens)
A$expo_derm <- log2(A$n_derm_text / A$enr * 10000 + num("design_zero_offset_derm"))
A$expo_ophth <- log2(A$n_ophth_text / A$enr * 10000 + num("design_zero_offset_ophth"))
res_of <- function(d, y) resid(lm(as.formula(paste(y, "~ lenroll + plan_type + metal3 + factor(issuer_id)")), data = d))
shr <- function(d) { r <- res_of(d, "expo"); z <- tapply(r^2, d$issuer_id, sum); sort(z / sum(z), decreasing = TRUE) }
sh <- shr(A)
check("identifying variation recomputed independently: top three issuers, shares, effective number", {
  names(sh)[1] == dk[["design_leverage_top1_issuer"]] && names(sh)[2] == dk[["design_leverage_top2_issuer"]] && names(sh)[3] == dk[["design_leverage_top3_issuer"]] &&
    abs(sh[[1]] - num("design_leverage_top1_share")) < 1e-8 && abs(sh[[2]] - num("design_leverage_top2_share")) < 1e-8 &&
    abs(sh[[3]] - num("design_leverage_top3_share")) < 1e-8 && abs(1 / sum(sh^2) - num("design_effective_issuers")) < 1e-8 && abs(sum(sh) - 1) < 1e-12
})
check("SAP states the top-three shares, the effective number and that K is not effective information", {
  all(vapply(c(sprintf("%.1f percent", 100 * sh[1:3]), sprintf("%.1f", 1 / sum(sh^2)), "not a measure of effective information", names(sh)[1:3]),
             function(x) grepl(x, sap_txt, fixed = TRUE), logical(1)))
})
check("identifying-variation CSV shares sum to 1 and match the recomputation", {
  iv <- read.csv("results/sap_check_identifying_variation.csv", colClasses = c(issuer_id = "character"))
  abs(sum(iv$share_of_residual_ss) - 1) < 1e-9 && identical(iv$issuer_id[1:3], names(sh)[1:3]) && nrow(iv) == length(sh)
})
check("t interval degrees of freedom equal K - 1 and the SAP says 32", num("design_t_interval_df") == K_ind - 1 && grepl("K - 1 = 32 degrees of freedom", sap_txt, fixed = TRUE))
check("sens_d leaves out the top-leverage issuer and sens_r the issuer with the most plans (SAP table equals the design keys)", {
  rd <- grep("^\\| sens_d \\|", lines, value = TRUE); rr <- grep("^\\| sens_r \\|", lines, value = TRUE)
  most <- names(sort(table(A$issuer_id), decreasing = TRUE))[1]
  grepl(paste0("without issuer ", dk[["design_leverage_top1_issuer"]]), rd, fixed = TRUE) && grepl("largest share of identifying variation", rd, fixed = TRUE) &&
    grepl(paste0("without issuer ", dk[["design_largest_issuer_id"]]), rr, fixed = TRUE) && most == dk[["design_largest_issuer_id"]]
})
check("sens_s and sens_t rows exist in the Supplement with the leave-one-out and t-interval wording", {
  rs <- grep("^\\| sens_s \\|", lines, value = TRUE); rt <- grep("^\\| sens_t \\|", lines, value = TRUE)
  grepl("without each of the K = 33 informative issuers", rs, fixed = TRUE) && grepl("smallest and largest odds ratio", rs, fixed = TRUE) &&
    grepl("excludes 1", rs, fixed = TRUE) && grepl("t interval", rt, fixed = TRUE) && num("design_informative_issuer_k") == 33
})
pc <- function(d, y) cor(res_of(d, "expo"), res_of(d, y))
Ap <- A[A$n_ophth_text > 0, ]
check("partial correlations recomputed independently equal the design keys", {
  abs(pc(A, "expo_derm") - num("design_partial_corr_derm")) < 1e-8 && abs(pc(A, "expo_ophth") - num("design_partial_corr_ophth")) < 1e-8 &&
    abs(cor(res_of(Ap, "expo"), res_of(Ap, "expo_ophth")) - num("design_partial_corr_ophth_positive")) < 1e-8
})
check("raw Spearman keys recomputed", {
  fin <- is.finite(log2(A$n_ophth_text / A$enr * 10000))
  abs(cor(A$expo, A$expo_derm, method = "spearman") - num("design_spearman_ortho_derm")) < 1e-9 &&
    abs(cor(A$expo[fin], log2(A$n_ophth_text[fin] / A$enr[fin] * 10000), method = "spearman") - num("design_spearman_ortho_ophth")) < 1e-9
})
check("SAP 6.4 reports both partial and raw correlations and the network-breadth reading", {
  all(vapply(c(sprintf("%.3f", c(num("design_partial_corr_derm"), num("design_partial_corr_ophth"), num("design_partial_corr_ophth_positive"))),
               sprintf("%.3f", c(num("design_spearman_ortho_derm"), num("design_spearman_ortho_ophth"))), "network breadth",
               "cannot be read as a lack of specificity", "beside rows sens_g and sens_h", "design_partial_corr_derm", "design_partial_corr_ophth"),
             function(x) grepl(x, sap_txt, fixed = TRUE), logical(1)))
})
nc <- pl[!(pl$plan_id %in% ex$plan_id), ]
okc <- merge(ex[ex$exposure_status == "ok", ], pl[, c("plan_id", "pa_network_id")], by = "plan_id")
okc <- okc[!is.na(okc$pa_network_id), ]
okc$sig <- paste(okc$n_ortho_surgeon_text, okc$n_ortho_apc_text, okc$n_derm_text, okc$n_ophth_text, okc$n_individual_npis)
okc$key <- paste(okc$issuer_id, okc$pa_network_id, sep = "|")
agree <- tapply(okc$sig, okc$key, function(x) length(unique(x)) == 1L)
nc$key <- paste(nc$issuer_id, nc$pa_network_id, sep = "|")
nc$donor <- !is.na(nc$pa_network_id) & nc$key %in% names(agree)[agree]
nc$numeric <- nc$avg_monthly_enrollment_marker == "numeric"
check("donor availability recomputed independently (plans outside the cohort, with donor, with donor and numeric enrollment)", {
  nrow(nc) == num("design_noncohort_plans") && sum(nc$donor) == num("design_noncohort_with_donor") &&
    sum(nc$donor & nc$numeric) == num("design_noncohort_with_donor_numeric_enrollment") &&
    grepl(paste0(sum(nc$donor), " of the ", format(nrow(nc), big.mark = ","), " plans outside the cohort"), sap_txt, fixed = TRUE) &&
    grepl(paste0("only ", sum(nc$donor & nc$numeric), " of those also have numeric enrollment"), sap_txt, fixed = TRUE)
})
check("SAP 9.4 says near-null by construction and not stress tests", grepl("near-null by construction", sap_txt, fixed = TRUE) && grepl("not described as stress tests", sap_txt, fixed = TRUE))
check("SAP no longer uses the undefined draft letters (m) and (g)", !grepl("\\(m\\)", sap_txt) && !grepl("\\(g\\)", sap_txt) && !grepl("Draft candidates", sap_txt, fixed = TRUE))
check("SAP sens_m states the literal reading (no glm weights, only two predictors, HC1 clustered by state)", {
  rm_ <- grep("^\\| sens_m \\|", lines, value = TRUE)
  all(vapply(c("NO glm weights", "used only to form these two means", "no plan type, metal level or state term", "HC1 SEs clustered by state"), function(x) grepl(x, rm_, fixed = TRUE), logical(1)))
})
check("SAP Fig 3 bin rule: rank by log2 density, ties by plan_id, floor(n / 8) with the remainder in the last bin; counts add up", {
  nn <- nrow(A); b <- floor(nn / 8)
  grepl("break ties by plan_id (character sort)", sap_txt, fixed = TRUE) && grepl("floor(n / 8) = 210", sap_txt, fixed = TRUE) && b == 210 && 7 * b + (nn - 7 * b) == nn && (nn - 7 * b) == 211 &&
    grepl("mostly reflects plan size", sap_txt, fixed = TRUE) && grepl("points are unadjusted", sap_txt, fixed = TRUE)
})
check("SAP 11.1 leads with the Jeffreys interval, not the point value", {
  l <- grep("^11\\.1 ", lines, value = TRUE)
  grepl("Jeffreys-based Monte Carlo interval is about 65 to 99 percent", l, fixed = TRUE) && grepl("prior-dependent", l, fixed = TRUE) && !grepl("is about 93 percent", l, fixed = TRUE)
})
check("SAP asks WP6b for claims per Table 1 rung and the claim-volume comparison", grepl("in-network claim lines received for every rung", sap_txt, fixed = TRUE) && grepl("claim volume", sap_txt, fixed = TRUE))

# ---- 6. outcome blindness ----------------------------------------------------------------
code_lines <- readLines(SCRIPT, warn = FALSE)
code_only <- code_lines[!grepl("^\\s*#", code_lines)]
forbidden <- c("plan_claims", "plan_reason", "claims_received", "claims_denied", "denied", "denial", "appeal", "resubmit", "reason",
               "in_network", "out_of_network", "oon_", "_rate", "issuer_state\\.csv", "issuer_volume", "ledger_kff", "ledger_desc",
               "ledger_figdesc", "desc_", "numbers_ledger", "marker_counts", "dictionary_vs", "issuer_count", "g2_reconciliation")
hits <- vapply(forbidden, function(p) any(grepl(p, code_lines, ignore.case = TRUE)), logical(1))
check("script (comments included) holds none of the forbidden outcome column or file patterns", if (!any(hits)) TRUE else names(hits)[hits])
check("PLAN_COLS in the script is exactly the nine allowed columns", {
  !is.null(E) && identical(E$PLAN_COLS, PLAN_COLS)
})
check("plans.csv is named exactly once in code, and read with select = PLAN_COLS", {
  hit <- grep("plans\\.csv", code_only, value = TRUE)
  length(hit) == 1L && any(grepl("select = PLAN_COLS", code_only, fixed = TRUE)) &&
    sum(grepl("fread\\(path, select = PLAN_COLS", code_only)) == 1L
})
check("every read call in the script is on the whitelist", {
  rc <- code_only[grepl("fread\\(|read\\.csv\\(|read_csv\\(|readLines\\(|readRDS\\(|read\\.table\\(|read_yaml\\(|load_config\\(", code_only)]
  ok_pat <- c("fread\\(path, select = PLAN_COLS", "variable_codebook\\.csv", "fread\\(path, colClasses = list\\(character = c\\(\"plan_id\"",
              "wp4_nppes_start\\.csv", "wp4_crosswalk_hash\\.txt", "ledger_ortho\\.csv", "fread\\(out, colClasses", "load_config\\(\\)")
  bad <- rc[!vapply(rc, function(l) any(vapply(ok_pat, function(p) grepl(p, l), logical(1))), logical(1))]
  if (length(bad)) bad else TRUE
})
check("read_plans_allowed() returns exactly the nine columns and no outcome column", {
  x <- E$read_plans_allowed()
  setequal(names(x), PLAN_COLS) && ncol(x) == 9L && nrow(x) == 4159L
})
check("no read of the cached harvest, NPI-level or outcome folders in the script", !any(grepl("\\.cache|harvest/|data_raw|g3_npi|g3_results|g3_sample|api/", code_only)))
check("no sap_check output has a provider name or address column", {
  fs <- list.files("results", pattern = "^sap_check_.*\\.csv$", full.names = TRUE)
  bad <- unlist(lapply(fs, function(f) { h <- strsplit(readLines(f, n = 1L, warn = FALSE), ",")[[1]]; h[grepl("name|address|street|zip|phone", h, ignore.case = TRUE)] }))
  length(bad) == 0L
})
check("sap_check outputs hold no outcome-like column names", {
  fs <- list.files("results", pattern = "^sap_check_.*\\.csv$", full.names = TRUE)
  bad <- unlist(lapply(fs, function(f) { h <- strsplit(readLines(f, n = 1L, warn = FALSE), ",")[[1]]; h[grepl("denied|denial|received|appeal|reason|claims", h, ignore.case = TRUE)] }))
  length(bad) == 0L
})

# ---- 7. NPPES -----------------------------------------------------------------------------
check("NPPES check file exists, no 200, and the design key equals its maximum status", {
  n <- read.csv("results/sap_check_nppes.csv")
  nrow(n) >= 1L && !any(n$http == 200L) && max(n$http) == num("design_nppes_http_status")
})

# ---- 8. text hygiene ----------------------------------------------------------------------
own <- c(SAP, SCRIPT, "tests/test_sap.R", DC, HASH, list.files("results", pattern = "^sap_check_.*\\.csv$", full.names = TRUE))
own <- own[file.exists(own)]
nonascii <- vapply(own, function(f) { l <- readLines(f, warn = FALSE, encoding = "bytes"); any(grepl("[^\\x01-\\x7F]", l, perl = TRUE, useBytes = TRUE)) }, logical(1))
check("write-set files are ASCII only (so no U+2014 either)", if (!any(nonascii)) TRUE else names(nonascii)[nonascii])
check("sap_frozen.md and the script have no em-dash (UTF-8 bytes E2 80 94)", {
  em <- vapply(c(SAP, SCRIPT, "tests/test_sap.R"), function(f) {
    r <- readBin(f, "raw", file.info(f)$size); n <- length(r)
    any(r[1:(n - 2L)] == as.raw(0xE2) & r[2:(n - 1L)] == as.raw(0x80) & r[3:n] == as.raw(0x94)) }, logical(1))
  !any(em)
})
check("sap_frozen.md has no hash character and no tab", !any(grepl("#", lines, fixed = TRUE)) && !any(grepl("\t", lines, fixed = TRUE)))
check("sap_frozen.md uses the required wording: 'per 10,000 reported enrollees', 'per DOUBLING', 'family size', Holm", {
  all(vapply(c("per 10,000 reported enrollees", "DOUBLING", "design_holm_family_size = 2", "Holm"), function(s) grepl(s, sap_txt, fixed = TRUE), logical(1)))
})
check("no conflict copies in the write set", {
  fs <- c(list.files("config", full.names = TRUE), list.files("results", full.names = TRUE), list.files("logs", full.names = TRUE), list.files("R", full.names = TRUE), list.files("tests", full.names = TRUE))
  !any(grepl(" [2-9]( |\\.|$)", basename(fs)))
})

cat("\n", N_PASS, " passed, ", N_FAIL, " failed\n", sep = "")
if (N_FAIL > 0L) { cat("FAILED: ", paste(FAILS, collapse = "; "), "\n", sep = ""); quit(status = 1L) }
