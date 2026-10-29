# =====================================================================
# test_cms_parse.R : WP2 tests (plain R). Run: Rscript "<root>/v1/tests/test_cms_parse.R"
# Exit status 1 if any check fails. Nothing here is tuned to pass. The one place where the CMS file
# itself breaks a property (out-of-network denied > received) is a PINNED KNOWN ANOMALY: the exact set of
# issuers and plans is asserted from the raw workbook and the test fails if that set changes.
# Every in-network check is strict (zero violations).
# NOT tested, on purpose: denial reason tallies versus denied claims (reasons are tallies).
# =====================================================================
.args <- commandArgs(trailingOnly = FALSE)
.f <- sub("^--file=", "", grep("^--file=", .args, value = TRUE)[1])
.here <- dirname(normalizePath(gsub("~+~", " ", .f, fixed = TRUE)))
source(file.path(dirname(.here), "R", "00_setup.R"))
source(file.path(dirname(.here), "R", "ledger.R"))
suppressPackageStartupMessages(library(readxl))

N_PASS <- 0L; N_FAIL <- 0L; FAILS <- character(0)
check <- function(name, expr) {
  res <- tryCatch({ r <- force(expr); if (isTRUE(r)) "ok" else paste0("FALSE", if (is.character(r)) paste0(": ", paste(utils::head(r, 3), collapse = " | ")) else "") },
                  error = function(e) paste0("ERROR: ", conditionMessage(e)))
  if (identical(res, "ok")) { N_PASS <<- N_PASS + 1L; cat("PASS  ", name, "\n", sep = "") }
  else { N_FAIL <<- N_FAIL + 1L; FAILS <<- c(FAILS, name); cat("FAIL  ", name, "  ->  ", res, "\n", sep = "") }
  invisible(NULL)
}
cfg <- load_config()$cms_sources
WB <- cfg$workbook
rd <- function(f, ...) utils::read.csv(file.path(P$clean_cms, f), stringsAsFactors = FALSE, na.strings = c("", "NA"),
                                      colClasses = c(issuer_id = "character", plan_id = "character", state = "character"), ...)
iss <- rd("issuer_state.csv"); pl <- rd("plans.csv"); vol <- rd("issuer_volume.csv"); mr <- rd("mr_urls.csv"); nw <- rd("network_puf.csv")
abs_of <- function(rel) if (startsWith(rel, "CACHE/")) file.path(P$cache, sub("^CACHE/", "", rel)) else file.path(P$data_raw, rel)

# ---- raw files: manifest, hashes, read-only ---------------------------------------
man <- utils::read.csv(file.path(P$provenance, "manifest.csv"), colClasses = "character", stringsAsFactors = FALSE)
check("manifest has the six required columns in order", identical(names(man), c("file", "source_url", "retrieved_utc", "http_status", "bytes", "sha256")))
check("manifest lists 17 files and no duplicates", nrow(man) == 17L && !anyDuplicated(man$file))
check("every manifest file exists", all(file.exists(vapply(man$file, abs_of, character(1)))))
check("raw file SHA-256 hashes match the manifest", {
  h <- vapply(man$file, function(r) sha256_file(abs_of(r))[[1]], character(1), USE.NAMES = FALSE); all(h == man$sha256) })
check("raw file sizes match the manifest", all(file.size(vapply(man$file, abs_of, character(1))) == as.numeric(man$bytes)))
check("every file under data_raw (outside provenance) is in the manifest", {
  f <- list.files(P$data_raw, recursive = TRUE); f <- f[!startsWith(f, "provenance/")]; all(f %in% man$file) })
check("raw files are read-only (no write bit)", all(bitwAnd(as.integer(file.mode(vapply(man$file, abs_of, character(1)))), 146L) == 0L))
check("all downloaded URLs are PY2026 CMS files (folder 2026 or py26) or the KFF working file", all(grepl("/marketplace-puf/2026/|py26", man$source_url) | grepl("files.kff.org", man$source_url, fixed = TRUE)))
check("transparency zip hash equals the hash WP1 recorded", man$sha256[man$file == "cms_py2026/transparency-in-coverage-puf.zip"] == "bcdc435e97bcd954d885a69e3332c28a414a4600dccb715e4d521b403a730193")

# ---- independent re-read of the workbook ------------------------------------------
wbp <- abs_of(man$file[grepl("Transparency_in_Coverage_PUF.xlsx$", man$file)])
raw_tab <- function(sheet) {
  r <- as.data.frame(read_excel(wbp, sheet = sheet, col_names = FALSE, col_types = "text", .name_repair = "minimal"), stringsAsFactors = FALSE)
  hr <- which(apply(r[1:20, , drop = FALSE], 1, function(x) any(x == "Plan_ID", na.rm = TRUE)))[1]
  h <- trimws(as.character(unlist(r[hr, ]))); d <- r[(hr + 1):nrow(r), , drop = FALSE]; names(d) <- h
  d <- d[rowSums(!is.na(d)) > 0, , drop = FALSE]; d[] <- lapply(d, function(v) trimws(as.character(v))); rownames(d) <- NULL
  list(hr = hr, d = d)
}
rq <- raw_tab(WB$sheet_qhp); rs <- raw_tab(WB$sheet_sadp); rh <- raw_tab(WB$sheet_shop)
check("header row is found by the string Plan_ID (row 3), Plan_ID is not in column 1", rq$hr == 3L && which(names(rq$d) == "Plan_ID") == 8L)
check("Individual QHP tab: 44 columns", ncol(rq$d) == WB$expected_columns)
check("Individual QHP tab: 4,159 plan rows (fails loudly if the file changes)", nrow(rq$d) == 4159L)
check("Individual SADP tab 652 rows and SHOP tab 145 rows (documented, not used)", nrow(rs$d) == 652L && nrow(rh$d) == 145L)
check("raw QHP tab has 185 distinct Issuer_ID and 30 states", length(unique(rq$d$Issuer_ID)) == 185L && length(unique(rq$d$State)) == 30L)
check("workbook has exactly the four expected sheets", setequal(excel_sheets(wbp), c(WB$sheet_disclaimer, WB$sheet_qhp, WB$sheet_sadp, WB$sheet_shop)))

# ---- parsed counts tie to raw -------------------------------------------------------
check("plans.csv has 4,159 rows = raw plan rows, same Plan_ID set", nrow(pl) == nrow(rq$d) && setequal(pl$plan_id, rq$d$Plan_ID))
check("issuer_state.csv has 185 rows = raw distinct Issuer_ID, same set", nrow(iss) == 185L && setequal(iss$issuer_id, rq$d$Issuer_ID))
check("issuer_volume.csv has the same 185 issuers", nrow(vol) == 185L && setequal(vol$issuer_id, iss$issuer_id))
check("30 states in issuer_state.csv", length(unique(iss$state)) == 30L)
check("tabs_summary.csv ties to raw row counts", { ts <- utils::read.csv(file.path(P$clean_cms, "tabs_summary.csv"), stringsAsFactors = FALSE); all(ts$data_rows == c(nrow(rq$d), nrow(rs$d), nrow(rh$d))) })
check("Plan_Type values are EPO/HMO/POS/PPO only (no Indemnity) in the QHP tab", setequal(unique(pl$plan_type), c("EPO", "HMO", "POS", "PPO")))

# ---- keys -----------------------------------------------------------------------------
check("no duplicate plan_id in plans.csv", !anyDuplicated(pl$plan_id))
check("no duplicate issuer_id in issuer_state.csv or issuer_volume.csv", !anyDuplicated(iss$issuer_id) && !anyDuplicated(vol$issuer_id))
check("no duplicate issuer-state key; each issuer is in one state", !anyDuplicated(paste(iss$issuer_id, iss$state)) && all(tapply(rq$d$State, rq$d$Issuer_ID, function(s) length(unique(s))) == 1L))
check("every plan_id is 14 characters and starts with its issuer_id", all(nchar(pl$plan_id) == 14L) && all(substr(pl$plan_id, 1, 5) == pl$issuer_id))
check("no duplicate issuer in mr_urls.csv", !anyDuplicated(mr$issuer_id))
check("repeated issuer-level values are identical across an issuer's plan rows (raw)", {
  ic <- names(rq$d)[13:24]; all(vapply(c(ic, "Issuer_Name", "State", "Exchange_Type", "URL_Claims_Payment_Policies"), function(cn) all(tapply(rq$d[[cn]], rq$d$Issuer_ID, function(v) length(unique(v))) == 1L), logical(1))) })

# ---- denied <= received, rates in [0,1] (strict, recomputed from the published counts) ----
cmp <- function(d, rcol, dcol) { ok <- !is.na(d[[rcol]]) & !is.na(d[[dcol]]); list(n = sum(ok), bad = sum(ok & d[[dcol]] > d[[rcol]])) }
msg_bad <- function(x, lab) if (x$bad == 0L) TRUE else paste0(lab, ": ", x$bad, " of ", x$n, " comparable rows have denied > received (a property of the CMS file)")
check("in-network denied <= received, issuer level (non-NA rows)", msg_bad(cmp(iss, "issuer_claims_received_in_network", "issuer_claims_denied_in_network"), "issuer in-network"))
check("in-network denied <= received, plan level (non-NA rows)", msg_bad(cmp(pl, "plan_claims_received_in_network", "plan_claims_denied_in_network"), "plan in-network"))
rate_ok <- function(d, rcol, dcol) { r <- d[[dcol]] / d[[rcol]]; r <- r[is.finite(r)]; all(r >= 0 & r <= 1) }
check("in-network rates recomputed from counts are within [0,1], issuer and plan", rate_ok(iss, "issuer_claims_received_in_network", "issuer_claims_denied_in_network") && rate_ok(pl, "plan_claims_received_in_network", "plan_claims_denied_in_network"))
check("the derived rate columns written to the CSVs are all within [0,1]", all(c(iss$in_network_denial_rate, iss$out_of_network_denial_rate, pl$plan_in_network_denial_rate, pl$plan_out_of_network_denial_rate) >= 0 &
      c(iss$in_network_denial_rate, iss$out_of_network_denial_rate, pl$plan_in_network_denial_rate, pl$plan_out_of_network_denial_rate) <= 1, na.rm = TRUE))

# ---- OUT-OF-NETWORK: PINNED KNOWN ANOMALY (not a blanket exemption) ---------------------------------------
# The CMS file itself has out-of-network denied > received for 2 issuers and 30 plans (checked against the raw
# workbook, read as text). The raw counts are never altered. Issuer 73836 (Moda Health Plan, AK): issuer-level
# received 35,374, denied 66,671, while its 7 plan rows sum to 35,374 received and only 6,671 denied, so the
# issuer-level 66,671 looks like a stray leading digit (an inference). Issuer 37160 (Blue Cross Blue Shield of
# North Dakota): received 7,251, denied 12,362; 8 of its 14 plan rows exceed. KFF's own working file carries
# identical numbers for these two issuers (Medical Issuers sheet) and flags the same 30 plans (Medical Plans
# sheet), so the anomaly is in the CMS source. In-network claims have NO such rows and stay strictly checked.
# The block below asserts EXACTLY this set (issuers, per-issuer plan counts, the 30 plan IDs) from the raw
# workbook and from the parsed tables, so any change (new vintage, parser change) fails the test.
OON_ISS_PIN <- data.frame(issuer_id = c("37160", "73836"), received = c(7251, 35374), denied = c(12362, 66671), stringsAsFactors = FALSE)
OON_PLAN_N_PIN <- c("14002" = 3L, "26065" = 15L, "34762" = 2L, "37160" = 8L, "39520" = 1L, "77552" = 1L)
OON_PLANS_PIN <- c("14002TN0400222", "14002TN0400226", "14002TN0400257", "26065SC0380023", "26065SC0670002", "26065SC0690002", "26065SC0700002",
                   "26065SC0700003", "26065SC0700006", "26065SC0710002", "26065SC0710003", "26065SC0710006", "26065SC0720002", "26065SC0720003",
                   "26065SC0720006", "26065SC0730002", "26065SC0730003", "26065SC0730006", "34762MO0590010", "34762MO0590019", "37160ND2410002",
                   "37160ND2410020", "37160ND2410027", "37160ND2410028", "37160ND2480001", "37160ND2480003", "37160ND2480009", "37160ND2480010",
                   "39520KS0040073", "77552OH0010202")
rawnum <- function(v) ifelse(grepl("^[0-9]+(\\.[0-9]+)?$", v), suppressWarnings(as.numeric(v)), NA_real_)
set_diff_msg <- function(got, want) if (setequal(got, want) && length(got) == length(want)) TRUE else
  paste0("got {", paste(setdiff(got, want), collapse = ","), "} not pinned; pinned {", paste(setdiff(want, got), collapse = ","), "} missing; n = ", length(got), " vs ", length(want))
r1 <- rq$d[!duplicated(rq$d$Issuer_ID), ]
raw_iss_bad <- r1$Issuer_ID[which(rawnum(r1$Issuer_Claims_Denied_Out_of_Network) > rawnum(r1$Issuer_Claims_Received_Out_of_Network))]
raw_plan_bad <- rq$d$Plan_ID[which(rawnum(rq$d$Plan_Number_Claims_Denied_Out_of_Network) > rawnum(rq$d$Plan_Number_Claims_Received_Out_of_Network))]
raw_plan_bad_n <- table(substr(raw_plan_bad, 1, 5))
check("OON anomaly, raw workbook: out-of-network denied > received at issuer level for exactly issuers 37160 and 73836", set_diff_msg(raw_iss_bad, OON_ISS_PIN$issuer_id))
check("OON anomaly, raw workbook: pinned issuer-level received and denied counts (7,251/12,362 and 35,374/66,671)", all(vapply(seq_len(nrow(OON_ISS_PIN)), function(i) {
  k <- r1[r1$Issuer_ID == OON_ISS_PIN$issuer_id[i], ]; nrow(k) == 1L && rawnum(k$Issuer_Claims_Received_Out_of_Network) == OON_ISS_PIN$received[i] && rawnum(k$Issuer_Claims_Denied_Out_of_Network) == OON_ISS_PIN$denied[i] }, logical(1))))
check("OON anomaly, raw workbook: out-of-network denied > received at plan level for exactly the 30 pinned plan IDs", set_diff_msg(raw_plan_bad, OON_PLANS_PIN))
check("OON anomaly, raw workbook: per-issuer plan counts 26065 (15), 37160 (8), 14002 (3), 34762 (2), 39520 (1), 77552 (1); 30 plans, 6 issuers", {
  identical(as.integer(raw_plan_bad_n[names(OON_PLAN_N_PIN)]), as.integer(OON_PLAN_N_PIN)) && length(raw_plan_bad_n) == length(OON_PLAN_N_PIN) && sum(raw_plan_bad_n) == 30L && length(raw_plan_bad) == 30L })
check("OON anomaly, raw workbook: issuer 73836 has 7 plan rows summing to 35,374 received and 6,671 denied; issuer 37160 has 14 plan rows", {
  d73 <- rq$d[rq$d$Issuer_ID == "73836", ]; d37 <- rq$d[rq$d$Issuer_ID == "37160", ]
  nrow(d73) == 7L && sum(rawnum(d73$Plan_Number_Claims_Received_Out_of_Network), na.rm = TRUE) == 35374 && sum(rawnum(d73$Plan_Number_Claims_Denied_Out_of_Network), na.rm = TRUE) == 6671 && nrow(d37) == 14L })
oon_flag_ids <- function(d, key) d[[key]][which(d$out_of_network_denied_exceeds_received %in% TRUE)]
check("OON anomaly, parsed tables: the flag is TRUE for exactly the two pinned issuers (issuer_state.csv)", set_diff_msg(oon_flag_ids(iss, "issuer_id"), OON_ISS_PIN$issuer_id))
check("OON anomaly, parsed tables: the flag is TRUE for exactly the 30 pinned plans (plans.csv), 30 rows", {
  m <- set_diff_msg(oon_flag_ids(pl, "plan_id"), OON_PLANS_PIN); if (!isTRUE(m)) m else sum(pl$out_of_network_denied_exceeds_received, na.rm = TRUE) == 30L })
check("OON anomaly, parsed tables: raw counts are kept as published for the flagged rows (not altered, not dropped)", {
  a <- iss[match(OON_ISS_PIN$issuer_id, iss$issuer_id), ]
  all(a$issuer_claims_received_out_of_network == OON_ISS_PIN$received) && all(a$issuer_claims_denied_out_of_network == OON_ISS_PIN$denied) &&
    all(pl$plan_claims_denied_out_of_network[pl$plan_id %in% OON_PLANS_PIN] == rawnum(rq$d$Plan_Number_Claims_Denied_Out_of_Network[match(pl$plan_id[pl$plan_id %in% OON_PLANS_PIN], rq$d$Plan_ID)])) })
check("OON anomaly, parsed tables: derived out-of-network denial rate is NA for every flagged row (issuer and plan)", {
  all(is.na(iss$out_of_network_denial_rate[iss$issuer_id %in% OON_ISS_PIN$issuer_id])) && all(is.na(pl$plan_out_of_network_denial_rate[pl$plan_id %in% OON_PLANS_PIN])) })
oon_rest_ok <- function(d, rcol, dcol, rate, flag) {
  rest <- !(d[[flag]] %in% TRUE); r <- d[[rcol]][rest]; x <- d[[dcol]][rest]; er <- d[[rate]][rest]
  comparable <- !is.na(r) & !is.na(x)
  strict <- sum(comparable & x > r) == 0L                                         # no other row violates
  rate_in_range <- all(er >= 0 & er <= 1, na.rm = TRUE)                           # every derived rate is a proportion
  na_explained <- identical(is.na(er), !comparable | (comparable & r == 0))       # NA only for a marker or received = 0
  strict && rate_in_range && na_explained }
check("OON anomaly, no other row violates (issuer level): denied <= received, rates in [0,1], rate NA only for a marker or received = 0", oon_rest_ok(iss, "issuer_claims_received_out_of_network", "issuer_claims_denied_out_of_network", "out_of_network_denial_rate", "out_of_network_denied_exceeds_received"))
check("OON anomaly, no other row violates (plan level): denied <= received, rates in [0,1], rate NA only for a marker or received = 0", oon_rest_ok(pl, "plan_claims_received_out_of_network", "plan_claims_denied_out_of_network", "plan_out_of_network_denial_rate", "out_of_network_denied_exceeds_received"))
check("OON anomaly, pinned counts (INFO): 2 of 159 comparable issuers and 30 of 2,411 comparable plans", {
  ci <- cmp(iss, "issuer_claims_received_out_of_network", "issuer_claims_denied_out_of_network"); cp <- cmp(pl, "plan_claims_received_out_of_network", "plan_claims_denied_out_of_network")
  cat("INFO  out-of-network comparable rows: issuers ", ci$n, " (", ci$bad, " violate), plans ", cp$n, " (", cp$bad, " violate)\n", sep = "")
  ci$n == 159L && ci$bad == 2L && cp$n == 2411L && cp$bad == 30L })
check("resubmitted is NOT assumed to be a subset of denied (no column or test ties them)", !any(grepl("resubmitted_subset|resubmitted_of_denied", names(pl))))
check("no column treats denial reason tallies as unique claims", !any(grepl("reason_count_is_unique_claim", c(names(pl), names(iss)))))

# ---- markers: every non-numeric marker is NA with a marker value ------------------------------
MK <- c("numeric", "star1", "star2", "star3", "na")
rawmark <- function(v) ifelse(v == "*", "star1", ifelse(v == "**", "star2", ifelse(v == "***", "star3", ifelse(v == "N/A", "na", "numeric"))))
mcols <- function(d) grep("_marker$", names(d), value = TRUE)
check("every marker column holds only numeric/star1/star2/star3/na (no NA, no boolean)", all(vapply(c(mcols(iss), mcols(pl)), function(cn) {
  d <- if (cn %in% names(pl)) pl else iss; all(d[[cn]] %in% MK) }, logical(1))))
check("typed value is NA exactly when its marker is not numeric (issuer_state, plans)", all(vapply(c(mcols(iss), mcols(pl)), function(cn) {
  d <- if (cn %in% names(pl)) pl else iss; v <- d[[sub("_marker$", "", cn)]]; identical(is.na(v), d[[cn]] != "numeric") }, logical(1))))
SPECRAW <- c(plan_claims_received_out_of_network = "Plan_Number_Claims_Received_Out_of_Network", plan_claims_received_in_network = "Plan_Number_Claims_Received_In_Network",
             plan_claims_denied_out_of_network = "Plan_Number_Claims_Denied_Out_of_Network", plan_claims_denied_in_network = "Plan_Number_Claims_Denied_In_Network",
             plan_claims_resubmitted_out_of_network = "Plan_Number_Claims_Resubmitted_Out_of_Network", plan_claims_resubmitted_in_network = "Plan_Number_Claims_Resubmitted_In_Network",
             plan_reason_referral_or_prior_auth_required = "Plan_Number_Claims_Denied_Referral_Required", plan_reason_out_of_network_provider = "Plan_Number_Claims_Denied_Due_To_Out_Of_Network",
             plan_reason_service_excluded = "Plan_Number_Claims_Denied_Services_Excluded",
             plan_reason_medical_necessity_excl_behavioral_health = "Plan_Number_Claims_Denied_Not_Medically_Necessary_Excluding_Behavioral_Health",
             plan_reason_medical_necessity_behavioral_health = "Plan_Number_Claims_Denied_Not_Medically_Necessary_Behavioral_Health_Only",
             plan_reason_benefit_limit_reached = "Plan_Number_Claims_Denied_Due_To_Enrolle_Benefit_Limit_Reached", plan_reason_member_not_covered = "Plan_Number_Claims_Denied_Due_To_Member_Not_Covered",
             plan_reason_investigational_experimental_cosmetic = "Plan_Number_Claims_Denied_Due_To_Investigational_Experimental_Cosmetic_Proceduce",
             plan_reason_administrative = "Plan_Number_Claims_Denied_Due_To_Administrative_Reason", plan_reason_other = "Plan_Number_Claims_Denied_Other",
             avg_monthly_enrollment = "Average Monthly Enrollment", avg_monthly_disenrollment = "Average Monthly Disenrollment")
check("plan-level and enrollment columns: parsed value and marker equal the raw cell for every plan row", all(vapply(names(SPECRAW), function(nm) {
  rawv <- rq$d[[SPECRAW[[nm]]]][match(pl$plan_id, rq$d$Plan_ID)]
  mk <- rawmark(rawv); isn <- mk == "numeric"
  identical(pl[[paste0(nm, "_marker")]], mk) && all(is.na(pl[[nm]][!isn])) && all(pl[[nm]][isn] == as.numeric(rawv[isn])) }, logical(1))))
ISSRAW <- c(issuer_claims_received_out_of_network = 13, issuer_claims_received_in_network = 14, issuer_claims_denied_out_of_network = 15, issuer_claims_denied_in_network = 16,
            issuer_claims_resubmitted_out_of_network = 17, issuer_claims_resubmitted_in_network = 18, issuer_internal_appeals_filed = 19, issuer_internal_appeals_overturned = 20,
            issuer_internal_appeals_overturned_percent = 21, issuer_external_appeals_filed = 22, issuer_external_appeals_overturned = 23, issuer_external_appeals_overturned_percent = 24)
check("issuer-level columns: parsed value and marker equal the raw cell for every issuer", all(vapply(names(ISSRAW), function(nm) {
  rawv <- rq$d[[ISSRAW[[nm]]]][match(iss$issuer_id, rq$d$Issuer_ID)]
  mk <- rawmark(rawv); isn <- mk == "numeric"
  identical(iss[[paste0(nm, "_marker")]], mk) && all(is.na(iss[[nm]][!isn])) && all(iss[[nm]][isn] == as.numeric(rawv[isn])) }, logical(1))))
check("only the four documented marker tokens occur in the raw numeric columns", all(vapply(c(names(rq$d)[13:40], names(rq$d)[43:44]), function(cn) {
  v <- rq$d[[cn]]; all(v %in% c("*", "**", "***", "N/A") | grepl("^[0-9]+(\\.[0-9]+)?$", v)) }, logical(1))))
check("raw-workbook trap: text-typed digit-string count cells exist (825 in Issuer_Claims_Resubmitted_Out_of_Network, 30 in Issuer_Claims_Received_In_Network, none elsewhere); a type-guessing sum gives 449,498,677 not 451,249,792", {
  rl <- suppressMessages(read_excel(wbp, sheet = WB$sheet_qhp, col_names = FALSE, col_types = "list", .name_repair = "minimal"))
  idx <- which(vapply(rl[[8]], function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x), logical(1))); idx <- idx[idx > rq$hr]
  is_td <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && grepl("^[0-9]+(\\.[0-9]+)?$", x)
  ncols <- names(rq$d)[c(13:40, 43:44)]
  td <- vapply(ncols, function(cn) sum(vapply(rl[[which(names(rq$d) == cn)]][idx], is_td, logical(1))), integer(1))
  v <- vapply(rl[[which(names(rq$d) == "Issuer_Claims_Received_In_Network")]][idx], function(x) if (is.numeric(x) && length(x) == 1L && !is.na(x)) as.numeric(x) else NA_real_, numeric(1))
  naive <- sum(v[!duplicated(rq$d$Issuer_ID)], na.rm = TRUE)
  length(idx) == nrow(rq$d) && td[["Issuer_Claims_Resubmitted_Out_of_Network"]] == 825L && td[["Issuer_Claims_Received_In_Network"]] == 30L && sum(td) == 855L &&
    naive == 449498677 && sum(iss$issuer_claims_received_in_network, na.rm = TRUE) == 451249792 })
check("marker counts per variable were written (marker_counts.csv) and tie to the parsed tables", {
  mc <- utils::read.csv(file.path(P$clean_cms, "marker_counts.csv"), stringsAsFactors = FALSE)
  all(mc$numeric + mc$star1 + mc$star2 + mc$star3 + mc$na == mc$rows) && sum(mc$star2[mc$variable == "plan_claims_denied_in_network"]) == sum(pl$plan_claims_denied_in_network_marker == "star2") })
check("no numeric 1 to 10 in any claim, reason or appeal count column, while 11 to 13 are common (evidence that ** stands for 1 to 10)", {
  cnt <- c(setdiff(grep("^issuer_(claims|internal_appeals|external_appeals)_(?!.*percent)", names(iss), perl = TRUE, value = TRUE), grep("_marker$", names(iss), value = TRUE)),
           grep("^plan_(claims|reason)_", names(pl), value = TRUE))
  cnt <- cnt[!grepl("_marker$|rate$", cnt)]
  vals <- unlist(lapply(cnt, function(cn) { d <- if (cn %in% names(pl)) pl else iss; d[[cn]] }))
  tot <- sum(vals >= 1 & vals <= 10, na.rm = TRUE); n1113 <- sum(vals >= 11 & vals <= 13, na.rm = TRUE)
  if (length(cnt) != 26L) paste0("expected 26 count columns, found ", length(cnt)) else if (n1113 < 100) paste0("only ", n1113, " cells with values 11 to 13: the check would be vacuous")
  else if (tot == 0) TRUE else paste0(tot, " numeric cells with a value from 1 to 10") })
check("smallest numeric average monthly enrollment is 11 (no value from 1 to 10)", min(pl$avg_monthly_enrollment, na.rm = TRUE) == 11 && sum(pl$avg_monthly_enrollment >= 1 & pl$avg_monthly_enrollment <= 10, na.rm = TRUE) == 0L)
check("the written QA report and codebook state ** as an inferred 1 to 10, not the stale 1 to 9", {
  md <- readLines(file.path(P$qa, "g2_reconciliation.md"), warn = FALSE); cbt <- readLines(file.path(P$config, "variable_codebook.csv"), warn = FALSE)
  !any(grepl("meaning \\(1 to 9\\)", md)) && any(grepl("^- The \\*\\* meaning is an inference: no numeric cell holds a value from 1 to 10", md)) && any(grepl("most likely 1 to 10, inferred", cbt, fixed = TRUE)) })
check("percent-overturned columns are on a 0 to 100 scale", { v <- c(iss$issuer_internal_appeals_overturned_percent, iss$issuer_external_appeals_overturned_percent); all(v >= 0 & v <= 100, na.rm = TRUE) && max(v, na.rm = TRUE) > 1 })

# ---- other tables ----------------------------------------------------------------------------
check("issuer_volume: in-network shares sum to 1 and total equals the sum of numeric in-network counts", abs(sum(vol$share_of_national_in_network_claims, na.rm = TRUE) - 1) < 1e-12 && sum(vol$in_network_claims_received, na.rm = TRUE) == 451249792)
check("issuer_volume: 157 issuers have more than 1,000 in-network claims", sum(vol$in_network_claims_over_1000) == 157L)
check("mr_urls.csv: 346 issuers, all URLs start with http(s), no contact column", nrow(mr) == 346L && all(grepl("^https?://", mr$mr_index_url)) && !any(grepl("email|poc", names(mr), ignore.case = TRUE)))
check("mr_urls.csv covers 183 of the 185 QHP issuers; the two missing are 13591 and 40356", sum(iss$issuer_id %in% mr$issuer_id) == 183L && setequal(setdiff(iss$issuer_id, mr$issuer_id), c("13591", "40356")))
check("network_puf.csv has 548 rows", nrow(nw) == 548L)
check("Plan Attributes join: 4,043 plans matched; plan type, issuer, state agree with the PUF where matched", {
  m <- pl$pa_matched; sum(m) == 4043L && all(pl$plan_type[m] == pl$pa_plan_type[m]) && all(pl$issuer_id[m] == pl$pa_issuer_id[m]) && all(pl$state[m] == pl$pa_state[m]) })
check("Expanded Bronze plans carry metal level Bronze in the PUF", all(pl$metal_level[which(pl$pa_expanded_bronze %in% TRUE)] == "Bronze"))
check("dictionary_vs_workbook.csv has one row per workbook column", nrow(utils::read.csv(file.path(P$clean_cms, "dictionary_vs_workbook.csv"))) == 44L)
check("the Tech POC email column was not carried into any derived file", !any(vapply(list.files(P$clean_cms, full.names = TRUE), function(f) any(grepl("@", readLines(f, warn = FALSE), fixed = TRUE)), logical(1))))

# ---- config files ---------------------------------------------------------------------------------
cb <- utils::read.csv(file.path(P$config, "variable_codebook.csv"), stringsAsFactors = FALSE, na.strings = character(0))
check("codebook has the columns variable, label, unit, source_tab, notes", all(c("variable", "label", "unit", "source_tab", "notes") %in% names(cb)))
check("codebook covers every column of issuer_state, plans and issuer_volume", all(c(names(iss), names(pl), names(vol)) %in% cb$variable))
check("codebook labels are human readable (no underscores)", !any(grepl("_", cb$label)))
rm <- utils::read.csv(file.path(P$config, "denial_reason_map.csv"), stringsAsFactors = FALSE)
check("denial_reason_map has the ten reasons, all present in plans.csv, nine marked as in KFF's 79 million", nrow(rm) == 10L && all(rm$variable %in% names(pl)) && sum(rm$included_in_kff_79m) == 9L)
check("cms_sources.yaml loads with load_config() without key collision and ties to the parse", { x <- load_config(); "cms_sources" %in% names(x) && x$cms_sources$workbook$expected_data_rows$qhp == nrow(pl) && x$cms_sources$workbook$expected_issuers_qhp == nrow(iss) })
check("cms_sources.yaml plan year 2026 / data year 2024", cfg$plan_year == 2026 && cfg$data_year == 2024)

# ---- G2 table and ledger -----------------------------------------------------------------------------
g2 <- utils::read.csv(file.path(P$results, "g2_reconciliation.csv"), stringsAsFactors = FALSE, na.strings = c("", "NA"))
check("g2_reconciliation.csv has the required columns", all(c("benchmark", "kff_sentence", "ours", "difference", "tolerance_low", "tolerance_high", "tolerance_rule", "pass", "status", "explanation") %in% names(g2)))
check("g2 statuses are from the allowed set and every difference carries an explanation", all(g2$status %in% c("reproduces exactly", "reproduces", "differs, explained", "differs, unexplained", "not testable")) &&
        all(!is.na(g2$explanation[!g2$status %in% c("reproduces", "reproduces exactly")])))
check("g2: every row marked reproduces lies inside its tolerance, no row marked pass lies outside", all(g2$status[g2$pass %in% TRUE] %in% c("reproduces", "reproduces exactly")) &&
        all(g2$ours[g2$pass %in% TRUE] >= g2$tolerance_low[g2$pass %in% TRUE] & g2$ours[g2$pass %in% TRUE] <= g2$tolerance_high[g2$pass %in% TRUE]))
check("g2: headline national totals and rates (A01 to A08) all reproduce", all(g2$status[grepl("^A0", g2$id)] %in% c("reproduces", "reproduces exactly")) && sum(grepl("^A0", g2$id)) == 8L)
check("g2: internal appeals 262,982 and external appeals 5,881 reproduce exactly", all(g2$status[g2$id %in% c("D01", "D05")] == "reproduces exactly"))
check("g2: the KFF working-file issuer set and issuer counts are identical to ours (E01, E02)", all(g2$status[g2$id %in% c("E01", "E02")] == "reproduces exactly"))
cat("INFO  G2 status counts: ", paste(names(table(g2$status)), table(g2$status), sep = " = ", collapse = "; "), "\n", sep = "")
cat("INFO  G2 rows differing without a verified explanation: ", paste(g2$id[g2$status == "differs, unexplained"], collapse = ", "), "\n", sep = "")
rdl <- function(part) utils::read.csv(file.path(P$ledger_parts, paste0("ledger_", part, ".csv")), colClasses = "character", na.strings = character(0), stringsAsFactors = FALSE)
lall <- rdl("kff"); lk <- lall[startsWith(lall$key, "kff_"), ]; la <- lall[startsWith(lall$key, "att_"), ]
check("ledger part kff: columns, snake_case keys, only kff_ and att_ prefixes, numeric values, no duplicates", {
  identical(names(lall), c("key", "value", "unit", "description", "source_script")) && all(grepl("^[a-z][a-z0-9]*(_[a-z0-9]+)*$", lall$key)) &&
    !anyDuplicated(lall$key) && all(nzchar(lall$value)) && !any(is.na(suppressWarnings(as.numeric(lall$value)))) && nrow(lk) + nrow(la) == nrow(lall) && nrow(lk) > 0 && nrow(la) > 0 })
check("no stray ledger_att.csv (att_ keys live in ledger_kff.csv)", !file.exists(file.path(P$ledger_parts, "ledger_att.csv")))
check("att ledger totals tie to the sum of reported issuer-level cells", {
  g <- function(k) as.numeric(la$value[la$key == k]); g("att_claims_received_in_network") == sum(iss$issuer_claims_received_in_network, na.rm = TRUE) &&
    g("att_claims_denied_in_network") == sum(iss$issuer_claims_denied_in_network, na.rm = TRUE) && g("att_internal_appeals_filed") == sum(iss$issuer_internal_appeals_filed, na.rm = TRUE) &&
    g("att_external_appeals_filed") == sum(iss$issuer_external_appeals_filed, na.rm = TRUE) && g("att_claims_resubmitted_in_network") == sum(iss$issuer_claims_resubmitted_in_network, na.rm = TRUE) })
check("kff ledger values equal the G2 table values (A02, D01, D05)", {
  g <- function(k) as.numeric(lk$value[lk$key == k]); o <- function(i) g2$ours[g2$id == i]
  g("kff_claims_received_in_network") == o("A02") && g("kff_internal_appeals_filed") == o("D01") && g("kff_external_appeals_filed") == o("D05") })
check("ledger parts read cleanly with the shared reader checks (no NA/Inf text, no leading-zero IDs)", {
  tmp <- file.path(tempdir(), "ledger_check"); dir.create(tmp, showWarnings = FALSE)
  file.copy(file.path(P$ledger_parts, "ledger_kff.csv"), tmp, overwrite = TRUE)
  L <- ledger_read_all(dir = tmp, design_csv = file.path(tmp, "none.csv")); nrow(L) == nrow(lall) })

# ---- hygiene -------------------------------------------------------------------------------------------
own_src <- file.path(P$R, c("01_download_cms.R", "02_parse_cms.R")); own_src <- c(own_src, file.path(P$tests, "test_cms_parse.R"))
check("owned R sources are ASCII", all(vapply(own_src, function(f) { assert_ascii_source(f); TRUE }, logical(1))))
emd <- intToUtf8(0x2014)
own_text <- c(list.files(P$clean_cms, full.names = TRUE), file.path(P$config, c("cms_sources.yaml", "variable_codebook.csv", "denial_reason_map.csv")),
              file.path(P$results, "g2_reconciliation.csv"), file.path(P$qa, "g2_reconciliation.md"), file.path(P$ledger_parts, "ledger_kff.csv"), own_src)
check("no em-dash in any owned text output or source", !any(vapply(own_text, function(f) any(grepl(emd, readLines(f, warn = FALSE, encoding = "UTF-8"), fixed = TRUE)), logical(1))))
conflict <- list.files(P$v1, recursive = TRUE, pattern = " [23](\\.[A-Za-z0-9]+)?$", full.names = TRUE)
check("no iCloud conflict copies ('name 2') under v1", length(conflict) == 0L)

cat("\n=====================================================================\n")
cat("test_cms_parse.R: ", N_PASS, " passed, ", N_FAIL, " failed (", N_PASS + N_FAIL, " checks)\n", sep = "")
if (N_FAIL) { cat("FAILED: ", paste(FAILS, collapse = "; "), "\n", sep = ""); quit(status = 1L) }
