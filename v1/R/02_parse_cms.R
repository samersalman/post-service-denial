# =====================================================================
# 02_parse_cms.R : WP2 steps 3 to 9. Parse the CMS PY2026 files, reconcile to KFF.
# =====================================================================
# PART A  parse
#   Individual QHP tab -> issuer_state.csv, plans.csv, issuer_volume.csv
#   MR-URL PUF -> mr_urls.csv, Network PUF -> network_puf.csv
#   Plan Attributes and Plan ID Crosswalk joined ONLY for what the PUF lacks
#   codebook, denial-reason map, marker counts, dictionary-vs-workbook table
# PART B  G2 reconciliation to KFF (suppressed-as-zero rule used HERE ONLY)
#   results/g2_reconciliation.csv, qa/g2_reconciliation.md, ledger_kff.csv
#
# Markers (kept, never imputed): "*" star1, "**" star2, "***" star3, "N/A" na.
# Every typed column <name> is NA for a marker and has <name>_marker with one of
# numeric / star1 / star2 / star3 / na.
# Run: Rscript "<root>/v1/R/02_parse_cms.R"   (after 01_download_cms.R)
# =====================================================================
.args <- commandArgs(trailingOnly = FALSE)
.f <- sub("^--file=", "", grep("^--file=", .args, value = TRUE)[1])
.here <- dirname(normalizePath(gsub("~+~", " ", .f, fixed = TRUE)))
source(file.path(.here, "00_setup.R"))
source(file.path(.here, "ledger.R"))
ensure_pkgs(c("digest", "yaml", "readr", "readxl", "data.table"))
suppressPackageStartupMessages({ library(data.table) })

LOG <- file.path(P$logs, "wp2_parse.log")
lg <- function(..., level = "INFO") log_msg(..., level = level, file = LOG)
SCRIPT <- "02_parse_cms.R"
cfg <- load_config()$cms_sources
WB <- cfg$workbook
RAW <- file.path(P$data_raw, cfg$download_dir)
EXT <- file.path(RAW, "extracted")
CEXT <- file.path(P$cache, cfg$download_dir, "extracted")
OUT <- P$clean_cms
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
EMD <- intToUtf8(0x2014)

# ---- 0. raw integrity ---------------------------------------------------
man <- utils::read.csv(file.path(P$provenance, "manifest.csv"), colClasses = "character", stringsAsFactors = FALSE)
abs_of <- function(rel) if (startsWith(rel, "CACHE/")) file.path(P$cache, sub("^CACHE/", "", rel)) else file.path(P$data_raw, rel)
hs <- vapply(man$file, function(r) sha256_file(abs_of(r))[[1]], character(1), USE.NAMES = FALSE)
if (any(hs != man$sha256)) stop("Raw file hash differs from manifest: ", paste(man$file[hs != man$sha256], collapse = "; "))
lg("raw integrity: ", nrow(man), " files match the manifest")

wb_path <- file.path(EXT, "transparency-in-coverage-puf", WB$file)
pa_path <- file.path(CEXT, "plan-attributes-puf", "plan-attributes-puf.csv")
cx_path <- file.path(CEXT, "plan-id-crosswalk-puf", "plan-id-crosswalk-puf.csv")
nw_path <- file.path(EXT, "network-puf", "network-puf.csv")
mr_path <- file.path(EXT, "machine-readable-url-puf", "machine-readable-url-puf.xlsx")
kff_path <- file.path(P$data_raw, "kff_2024", "2024-kff-transparency-data-working-file.xlsx")
dict_path <- file.path(RAW, "transparency-coverage-puf-datadictionary-py26.pdf")
disc_path <- file.path(RAW, "transparency-coverage-datadisclaimer-py26.pdf-0")
for (p in c(wb_path, pa_path, cx_path, nw_path, mr_path, kff_path, dict_path, disc_path)) if (!file.exists(p)) stop("missing: ", p)

# ---- 1. column specification (44 columns of the Individual QHP tab) -------
# raw header (verbatim) | clean name | kind | label | unit
SPEC <- data.frame(
  raw = c("Individual/SHOP", "Exchange_Type", "State", "Issuer_Name", "Issuer_ID",
          "Is_Issuer_New_to_Exchange?(Yes_or_No)", "SADP_Only", "Plan_ID", "Plan_Type", "QHP or SADP?",
          "Metal_Level", "URL_Claims_Payment_Policies",
          "Issuer_Claims_Received_Out_of_Network", "Issuer_Claims_Received_In_Network",
          "Issuer_Claims_Denied_Out_of_Network", "Issuer_Claims_Denied_In_Network",
          "Issuer_Claims_Resubmitted_Out_of_Network", "Issuer_Claims_Resubmitted_In_Network",
          "Issuer_Internal_Appeals_Filed", "Issuer_Number_Internal_Appeals_Overturned",
          "Issuer_Percent_Internal_Appeals_Overturned", "Issuer_External_Appeals_Filed",
          "Issuer_Number_External_Appeals_Overturned", "Issuer_Percent_External_Appeals_Overturned",
          "Plan_Number_Claims_Received_Out_of_Network", "Plan_Number_Claims_Received_In_Network",
          "Plan_Number_Claims_Denied_Out_of_Network", "Plan_Number_Claims_Denied_In_Network",
          "Plan_Number_Claims_Resubmitted_Out_of_Network", "Plan_Number_Claims_Resubmitted_In_Network",
          "Plan_Number_Claims_Denied_Referral_Required", "Plan_Number_Claims_Denied_Due_To_Out_Of_Network",
          "Plan_Number_Claims_Denied_Services_Excluded",
          "Plan_Number_Claims_Denied_Not_Medically_Necessary_Excluding_Behavioral_Health",
          "Plan_Number_Claims_Denied_Not_Medically_Necessary_Behavioral_Health_Only",
          "Plan_Number_Claims_Denied_Due_To_Enrolle_Benefit_Limit_Reached",
          "Plan_Number_Claims_Denied_Due_To_Member_Not_Covered",
          "Plan_Number_Claims_Denied_Due_To_Investigational_Experimental_Cosmetic_Proceduce",
          "Plan_Number_Claims_Denied_Due_To_Administrative_Reason", "Plan_Number_Claims_Denied_Other",
          "Rate_Review", "Financial_Information", "Average Monthly Enrollment", "Average Monthly Disenrollment"),
  name = c("market", "exchange_type", "state", "issuer_name", "issuer_id", "issuer_new_to_exchange", "sadp_only",
           "plan_id", "plan_type", "qhp_or_sadp", "metal_level", "url_claims_payment_policies",
           "issuer_claims_received_out_of_network", "issuer_claims_received_in_network",
           "issuer_claims_denied_out_of_network", "issuer_claims_denied_in_network",
           "issuer_claims_resubmitted_out_of_network", "issuer_claims_resubmitted_in_network",
           "issuer_internal_appeals_filed", "issuer_internal_appeals_overturned", "issuer_internal_appeals_overturned_percent",
           "issuer_external_appeals_filed", "issuer_external_appeals_overturned", "issuer_external_appeals_overturned_percent",
           "plan_claims_received_out_of_network", "plan_claims_received_in_network",
           "plan_claims_denied_out_of_network", "plan_claims_denied_in_network",
           "plan_claims_resubmitted_out_of_network", "plan_claims_resubmitted_in_network",
           "plan_reason_referral_or_prior_auth_required", "plan_reason_out_of_network_provider", "plan_reason_service_excluded",
           "plan_reason_medical_necessity_excl_behavioral_health", "plan_reason_medical_necessity_behavioral_health",
           "plan_reason_benefit_limit_reached", "plan_reason_member_not_covered",
           "plan_reason_investigational_experimental_cosmetic", "plan_reason_administrative", "plan_reason_other",
           "rate_review_url", "financial_information_url", "avg_monthly_enrollment", "avg_monthly_disenrollment"),
  kind = c(rep("text", 12), rep("count", 8), "pct", "count", "count", "pct", rep("count", 16), "url", "url", "count", "count"),
  level = c(rep("plan", 12), rep("issuer", 12), rep("plan", 16), "issuer", "issuer", "plan", "plan"),
  label = c("Market (Individual or SHOP)", "Exchange type", "State", "Issuer name", "Issuer HIOS ID",
            "Issuer new to the Exchange", "Stand-alone dental issuer", "Plan ID (14 characters)", "Plan type",
            "Medical or dental plan", "Metal level", "Claims payment policies URL",
            "Issuer claims received, out-of-network", "Issuer claims received, in-network",
            "Issuer claims denied, out-of-network", "Issuer claims denied, in-network",
            "Issuer claims resubmitted, out-of-network", "Issuer claims resubmitted, in-network",
            "Issuer internal appeals filed", "Issuer internal appeals overturned",
            "Issuer internal appeals overturned (percent, 0 to 100)", "Issuer external appeals filed",
            "Issuer external appeals overturned", "Issuer external appeals overturned (percent, 0 to 100)",
            "Plan claims received, out-of-network", "Plan claims received, in-network",
            "Plan claims denied, out-of-network", "Plan claims denied, in-network",
            "Plan claims resubmitted, out-of-network", "Plan claims resubmitted, in-network",
            "Denial reason tally: referral or prior authorization required", "Denial reason tally: out-of-network provider",
            "Denial reason tally: service excluded", "Denial reason tally: medical necessity, excluding behavioral health",
            "Denial reason tally: medical necessity, behavioral health only", "Denial reason tally: enrollee benefit limit reached",
            "Denial reason tally: member not covered", "Denial reason tally: investigational, experimental or cosmetic",
            "Denial reason tally: administrative reason", "Denial reason tally: other reason",
            "Rate review URL", "Financial information URL", "Average monthly enrollment (2024)", "Average monthly disenrollment (2024)"),
  unit = c(rep("text", 12), rep("claim lines", 6), rep("appeal requests", 2), "percent", "appeal requests", "appeal requests", "percent",
           rep("claim lines", 6), rep("reason tallies", 10), "URL", "URL", "enrollees", "enrollees"),
  stringsAsFactors = FALSE
)
stopifnot(nrow(SPEC) == WB$expected_columns)
NUMCOLS <- SPEC$name[SPEC$kind %in% c("count", "pct")]
COUNTCOLS <- SPEC$name[SPEC$kind == "count"]
ISSUER_NUM <- SPEC$name[SPEC$level == "issuer" & SPEC$kind %in% c("count", "pct")]
PLAN_NUM <- SPEC$name[SPEC$level == "plan" & SPEC$kind == "count"]
REASON_COLS <- grep("^plan_reason_", SPEC$name, value = TRUE)
stopifnot(length(NUMCOLS) == 30L, length(ISSUER_NUM) == 12L, length(PLAN_NUM) == 18L, length(REASON_COLS) == 10L)

# ---- 2. read a tab, find the header row by the string Plan_ID ------------
read_tab <- function(sheet) {
  raw <- readxl::read_excel(wb_path, sheet = sheet, col_names = FALSE, col_types = "text", .name_repair = "minimal")
  head_n <- min(20L, nrow(raw))
  hits <- which(vapply(seq_len(head_n), function(i) any(unlist(raw[i, ]) == WB$header_anchor, na.rm = TRUE), logical(1)))
  if (!length(hits)) stop("No row containing ", WB$header_anchor, " in the first ", head_n, " rows of ", sheet)
  hr <- hits[1]
  hdr <- trimws(as.character(unlist(raw[hr, ])))
  dat <- as.data.frame(raw[(hr + 1L):nrow(raw), ], stringsAsFactors = FALSE)
  names(dat) <- hdr
  dat[] <- lapply(dat, function(v) trimws(as.character(v)))
  keep <- rowSums(!is.na(dat) & dat != "") > 0
  src_rows <- (hr + 1L):nrow(raw)
  dat <- dat[keep, , drop = FALSE]
  rownames(dat) <- NULL
  list(data = dat, header_row = hr, header = hdr, title = as.character(raw[[1]][1]), src_rows = src_rows[keep])
}
tabs <- list(qhp = read_tab(WB$sheet_qhp), sadp = read_tab(WB$sheet_sadp), shop = read_tab(WB$sheet_shop))
sheets_found <- readxl::excel_sheets(wb_path)
if (!setequal(sheets_found, c(WB$sheet_disclaimer, WB$sheet_qhp, WB$sheet_sadp, WB$sheet_shop))) {
  stop("Workbook sheets changed: ", paste(sheets_found, collapse = " | "))
}
for (k in names(tabs)) {
  lg("tab ", k, ": header row ", tabs[[k]]$header_row, ", ", nrow(tabs[[k]]$data), " data rows, ", ncol(tabs[[k]]$data), " columns")
  if (nrow(tabs[[k]]$data) != WB$expected_data_rows[[k]]) stop("Row count changed for tab ", k, ": ", nrow(tabs[[k]]$data), " vs expected ", WB$expected_data_rows[[k]])
  if (!identical(tabs[[k]]$header, SPEC$raw)) stop("Header of tab ", k, " differs from the expected 44 PUF headers")
}
if (tabs$qhp$header_row != 3L) lg("header row is ", tabs$qhp$header_row, " (WP1 saw row 3)", level = "WARN")

# ---- 3. marker classification -------------------------------------------
classify <- function(v, nm) {
  m <- rep(NA_character_, length(v))
  m[!is.na(v) & v == cfg$markers$star1] <- "star1"
  m[!is.na(v) & v == cfg$markers$star2] <- "star2"
  m[!is.na(v) & v == cfg$markers$star3] <- "star3"
  m[!is.na(v) & v == cfg$markers$na] <- "na"
  isnum <- is.na(m) & !is.na(v) & grepl("^[0-9]+(\\.[0-9]+)?([eE][+-]?[0-9]+)?$", v)
  m[isnum] <- "numeric"
  if (anyNA(m)) stop("Unclassified cell value(s) in ", nm, ": ", paste(utils::head(unique(v[is.na(m)]), 8), collapse = ", "))
  val <- suppressWarnings(as.numeric(v)); val[m != "numeric"] <- NA_real_
  list(value = val, marker = m)
}
to_typed <- function(dat, spec) {
  out <- data.frame(row.names = seq_len(nrow(dat)))
  for (i in seq_len(nrow(spec))) {
    nm <- spec$name[i]; v <- dat[[spec$raw[i]]]
    if (spec$kind[i] %in% c("count", "pct")) {
      cl <- classify(v, spec$raw[i])
      if (spec$kind[i] == "count" && any(cl$value != round(cl$value), na.rm = TRUE)) stop("Non-integer count in ", nm)
      if (any(cl$value < 0, na.rm = TRUE)) stop("Negative value in ", nm)
      if (spec$kind[i] == "pct" && any(cl$value > 100, na.rm = TRUE)) stop("Percent above 100 in ", nm)
      out[[nm]] <- cl$value; out[[paste0(nm, "_marker")]] <- cl$marker
    } else out[[nm]] <- v
  }
  out
}
Q <- to_typed(tabs$qhp$data, SPEC)

# ---- 4. structural checks on the Individual QHP tab ----------------------
stopifnot(!anyDuplicated(Q$plan_id), all(nchar(Q$plan_id) == 14L), all(Q$qhp_or_sadp == "QHP"), all(Q$market == "Individual"))
n_issuers <- length(unique(Q$issuer_id)); n_states <- length(unique(Q$state))
if (n_issuers != WB$expected_issuers_qhp || n_states != WB$expected_states_qhp) {
  stop("Issuer/state count changed: ", n_issuers, " issuers, ", n_states, " states")
}
iss_states <- tapply(Q$state, Q$issuer_id, function(s) length(unique(s)))
if (any(iss_states != 1L)) stop("An issuer appears in more than one state: ", paste(names(iss_states)[iss_states != 1L], collapse = ","))
lg("QHP tab: ", nrow(Q), " plans, ", n_issuers, " issuers, ", n_states, " states, each issuer in one state")

# repeated issuer-level values identical across an issuer's plan rows (raw text compared)
iss_raw_cols <- c(SPEC$raw[SPEC$level == "issuer"], "Exchange_Type", "State", "Issuer_Name", "Is_Issuer_New_to_Exchange?(Yes_or_No)",
                  "SADP_Only", "URL_Claims_Payment_Policies")
rd <- tabs$qhp$data
nonident <- vapply(iss_raw_cols, function(cn) {
  sum(tapply(rd[[cn]], rd$Issuer_ID, function(v) length(unique(v))) > 1L)
}, integer(1))
repeat_check <- data.frame(raw_header = names(nonident), issuers_with_differing_values = as.integer(nonident), stringsAsFactors = FALSE)
lg("issuer-level columns repeated on plan rows: issuers with differing values = ", sum(nonident), " (all columns checked: ", length(nonident), ")")
if (sum(nonident) > 0) lg("NON-IDENTICAL repeated issuer values: ", paste(names(nonident)[nonident > 0], collapse = "; "), level = "WARN")

# ---- 4b. raw cell types: count cells stored as TEXT digit-strings ----------------------------
# RAW-WORKBOOK TRAP: in the xlsx a count can be a numeric cell in one row and a text digit-string in
# another row of the SAME issuer. A reader that guesses column types reads the text cells as missing and
# mis-sums. This script reads every cell as text (col_types = "text") and classifies it by regex, so it is
# immune; the audit below re-reads the sheet cell by cell (col_types = "list") only to document the trap.
rl_types <- readxl::read_excel(wb_path, sheet = WB$sheet_qhp, col_names = FALSE, col_types = "list", .name_repair = "minimal")
sr <- tabs$qhp$src_rows
stopifnot(length(sr) == nrow(tabs$qhp$data), ncol(rl_types) == nrow(SPEC))
cell_textdigit <- function(j) vapply(rl_types[[j]][sr], function(x) is.character(x) && length(x) == 1L && !is.na(x) && grepl("^[0-9]+(\\.[0-9]+)?$", x), logical(1))
cell_numeric <- function(j) vapply(rl_types[[j]][sr], function(x) is.numeric(x) && length(x) == 1L && !is.na(x), logical(1))
td_cols <- which(SPEC$kind %in% c("count", "pct"))
td_cells <- vapply(td_cols, function(j) sum(cell_textdigit(j)), integer(1)); names(td_cells) <- SPEC$raw[td_cols]
td_cells <- td_cells[td_cells > 0]
td_info <- function(raw_name) {
  j <- match(raw_name, SPEC$raw); td <- cell_textdigit(j); nu <- cell_numeric(j); iid <- tabs$qhp$data$Issuer_ID
  list(cells = sum(td), issuers = sum(tapply(td, iid, any)), mixed = sum(tapply(seq_along(td), iid, function(i) any(td[i]) && any(nu[i]))))
}
TD_RESUB_OON <- td_info("Issuer_Claims_Resubmitted_Out_of_Network")
TD_RECV_IN <- td_info("Issuer_Claims_Received_In_Network")
j_recv_in <- match("Issuer_Claims_Received_In_Network", SPEC$raw)
v_recv_in_numtyped <- vapply(rl_types[[j_recv_in]][sr], function(x) if (is.numeric(x) && length(x) == 1L && !is.na(x)) as.numeric(x) else NA_real_, numeric(1))
NAIVE_IN_RECV <- sum(v_recv_in_numtyped[!duplicated(tabs$qhp$data$Issuer_ID)], na.rm = TRUE)   # first row of each issuer, text-typed cells read as missing
lg("raw cell types: text-typed digit-string count cells per column (all other count columns have none): ",
   paste(names(td_cells), td_cells, sep = " = ", collapse = "; "), ". Issuers with a text digit-string cell: Resubmitted_Out_of_Network ", TD_RESUB_OON$issuers,
   " (", TD_RESUB_OON$mixed, " also hold numeric cells), Received_In_Network ", TD_RECV_IN$issuers, " (", TD_RECV_IN$mixed, " also hold numeric cells)")
rm(rl_types)

# ---- 5. issuer_state.csv (de-duplicate on Issuer_ID) -----------------------
iss_cols <- c("issuer_id", "state", "issuer_name", "exchange_type", "issuer_new_to_exchange", "sadp_only",
              "url_claims_payment_policies", as.vector(rbind(ISSUER_NUM, paste0(ISSUER_NUM, "_marker"))))
first_idx <- !duplicated(Q$issuer_id)
issuer_state <- Q[first_idx, iss_cols]
issuer_state$n_plan_rows <- as.integer(table(Q$issuer_id)[issuer_state$issuer_id])
rate_fun <- function(d, r) ifelse(!is.na(d) & !is.na(r) & r > 0 & d <= r, d / r, NA_real_)   # NA if either is a marker, received is 0, or denied exceeds received (flagged separately)
exceeds_fun <- function(d, r) ifelse(!is.na(d) & !is.na(r), d > r, NA)
issuer_state$in_network_denial_rate <- rate_fun(issuer_state$issuer_claims_denied_in_network, issuer_state$issuer_claims_received_in_network)
issuer_state$out_of_network_denial_rate <- rate_fun(issuer_state$issuer_claims_denied_out_of_network, issuer_state$issuer_claims_received_out_of_network)
issuer_state$in_network_denied_exceeds_received <- exceeds_fun(issuer_state$issuer_claims_denied_in_network, issuer_state$issuer_claims_received_in_network)
issuer_state$out_of_network_denied_exceeds_received <- exceeds_fun(issuer_state$issuer_claims_denied_out_of_network, issuer_state$issuer_claims_received_out_of_network)
issuer_state <- issuer_state[order(issuer_state$state, issuer_state$issuer_id), ]
rownames(issuer_state) <- NULL
stopifnot(nrow(issuer_state) == WB$expected_issuers_qhp, !anyDuplicated(issuer_state$issuer_id), !anyDuplicated(paste(issuer_state$issuer_id, issuer_state$state)))

# ---- 6. MR URL, Network PUF (small) ---------------------------------------
mr_raw <- as.data.frame(readxl::read_excel(mr_path, col_types = "text"), stringsAsFactors = FALSE)
stopifnot(identical(names(mr_raw), c("State", "Issuer ID", "URL Submitted", "Tech POC Email")))
mr_raw[] <- lapply(mr_raw, trimws)
mr_urls <- data.frame(state = mr_raw$State, issuer_id = mr_raw$`Issuer ID`, mr_index_url = mr_raw$`URL Submitted`, stringsAsFactors = FALSE)
mr_urls$in_individual_qhp_tab <- mr_urls$issuer_id %in% issuer_state$issuer_id
mr_urls <- mr_urls[order(mr_urls$state, mr_urls$issuer_id), ]; rownames(mr_urls) <- NULL
stopifnot(!anyDuplicated(mr_urls$issuer_id), all(grepl("^https?://", mr_urls$mr_index_url)))
nw_raw <- readr::read_csv(nw_path, col_types = readr::cols(.default = "c"), na = character(), show_col_types = FALSE)
nw <- data.frame(
  business_year = nw_raw$BusinessYear, state = nw_raw$StateCode, issuer_id = nw_raw$IssuerId, source_name = nw_raw$SourceName,
  import_date = nw_raw$ImportDate, network_name = nw_raw$NetworkName, network_id = nw_raw$NetworkId,
  network_url = nw_raw$NetworkURL, market_coverage = nw_raw$MarketCoverage, dental_only_plan = nw_raw$DentalOnlyPlan,
  stringsAsFactors = FALSE
)
nw$in_individual_qhp_tab <- nw$issuer_id %in% issuer_state$issuer_id
lg("MR URL PUF: ", nrow(mr_urls), " rows (", sum(mr_urls$in_individual_qhp_tab), " in the QHP tab); Network PUF: ", nrow(nw), " rows")
no_mr <- setdiff(issuer_state$issuer_id, mr_urls$issuer_id)
no_nw <- setdiff(issuer_state$issuer_id, nw$issuer_id[nw$market_coverage == "Individual" & nw$dental_only_plan == "No"])
lg("QHP issuers with no MR URL row: ", paste(no_mr, collapse = ","), " ; with no individual medical Network PUF row: ", paste(no_nw, collapse = ","))
issuer_state$has_mr_index_url <- issuer_state$issuer_id %in% mr_urls$issuer_id
issuer_state$in_network_puf <- issuer_state$issuer_id %in% nw$issuer_id[nw$market_coverage == "Individual" & nw$dental_only_plan == "No"]

# ---- 7. plans.csv: PUF columns plus the few joins the PUF lacks -------------
pa_cols <- c("StandardComponentId", "MarketCoverage", "DentalOnlyPlan", "NetworkId", "ServiceAreaId", "HIOSProductId",
             "IsNewPlan", "MetalLevel", "PlanType", "IssuerId", "StateCode")
pa <- readr::read_csv(pa_path, col_types = readr::cols(.default = "c"), na = character(), show_col_types = FALSE, col_select = dplyr::all_of(pa_cols))
pai <- as.data.table(pa)[MarketCoverage == "Individual" & DentalOnlyPlan == "No"]
pa_const <- sapply(setdiff(pa_cols, c("StandardComponentId", "MarketCoverage", "DentalOnlyPlan")),
                   function(cn) max(pai[, uniqueN(get(cn)), by = StandardComponentId]$V1))
if (any(pa_const != 1L)) stop("A Plan Attributes field varies across CSR variants of one plan: ", paste(names(pa_const)[pa_const != 1L], collapse = ", "))
pa1 <- unique(pai[, .(StandardComponentId, pa_network_id = NetworkId, pa_service_area_id = ServiceAreaId, pa_hios_product_id = HIOSProductId,
                      pa_is_new_plan = IsNewPlan, pa_metal_level = MetalLevel, pa_plan_type = PlanType, pa_issuer_id = IssuerId, pa_state = StateCode)])
stopifnot(!anyDuplicated(pa1$StandardComponentId))
cx <- readr::read_csv(cx_path, col_types = readr::cols(.default = "c"), na = character(), show_col_types = FALSE,
                      col_select = dplyr::all_of(c("DentalPlan", "PlanID_2025", "PlanID_2026")))
cx_ids25 <- unique(cx$PlanID_2025[cx$DentalPlan == "N"]); cx_ids26 <- unique(cx$PlanID_2026[cx$DentalPlan == "N"])

plans <- Q[, c("plan_id", "issuer_id", "state", "exchange_type", "plan_type", "metal_level",
               as.vector(rbind(PLAN_NUM, paste0(PLAN_NUM, "_marker"))))]
plans$plan_in_network_denial_rate <- rate_fun(plans$plan_claims_denied_in_network, plans$plan_claims_received_in_network)
plans$plan_out_of_network_denial_rate <- rate_fun(plans$plan_claims_denied_out_of_network, plans$plan_claims_received_out_of_network)
plans$in_network_denied_exceeds_received <- exceeds_fun(plans$plan_claims_denied_in_network, plans$plan_claims_received_in_network)
plans$out_of_network_denied_exceeds_received <- exceeds_fun(plans$plan_claims_denied_out_of_network, plans$plan_claims_received_out_of_network)
plans$plan_level_reported <- plans$plan_claims_received_in_network_marker %in% c("numeric", "star2")
m <- match(plans$plan_id, pa1$StandardComponentId)
plans$pa_matched <- !is.na(m)
for (cn in setdiff(names(pa1), "StandardComponentId")) plans[[cn]] <- pa1[[cn]][m]
plans$pa_expanded_bronze <- ifelse(plans$pa_matched, plans$pa_metal_level == "Expanded Bronze", NA)
plans$plan_listed_in_2025_crosswalk <- plans$plan_id %in% cx_ids25
plans$plan_listed_in_2026_crosswalk <- plans$plan_id %in% cx_ids26
plans <- plans[order(plans$state, plans$issuer_id, plans$plan_id), ]; rownames(plans) <- NULL
stopifnot(nrow(plans) == WB$expected_data_rows$qhp, !anyDuplicated(plans$plan_id))
# the join must agree with the PUF where both carry a field (checked, not assumed)
chk <- plans[plans$pa_matched, ]
join_agree <- c(plan_type = all(chk$plan_type == chk$pa_plan_type), issuer = all(chk$issuer_id == chk$pa_issuer_id), state = all(chk$state == chk$pa_state),
                metal = all(chk$metal_level == ifelse(chk$pa_metal_level == "Expanded Bronze", "Bronze", chk$pa_metal_level)))
lg("join agreement with PUF fields (plan type, issuer, state, metal level with Expanded Bronze counted as Bronze): ", paste(names(join_agree), join_agree, collapse = "; "))
if (!all(join_agree)) stop("Plan Attributes disagree with PUF on a shared field")
lg("Plan Attributes (Individual, medical) matched ", sum(plans$pa_matched), " of ", nrow(plans), " plans; PA-only plans: ", sum(!pa1$StandardComponentId %in% plans$plan_id))

# ---- 8. issuer_volume.csv ---------------------------------------------------
vol <- issuer_state[, c("issuer_id", "state", "issuer_name",
                        "issuer_claims_received_in_network", "issuer_claims_received_in_network_marker",
                        "issuer_claims_received_out_of_network", "issuer_claims_received_out_of_network_marker",
                        "issuer_claims_denied_in_network", "issuer_claims_denied_in_network_marker",
                        "issuer_claims_denied_out_of_network", "issuer_claims_denied_out_of_network_marker")]
names(vol) <- sub("^issuer_claims_", "", names(vol)); names(vol) <- sub("^received_in_network", "in_network_claims_received", names(vol))
names(vol) <- sub("^received_out_of_network", "out_of_network_claims_received", names(vol))
names(vol) <- sub("^denied_in_network", "in_network_claims_denied", names(vol)); names(vol) <- sub("^denied_out_of_network", "out_of_network_claims_denied", names(vol))
tot_in <- sum(vol$in_network_claims_received, na.rm = TRUE)
vol$share_of_national_in_network_claims <- vol$in_network_claims_received / tot_in
vol$in_network_claims_over_1000 <- !is.na(vol$in_network_claims_received) & vol$in_network_claims_received > 1000
vol$has_reported_in_network_count <- vol$in_network_claims_received_marker == "numeric"
vol$has_mr_index_url <- issuer_state$has_mr_index_url
vol$in_network_puf <- issuer_state$in_network_puf
vol <- vol[order(-vol$in_network_claims_received, vol$issuer_id), ]; rownames(vol) <- NULL
stopifnot(abs(sum(vol$share_of_national_in_network_claims, na.rm = TRUE) - 1) < 1e-12)

# ---- 9. marker counts per variable, other tabs -------------------------------
mk_levels <- c("numeric", "star1", "star2", "star3", "na")
mk_count <- function(typed, spec_names, tab) {
  do.call(rbind, lapply(spec_names, function(nm) {
    mk <- factor(typed[[paste0(nm, "_marker")]], levels = mk_levels)
    v <- typed[[nm]]
    data.frame(tab = tab, variable = nm, rows = length(mk), as.list(setNames(as.integer(table(mk)), mk_levels)),
               numeric_1_to_9 = sum(v >= 1 & v <= 9, na.rm = TRUE), numeric_1_to_10 = sum(v >= 1 & v <= 10, na.rm = TRUE), numeric_zero = sum(v == 0, na.rm = TRUE),
               min_numeric = if (any(!is.na(v))) min(v, na.rm = TRUE) else NA_real_,
               min_positive = if (any(v > 0, na.rm = TRUE)) min(v[v > 0], na.rm = TRUE) else NA_real_, stringsAsFactors = FALSE)
  }))
}
# issuer-level columns counted once per issuer (de-duplicated), plan-level once per plan row
mk_iss <- mk_count(issuer_state, ISSUER_NUM, "Individual QHP (issuers, de-duplicated)")
mk_plan <- mk_count(Q, PLAN_NUM, "Individual QHP (plan rows)")
marker_counts <- rbind(mk_iss, mk_plan)
marker_counts$is_percent <- marker_counts$variable %in% SPEC$name[SPEC$kind == "pct"]
readr::write_csv(marker_counts, file.path(OUT, "marker_counts.csv"), na = "")
is_cnt <- !marker_counts$is_percent & !marker_counts$variable %in% c("avg_monthly_enrollment", "avg_monthly_disenrollment")
count_1to9 <- sum(marker_counts$numeric_1_to_9[is_cnt]); count_1to10 <- sum(marker_counts$numeric_1_to_10[is_cnt])
cnt_vals <- c(unlist(issuer_state[setdiff(ISSUER_NUM, SPEC$name[SPEC$kind == "pct"])]), unlist(Q[PLAN_NUM[!PLAN_NUM %in% c("avg_monthly_enrollment", "avg_monthly_disenrollment")]]))
cnt_vals <- cnt_vals[!is.na(cnt_vals)]
min_pos_count <- min(cnt_vals[cnt_vals > 0]); n_zero_count <- sum(cnt_vals == 0)
freq_small <- table(factor(cnt_vals[cnt_vals >= 9 & cnt_vals <= 13], levels = 9:13))
enr_vals <- Q$avg_monthly_enrollment[!is.na(Q$avg_monthly_enrollment)]
lg("count columns (", sum(is_cnt), "): numeric cells with 1 to 9 = ", count_1to9, "; with 1 to 10 = ", count_1to10, "; smallest positive value = ", min_pos_count, "; zeros = ", n_zero_count,
   "; cells with value 9..13: ", paste(names(freq_small), freq_small, sep = "=", collapse = ", "))
enr_min <- marker_counts$min_numeric[marker_counts$variable == "avg_monthly_enrollment"]
lg("smallest numeric average monthly enrollment: ", enr_min)

tab_summary <- do.call(rbind, lapply(names(tabs), function(k) {
  d <- tabs[[k]]$data
  data.frame(tab = c(qhp = WB$sheet_qhp, sadp = WB$sheet_sadp, shop = WB$sheet_shop)[[k]], header_row = tabs[[k]]$header_row,
             data_rows = nrow(d), columns = ncol(d), distinct_plan_ids = length(unique(d$Plan_ID)),
             distinct_issuers = length(unique(d$Issuer_ID)), distinct_states = length(unique(d$State)),
             plan_types = paste(sort(unique(d$Plan_Type)), collapse = "/"),
             used_in_analysis = k == "qhp", stringsAsFactors = FALSE)
}))
readr::write_csv(tab_summary, file.path(OUT, "tabs_summary.csv"), na = "")

# ---- 10. dictionary versus workbook headers ------------------------------------
pdftotext <- "/opt/homebrew/bin/pdftotext"
pdf_text <- function(p) {
  tf <- tempfile(fileext = ".txt")
  rc <- system2(pdftotext, shQuote(c("-layout", p, tf)))
  if (rc != 0) stop("pdftotext failed for ", p)
  x <- readLines(tf, warn = FALSE); unlink(tf); x
}
dtxt <- pdf_text(dict_path); distxt <- pdf_text(disc_path)
dict_labels <- {
  idx <- grep("^\\s*Variable Label:", dtxt); out <- character(0)
  for (k in idx) {
    lab <- trimws(sub("^\\s*Variable Label:\\s*", "", dtxt[k])); j <- k + 1L
    while (j <= length(dtxt) && nzchar(trimws(dtxt[j])) && !grepl(":", dtxt[j], fixed = TRUE)) { lab <- paste0(lab, trimws(dtxt[j])); j <- j + 1L }
    out <- c(out, lab)
  }
  out
}
norm <- function(s) gsub("[^a-z0-9]", "", tolower(s))
file_hdr <- SPEC$raw
nh <- norm(file_hdr); nl <- norm(dict_labels)
dist_m <- adist(nh, nl)
# a label that merely adds a suffix such as "? (Yes or No)" counts as a near match: compare against the label truncated to the header length
for (i in seq_along(nh)) for (j in seq_along(nl)) {
  d2 <- adist(nh[i], substr(nl[j], 1, nchar(nh[i])))[1, 1]
  if (d2 == 0 && nchar(nl[j]) > nchar(nh[i])) dist_m[i, j] <- min(dist_m[i, j], 1)
}
best <- apply(dist_m, 1, which.min)
dv <- data.frame(workbook_header = file_hdr, dictionary_label = dict_labels[best], edit_distance = dist_m[cbind(seq_along(best), best)],
                 stringsAsFactors = FALSE)
dv$relation <- ifelse(dv$workbook_header == dv$dictionary_label, "identical",
                 ifelse(norm(dv$workbook_header) == norm(dv$dictionary_label), "same after ignoring spaces, case, punctuation",
                   ifelse(dv$edit_distance <= 8, "spelling or wording differs", "no similar dictionary label")))
ov <- c("Average Monthly Enrollment" = "Enrollment_Data", "Average Monthly Disenrollment" = "Disenrollment_Data")
for (h in names(ov)) {
  i <- which(dv$workbook_header == h)
  dv$dictionary_label[i] <- ov[[h]]; dv$relation[i] <- "dictionary Variable Name matches the header; its Variable Label differs"
}
dv$note <- ""
dv$note[dv$workbook_header == "Plan_Number_Claims_Denied_Due_To_Enrolle_Benefit_Limit_Reached"] <- "typo in file (Enrolle); dictionary also writes Claim not Claims"
dv$note[grepl("Proceduce", dv$workbook_header)] <- "typo in file (Proceduce); dictionary also writes Claim not Claims"
dv$note[dv$workbook_header == "Issuer_Internal_Appeals_Filed"] <- "dictionary label has the typo Filled"
dv$note[dv$workbook_header == "Plan_Type"] <- "file values EPO/HMO/POS/PPO only; dictionary also lists Indemnity"
dv$note[dv$workbook_header == "Individual/SHOP"] <- "header row cell; Plan_ID is in column 8, not column 1"
dict_only <- setdiff(dict_labels, dv$dictionary_label)
readr::write_csv(dv, file.path(OUT, "dictionary_vs_workbook.csv"), na = "")
n_mismatch <- sum(dv$relation != "identical")
lg("dictionary vs workbook: ", sum(dv$relation == "identical"), " identical, ", n_mismatch, " differ; dictionary labels with no workbook column: ",
   if (length(dict_only)) paste(dict_only, collapse = "; ") else "none")
obs_values <- list(plan_type = sort(unique(Q$plan_type)), metal_level = sort(unique(Q$metal_level)), exchange_type = sort(unique(Q$exchange_type)))
supp_pat <- "suppress|small cell|fewer than|less than|under 10|1 to 9|threshold"
supp_hits <- c(dictionary = sum(grepl(supp_pat, dtxt, ignore.case = TRUE)), disclaimer = sum(grepl(supp_pat, distxt, ignore.case = TRUE)))
lg("suppression-rule search (", supp_pat, ") hits: dictionary ", supp_hits[["dictionary"]], ", disclaimer ", supp_hits[["disclaimer"]])
supp_lines <- trimws(c(dtxt[grepl(supp_pat, dtxt, ignore.case = TRUE)], distxt[grepl(supp_pat, distxt, ignore.case = TRUE)]))

# ---- 11. codebook and denial-reason map -------------------------------------------
src_tab <- WB$sheet_qhp
dict_by_raw <- setNames(dv$dictionary_label, dv$workbook_header)
cb <- list()
addcb <- function(variable, label, unit, file, puf_header, notes, kind = "") {
  cb[[length(cb) + 1L]] <<- data.frame(variable = variable, label = label, unit = unit, source_tab = src_tab, output_file = file,
                                       puf_header = puf_header, dictionary_label = if (puf_header %in% names(dict_by_raw)) dict_by_raw[[puf_header]] else "",
                                       notes = notes, stringsAsFactors = FALSE)
}
note_for <- function(i) {
  nm <- SPEC$name[i]; r <- SPEC$raw[i]
  base <- switch(SPEC$kind[i],
    count = "Count. Markers: * not available, ** suppressed small cell (most likely 1 to 10, inferred), *** not required for plan type, N/A new to Exchange. Raw workbook cells can be text digit-strings: read the workbook as text and classify cells by regex, never by cell type.",
    pct = "Percent on a 0 to 100 scale as CMS reports it. Markers as for counts.",
    text = "Text.", url = "URL; not carried into the clean tables.")
  extra <- c(
    Plan_Number_Claims_Denied_Due_To_Enrolle_Benefit_Limit_Reached = " Header typo kept in the PUF (Enrolle).",
    Plan_Number_Claims_Denied_Due_To_Investigational_Experimental_Cosmetic_Proceduce = " Header typo kept in the PUF (Proceduce).",
    Issuer_Internal_Appeals_Filed = " Dictionary spells the label Filled.",
    Issuer_Claims_Resubmitted_Out_of_Network = paste0(" RAW-WORKBOOK TRAP: ", TD_RESUB_OON$cells, " plan-row cells are text-typed digit-strings (", TD_RESUB_OON$issuers, " issuers, ", TD_RESUB_OON$mixed,
                                                      " of them also hold numeric cells in other rows); a type-guessing reader reads them as missing and mis-sums. Read the workbook as text and classify by regex."),
    Issuer_Claims_Received_In_Network = paste0(" RAW-WORKBOOK TRAP: ", TD_RECV_IN$cells, " plan-row cells are text-typed digit-strings (", TD_RECV_IN$issuers, " issuers, ", TD_RECV_IN$mixed,
                                               " of them also hold numeric cells in other rows); a type-guessing reader reads them as missing and mis-sums. Read the workbook as text and classify by regex."),
    Plan_Type = " Values EPO/HMO/POS/PPO; no Indemnity in this tab.",
    Metal_Level = " Bronze here includes CMS Expanded Bronze plans; plans.csv pa_metal_level separates them.",
    Plan_ID = " 14 characters, the standard component ID without the CSR variant suffix.",
    avg_monthly_enrollment = "",
    `Average Monthly Enrollment` = " Text column mixing numbers with * and **; no numeric value below 11 appears (the same lower edge as the claim counts).",
    `Average Monthly Disenrollment` = " Text column mixing numbers with * and **.")
  paste0(base, if (r %in% names(extra)) extra[[r]] else "")
}
files_for <- function(i) {
  nm <- SPEC$name[i]
  if (SPEC$kind[i] == "url" && nm != "url_claims_payment_policies") return("(not carried forward)")
  if (nm == "url_claims_payment_policies") return("issuer_state.csv")
  if (SPEC$level[i] == "issuer") return("issuer_state.csv; issuer_volume.csv (claims only)")
  if (nm %in% c("avg_monthly_enrollment", "avg_monthly_disenrollment")) return("plans.csv")
  if (nm %in% c("market")) return("(not carried forward: all Individual)")
  if (nm == "issuer_name") return("issuer_state.csv; issuer_volume.csv")
  if (nm %in% c("issuer_new_to_exchange", "sadp_only")) return("issuer_state.csv")
  if (nm == "exchange_type") return("issuer_state.csv; plans.csv")
  if (nm %in% c("state", "issuer_id")) return("issuer_state.csv; plans.csv; issuer_volume.csv")
  if (nm %in% c("qhp_or_sadp")) return("(not carried forward: all QHP)")
  "plans.csv"
}
for (i in seq_len(nrow(SPEC))) {
  addcb(SPEC$name[i], SPEC$label[i], SPEC$unit[i], files_for(i), SPEC$raw[i], note_for(i))
  if (SPEC$kind[i] %in% c("count", "pct")) {
    addcb(paste0(SPEC$name[i], "_marker"), paste0("Marker for: ", SPEC$label[i]), "category", files_for(i), SPEC$raw[i],
          "One of numeric, star1 (*), star2 (**), star3 (***), na (N/A). The typed column is NA unless numeric.")
  }
}
derived <- list(
  c("in_network_denial_rate", "In-network denial rate (issuer level)", "proportion 0 to 1", "issuer_state.csv", "Issuer in-network claims denied divided by received; NA when either is not numeric, received is 0, or denied exceeds received."),
  c("out_of_network_denial_rate", "Out-of-network denial rate (issuer level)", "proportion 0 to 1", "issuer_state.csv", "As above for out-of-network. NA for the 2 issuers whose out-of-network denied exceeds received in the CMS file (PINNED KNOWN ANOMALY, see qa/g2_reconciliation.md Section 4B and tests/test_cms_parse.R)."),
  c("in_network_denied_exceeds_received", "In-network denied exceeds received (data flag)", "flag", "issuer_state.csv; plans.csv", "TRUE when both counts are numeric and denied is larger than received. No such row exists for in-network claims."),
  c("out_of_network_denied_exceeds_received", "Out-of-network denied exceeds received (data flag)", "flag", "issuer_state.csv; plans.csv", "TRUE when both counts are numeric and denied is larger than received: 2 issuers and 30 plans in the CMS file itself (PINNED KNOWN ANOMALY: the test fails if the set changes). Raw counts are kept as published."),
  c("n_plan_rows", "Plan rows in the Individual QHP tab", "count", "issuer_state.csv", "Number of plan rows the issuer has in the tab."),
  c("has_mr_index_url", "Issuer has a machine-readable index URL", "flag", "issuer_state.csv; issuer_volume.csv", "Issuer ID present in the PY2026 Machine-readable URL PUF."),
  c("in_network_puf", "Issuer present in the Network PUF (individual medical)", "flag", "issuer_state.csv; issuer_volume.csv", "Used with has_mr_index_url as evidence an issuer offers plans in 2026."),
  c("plan_in_network_denial_rate", "In-network denial rate (plan level)", "proportion 0 to 1", "plans.csv", "Plan in-network denied divided by received; NA unless both numeric, received > 0 and denied <= received."),
  c("plan_out_of_network_denial_rate", "Out-of-network denial rate (plan level)", "proportion 0 to 1", "plans.csv", "As above for out-of-network."),
  c("plan_level_reported", "Plan-level claims reported", "flag", "plans.csv", "TRUE when plan in-network claims received is numeric or suppressed (not N/A). 2,559 of 4,159 plans."),
  c("pa_matched", "Plan found in Plan Attributes PUF (individual, medical)", "flag", "plans.csv", "Join on Plan_ID = StandardComponentId; 116 plans do not match."),
  c("pa_network_id", "Network ID (Plan Attributes)", "text", "plans.csv", "Joined: the PUF lacks it. Links a plan to the Network PUF."),
  c("pa_service_area_id", "Service area ID (Plan Attributes)", "text", "plans.csv", "Joined: the PUF lacks it."),
  c("pa_hios_product_id", "HIOS product ID (Plan Attributes)", "text", "plans.csv", "Joined: the PUF lacks it."),
  c("pa_is_new_plan", "Plan new for 2026 (Plan Attributes IsNewPlan)", "text", "plans.csv", "Not the same as plan-level N/A in the transparency PUF."),
  c("pa_metal_level", "Metal level (Plan Attributes)", "text", "plans.csv", "Separates Expanded Bronze (1,156 plans) from Bronze, which the transparency PUF merges."),
  c("pa_plan_type", "Plan type (Plan Attributes, agreement check only)", "text", "plans.csv", "Agreement check column, do not use for analysis (use plan_type). Duplicates the PUF Plan_Type; joined only to check agreement, which holds on every matched plan."),
  c("pa_issuer_id", "Issuer ID (Plan Attributes, agreement check only)", "text", "plans.csv", "Agreement check column, do not use for analysis (use issuer_id). Duplicates the PUF Issuer_ID; joined only to check agreement, which holds on every matched plan."),
  c("pa_state", "State (Plan Attributes, agreement check only)", "text", "plans.csv", "Agreement check column, do not use for analysis (use state). Duplicates the PUF State; joined only to check agreement, which holds on every matched plan."),
  c("pa_expanded_bronze", "Expanded Bronze plan", "flag", "plans.csv", "TRUE when pa_metal_level is Expanded Bronze; NA when the plan did not match."),
  c("plan_listed_in_2025_crosswalk", "Plan ID listed as a 2025 plan in the Plan ID Crosswalk", "flag", "plans.csv", "Individual medical rows only. Diagnostic for continuing plans."),
  c("plan_listed_in_2026_crosswalk", "Plan ID listed as a 2026 plan in the Plan ID Crosswalk", "flag", "plans.csv", "Individual medical rows only."),
  c("in_network_claims_received", "In-network claims received (issuer volume)", "claim lines", "issuer_volume.csv", "Same value as issuer_claims_received_in_network, renamed in the volume table. NA for markers; see the marker column."),
  c("in_network_claims_received_marker", "Marker for: in-network claims received (issuer volume)", "category", "issuer_volume.csv", "One of numeric, star1, star2, star3, na."),
  c("out_of_network_claims_received", "Out-of-network claims received (issuer volume)", "claim lines", "issuer_volume.csv", "Same value as issuer_claims_received_out_of_network."),
  c("out_of_network_claims_received_marker", "Marker for: out-of-network claims received (issuer volume)", "category", "issuer_volume.csv", "One of numeric, star1, star2, star3, na."),
  c("in_network_claims_denied", "In-network claims denied (issuer volume)", "claim lines", "issuer_volume.csv", "Same value as issuer_claims_denied_in_network."),
  c("in_network_claims_denied_marker", "Marker for: in-network claims denied (issuer volume)", "category", "issuer_volume.csv", "One of numeric, star1, star2, star3, na."),
  c("out_of_network_claims_denied", "Out-of-network claims denied (issuer volume)", "claim lines", "issuer_volume.csv", "Same value as issuer_claims_denied_out_of_network."),
  c("out_of_network_claims_denied_marker", "Marker for: out-of-network claims denied (issuer volume)", "category", "issuer_volume.csv", "One of numeric, star1, star2, star3, na."),
  c("share_of_national_in_network_claims", "Share of national in-network claims", "proportion 0 to 1", "issuer_volume.csv", "Issuer in-network claims received over the sum across issuers with a numeric count (all 185 issuers, no restriction)."),
  c("in_network_claims_over_1000", "More than 1,000 in-network claims", "flag", "issuer_volume.csv", "KFF's inclusion rule, reproduced exactly (157 issuers)."),
  c("has_reported_in_network_count", "Has a reported in-network claims count", "flag", "issuer_volume.csv", "Issuers with a numeric in-network claims received value.")
)
for (d in derived) addcb(d[1], d[2], d[3], d[4], "", d[5])
codebook <- do.call(rbind, cb)
codebook <- codebook[!duplicated(paste(codebook$variable, codebook$output_file)), ]
readr::write_csv(codebook, file.path(P$config, "variable_codebook.csv"), na = "")

rm_def <- data.frame(
  puf_header = SPEC$raw[match(REASON_COLS, SPEC$name)],
  variable = REASON_COLS,
  label = c("Referral or prior authorization required", "Out-of-network provider", "Service excluded", "Medical necessity, excluding behavioral health",
            "Medical necessity, behavioral health only", "Enrollee benefit limit reached", "Member not covered", "Investigational, experimental or cosmetic",
            "Administrative reason", "Other reason not listed"),
  kff_category = c("Lack of prior authorization or referral", "Out-of-network provider (excluded by KFF)", "Excluded service",
                   "Not medically necessary, excluding behavioral health", "Medical necessity, behavioral health", "Benefit limit reached",
                   "Member not covered", "Investigational, experimental or cosmetic", "Administrative", "Other reason not listed"),
  kff_group = c("Prior authorization or referral", "Out-of-network provider", "Service excluded", "Medical necessity", "Medical necessity",
                "Benefit limit reached", "Member not covered", "Investigational, experimental or cosmetic", "Administrative", "Other"),
  included_in_kff_79m = c(TRUE, FALSE, rep(TRUE, 8)),
  cms_definition = c("Non-emergency claims for services that required prior authorization, referral, prior approval or precertification and were denied.",
                     "Claims denied for services from outside the plan network when the plan has a closed network.",
                     "Claims denied for limits or exclusions of services, supplies and so on, including drugs not on the formulary.",
                     "In-network claims denied for services not meeting accepted standards to diagnose or treat, medical services.",
                     "As above, related to behavioral or mental health.",
                     "In-network claims denied because the beneficiary reached an annual benefit limit.",
                     "In-network claims denied because of the beneficiary's enrollment status when services were rendered.",
                     "In-network claims denied because the procedure was investigational, cosmetic or experimental.",
                     "In-network claims denied for administrative reasons such as duplicate claim, missing information, untimely filing, billing provider not approved, coordination of benefits, inconsistent codes, workers compensation, paid by other insurance, unable to identify patient.",
                     "In-network claims denied for any reason not enumerated in another category."),
  note = "A tally of denial reasons over the life of claims, not a count of unique claims; the ten tallies are not asserted to sum to denied claims.",
  stringsAsFactors = FALSE
)
readr::write_csv(rm_def, file.path(P$config, "denial_reason_map.csv"), na = "")

# ---- 12. write the clean tables ---------------------------------------------------
readr::write_csv(issuer_state, file.path(OUT, "issuer_state.csv"), na = "")
readr::write_csv(plans, file.path(OUT, "plans.csv"), na = "")
readr::write_csv(vol, file.path(OUT, "issuer_volume.csv"), na = "")
readr::write_csv(mr_urls, file.path(OUT, "mr_urls.csv"), na = "")
readr::write_csv(nw, file.path(OUT, "network_puf.csv"), na = "")
lg("wrote issuer_state (", nrow(issuer_state), "), plans (", nrow(plans), "), issuer_volume (", nrow(vol), "), mr_urls (", nrow(mr_urls), "), network_puf (", nrow(nw), ")")

# =====================================================================
# PART B : G2 reconciliation to KFF. Suppressed (and N/A) cells are counted as ZERO
# in this part only, because that is KFF's stated rule ("they were assumed to be zero").
# =====================================================================
z0 <- function(x) { x[is.na(x)] <- 0; x }
fmt <- function(x, d = 0) formatC(x, format = "f", digits = d, big.mark = ",")
pc <- function(x, d = 1) paste0(formatC(100 * x, format = "f", digits = d), "%")
iss <- issuer_state
iss$rin <- z0(iss$issuer_claims_received_in_network); iss$roon <- z0(iss$issuer_claims_received_out_of_network)
iss$din <- z0(iss$issuer_claims_denied_in_network); iss$doon <- z0(iss$issuer_claims_denied_out_of_network)
iss$ifil <- z0(iss$issuer_internal_appeals_filed); iss$iov <- z0(iss$issuer_internal_appeals_overturned); iss$efil <- z0(iss$issuer_external_appeals_filed)
iss$eov <- z0(iss$issuer_external_appeals_overturned)
iss$in_kff <- iss$rin > 1000                       # KFF restriction: more than 1,000 in-network claims
iss$in_kff_total <- (iss$rin + iss$roon) > 1000    # alternative reading: more than 1,000 claims in total
K <- iss[iss$in_kff, ]
lg("issuers with more than 1,000 in-network claims: ", nrow(K), "; with more than 1,000 claims in total: ", sum(iss$in_kff_total))

# ---- KFF working file (row level) ----------------------------------------------------
kf_iss <- as.data.frame(readxl::read_excel(kff_path, sheet = "Medical Issuers", col_types = "text"), stringsAsFactors = FALSE)
kf_ex <- as.data.frame(readxl::read_excel(kff_path, sheet = "Excluded Medical Issuers", col_types = "text"), stringsAsFactors = FALSE)
kf_pl <- as.data.frame(readxl::read_excel(kff_path, sheet = "Medical Plans", col_types = "text"), stringsAsFactors = FALSE)
kid <- function(v) as.character(as.integer(as.numeric(v)))
kn <- function(v) suppressWarnings(as.numeric(v))
kf_iss$id <- kid(kf_iss$Issuer_Id); kf_ex$id <- kid(kf_ex$Issuer_Id)
set_equal <- setequal(kf_iss$id, K$issuer_id)
kf_m <- merge(kf_iss, K[, c("issuer_id", "rin", "roon", "din", "doon", "ifil", "iov", "efil")], by.x = "id", by.y = "issuer_id")
issuer_identical <- c(
  in_recv = sum(kn(kf_m$Claims_Received_In_Network) == kf_m$rin), in_den = sum(kn(kf_m$Claims_Denied_In_Network) == kf_m$din),
  oon_recv = sum(kn(kf_m$Claims_Received_Out_Of_Network) == kf_m$roon), oon_den = sum(kn(kf_m$Claims_Denied_Out_Of_Network) == kf_m$doon),
  int_filed = sum(z0(kn(kf_m$Internal_Appeals_Filed)) == kf_m$ifil), ext_filed = sum(z0(kn(kf_m$External_Appeals_Filed)) == kf_m$efil)
)
lg("KFF working file: ", nrow(kf_iss), " included issuers, ", nrow(kf_ex), " excluded; issuer set identical to ours (>1000 in-network): ", set_equal,
   "; per-issuer identical cells: ", paste(names(issuer_identical), issuer_identical, collapse = ", "))
kf_plan_ids <- kf_pl$Plan_Id
pl_k <- plans[plans$issuer_id %in% K$issuer_id & plans$plan_level_reported, ]
plan_set_equal <- setequal(kf_plan_ids, pl_k$plan_id)
lg("KFF Medical Plans rows ", length(kf_plan_ids), "; ours (KFF issuers, plan level reported) ", nrow(pl_k), "; plan ID sets identical: ", plan_set_equal)
reason_kff_cols <- c("Plan_Claims_Denied_Due_To_Referral_Required", "Plan_Claims_Denied_Due_To_Out_Of_Network", "Plan_Claims_Denied_Due_To_Services_Excluded",
                     "Plan_Claims_Denied_Due_To_Not_Medically_Necessary_Excluding_Behavioral_Health", "Plan_Claims_Denied_Due_To_Not_Medically_Necessary_Only_Behavioral_Health",
                     "Plan_Claims_Denied_Due_To_Enrollee_Benefit_Limit_Reached", "Plan_Claims_Denied_Due_To_Member_Not_Covered",
                     "Plan_Claims_Denied_Due_To_Investigational_Experimental_Cosmetic_Procedure", "Plan_Claims_Denied_Due_To_Administrative_Reason", "Plan_Claims_Denied_Due_To_Other")
kf_reason_sums <- vapply(reason_kff_cols, function(cn) sum(z0(kn(kf_pl[[cn]]))), numeric(1))
our_reason_sums_k <- vapply(REASON_COLS, function(cn) sum(z0(pl_k[[cn]])), numeric(1))
our_reason_sums_all <- vapply(REASON_COLS, function(cn) sum(z0(plans[[cn]])), numeric(1))
reason_maxdiff_k <- max(abs(kf_reason_sums - our_reason_sums_k))
lg("plan-level reason tallies, ours (KFF issuers) minus KFF working file: max abs difference = ", reason_maxdiff_k)
kf_plan_in_recv <- sum(z0(kn(kf_pl$Plan_Claims_Received_In_Network))); kf_plan_in_den <- sum(z0(kn(kf_pl$Plan_Claims_Denied_In_Network)))

# ---- facts used by several rows -------------------------------------------------------
nat <- function(S) list(rin = sum(S$rin), roon = sum(S$roon), din = sum(S$din), doon = sum(S$doon))
nK <- nat(K); nA <- nat(iss)
ratesK <- K$din / K$rin
# issuers offering plans in 2026: in 2026 Network PUF and MR URL PUF
offer2026 <- iss$issuer_id[iss$has_mr_index_url & iss$in_network_puf]
not_offering <- setdiff(iss$issuer_id, offer2026)
kff_incl_not_offering <- intersect(K$issuer_id, not_offering)
lg("issuers absent from BOTH 2026 MR-URL PUF and Network PUF: ", paste(not_offering, collapse = ","), "; of these in the KFF set: ", paste(kff_incl_not_offering, collapse = ","))
n_over1000_offering <- sum(K$issuer_id %in% offer2026)

# internal appeals variants
fil_pct_numeric <- !is.na(issuer_state$issuer_internal_appeals_overturned_percent)
A_sub <- issuer_state[fil_pct_numeric & !is.na(issuer_state$issuer_internal_appeals_filed) & issuer_state$issuer_id %in% K$issuer_id, ]
if (anyNA(A_sub$issuer_internal_appeals_overturned)) stop("an issuer reports a percent overturned but no overturned count")
A_filed <- sum(A_sub$issuer_internal_appeals_filed)
A_overturned <- sum(A_sub$issuer_internal_appeals_overturned)
A_upheld_pct <- A_filed - A_overturned          # upheld = filed minus overturned counts, on issuers that report a percent overturned
A_upheld_viapct <- sum(A_sub$issuer_internal_appeals_filed * (1 - A_sub$issuer_internal_appeals_overturned_percent / 100))
B_sub <- K[!is.na(K$issuer_internal_appeals_filed) & !is.na(K$issuer_internal_appeals_overturned), ]
B_filed <- sum(B_sub$issuer_internal_appeals_filed); B_upheld <- B_filed - sum(B_sub$issuer_internal_appeals_overturned)
C_filed <- sum(K$ifil); C_upheld <- C_filed - sum(K$iov)
kf_upheld_col_sum <- sum(kn(kf_iss$Internal_Appeals_Upheld), na.rm = TRUE)
kf_upheld_filed_sub <- sum(kn(kf_iss$Internal_Appeals_Filed)[!is.na(kn(kf_iss$Internal_Appeals_Upheld))])
lg("internal appeals upheld variants: A (CMS percent reported, ", nrow(A_sub), " issuers) ", round(A_upheld_pct), " of ", A_filed, " = ", round(A_upheld_pct / A_filed, 4),
   "; B (counts reported, ", nrow(B_sub), " issuers) ", B_upheld, " of ", B_filed, " = ", round(B_upheld / B_filed, 4),
   "; C (all, suppressed overturned as 0) ", C_upheld, " of ", C_filed, " = ", round(C_upheld / C_filed, 4),
   "; KFF working file upheld column sums to ", kf_upheld_col_sum, " on ", kf_upheld_filed_sub, " filed")
# external appeals as a share of upheld internal appeals, issuers reporting at least 10 external appeals
E_sub <- K[K$efil >= 10, ]; E_up <- sum(E_sub$ifil - E_sub$iov); E_share <- sum(E_sub$efil) / E_up

# plan-level
plans_k_rep <- plans[plans$issuer_id %in% K$issuer_id & plans$plan_level_reported, ]
plan_recv_all <- sum(z0(plans$plan_claims_received_in_network)); plan_den_all <- sum(z0(plans$plan_claims_denied_in_network))
reasons9_all <- sum(our_reason_sums_all[REASON_COLS != "plan_reason_out_of_network_provider"])
reasons9_k <- sum(our_reason_sums_k[REASON_COLS != "plan_reason_out_of_network_provider"])
metal_rate <- function(P_) tapply(z0(P_$plan_claims_denied_in_network), P_$metal_level, sum) / tapply(z0(P_$plan_claims_received_in_network), P_$metal_level, sum)
mrK <- metal_rate(plans_k_rep)
r9 <- our_reason_sums_k[REASON_COLS != "plan_reason_out_of_network_provider"]
share9 <- r9 / sum(r9); names(share9) <- sub("^plan_reason_", "", names(share9))
medneed_share <- sum(r9[c("plan_reason_medical_necessity_excl_behavioral_health", "plan_reason_medical_necessity_behavioral_health")]) / sum(r9)
returning <- iss$issuer_new_to_exchange == "No"
sum_plan_all_claims <- function(ids) sum(z0(plans$plan_claims_received_in_network[plans$issuer_id %in% ids])) + sum(z0(plans$plan_claims_received_out_of_network[plans$issuer_id %in% ids]))
ret_ids <- iss$issuer_id[returning & iss$in_kff]
share_returning_claims <- sum_plan_all_claims(ret_ids) / sum(iss$rin[iss$issuer_id %in% ret_ids] + iss$roon[iss$issuer_id %in% ret_ids])
share_returning_in <- sum(z0(plans$plan_claims_received_in_network[plans$issuer_id %in% ret_ids])) / sum(iss$rin[iss$issuer_id %in% ret_ids])
share_plans_with_data <- sum(plans$plan_level_reported) / nrow(plans)
# state averages (claims weighted) and Texas range
st_w <- tapply(K$din, K$state, sum) / tapply(K$rin, K$state, sum)
st_simple <- tapply(ratesK, K$state, mean)
tx <- ratesK[K$state == "TX"]

# ---- rows of the G2 table ----------------------------------------------------------------
G <- list()
add <- function(id, group, benchmark, src, sentence, kff_value, unit, ours, basis, lo, hi, exact = FALSE, fail_status = "differs, explained",
                explanation = "", ours_alt = NA_real_, alt_basis = "", verified = TRUE, rule = NULL) {
  pass <- if (is.na(lo)) NA else (ours >= lo & ours <= hi)
  status <- if (is.na(pass)) "not testable" else if (pass) (if (exact) "reproduces exactly" else "reproduces") else if (verified) fail_status else "differs, unexplained"
  G[[length(G) + 1L]] <<- data.frame(id = id, group = group, benchmark = benchmark, kff_source = src, kff_sentence = sentence, kff_value = kff_value, unit = unit,
    ours = ours, ours_basis = basis, difference = if (is.na(kff_value)) NA_real_ else ours - kff_value,
    tolerance_low = lo, tolerance_high = hi, tolerance_rule = if (!is.null(rule)) rule else if (is.na(lo)) "none" else if (exact) "exact" else "KFF figure rounded to KFF's own precision",
    pass = pass, near_tolerance_edge = if (is.na(pass) || !pass || lo == hi) NA else (min(ours - lo, hi - ours) < 0.1 * (hi - lo)),
    status = status, explanation = if (status %in% c("reproduces", "reproduces exactly")) "" else explanation,
    ours_alternative = ours_alt, alternative_basis = alt_basis, stringsAsFactors = FALSE)
}
rnd <- function(v, unit) list(lo = v - unit / 2, hi = v + unit / 2)
S_KFF1 <- "Insurers reported receiving about 496 million claims in 2024, with 91% (451 million claims) filed for in-network services."
S_KFF2 <- "Of these in-network claims, approximately 85 million were ultimately denied, resulting in an average in-network denial rate of 19% (Figure 1)."
S_KFF3 <- "Out-of-network claims totaled 44 million, with an overall higher denial rate of 37%."
S_KFF4 <- "Insurers of qualified health plans (QHPs) sold on HealthCare.gov denied 19% of in-network claims in 2024 and 37% of out-of-network claims for a combined average of 20% of all claims, all similar to 2023."
BK <- "Issuers with more than 1,000 in-network claims (n=157), suppressed counted as zero"
BA <- "all 185 issuers, suppressed counted as zero"
r <- rnd(496e6, 1e6); add("A01", "National totals", "Claims received, all", "KFF text", S_KFF1, 496e6, "claims", nK$rin + nK$roon, BK, r$lo, r$hi, ours_alt = nA$rin + nA$roon, alt_basis = BA)
r <- rnd(451e6, 1e6); add("A02", "National totals", "In-network claims received", "KFF text", S_KFF1, 451e6, "claims", nK$rin, BK, r$lo, r$hi, ours_alt = nA$rin, alt_basis = BA)
r <- rnd(0.91, 0.01); add("A03", "National totals", "In-network share of claims received", "KFF text", S_KFF1, 0.91, "proportion", nK$rin / (nK$rin + nK$roon), BK, r$lo, r$hi, ours_alt = nA$rin / (nA$rin + nA$roon), alt_basis = BA)
r <- rnd(85e6, 1e6); add("A04", "National totals", "In-network claims denied", "KFF text", S_KFF2, 85e6, "claims", nK$din, BK, r$lo, r$hi, ours_alt = nA$din, alt_basis = BA)
r <- rnd(0.19, 0.01); add("A05", "National totals", "In-network denial rate", "KFF text", S_KFF2, 0.19, "proportion", nK$din / nK$rin, BK, r$lo, r$hi, ours_alt = nA$din / nA$rin, alt_basis = BA)
r <- rnd(44e6, 1e6); add("A06", "National totals", "Out-of-network claims received", "KFF text", S_KFF3, 44e6, "claims", nK$roon, BK, r$lo, r$hi, ours_alt = nA$roon, alt_basis = BA)
r <- rnd(0.37, 0.01); add("A07", "National totals", "Out-of-network denial rate", "KFF text", S_KFF3, 0.37, "proportion", nK$doon / nK$roon, BK, r$lo, r$hi, ours_alt = nA$doon / nA$roon, alt_basis = BA)
r <- rnd(0.20, 0.01); add("A08", "National totals", "Combined denial rate", "KFF text", S_KFF4, 0.20, "proportion", (nK$din + nK$doon) / (nK$rin + nK$roon), BK, r$lo, r$hi, ours_alt = (nA$din + nA$doon) / (nA$rin + nA$roon), alt_basis = BA)
S_RNG <- "The in-network denial rate ranged from 3% to 36%, with significant variation by insurer and by state."
r <- rnd(0.03, 0.01); add("B01", "Insurer distribution", "Lowest insurer in-network denial rate", "KFF text", S_RNG, 0.03, "proportion", min(ratesK), BK, r$lo, r$hi)
r <- rnd(0.36, 0.01); add("B02", "Insurer distribution", "Highest insurer in-network denial rate", "KFF text", S_RNG, 0.36, "proportion", max(ratesK), BK, r$lo, r$hi)
S_156 <- "Of the 183 major medical insurers offering plans in 2026 in HealthCare.gov states, 156 reported receiving more than 1,000 claims and showed data on claims received and denied in 2024."
ver156 <- length(kff_incl_not_offering) == 1L && nrow(K) - 1L == 156L && length(offer2026) == 183L && n_over1000_offering == 156L && set_equal
add("B03", "Insurer distribution", "Insurers offering plans in 2026", "KFF text", S_156, 183, "insurers", length(offer2026),
    "185 issuer IDs minus the 2 absent from both the 2026 Machine-readable URL PUF and the Network PUF", 183, 183, exact = TRUE)
add("B04", "Insurer distribution", "Insurers with more than 1,000 claims (Methods sentence)", "KFF text", S_156, 156, "insurers", nrow(K),
    "Issuers with more than 1,000 in-network claims, all 185 issuers", 156, 156, exact = TRUE, fail_status = "differs, explained", verified = ver156,
    explanation = paste0("We count ", nrow(K), ". KFF's own working file lists the identical ", nrow(kf_iss), " issuers, and the same brief says ", nrow(kf_iss),
                         " reporting insurers in the Figure 3 text. The Methods say one of the two issuers in the PUF that do not offer plans in 2026 submitted 2024 claims and is kept: ",
                         paste(kff_incl_not_offering, collapse = ","), " (CareSource North Carolina) is in our ", nrow(K), " and absent from the 2026 Machine-readable URL and Network PUFs, so ",
                         nrow(K), " minus 1 = ", n_over1000_offering, " issuers offering plans in 2026. Inference: the two issuers are identified by absence from those two 2026 PUFs, not from the QHP Landscape file."),
    ours_alt = n_over1000_offering, alt_basis = "of the issuers with more than 1,000 in-network claims, those present in the 2026 MR-URL and Network PUFs")
add("B05", "Insurer distribution", "Insurers with more than 1,000 claims (Figure 3 sentence)", "KFF text", "Seventeen of the 157 reporting insurers had an in-network denial rate of less than 10%, while 26 insurers had a denial rate of 25% or more (Figure 3).",
    157, "insurers", nrow(K), "Issuers with more than 1,000 in-network claims", 157, 157, exact = TRUE)
add("B06", "Insurer distribution", "Insurers with in-network denial rate below 10%", "KFF text", "Seventeen of the 157 reporting insurers had an in-network denial rate of less than 10%", 17, "insurers",
    sum(ratesK < 0.10), "exact rates, issuers with more than 1,000 in-network claims", 17, 17, exact = TRUE, fail_status = "differs, explained",
    verified = sum(round(ratesK * 100) < 10) == 17, ours_alt = sum(round(ratesK * 100) < 10), alt_basis = "rates first rounded to whole percent",
    explanation = paste0("With exact rates ", sum(ratesK < 0.10), " issuers are below 10%; with each issuer's rate first rounded to a whole percent (the precision KFF displays) the count is ",
                         sum(round(ratesK * 100) < 10), ", matching KFF. The rounding convention is inferred, not stated by KFF."))
add("B07", "Insurer distribution", "Insurers with in-network denial rate of 25% or more", "KFF text", "while 26 insurers had a denial rate of 25% or more (Figure 3).", 26, "insurers",
    sum(ratesK >= 0.25), "exact rates, issuers with more than 1,000 in-network claims", 26, 26, exact = TRUE, fail_status = "differs, explained",
    verified = sum(round(ratesK * 100) >= 25) == 26, ours_alt = sum(round(ratesK * 100) >= 25), alt_basis = "rates first rounded to whole percent",
    explanation = paste0("With exact rates ", sum(ratesK >= 0.25), " issuers are at or above 25%; with rates first rounded to a whole percent the count is ",
                         sum(round(ratesK * 100) >= 25), ", matching KFF (same inferred rounding convention as B06)."))
r <- rnd(0.03, 0.01); add("B08", "Insurer distribution", "Share of insurers with in-network denial rate of 30% or more", "KFF text", "About 3% of reporting insurers had in-network denial rates of 30% or higher in 2024", 0.03, "proportion",
    mean(ratesK >= 0.30), BK, r$lo, r$hi)
S_TX <- "denial rates for insurers in Texas had greater variability than any other state included in this analysis, ranging from 12% to 36% (the highest single insurer-level denial rate in the country)."
r <- rnd(0.12, 0.01); add("B09", "Insurer distribution", "Texas lowest insurer in-network denial rate", "KFF text", S_TX, 0.12, "proportion", min(tx), BK, r$lo, r$hi)
r <- rnd(0.36, 0.01); add("B10", "Insurer distribution", "Texas highest insurer in-network denial rate", "KFF text", S_TX, 0.36, "proportion", max(tx), BK, r$lo, r$hi)
S_ST <- "The state with the highest average in-network denial rate for HealthCare.gov insurers was Hawaii (27%), and the lowest average was in South Dakota (7%)."
r <- rnd(0.27, 0.01); add("B11", "Insurer distribution", "Hawaii average in-network denial rate (claims weighted)", "KFF text", S_ST, 0.27, "proportion", st_w[["HI"]], BK, r$lo, r$hi,
    ours_alt = st_simple[["HI"]], alt_basis = "simple mean of insurer rates (does not match; KFF's state average is claims weighted)")
r <- rnd(0.07, 0.01); add("B12", "Insurer distribution", "South Dakota average in-network denial rate (claims weighted)", "KFF text", S_ST, 0.07, "proportion", st_w[["SD"]], BK, r$lo, r$hi,
    ours_alt = st_simple[["SD"]], alt_basis = "simple mean of insurer rates")
S_PL <- "In all, insurers reported on about 66 million in-network claims at the plan level that were ultimately denied that year."
r <- rnd(66e6, 1e6); add("C01", "Plan level", "Plan-level in-network claims denied", "KFF text", S_PL, 66e6, "claims", plan_den_all, "all plan rows, suppressed counted as zero", r$lo, r$hi,
    ours_alt = kf_plan_in_den, alt_basis = "KFF working file Medical Plans sheet (2,540 plan rows)")
S_79 <- "Insurers reported about 79 million denial reasons, excluding denials for claims being out-of-network, for claims that were denied at some point in the adjudication process during the 2024 coverage year."
r <- rnd(79e6, 1e6); add("C02", "Plan level", "Denial reason tallies excluding the out-of-network provider reason", "KFF text", S_79, 79e6, "reason tallies", reasons9_all,
    "all plan rows, nine reasons (out-of-network provider excluded), suppressed counted as zero", r$lo, r$hi, ours_alt = sum(our_reason_sums_all), alt_basis = "all ten reasons (does not match: 87.2 million)")
S_SH <- "Of in-network claims, 13% of denials were because the claim was for an excluded service, 9% of denials were due to lack of prior authorization or referral, and only 5% were based on medical necessity. The share of denial reasons related to administrative reasons was 25%, the most common reason aside from \"other\" (36%)."
r <- rnd(0.36, 0.01); add("C03", "Plan level", "Share of reason tallies: other", "KFF text", S_SH, 0.36, "proportion", share9[["other"]], "nine reasons, KFF issuers", r$lo, r$hi)
r <- rnd(0.25, 0.01); add("C04", "Plan level", "Share of reason tallies: administrative", "KFF text", S_SH, 0.25, "proportion", share9[["administrative"]], "nine reasons, KFF issuers", r$lo, r$hi)
r <- rnd(0.13, 0.01); add("C05", "Plan level", "Share of reason tallies: service excluded", "KFF text", S_SH, 0.13, "proportion", share9[["service_excluded"]], "nine reasons, KFF issuers", r$lo, r$hi)
r <- rnd(0.09, 0.01); add("C06", "Plan level", "Share of reason tallies: referral or prior authorization", "KFF text", S_SH, 0.09, "proportion", share9[["referral_or_prior_auth_required"]], "nine reasons, KFF issuers", r$lo, r$hi)
r <- rnd(0.05, 0.01); add("C07", "Plan level", "Share of reason tallies: medical necessity (both categories)", "KFF text", S_SH, 0.05, "proportion", medneed_share, "nine reasons, KFF issuers", r$lo, r$hi,
    ours_alt = share9[["medical_necessity_excl_behavioral_health"]], alt_basis = "excluding behavioral health only (also rounds to 5%)")
S_MT <- "denied 19% of in-network claims in their bronze plans, 20% in silver plans, 17% in gold plans, 18% in platinum plans, and 22% in catastrophic plans."
for (ml in list(c("Bronze", 0.19), c("Silver", 0.20), c("Gold", 0.17), c("Platinum", 0.18), c("Catastrophic", 0.22))) {
  r <- rnd(as.numeric(ml[2]), 0.01)
  add(sprintf("C%02d", 7 + match(ml[1], c("Bronze", "Silver", "Gold", "Platinum", "Catastrophic"))), "Plan level", paste0("In-network denial rate, ", ml[1], " plans"), "KFF text", S_MT, as.numeric(ml[2]), "proportion",
      mrK[[ml[1]]], "plan rows of KFF issuers with plan-level data, claims weighted", r$lo, r$hi)
}
S_62 <- "only 62% of plans in the CMS dataset were also offered in 2024 and are included in the plan-level reporting for denial reasons."
r <- rnd(0.62, 0.01); add("C13", "Plan level", "Share of plans in the dataset with plan-level data", "KFF text", S_62, 0.62, "proportion", share_plans_with_data, "4,159 Individual QHP plan rows; plan-level in-network claims not N/A", r$lo, r$hi)
S_75 <- "Among returning insurers, such denial information was only reported for 75% of their claims (the share of claims attributable to returning plans)"
r <- rnd(0.75, 0.01); add("C14", "Plan level", "Share of returning insurers' claims with plan-level data", "KFF text", S_75, 0.75, "proportion", share_returning_claims,
    "plan-level claims received (in plus out-of-network) over issuer-level claims received, issuers flagged not new to the Exchange with more than 1,000 in-network claims", r$lo, r$hi,
    fail_status = "differs, unexplained", verified = FALSE, ours_alt = share_returning_in, alt_basis = "in-network claims only",
    explanation = "KFF's definition of the returning-insurer set is not stated precisely enough to reproduce.")
n_pl_pa <- sum(plans$plan_level_reported & plans$pa_matched)
add("C15", "Plan level", "Plans with 2024 plan-level data that are offered in 2026 (KFF: 2,514)", "KFF text", "of the 4,415 plans offered in 2024, only 2,514 (57%) were offered in 2026 and are included in the plan-level reporting of information on denial reasons.", 2514, "plans",
    n_pl_pa, "plan rows with plan-level claims that are also in the PY2026 Plan Attributes PUF (individual, medical). Inferred definition, identified by inspecting the join after the plain count of plan rows with plan-level data gave 2,559; KFF used the QHP Landscape files, which were not downloaded", 2514, 2514, exact = TRUE,
    ours_alt = sum(plans$plan_level_reported), alt_basis = "plan rows with plan-level claims, no Plan Attributes restriction (2,559); 2,540 remain in KFF's working file after its issuer restriction")
add("C16", "Plan level", "Share of 2024 plans offered in 2026 (2,514 of 4,415 = 57%)", "KFF text", "of the 4,415 plans offered in 2024, only 2,514 (57%) were offered in 2026", 0.57, "proportion",
    NA_real_, "needs the count of plans offered in 2024 (4,415), which is not in the PY2026 files", NA_real_, NA_real_,
    explanation = "Not testable from the files downloaded: the 4,415 plans offered in 2024 come from KFF's comparison with the QHP Landscape files. The numerator 2,514 is reproduced in C15.")
S_262 <- "Of the approximately 85 million in-network denied claims in 2024, HealthCare.gov consumers appealed at least 262,982 - an appeal rate of less than 1%."
add("D01", "Appeals", "Internal appeals filed", "KFF text", S_262, 262982, "appeal requests", sum(K$ifil), "issuers with more than 1,000 in-network claims, suppressed counted as zero", 262982, 262982, exact = TRUE,
    ours_alt = sum(iss$ifil), alt_basis = "all 185 issuers (263,021; WP1 count reproduced)")
add("D02", "Appeals", "Internal appeals as a share of in-network denied claims", "KFF text", S_262, 0.01, "proportion (KFF: less than 1%)", sum(K$ifil) / nK$din, BK, 0, 0.01, rule = "KFF states less than 1%")
S_UP <- "Insurers upheld 165,863 (66%) denials on appeal."
add("D03", "Appeals", "Internal appeals upheld, share (66%), on all 262,982 appeals", "KFF text", S_UP, 0.66, "proportion", C_upheld / C_filed,
    "all appeals of the 157 issuers; upheld = filed minus overturned, suppressed overturned counted as zero", 0.655, 0.665, fail_status = "differs, explained",
    verified = (A_upheld_pct / A_filed >= 0.655 & A_upheld_pct / A_filed <= 0.665),
    explanation = paste0("On the 262,982 appeals the upheld share is ", round(100 * C_upheld / C_filed, 1), "%, which is not 66%. KFF's denominator for the 66% is not stated. On the ",
                         nrow(A_sub), " issuers that report a percent overturned (", format(A_filed, big.mark = ","), " appeals) the upheld share is ", round(100 * A_upheld_pct / A_filed, 1),
                         "% (D03b), and KFF's own working file gives ", round(100 * kf_upheld_col_sum / kf_upheld_filed_sub, 1), "% on its issuers with a reported outcome. KFF's 66% matches the reported-outcome subsets, not the all-appeals denominator. This is a denominator difference, not an error in KFF's figure."),
    ours_alt = A_upheld_pct / A_filed, alt_basis = "issuers that report a percent overturned (see D03b)")
add("D03b", "Appeals", "Internal appeals upheld, share (66%), on issuers that report a percent overturned", "KFF text", S_UP, 0.66, "proportion", A_upheld_pct / A_filed,
    "issuers that report a percent overturned (CMS percent column) in the 157; upheld = filed minus overturned counts", 0.655, 0.665,
    ours_alt = B_upheld / B_filed, alt_basis = paste0("issuers with both counts reported (", nrow(B_sub), " issuers)"))
add("D04", "Appeals", "Internal appeals upheld, count", "KFF text", S_UP, 165863, "appeal requests", A_upheld_pct, "issuers that report a percent overturned (CMS percent column)", 165863, 165863, exact = TRUE,
    fail_status = "differs, unexplained", verified = FALSE,
    explanation = paste0("The count is not reproduced by any definition we can justify: ", format(round(A_upheld_pct), big.mark = ","), " on the reported-percent subset (", round(100 * (A_upheld_pct / 165863 - 1), 2),
                         "% from KFF's value; ", format(round(A_upheld_viapct), big.mark = ","), " if upheld is computed as filed times one minus the CMS percent), ", format(B_upheld, big.mark = ","), " on the reported-count subset, ", format(C_upheld, big.mark = ","), " on all appeals, and ",
                         format(kf_upheld_col_sum, big.mark = ","), " in KFF's own working file upheld column (filed minus overturned where reported). The KFF text figure differs from its own working file by ",
                         format(kf_upheld_col_sum - 165863, big.mark = ","), ". Origin unexplained; not forced to pass."),
    ours_alt = B_upheld, alt_basis = paste0("filed minus overturned on issuers with both counts reported (", nrow(B_sub), " issuers)"))
S_EX <- "Among insurers that reported at least 10 external appeals in 2024, Marketplace enrollees externally appealed at least 5,881 claims in 2024"
add("D05", "Appeals", "External appeals filed", "KFF text", S_EX, 5881, "appeal requests", sum(K$efil), "issuers with more than 1,000 in-network claims, suppressed counted as zero", 5881, 5881, exact = TRUE,
    ours_alt = sum(iss$efil), alt_basis = "all 185 issuers")
r <- rnd(0.04, 0.01); add("D06", "Appeals", "External appeals as a share of upheld internal appeals", "KFF text", "Among insurers that reported at least 10 external appeals in 2024, 4% of upheld appeals were externally appealed.", 0.04, "proportion",
    E_share, "issuers with at least 10 external appeals; upheld = filed minus overturned, suppressed counted as zero", r$lo, r$hi)
# row-level working-file checks
add("E01", "Row-level check against KFF working file", "Included issuers: same set as ours (more than 1,000 in-network claims)", "KFF working file", "Medical Issuers sheet", nrow(kf_iss), "insurers", as.numeric(sum(K$issuer_id %in% kf_iss$id)),
    "issuers in both sets", nrow(kf_iss), nrow(kf_iss), exact = TRUE, ours_alt = as.numeric(nrow(K)), alt_basis = "size of our set")
add("E02", "Row-level check against KFF working file", "Issuers whose in-network received and denied counts are identical", "KFF working file", "Medical Issuers sheet", nrow(kf_iss), "insurers",
    as.numeric(sum(kn(kf_m$Claims_Received_In_Network) == kf_m$rin & kn(kf_m$Claims_Denied_In_Network) == kf_m$din)), "issuers in both sets", nrow(kf_iss), nrow(kf_iss), exact = TRUE)
extra_plans <- setdiff(pl_k$plan_id, kf_plan_ids)
extra_rows <- plans[match(extra_plans, plans$plan_id), ]
extra_zero_or_supp <- all(extra_rows$plan_claims_received_in_network_marker == "star2" | (extra_rows$plan_claims_received_in_network_marker == "numeric" & extra_rows$plan_claims_received_in_network == 0))
add("E03", "Row-level check against KFF working file", "Plan rows in KFF's Medical Plans sheet that are also in our plan-level set", "KFF working file", "Medical Plans sheet", length(kf_plan_ids), "plans",
    as.numeric(sum(kf_plan_ids %in% pl_k$plan_id)), "plans in both sets", length(kf_plan_ids), length(kf_plan_ids), exact = TRUE)
add("E03b", "Row-level check against KFF working file", "Plan rows with plan-level data in our set (KFF issuers) but not in KFF's Medical Plans sheet", "KFF working file", "Medical Plans sheet", 0, "plans",
    as.numeric(length(extra_plans)), "KFF issuers, plan-level in-network claims not N/A", 0, 0, exact = TRUE, fail_status = "differs, explained", verified = extra_zero_or_supp,
    explanation = paste0(length(extra_plans), " plan rows are in our plan-level set but not in KFF's sheet; every one has in-network claims received equal to 0 or suppressed (**), so no denial rate exists for it. KFF's sheet keeps only plans with a usable denominator (inferred from these rows). Their denial reason tallies add nothing: the reason sums equal KFF's exactly (E04)."))
add("E04", "Row-level check against KFF working file", "Plan-level denial reason tallies: largest absolute difference over the ten categories", "KFF working file", "Medical Plans sheet", 0, "reason tallies", reason_maxdiff_k,
    "KFF issuers, suppressed counted as zero", 0, 0, exact = TRUE, fail_status = "differs, unexplained", verified = FALSE, explanation = "Our reason sums differ from KFF's working file.")
G2 <- do.call(rbind, G)
G2$counts_toward_gate <- G2$status != "not testable"
readr::write_csv(G2, file.path(P$results, "g2_reconciliation.csv"), na = "")
tab_status <- table(factor(G2$status, levels = c("reproduces exactly", "reproduces", "differs, explained", "differs, unexplained", "not testable")))
lg("G2 rows: ", nrow(G2), " | ", paste(names(tab_status), tab_status, sep = " = ", collapse = "; "))
unexpl <- G2$id[G2$status == "differs, unexplained"]
G2_verdict <- if (length(unexpl) == 0L) "PASS" else "PASS-WITH-DIFFERENCES"
G2_verdict_text <- if (length(unexpl) == 0L) "Every testable benchmark reproduces within rounding or differs for a verified reason." else
  paste0("Every headline total and rate reproduces within rounding or exactly; ", length(unexpl), " row(s) differ with no verified explanation: ", paste(unexpl, collapse = ", "), ".")
lg("G2 verdict: ", G2_verdict, " | ", G2_verdict_text)

# ---- ledger: kff_ (replication) and att_ (attrition ladder, no restriction) --------------------
ledger_reset("kff")
S <- SCRIPT
la <- function(key, value, unit, desc) ledger_add("kff", key, value, unit, desc, S)
la("kff_claims_received_total", nK$rin + nK$roon, "claim lines", "Claims received, in plus out-of-network, issuers with more than 1,000 in-network claims, 2024 (KFF replication; suppressed counted as zero)")
la("kff_claims_received_in_network", nK$rin, "claim lines", "In-network claims received, issuers with more than 1,000 in-network claims (KFF replication)")
la("kff_claims_received_out_of_network", nK$roon, "claim lines", "Out-of-network claims received, same issuers (KFF replication)")
la("kff_claims_denied_in_network", nK$din, "claim lines", "In-network claims denied, same issuers (KFF replication)")
la("kff_claims_denied_out_of_network", nK$doon, "claim lines", "Out-of-network claims denied, same issuers (KFF replication)")
la("kff_in_network_share_of_claims", nK$rin / (nK$rin + nK$roon), "proportion", "In-network share of all claims received (KFF replication)")
la("kff_denial_rate_in_network", nK$din / nK$rin, "proportion", "Claims-weighted in-network denial rate (KFF replication)")
la("kff_denial_rate_out_of_network", nK$doon / nK$roon, "proportion", "Claims-weighted out-of-network denial rate (KFF replication)")
la("kff_denial_rate_combined", (nK$din + nK$doon) / (nK$rin + nK$roon), "proportion", "Claims-weighted combined denial rate (KFF replication)")
la("kff_issuer_in_network_rate_min", min(ratesK), "proportion", "Lowest issuer in-network denial rate among issuers with more than 1,000 in-network claims")
la("kff_issuer_in_network_rate_max", max(ratesK), "proportion", "Highest issuer in-network denial rate among issuers with more than 1,000 in-network claims")
la("kff_issuers_over_1000_in_network_claims", nrow(K), "issuers", "Issuers with more than 1,000 in-network claims received (KFF restriction); identical to the 157 issuers in KFF's working file")
la("kff_issuers_in_puf", nrow(iss), "issuers", "Issuer IDs in the Individual QHP tab")
la("kff_issuers_offering_2026", length(offer2026), "issuers", "Issuer IDs present in both the 2026 Machine-readable URL PUF and the Network PUF (KFF: 183 offering plans in 2026)")
la("kff_internal_appeals_filed", sum(K$ifil), "appeal requests", "Internal appeals filed, issuers with more than 1,000 in-network claims (KFF: at least 262,982)")
la("kff_internal_appeals_filed_unrestricted", sum(iss$ifil), "appeal requests", "Internal appeals filed, all 185 issuers, suppressed counted as zero")
la("kff_internal_appeals_share_of_denied", sum(K$ifil) / nK$din, "proportion", "Internal appeals filed over in-network claims denied (KFF: less than 1%)")
la("kff_internal_upheld_share_reported_percent", A_upheld_pct / A_filed, "proportion", "Share of internal appeals upheld on issuers that report a percent overturned (CMS percent column)")
la("kff_internal_appeals_reported_percent_issuers", nrow(A_sub), "issuers", "Issuers (of the KFF set) with a reported percent of internal appeals overturned")
la("kff_internal_appeals_filed_reported_percent_issuers", A_filed, "appeal requests", "Internal appeals filed by issuers that report a percent overturned")
la("kff_internal_upheld_share_all_appeals", C_upheld / C_filed, "proportion", "Filed minus overturned over filed, KFF issuers, suppressed overturned counted as zero")
la("kff_external_appeals_filed", sum(K$efil), "appeal requests", "External appeals filed, issuers with more than 1,000 in-network claims (KFF: at least 5,881)")
la("kff_external_share_of_upheld_internal", E_share, "proportion", "External appeals over upheld internal appeals, issuers with at least 10 external appeals (KFF: 4%)")
la("kff_plan_level_in_network_denied", plan_den_all, "claim lines", "Plan-level in-network claims denied, all plan rows (KFF: about 66 million)")
la("kff_plan_level_in_network_received", plan_recv_all, "claim lines", "Plan-level in-network claims received, all plan rows")
la("kff_denial_reason_tallies_nine", reasons9_all, "reason tallies", "Sum of the nine denial reason tallies excluding out-of-network provider, all plan rows (KFF: about 79 million)")
la("kff_denial_reason_tallies_ten", sum(our_reason_sums_all), "reason tallies", "Sum of all ten denial reason tallies, all plan rows")
la("kff_plan_rows_individual_qhp", nrow(plans), "plans", "Plan rows in the Individual QHP tab")
la("kff_plan_rows_with_plan_level_data", sum(plans$plan_level_reported), "plans", "Plan rows whose plan-level in-network claims are not N/A")
la("kff_plan_level_share_of_plans", share_plans_with_data, "proportion", "Plan rows with plan-level data over all plan rows (KFF: 62%)")
la("kff_states_individual_qhp", n_states, "states", "States in the Individual QHP tab")
la("kff_g2_rows_reproduce", sum(G2$status %in% c("reproduces", "reproduces exactly")), "rows", "G2 table rows that reproduce within rounding or exactly")
la("kff_g2_rows_differs_explained", sum(G2$status == "differs, explained"), "rows", "G2 table rows that differ with a verified explanation")
la("kff_g2_rows_differs_unexplained", sum(G2$status == "differs, unexplained"), "rows", "G2 table rows that differ with no verified explanation")
la("kff_g2_rows_not_testable", sum(G2$status == "not testable"), "rows", "G2 table rows not testable from the PUF")
metal_names <- c("Bronze", "Silver", "Gold", "Platinum", "Catastrophic")
for (mn in metal_names) la(paste0("kff_denial_rate_in_network_", tolower(mn)), mrK[[mn]], "proportion", paste0("Plan-level in-network denial rate, ", mn, " plans, KFF issuers (replication)"))

# att_: attrition ladder inputs from ALL issuers; reported numeric cells only (suppressed cells contribute nothing, so these are lower bounds)
sumnum <- function(col) sum(issuer_state[[col]], na.rm = TRUE)
nsup <- function(col) sum(issuer_state[[paste0(col, "_marker")]] == "star2")
nrep <- function(col) sum(issuer_state[[paste0(col, "_marker")]] == "numeric")
attq <- c("claims_received_in_network" = "issuer_claims_received_in_network", "claims_received_out_of_network" = "issuer_claims_received_out_of_network",
          "claims_denied_in_network" = "issuer_claims_denied_in_network", "claims_denied_out_of_network" = "issuer_claims_denied_out_of_network",
          "claims_resubmitted_in_network" = "issuer_claims_resubmitted_in_network", "claims_resubmitted_out_of_network" = "issuer_claims_resubmitted_out_of_network",
          "internal_appeals_filed" = "issuer_internal_appeals_filed", "internal_appeals_overturned" = "issuer_internal_appeals_overturned",
          "external_appeals_filed" = "issuer_external_appeals_filed", "external_appeals_overturned" = "issuer_external_appeals_overturned")
unitq <- ifelse(grepl("^claims", names(attq)), "claim lines", "appeal requests")
for (i in seq_along(attq)) {
  col <- attq[[i]]; nm <- names(attq)[i]
  ledger_add("kff", paste0("att_", nm), sumnum(col), unitq[i], paste0("2024 national total, sum of reported issuer-level cells over all 185 issuers (", gsub("_", " ", nm), "); suppressed cells contribute nothing, so a lower bound"), S)
  ledger_add("kff", paste0("att_", nm, "_issuers_reported"), nrep(col), "issuers", paste0("Issuers with a numeric value for ", gsub("_", " ", nm)), S)
  ledger_add("kff", paste0("att_", nm, "_issuers_suppressed"), nsup(col), "issuers", paste0("Issuers with a suppressed (**) value for ", gsub("_", " ", nm)), S)
}
ledger_add("kff", "att_issuers_in_puf", nrow(issuer_state), "issuers", "Issuer IDs in the Individual QHP tab (denominator for the issuers_reported counts)", S)
ledger_add("kff", "att_issuers_na_issuer_level", sum(issuer_state$issuer_claims_received_in_network_marker == "na"), "issuers", "Issuers whose issuer-level claims are N/A (new to the Exchange); they contribute nothing to the att_ totals", S)
ledger_write("kff")
lg("ledger part written: kff (kff_ and att_ keys, as the WP2 spec asks)")
lg("PART A and B complete")

# ---- QA report ----------------------------------------------------------------------------------
fmt <- function(x, d = 0) formatC(x, format = "f", digits = d, big.mark = ",")
pc <- function(x, d = 1) paste0(formatC(100 * x, format = "f", digits = d), "%")
md_tab <- function(df) {
  df[] <- lapply(df, function(v) { v <- as.character(v); v[is.na(v)] <- ""; gsub("\\|", "/", v) })
  c(paste0("| ", paste(names(df), collapse = " | "), " |"), paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|"),
    apply(df, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |")))
}
g2v <- function(x, unit) if (is.na(x)) "" else if (grepl("^proportion", unit)) pc(x, 2) else fmt(x, if (abs(x) < 100 && x != round(x)) 2 else 0)
cmp <- data.frame(
  ID = G2$id, Benchmark = G2$benchmark,
  KFF = mapply(function(x, u) g2v(x, u), G2$kff_value, G2$unit),
  Ours = mapply(function(x, u) g2v(x, u), G2$ours, G2$unit),
  Tolerance = ifelse(is.na(G2$tolerance_low), "none", ifelse(G2$tolerance_rule == "exact", "exact",
              mapply(function(a, b, u) paste0(g2v(a, u), " to ", g2v(b, u)), G2$tolerance_low, G2$tolerance_high, G2$unit))),
  Result = G2$status, stringsAsFactors = FALSE)
expl <- G2[G2$status %in% c("differs, explained", "differs, unexplained", "not testable"), c("id", "benchmark", "status", "explanation")]
mk_show <- marker_counts[, c("tab", "variable", "numeric", "star1", "star2", "star3", "na", "numeric_1_to_10", "min_positive")]
mk_show$variable <- SPEC$label[match(mk_show$variable, SPEC$name)]
names(mk_show) <- c("Table", "Variable", "Numeric", "Star 1 (*)", "Star 2 (**)", "Star 3 (***)", "N/A", "Numeric 1 to 10", "Smallest positive")
dv_show <- dv[dv$relation != "identical", c("workbook_header", "dictionary_label", "relation", "note")]
names(dv_show) <- c("Workbook header", "Dictionary label", "Relation", "Note")
tab_show <- tab_summary[, c("tab", "header_row", "data_rows", "columns", "distinct_issuers", "distinct_states", "plan_types", "used_in_analysis")]
names(tab_show) <- c("Tab", "Header row", "Data rows", "Columns", "Issuers", "States", "Plan types", "Used in analysis")
enr <- marker_counts[marker_counts$variable == "avg_monthly_enrollment", ]
enr_rep_num <- sum(plans$plan_level_reported & !is.na(Q$avg_monthly_enrollment[match(plans$plan_id, Q$plan_id)]))
# PINNED KNOWN ANOMALY: out-of-network denied exceeds received in the CMS file itself (facts computed from the parsed tables)
oon_iss_bad <- issuer_state[which(issuer_state$out_of_network_denied_exceeds_received %in% TRUE), ]
oon_plan_bad <- plans[which(plans$out_of_network_denied_exceeds_received %in% TRUE), ]
oon_by_issuer <- table(oon_plan_bad$issuer_id)
oon_by_issuer <- oon_by_issuer[order(-as.integer(oon_by_issuer), names(oon_by_issuer))]
oon_issuer_text <- vapply(seq_len(nrow(oon_iss_bad)), function(i) {
  id <- oon_iss_bad$issuer_id[i]; pr <- plans[plans$issuer_id == id, ]
  psum_r <- sum(pr$plan_claims_received_out_of_network, na.rm = TRUE); psum_d <- sum(pr$plan_claims_denied_out_of_network, na.rm = TRUE)
  paste0(id, " (", oon_iss_bad$issuer_name[i], ", ", oon_iss_bad$state[i], "; received ", fmt(oon_iss_bad$issuer_claims_received_out_of_network[i]), ", denied ", fmt(oon_iss_bad$issuer_claims_denied_out_of_network[i]),
         "; its ", nrow(pr), " plan rows sum to ", fmt(psum_r), " received and ", fmt(psum_d), " denied; ", sum(pr$out_of_network_denied_exceeds_received, na.rm = TRUE), " of its plan rows exceed",
         if (identical(as.character(oon_iss_bad$issuer_claims_denied_out_of_network[i]), paste0("6", psum_d)) && psum_r == oon_iss_bad$issuer_claims_received_out_of_network[i])
           paste0("; the issuer-level denied count is the plan-level sum with a leading 6 added (", fmt(oon_iss_bad$issuer_claims_denied_out_of_network[i]), " versus ", fmt(psum_d), "), which looks like a stray digit: an inference, not verified with CMS") else "", ")")
}, character(1))
kf_oon_iss_same <- all(vapply(seq_len(nrow(oon_iss_bad)), function(i) {
  k <- kf_iss[kf_iss$id == oon_iss_bad$issuer_id[i], ]
  nrow(k) == 1L && kn(k$Claims_Received_Out_Of_Network) == oon_iss_bad$issuer_claims_received_out_of_network[i] && kn(k$Claims_Denied_Out_Of_Network) == oon_iss_bad$issuer_claims_denied_out_of_network[i] }, logical(1)))
kf_oon_plan_bad <- kf_pl$Plan_Id[which(kn(kf_pl$Plan_Claims_Denied_Out_Of_Network) > kn(kf_pl$Plan_Claims_Received_Out_Of_Network))]
kf_oon_plan_same <- setequal(kf_oon_plan_bad, oon_plan_bad$plan_id)
lg("PINNED KNOWN ANOMALY out-of-network denied > received: issuers ", paste(oon_iss_bad$issuer_id, collapse = ","), "; plans ", nrow(oon_plan_bad), " (", paste(names(oon_by_issuer), as.integer(oon_by_issuer), sep = "=", collapse = ", "),
   "); KFF working file identical issuer values: ", kf_oon_iss_same, "; same plan set flagged in KFF Medical Plans: ", kf_oon_plan_same)
# upheld internal appeals: why no att_internal_appeals_upheld ledger key
up_fil <- issuer_state$issuer_internal_appeals_filed_marker == "numeric"; up_ovt <- issuer_state$issuer_internal_appeals_overturned_marker == "numeric"
lines <- c(
  "G2 RECONCILIATION TO KFF (WP2), generated by v1/R/02_parse_cms.R on 2026-10-28",
  "",
  paste0("G2 VERDICT: ", G2_verdict, ". ", G2_verdict_text),
  "",
  paste0("Rows: ", nrow(G2), ". ", paste(names(tab_status), tab_status, sep = ": ", collapse = "; "), "."),
  "Rule: suppressed cells are counted as zero in this reconciliation ONLY, because KFF states it did so. Nothing downstream uses that rule.",
  "Tolerance: 'within rounding' means KFF's figure rounded to KFF's own precision (19% means 18.5% to 19.5%; about 496 million means 495.5 to 496.5 million). Exact-match targets are marked exact.",
  "Default basis (A rows and most others): issuers with more than 1,000 in-network claims received (157 issuers, the same 157 as in KFF's working file). Unrestricted values (all 185 issuers) are in g2_reconciliation.csv, column ours_alternative.",
  "No number was scaled, shifted or tuned. Where a row differs, the reason is stated with the evidence computed in the script; where no reason could be verified the row says so.",
  paste0("WARNING (raw workbook): count cells are stored as TEXT digit-strings in places, numeric in other rows of the same issuer (Issuer_Claims_Resubmitted_Out_of_Network: ", TD_RESUB_OON$cells, " text cells in ", TD_RESUB_OON$issuers, " issuers, ",
         TD_RESUB_OON$mixed, " of them also with numeric cells; Issuer_Claims_Received_In_Network: ", TD_RECV_IN$cells, " text cells in ", TD_RECV_IN$issuers, " issuers, ", TD_RECV_IN$mixed,
         " also numeric). A reader that guesses column types reads the text cells as missing and mis-sums (in-network claims received ", fmt(NAIVE_IN_RECV), " instead of ", fmt(sum(issuer_state$issuer_claims_received_in_network, na.rm = TRUE)),
         " when each issuer's first plan row is used); read the workbook as text and classify every cell by regex, as 02_parse_cms.R does."),
  "",
  "SECTION 1. G2 TABLE (full columns, including the exact KFF sentences, are in v1/results/g2_reconciliation.csv)",
  "",
  md_tab(cmp),
  "",
  paste0("Rows that pass within rounding but lie within 10% of the tolerance width of an edge (they round to KFF's figure by a small margin): ", paste(G2$id[which(G2$near_tolerance_edge %in% TRUE)], collapse = ", "), "."),
  "",
  "SECTION 2. DIFFERENCES AND NOT-TESTABLE ROWS",
  "",
  unlist(lapply(seq_len(nrow(expl)), function(i) paste0("- ", expl$id[i], " (", expl$benchmark[i], "), ", expl$status[i], ": ", expl$explanation[i]))),
  "",
  "SECTION 3. WHAT DIFFERS FROM THE PLAN AND FROM WP1",
  "",
  paste0("- Insurers: the file has ", nrow(iss), " issuer IDs. ", nrow(K), " have more than 1,000 in-network claims; KFF's working file lists ", nrow(kf_iss), " included issuers (identical set: ", set_equal, "). KFF's brief says 157 reporting insurers in the Figure 3 text and 156 in Methods, and keeps CareSource North Carolina, which no longer offers plans (see B04)."),
  paste0("- Reading 'more than 1,000 claims' as in-network claims gives ", nrow(K), " issuers; reading it as claims in total gives ", sum(iss$in_kff_total), ". The in-network reading is the one that matches KFF's working file."),
  paste0("- National totals and rates (A rows): ", sum(G2$status[G2$group == "National totals"] %in% c("reproduces", "reproduces exactly")), " of ", sum(G2$group == "National totals"), " reproduce within rounding on the 157-issuer basis. On all 185 issuers the in-network claims received are ", fmt(nA$rin), " against ", fmt(nK$rin), " (", fmt(nA$rin - nK$rin), " claims more)."),
  paste0("- Internal appeals filed reproduce exactly (", fmt(sum(K$ifil)), ") only when issuers are restricted to more than 1,000 in-network claims; unrestricted the sum is ", fmt(sum(iss$ifil)), ", as WP1 found."),
  paste0("- Plan-level denial reason tallies on KFF's issuers: largest absolute difference from KFF's working file over the ten categories = ", reason_maxdiff_k, ". The nine-category sum is ", fmt(reasons9_k), " on KFF's issuers and ", fmt(reasons9_all), " on all plan rows; the ten-category sum is ", fmt(sum(our_reason_sums_all)), "."),
  paste0("- Upheld internal appeals: ", pc(A_upheld_pct / A_filed), " on the ", nrow(A_sub), " issuers that report a percent overturned, ", pc(B_upheld / B_filed), " on issuers with both counts reported, ", pc(C_upheld / C_filed), " on all appeals; KFF states 66%. The 66% is not treated as a KFF error; its denominator is unstated. The count 165,863 is reported in D04."),
  paste0("- Insurer counts below 10% and at or above 25%: exact rates give ", sum(ratesK < 0.10), " and ", sum(ratesK >= 0.25), "; rates first rounded to whole percent give ", sum(round(ratesK * 100) < 10), " and ", sum(round(ratesK * 100) >= 25), "; KFF states 17 and 26."),
  paste0("- State averages: Hawaii ", pc(st_w[["HI"]]), " claims weighted against ", pc(st_simple[["HI"]]), " simple mean of insurer rates; KFF states 27%, which is the claims-weighted value."),
  "",
  "SECTION 4. SUPPRESSION AND MARKERS",
  "",
  "Markers found in the Individual QHP tab (workbook legend): * data not available for this issuer or plan; ** suppressed data due to small cell sizes; *** data not required due to plan type; N/A not applicable because the issuer or plan offering is new to the Exchange. No other non-numeric token occurs in any numeric column (the script stops on an unrecognised value).",
  paste0("- Suppression rule: a search of the CMS data dictionary and data disclaimer for suppress, small cell, fewer than, less than, under 10, 1 to 9 and threshold finds ", supp_hits[["dictionary"]], " hits in the dictionary and ", supp_hits[["disclaimer"]], " in the disclaimer",
         if (sum(supp_hits) > 0) paste0("; matching lines: ", paste(unique(supp_lines), collapse = " / ")) else "", ". No numeric suppression threshold is stated by CMS in these two documents (WP1 found none in the Exchange PUF FAQ or the issuer instructions either); the statement 'CMS does not report values under 10' is KFF's."),
  paste0("- Empirical pattern: across the ", sum(is_cnt), " claim, denial-reason and appeal count columns (issuer and plan level) there are ", count_1to9, " numeric cells with a value from 1 to 9 and ", count_1to10, " with a value from 1 to 10, while true zeros appear as 0 (", fmt(n_zero_count), " cells). The smallest positive value anywhere is ", min_pos_count, ". Cells with value 9 to 13: ", paste(names(freq_small), freq_small, sep = " = ", collapse = ", "),
         ". So ** most likely stands for a count of 1 to 10 (a count of exactly 10 is also never shown although neighbouring values are common). This is an inference, not a CMS statement, and it is one unit wider than WP1's reading (1 to 9) and than KFF's wording (values under 10). The sensitivity bounds downstream should treat ** as 1 to 10 (WP5 sets them; a lower bound of 1 and an upper bound of 10 are defensible, 9 is not supported by the data); only star2 cells are eligible for bounds."),
  paste0("- Enrollment is different: average monthly enrollment is numeric for ", fmt(enr$numeric), " of ", fmt(enr$rows), " plan rows (smallest value ", enr$min_numeric, "), with ", fmt(enr$star1), " star1, ", fmt(enr$star2), " star2 and ", fmt(enr$na), " N/A. No numeric enrollment value below ", enr$min_numeric, " exists (values 9 and 10: ", sum(enr_vals %in% 9:10), "; value 11: ", sum(enr_vals == 11), "), the same lower edge as the claim counts, so enrollment also appears to be suppressed at 10 or fewer. Among the ", fmt(sum(plans$plan_level_reported)), " plans with plan-level claims, ", fmt(enr_rep_num), " have a numeric enrollment (", pc(enr_rep_num / sum(plans$plan_level_reported)), "). This limits any exposure density that uses enrollment as the denominator."),
  paste0("- Plan-level N/A: ", fmt(sum(plans$plan_claims_received_in_network_marker == "na")), " of ", fmt(nrow(plans)), " plan rows (", pc(mean(plans$plan_claims_received_in_network_marker == "na")), ") have N/A plan-level in-network claims (plan or issuer new to the Exchange). Issuer-level N/A: ", sum(issuer_state$issuer_claims_received_in_network_marker == "na"), " of ", nrow(issuer_state), " issuers."),
  "",
  "Marker counts per variable (issuer-level variables counted once per issuer; plan-level once per plan row):",
  "",
  md_tab(mk_show),
  "",
  "SECTION 4B. INTERNAL CONSISTENCY OF THE CMS FILE (checked on the parsed tables, raw counts as published)",
  "",
  paste0("- In-network denied exceeds received: ", sum(issuer_state$in_network_denied_exceeds_received, na.rm = TRUE), " issuers and ", sum(plans$in_network_denied_exceeds_received, na.rm = TRUE), " plans (of ",
         sum(!is.na(issuer_state$in_network_denied_exceeds_received)), " and ", sum(!is.na(plans$in_network_denied_exceeds_received)), " comparable rows)."),
  paste0("- PINNED KNOWN ANOMALY, out-of-network denied exceeds received: ", nrow(oon_iss_bad), " issuers (of ", sum(!is.na(issuer_state$out_of_network_denied_exceeds_received)), " comparable) and ", nrow(oon_plan_bad), " plans (of ",
         sum(!is.na(plans$out_of_network_denied_exceeds_received)), " comparable), in the CMS file itself, not from the parser. Issuer level: ", paste(oon_issuer_text, collapse = "; and "),
         ". Plan level: ", nrow(oon_plan_bad), " plans over ", length(oon_by_issuer), " issuers: ", paste0(names(oon_by_issuer), " (", as.integer(oon_by_issuer), ")", collapse = ", "), "."),
  paste0("- KFF working file note: KFF's own working file carries identical out-of-network received and denied counts for these issuers (Medical Issuers sheet: identical = ", kf_oon_iss_same, ") and its Medical Plans sheet flags the same plans with denied above received (same set = ", kf_oon_plan_same,
         "), so the anomaly is in the CMS source; KFF's published out-of-network figures (44 million claims, 37% denied) reproduce with these counts included (A06, A07), and A06 to A08 and the kff_ ledger values use the counts as published, anomaly included."),
  "- Handling: raw counts are never altered; the derived out-of-network denial rate is NA and the flag out_of_network_denied_exceeds_received is TRUE for exactly these rows. The out-of-network figures are descriptive only in this project (the outcome is in-network). Every in-network check stays strict (zero violations, issuer and plan level). tests/test_cms_parse.R pins this exact set (issuers, per-issuer plan counts and plan IDs, read from the raw workbook) and FAILS if it changes, for example with a new vintage or a parser change.",
  paste0("- Resubmitted exceeds received (in-network): ", sum(issuer_state$issuer_claims_resubmitted_in_network > issuer_state$issuer_claims_received_in_network, na.rm = TRUE), " issuers, ", sum(plans$plan_claims_resubmitted_in_network > plans$plan_claims_received_in_network, na.rm = TRUE), " plans. Internal appeals overturned exceed filed: ",
         sum(issuer_state$issuer_internal_appeals_overturned > issuer_state$issuer_internal_appeals_filed, na.rm = TRUE), " issuers. External appeals overturned exceed filed: ", sum(issuer_state$issuer_external_appeals_overturned > issuer_state$issuer_external_appeals_filed, na.rm = TRUE), " issuers."),
  "- CMS expects the denial reason tallies to total at least the in-network denied claims; that expectation is not asserted or tested here (reasons are tallies, not unique claims).",
  "",
  "SECTION 4C. COUNTS FOR THE SAP AND DOWNSTREAM TASKS (from the parsed tables, no imputation)",
  "",
  paste0("- Plans: ", fmt(nrow(plans)), " Individual QHP plan rows; ", fmt(sum(plans$plan_level_reported)), " with plan-level claims (in-network received numeric or **); ",
         fmt(sum(plans$plan_claims_received_in_network_marker == "numeric" & plans$plan_claims_received_in_network > 0 & plans$plan_claims_denied_in_network_marker == "numeric")),
         " have a computable in-network denial proportion (both counts numeric, received above 0)."),
  paste0("- Plans with plan-level data but no computable proportion: received suppressed (**) ", sum(plans$plan_claims_received_in_network_marker == "star2"), "; received numeric 0 ",
         sum(plans$plan_claims_received_in_network_marker == "numeric" & plans$plan_claims_received_in_network == 0), "; denied suppressed (**) with received numeric above 0 ",
         sum(plans$plan_claims_denied_in_network_marker == "star2" & plans$plan_claims_received_in_network_marker == "numeric" & plans$plan_claims_received_in_network > 0),
         ". Suppressed cells are the only ones eligible for the suppression bounds."),
  paste0("- Plans with a computable proportion that also have numeric average monthly enrollment: ",
         fmt(sum(plans$plan_claims_received_in_network_marker == "numeric" & plans$plan_claims_received_in_network > 0 & plans$plan_claims_denied_in_network_marker == "numeric" & plans$avg_monthly_enrollment_marker == "numeric")),
         "; in a plan-level analysis restricted to the 157 issuers with more than 1,000 in-network claims: ",
         fmt(sum(plans$issuer_id %in% K$issuer_id & plans$plan_claims_received_in_network_marker == "numeric" & plans$plan_claims_received_in_network > 0 & plans$plan_claims_denied_in_network_marker == "numeric")), " computable, ",
         fmt(sum(plans$issuer_id %in% K$issuer_id & plans$plan_claims_received_in_network_marker == "numeric" & plans$plan_claims_received_in_network > 0 & plans$plan_claims_denied_in_network_marker == "numeric" & plans$avg_monthly_enrollment_marker == "numeric")), " of them with numeric enrollment."),
  paste0("- Issuer-state level: ", sum(issuer_state$issuer_claims_received_in_network_marker == "numeric"), " issuers have a numeric in-network claims count (", fmt(sum(issuer_state$issuer_claims_received_in_network, na.rm = TRUE)), " claims); ",
         sum(issuer_state$issuer_claims_received_in_network_marker == "na"), " are N/A (new to the Exchange); in-network denied is suppressed (**) for ", sum(issuer_state$issuer_claims_denied_in_network_marker == "star2"), " issuer(s): ",
         paste(issuer_state$issuer_id[issuer_state$issuer_claims_denied_in_network_marker == "star2"], collapse = ", "), " (in the 157: ", sum(issuer_state$issuer_id[issuer_state$issuer_claims_denied_in_network_marker == "star2"] %in% K$issuer_id), ")."),
  paste0("- Issuers with a machine-readable index URL hold ", pc(sum(vol$in_network_claims_received[vol$has_mr_index_url], na.rm = TRUE) / sum(vol$in_network_claims_received, na.rm = TRUE), 2), " of the in-network claims received; distinct index URLs among the ",
         sum(vol$has_mr_index_url), " issuers with one: ", length(unique(mr_urls$mr_index_url[mr_urls$issuer_id %in% issuer_state$issuer_id])), " (several issuers share one index URL)."),
  paste0("- Plans linked to the Network PUF through the Plan Attributes network ID (same issuer and network ID): ", fmt(sum(paste(plans$issuer_id, plans$pa_network_id) %in% paste(nw$issuer_id, nw$network_id))), " of ", fmt(sum(plans$pa_matched)), " plans with a Plan Attributes row."),
  "",
  "SECTION 5. FILES, TABS AND JOINS",
  "",
  paste0("- Raw files are saved unmodified under v1/data_raw (zips, PDFs and the KFF working file; extracted members under extracted/; the two large CSVs, ", fmt(file.size(pa_path) / 1e6, 1), " MB and ", fmt(file.size(cx_path) / 1e6, 1), " MB, are in the cache). SHA-256 hashes are in v1/data_raw/provenance/manifest.csv; the transparency zip hash bcdc435e... and the dictionary hash 7663eca1... equal WP1's."),
  paste0("- Vintage: the CMS page says the Transparency in Coverage PUF was Updated September 26, 2025 (KFF's vintage), but the zip on the server is Last-Modified 2025-10-28 and the workbook inside is dated 2025-10-27. Issuer-level claims received and denied equal KFF's working file for ", sum(kn(kf_m$Claims_Received_In_Network) == kf_m$rin & kn(kf_m$Claims_Denied_In_Network) == kf_m$din), " of ", nrow(kf_iss), " issuers (E02); so any vintage difference is immaterial for the issuer-level reproduction."),
  "",
  md_tab(tab_show),
  "",
  paste0("- Issuer-level columns are repeated on every plan row of the issuer. De-duplicating on Issuer_ID: ", nrow(issuer_state), " issuers, ", n_states, " states, each issuer in one state. Repeated values differ for ", sum(nonident), " issuers across the ", length(nonident), " repeated columns checked."),
  paste0("- Plan Attributes PUF joined on Plan_ID = StandardComponentId (Individual, medical rows only) for fields the transparency PUF lacks: network ID and service area ID (link a plan to the Network PUF and service areas), HIOS product ID, IsNewPlan, and the metal level that separates Expanded Bronze (", sum(plans$pa_expanded_bronze, na.rm = TRUE), " plans) from Bronze. Matched ", fmt(sum(plans$pa_matched)), " of ", fmt(nrow(plans)), " plans; ", sum(!plans$pa_matched), " plan IDs have no Plan Attributes row (", sum(!plans$pa_matched & !plans$plan_level_reported), " of them have no plan-level claims). Plan type, issuer, state and metal level agree with the PUF on every matched plan, so none of those was taken from the join. All Plan Attributes fields used are constant across a plan's CSR variants (checked)."),
  paste0("- Plan ID Crosswalk joined only as two flags (plan ID listed as a 2025 plan or a 2026 plan). ", fmt(sum(plans$plan_listed_in_2025_crosswalk & plans$plan_level_reported)), " of ", fmt(sum(plans$plan_level_reported)), " plans with plan-level data were listed in 2025; ", sum(plans$plan_listed_in_2025_crosswalk & !plans$plan_level_reported), " of the ", fmt(sum(!plans$plan_level_reported)), " plan-level N/A plans were also listed in 2025 (offered in 2025 but not in 2024). The crosswalk is not used for the outcome."),
  paste0("- Catastrophic flag: the transparency PUF already carries Metal_Level = Catastrophic (", sum(plans$metal_level == "Catastrophic"), " plans); no join was needed for it."),
  paste0("- Machine-readable URL PUF: ", nrow(mr_urls), " issuers; ", sum(mr_urls$in_individual_qhp_tab), " of the ", nrow(issuer_state), " QHP issuers have a row. The Tech POC Email column was dropped (contact details are not carried into derived files). Missing: ", paste(no_mr, collapse = ", "), "."),
  paste0("- Network PUF: ", nrow(nw), " rows, ", sum(nw$in_individual_qhp_tab), " rows for QHP-tab issuers. QHP issuers with no individual medical Network PUF row: ", paste(no_nw, collapse = ", "), "."),
  "",
  "SECTION 6. DICTIONARY VERSUS WORKBOOK (every header that is not identical; full table in v1/data_clean/cms/dictionary_vs_workbook.csv)",
  "",
  md_tab(dv_show),
  "",
  paste0("- Dictionary labels with no workbook column: ", if (length(dict_only)) paste(dict_only, collapse = "; ") else "none", "."),
  paste0("- Plan_Type values in the file: ", paste(obs_values$plan_type, collapse = ", "), " (dictionary also lists Indemnity). Metal_Level values: ", paste(obs_values$metal_level, collapse = ", "), ". Exchange_Type values: ", paste(obs_values$exchange_type, collapse = ", "), ". Percent-overturned columns are on a 0 to 100 scale (observed maximum ",
         max(c(Q$issuer_internal_appeals_overturned_percent, Q$issuer_external_appeals_overturned_percent), na.rm = TRUE), ")."),
  paste0("- Header row: row ", tabs$qhp$header_row, " of the sheet, found as the first row containing the string Plan_ID in any column (column 8)."),
  "",
  "SECTION 7. OPEN ITEMS",
  "",
  "- D04 (the 165,863 upheld count) is unexplained; the percentage reproduces. It does not affect any number this project reports, because the project reports its own appeal totals.",
  "- 'Offering plans in 2026' for the two excluded issuers is inferred from absence in the 2026 Machine-readable URL and Network PUFs, not from the QHP Landscape file.",
  paste0("- The ** meaning is an inference: no numeric cell holds a value from 1 to 10 in the ", sum(is_cnt), " count columns (smallest positive value ", min_pos_count, ", true zeros shown as 0), so ** most likely stands for a count of 1 to 10. CMS states no threshold; this is not a CMS statement and is one unit wider than KFF's 'under 10' wording."),
  "",
  "SECTION 8. LEDGER KEYS NOT WRITTEN",
  "",
  paste0("- No att_internal_appeals_upheld key, although the WP2 spec step 8 lists upheld appeals: upheld is not computable consistently from this file. The overturned count is numeric for ", sum(up_ovt), " issuers but the filed count for ", sum(up_fil),
         " (both for ", sum(up_fil & up_ovt), " of the ", nrow(issuer_state), "); ", sum(up_fil & !up_ovt), " issuers report filed appeals (", fmt(sum(issuer_state$issuer_internal_appeals_filed[up_fil & !up_ovt])), ") with overturned suppressed, and ", sum(!up_fil & up_ovt),
         " report overturned with filed not numeric, so filed minus overturned mixes populations. The CMS percent overturned is numeric for only ", nrow(A_sub), " of the ", nrow(K), " issuers with more than 1,000 in-network claims. The upheld total therefore depends on the population: ", fmt(round(A_upheld_pct)), " (", pc(A_upheld_pct / A_filed), ") on the reported-percent subset, ",
         fmt(B_upheld), " (", pc(B_upheld / B_filed), ") on issuers with both counts, ", fmt(C_upheld), " (", pc(C_upheld / C_filed), ") on all appeals with suppressed overturned counted as zero (G2 rows D03, D03b, D04). The ledger instead holds att_internal_appeals_filed and att_internal_appeals_overturned with their issuers_reported and issuers_suppressed counts, so each total shows which issuers it covers, and kff_internal_upheld_share_reported_percent for the one upheld share the G2 table reproduces. Any downstream upheld figure must name its population.")
)
if (any(grepl(EMD, lines, fixed = TRUE))) stop("em-dash in report text")
writeLines(lines, file.path(P$qa, "g2_reconciliation.md"))
lg("wrote ", file.path(P$qa, "g2_reconciliation.md"))
lg("DONE")
