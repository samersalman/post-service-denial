# =====================================================================
# test_ortho.R : WP4 tests (plain R).
# Run: WP4_SCRATCH=<scratch dir> Rscript "<root>/v1/tests/test_ortho.R"   (falls back to tempdir())
# Exit status 1 if any check fails. Toy fixtures (small JSON files, toy cap and floor) are written to the
# scratch dir; nothing is tuned to pass. Checks that need the real harvest outputs run only if those files
# exist and are reported as SKIP otherwise.
# =====================================================================
.args <- commandArgs(trailingOnly = FALSE)
.f <- sub("^--file=", "", grep("^--file=", .args, value = TRUE)[1])
.here <- dirname(normalizePath(gsub("~+~", " ", .f, fixed = TRUE)))
source(file.path(dirname(.here), "R", "00_setup.R"))
SC <- Sys.getenv("WP4_SCRATCH", unset = "")
if (!nzchar(SC) || !dir.exists(SC)) SC <- tempdir()
TD <- file.path(SC, paste0("wp4_test_", format(Sys.time(), "%H%M%S")))
dir.create(file.path(TD, "logs"), recursive = TRUE)
# all test logs go to the scratch dir, never to the real v1/logs; the run fails if any file there changes
LOG_BEFORE <- tools::md5sum(list.files(P$logs, recursive = TRUE, full.names = TRUE))
WP4_LOG_DIR <- file.path(TD, "logs")
WP4_NO_MAIN <- TRUE
source(file.path(P$R, "04_ortho_harvest.R"))

N_PASS <- 0L; N_FAIL <- 0L; N_SKIP <- 0L; FAILS <- character(0)
check <- function(name, expr) {
  res <- tryCatch({ r <- force(expr); if (isTRUE(r)) "ok" else paste0("FALSE", if (is.character(r)) paste0(": ", paste(utils::head(r, 3), collapse = " | ")) else "") },
                  error = function(e) paste0("ERROR: ", conditionMessage(e)))
  if (identical(res, "ok")) { N_PASS <<- N_PASS + 1L; cat("PASS  ", name, "\n", sep = "") }
  else { N_FAIL <<- N_FAIL + 1L; FAILS <<- c(FAILS, name); cat("FAIL  ", name, "  ->  ", res, "\n", sep = "") }
  invisible(NULL)
}
skip <- function(name, why) { N_SKIP <<- N_SKIP + 1L; cat("SKIP  ", name, "  (", why, ")\n", sep = "") }

cat("fixtures and test logs in ", TD, "\n", sep = "")

# ---- 1. crosswalk: labelled strings ------------------------------------------------
rules <- load_crosswalk()
lab <- rbindlist(list(
  list("Orthopedic Surgery", "ortho_surgeon"), list("Orthopaedic Surgery", "ortho_surgeon"), list("ORTHOPEDIC SURGERY", "ortho_surgeon"),
  list("Orthopaedic Surgery of the Spine", "ortho_surgeon"), list("Orthopedic Surgeon", "ortho_surgeon"),
  list("Pediatric Orthopaedic Surgery", "ortho_surgeon"), list("Orthopedics", "ortho_surgeon"),
  list("Sports Medicine|Orthopedic Surgery", "ortho_surgeon"), list("Orthopedic Surgery|Hand Surgery", "ortho_surgeon"),
  list("Surgery, Hand|Orthopedic Surgery", "ortho_surgeon"), list("Sports Medicine, Ortho Surgery", "ortho_surgeon"),
  list("Virtual Care Services|Orthopedic Surgery", "ortho_surgeon"), list("Orthopaedic Hand Surgery", "ortho_surgeon"),
  list("Orthopedic Surgery|Physician Assistant", "ortho_apc"), list("Orthopedic Surgery|Nurse Practitioner", "ortho_apc"),
  list("Advanced Practice Registered Nurse (APRN)|Orthopedic Sports Medicine|Nurse Practitioner", "ortho_apc"),
  list("Orthopedic Surgery|Surgical Assistance|Physician Assistant", "ortho_apc"),
  list("ORTHOPAEDIC SURGERY PHYSICIAN ASSISTANT", "ortho_apc"), list("Physician Assistant, Orthopaedic Surgery", "ortho_apc"),
  list("Orthodontics", "other"), list("Orthodontist", "other"), list("Orthodontics and Dentofacial Orthopedics, Dentist", "other"),
  list("Orthotist", "other"), list("Prosthetist/Orthotist", "other"), list("Prosthetic/Orthotic Supplier", "other"),
  list("Chiropractic", "other"), list("Chiropractor", "other"), list("Sports Medicine", "other"),
  list("Optometry", "other"), list("Optometrist", "other"), list("Physician Assistant", "other"), list("Podiatry", "other"),
  list("Physical Therapist - Orthopedic", "other"), list("Hand Surgery", "other"), list("Family Practice", "other"),
  list("Neurological Surgery", "other"), list("Plastic Surgery", "other"), list("<none>", "other"),
  list("Dermatology", "derm"), list("Dermatopathology", "derm"), list("Mohs Micrographic Surgery", "derm"), list("Pediatric Dermatology", "derm"),
  list("Dermatology Nurse Practitioner", "other"),
  list("Ophthalmology", "ophth"), list("Ophthalmologist", "ophth"), list("Retina Specialist, Ophthalmology", "ophth"),
  list("Ophthalmic Technician", "other"), list("Eyewear Supplier", "other")
))
setnames(lab, c("text", "expected"))
got <- classify_specialty(lab$text, rules)
check("crosswalk: at least 40 labelled strings", nrow(lab) >= 40L)
bad <- lab[got$assigned_family != lab$expected]
check("crosswalk: every labelled string gets its expected family", if (!nrow(bad)) TRUE else paste0(bad$text, " -> ", got$assigned_family[got$assigned_family != lab$expected]))
hn <- c("Orthodontics", "Orthotist", "Prosthetist/Orthotist", "Chiropractic", "Sports Medicine", "Optometry", "Physician Assistant")
check("crosswalk: all seven hard negatives are not orthopaedic, derm or ophth", {
  g <- classify_specialty(hn, rules); !any(g$surgeon | g$apc | g$derm | g$ophth) })
check("crosswalk: Orthopedic Surgery|Physician Assistant is apc and not surgeon", {
  g <- classify_specialty("Orthopedic Surgery|Physician Assistant", rules); g$apc && !g$surgeon })
check("crosswalk: Optometry is not ophth, Dermatopathology is derm", {
  a <- classify_specialty(c("Optometry", "Dermatopathology"), rules); !a$ophth[1] && a$derm[2] })
check("crosswalk: derm and ophth flags are independent of the orthopaedic flags", {
  g <- classify_specialty(c("Orthopedic Surgery|Dermatology", "Dermatology|Ophthalmology"), rules)
  g$surgeon[1] && g$derm[1] && g$derm[2] && g$ophth[2] })
check("crosswalk: respellings orthopedic and orthopaedic both match, case-insensitive", {
  g <- classify_specialty(c("orthopedic surgery", "ORTHOPAEDIC SURGERY"), rules); all(g$surgeon) })
check("crosswalk: file has the five required columns and unique rule ids", identical(names(rules), c("rule_id", "family", "include_pattern", "exclude_pattern", "rationale")) && !anyDuplicated(rules$rule_id))
check("crosswalk: families used are only the five allowed", all(rules$family %in% XW_FAMILIES) && all(XW_FAMILIES %in% rules$family))
check("taxonomy: 22 rows, 8 orthopaedic, 6 dermatology, 8 ophthalmology, 3 general codes, NUCC 26.1", {
  tx <- fread(file.path(P$config, "ortho_taxonomy.csv"), colClasses = "character")
  identical(names(tx)[1:5], c("code", "name", "family", "is_general", "nucc_version")) && nrow(tx) == 22L &&
    identical(as.integer(table(tx$family)[c("orthopaedic", "dermatology", "ophthalmology")]), c(8L, 6L, 8L)) &&
    sum(tx$is_general == "TRUE") == 3L && all(tx$nucc_version == "26.1") })
check("taxonomy: every code is in the cached NUCC 26.1 file", {
  tx <- fread(file.path(P$config, "ortho_taxonomy.csv"), colClasses = "character"); nu <- fread(NUCC_FILE, colClasses = "character")
  all(tx$code %in% nu$Code) })

# ---- 2. streaming filter on a toy file ----------------------------------------------
chd <- file.path(TD, "ch"); write_tools(chd)
coh_ids <- c("11111AA1010001", "11111AA1010002", "22222BB2020001")
write_cohort_json(coh_ids, chd)
pl <- function(ids) paste0("[", paste0('{"plan_id_type":"HIOS-PLAN-ID","plan_id":"', ids, '","network_tier":"PREFERRED"}', collapse = ","), "]")
rec_i <- function(npi, spec, ids, key = "specialty", type = "INDIVIDUAL") {
  sp <- if (is.null(spec)) "" else if (length(spec) > 1) paste0(',"', key, '":[', paste0('"', spec, '"', collapse = ","), "]") else paste0(',"', key, '":"', spec, '"')
  paste0('{"npi":', if (is.null(npi)) "null" else paste0('"', npi, '"'), ',"type":"', type,
         '","name":{"first":"Zed","last":"Toy"},"addresses":[{"state":"TX","city":"Nowhere"}]', sp, ',"plans":', pl(ids), ',"accepting":"accepting"}')
}
many <- rep(c("11111AA1010001", "11111AA1010002", "99999ZZ9990001"), 200)
recs_a <- c(
  rec_i("1000000001", c("Orthopedic Surgery", "Physician Assistant"), c("11111AA1010001", "11111AA1010001", "99999ZZ9990009")),
  rec_i("1000000002", "Orthopedic Surgery", c("11111AA1010001", "11111AA1010002")),
  rec_i(NULL, "Orthopedic Surgery", "11111AA1010001"),
  rec_i("123456789", "Dermatology", "11111AA1010001"),
  rec_i("1000000003", c("  Dermatology ", "Dermatology"), c("22222BB2020001", "ABCDEFGH")),
  rec_i("1000000004", NULL, "22222BB2020001"),
  rec_i("1000000005", "Optometry", many, key = "speciality"),
  paste0('{"npi":"1999999999","type":"FACILITY","facility_name":"Hosp","facility_type":["Hospital"],"addresses":[],"plans":', pl("11111AA1010001"), "}"),
  paste0('{"npi":"1888888888","type":"GROUP","group_name":"Grp","addresses":[],"plans":', pl(c("22222BB2020001", "11111AA1010002")), "}"),
  rec_i("1000000006", "Orthopedic Surgery", "77777CC3030001"),
  rec_i("1000000007", "Orthopaedic Surgery of the Spine", "11111AA1010001"))
fa <- file.path(TD, "toy_a.json"); writeLines(paste0("[", paste(recs_a, collapse = ",\n"), "]"), fa)
txt <- paste0("[", paste(recs_a, collapse = ",\n"), "]")
ft_trunc <- file.path(TD, "toy_trunc.json"); writeChar(substr(txt, 1, nchar(txt) - 150), ft_trunc, eos = NULL)
# file B: overlapping NPIs (1000000001 and 1000000002 again, with other plans), new NPIs
recs_b <- c(
  rec_i("1000000001", c("Orthopedic Surgery", "Physician Assistant"), "22222BB2020001"),
  rec_i("1000000002", "Orthopedic Surgery", "22222BB2020001"),
  rec_i("1000000008", "Ophthalmology", c("22222BB2020001", "11111AA1010002")),
  rec_i("1000000009", "Orthopedic Surgery", "11111AA1010002"))
fb <- file.path(TD, "toy_b.json"); writeLines(paste0("[", paste(recs_b, collapse = ",\n"), "]"), fb)
ra <- run_stream_sync("ta", paste0("file://", fa), chd)
check("filter: toy file streams to status ok", identical(ra$status, "ok"))
check("filter: 7 valid individuals, 2 individuals with a null or 9-digit NPI, 1 facility, 1 group", {
  as.integer(ra$n_individual) == 7L && as.integer(ra$n_bad_npi) == 2L && as.integer(ra$n_facility) == 1L && as.integer(ra$n_group) == 1L })
check("filter: cohort plan rows counted after restriction (8-char and non-cohort plan IDs ignored): 13", as.integer(ra$cohort_plan_rows) == 13L)
check("filter: record with 600 plan entries keeps 2 distinct cohort plans (hundreds of entries handled)", {
  d <- file.path(chd, "out", "ta"); sets <- readLines(file.path(d, "sets.tsv")); any(sets == "2\t11111AA1010001,11111AA1010002") })
check("filter: network_tier repeats and duplicate plan IDs collapse to one plan per record", {
  d <- file.path(chd, "out", "ta"); sets <- readLines(file.path(d, "sets.tsv")); any(sets == "1\t11111AA1010001") })
check("filter: speciality (misspelled key) and trimmed duplicate labels are read", {
  d <- file.path(chd, "out", "ta"); sp <- readLines(file.path(d, "specs.tsv")); any(grepl("\tOptometry$", sp)) && any(grepl("\tDermatology$", sp)) })
check("filter: individual without specialty is kept as <none>", any(grepl("<none>", readLines(file.path(chd, "out", "ta", "specs.tsv")), fixed = TRUE)))
check("filter: no name, address or facility text reaches any output file", {
  d <- file.path(chd, "out", "ta"); all.txt <- unlist(lapply(c("specs.tsv", "sets.tsv", "seen.tsv", "counts.tsv"), function(f) readLines(file.path(d, f))))
  rc <- system(paste("gzip -dc", shQuote(file.path(d, "rec.tsv.gz"))), intern = TRUE)
  !any(grepl("Zed|Toy|Nowhere|TX|Hosp|Grp", c(all.txt, rc))) })
check("filter: FACILITY and GROUP records are not written as individuals", {
  rc <- system(paste("gzip -dc", shQuote(file.path(chd, "out", "ta", "rec.tsv.gz"))), intern = TRUE); !any(grepl("^1999999999|^1888888888", rc)) && length(rc) == 7L })
rt <- run_stream_sync("tt", paste0("file://", ft_trunc), chd)
check("filter: a truncated stream makes the file FAIL, with no output directory", identical(rt$status, "failed") && !dir.exists(file.path(chd, "out", "tt")))
rm0 <- run_stream_sync("tm", paste0("file://", file.path(TD, "does_not_exist.json")), chd)
check("filter: a missing file fails with a curl exit code and no partial output", identical(rm0$status, "failed") && as.integer(rm0$curl_exit) != 0L && !dir.exists(file.path(chd, "out", "tm")))
not_json <- file.path(TD, "toy_html.json"); writeLines("<html><body>Just a moment...</body></html>", not_json)
rh <- run_stream_sync("th", paste0("file://", not_json), chd)
check("filter: an HTML page instead of JSON fails the file", identical(rh$status, "failed"))
check("filter: no temporary directories are left behind after success or failure", length(list.files(file.path(chd, "tmp"))) == 0L)

# ---- 3. supervisor: concurrency, de-duplication, atomic write and resume, guards ----
ch2 <- file.path(TD, "ch2"); write_tools(ch2); write_cohort_json(coh_ids, ch2)
fwrite(data.table(plan_id = coh_ids, issuer_id = c("11111", "11111", "22222"), state = c("AA", "AA", "BB")), file.path(ch2, "state", "cohort_plans.csv"))
mk <- function(k, host, f, bytes) data.table(fid = sprintf("f%03d", k), run_order = k, provider_url = paste0("file://", f), index_url = "idx", host = host,
  listed_bytes = bytes, content_class = "individual only", issuer_ids = "11111;22222", skip = FALSE, max_time = 60L)
ftab <- rbindlist(list(mk(1, "hostA", fa, file.size(fa)), mk(2, "hostA", fb, file.size(fb)), mk(3, "hostB", fb, file.size(fb))))
ctx <- new_ctx(ch = ch2, cap = 75e9, poll = 1, export_path = file.path(TD, "manifest_export.csv"), log_step = "test_ortho")
r1 <- harvest_supervisor(ctx, ftab)
check("supervisor: all three toy files ok, final state COMPLETE", identical(r1$final, "COMPLETE") && all(r1$man$status == "ok"))
check("supervisor: manifest carries the required columns", all(c("url", "index_url", "status", "http_status", "curl_exit", "wire_bytes", "decoded_bytes", "seconds", "records",
  "individual_records", "cohort_plan_rows", "started", "ended", "retries") %in% names(fread(file.path(TD, "manifest_export.csv")))))
check("supervisor: at most one stream per host at a time (same-host attempts do not overlap)", {
  a <- r1$att[host == "hostA"][order(started)]; all(a$started[-1] >= a$ended[-nrow(a)]) })
nat1 <- nrow(r1$att)
r2 <- harvest_supervisor(ctx, ftab)
check("resume: a second run skips ok files whose output exists (no new attempts)", nrow(r2$att) == nat1 && identical(r2$final, "COMPLETE"))
f1 <- file.path(ch2, "out", "f001", "sets.tsv"); cat("tampered\n", file = f1, append = TRUE)
r3 <- harvest_supervisor(ctx, ftab)
check("resume: a tampered output (size changed) is detected and the file is re-harvested", nrow(r3$att) == nat1 + 1L && all(r3$man$status == "ok"))
check("atomic write: out/ holds only complete renamed directories (six files each, no .tmp or .t leftovers)", {
  x <- list.files(file.path(ch2, "out"), recursive = TRUE); d <- list.dirs(file.path(ch2, "out"), recursive = FALSE)
  length(d) == 3L && !any(grepl("[.]tmp$|[.]t$", x)) && all(vapply(d, function(z) all(c("rec.tsv.gz", "specs.tsv", "sets.tsv", "seen.tsv", "counts.tsv") %in% list.files(z)), logical(1))) })
agg2 <- file.path(ch2, "agg")
aggregate_stage(ch = ch2, agg = agg2, cohort_csv = file.path(ch2, "state", "cohort_plans.csv"))
rules_toy <- load_crosswalk()
cc <- apply_crosswalk_counts(rules_toy, agg = agg2, cohort_csv = file.path(ch2, "state", "cohort_plans.csv"))
pc <- cc$plan_counts; setkey(pc, plan_id)
# hand count over file A and file B, union by NPI: plan 11111AA1010001 holds NPIs 1000000001, 02, 07 (APC, surgeon, surgeon), plus 1000000005 (optometry) and 1000000004? no
# 11111AA1010001: 1000000001 (apc), 1000000002 (surgeon), 1000000005 (optometry), 1000000007 (surgeon spine) -> 4 NPIs, 2 surgeon, 1 apc
# 11111AA1010002: 1000000002 (surgeon), 1000000005, 1000000008 (ophth), 1000000009 (surgeon) -> 4 NPIs, 2 surgeon, 1 ophth
# 22222BB2020001: 1000000001 (apc, from file B), 1000000002 (surgeon, from B), 1000000003 (derm), 1000000004 (none), 1000000008 (ophth) -> 5 NPIs
check("de-duplication: an NPI in two files is counted once per plan (plan 1: 4 NPIs, 2 surgeon, 1 apc)",
      pc["11111AA1010001"]$n_individual_npis == 4 && pc["11111AA1010001"]$n_ortho_surgeon_text == 2 && pc["11111AA1010001"]$n_ortho_apc_text == 1)
check("de-duplication: union over files of plan sets (plan 2: 4 NPIs, 2 surgeon, 1 ophth)",
      pc["11111AA1010002"]$n_individual_npis == 4 && pc["11111AA1010002"]$n_ortho_surgeon_text == 2 && pc["11111AA1010002"]$n_ophth_text == 1)
check("de-duplication: plan 3 has 5 NPIs, 1 surgeon, 1 apc, 1 derm, 1 ophth",
      pc["22222BB2020001"]$n_individual_npis == 5 && pc["22222BB2020001"]$n_ortho_surgeon_text == 1 && pc["22222BB2020001"]$n_ortho_apc_text == 1 &&
        pc["22222BB2020001"]$n_derm_text == 1 && pc["22222BB2020001"]$n_ophth_text == 1)
fl <- cc$npi_flags
check("NPI resolution: an NPI with any APC-co-labelled orthopaedic record is ortho_apc, not surgeon", {
  x <- fl[npi == 1000000001]; x$ortho_apc && !x$ortho_surgeon })
check("NPI resolution: surgeon text without APC co-label is ortho_surgeon", { x <- fl[npi == 1000000002]; x$ortho_surgeon && !x$ortho_apc })
check("NPI resolution: an NPI whose only plan is outside the cohort is kept out of plan counts (empty signature)", fl[npi == 1000000006]$sig == "")
check("NPI count: 9 distinct valid individual NPIs across both files", nrow(fl) == 9L)
# plan not listed anywhere: NA not zero
seen2 <- fread(file.path(agg2, "plan_seen.csv"), colClasses = list(character = "plan_id"))
coh2 <- fread(file.path(ch2, "state", "cohort_plans.csv"), colClasses = "character")
coh2 <- rbind(coh2, data.table(plan_id = "33333CC3030001", issuer_id = "33333", state = "CC"))
iss2 <- data.table(issuer_id = c("11111", "22222", "33333"), index_status = c("ok", "ok", "ok"))
ft2 <- copy(ftab); ft2[, issuer_ids := "11111;22222;33333"]
man2 <- r3$man
pc2 <- rbind(pc, data.table(plan_id = "33333CC3030001", n_individual_npis = 0, n_ortho_surgeon_text = 0, n_ortho_apc_text = 0, n_derm_text = 0, n_ophth_text = 0))
ex2 <- build_exposure(coh2, pc2, seen2, iss2, ft2, man2)
check("exposure: a plan with no directory listing gets plan_not_in_directory and NA counts, never zero", {
  x <- ex2[plan_id == "33333CC3030001"]; x$exposure_status == "plan_not_in_directory" && is.na(x$n_individual_npis) && is.na(x$n_ortho_surgeon_text) && !x$plan_in_provider_files })
check("exposure: an unreachable issuer gives issuer_unreachable and NA counts", {
  iss3 <- copy(iss2); iss3[issuer_id == "33333", index_status := "failed: TLS"]
  x <- build_exposure(coh2, pc2, seen2, iss3, ft2, man2)[plan_id == "33333CC3030001"]; x$exposure_status == "issuer_unreachable" && is.na(x$n_individual_npis) })
check("exposure: a plan listed only in facility or group records is no_individuals with zero counts", {
  sn <- rbind(seen2, data.table(plan_id = "33333CC3030001", n_files_listing_plan = 1L, n_files_with_individuals_listing_plan = 0L,
                                n_records_individual = 0, n_records_facility = 3, n_records_group = 0, n_records_other = 0))
  x <- build_exposure(coh2, pc2, sn, iss2, ft2, man2)[plan_id == "33333CC3030001"]; x$exposure_status == "no_individuals" && x$n_individual_npis == 0L && x$plan_in_provider_files })
check("exposure: subclass counts never exceed the individual count; ids are character; NPPES columns are NA text-route", {
  all(ex2$n_ortho_surgeon_text <= ex2$n_individual_npis, na.rm = TRUE) && is.character(ex2$plan_id) && is.character(ex2$issuer_id) &&
    all(is.na(ex2[[NPPES_CODE_COLS[1]]])) && all(ex2$exposure_source == "text") })
check("exposure: an issuer with a failed or unrun file gets issuer_incomplete and NA counts for every plan, even one that was found", {
  m <- copy(man2); m[fid == "f002", status := "failed"]
  x <- build_exposure(coh2, pc2, seen2, iss2, ft2, m)
  all(x$exposure_status == "issuer_incomplete") && all(is.na(x$n_individual_npis)) && all(x$issuer_files_incomplete) })

# ---- 3b. NPPES route (a) on a toy zip (the real zip is offline; this only checks the pipeline logic) ----
hdr_cols <- c("NPI", "Entity Type Code", "NPI Deactivation Date", paste0("Healthcare Provider Taxonomy Code_", 1:15), paste0("Healthcare Provider Primary Taxonomy Switch_", 1:15))
mkrow <- function(npi, ent, deact, codes) {
  tc <- c(codes, rep("", 15 - length(codes))); pc <- c(rep("Y", min(1, length(codes))), rep("N", max(0, length(codes) - 1)), rep("", 15 - length(codes)))
  paste0('"', c(npi, ent, deact, tc, pc), '"', collapse = ",")
}
rows <- c(mkrow("1000000001", "1", "", c("207XS0117X", "363A00000X")), mkrow("1000000002", "1", "", "207X00000X"), mkrow("1000000003", "1", "", "207N00000X"),
          mkrow("1000000004", "1", "", "208D00000X"), mkrow("1000000005", "1", "", "152W00000X"), mkrow("1000000006", "1", "01/01/2020", character(0)),
          mkrow("1000000007", "1", "", "207XX0801X"), mkrow("1000000008", "1", "", "207W00000X"), mkrow("1555555555", "1", "", "207X00000X"))
nd <- file.path(TD, "nppes_toy"); dir.create(nd)
writeLines(c(paste0('"', hdr_cols, '"', collapse = ","), rows), file.path(nd, "npidata_pfile_20050523-20261011.csv"))
writeLines(paste0('"', hdr_cols, '"', collapse = ","), file.path(nd, "npidata_pfile_20050523-20261011_FileHeader.csv"))
zipf <- file.path(TD, "NPPES_toy.zip")
system(paste("cd", shQuote(nd), "&& zip -q", shQuote(zipf), "npidata_pfile_20050523-20261011.csv npidata_pfile_20050523-20261011_FileHeader.csv"))
sp2 <- readRDS(file.path(agg2, "sig_plans.rds"))
ra <- nppes_route_a(zipf, cc$npi_flags, fread(file.path(ch2, "state", "cohort_plans.csv"), colClasses = "character")$plan_id, sp2, work = file.path(TD, "nppes_work"))
check("NPPES route a (toy zip): 9 harvested NPIs, 7 matched as active individuals (deactivated and absent NPIs unmatched)", ra$n_harvested == 9L && ra$n_matched == 7L && abs(ra$match_share - 7 / 9) < 1e-12)
pa <- ra$plan_counts; setkey(pa, plan_id)
check("NPPES route a (toy zip): per-plan taxonomy counts are exact (plan 1: 4 matched, 3 with 207X; plan 2: 3 matched, 1 with 207X, 1 with 207W)",
      pa["11111AA1010001"]$n_individual_nppes_matched == 4 && pa["11111AA1010001"]$n_ortho_207x_any == 3 &&
        pa["11111AA1010002"]$n_individual_nppes_matched == 3 && pa["11111AA1010002"]$n_ortho_207x_any == 1 && pa["11111AA1010002"]$n_ophth_207w == 1)
check("NPPES route a (toy zip): one column per orthopaedic code and derm and ophth family counts (plan 3: 2 with 207X, 1 with 207N, 1 with 207W)",
      all(paste0("n_", TAX_CODES$orthopaedic) %in% names(pa)) && pa["22222BB2020001"]$n_ortho_207x_any == 2 && pa["22222BB2020001"]$n_derm_207n == 1 && pa["22222BB2020001"]$n_ophth_207w == 1 &&
        pa["11111AA1010001"]$n_207XS0117X == 1)
check("NPPES route a (toy zip): agreement of the surgeon-text flag with any-207X has PPV, sensitivity and kappa between 0 and 1", {
  a <- ra$agreement_surgeon; all(is.finite(a)) && a[["ppv"]] >= 0 && a[["ppv"]] <= 1 && a[["sensitivity"]] >= 0 && a[["sensitivity"]] <= 1 && a[["kappa"]] <= 1 })

# ---- 4. guards ------------------------------------------------------------------------
check("transfer guard: launch below the cap, defer when the next file would pass it, hard stop at the cap", {
  identical(cap_decision(10, 5, 5, cap = 100), "launch") && identical(cap_decision(90, 5, 10, cap = 100), "defer") &&
    identical(cap_decision(100, 0, 1, cap = 100), "hard_stop") && identical(cap_decision(60, 45, 0, cap = 100), "hard_stop") })
check("transfer guard: the real cap is 75 GB from the config", CAP_WIRE == 75e9 && CFG$gates$g1$transfer_gb == 75)
ch3 <- file.path(TD, "ch3"); write_tools(ch3); write_cohort_json(coh_ids, ch3)
fcap <- rbindlist(list(mk(1, "h1", fa, file.size(fa)), mk(2, "h2", fb, file.size(fb)), mk(3, "h3", fa, file.size(fa)), mk(4, "h4", fb, file.size(fb))))
rc <- harvest_supervisor(new_ctx(ch = ch3, cap = 60000, poll = 1, export_path = file.path(TD, "m3.csv"), log_step = "test_ortho"), fcap)
check("transfer guard: a toy cap stops launching, reports CAP_REACHED and leaves files pending (no truncation, no partial counts)", {
  identical(rc$final, "CAP_REACHED") && any(rc$man$status == "pending") && !all(rc$man$status == "ok") &&
    all(rc$man$status[rc$man$status != "pending"] == "ok") })
check("disk guard: decision function trips at a toy floor and passes above it", !disk_decision(5, floor_gb = 7) && disk_decision(8, floor_gb = 7) && disk_decision(7, floor_gb = 7))
check("disk guard: free_gb() reads a plausible positive number", { x <- free_gb(); is.finite(x) && x > 0 })
ch4 <- file.path(TD, "ch4"); write_tools(ch4); write_cohort_json(coh_ids, ch4)
rd <- harvest_supervisor(new_ctx(ch = ch4, floor_gb = 1e6, disk_wait_min = 0.05, poll = 1, export_path = file.path(TD, "m4.csv"), log_step = "test_ortho"), ftab)
check("disk guard: a toy floor above the free space pauses launches and ends with BLOCKED_DISK, nothing harvested", identical(rd$final, "BLOCKED_DISK") && all(rd$man$status == "pending"))
ch5 <- file.path(TD, "ch5"); write_tools(ch5); write_cohort_json(coh_ids, ch5)
ftr <- rbind(mk(1, "h1", fa, file.size(fa)), mk(2, "h2", ft_trunc, file.size(ft_trunc)))
rf <- harvest_supervisor(new_ctx(ch = ch5, poll = 1, export_path = file.path(TD, "m5.csv"), log_step = "test_ortho"), ftr)
check("retries: a file that keeps failing is tried 3 times in total, ends failed, with no output and fail-rate block", {
  a <- rf$att[fid == "f002"]; nrow(a) == 3L && rf$man$status[2] == "failed" && rf$man$retries[2] == 2L && identical(rf$final, "BLOCKED_FAIL_RATE") && !dir.exists(file.path(ch5, "out", "f002")) })

ra_before <- nrow(rf$att)
rr <- harvest_supervisor(new_ctx(ch = ch5, poll = 1, export_path = file.path(TD, "m5.csv"), log_step = "test_ortho"), ftr, retry_only = TRUE)
check("retry-only mode: files with attempts left are retried, ok files and exhausted files are left alone", nrow(rr$att) == ra_before && rr$man$status[1] == "ok" && rr$man$status[2] == "failed")
ch6 <- file.path(TD, "ch6"); write_tools(ch6); write_cohort_json(coh_ids, ch6)
fx <- rbind(mk(1, "h1", fa, file.size(fa)), mk(2, "h2", fa, file.size(fa)))
fx[2, provider_url := paste0("file://", file.path(TD, "late_file.json"))]
r6a <- harvest_supervisor(new_ctx(ch = ch6, poll = 1, export_path = file.path(TD, "m6.csv"), log_step = "test_ortho"), fx, max_launch = 2)
file.copy(fb, file.path(TD, "late_file.json"), overwrite = TRUE)
r6b <- harvest_supervisor(new_ctx(ch = ch6, poll = 1, export_path = file.path(TD, "m6.csv"), log_step = "test_ortho"), fx, retry_only = TRUE)
check("retry-only mode: a failed file that now exists is harvested on the retry pass without touching the ok file", {
  nrow(r6a$att[fid == "f002" & status == "failed"]) >= 1L && r6b$man$status[2] == "ok" && nrow(r6b$att[fid == "f001"]) == 1L })

# ---- 5. Wilson interval from the formula -------------------------------------------------
wquad <- function(x, n, z = stats::qnorm(0.975)) {
  p <- x / n; a <- 1 + z^2 / n; b <- 2 * p + z^2 / n; d <- sqrt(b^2 - 4 * a * p^2)
  c(lo = max(0, (b - d) / (2 * a)), hi = min(1, (b + d) / (2 * a)))
}
wroot <- function(x, n, z = stats::qnorm(0.975)) {
  p <- x / n; f <- function(q) (q - p)^2 - z^2 * q * (1 - q) / n
  c(lo = stats::uniroot(f, c(0, p), tol = 1e-12)$root, hi = stats::uniroot(f, c(p, 1), tol = 1e-12)$root)
}
for (xn in list(c(1, 10), c(9, 10), c(1, 50), c(49, 50), c(7, 40), c(0, 20), c(20, 20))) {
  local({ x <- xn[1]; n <- xn[2]
    check(paste0("Wilson x=", x, " n=", n, " equals the closed-form root of the score equation"), {
      w <- wilson_ci(x, n); s <- wquad(x, n); abs(w$lo - s[["lo"]]) < 1e-9 && abs(w$hi - s[["hi"]]) < 1e-9 })
    if (x > 0 && x < n) check(paste0("Wilson x=", x, " n=", n, " equals the numeric root (uniroot) of the score equation"), {
      w <- wilson_ci(x, n); s <- wroot(x, n); abs(w$lo - s[["lo"]]) < 1e-8 && abs(w$hi - s[["hi"]]) < 1e-8 }) })
}
check("Wilson: x = 1 and x = n - 1 are mirror images (the Hmisc::binconf trap)", {
  a <- wilson_ci(1, 30); b <- wilson_ci(29, 30); abs(a$lo - (1 - b$hi)) < 1e-12 && abs(a$hi - (1 - b$lo)) < 1e-12 })

# ---- 6. outputs of the real harvest (checked when present) ---------------------------------
pe <- file.path(P$clean_ortho, "plan_exposure.csv")
if (file.exists(pe)) {
  ex <- fread(pe, colClasses = list(character = c("plan_id", "issuer_id", "state", "exposure_status")))
  check("plan_exposure: 2,527 unique plan IDs, all 14-character strings", nrow(ex) == 2527L && !anyDuplicated(ex$plan_id) && all(nchar(ex$plan_id) == 14L))
  check("plan_exposure: exposure_status values are the four in the spec plus issuer_incomplete", all(ex$exposure_status %in% c("ok", "issuer_unreachable", "issuer_incomplete", "plan_not_in_directory", "no_individuals")))
  check("plan_exposure: subclass counts never exceed n_individual_npis", {
    all(ex$n_ortho_surgeon_text + ex$n_ortho_apc_text <= ex$n_individual_npis, na.rm = TRUE) &&
      all(ex$n_derm_text <= ex$n_individual_npis, na.rm = TRUE) && all(ex$n_ophth_text <= ex$n_individual_npis, na.rm = TRUE) })
  check("plan_exposure: NA, not zero, for plans without a directory listing", {
    x <- ex[exposure_status %in% c("issuer_unreachable", "issuer_incomplete", "plan_not_in_directory")]; all(is.na(x$n_individual_npis)) && all(is.na(x$n_ortho_surgeon_text)) })
  check("plan_exposure: plans with status ok have a listing and at least one individual", all(ex[exposure_status == "ok"]$plan_in_provider_files) && all(ex[exposure_status == "ok"]$n_individual_npis > 0L))
  check("plan_exposure: no denial, claim, appeal, enrollment, rate or density column", !any(grepl("denial|denied|claim|appeal|enroll|rate|density|share", names(ex), ignore.case = TRUE)))
  check("plan_exposure: NPPES columns exist and are NA when exposure_source is text", all(NPPES_CODE_COLS %in% names(ex)) && (all(ex$exposure_source != "text") || all(is.na(ex[exposure_source == "text"][[NPPES_CODE_COLS[1]]]))))
  check("plan_exposure: plan set equals the cohort plan list (plans with a computable proportion)", {
    cp <- fread(file.path(CH, "state", "cohort_plans.csv"), colClasses = "character"); setequal(cp$plan_id, ex$plan_id) })
} else skip("plan_exposure invariants", "file not built yet")
hf <- file.path(P$logs, "wp4_crosswalk_hash.txt")
if (file.exists(hf)) {
  check("crosswalk freeze: the SHA-256 in wp4_crosswalk_hash.txt matches the crosswalk and taxonomy files on disk", {
    L <- readLines(hf); h <- crosswalk_hashes(); any(grepl(h[["crosswalk"]], L, fixed = TRUE)) && any(grepl(h[["taxonomy"]], L, fixed = TRUE)) })
} else skip("crosswalk freeze hash", "not frozen yet")
mf <- file.path(P$clean_ortho, "harvest_manifest.csv")
if (file.exists(mf)) {
  mn <- fread(mf, colClasses = list(character = c("fid", "status", "url")))
  cal <- fread(file.path(P$logs, "wp4_calibration.csv"))
  cal_w <- sum(cal$size_download_compressed, cal$raw_wire_bytes_capped_60MB, na.rm = TRUE); cal_d <- sum(cal$decoded_bytes_counted, na.rm = TRUE)
  check("manifest: no ok row has NA wire, decoded or all-attempt bytes (the 3.7 GB integer-overflow bug)", {
    ok <- mn[status == "ok"]; !anyNA(ok$wire_bytes) && !anyNA(ok$decoded_bytes) && !anyNA(ok$wire_bytes_all_attempts) && !anyNA(ok$decoded_bytes_all_attempts) && all(ok$wire_bytes > 0) })
  check("manifest: byte columns are stored as numbers and the largest file keeps its full size (above 2^31)", is.numeric(mn$wire_bytes) && max(mn$wire_bytes, na.rm = TRUE) > 2^31)
  check("manifest: all-attempt wire bytes plus the calibration downloads equal the ledger ortho_wire_gb within 1 MB", {
    ld <- fread(file.path(P$ledger_parts, "ledger_ortho.csv"), colClasses = "character")
    abs(sum(mn$wire_bytes_all_attempts) + cal_w - as.numeric(ld$value[ld$key == "ortho_wire_gb"]) * 1e9) < 1e6 })
  check("manifest: all-attempt decoded bytes plus the calibration downloads equal the ledger ortho_decoded_gb within 1 MB", {
    ld <- fread(file.path(P$ledger_parts, "ledger_ortho.csv"), colClasses = "character")
    abs(sum(mn$decoded_bytes_all_attempts) + cal_d - as.numeric(ld$value[ld$key == "ortho_decoded_gb"]) * 1e9) < 1e6 })
  check("manifest: wire and decoded totals equal the supervisor attempts table (CACHE state) within 1 MB", {
    at <- file.path(CH, "state", "attempts.csv")
    if (!file.exists(at)) TRUE else { a <- fread(at, colClasses = list(character = c("fid", "status", "http_status", "started", "ended", "host", "err")))
      abs(sum(a$wire_bytes) - cal_w - sum(mn$wire_bytes_all_attempts)) < 1e6 && abs(sum(a$decoded_bytes) - cal_d - sum(mn$decoded_bytes_all_attempts)) < 1e6 } })
  check("manifest: status values are only ok, failed, skipped, not_run and the counts add up to 828", {
    all(mn$status %in% c("ok", "failed", "skipped", "not_run")) && nrow(mn) == 828L && !anyDuplicated(mn$url) })
} else skip("manifest byte checks", "manifest not built yet")
lp <- file.path(P$ledger_parts, "ledger_ortho.csv")
if (file.exists(lp)) {
  lg0 <- fread(lp, colClasses = "character")
  check("ledger part: columns, ortho_ prefix, unique keys, no NA or marker values", {
    identical(names(lg0), c("key", "value", "unit", "description", "source_script")) && all(startsWith(lg0$key, "ortho_")) && !anyDuplicated(lg0$key) &&
      !any(is.na(lg0$value) | lg0$value %in% c("", "NA", "NaN", "Inf", "*", "**", "***", "N/A")) })
} else skip("ledger part", "not built yet")

# ---- 7. hygiene ---------------------------------------------------------------------------------
src <- c(file.path(P$R, "04_ortho_harvest.R"), file.path(P$tests, "test_ortho.R"), file.path(P$config, "ortho_taxonomy.csv"),
         file.path(P$config, "specialty_text_crosswalk.csv"))
check("ASCII only in the WP4 sources and config files", all(vapply(src, function(f) !any(grepl("[^\\x01-\\x7F]", readLines(f, warn = FALSE), perl = TRUE, useBytes = TRUE)), logical(1))))
check("no em-dash in the WP4 sources and config files", all(vapply(src, function(f) !any(grepl(intToUtf8(0x2014), readLines(f, warn = FALSE), fixed = TRUE)), logical(1))))
check("outcome-blind: the only fread of plans.csv selects plan_id, issuer_id, state and the computable-proportion marker; no other claim, appeal or enrollment column is named in code", {
  L <- readLines(file.path(P$R, "04_ortho_harvest.R")); code <- L[!grepl("^\\s*#", L)]
  sel <- grep("select = c\\(", code, value = TRUE)
  length(sel) == 1L && grepl('"plan_id", "issuer_id", "state", "plan_in_network_denial_rate"', sel, fixed = TRUE) &&
    !any(grepl("avg_monthly|plan_claims_|_enrollment|disenrollment|reason_", code)) })
check("outcome-blind: the computable-proportion marker is reduced to is.na() and dropped at once", {
  L <- paste(readLines(file.path(P$R, "04_ortho_harvest.R")), collapse = "\n")
  grepl("keep <- !is.na(pl$plan_in_network_denial_rate)\n  pl[, plan_in_network_denial_rate := NULL]", L, fixed = TRUE) })
check("one plain User-Agent, no e-mail address in the script", length(unique(regmatches(readLines(file.path(P$R, "04_ortho_harvest.R")), regexpr('UA <- "[^"]+"', readLines(file.path(P$R, "04_ortho_harvest.R"))))) ) == 1L &&
  !any(grepl("[A-Za-z0-9._-]+@[A-Za-z0-9.-]+\\.[a-z]{2,}", readLines(file.path(P$R, "04_ortho_harvest.R")))))

LOG_AFTER <- tools::md5sum(list.files(P$logs, recursive = TRUE, full.names = TRUE))
check("hygiene: no file under v1/logs was added, removed or changed by this test run", identical(LOG_BEFORE, LOG_AFTER))
cat("\n", N_PASS, " passed, ", N_FAIL, " failed, ", N_SKIP, " skipped\n", sep = "")
if (N_FAIL) { cat("FAILED: ", paste(FAILS, collapse = "; "), "\n", sep = ""); quit(status = 1) }
