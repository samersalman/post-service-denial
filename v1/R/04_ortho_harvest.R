# =====================================================================
# 04_ortho_harvest.R : WP4 orthopaedic network harvest (relaxed G1)
# =====================================================================
# Builds blind-to-outcome orthopaedic exposure counts per cohort plan from the
# issuers' machine-readable provider files. Never reads a denial, claim, appeal
# or enrollment column (plans.csv is read by name: plan_id, issuer_id, state and
# the plan_in_network_denial_rate column ONLY as an is.na() marker of "has a
# computable in-network proportion"; the values are dropped on the next line).
#
# Run one stage:  Rscript "<this file>" <stage> [args]
#   tools            write the jq / awk / perl / bash helpers to CACHE/harvest/bin
#   nppes_start      HEAD the NPPES monthly zips, log to v1/logs/wp4_nppes_start.csv
#   nppes_end        same, to wp4_nppes_end.csv
#   prep             cohort plan list + file table in CACHE/harvest/state
#   calibrate        wire-byte calibration (one small file per method)
#   run [n]          supervisor: harvest all files (n = only the first n files, pilot)
#   aggregate        NPI x plan de-duplication, NPI-level flags (CACHE only)
#   vocab            specialty vocabulary table + crosswalk coverage
#   freeze           SHA-256 of crosswalk and taxonomy
#   exposure         v1/data_clean/ortho/plan_exposure.csv
#   g3sample / g3run / g3report    NPI Registry API validation sample
#   ledger           v1/results/ledger_parts/ledger_ortho.csv
# Detached launch (example):
#   nohup Rscript "<this file>" run > CACHE/harvest/run.out 2>&1 &
#
# Rules kept here: every request is a full-file stream we intend to finish or is
# byte capped (HEAD, head -c); one connection per host; at most 8 streams; one
# plain User-Agent; 429/503 honoured with back-off; no evasion. Names and
# addresses are never written (jq keeps npi, type, specialty, plan_id only).
# Keep this file ASCII (build non-ASCII with intToUtf8()).
# =====================================================================

if (!exists("P", inherits = TRUE) || !is.list(get("P", inherits = TRUE))) {
  .args <- commandArgs(trailingOnly = FALSE)
  .f <- sub("^--file=", "", grep("^--file=", .args, value = TRUE)[1])
  .here <- dirname(normalizePath(gsub("~+~", " ", .f, fixed = TRUE)))
  source(file.path(.here, "00_setup.R"))
}
source(file.path(P$R, "ledger.R"))
suppressPackageStartupMessages(library(data.table))
options(datatable.integer64 = "double")
ensure_pkgs(c("data.table", "digest", "yaml", "jsonlite"))

UA <- "Mozilla/5.0 (compatible; public-data-feasibility-check)"
GBYTES <- 1e9
CFG <- load_config()
SEED <- CFG$study$seed
CAP_WIRE <- CFG$gates$g1$transfer_gb * GBYTES
CAP_DEC <- 120 * GBYTES
DISK_PAUSE_GB <- 7
DISK_WAIT_MIN <- 30
INTERMEDIATE_CAP <- 3 * GBYTES
MAX_STREAMS <- 8L
MAX_ATTEMPTS <- 3L
CH <- file.path(P$cache, "harvest")
CN <- file.path(P$cache, "nppes")
CA <- file.path(P$cache, "api")
NPPES_BASE <- "https://download.cms.gov/nppes/"
OUT_DIR <- P$clean_ortho

ts_utc <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
if (!exists("WP4_LOG_DIR", inherits = TRUE)) WP4_LOG_DIR <- P$logs
lg <- function(..., step = "run", level = "INFO") {
  log_msg(..., level = level, file = file.path(WP4_LOG_DIR, paste0("wp4_", step, ".log")))
}
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L || (length(a) == 1L && is.na(a))) b else a

free_gb <- function(path = P$cache) {
  out <- suppressWarnings(system2("df", c("-k", shQuote(path)), stdout = TRUE, stderr = TRUE))
  f <- strsplit(trimws(out[length(out)]), "[[:space:]]+")[[1]]
  as.numeric(f[4]) * 1024 / GBYTES
}

dir_bytes <- function(path) {
  if (!dir.exists(path)) return(0)
  out <- suppressWarnings(system2("du", c("-sk", shQuote(path)), stdout = TRUE, stderr = TRUE))
  as.numeric(strsplit(out[length(out)], "[[:space:]]+")[[1]][1]) * 1024
}

# ---- pure guard functions (tested with toy values) -------------------
disk_decision <- function(free_gb_now, floor_gb = DISK_PAUSE_GB) free_gb_now >= floor_gb

# Decide whether a stream may start. committed = wire bytes of finished attempts,
# inflight = estimated wire bytes of running streams, next_est = estimate for the
# candidate. Returns "launch", "defer" (would pass the cap: stop launching) or
# "hard_stop" (committed + inflight already past the cap).
cap_decision <- function(committed, inflight, next_est, cap = CAP_WIRE) {
  if (committed + inflight >= cap) return("hard_stop")
  if (committed + inflight + next_est > cap) return("defer")
  "launch"
}

# Wilson score interval written from the formula (no Hmisc::binconf: it returns a
# non-Wilson limit at x == 1 and x == n - 1).
wilson_ci <- function(x, n, conf = 0.95) {
  if (any(n <= 0)) return(list(est = NA_real_, lo = NA_real_, hi = NA_real_))
  z <- stats::qnorm(1 - (1 - conf) / 2)
  p <- x / n
  den <- 1 + z^2 / n
  ctr <- (p + z^2 / (2 * n)) / den
  hw <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / den
  list(est = p, lo = max(0, ctr - hw), hi = min(1, ctr + hw))
}

# ---------------------------------------------------------------------
# Streaming tools (text of the helper files; written by write_tools())
# ---------------------------------------------------------------------
TOOL_JQ <- r"---(
def trim: gsub("^\\s+|\\s+$"; "");
$coh[0] as $c
| fromstream(1 | truncate_stream(inputs | select(
    ((length == 1) and ((.[0] | length) == 2))
    or ((length == 2) and (.[0][1] as $k
        | ($k == "npi" or $k == "type" or $k == "specialty" or $k == "speciality"
           or ($k == "plans" and (((.[0] | length) == 2)
                                  or (((.[0] | length) == 4) and (.[0][3] == "plan_id"))))))))
  )) as $r
| if ($r | type) != "object" then ["O", "", "", ""] | @tsv
  else
    (($r.type // "") | tostring | ascii_upcase) as $t
    | (($r.npi // "") | tostring) as $npi
    | ([ ($r.plans // [])[]? | (.plan_id? // empty) | tostring
         | (if length > 14 then .[0:14] else . end) ]
       | unique | map(select($c[.] == true))) as $cp
    | (($r.specialty // $r.speciality // null)
       | if . == null then [] elif type == "array" then . else [.] end
       | map(select(. != null) | tostring | trim) | map(select(length > 0))
       | unique | join("|")) as $sp0
    | (if $sp0 == "" then "<none>" else $sp0 end) as $sp
    | if $t == "INDIVIDUAL" then
        (if ($npi | test("^[0-9]{10}$")) then "I" else "B" end) as $code
        | [$code, $npi, $sp, ($cp | join(","))] | @tsv
      elif $t == "FACILITY" then ["F", "", "", ($cp | join(","))] | @tsv
      elif $t == "GROUP" then ["G", "", "", ($cp | join(","))] | @tsv
      else ["O", "", "", ($cp | join(","))] | @tsv end
  end
)---"

TOOL_AWK <- r"---(
BEGIN {
  FS = "\t"
  recf = "gzip -1 -c > \"" out "/rec.tsv.gz\""
  ns = 0; nset = 0
}
{
  c = $1
  n[c]++
  pl = $4
  pc[c SUBSEP pl]++
  if (c == "I") {
    sp = $3
    if (!(sp in specid)) { specid[sp] = ++ns; specstr[ns] = sp }
    if (pl == "") { sid = 0 }
    else {
      if (!(pl in setid)) { setid[pl] = ++nset; setstr[nset] = pl }
      sid = setid[pl]
    }
    print $2 "\t" specid[sp] "\t" sid | recf
  }
}
END {
  close(recf)
  sf = out "/specs.tsv"; tf = out "/sets.tsv"; nf = out "/seen.tsv"; cf = out "/counts.tsv"
  printf "" > sf
  for (i = 1; i <= ns; i++) print i "\t" specstr[i] > sf
  close(sf)
  printf "" > tf
  for (i = 1; i <= nset; i++) print i "\t" setstr[i] > tf
  close(tf)
  cpr = 0
  for (k in pc) {
    split(k, kk, SUBSEP)
    cc = kk[1]; pp = kk[2]
    if (pp == "") continue
    m = split(pp, a, ",")
    cpr += pc[k] * m
    for (j = 1; j <= m; j++) { sc[a[j] SUBSEP cc] += pc[k]; seenp[a[j]] = 1 }
  }
  nseen = 0
  printf "" > nf
  for (p in seenp) {
    nseen++
    print p "\t" (sc[p SUBSEP "I"] + 0) "\t" (sc[p SUBSEP "F"] + 0) "\t" (sc[p SUBSEP "G"] + 0) "\t" (sc[p SUBSEP "O"] + 0) "\t" (sc[p SUBSEP "B"] + 0) > nf
  }
  close(nf)
  print (n["I"] + 0) "\t" (n["B"] + 0) "\t" (n["F"] + 0) "\t" (n["G"] + 0) "\t" (n["O"] + 0) "\t" cpr "\t" ns "\t" nset "\t" nseen "\tdone" > cf
  close(cf)
}
)---"

TOOL_PL <- r"---(
use strict;
use warnings;
my ($pf, $stop, $cap, $resf) = @ARGV;
binmode(STDIN);
binmode(STDOUT);
$SIG{PIPE} = 'IGNORE';
my ($n, $last, $buf, $first, $rc) = (0, 0, '', '', 0);
sub wr {
  my ($f, $txt) = @_;
  open(my $h, '>', "$f.t") or return;
  print $h $txt;
  close($h);
  rename("$f.t", $f);
}
while (my $r = sysread(STDIN, $buf, 1048576)) {
  $n += $r;
  if ($first eq '') {
    my $t = $buf;
    $t =~ s/^[\s\xEF\xBB\xBF]+//;
    $first = substr($t, 0, 1) if length($t) > 0;
  }
  my $off = 0;
  while ($off < $r) {
    my $w = syswrite(STDOUT, $buf, $r - $off, $off);
    if (!defined($w)) { $rc = 5; last; }
    $off += $w;
  }
  last if $rc;
  my $t = time;
  if ($t != $last) {
    $last = $t;
    wr($pf, $n);
    if (-e $stop) { $rc = 3; last; }
    if ($cap > 0 && $n > $cap) { $rc = 4; last; }
  }
}
wr($pf, $n);
wr($resf, "$n\t" . ($first eq '' ? '-' : $first) . "\n");
exit $rc;
)---"

TOOL_SH <- r"---(#!/bin/bash
# stream_one.sh FID ATTEMPT URL CH UA MAXTIME STOPFLAG BYTECAP
FID="$1"; ATT="$2"; URL="$3"; CH="$4"; UA="$5"; MAXT="$6"; STOP="$7"; CAP="$8"
TMP="$CH/tmp/$FID.a$ATT.$$"
RES="$CH/res/$FID.a$ATT.res"
mkdir -p "$TMP" "$CH/res" "$CH/progress" "$CH/out" "$CH/pids"
echo $$ > "$CH/pids/$FID.pid"
START=$(date -u +%Y-%m-%dT%H:%M:%SZ)
T0=$(date +%s)
set -o pipefail
curl -sS -L --compressed --fail --connect-timeout 20 --max-time "$MAXT" --speed-limit 5000 --speed-time 180 \
  -A "$UA" -D "$TMP/hdr.txt" \
  -w "%output{$TMP/w.txt}%{http_code}\t%{size_download}\t%{time_total}\t%{num_redirects}\t%{url_effective}\n" \
  "$URL" 2> "$TMP/curl.err" \
 | perl "$CH/bin/count.pl" "$CH/progress/$FID.bytes" "$STOP" "$CAP" "$TMP/dec.txt" \
 | jq -r --stream -n --slurpfile coh "$CH/cohort_plans.json" -f "$CH/bin/filter.jq" 2> "$TMP/jq.err" \
 | awk -v out="$TMP" -f "$CH/bin/collect.awk" 2> "$TMP/awk.err"
ST=("${PIPESTATUS[@]}")
T1=$(date +%s)
ENDT=$(date -u +%Y-%m-%dT%H:%M:%SZ)
CURLX="${ST[0]}"; PLX="${ST[1]}"; JQX="${ST[2]}"; AWKX="${ST[3]}"
HTTP="NA"; WIRE=0; TTOT=0; NREDIR=0; FURL="-"
if [ -s "$TMP/w.txt" ]; then IFS=$'\t' read -r HTTP WIRE TTOT NREDIR FURL < "$TMP/w.txt"; fi
DEC=0; FIRST="-"
if [ -s "$TMP/dec.txt" ]; then IFS=$'\t' read -r DEC FIRST < "$TMP/dec.txt"; fi
CE=$(grep -i '^content-encoding:' "$TMP/hdr.txt" 2>/dev/null | tail -1 | tr -d '\r' | sed 's/^[^:]*: *//')
RA=$(grep -i '^retry-after:' "$TMP/hdr.txt" 2>/dev/null | tail -1 | tr -d '\r' | sed 's/^[^:]*: *//')
[ -z "$CE" ] && CE="-"
[ -z "$RA" ] && RA="-"
nI=0; nB=0; nF=0; nG=0; nO=0; CPR=0; NSP=0; NSET=0; NSEEN=0; FIN="-"
if [ -s "$TMP/counts.tsv" ]; then IFS=$'\t' read -r nI nB nF nG nO CPR NSP NSET NSEEN FIN < "$TMP/counts.tsv"; fi
STATUS=failed
if [ "$PLX" = 3 ] || [ "$PLX" = 4 ]; then
  STATUS=stopped
elif [ "$CURLX" = 0 ] && [ "$PLX" = 0 ] && [ "$JQX" = 0 ] && [ "$AWKX" = 0 ] && [ "$FIN" = done ] && [ "$FIRST" = "[" ]; then
  STATUS=ok
fi
OUTB=0; SHA="-"; ERR="-"
if [ "$STATUS" = ok ]; then
  [ -f "$TMP/rec.tsv.gz" ] || : | gzip -c > "$TMP/rec.tsv.gz"
  rm -f "$TMP/curl.err" "$TMP/jq.err" "$TMP/awk.err" "$TMP/w.txt" "$TMP/dec.txt"
  OUTB=$(cat "$TMP"/rec.tsv.gz "$TMP"/specs.tsv "$TMP"/sets.tsv "$TMP"/seen.tsv | wc -c | tr -d ' ')
  SHA=$(shasum -a 256 "$TMP/rec.tsv.gz" | cut -d' ' -f1)
  rm -rf "$CH/out/$FID"
  mv "$TMP" "$CH/out/$FID"
else
  ERR=$(cat "$TMP/curl.err" "$TMP/jq.err" "$TMP/awk.err" 2>/dev/null | head -c 300 | tr '\t\r\n' '   ' | tr -s ' ')
  [ -z "$ERR" ] && ERR="-"
  rm -rf "$TMP"
fi
SECS=$((T1 - T0))
{
  printf 'fid\tattempt\tstatus\thttp_status\tcurl_exit\tpl_exit\tjq_exit\tawk_exit\twire_bytes\tdecoded_bytes\tseconds\tstarted\tended\tfinal_url\tcontent_encoding\tretry_after\tn_individual\tn_bad_npi\tn_facility\tn_group\tn_other\tcohort_plan_rows\tn_cohort_plans_seen\tout_bytes\tout_sha256\terr\n'
  printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
    "$FID" "$ATT" "$STATUS" "$HTTP" "$CURLX" "$PLX" "$JQX" "$AWKX" "$WIRE" "$DEC" "$SECS" "$START" "$ENDT" "$FURL" "$CE" "$RA" \
    "$nI" "$nB" "$nF" "$nG" "$nO" "$CPR" "$NSEEN" "$OUTB" "$SHA" "$ERR"
} > "$RES.tmp"
mv "$RES.tmp" "$RES"
rm -f "$CH/pids/$FID.pid" "$CH/progress/$FID.bytes"
exit 0
)---"

write_tools <- function(ch = CH) {
  bin <- file.path(ch, "bin")
  for (d in c(bin, file.path(ch, c("tmp", "res", "progress", "out", "pids", "state")))) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  writeLines(TOOL_JQ, file.path(bin, "filter.jq"))
  writeLines(TOOL_AWK, file.path(bin, "collect.awk"))
  writeLines(TOOL_PL, file.path(bin, "count.pl"))
  writeLines(TOOL_SH, file.path(bin, "stream_one.sh"))
  Sys.chmod(file.path(bin, "stream_one.sh"), "0755")
  invisible(bin)
}

# cohort plan list as a JSON object {plan_id: true} for jq --slurpfile
write_cohort_json <- function(plan_ids, ch = CH) {
  dir.create(ch, recursive = TRUE, showWarnings = FALSE)
  f <- file.path(ch, "cohort_plans.json")
  writeLines(paste0("{", paste0('"', sort(unique(plan_ids)), '":true', collapse = ","), "}"), f)
  invisible(f)
}

# Run the per-file pipeline synchronously (tests and calibration). url may be file://.
run_stream_sync <- function(fid, url, ch = CH, attempt = 1L, maxtime = 600, cap_bytes = 0, stop_flag = NULL) {
  stop_flag <- stop_flag %||% file.path(ch, "HARDSTOP")
  cmd <- paste("bash", shQuote(file.path(ch, "bin", "stream_one.sh")), fid, attempt, shQuote(url),
               shQuote(ch), shQuote(UA), maxtime, shQuote(stop_flag), format(cap_bytes, scientific = FALSE))
  system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
  rf <- file.path(ch, "res", sprintf("%s.a%d.res", fid, attempt))
  if (!file.exists(rf)) return(NULL)
  fread(rf, sep = "\t", colClasses = "character", quote = "")
}

# ---------------------------------------------------------------------
# NPPES bulk-file retry (start and end of the harvest)
# ---------------------------------------------------------------------
curl_head <- function(url, maxtime = 30) {
  out <- suppressWarnings(system2("curl", c("-sS", "-I", "-L", "--max-time", maxtime, "-A", shQuote(UA), shQuote(url)),
                                  stdout = TRUE, stderr = TRUE))
  out <- sub("\r$", "", out)
  st <- grep("^HTTP/", out, value = TRUE)
  code <- if (length(st)) as.integer(strsplit(st[length(st)], " ")[[1]][2]) else NA_integer_
  pick <- function(h) { x <- grep(paste0("^", h, ":"), out, ignore.case = TRUE, value = TRUE); if (length(x)) trimws(sub("^[^:]*:", "", x[length(x)])) else "" }
  list(code = code, len = pick("content-length"), lm = pick("last-modified"), ct = pick("content-type"))
}

nppes_check <- function(tag = "start") {
  rows <- list()
  add <- function(test, url, code, len, lm, ct, note = "") {
    rows[[length(rows) + 1L]] <<- data.table(test = test, url = url, http = code, content_length = len,
                                             last_modified = lm, content_type = ct, note = note, tested_utc = ts_utc())
  }
  lst <- paste0(NPPES_BASE, "NPI_Files.html")
  body <- suppressWarnings(system2("bash", c("-c", shQuote(paste("curl -sS -L --max-time 30 -A", shQuote(UA), shQuote(lst), "| head -c 300000"))),
                                   stdout = TRUE, stderr = TRUE))
  txt <- paste(body, collapse = "\n")
  zips <- unique(regmatches(txt, gregexpr("href=\"[^\"]*\\.zip\"", txt))[[1]])
  zips <- unique(sub("^.*/", "", sub("\"$", "", sub("^href=\"", "", zips))))
  plain <- gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", sub("(?s)<style.*?</style>", "", txt, perl = TRUE)))
  down <- grepl("down for main", plain, ignore.case = TRUE)
  add("listing page", lst, 200L, as.character(nchar(txt, "bytes")), "", "text/html",
      paste0("zip links listed: ", length(zips), "; maintenance banner: ", down))
  cand <- unique(c("NPPES_Data_Dissemination_October_2026_V2.zip", "NPPES_Data_Dissemination_September_2026_V2.zip", zips))
  reach <- FALSE
  for (z in cand) {
    h <- curl_head(paste0(NPPES_BASE, z))
    add(paste("HEAD", z), paste0(NPPES_BASE, z), h$code, h$len, h$lm, h$ct, "")
    if (identical(h$code, 200L) && suppressWarnings(as.numeric(h$len)) > 1e8 && grepl("Dissemination", z) && !grepl("Weekly", z)) reach <- TRUE
    Sys.sleep(1)
  }
  tab <- rbindlist(rows)
  tab[, monthly_zip_reachable := reach]
  dir.create(P$logs, showWarnings = FALSE, recursive = TRUE)
  fwrite(tab, file.path(P$logs, paste0("wp4_nppes_", tag, ".csv")))
  lg("NPPES ", tag, ": monthly zip reachable = ", reach, "; maintenance banner = ", down, step = "nppes")
  invisible(reach)
}

# ---------------------------------------------------------------------
# Cohort and file table (blind to the outcome)
# ---------------------------------------------------------------------
load_cohort <- function(plans_csv = file.path(P$clean_cms, "plans.csv")) {
  pl <- fread(plans_csv, select = c("plan_id", "issuer_id", "state", "plan_in_network_denial_rate"),
              colClasses = list(character = c("plan_id", "issuer_id", "state")))
  keep <- !is.na(pl$plan_in_network_denial_rate)
  pl[, plan_in_network_denial_rate := NULL]
  pl <- pl[keep]
  stopifnot(all(nchar(pl$plan_id) == 14L), !anyDuplicated(pl$plan_id))
  pl[]
}

build_file_table <- function(inv_csv = file.path(OUT_DIR, "provider_file_inventory.csv"),
                             iss_csv = file.path(OUT_DIR, "issuer_sources.csv")) {
  inv <- fread(inv_csv, colClasses = "character")
  iss <- fread(iss_csv, colClasses = c(issuer_id = "character"))
  ncp <- setNames(iss$n_computable_plans, iss$issuer_id)
  inv[, bytes_n := suppressWarnings(as.numeric(bytes))]
  tab <- inv[, .(index_url = index_url[1], index_urls = paste(unique(index_url), collapse = ";"), host = host[1],
                 listed_bytes = if (all(is.na(bytes_n))) NA_real_ else max(bytes_n, na.rm = TRUE),
                 content_class = content_class_from_prefix[1],
                 issuer_ids = paste(sort(unique(unlist(strsplit(issuer_ids, ";", fixed = TRUE)))), collapse = ";"),
                 gzip_behaviour = gzip_behaviour[1]), by = provider_url]
  tab[, n_cohort_plans_in_issuers := vapply(strsplit(issuer_ids, ";", fixed = TRUE), function(x) sum(ncp[x], na.rm = TRUE), numeric(1))]
  tab[, skip := n_cohort_plans_in_issuers == 0]
  setorder(tab, provider_url)
  tab[, fid := sprintf("f%03d", seq_len(.N))]
  tab[, class_rank := fifelse(grepl("^facility", content_class), 2L, 1L)]
  tab[, has_individual_prefix := class_rank == 1L]
  tab[, max_time := as.integer(pmax(7200, ceiling(fifelse(is.na(listed_bytes), 3e9, listed_bytes) / 0.25e6) + 900))]
  setorder(tab, class_rank, listed_bytes, na.last = TRUE)
  tab[, run_order := seq_len(.N)]
  setcolorder(tab, c("fid", "run_order", "provider_url"))
  tab[]
}

# ---------------------------------------------------------------------
# Manifest and attempts (resumable state)
# ---------------------------------------------------------------------
MAN_COLS <- c("url", "index_url", "status", "http_status", "curl_exit", "wire_bytes", "decoded_bytes", "seconds",
              "records", "individual_records", "cohort_plan_rows", "started", "ended", "retries",
              "fid", "host", "listed_bytes", "content_class", "wire_bytes_all_attempts", "decoded_bytes_all_attempts", "individual_bad_npi",
              "facility_records", "group_records", "other_records", "n_cohort_plans_seen", "out_bytes",
              "out_sha256", "final_url", "content_encoding", "error", "issuer_ids", "skip_reason")

manifest_init <- function(files) {
  man <- data.table(fid = files$fid, url = files$provider_url, index_url = files$index_url, host = files$host,
                    listed_bytes = files$listed_bytes, content_class = files$content_class,
                    issuer_ids = files$issuer_ids,
                    status = fifelse(files$skip, "skipped", "pending"),
                    http_status = NA_integer_, curl_exit = NA_integer_, wire_bytes = NA_real_, decoded_bytes = NA_real_,
                    seconds = NA_real_, records = NA_real_, individual_records = NA_real_, cohort_plan_rows = NA_real_,
                    started = NA_character_, ended = NA_character_, retries = 0L, wire_bytes_all_attempts = 0, decoded_bytes_all_attempts = 0,
                    individual_bad_npi = NA_real_, facility_records = NA_real_, group_records = NA_real_,
                    other_records = NA_real_, n_cohort_plans_seen = NA_real_, out_bytes = NA_real_,
                    out_sha256 = NA_character_, final_url = NA_character_, content_encoding = NA_character_,
                    error = NA_character_, skip_reason = NA_character_)
  man[status == "skipped", skip_reason := "no computable plan among the issuers sharing the index"]
  man[]
}

write_atomic_csv <- function(dt, path) {
  tmp <- paste0(path, ".tmp")
  fwrite(dt, tmp)
  file.rename(tmp, path)
  invisible(path)
}

manifest_load <- function(ctx, files) {
  f <- file.path(ctx$ch, "state", "manifest.csv")
  fresh <- manifest_init(files)
  if (!file.exists(f)) return(fresh)
  old <- fread(f, colClasses = list(character = c("fid", "url", "index_url", "host", "status", "started", "ended",
                                                  "out_sha256", "final_url", "content_encoding", "error", "issuer_ids",
                                                  "skip_reason", "content_class"),
                            numeric = c("listed_bytes", "wire_bytes", "decoded_bytes", "seconds", "records", "individual_records",
                                        "cohort_plan_rows", "wire_bytes_all_attempts", "decoded_bytes_all_attempts", "individual_bad_npi", "facility_records",
                                        "group_records", "other_records", "n_cohort_plans_seen", "out_bytes")))
  old <- old[match(fresh$fid, fid)]
  stopifnot(identical(old$url, fresh$url))
  for (cc in setdiff(names(fresh), names(old))) set(old, j = cc, value = fresh[[cc]])
  old[status == "running", status := "pending"]
  for (i in which(old$status == "ok")) {
    d <- file.path(ctx$ch, "out", old$fid[i])
    ok <- file.exists(file.path(d, "rec.tsv.gz")) && !is.na(old$out_bytes[i]) &&
      sum(file.size(file.path(d, c("rec.tsv.gz", "specs.tsv", "sets.tsv", "seen.tsv")))) == old$out_bytes[i]
    if (!isTRUE(ok)) { old[i, `:=`(status = "pending", error = "resume check failed: output missing or size changed")] }
  }
  old[]
}

# Byte columns come from the attempts table, which is the single source of truth: fill NA bytes of ok rows and
# recompute the all-attempts total for every row (the F4 fix for a 3.7 GB file that overflowed an integer column).
manifest_repair_bytes <- function(man, att) {
  a <- att[status == "ok"]
  for (i in which(man$status == "ok")) {
    r <- a[fid == man$fid[i]]
    if (nrow(r)) {
      r <- r[nrow(r)]
      if (is.na(man$wire_bytes[i])) set(man, i, "wire_bytes", r$wire_bytes)
      if (is.na(man$decoded_bytes[i])) set(man, i, "decoded_bytes", r$decoded_bytes)
    }
  }
  set(man, j = "wire_bytes_all_attempts", value = vapply(man$fid, function(f) sum(att$wire_bytes[att$fid == f]), numeric(1)))
  set(man, j = "decoded_bytes_all_attempts", value = vapply(man$fid, function(f) sum(att$decoded_bytes[att$fid == f]), numeric(1)))
  man
}

attempts_load <- function(ctx) {
  f <- file.path(ctx$ch, "state", "attempts.csv")
  if (!file.exists(f)) {
    return(data.table(fid = character(), attempt = integer(), status = character(), http_status = character(),
                      curl_exit = integer(), wire_bytes = numeric(), decoded_bytes = numeric(), seconds = numeric(),
                      started = character(), ended = character(), host = character(), err = character()))
  }
  fread(f, colClasses = list(character = c("fid", "status", "http_status", "started", "ended", "host", "err")))
}

export_manifest <- function(man, path = file.path(OUT_DIR, "harvest_manifest.csv")) {
  out <- copy(man)
  out[, records := rowSums(cbind(individual_records, individual_bad_npi, facility_records, group_records, other_records), na.rm = FALSE)]
  out[status == "pending", skip_reason := "not run in this pass (guard, time-box or interruption); a rerun of the run stage resumes it"]
  out[status == "pending", status := "not_run"]
  setcolorder(out, intersect(MAN_COLS, names(out)))
  setorder(out, fid)
  write_atomic_csv(out, path)
}

pid_alive <- function(pid) {
  if (is.na(pid)) return(FALSE)
  suppressWarnings(system2("kill", c("-0", pid), stdout = FALSE, stderr = FALSE)) == 0L
}

new_ctx <- function(ch = CH, cap = CAP_WIRE, cap_dec = CAP_DEC, floor_gb = DISK_PAUSE_GB, disk_wait_min = DISK_WAIT_MIN,
                    inter_cap = INTERMEDIATE_CAP, max_streams = MAX_STREAMS, poll = 3, file_dec_cap = 20e9,
                    export_path = file.path(OUT_DIR, "harvest_manifest.csv"), log_step = "harvest", max_attempts = MAX_ATTEMPTS) {
  list(ch = ch, cap = cap, cap_dec = cap_dec, floor_gb = floor_gb, disk_wait_min = disk_wait_min, inter_cap = inter_cap,
       max_streams = max_streams, poll = poll, file_dec_cap = file_dec_cap, export_path = export_path, log_step = log_step,
       max_attempts = max_attempts)
}

# ---------------------------------------------------------------------
# Supervisor: up to max_streams streams, one per host, wire and disk guards
# ---------------------------------------------------------------------
harvest_supervisor <- function(ctx, files, max_launch = Inf, retry_only = FALSE) {
  ch <- ctx$ch
  write_tools(ch)
  L <- function(...) lg(..., step = ctx$log_step)
  stop_flag <- file.path(ch, "HARDSTOP")
  soft_flag <- file.path(ch, "STOP")
  pause_flag <- file.path(ch, "PAUSE")
  sup_pid <- file.path(ch, "supervisor.pid")
  if (file.exists(sup_pid) && pid_alive(as.integer(readLines(sup_pid, warn = FALSE)[1]))) {
    stop("another supervisor is running (", sup_pid, ")", call. = FALSE)
  }
  for (pf in list.files(file.path(ch, "pids"), full.names = TRUE)) {
    if (pid_alive(as.integer(readLines(pf, warn = FALSE)[1]))) stop("a stream process from an earlier run is still alive: ", pf, call. = FALSE)
    unlink(pf)
  }
  unlink(list.files(file.path(ch, "tmp"), full.names = TRUE), recursive = TRUE)
  unlink(stop_flag)
  writeLines(as.character(Sys.getpid()), sup_pid)
  on.exit(unlink(sup_pid), add = TRUE)
  files <- copy(files)[order(run_order)]
  man <- manifest_load(ctx, files)
  setkey(man, NULL)
  man <- man[match(files$fid, fid)]
  att <- attempts_load(ctx)
  man <- manifest_repair_bytes(man, att)
  if (retry_only) {
    # re-run only the files that failed earlier (and have attempts left); everything else stays as it is
    nf <- vapply(man$fid, function(f) sum(att$fid == f & att$status == "failed"), integer(1))
    man[status %in% c("pending"), status := "deferred"]
    man[status == "failed" & nf < ctx$max_attempts, status := "pending"]
  }
  fmax <- setNames(files$max_time, files$fid)
  furl <- setNames(files$provider_url, files$fid)
  known <- files$listed_bytes[!is.na(files$listed_bytes)]
  med_known <- if (length(known)) median(known) else 1e8
  n_failed_counted <- function(fid) sum(att$fid == fid & att$status == "failed")
  committed_wire <- function() sum(att$wire_bytes)
  committed_dec <- function() sum(att$decoded_bytes)
  host_ratio <- function(h) {
    a <- att[host == h & status == "ok" & decoded_bytes > 0]
    if (!nrow(a)) return(1)
    min(1, sum(a$wire_bytes) / sum(a$decoded_bytes))
  }
  est_wire <- function(i) {
    b <- man$listed_bytes[i]; if (is.na(b)) b <- med_known
    b * host_ratio(man$host[i])
  }
  inflight <- function() {
    w <- 0; d <- 0; r <- 0
    for (fid in names(active)) {
      pf <- file.path(ch, "progress", paste0(fid, ".bytes"))
      v <- if (file.exists(pf)) suppressWarnings(as.numeric(readLines(pf, warn = FALSE)[1])) else 0
      if (is.na(v)) v <- 0
      vw <- v * host_ratio(active[[fid]]$host)
      d <- d + v; w <- w + vw; r <- r + max(vw, active[[fid]]$est)
    }
    c(wire = w, dec = d, reserved = r)
  }
  active <- list(); cool <- list()
  n_launched <- 0L; n_done_since_export <- 0L; last_fid <- ""; last_hb <- 0; last_disk <- 0; last_du <- 0
  fg <- free_gb(ch); inter <- dir_bytes(file.path(ch, "out")); low_since <- NA_real_
  final <- "COMPLETE"; drained <- FALSE; hard <- FALSE; retry_pass <- 0L
  t_start <- as.numeric(Sys.time())
  L("supervisor start: ", sum(man$status == "pending"), " pending, ", sum(man$status == "ok"), " ok, ",
    sum(man$status == "skipped"), " skipped; cap ", round(ctx$cap / GBYTES, 1), " GB wire; committed ",
    round(committed_wire() / GBYTES, 3), " GB; free ", round(fg, 2), " GB")
  heartbeat <- function(state = "RUNNING") {
    inf <- inflight()
    writeLines(c(paste0("time_utc=", ts_utc()), paste0("state=", state), paste0("final=", final),
                 paste0("ok=", sum(man$status == "ok")), paste0("failed=", sum(man$status == "failed")),
                 paste0("pending=", sum(man$status == "pending")), paste0("skipped=", sum(man$status == "skipped")),
                 paste0("active=", paste(names(active), collapse = ",")),
                 paste0("last_file_finished=", last_fid),
                 paste0("wire_gb_committed=", round(committed_wire() / GBYTES, 3)),
                 paste0("wire_gb_inflight_est=", round(inf[["wire"]] / GBYTES, 3)),
                 paste0("decoded_gb_committed=", round(committed_dec() / GBYTES, 3)),
                 paste0("free_gb=", round(fg, 2)), paste0("intermediate_mb=", round(inter / 1e6, 1)),
                 paste0("retry_pass=", retry_pass), paste0("elapsed_min=", round((as.numeric(Sys.time()) - t_start) / 60, 1))),
               file.path(ch, "heartbeat.txt"))
  }
  repeat {
    now <- as.numeric(Sys.time())
    # ---- reap finished streams
    for (fid in names(active)) {
      a <- active[[fid]]
      rf <- file.path(ch, "res", sprintf("%s.a%d.res", fid, a$attempt))
      res <- NULL
      if (file.exists(rf)) {
        res <- fread(rf, sep = "\t", colClasses = "character", quote = "", na.strings = NULL)
      } else if (!pid_alive(a$pid) && now - a$t0 > 20) {
        res <- data.table(fid = fid, attempt = as.character(a$attempt), status = "failed", http_status = "NA",
                          curl_exit = "NA", pl_exit = "NA", jq_exit = "NA", awk_exit = "NA", wire_bytes = "0",
                          decoded_bytes = "0", seconds = as.character(round(now - a$t0)), started = a$started,
                          ended = ts_utc(), final_url = "-", content_encoding = "-", retry_after = "-",
                          n_individual = "0", n_bad_npi = "0", n_facility = "0", n_group = "0", n_other = "0",
                          cohort_plan_rows = "0", n_cohort_plans_seen = "0", out_bytes = "0", out_sha256 = "-",
                          err = "stream process vanished without a result file")
      }
      if (is.null(res)) next
      active[[fid]] <- NULL
      i <- which(man$fid == fid)
      num <- function(x) suppressWarnings(as.numeric(x))
      att <- rbind(att, data.table(fid = fid, attempt = as.integer(res$attempt), status = res$status,
                                   http_status = res$http_status, curl_exit = as.integer(num(res$curl_exit)),
                                   wire_bytes = num(res$wire_bytes), decoded_bytes = num(res$decoded_bytes),
                                   seconds = num(res$seconds), started = res$started, ended = res$ended,
                                   host = a$host, err = res$err))
      write_atomic_csv(att, file.path(ch, "state", "attempts.csv"))
      nfail <- n_failed_counted(fid)
      http <- suppressWarnings(as.integer(res$http_status))
      st <- res$status
      set(man, i, "wire_bytes_all_attempts", sum(att$wire_bytes[att$fid == fid]))
      set(man, i, "decoded_bytes_all_attempts", sum(att$decoded_bytes[att$fid == fid]))
      set(man, i, "retries", as.integer(max(0L, sum(att$fid == fid & att$status != "stopped") - 1L)))
      if (st == "ok") {
        set(man, i, c("status", "http_status", "curl_exit", "wire_bytes", "decoded_bytes", "seconds", "started", "ended",
                      "individual_records", "individual_bad_npi", "facility_records", "group_records", "other_records",
                      "cohort_plan_rows", "n_cohort_plans_seen", "out_bytes", "out_sha256", "final_url",
                      "content_encoding", "error"),
            list("ok", http, as.integer(num(res$curl_exit)), num(res$wire_bytes), num(res$decoded_bytes), num(res$seconds),
                 res$started, res$ended, num(res$n_individual), num(res$n_bad_npi), num(res$n_facility),
                 num(res$n_group), num(res$n_other), num(res$cohort_plan_rows), num(res$n_cohort_plans_seen),
                 num(res$out_bytes), res$out_sha256, res$final_url, res$content_encoding, NA_character_))
      } else if (st == "stopped") {
        set(man, i, c("status", "error"), list("pending", "stopped by a guard; not counted as a failure"))
      } else {
        set(man, i, c("status", "http_status", "curl_exit", "wire_bytes", "decoded_bytes", "seconds", "started", "ended",
                      "final_url", "content_encoding", "error"),
            list("failed", http, as.integer(num(res$curl_exit)), num(res$wire_bytes), num(res$decoded_bytes),
                 num(res$seconds), res$started, res$ended, res$final_url, res$content_encoding, res$err))
        if (!is.na(http) && (http == 429L || (http >= 500L && http <= 599L))) {
          ra <- suppressWarnings(as.numeric(res$retry_after))
          wait <- max(if (is.na(ra)) 0 else ra, 120 * 2^(nfail - 1L))
          cool[[a$host]] <- now + wait
          L("HTTP ", http, " from ", a$host, ": host cooled for ", round(wait), " s (file ", fid, ")")
          if (nfail < ctx$max_attempts) set(man, i, "status", "pending")
        }
      }
      last_fid <- fid
      L(fid, " ", st, " http=", res$http_status, " curl=", res$curl_exit, " wire=", round(num(res$wire_bytes) / 1e6, 1),
        " MB dec=", round(num(res$decoded_bytes) / 1e6, 1), " MB in ", res$seconds, " s; ind=", res$n_individual,
        if (st != "ok") paste0(" err=", substr(res$err, 1, 120)) else "")
      write_atomic_csv(man, file.path(ch, "state", "manifest.csv"))
      n_done_since_export <- n_done_since_export + 1L
      if (n_done_since_export >= 20L) { export_manifest(man, ctx$export_path); n_done_since_export <- 0L }
    }
    # ---- guards
    if (now - last_disk > 15) {
      fg <- free_gb(ch); last_disk <- now
      if (!disk_decision(fg, ctx$floor_gb)) {
        if (is.na(low_since)) { low_since <- now; L("free disk ", round(fg, 2), " GB is below ", ctx$floor_gb, " GB: launches paused", level = "WARN") }
      } else if (!is.na(low_since)) { low_since <- NA_real_; L("free disk recovered to ", round(fg, 2), " GB: launches resume") }
    }
    if (!is.na(low_since) && now - low_since >= ctx$disk_wait_min * 60 && !drained) {
      drained <- TRUE; final <- "BLOCKED_DISK"; L("free disk stayed below the floor for ", ctx$disk_wait_min, " min: stopping cleanly", level = "ERROR")
    }
    if (now - last_du > 60) {
      inter <- dir_bytes(file.path(ch, "out")); last_du <- now
      if (inter > ctx$inter_cap && !drained) {
        drained <- TRUE; final <- "BLOCKED_INTERMEDIATES"; L("intermediates ", round(inter / 1e6), " MB exceed the cap: stopping cleanly", level = "ERROR")
      }
    }
    inf <- inflight()
    if (!hard && (committed_wire() + inf[["wire"]] >= ctx$cap || committed_dec() + inf[["dec"]] >= ctx$cap_dec)) {
      file.create(stop_flag); hard <- TRUE; drained <- TRUE
      final <- if (committed_wire() + inf[["wire"]] >= ctx$cap) "CAP_REACHED" else "DECODED_CAP_REACHED"
      L("transfer guard tripped: ", round((committed_wire() + inf[["wire"]]) / GBYTES, 2), " GB wire, ",
        round((committed_dec() + inf[["dec"]]) / GBYTES, 2), " GB decoded; stopping all streams", level = "ERROR")
    }
    if (file.exists(soft_flag) && !drained) { drained <- TRUE; final <- "STOPPED_BY_FLAG"; L("STOP flag found: draining") }
    # ---- launch
    can <- !drained && length(active) < ctx$max_streams && is.na(low_since) && !file.exists(pause_flag) && n_launched < max_launch
    if (can) {
      for (i in which(man$status == "pending")) {
        if (length(active) >= ctx$max_streams || n_launched >= max_launch) break
        h <- man$host[i]
        if (any(vapply(active, function(a) identical(a$host, h), logical(1)))) next
        if (!is.null(cool[[h]]) && cool[[h]] > now) next
        dec <- cap_decision(committed_wire(), inf[["reserved"]], est_wire(i), ctx$cap)
        if (dec != "launch") {
          drained <- TRUE; final <- "CAP_REACHED"
          L("next file ", man$fid[i], " (est ", round(est_wire(i) / 1e6), " MB wire) would pass the cap: no further launches")
          break
        }
        fid <- man$fid[i]
        attempt <- sum(att$fid == fid) + 1L
        cmd <- sprintf("nohup bash %s %s %d %s %s %s %d %s %s > /dev/null 2>&1 & echo $!",
                       shQuote(file.path(ch, "bin", "stream_one.sh")), fid, attempt, shQuote(furl[[fid]]), shQuote(ch),
                       shQuote(UA), fmax[[fid]], shQuote(stop_flag), format(ctx$file_dec_cap, scientific = FALSE))
        pid <- suppressWarnings(as.integer(system(cmd, intern = TRUE)[1]))
        active[[fid]] <- list(pid = pid, host = h, attempt = attempt, t0 = now, started = ts_utc(), est = est_wire(i))
        set(man, i, "status", "running")
        n_launched <- n_launched + 1L
        inf <- inflight()
      }
    }
    # ---- heartbeat
    if (now - last_hb > 30) { heartbeat(); last_hb <- now }
    # ---- termination and retry passes
    if (!length(active)) {
      if (drained || n_launched >= max_launch) break
      if (!any(man$status == "pending")) {
        retry <- which(man$status == "failed" & vapply(man$fid, n_failed_counted, integer(1)) < ctx$max_attempts)
        if (length(retry)) {
          retry_pass <- retry_pass + 1L
          set(man, retry, "status", "pending")
          L("retry pass ", retry_pass, ": ", length(retry), " failed file(s) re-queued")
        } else break
      }
    }
    Sys.sleep(ctx$poll)
  }
  man[status == "deferred", status := "pending"]
  if (final == "COMPLETE" && n_launched >= max_launch && any(man$status == "pending")) final <- "PILOT_DONE"
  failed_n <- sum(man$status == "failed"); eligible <- sum(man$status != "skipped")
  if (final == "COMPLETE" && failed_n / max(1, eligible) > 0.10) final <- "BLOCKED_FAIL_RATE"
  write_atomic_csv(man, file.path(ch, "state", "manifest.csv"))
  export_manifest(man, ctx$export_path)
  heartbeat(state = paste0("ENDED_", final))
  writeLines(final, file.path(ch, "state", "final_state.txt"))
  L("supervisor end: ", final, "; ok ", sum(man$status == "ok"), ", failed ", failed_n, ", pending ", sum(man$status == "pending"),
    "; wire ", round(committed_wire() / GBYTES, 3), " GB, decoded ", round(committed_dec() / GBYTES, 3), " GB")
  invisible(list(final = final, man = man, att = att))
}

# ---------------------------------------------------------------------
# Stages: prep, calibrate, pilot projection
# ---------------------------------------------------------------------
read_file_table <- function(ch = CH) {
  fread(file.path(ch, "state", "file_table.csv"),
        colClasses = list(character = c("fid", "provider_url", "index_url", "index_urls", "host", "content_class", "issuer_ids", "gzip_behaviour")))
}

prep_stage <- function() {
  write_tools()
  coh <- load_cohort()
  write_cohort_json(coh$plan_id)
  fwrite(coh, file.path(CH, "state", "cohort_plans.csv"))
  ft <- build_file_table()
  fwrite(ft, file.path(CH, "state", "file_table.csv"))
  kn <- ft[!is.na(listed_bytes)]
  lg("cohort plans ", nrow(coh), " (", uniqueN(coh$issuer_id), " issuers, ", uniqueN(coh$state), " states); files ", nrow(ft),
     " (distinct URLs); skipped ", sum(ft$skip), " (", round(sum(ft$listed_bytes[ft$skip], na.rm = TRUE) / GBYTES, 3),
     " GB listed, ", sum(ft$skip & is.na(ft$listed_bytes)), " of unknown size); to run ", sum(!ft$skip),
     "; individual-prefix files ", sum(ft$has_individual_prefix & !ft$skip), ", other ", sum(!ft$has_individual_prefix & !ft$skip),
     "; known size ", nrow(kn), " files ", round(sum(kn$listed_bytes) / GBYTES, 2), " GB; hosts ", uniqueN(ft$host), step = "prep")
  invisible(ft)
}

calibrate_stage <- function() {
  ft <- read_file_table()
  cal <- file.path(CH, "calib")
  unlink(cal, recursive = TRUE)
  write_tools(cal)
  file.copy(file.path(CH, "cohort_plans.json"), file.path(cal, "cohort_plans.json"), overwrite = TRUE)
  pick <- function(f) f[order(listed_bytes)][1]
  comp <- pick(ft[!skip & grepl("^gzip", gzip_behaviour) & !is.na(listed_bytes) & listed_bytes >= 5e6 & listed_bytes <= 250e6])
  none <- pick(ft[!skip & gzip_behaviour == "none" & !is.na(listed_bytes) & listed_bytes >= 1e6 & listed_bytes <= 50e6])
  rows <- list()
  for (k in list(list(kind = "compressing", f = comp), list(kind = "non-compressing", f = none))) {
    f <- k$f
    if (is.na(f$fid)) next
    stopifnot(free_gb() > DISK_PAUSE_GB)
    r <- run_stream_sync(f$fid, f$provider_url, ch = cal, maxtime = 900)
    rawf <- file.path(cal, paste0(f$fid, ".raw"))
    hdf <- file.path(cal, paste0(f$fid, ".raw.hdr"))
    raw_bytes <- NA_real_; raw_hdr <- ""
    if (k$kind == "compressing") {
      cmd <- paste("curl -sS --raw -H 'Accept-Encoding: gzip' -A", shQuote(UA), "-D", shQuote(hdf), shQuote(f$provider_url),
                   "| head -c 60000000 >", shQuote(rawf))
      system2("bash", c("-c", shQuote(cmd)), stdout = FALSE, stderr = FALSE)
      raw_bytes <- file.size(rawf)
      raw_hdr <- paste(grep("^(content-encoding|transfer-encoding|content-length)", sub("\r$", "", readLines(hdf, warn = FALSE)),
                            ignore.case = TRUE, value = TRUE), collapse = " | ")
      unlink(rawf)
    }
    rows[[length(rows) + 1L]] <- data.table(kind = k$kind, host = f$host, url = f$provider_url, listed_bytes = f$listed_bytes,
                                            size_download_compressed = as.numeric(r$wire_bytes), decoded_bytes_counted = as.numeric(r$decoded_bytes),
                                            raw_wire_bytes_capped_60MB = raw_bytes, raw_headers = raw_hdr,
                                            content_encoding_seen = r$content_encoding, status = r$status, tested_utc = ts_utc())
  }
  tab <- rbindlist(rows, fill = TRUE)
  tab[, ratio_wire_to_decoded := size_download_compressed / decoded_bytes_counted]
  tab[, size_download_minus_raw_pct := 100 * (size_download_compressed - raw_wire_bytes_capped_60MB) / raw_wire_bytes_capped_60MB]
  fwrite(tab, file.path(P$logs, "wp4_calibration.csv"))
  print(t(tab))
  account_calibration()
  invisible(tab)
}

# Calibration downloads count against the transfer cap: one pseudo-attempt row.
account_calibration <- function(ch = CH) {
  tab <- fread(file.path(P$logs, "wp4_calibration.csv"))
  wire <- sum(tab$size_download_compressed, tab$raw_wire_bytes_capped_60MB, na.rm = TRUE)
  dec <- sum(tab$decoded_bytes_counted, na.rm = TRUE)
  att <- attempts_load(list(ch = ch))
  att <- att[fid != "calib"]
  att <- rbind(att, data.table(fid = "calib", attempt = 1L, status = "calibration", http_status = "200", curl_exit = 0L,
                               wire_bytes = wire, decoded_bytes = dec, seconds = 0, started = ts_utc(), ended = ts_utc(),
                               host = "calibration", err = "-"))
  write_atomic_csv(att, file.path(ch, "state", "attempts.csv"))
  lg("calibration accounted against the cap: ", wire, " wire bytes", step = "calibrate")
}

# Extrapolate intermediate size from the first n ok files (pilot).
pilot_projection <- function(ch = CH, n = 5L) {
  man <- fread(file.path(ch, "state", "manifest.csv"), colClasses = list(character = c("fid", "status", "host")))
  ft <- read_file_table(ch)
  ok <- man[status == "ok"][order(fid)]
  pil <- ok[1:min(n, nrow(ok))]
  known <- ft[!skip & !is.na(listed_bytes)]
  med <- median(known$listed_bytes)
  tot_dec <- sum(ft$listed_bytes[!ft$skip], na.rm = TRUE) + med * sum(!ft$skip & is.na(ft$listed_bytes))
  per_byte <- sum(pil$out_bytes) / sum(pil$decoded_bytes)
  per_rec <- sum(pil$out_bytes) / sum(pil$individual_records)
  data.table(pilot_files = nrow(pil), pilot_out_mb = sum(pil$out_bytes) / 1e6, pilot_decoded_mb = sum(pil$decoded_bytes) / 1e6,
             out_bytes_per_decoded_byte = per_byte, out_bytes_per_individual_record = per_rec,
             projected_out_gb_by_decoded_bytes = per_byte * tot_dec / GBYTES, total_decoded_gb_assumed = tot_dec / GBYTES)
}

# ---------------------------------------------------------------------
# Taxonomy table (step 1)
# ---------------------------------------------------------------------
TAX_CODES <- list(
  orthopaedic = c("207X00000X", "207XS0114X", "207XX0004X", "207XS0106X", "207XS0117X", "207XX0801X", "207XP3100X", "207XX0005X"),
  dermatology = c("207N00000X", "207NI0002X", "207ND0900X", "207ND0101X", "207NP0225X", "207NS0135X"),
  ophthalmology = c("207W00000X", "207WX0120X", "207WX0009X", "207WX0109X", "207WX0200X", "207WX0110X", "207WX0107X", "207WX0108X"))
NUCC_FILE <- file.path(P$cache, "spike", "nucc_taxonomy_261.csv")
NUCC_SHA <- "0615cce1dac2fae75e8a0497657fd3ad81f9505c829fb6129a6551583f4fe0fb"

taxonomy_table <- function(nucc_file = NUCC_FILE) {
  nu <- fread(nucc_file, colClasses = "character", encoding = "UTF-8")
  tab <- rbindlist(lapply(names(TAX_CODES), function(f) data.table(code = TAX_CODES[[f]], family = f)))
  m <- nu[match(tab$code, Code)]
  if (anyNA(m$Code)) stop("taxonomy code(s) not in the NUCC file: ", paste(tab$code[is.na(m$Code)], collapse = ", "), call. = FALSE)
  tab[, `:=`(name = m$Display.Name %||% m[["Display Name"]], is_general = grepl("^207[XNW]00000X$", code),
             nucc_classification = m$Classification, nucc_specialization = m$Specialization, nucc_version = "26.1")]
  setcolorder(tab, c("code", "name", "family", "is_general", "nucc_version", "nucc_classification", "nucc_specialization"))
  tab[]
}

taxonomy_stage <- function() {
  sha <- sha256_file(NUCC_FILE)[[1]]
  tab <- taxonomy_table()
  fwrite(tab, file.path(P$config, "ortho_taxonomy.csv"))
  lg("taxonomy written: ", nrow(tab), " rows (", paste(names(table(tab$family)), table(tab$family), collapse = ", "),
     "); NUCC file sha256 matches WP1: ", identical(sha, NUCC_SHA), step = "taxonomy")
  invisible(tab)
}

# ---------------------------------------------------------------------
# Specialty text crosswalk (steps 2 and 4)
# ---------------------------------------------------------------------
XW_FAMILIES <- c("ortho_surgeon", "ortho_apc", "derm", "ophth", "other")
XW_PRECEDENCE <- c("ortho_apc", "ortho_surgeon", "derm", "ophth", "other")

load_crosswalk <- function(path = file.path(P$config, "specialty_text_crosswalk.csv")) {
  x <- fread(path, colClasses = "character", na.strings = NULL)
  need <- c("rule_id", "family", "include_pattern", "exclude_pattern", "rationale")
  if (!identical(names(x), need)) stop("crosswalk columns must be exactly: ", paste(need, collapse = ", "), call. = FALSE)
  if (!all(x$family %in% XW_FAMILIES)) stop("unknown family in crosswalk", call. = FALSE)
  if (anyDuplicated(x$rule_id)) stop("duplicate rule_id in crosswalk", call. = FALSE)
  x[]
}

# x: character vector of record-level specialty strings (labels joined by "|").
# Returns one row per x: logical flags per family plus the assigned family and rule.
classify_specialty <- function(x, rules) {
  x <- as.character(x)
  x[is.na(x) | !nzchar(x)] <- "<none>"
  hit <- matrix(FALSE, nrow = length(x), ncol = nrow(rules))
  for (j in seq_len(nrow(rules))) {
    inc <- grepl(rules$include_pattern[j], x, perl = TRUE, ignore.case = TRUE)
    if (nzchar(rules$exclude_pattern[j])) inc <- inc & !grepl(rules$exclude_pattern[j], x, perl = TRUE, ignore.case = TRUE)
    hit[, j] <- inc
  }
  fam_hit <- function(f) { j <- which(rules$family == f); if (length(j)) rowSums(hit[, j, drop = FALSE]) > 0 else rep(FALSE, length(x)) }
  out <- data.table(surgeon = fam_hit("ortho_surgeon"), apc = fam_hit("ortho_apc"), derm = fam_hit("derm"),
                    ophth = fam_hit("ophth"), other = fam_hit("other"))
  asg <- rep("other", length(x)); rid <- rep("none", length(x))
  for (f in rev(XW_PRECEDENCE)) {
    fl <- switch(f, ortho_apc = out$apc, ortho_surgeon = out$surgeon, derm = out$derm, ophth = out$ophth, other = out$other)
    j <- which(rules$family == f)
    first <- if (length(j)) apply(hit[, j, drop = FALSE], 1, function(r) if (any(r)) rules$rule_id[j][which(r)[1]] else NA_character_) else rep(NA_character_, length(x))
    asg[fl] <- f
    rid[fl] <- first[fl]
  }
  out[, `:=`(assigned_family = asg, rule_id = rid)]
  out[]
}

# WP3's mappability metric, kept for continuity: normalised text of any label equals a NUCC
# classification, specialization, display name, combined name or code.
norm_txt <- function(x) {
  x <- tolower(x)
  x <- gsub("orthopedic", "orthopaedic", x, fixed = TRUE)
  x <- gsub("[^a-z0-9 ]+", " ", x)
  trimws(gsub("\\s+", " ", x))
}
nucc_keys <- function(nucc_file = NUCC_FILE) {
  x <- fread(nucc_file, colClasses = "character", encoding = "UTF-8")
  setnames(x, make.names(names(x)))
  keys <- unique(c(norm_txt(x$Classification), norm_txt(x$Specialization), norm_txt(x$Display.Name),
                   norm_txt(paste(x$Classification, x$Specialization)), tolower(x$Code)))
  keys[nzchar(keys)]
}
# per record string: any label mappable, all labels mappable
nucc_mappable <- function(x, keys) {
  parts <- strsplit(x, "|", fixed = TRUE)
  any_l <- vapply(parts, function(p) any(norm_txt(p) %in% keys), logical(1))
  all_l <- vapply(parts, function(p) length(p) > 0L && all(norm_txt(p) %in% keys), logical(1))
  list(any_label = any_l, all_labels = all_l)
}

# ---------------------------------------------------------------------
# Aggregation: exact cross-file NPI x plan de-duplication
# ---------------------------------------------------------------------
read_gz_tsv <- function(f, cols, classes) {
  if (file.size(f) <= 20L) return(NULL)
  tryCatch(fread(cmd = paste("gzip -dc", shQuote(f)), header = FALSE, sep = "\t", quote = "", col.names = cols,
                 colClasses = classes, na.strings = NULL),
           error = function(e) NULL)
}

read_tsv0 <- function(f, cols, classes) {
  if (!file.exists(f) || file.size(f) == 0L) {
    e <- as.data.table(setNames(lapply(classes, function(cl) get(cl)(0)), cols))
    return(e)
  }
  fread(f, header = FALSE, sep = "\t", quote = "", col.names = cols, colClasses = classes, na.strings = NULL)
}

aggregate_stage <- function(ch = CH, agg = file.path(ch, "agg"), cohort_csv = file.path(ch, "state", "cohort_plans.csv")) {
  dir.create(agg, recursive = TRUE, showWarnings = FALSE)
  man <- fread(file.path(ch, "state", "manifest.csv"), colClasses = list(character = c("fid", "status")))
  ok <- man[status == "ok"][order(fid)]
  cohort_ids <- fread(cohort_csv, colClasses = "character")$plan_id
  lg("aggregate: ", nrow(ok), " ok files; free disk ", round(free_gb(ch), 2), " GB", step = "aggregate")
  specl <- vector("list", nrow(ok)); recl <- vector("list", nrow(ok)); setl <- vector("list", nrow(ok)); seenl <- vector("list", nrow(ok))
  for (k in seq_len(nrow(ok))) {
    d <- file.path(ch, "out", ok$fid[k])
    sp <- read_tsv0(file.path(d, "specs.tsv"), c("id", "text"), c("integer", "character"))
    st <- read_tsv0(file.path(d, "sets.tsv"), c("id", "plans"), c("integer", "character"))
    sn <- read_tsv0(file.path(d, "seen.tsv"), c("plan_id", "n_ind", "n_fac", "n_grp", "n_oth", "n_bad"),
                    c("character", rep("numeric", 5)))
    rc <- read_gz_tsv(file.path(d, "rec.tsv.gz"), c("npi", "spec", "set"), c("numeric", "integer", "integer"))
    specl[[k]] <- sp; setl[[k]] <- st; seenl[[k]] <- cbind(fi = k, sn)
    recl[[k]] <- if (is.null(rc)) NULL else cbind(fi = k, rc)
  }
  all_spec <- unique(unlist(lapply(specl, function(s) s$text)))
  offs <- c(0L, cumsum(vapply(setl, nrow, integer(1))))
  gset_plans <- vector("list", offs[length(offs)])
  for (k in seq_len(nrow(ok))) {
    if (nrow(setl[[k]])) {
      pl <- lapply(strsplit(setl[[k]]$plans, ",", fixed = TRUE), function(v) match(v, cohort_ids))
      gset_plans[offs[k] + setl[[k]]$id] <- pl
    }
  }
  R <- rbindlist(lapply(seq_len(nrow(ok)), function(k) {
    r <- recl[[k]]; if (is.null(r)) return(NULL)
    r[, .(npi, fi, spec_g = match(specl[[k]]$text, all_spec)[match(spec, specl[[k]]$id)],
          gset = fifelse(set == 0L, 0L, set + offs[k]))]
  }))
  rm(recl); invisible(gc())
  lg("aggregate: ", nrow(R), " individual records read; ", length(all_spec), " distinct record-level specialty strings; ",
     length(gset_plans), " plan sets", step = "aggregate")
  # per NPI: files, records, plan-set signature (union over files)
  setorder(R, npi, gset)
  U <- unique(R[gset > 0L, .(npi, gset)])
  U[, k := .N, by = npi]
  sg1 <- U[k == 1L, .(npi, sig = as.character(gset))]
  sgm <- U[k > 1L, .(sig = paste(gset, collapse = ".")), by = npi]
  rm(U)
  nf <- R[, .(n_files = uniqueN(fi), n_records = .N), by = npi]
  sg <- rbind(sg1, sgm)
  npi_core <- merge(nf, sg, by = "npi", all.x = TRUE)
  npi_core[is.na(sig), sig := ""]
  npi_spec <- unique(R[, .(npi, spec_g)])
  sigs <- unique(npi_core$sig[npi_core$sig != ""])
  sig_plans <- lapply(strsplit(sigs, ".", fixed = TRUE), function(ids) {
    v <- unique(unlist(gset_plans[as.integer(ids)])); sort(v[!is.na(v)])
  })
  names(sig_plans) <- sigs
  fi_spec <- unique(R[, .(fi, spec_g)])
  nrec_spec <- R[, .N, by = spec_g]
  saveRDS(npi_core, file.path(agg, "npi_core.rds")); saveRDS(npi_spec, file.path(agg, "npi_spec.rds"))
  saveRDS(sig_plans, file.path(agg, "sig_plans.rds")); saveRDS(all_spec, file.path(agg, "all_spec.rds"))
  saveRDS(fi_spec, file.path(agg, "fi_spec.rds")); saveRDS(nrec_spec, file.path(agg, "nrec_spec.rds"))
  seen <- rbindlist(seenl)
  seen_pl <- seen[, .(n_files_listing_plan = uniqueN(fi), n_files_with_individuals_listing_plan = uniqueN(fi[n_ind > 0]),
                      n_records_individual = sum(n_ind), n_records_facility = sum(n_fac), n_records_group = sum(n_grp),
                      n_records_other = sum(n_oth)), by = plan_id]
  fwrite(seen_pl, file.path(agg, "plan_seen.csv"))
  fwrite(ok[, .(fid)][, fi := .I], file.path(agg, "file_index.csv"))
  smry <- data.table(
    n_files_ok = nrow(ok), n_individual_records = nrow(R), n_distinct_individual_npis = nrow(npi_core),
    n_npis_in_cohort_plans = sum(npi_core$sig != ""), n_distinct_record_strings = length(all_spec),
    multi_file_npi_share = mean(npi_core$n_files > 1L), n_distinct_plan_sets = length(gset_plans),
    n_distinct_signatures = length(sigs))
  fwrite(smry, file.path(agg, "agg_summary.csv"))
  lg("aggregate: ", nrow(npi_core), " distinct individual NPIs (", smry$n_npis_in_cohort_plans, " in at least one cohort plan); ",
     round(100 * smry$multi_file_npi_share, 1), " percent appear in more than one file; ", length(sigs), " distinct plan signatures",
     step = "aggregate")
  invisible(smry)
}

# sc: one row per plan-set signature with numeric count columns; sig_plans: named list of plan
# indices per signature. Returns a plans x columns matrix of exact sums (each NPI counted once per plan).
sig_plan_sums <- function(sc, cols, sig_plans, n_plans, chunk = 20000L) {
  tot <- matrix(0, nrow = n_plans, ncol = length(cols), dimnames = list(NULL, cols))
  for (s0 in seq(1L, nrow(sc), by = chunk)) {
    sub <- sc[s0:min(nrow(sc), s0 + chunk - 1L)]
    pl <- sig_plans[match(sub$sig, names(sig_plans))]
    ex <- sub[rep(seq_len(.N), lengths(pl))]
    ex[, plan := unlist(pl, use.names = FALSE)]
    ag <- ex[, lapply(.SD, sum), by = plan, .SDcols = cols]
    tot[ag$plan, ] <- tot[ag$plan, , drop = FALSE] + as.matrix(ag[, cols, with = FALSE])
  }
  tot
}

# NPI flags and per-plan counts under a crosswalk. Returns list(npi_flags, plan_counts, label_vocab, summary).
apply_crosswalk_counts <- function(rules, agg = file.path(CH, "agg"), cohort_csv = file.path(CH, "state", "cohort_plans.csv")) {
  cohort_ids <- fread(cohort_csv, colClasses = "character")$plan_id
  all_spec <- readRDS(file.path(agg, "all_spec.rds"))
  npi_core <- readRDS(file.path(agg, "npi_core.rds"))
  npi_spec <- readRDS(file.path(agg, "npi_spec.rds"))
  sig_plans <- readRDS(file.path(agg, "sig_plans.rds"))
  cls <- classify_specialty(all_spec, rules)
  npi_spec[, `:=`(zs = as.integer(cls$surgeon[spec_g]), za = as.integer(cls$apc[spec_g]),
                  zd = as.integer(cls$derm[spec_g]), zo = as.integer(cls$ophth[spec_g]))]
  ns <- npi_spec[, .(has_surgeon = max(zs) > 0L, has_apc = max(za) > 0L, has_derm = max(zd) > 0L, has_ophth = max(zo) > 0L), by = npi]
  ns[, ortho_apc := has_apc]
  ns[, ortho_surgeon := has_surgeon & !has_apc]
  fl <- merge(npi_core, ns[, .(npi, ortho_surgeon, ortho_apc, derm = has_derm, ophth = has_ophth)], by = "npi")
  fl[, fam := as.integer(ortho_surgeon) + 2L * as.integer(ortho_apc) + 4L * as.integer(derm) + 8L * as.integer(ophth)]
  inc <- fl[sig != ""]
  sc <- inc[, .(n_all = .N, n_s = sum(bitwAnd(fam, 1L) > 0L), n_a = sum(bitwAnd(fam, 2L) > 0L),
                n_d = sum(bitwAnd(fam, 4L) > 0L), n_o = sum(bitwAnd(fam, 8L) > 0L)), by = sig]
  tot <- sig_plan_sums(sc, c("n_all", "n_s", "n_a", "n_d", "n_o"), sig_plans, length(cohort_ids))
  pc <- data.table(plan_id = cohort_ids, n_individual_npis = tot[, "n_all"], n_ortho_surgeon_text = tot[, "n_s"],
                   n_ortho_apc_text = tot[, "n_a"], n_derm_text = tot[, "n_d"], n_ophth_text = tot[, "n_o"])
  list(npi_flags = fl, plan_counts = pc, class_by_spec = cls, all_spec = all_spec)
}

# Label-level vocabulary: specialty_text, n_npis, n_files, assigned_family, rule_id (no names).
build_vocab <- function(rules, agg = file.path(CH, "agg")) {
  all_spec <- readRDS(file.path(agg, "all_spec.rds"))
  npi_spec <- readRDS(file.path(agg, "npi_spec.rds"))
  fi_spec <- readRDS(file.path(agg, "fi_spec.rds"))
  labs <- strsplit(all_spec, "|", fixed = TRUE)
  lab_all <- unique(unlist(labs))
  lab_ids <- lapply(labs, function(v) match(v, lab_all))
  nl <- lengths(lab_ids)
  ex <- npi_spec[rep(seq_len(.N), nl[spec_g])]
  ex[, lab := unlist(lab_ids[npi_spec$spec_g], use.names = FALSE)]
  n_npis <- unique(ex[, .(npi, lab)])[, .N, by = lab]
  rm(ex)
  ef <- fi_spec[rep(seq_len(.N), nl[spec_g])]
  ef[, lab := unlist(lab_ids[fi_spec$spec_g], use.names = FALSE)]
  n_files <- unique(ef[, .(fi, lab)])[, .N, by = lab]
  cls <- classify_specialty(lab_all, rules)
  v <- data.table(specialty_text = lab_all, lab = seq_along(lab_all), assigned_family = cls$assigned_family, rule_id = cls$rule_id)
  v[n_npis, n_npis := i.N, on = "lab"]
  v[n_files, n_files := i.N, on = "lab"]
  setorder(v, -n_npis)
  v[, .(specialty_text, n_npis, n_files, assigned_family, rule_id)]
}

# ---------------------------------------------------------------------
# Vocabulary, freeze, exposure table (steps 4 and 5)
# ---------------------------------------------------------------------
WATCH_RE <- "orthop|ortho|bone|joint|musculoskeletal|spine|spinal|hand|foot|ankle|trauma|sport|derm|skin|ophth|eye"

vocab_stage <- function() {
  rules <- load_crosswalk()
  v <- build_vocab(rules)
  fwrite(v, file.path(OUT_DIR, "specialty_vocab.csv"))
  w <- v[assigned_family == "other" & rule_id == "none" & grepl(WATCH_RE, specialty_text, ignore.case = TRUE)]
  fwrite(w, file.path(P$logs, "wp4_vocab_watch_unmapped.csv"))
  lg("vocab: ", nrow(v), " distinct labels; assigned: ", paste(names(table(v$assigned_family)), table(v$assigned_family), collapse = ", "),
     "; unmapped labels with watch terms: ", nrow(w), step = "vocab")
  invisible(v)
}

crosswalk_hashes <- function() {
  c(crosswalk = sha256_file(file.path(P$config, "specialty_text_crosswalk.csv"))[[1]],
    taxonomy = sha256_file(file.path(P$config, "ortho_taxonomy.csv"))[[1]])
}

freeze_stage <- function() {
  h <- crosswalk_hashes()
  writeLines(c(paste0("frozen_utc=", ts_utc()),
               paste0("specialty_text_crosswalk.csv sha256=", h[["crosswalk"]]),
               paste0("ortho_taxonomy.csv sha256=", h[["taxonomy"]]),
               "No outcome (denial, claim, appeal or enrollment) column was read by WP4 at any point; the crosswalk was refined on vocabulary evidence only (see wp4_crosswalk_changes.csv)."),
             file.path(P$logs, "wp4_crosswalk_hash.txt"))
  lg("crosswalk frozen: ", h[["crosswalk"]], step = "vocab")
  invisible(h)
}

frozen_hash_ok <- function() {
  f <- file.path(P$logs, "wp4_crosswalk_hash.txt")
  if (!file.exists(f)) return(NA)
  L <- readLines(f, warn = FALSE)
  old <- sub("^.*sha256=", "", grep("^specialty_text_crosswalk.csv", L, value = TRUE))
  identical(old, crosswalk_hashes()[["crosswalk"]])
}

NPPES_CODE_COLS <- c("n_ortho_207x_any", paste0("n_", TAX_CODES$orthopaedic), "n_derm_207n", "n_ophth_207w", "n_individual_nppes_matched")

# Pure function: one row per cohort plan with an explicit exposure_status.
build_exposure <- function(coh, pc, seen, iss, ft, man) {
  x <- merge(coh, pc, by = "plan_id", all.x = TRUE, sort = FALSE)
  x <- merge(x, seen[, .(plan_id, n_files_listing_plan, n_files_with_individuals_listing_plan)], by = "plan_id", all.x = TRUE, sort = FALSE)
  x[is.na(n_files_listing_plan), n_files_listing_plan := 0L]
  x[is.na(n_files_with_individuals_listing_plan), n_files_with_individuals_listing_plan := 0L]
  x[, plan_in_provider_files := n_files_listing_plan > 0L]
  f <- ft[skip == FALSE, .(fid, issuer_ids)]
  fl <- f[, .(issuer_id = unlist(strsplit(issuer_ids, ";", fixed = TRUE))), by = fid]
  fl <- merge(fl, man[, .(fid, status)], by = "fid", all.x = TRUE)
  isf <- fl[, .(issuer_n_files = .N, issuer_n_files_ok = sum(status == "ok", na.rm = TRUE)), by = issuer_id]
  x <- merge(x, isf, by = "issuer_id", all.x = TRUE, sort = FALSE)
  x <- merge(x, iss[, .(issuer_id, index_status)], by = "issuer_id", all.x = TRUE, sort = FALSE)
  x[is.na(issuer_n_files), `:=`(issuer_n_files = 0L, issuer_n_files_ok = 0L)]
  x[, unreachable := is.na(index_status) | index_status != "ok" | issuer_n_files == 0L]
  x[, incomplete := !unreachable & issuer_n_files_ok < issuer_n_files]
  x[, exposure_status := fcase(
    unreachable, "issuer_unreachable",
    incomplete, "issuer_incomplete",
    plan_in_provider_files & n_individual_npis > 0, "ok",
    plan_in_provider_files, "no_individuals",
    default = "plan_not_in_directory")]
  cnt <- c("n_individual_npis", "n_ortho_surgeon_text", "n_ortho_apc_text", "n_derm_text", "n_ophth_text")
  for (cc in cnt) {
    x[exposure_status %in% c("issuer_unreachable", "issuer_incomplete", "plan_not_in_directory"), (cc) := NA_real_]
    x[exposure_status == "no_individuals", (cc) := 0]
    set(x, j = cc, value = as.integer(x[[cc]]))
  }
  for (cc in NPPES_CODE_COLS) set(x, j = cc, value = NA_integer_)
  x[, exposure_source := "text"]
  x[, issuer_files_incomplete := incomplete]
  setcolorder(x, c("plan_id", "issuer_id", "state", "exposure_status", "plan_in_provider_files", "n_files_listing_plan",
                   "n_individual_npis", "n_ortho_surgeon_text", "n_ortho_apc_text", "n_derm_text", "n_ophth_text",
                   NPPES_CODE_COLS, "exposure_source", "n_files_with_individuals_listing_plan", "issuer_n_files",
                   "issuer_n_files_ok", "issuer_files_incomplete"))
  x[, c("index_status", "unreachable", "incomplete") := NULL]
  x[order(match(plan_id, coh$plan_id))]
}

exposure_stage <- function() {
  rules <- load_crosswalk()
  fz <- frozen_hash_ok()
  if (isFALSE(fz)) stop("crosswalk changed after the freeze; rerun freeze deliberately", call. = FALSE)
  if (is.na(fz)) lg("WARNING: crosswalk not frozen yet", step = "exposure", level = "WARN")
  r <- apply_crosswalk_counts(rules)
  coh <- fread(file.path(CH, "state", "cohort_plans.csv"), colClasses = "character")
  seen <- fread(file.path(CH, "agg", "plan_seen.csv"), colClasses = list(character = "plan_id"))
  iss <- fread(file.path(OUT_DIR, "issuer_sources.csv"), colClasses = c(issuer_id = "character"))
  ft <- read_file_table()
  man <- fread(file.path(CH, "state", "manifest.csv"), colClasses = list(character = c("fid", "status")))
  x <- build_exposure(coh, r$plan_counts, seen, iss, ft, man)
  fwrite(x, file.path(OUT_DIR, "plan_exposure.csv"))
  saveRDS(r$npi_flags, file.path(CH, "agg", "npi_flags.rds"))
  fl <- r$npi_flags
  st <- list(
    n_npis = nrow(fl), n_npis_cohort = sum(fl$sig != ""),
    n_surgeon = sum(bitwAnd(fl$fam, 1L) > 0L), n_apc = sum(bitwAnd(fl$fam, 2L) > 0L),
    n_derm = sum(bitwAnd(fl$fam, 4L) > 0L), n_ophth = sum(bitwAnd(fl$fam, 8L) > 0L),
    n_surgeon_cohort = sum(bitwAnd(fl$fam, 1L) > 0L & fl$sig != ""), n_apc_cohort = sum(bitwAnd(fl$fam, 2L) > 0L & fl$sig != ""),
    n_derm_cohort = sum(bitwAnd(fl$fam, 4L) > 0L & fl$sig != ""), n_ophth_cohort = sum(bitwAnd(fl$fam, 8L) > 0L & fl$sig != ""))
  saveRDS(st, file.path(CH, "agg", "exposure_stats.rds"))
  lg("exposure: ", nrow(x), " plans; status ", paste(names(table(x$exposure_status)), table(x$exposure_status), collapse = ", "),
     "; issuer-states with status ok: ", uniqueN(x[exposure_status == "ok", paste(issuer_id, state)]), step = "exposure")
  invisible(x)
}

# ---------------------------------------------------------------------
# G3 validation, route (b): NPI Registry API on a stratified random sample
# ---------------------------------------------------------------------
API_URL <- "https://npiregistry.cms.hhs.gov/api/?version=2.1&number="
AMBIGUOUS_RE <- "surgery|sports|hand|spine|spinal|trauma|musculoskeletal|joint"
G3_N <- c(A = 300L, B = 100L, C1 = 400L, C2 = 100L, D_derm = 50L, D_ophth = 50L)

fmt_npi <- function(x) sprintf("%.0f", x)

# Stratified sample of at most 1,000 NPIs from the frame (individual NPIs listed in at least one cohort plan).
g3_design <- function(fl, amb_npi, seed = SEED, n = G3_N) {
  set.seed(seed)
  fr <- fl[sig != ""]
  take <- function(v, k) if (length(v) <= k) v else v[sample.int(length(v), k)]
  A <- fr$npi[bitwAnd(fr$fam, 1L) > 0L]
  B <- fr$npi[bitwAnd(fr$fam, 2L) > 0L]
  U <- fr$npi[fr$fam == 0L]
  Dd <- fr$npi[bitwAnd(fr$fam, 3L) == 0L & bitwAnd(fr$fam, 4L) > 0L]
  Do <- fr$npi[bitwAnd(fr$fam, 3L) == 0L & bitwAnd(fr$fam, 4L) == 0L & bitwAnd(fr$fam, 8L) > 0L]
  Uamb <- U[U %in% amb_npi]
  sA <- take(A, n[["A"]]); sB <- take(B, n[["B"]]); sC1 <- take(U, n[["C1"]])
  sC2 <- take(setdiff(Uamb, sC1), n[["C2"]])
  sDd <- take(Dd, n[["D_derm"]]); sDo <- take(Do, n[["D_ophth"]])
  smp <- rbindlist(list(data.table(npi = sA, stratum = "A"), data.table(npi = sB, stratum = "B"),
                        data.table(npi = sC1, stratum = "C1"), data.table(npi = sC2, stratum = "C2"),
                        data.table(npi = sDd, stratum = "D_derm"), data.table(npi = sDo, stratum = "D_ophth")))
  smp[, npi := fmt_npi(npi)]
  smp[, ambiguous := npi %in% fmt_npi(Uamb)]
  frame <- data.table(stratum = c("A", "B", "U_all", "U_ambiguous", "D_derm", "D_ophth"),
                      frame_n = c(length(A), length(B), length(U), length(Uamb), length(Dd), length(Do)))
  list(sample = smp, frame = frame)
}

parse_api <- function(txt) {
  j <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(j)) return(NULL)
  rc <- suppressWarnings(as.integer(j$result_count %||% 0L))
  if (is.na(rc)) rc <- 0L
  base <- list(result_count = rc, enumeration_type = "", status = "", codes = "", primary = "")
  if (rc < 1L || !length(j$results)) return(base)
  r <- j$results[[1]]
  tx <- r$taxonomies %||% list()
  base$enumeration_type <- r$enumeration_type %||% ""
  base$status <- (r$basic %||% list())$status %||% ""
  base$codes <- paste(vapply(tx, function(t) t$code %||% "", character(1)), collapse = "|")
  base$primary <- paste(vapply(tx, function(t) if (isTRUE(t$primary)) (t$code %||% "") else "", character(1)), collapse = "")
  base
}

api_lookup <- function(npi, max_try = 3L) {
  for (k in seq_len(max_try)) {
    out <- suppressWarnings(system2("curl", c("-sS", "--max-time", "30", "-A", shQuote(UA), "-w", shQuote("\\n%{http_code}"),
                                              shQuote(paste0(API_URL, npi))), stdout = TRUE, stderr = FALSE))
    code <- suppressWarnings(as.integer(out[length(out)]))
    body <- paste(out[-length(out)], collapse = "\n")
    if (!is.na(code) && code == 200L) {
      pr <- parse_api(body)
      if (!is.null(pr)) return(c(list(npi = npi, http = 200L), pr))
    }
    if (!is.na(code) && (code == 429L || code >= 500L)) Sys.sleep(30 * 2^(k - 1L)) else Sys.sleep(5)
  }
  list(npi = npi, http = if (is.na(code)) NA_integer_ else code, result_count = NA_integer_, enumeration_type = "", status = "", codes = "", primary = "")
}

g3_sample_stage <- function() {
  dir.create(CA, recursive = TRUE, showWarnings = FALSE)
  fl <- readRDS(file.path(CH, "agg", "npi_flags.rds"))
  npi_spec <- readRDS(file.path(CH, "agg", "npi_spec.rds"))
  all_spec <- readRDS(file.path(CH, "agg", "all_spec.rds"))
  amb_spec <- grepl(AMBIGUOUS_RE, all_spec, ignore.case = TRUE)
  amb_npi <- unique(npi_spec$npi[amb_spec[npi_spec$spec_g]])
  d <- g3_design(fl, amb_npi)
  fwrite(d$sample, file.path(CA, "g3_sample.csv"))
  fwrite(d$frame, file.path(CA, "g3_frame.csv"))
  lg("G3 sample: ", paste(names(table(d$sample$stratum)), table(d$sample$stratum), collapse = ", "), "; frame ",
     paste(d$frame$stratum, d$frame$frame_n, collapse = ", "), step = "g3")
  invisible(d)
}

g3_run_stage <- function(max_new = Inf) {
  smp <- fread(file.path(CA, "g3_sample.csv"), colClasses = c(npi = "character"))
  rf <- file.path(CA, "g3_results.csv")
  done <- if (file.exists(rf)) fread(rf, colClasses = c(npi = "character"))$npi else character(0)
  todo <- smp[!npi %in% done]
  lg("G3 run: ", nrow(todo), " NPIs to query (", length(done), " already done)", step = "g3")
  consec <- 0L; n <- 0L
  for (i in seq_len(nrow(todo))) {
    if (n >= max_new) break
    t0 <- as.numeric(Sys.time())
    r <- api_lookup(todo$npi[i])
    row <- data.table(npi = r$npi, stratum = todo$stratum[i], http = r$http, result_count = r$result_count,
                      enumeration_type = r$enumeration_type, status = r$status, codes = r$codes, primary_code = r$primary,
                      queried_utc = ts_utc())
    fwrite(row, rf, append = file.exists(rf))
    consec <- if (is.na(r$result_count)) consec + 1L else 0L
    n <- n + 1L
    if (consec >= 10L) { lg("G3 run: 10 consecutive failures, stopping", step = "g3", level = "ERROR"); break }
    Sys.sleep(max(0, 1.05 - (as.numeric(Sys.time()) - t0)))
  }
  lg("G3 run: queried ", n, " NPIs", step = "g3")
  invisible(n)
}

# Reference-standard flags from taxonomy codes (any slot).
tax_flags <- function(codes) {
  sp <- strsplit(codes, "|", fixed = TRUE)
  anyp <- function(re) vapply(sp, function(v) any(grepl(re, v)), logical(1))
  data.table(any_207x = anyp("^207X"), any_207n = anyp("^207N"), any_207w = anyp("^207W"),
             any_physician = anyp("^20"), any_pa = anyp("^363A"), any_np = anyp("^363L"), any_apc_other = anyp("^36[4-7]"))
}

# Weighted sensitivity of the surgeon text flag (and surgeon-or-APC) for any 207X, stratum proportions weighted back
# to the frame by stratum size. Point estimate = median, limits = 2.5 and 97.5 percentiles of 2,000 Monte Carlo draws of each stratum
# proportion from its Jeffreys Beta(x + 0.5, n - x + 0.5) posterior (percentile limits). A nonparametric bootstrap is
# degenerate when a stratum has zero events, which is the case that matters here.
g3_sensitivity <- function(res, frame, B = 2000L, seed = SEED) {
  fn <- function(s) frame$frame_n[frame$stratum == s]
  rest_n <- fn("U_all") - fn("U_ambiguous"); amb_n <- fn("U_ambiguous")
  f <- res[found_active == TRUE]
  grp <- list(A = f[stratum == "A"]$any_207x, B = f[stratum == "B"]$any_207x,
              Urest = f[(stratum == "C1") & ambiguous == FALSE]$any_207x,
              Uamb = f[(stratum == "C2") | (stratum == "C1" & ambiguous == TRUE)]$any_207x)
  est <- function(p) {
    tpA <- fn("A") * p[["A"]]; tpB <- fn("B") * p[["B"]]; fnn <- rest_n * p[["Urest"]] + amb_n * p[["Uamb"]]
    c(sens_surgeon = tpA / (tpA + tpB + fnn), sens_surgeon_or_apc = (tpA + tpB) / (tpA + tpB + fnn),
      est_207x_holders_missed_by_text = fnn, est_207x_holders_flagged_surgeon = tpA, est_207x_holders_flagged_apc = tpB)
  }
  pt <- est(vapply(grp, mean, numeric(1)))
  set.seed(seed)
  bs <- t(vapply(seq_len(B), function(b) est(vapply(grp, function(v) stats::rbeta(1, sum(v) + 0.5, length(v) - sum(v) + 0.5), numeric(1))), numeric(5)))
  lim <- apply(bs, 2, stats::quantile, probs = c(0.025, 0.975), na.rm = TRUE)
  pt <- apply(bs, 2, stats::median)  # posterior median: coherent with the limits when a stratum has zero events
  list(point = pt, lo = lim[1, ], hi = lim[2, ],
       events = c(unflagged_found = length(grp$Urest) + length(grp$Uamb), unflagged_207x = sum(grp$Urest) + sum(grp$Uamb),
                  surgeon_found = length(grp$A), surgeon_207x = sum(grp$A), apc_found = length(grp$B), apc_207x = sum(grp$B)))
}

g3_report_stage <- function() {
  smp <- fread(file.path(CA, "g3_sample.csv"), colClasses = c(npi = "character"))
  fr <- fread(file.path(CA, "g3_frame.csv"))
  res <- fread(file.path(CA, "g3_results.csv"), colClasses = c(npi = "character"))
  # strata come from the CURRENT sample; cached API answers of NPIs that left the sample are ignored
  res <- merge(smp[, .(npi, stratum, ambiguous)], res[, !"stratum"], by = "npi")
  if (nrow(res) != nrow(smp)) stop("API results missing for ", nrow(smp) - nrow(res), " sampled NPIs; run g3run first", call. = FALSE)
  res[, queried_ok := !is.na(result_count)]
  res[, found_active := queried_ok & result_count >= 1L & enumeration_type == "NPI-1" & status == "A"]
  tf <- tax_flags(res$codes)
  res <- cbind(res, tf)
  W <- function(x, n) { w <- wilson_ci(x, n); c(share = w$est, lo = w$lo, hi = w$hi) }
  by_s <- res[, .(n_sampled = .N, n_queried = sum(queried_ok), n_found_active = sum(found_active),
                  n_not_in_registry = sum(queried_ok & result_count == 0L),
                  n_org_npi = sum(queried_ok & result_count >= 1L & enumeration_type != "NPI-1"),
                  n_inactive = sum(queried_ok & result_count >= 1L & enumeration_type == "NPI-1" & status != "A")), by = stratum]
  by_s <- merge(by_s, data.table(stratum = c("A", "B", "C1", "C2", "D_derm", "D_ophth")), by = "stratum", all = TRUE)
  ppv <- function(s, col) { r <- res[stratum == s & found_active == TRUE]; list(x = sum(r[[col]]), n = nrow(r)) }
  rows <- list(
    c(list(measure = "PPV surgeon text flag (A) for any 207X among found active"), ppv("A", "any_207x")),
    c(list(measure = "PPV APC text flag (B) for any 207X among found active"), ppv("B", "any_207x")),
    c(list(measure = "Stratum B holding a physician taxonomy (20xxxxxxx)"), ppv("B", "any_physician")),
    c(list(measure = "Stratum B holding a physician assistant taxonomy (363A)"), ppv("B", "any_pa")),
    c(list(measure = "Stratum B holding a nurse practitioner taxonomy (363L)"), ppv("B", "any_np")),
    c(list(measure = "PPV derm text flag for any 207N among found active"), ppv("D_derm", "any_207n")),
    c(list(measure = "PPV ophth text flag for any 207W among found active"), ppv("D_ophth", "any_207w")))
  pp <- rbindlist(lapply(rows, function(r) { w <- W(r$x, r$n); data.table(measure = r$measure, x = r$x, n = r$n, share = w[["share"]], lo = w[["lo"]], hi = w[["hi"]]) }))
  cc <- res[stratum %in% c("C1", "C2")]
  cf <- W(sum(cc$found_active), nrow(cc))
  c1 <- res[stratum == "C1"]; c1f <- W(sum(c1$found_active), nrow(c1))
  c2 <- res[stratum == "C2"]; c2f <- W(sum(c2$found_active), nrow(c2))
  unmatched <- cc[found_active == FALSE]
  n_unexpl <- sum(unmatched$queried_ok & unmatched$result_count == 0L)
  share_unexpl <- n_unexpl / nrow(cc)
  verdict <- if (cf[["share"]] >= 0.95) "PASS" else if (share_unexpl <= 0.05) "PASS-CONDITIONAL" else "FAIL"
  sens <- g3_sensitivity(res, fr)
  # all strata: found share
  allf <- W(sum(res$found_active), nrow(res))
  out <- rbindlist(list(
    data.table(item = "route", value = "b (NPI Registry API sample)"),
    data.table(item = "verdict", value = verdict),
    data.table(item = "found_active_share_C1_C2", value = sprintf("%.4f", cf[["share"]])),
    data.table(item = "found_active_share_C1_C2_lo", value = sprintf("%.4f", cf[["lo"]])),
    data.table(item = "found_active_share_C1_C2_hi", value = sprintf("%.4f", cf[["hi"]])),
    data.table(item = "found_active_x_C1_C2", value = as.character(sum(cc$found_active))),
    data.table(item = "n_C1_C2", value = as.character(nrow(cc))),
    data.table(item = "found_active_share_C1", value = sprintf("%.4f", c1f[["share"]])),
    data.table(item = "found_active_share_C2", value = sprintf("%.4f", c2f[["share"]])),
    data.table(item = "found_active_share_all_strata", value = sprintf("%.4f", allf[["share"]])),
    data.table(item = "unmatched_not_in_registry_share_C1_C2", value = sprintf("%.4f", share_unexpl)),
    data.table(item = paste0("ppv_", c("surgeon", "apc"), "_any_207x"), value = sprintf("%.4f", pp$share[1:2])),
    data.table(item = paste0("ppv_", c("surgeon", "apc"), "_any_207x_lo"), value = sprintf("%.4f", pp$lo[1:2])),
    data.table(item = paste0("ppv_", c("surgeon", "apc"), "_any_207x_hi"), value = sprintf("%.4f", pp$hi[1:2])),
    data.table(item = "ppv_derm_207n", value = sprintf("%.4f", pp$share[6])),
    data.table(item = "ppv_ophth_207w", value = sprintf("%.4f", pp$share[7])),
    data.table(item = "apc_physician_taxonomy_share", value = sprintf("%.4f", pp$share[3])),
    data.table(item = "apc_pa_taxonomy_share", value = sprintf("%.4f", pp$share[4])),
    data.table(item = "apc_np_taxonomy_share", value = sprintf("%.4f", pp$share[5])),
    data.table(item = names(sens$point), value = sprintf("%.4f", sens$point)),
    data.table(item = paste0(names(sens$lo), "_lo"), value = sprintf("%.4f", sens$lo)),
    data.table(item = paste0(names(sens$hi), "_hi"), value = sprintf("%.4f", sens$hi)),
    data.table(item = names(sens$events), value = as.character(sens$events))))
  keys <- c("surgeon_207x", "apc_207x", "apc_physician", "apc_pa", "apc_np", "derm_207n", "ophth_207w")
  extra <- rbindlist(lapply(seq_along(keys), function(i) data.table(
    item = paste0("pp_", keys[i], c("_x", "_n", "_share", "_lo", "_hi")),
    value = c(as.character(pp$x[i]), as.character(pp$n[i]), sprintf("%.4f", c(pp$share[i], pp$lo[i], pp$hi[i]))))))
  a_no <- res[stratum == "A" & found_active == TRUE & any_207x == FALSE]
  extra2 <- data.table(item = c("a_no207x_n", "a_no207x_pa", "a_no207x_np", "a_no207x_other_physician", "a_no207x_none", "n_sampled_total", "n_found_active_total"),
                       value = as.character(c(nrow(a_no), sum(a_no$any_pa), sum(a_no$any_np), sum(a_no$any_physician & !a_no$any_pa & !a_no$any_np),
                                              sum(!a_no$any_physician & !a_no$any_pa & !a_no$any_np), nrow(res), sum(res$found_active))))
  out <- rbind(out, extra, extra2)
  fwrite(out, file.path(OUT_DIR, "npi_match_summary.csv"))
  fwrite(by_s, file.path(CA, "g3_by_stratum.csv")); fwrite(pp, file.path(CA, "g3_ppv.csv"))
  g3_write_md(out, by_s, pp, sens, fr, verdict)
  lg("G3 report: verdict ", verdict, "; found active share in C1+C2 ", sprintf("%.3f", cf[["share"]]), step = "g3")
  invisible(out)
}

g3_write_md <- function(out, by_s, pp, sens, fr, verdict) {
  v <- function(k) out$value[out$item == k]
  pc <- function(x) sprintf("%.1f percent", 100 * as.numeric(x))
  L <- c(
    "G3 VALIDATION: NPI MATCH AND TEXT-FLAG MEASUREMENT ERROR (WP4)",
    "",
    paste0("Route used: b (NPI Registry API on a stratified random sample), because the NPPES bulk zip was not reachable at the start of the harvest (see v1/logs/wp4_nppes_start.csv and wp4_nppes_end.csv). Verdict: ", verdict, "."),
    "",
    "Verdict rule: the task spec gives PASS if at least 95 percent of the sampled unflagged individual NPIs (strata C1 and C2) are found as active individuals, PASS-CONDITIONAL if the share is lower but the unmatched are explained, and FAIL otherwise. WP4 operationalised \"explained\" with a numeric bound of its own, which the spec does not give: PASS-CONDITIONAL requires that at most 5 percent of those NPIs are absent from the registry altogether (the rest then carry a stated reason in the registry record: organisation NPI or inactive status).",
    "",
    paste0("Found as active individual (strata C1 and C2, ", v("n_C1_C2"), " NPIs): ", pc(v("found_active_share_C1_C2")), " (Wilson 95 percent limits ", pc(v("found_active_share_C1_C2_lo")), " to ", pc(v("found_active_share_C1_C2_hi")), "); C1 alone ", pc(v("found_active_share_C1")), ", C2 alone ", pc(v("found_active_share_C2")), ". Share of C1 and C2 NPIs absent from the registry altogether: ", pc(v("unmatched_not_in_registry_share_C1_C2")), "."),
    "",
    "Sample design (seed from config study.seed; frame = individual NPIs listed in at least one cohort plan, classified under the frozen crosswalk): A 300 surgeon-text NPIs, B 100 APC-text NPIs, C1 400 random unflagged, C2 100 unflagged with an ambiguous term (surgery, sports, hand, spine, trauma, musculoskeletal, joint), D 50 derm-text and 50 ophth-text; a stratum with fewer NPIs contributes all of them. At most one request per second, back-off on 429 or 5xx. Frame sizes:",
    "",
    paste0("- ", fr$stratum, ": ", format(fr$frame_n, big.mark = ","), collapse = "\n"),
    "",
    "Results by stratum (found = result_count at least 1 and enumeration type NPI-1 and status A):",
    "",
    paste0("- ", by_s$stratum, ": sampled ", by_s$n_sampled, ", queried ", by_s$n_queried, ", found active ", by_s$n_found_active,
           ", not in registry ", by_s$n_not_in_registry, ", organisation NPI ", by_s$n_org_npi, ", inactive ", by_s$n_inactive, collapse = "\n"),
    "",
    "Positive predictive value against taxonomy codes (any slot), among sampled NPIs found as active individuals, Wilson 95 percent limits:",
    "",
    paste0("- ", pp$measure, ": ", pp$x, " of ", pp$n, " = ", pc(pp$share), " (", pc(pp$lo), " to ", pc(pp$hi), ")", collapse = "\n"),
    "",
    paste0("Sensitivity of the text flags for any 207X code, from the unflagged strata weighted back to the frame by stratum size (point estimate = median and limits = 2.5th and 97.5th percentiles of 2,000 Monte Carlo draws of each stratum proportion from its Jeffreys Beta posterior; used instead of a bootstrap because a stratum with zero events makes the bootstrap degenerate): surgeon-text flag ", pc(sens$point[["sens_surgeon"]]), " (", pc(sens$lo[["sens_surgeon"]]), " to ", pc(sens$hi[["sens_surgeon"]]), "); surgeon or APC text ", pc(sens$point[["sens_surgeon_or_apc"]]), " (", pc(sens$lo[["sens_surgeon_or_apc"]]), " to ", pc(sens$hi[["sens_surgeon_or_apc"]]), ")."),
    "",
    paste0("How few events there are: among ", sens$events[["unflagged_found"]], " unflagged NPIs found active, ", sens$events[["unflagged_207x"]], " held a 207X code; ", sens$events[["surgeon_207x"]], " of ", sens$events[["surgeon_found"]], " surgeon-text NPIs and ", sens$events[["apc_207x"]], " of ", sens$events[["apc_found"]], " APC-text NPIs held one. With so few unflagged events the sensitivity estimate is imprecise: it is weighted up by the size of the unflagged strata (about 1.4 million NPIs), so each event in the sample stands for thousands of NPIs and a zero count still leaves room for thousands of missed 207X holders; the lower limit reflects that. Treat sensitivity as a rough magnitude only."),
    "",
    "No PPV or sensitivity threshold was pre-set; these numbers describe measurement error of the text flag and are inputs to the Methods and to any adjustment WP5 pre-specifies. Only fields needed for the tabulation (found, enumeration type, status, taxonomy codes and primary flag) were kept, in CACHE only; no names or addresses were written anywhere."
  )
  writeLines(L, file.path(P$qa, "g3_npi_match.md"))
}

# ---------------------------------------------------------------------
# G3 route (a): NPPES bulk file (runs only if the monthly zip is reachable)
# ---------------------------------------------------------------------
TOOL_NPPES_AWK <- r"---(
BEGIN { FS = "\",\""; nt = split(tcols, tc, ","); split(pcols, pc, ","); while ((getline l < npis) > 0) want[l] = 1 }
{
  gsub(/^"|"$/, "")
  if (!($cn in want)) next
  s = $cn "\t" $ce "\t" $cd
  for (i = 1; i <= nt; i++) s = s "\t" $(tc[i]) ":" $(pc[i])
  print s
}
)---"

# Builds the shell command that streams the main NPPES file out of the zip (never extracted to disk) and
# keeps only the harvested NPIs. header_cols: the column names of the main file. Route (a) is run only if the
# bulk zip is reachable; it was exercised on a toy zip in tests, not on the real file.
nppes_extract_cmd <- function(zip, header_cols, npi_file, out_file, awk_file) {
  idx <- function(nm) match(nm, header_cols)
  tcols <- idx(paste0("Healthcare Provider Taxonomy Code_", 1:15))
  pcols <- idx(paste0("Healthcare Provider Primary Taxonomy Switch_", 1:15))
  cn <- idx("NPI"); ce <- idx("Entity Type Code"); cd <- idx("NPI Deactivation Date")
  if (anyNA(c(cn, ce, cd, tcols, pcols))) stop("NPPES header lacks expected columns", call. = FALSE)
  writeLines(TOOL_NPPES_AWK, awk_file)
  paste0("unzip -p ", shQuote(zip), " 'npidata_pfile_*.csv' -x '*FileHeader*' | awk -v cn=", cn, " -v ce=", ce, " -v cd=", cd,
         " -v tcols=", paste(tcols, collapse = ","), " -v pcols=", paste(pcols, collapse = ","), " -v npis=", shQuote(npi_file),
         " -f ", shQuote(awk_file), " > ", shQuote(out_file))
}

# Parses the NPPES extract (npi, entity type, deactivation date, 15 "code:primary" fields) into per-NPI indicators.
nppes_indicators <- function(ext) {
  codes <- c(TAX_CODES$orthopaedic)
  tc <- as.matrix(ext[, 4:18])
  code_only <- sub(":.*$", "", tc)
  has <- function(re) rowSums(matrix(grepl(re, code_only), nrow = nrow(code_only))) > 0
  out <- data.table(npi = as.numeric(ext[[1]]), matched = ext[[2]] == "1" & (is.na(ext[[3]]) | ext[[3]] == ""),
                    n_ortho_207x_any = has("^207X"), n_derm_207n = has("^207N"), n_ophth_207w = has("^207W"))
  for (cd in codes) set(out, j = paste0("n_", cd), value = rowSums(matrix(code_only == cd, nrow = nrow(code_only))) > 0)
  out[]
}

# Route (a). Streams the main NPPES file from the zip (never extracted), keeps the harvested NPIs, and returns
# per-NPI indicators plus the plan-level counts and the agreement of the text flag with the taxonomy flag.
nppes_route_a <- function(zip, fl, cohort_ids, sig_plans, work = CN) {
  dir.create(work, recursive = TRUE, showWarnings = FALSE)
  hdr_line <- system(paste("unzip -p", shQuote(zip), "'*FileHeader.csv' | head -1"), intern = TRUE)
  hdr <- gsub('"', "", strsplit(hdr_line, ",", fixed = TRUE)[[1]])
  npif <- file.path(work, "harvested_npis.txt"); outf <- file.path(work, "nppes_harvested.tsv")
  writeLines(fmt_npi(fl$npi), npif)
  system(nppes_extract_cmd(zip, hdr, npif, outf, file.path(work, "nppes_extract.awk")))
  ext <- fread(outf, header = FALSE, sep = "\t", quote = "", colClasses = "character", na.strings = NULL)
  ind <- nppes_indicators(ext)
  x <- merge(fl, ind, by = "npi", all.x = TRUE)
  for (cc in setdiff(names(ind), "npi")) x[is.na(get(cc)), (cc) := FALSE]
  match_share <- mean(x$matched)
  text_s <- bitwAnd(x$fam, 1L) > 0L; text_any <- bitwAnd(x$fam, 3L) > 0L; ref <- x$n_ortho_207x_any & x$matched
  agree <- function(flag) {
    f <- flag & x$matched; tp <- sum(f & ref); fp <- sum(f & !ref); fn <- sum(!f & ref & x$matched); tn <- sum(!f & !ref & x$matched)
    n <- tp + fp + fn + tn; po <- (tp + tn) / n; pe <- ((tp + fp) * (tp + fn) + (fn + tn) * (fp + tn)) / n^2
    c(ppv = tp / (tp + fp), sensitivity = tp / (tp + fn), kappa = (po - pe) / (1 - pe))
  }
  inc <- x[sig != ""]
  cols <- c("matched", "n_ortho_207x_any", paste0("n_", TAX_CODES$orthopaedic), "n_derm_207n", "n_ophth_207w")
  sc <- inc[, lapply(.SD, sum), by = sig, .SDcols = cols]
  tot <- sig_plan_sums(sc, cols, sig_plans, length(cohort_ids))
  pc <- as.data.table(tot); setnames(pc, "matched", "n_individual_nppes_matched"); pc[, plan_id := cohort_ids]
  list(match_share = match_share, n_harvested = nrow(x), n_matched = sum(x$matched), plan_counts = pc,
       agreement_surgeon = agree(text_s), agreement_surgeon_or_apc = agree(text_any))
}

# ---------------------------------------------------------------------
# Mappability metrics and the numbers ledger (step 8)
# ---------------------------------------------------------------------
vocab_metrics <- function(agg = file.path(CH, "agg")) {
  all_spec <- readRDS(file.path(agg, "all_spec.rds"))
  npi_spec <- readRDS(file.path(agg, "npi_spec.rds"))
  nrec <- readRDS(file.path(agg, "nrec_spec.rds"))
  mp <- nucc_mappable(all_spec, nucc_keys())
  rec_any <- sum(nrec$N[mp$any_label[nrec$spec_g]]) / sum(nrec$N)
  rec_all <- sum(nrec$N[mp$all_labels[nrec$spec_g]]) / sum(nrec$N)
  npi_spec[, `:=`(ma = mp$any_label[spec_g], mall = mp$all_labels[spec_g])]
  nn <- npi_spec[, .(any_rec = max(as.integer(ma)) > 0L, all_rec = min(as.integer(mall)) > 0L), by = npi]
  list(records_nucc_any_label = rec_any, records_nucc_all_labels = rec_all,
       npis_any_record_nucc = mean(nn$any_rec), npis_all_records_nucc = mean(nn$all_rec))
}

ledger_stage <- function() {
  ledger_reset("ortho")
  SRC <- "04_ortho_harvest.R"
  add <- function(key, value, unit, desc) ledger_add("ortho", key, value, unit, desc, SRC)
  man <- fread(file.path(CH, "state", "manifest.csv"), colClasses = list(character = c("fid", "status")))
  att <- attempts_load(list(ch = CH))
  ex <- fread(file.path(OUT_DIR, "plan_exposure.csv"), colClasses = list(character = c("plan_id", "issuer_id", "state", "exposure_status")))
  st <- readRDS(file.path(CH, "agg", "exposure_stats.rds"))
  g3 <- fread(file.path(OUT_DIR, "npi_match_summary.csv"), colClasses = "character")
  g <- function(k) g3$value[g3$item == k]
  gn <- function(k) as.numeric(g(k))
  vm <- vocab_metrics()
  ns <- fread(file.path(P$logs, "wp4_nppes_start.csv"))
  ne <- fread(file.path(P$logs, "wp4_nppes_end.csv"))
  add("ortho_files_total", nrow(man), "files", "Distinct provider file URLs in the inventory")
  add("ortho_files_ok", sum(man$status == "ok"), "files", "Provider files harvested successfully")
  add("ortho_files_failed", sum(man$status == "failed"), "files", "Provider files that failed after retries (excluded, never partially counted)")
  add("ortho_files_skipped", sum(man$status == "skipped"), "files", "Files skipped because no issuer sharing the index has a computable cohort plan")
  add("ortho_files_not_run", sum(man$status %in% c("pending", "running")), "files", "Files not run (transfer or disk guard)")
  add("ortho_wire_gb", sum(att$wire_bytes) / GBYTES, "GB", "Cumulative wire bytes of all attempts including calibration (decimal GB)")
  add("ortho_decoded_gb", sum(att$decoded_bytes) / GBYTES, "GB", "Cumulative decoded bytes streamed through the filter (decimal GB)")
  add("ortho_wall_clock_hours", as.numeric(difftime(max(as.POSIXct(att$ended[att$fid != "calib"], format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
                                                    min(as.POSIXct(att$started[att$fid != "calib"], format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")), units = "hours")),
      "hours", "Elapsed time from first to last stream attempt")
  isu <- ex[, .(all_ok = all(issuer_n_files > 0L & issuer_n_files_ok == issuer_n_files)), by = issuer_id]
  add("ortho_issuers_harvested", sum(isu$all_ok), "issuers", "Cohort issuers whose provider files were all harvested")
  add("ortho_cohort_plans_listed", sum(ex$plan_in_provider_files), "plans", "Cohort plans with a directory listing in any harvested file")
  add("ortho_cohort_plans_listed_share", mean(ex$plan_in_provider_files), "proportion", "Share of the 2,527 cohort plans with a directory listing")
  add("ortho_cohort_plans_ok", sum(ex$exposure_status == "ok"), "plans", "Cohort plans with exposure status ok")
  add("ortho_cohort_plans_ok_share", mean(ex$exposure_status == "ok"), "proportion", "Share of cohort plans with exposure status ok")
  add("ortho_issuer_states_ok", uniqueN(ex[exposure_status == "ok", paste(issuer_id, state)]), "issuer-states", "Issuer-states with at least one plan with exposure status ok")
  add("ortho_yield_meets_g1", if (uniqueN(ex[exposure_status == "ok", paste(issuer_id, state)]) >= CFG$gates$g1$min_issuer_states &&
                                  sum(ex$exposure_status == "ok") >= CFG$gates$g1$min_plans) "yes" else "no",
      "text", "Yield at least 60 issuer-states and 500 plans with exposure status ok")
  add("ortho_individual_npis", st$n_npis, "NPIs", "Distinct individual NPIs in harvested files (cohort or not)")
  add("ortho_individual_npis_cohort", st$n_npis_cohort, "NPIs", "Distinct individual NPIs listed in at least one cohort plan")
  add("ortho_surgeon_npis", st$n_surgeon, "NPIs", "Distinct NPIs flagged ortho_surgeon by the frozen text crosswalk")
  add("ortho_apc_npis", st$n_apc, "NPIs", "Distinct NPIs flagged ortho_apc (orthopaedic text with a PA, NP, APRN, nurse or assistant co-label)")
  add("ortho_derm_npis", st$n_derm, "NPIs", "Distinct NPIs flagged dermatology by text")
  add("ortho_ophth_npis", st$n_ophth, "NPIs", "Distinct NPIs flagged ophthalmology by text")
  add("ortho_surgeon_npis_cohort", st$n_surgeon_cohort, "NPIs", "ortho_surgeon NPIs listed in at least one cohort plan")
  add("ortho_apc_npis_cohort", st$n_apc_cohort, "NPIs", "ortho_apc NPIs listed in at least one cohort plan")
  add("ortho_derm_npis_cohort", st$n_derm_cohort, "NPIs", "Dermatology-text NPIs listed in at least one cohort plan")
  add("ortho_ophth_npis_cohort", st$n_ophth_cohort, "NPIs", "Ophthalmology-text NPIs listed in at least one cohort plan")
  oks <- ex[exposure_status == "ok"]
  add("ortho_surgeon_per_plan_median", median(oks$n_ortho_surgeon_text), "NPIs per plan", "Median ortho_surgeon NPIs per plan with exposure status ok")
  add("ortho_surgeon_per_plan_min", min(oks$n_ortho_surgeon_text), "NPIs per plan", "Minimum ortho_surgeon NPIs per plan with exposure status ok")
  add("ortho_surgeon_per_plan_max", max(oks$n_ortho_surgeon_text), "NPIs per plan", "Maximum ortho_surgeon NPIs per plan with exposure status ok")
  add("ortho_nppes_reachable_start", if (isTRUE(ns$monthly_zip_reachable[1])) "yes" else "no", "text", "NPPES monthly zip reachable at the start of the harvest")
  add("ortho_nppes_reachable_end", if (isTRUE(ne$monthly_zip_reachable[1])) "yes" else "no", "text", "NPPES monthly zip reachable after the harvest")
  add("ortho_validation_route", "NPI Registry API sample", "text", "G3 route used (b)")
  add("ortho_g3_verdict", g("verdict"), "text", "G3 verdict under the WP4 operationalisation")
  add("ortho_npi_found_share", gn("found_active_share_C1_C2"), "proportion", "Sampled unflagged individual NPIs (C1 and C2) found as active individuals in the NPI Registry")
  add("ortho_npi_found_share_lo", gn("found_active_share_C1_C2_lo"), "proportion", "Wilson 95 percent lower limit")
  add("ortho_npi_found_share_hi", gn("found_active_share_C1_C2_hi"), "proportion", "Wilson 95 percent upper limit")
  add("ortho_ppv_surgeon", gn("ppv_surgeon_any_207x"), "proportion", "Share of surgeon-text NPIs holding any 207X code")
  add("ortho_ppv_surgeon_lo", gn("ppv_surgeon_any_207x_lo"), "proportion", "Wilson 95 percent lower limit")
  add("ortho_ppv_surgeon_hi", gn("ppv_surgeon_any_207x_hi"), "proportion", "Wilson 95 percent upper limit")
  add("ortho_ppv_apc", gn("ppv_apc_any_207x"), "proportion", "Share of APC-text NPIs holding any 207X code")
  add("ortho_sensitivity_surgeon", gn("sens_surgeon"), "proportion", "Estimated sensitivity of the surgeon-text flag for any 207X")
  add("ortho_sensitivity_surgeon_lo", gn("sens_surgeon_lo"), "proportion", "Monte Carlo (Jeffreys) 95 percent lower limit")
  add("ortho_sensitivity_surgeon_hi", gn("sens_surgeon_hi"), "proportion", "Monte Carlo (Jeffreys) 95 percent upper limit")
  for (k in list(c("apc", "ppv_apc_any_207x", "Share of APC-text NPIs holding any 207X code (PPV)", "ortho_ppv_apc"),
                 c("derm", "pp_derm_207n", "Share of derm-text NPIs holding any 207N code (PPV)", "ortho_ppv_derm"),
                 c("ophth", "pp_ophth_207w", "Share of ophth-text NPIs holding any 207W code (PPV)", "ortho_ppv_ophth"),
                 c("apc_phys", "pp_apc_physician", "Share of APC-text NPIs holding a physician taxonomy code (20xxxxxxx)", "ortho_apc_physician_share"),
                 c("apc_pa", "pp_apc_pa", "Share of APC-text NPIs holding a physician assistant code (363A)", "ortho_apc_pa_share"),
                 c("apc_np", "pp_apc_np", "Share of APC-text NPIs holding a nurse practitioner code (363L)", "ortho_apc_np_share"))) {
    base <- if (k[1] == "apc") "ppv_apc_any_207x" else k[2]
    sh <- if (k[1] == "apc") gn("ppv_apc_any_207x") else gn(paste0(k[2], "_share"))
    lo <- if (k[1] == "apc") gn("ppv_apc_any_207x_lo") else gn(paste0(k[2], "_lo"))
    hi <- if (k[1] == "apc") gn("ppv_apc_any_207x_hi") else gn(paste0(k[2], "_hi"))
    if (k[1] != "apc") add(k[4], sh, "proportion", k[3])
    add(paste0(k[4], "_lo"), lo, "proportion", "Wilson 95 percent lower limit")
    add(paste0(k[4], "_hi"), hi, "proportion", "Wilson 95 percent upper limit")
  }
  add("ortho_sensitivity_surgeon_or_apc", gn("sens_surgeon_or_apc"), "proportion", "Estimated sensitivity of the surgeon or APC text flag for any 207X")
  add("ortho_sensitivity_surgeon_or_apc_lo", gn("sens_surgeon_or_apc_lo"), "proportion", "Monte Carlo (Jeffreys) 95 percent lower limit")
  add("ortho_sensitivity_surgeon_or_apc_hi", gn("sens_surgeon_or_apc_hi"), "proportion", "Monte Carlo (Jeffreys) 95 percent upper limit")
  add("ortho_g3_sampled", gn("n_sampled_total"), "NPIs", "NPIs sampled and queried in the NPI Registry validation")
  add("ortho_g3_found_active", gn("n_found_active_total"), "NPIs", "Sampled NPIs found as active individuals (all strata)")
  add("ortho_g3_unflagged_sampled", gn("n_C1_C2"), "NPIs", "Unflagged sampled NPIs (strata C1 and C2)")
  add("ortho_g3_unflagged_found_active", gn("found_active_x_C1_C2"), "NPIs", "Unflagged sampled NPIs found as active individuals")
  add("ortho_g3_unflagged_with_207x", gn("unflagged_207x"), "NPIs", "Unflagged sampled NPIs holding a 207X code (sensitivity events)")
  add("ortho_g3_surgeon_n", gn("pp_surgeon_207x_n"), "NPIs", "Surgeon-text NPIs sampled and found active")
  add("ortho_g3_surgeon_with_207x", gn("pp_surgeon_207x_x"), "NPIs", "Surgeon-text NPIs holding a 207X code")
  add("ortho_g3_apc_n", gn("pp_apc_207x_n"), "NPIs", "APC-text NPIs sampled and found active")
  add("ortho_g3_apc_with_207x", gn("pp_apc_207x_x"), "NPIs", "APC-text NPIs holding a 207X code")
  add("ortho_g3_surgeon_no207x_n", gn("a_no207x_n"), "NPIs", "Surgeon-text NPIs without a 207X code")
  add("ortho_g3_surgeon_no207x_pa", gn("a_no207x_pa"), "NPIs", "Of those, holding a PA code (363A)")
  add("ortho_g3_surgeon_no207x_np", gn("a_no207x_np"), "NPIs", "Of those, holding an NP code (363L)")
  add("ortho_g3_surgeon_no207x_other_physician", gn("a_no207x_other_physician"), "NPIs", "Of those, holding another physician code")
  add("ortho_g3_surgeon_no207x_none", gn("a_no207x_none"), "NPIs", "Of those, holding none of these")
  iss <- fread(file.path(OUT_DIR, "issuer_sources.csv"), colClasses = c(issuer_id = "character"))
  add("ortho_cohort_plans", nrow(ex), "plans", "Cohort plans (computable in-network proportion)")
  add("ortho_cohort_issuers", uniqueN(ex$issuer_id), "issuers", "Cohort issuers")
  add("ortho_issuers_not_retrievable_universe", sum(iss$index_status != "ok"), "issuers", "Issuers of the 185 whose index could not be fetched (12; one of them has no computable plan)")
  add("ortho_issuers_unreachable", uniqueN(ex$issuer_id[ex$exposure_status == "issuer_unreachable"]), "issuers", "Cohort issuers with status issuer_unreachable")
  add("ortho_issuers_incomplete", uniqueN(ex$issuer_id[ex$exposure_status == "issuer_incomplete"]), "issuers", "Cohort issuers with status issuer_incomplete (some provider files not harvested)")
  add("ortho_plans_issuer_unreachable", sum(ex$exposure_status == "issuer_unreachable"), "plans", "Cohort plans with status issuer_unreachable")
  add("ortho_plans_issuer_incomplete", sum(ex$exposure_status == "issuer_incomplete"), "plans", "Cohort plans with status issuer_incomplete")
  add("ortho_plans_not_in_directory", sum(ex$exposure_status == "plan_not_in_directory"), "plans", "Cohort plans absent from every harvested file of a fully harvested issuer")
  add("ortho_plans_no_individuals", sum(ex$exposure_status == "no_individuals"), "plans", "Cohort plans listed only in facility or group records")
  add("ortho_states_ok", uniqueN(ex$state[ex$exposure_status == "ok"]), "states", "States with at least one plan with exposure status ok")
  add("ortho_surgeon_per_plan_p05", as.numeric(quantile(oks$n_ortho_surgeon_text, 0.05)), "NPIs per plan", "5th percentile of ortho_surgeon NPIs per ok plan")
  add("ortho_surgeon_per_plan_p95", as.numeric(quantile(oks$n_ortho_surgeon_text, 0.95)), "NPIs per plan", "95th percentile of ortho_surgeon NPIs per ok plan")
  sshare <- oks$n_ortho_surgeon_text / oks$n_individual_npis
  add("ortho_surgeon_share_median", median(sshare), "proportion", "Median share of listed clinicians that are ortho_surgeon, ok plans")
  add("ortho_surgeon_share_min", min(sshare), "proportion", "Minimum surgeon share, ok plans")
  add("ortho_surgeon_share_max", max(sshare), "proportion", "Maximum surgeon share, ok plans")
  add("ortho_apc_per_plan_median", median(oks$n_ortho_apc_text), "NPIs per plan", "Median ortho_apc NPIs per ok plan")
  add("ortho_derm_per_plan_median", median(oks$n_derm_text), "NPIs per plan", "Median dermatology-text NPIs per ok plan")
  add("ortho_ophth_per_plan_median", median(oks$n_ophth_text), "NPIs per plan", "Median ophthalmology-text NPIs per ok plan")
  okm <- man[status == "ok"]
  add("ortho_individual_records", sum(okm$individual_records), "records", "Individual records read in harvested files")
  add("ortho_facility_records_ignored", sum(okm$facility_records), "records", "FACILITY records ignored")
  add("ortho_group_records_ignored", sum(okm$group_records), "records", "GROUP records ignored (type not in the published schema)")
  add("ortho_not_run_listed_gb", sum(man$listed_bytes[man$status %in% c("pending", "running")], na.rm = TRUE) / GBYTES, "GB", "Listed (decoded) size of the files not run")
  add("ortho_manifest_wire_gb", sum(man$wire_bytes_all_attempts, na.rm = TRUE) / GBYTES, "GB", "Wire bytes of all attempts summed from the manifest (excludes the calibration downloads)")
  add("ortho_share_npis_mapped_ortho", (st$n_surgeon + st$n_apc) / st$n_npis, "proportion", "Share of individual NPIs mapped to an orthopaedic family (surgeon or APC)")
  add("ortho_share_npis_mapped_derm", st$n_derm / st$n_npis, "proportion", "Share of individual NPIs mapped to dermatology")
  add("ortho_share_npis_mapped_ophth", st$n_ophth / st$n_npis, "proportion", "Share of individual NPIs mapped to ophthalmology")
  add("ortho_share_records_nucc_exact", vm$records_nucc_any_label, "proportion", "Share of individual records with at least one label exactly equal to a NUCC name (WP3 metric, 0.55 in the WP3 samples)")
  add("ortho_share_npis_nucc_exact_any_record", vm$npis_any_record_nucc, "proportion", "Share of individual NPIs with at least one NUCC-exact record")
  add("ortho_share_npis_nucc_exact_all_records", vm$npis_all_records_nucc, "proportion", "Share of individual NPIs whose every label of every record equals a NUCC name")
  p <- ledger_write("ortho")
  lg("ledger written: ", p, step = "ledger")
  invisible(p)
}

# ---- (parts below are appended; do not delete this marker) -----------
# ==== END OF PARTS ====

# ---------------------------------------------------------------------
# Dispatch
# ---------------------------------------------------------------------
if (!exists("WP4_NO_MAIN", inherits = TRUE) && !interactive()) {
  stage <- commandArgs(trailingOnly = TRUE)
  arg <- if (length(stage) > 1L) stage[-1L] else character(0)
  stage <- if (length(stage)) stage[1] else "help"
  switch(stage,
    tools = write_tools(),
    nppes_start = nppes_check("start"),
    nppes_end = nppes_check("end"),
    prep = prep_stage(),
    calibrate = calibrate_stage(),
    calib_account = account_calibration(),
    run = {
      ft <- read_file_table()
      if (length(arg) && arg[1] == "retry") harvest_supervisor(new_ctx(), ft, retry_only = TRUE)
      else if (length(arg) && arg[1] == "retry_extra") harvest_supervisor(new_ctx(max_attempts = MAX_ATTEMPTS + 1L), ft, retry_only = TRUE)
      else harvest_supervisor(new_ctx(), ft, max_launch = if (length(arg)) as.numeric(arg[1]) else Inf)
    },
    repair_manifest = {
      ft <- read_file_table()[order(run_order)]; ctx <- new_ctx()
      man <- manifest_repair_bytes(manifest_load(ctx, ft), attempts_load(ctx))
      write_atomic_csv(man, file.path(ctx$ch, "state", "manifest.csv")); export_manifest(man)
      print(man[, .(files = .N, wire_gb_all_attempts = sum(wire_bytes_all_attempts) / 1e9, na_bytes_ok = sum(status == "ok" & (is.na(wire_bytes) | is.na(decoded_bytes)))), by = status])
    },
    taxonomy = taxonomy_stage(),
    aggregate = aggregate_stage(),
    vocab = vocab_stage(),
    freeze = freeze_stage(),
    exposure = exposure_stage(),
    g3sample = g3_sample_stage(),
    g3run = g3_run_stage(if (length(arg)) as.numeric(arg[1]) else Inf),
    g3report = g3_report_stage(),
    ledger = ledger_stage(),
    pilot = { pp <- pilot_projection(); fwrite(pp, file.path(P$logs, "wp4_pilot_projection.csv")); print(t(pp)) },
    stop("unknown stage: ", stage, call. = FALSE))
}
