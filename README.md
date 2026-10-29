# Post-service claim denial and orthopaedic network density in Marketplace plans

Analysis code, the frozen statistical analysis plan, derived plan-level data, results and figures for an ecological, plan-level cross-sectional study of 2024 in-network post-service claim denial in Individual-market qualified health plans (CMS plan year 2026 Transparency in Coverage public use file) in relation to orthopaedic surgeon density in issuers' published provider directories.

The repository holds no patient data, no clinician names or addresses, and no manuscript files. Everything here is derived from public sources (see Data sources).

## Result in one paragraph

No association was detected between orthopaedic surgeon network density and in-network claim denial: odds ratio 0.956 per doubling of density (95% CI 0.848 to 1.077; 1,681 plans from 135 issuers), at fixed reported enrollment and within issuer. The estimate is imprecise because few issuers carry the identifying variation (33 informative issuers; effective number about 6). The outcome covers all specialties, so the study cannot estimate an orthopaedic denial rate. All estimates are in `v1/results/model_estimates.csv`; the sensitivity analyses are in `v1/results/model_sensitivity_rows.csv`.

## Read this before using the exposure

- Density is the number of orthopaedic surgeons listed in a plan's directory network per 10,000 reported average monthly enrollees. It is a relative measure, not a population density: it divides a directory-wide count by a single plan's reported enrollment, which looks about ten times too small relative to claim volume.
- Orthopaedic surgeons are identified from free-text specialty labels with a crosswalk fixed before any outcome was joined (`v1/config/specialty_text_crosswalk.csv`). Against NPI Registry taxonomy codes in a stratified sample, 85% of flagged NPIs held an orthopaedic surgery code (`v1/qa/g3_npi_match.md`).
- Directory files were retrieved in October 2026 for claims of 2024. Eleven issuers' files could not be retrieved and one insurer's files are incomplete, so exposure is available for 2,175 of 2,527 cohort plans (78.8% of cohort claim lines).
- The ecological design cannot support causal claims.

## Layout

All code assumes the folder `v1/` as the project root (the scripts find it from `v1/config/study_config.yaml`).

| Folder | Contents |
|---|---|
| `v1/R/` | numbered analysis scripts `00` to `08` plus helpers (`ledger.R` writes and merges the numbers ledger) |
| `v1/config/` | study configuration, the frozen analysis plan (`sap_frozen.md`), design constants, specialty crosswalk, NUCC taxonomy codes used, variable codebook |
| `v1/data_clean/cms/` | parsed CMS plan and issuer tables (public aggregates) |
| `v1/data_clean/ortho/` | plan-level directory exposure counts, harvest manifest, specialty label vocabulary, NPI Registry validation summary (counts only) |
| `v1/results/` | descriptive and model outputs, `ledger_parts/` and the merged `numbers_ledger.csv` (every reported number with its key) |
| `v1/figures/`, `v1/tables/` | Figures 1 to 3 (vector PDF, 600 dpi PNG and TIFF) and Tables 1 to 3 plus two supplementary tables (CSV and RDS) |
| `v1/qa/` | reconciliation with the KFF benchmarks and the NPI Registry match report |
| `v1/tests/` | tests that run on the published files |
| `v1/logs/` | analysis plan hash records and the few logs the tests read |

## Internal labels

The code, tests and notes were produced in numbered pipeline stages. Labels such as WP0 to WP7, "the lead" and "the user" refer to those stages and to the study author. They carry no other meaning.

## Frozen analysis plan

`v1/config/sap_frozen.md` was written before any model combined exposure and outcome. Its SHA-256 and those of the design constants and the two frozen directory files are in `v1/logs/sap_hash.txt`. The plan was frozen once and re-frozen once before any outcome analysis; the superseded hash is in `v1/logs/sap_hash_history.txt`. `v1/R/06_models.R` stops if the hash does not match. The plan file is hash-frozen, so its internal wording is left as written.

## Reproducing

R 4.6.0. Packages: data.table, ggplot2, patchwork, ragg, systemfonts, readr, readxl, jsonlite, yaml, digest, xml2, sandwich, lme4, flextable, officer. Figures use Arial.

Run from `v1/`:

```bash
Rscript R/01_download_cms.R      # downloads the CMS public use files (zips of a few MB)
Rscript R/02_parse_cms.R         # parses them into data_clean/cms
Rscript R/03_ortho_spike.R       # sizes the provider-file sources (optional)
Rscript R/04_ortho_harvest.R run # streams issuer provider files; about 63 GB of transfer and hours of run time. The result is already in data_clean/ortho, so this step can be skipped.
Rscript R/05_descriptives.R
# R/05b_sap_checks.R wrote the exposure-only diagnostics in results/sap_check_*.csv (included). It needs logs that are not published and makes one live HEAD request, so it is not expected to run here.
Rscript R/06_models.R
Rscript R/07a_figures_desc.R
Rscript R/07b_figures_models.R
Rscript R/08_merge_ledger.R
```

Tests that run on the published files: `tests/test_helpers_tables.R`, `test_sap.R`, `test_desc.R` and `test_models.R`. `tests/test_cms_parse.R` needs the raw files from step 1, and `tests/test_ortho.R` needs harvest logs that are not published. The ledger-merge helper expects every ledger part to be present; `ledger_text.csv` holds derived text quantities from the manuscript build, which is not published here.

`R/helpers_docx.R` is a subset: only the number-formatting and placeholder-rendering functions the figure and table scripts use. The manuscript builder is not part of this repository.

## Data sources

- Centers for Medicare and Medicaid Services, Marketplace public use files, plan year 2026 (Transparency in Coverage; Plan Attributes; Machine-readable URL): https://www.cms.gov/marketplace/resources/data/public-use-files. U.S. federal public data. Retrieval times and SHA-256 hashes are in `v1/data_raw/provenance/manifest.csv`; the raw files themselves are not redistributed.
- Issuer provider files published for CMS (QHP provider and formulary API schema), retrieved October 2026; only NPI-level counts and specialty label vocabulary were kept.
- NPI Registry API (https://npiregistry.cms.hhs.gov/api/) for validation of a stratified sample; only taxonomy-code tabulations were kept.
- KFF, claims denials and appeals in ACA Marketplace plans in 2024, used as the replication benchmark (`v1/qa/g2_reconciliation.md`).

See `NOTICE` for third-party notices.

## License and citation

Code and derived data: MIT License (`LICENSE`). To cite, use `CITATION.cff`; a Zenodo DOI is added after the first release.
