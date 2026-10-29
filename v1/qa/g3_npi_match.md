G3 VALIDATION: NPI MATCH AND TEXT-FLAG MEASUREMENT ERROR (WP4)

Route used: b (NPI Registry API on a stratified random sample), because the NPPES bulk zip was not reachable at the start of the harvest (see v1/logs/wp4_nppes_start.csv and wp4_nppes_end.csv). Verdict: PASS.

Verdict rule: the task spec gives PASS if at least 95 percent of the sampled unflagged individual NPIs (strata C1 and C2) are found as active individuals, PASS-CONDITIONAL if the share is lower but the unmatched are explained, and FAIL otherwise. WP4 operationalised "explained" with a numeric bound of its own, which the spec does not give: PASS-CONDITIONAL requires that at most 5 percent of those NPIs are absent from the registry altogether (the rest then carry a stated reason in the registry record: organisation NPI or inactive status).

Found as active individual (strata C1 and C2, 500 NPIs): 99.4 percent (Wilson 95 percent limits 98.2 percent to 99.8 percent); C1 alone 99.5 percent, C2 alone 99.0 percent. Share of C1 and C2 NPIs absent from the registry altogether: 0.4 percent.

Sample design (seed from config study.seed; frame = individual NPIs listed in at least one cohort plan, classified under the frozen crosswalk): A 300 surgeon-text NPIs, B 100 APC-text NPIs, C1 400 random unflagged, C2 100 unflagged with an ambiguous term (surgery, sports, hand, spine, trauma, musculoskeletal, joint), D 50 derm-text and 50 ophth-text; a stratum with fewer NPIs contributes all of them. At most one request per second, back-off on 429 or 5xx. Frame sizes:

- A:    19,720
- B:     2,185
- U_all: 1,411,003
- U_ambiguous:    54,323
- D_derm:     9,587
- D_ophth:    13,957

Results by stratum (found = result_count at least 1 and enumeration type NPI-1 and status A):

- A: sampled 300, queried 300, found active 300, not in registry 0, organisation NPI 0, inactive 0
- B: sampled 100, queried 100, found active 100, not in registry 0, organisation NPI 0, inactive 0
- C1: sampled 400, queried 400, found active 398, not in registry 1, organisation NPI 1, inactive 0
- C2: sampled 100, queried 100, found active 99, not in registry 1, organisation NPI 0, inactive 0
- D_derm: sampled 50, queried 50, found active 50, not in registry 0, organisation NPI 0, inactive 0
- D_ophth: sampled 50, queried 50, found active 50, not in registry 0, organisation NPI 0, inactive 0

Positive predictive value against taxonomy codes (any slot), among sampled NPIs found as active individuals, Wilson 95 percent limits:

- PPV surgeon text flag (A) for any 207X among found active: 255 of 300 = 85.0 percent (80.5 percent to 88.6 percent)
- PPV APC text flag (B) for any 207X among found active: 6 of 100 = 6.0 percent (2.8 percent to 12.5 percent)
- Stratum B holding a physician taxonomy (20xxxxxxx): 9 of 100 = 9.0 percent (4.8 percent to 16.2 percent)
- Stratum B holding a physician assistant taxonomy (363A): 79 of 100 = 79.0 percent (70.0 percent to 85.8 percent)
- Stratum B holding a nurse practitioner taxonomy (363L): 18 of 100 = 18.0 percent (11.7 percent to 26.7 percent)
- PPV derm text flag for any 207N among found active: 46 of 50 = 92.0 percent (81.2 percent to 96.8 percent)
- PPV ophth text flag for any 207W among found active: 49 of 50 = 98.0 percent (89.5 percent to 99.6 percent)

Sensitivity of the text flags for any 207X code, from the unflagged strata weighted back to the frame by stratum size (point estimate = median and limits = 2.5th and 97.5th percentiles of 2,000 Monte Carlo draws of each stratum proportion from its Jeffreys Beta posterior; used instead of a bootstrap because a stratum with zero events makes the bootstrap degenerate): surgeon-text flag 93.0 percent (64.7 percent to 99.1 percent); surgeon or APC text 93.7 percent (65.1 percent to 99.8 percent).

How few events there are: among 497 unflagged NPIs found active, 0 held a 207X code; 255 of 300 surgeon-text NPIs and 6 of 100 APC-text NPIs held one. With so few unflagged events the sensitivity estimate is imprecise: it is weighted up by the size of the unflagged strata (about 1.4 million NPIs), so each event in the sample stands for thousands of NPIs and a zero count still leaves room for thousands of missed 207X holders; the lower limit reflects that. Treat sensitivity as a rough magnitude only.

No PPV or sensitivity threshold was pre-set; these numbers describe measurement error of the text flag and are inputs to the Methods and to any adjustment WP5 pre-specifies. Only fields needed for the tabulation (found, enumeration type, status, taxonomy codes and primary flag) were kept, in CACHE only; no names or addresses were written anywhere.
