# Chapter 4 Stata reproduction

The main do-file is [reproduce_chapter4_main.do](reproduce_chapter4_main.do).
It starts from the raw WBES microdata and performs the cleaning, creative-firm
classification, outcome construction, controls, semantic-score matching,
country-normalized weights, descriptive means, and fixed-effect regressions in
Stata.

The only substantive analysis data imported from outside Stata is the single
hybrid six-channel country-year semantic-score CSV. The do-file does not read
the RDS analysis contexts, the R bridge script, or R-generated expected
results. The small Excel workbook `data/mappings/chapter4_crosswalks.xlsx`
contains the country-code, country-name, region, and channel-name lookups used
by the do-file. It uses only official Stata commands; `reghdfe` is not required.

Run from the project root:

```text
stata-mp -b do code/empirical/stata/reproduce_chapter4_main.do
```

The script creates:

- `output/tables/chapter4_stata_descriptives.csv`: high/low weighted means and firm counts.
- `output/tables/chapter4_stata_regressions.csv`: coefficients, clustered standard errors,
  p-values, and model sample counts.

The sample rule is deliberately visible in the do-file: WBES firms surveyed
from 2021 onward, in the 72-country mapping used by the report, with either a
direct Rev.4 ITC creative-industry code or a Rev.3.1 code that maps to one, and
a positive WBES weight. Each firm is matched by ISO2 to the latest completed
UNESCO report strictly before the survey year, within a four-year window. High
exposure is a strictly-above-median split across distinct country-waves.

The two editable Excel workbooks in `output/tables/` are refreshed from these
Stata CSV files: `chapter4_descriptive_figures.xlsx` and
`chapter4_regression_figures.xlsx`.

When the project is run from the synced Google Drive folder, the do-file first
copies the large WBES `.dta` file to Stata's local temporary directory. This
avoids a Google Drive random-access error; the raw file itself is not modified.
