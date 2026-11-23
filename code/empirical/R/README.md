# Active R reproduction route

This folder contains only the R scripts needed for the current
outputs:

1. `run_chapter4_main_analysis.R` rebuilds the empirical sample and
   reproduces the canonical analysis tables.
2. `run_chapter4_report_and_scan.R` runs the exact-sample full
   regression scan and updates the HTML report.
3. `render_chapter4_report.R`
   is the report renderer called by the preceding script.

Run from the project root:

```text
Rscript code/empirical/R/run_chapter4_main_analysis.R
Rscript code/empirical/R/run_chapter4_report_and_scan.R
```

The second command also produces the complete regression workbook under
`output/reports/chapter4_stata_aligned_full_regression_scan.xlsx`.

`make_figures_2_3_policy_coverage.R` builds main-text Figures 2 and 3.

Superseded report variants and audit scripts were moved to
`_archive_2026-11-06/reproduction_bundle_leftovers/99_archive/` (not part of the
active reproduction path). The few inputs the report script still needed from
that archive now live in `data/report_inputs/`.
