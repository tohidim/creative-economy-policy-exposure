# Creative Firms and Cultural Policy Exposure

Statistical background to Chapter 4 of the ITC *SME Competitiveness Outlook 2027*
(forthcoming). It links a semantic-analysis measure of what national cultural policies focus on
to the outcomes of creative-industry firms in 72 countries.

**See the work**

- [Creative Economy Policy Explorer](https://tohidim.github.io/creative-economy-policy-exposure/): interactive map,
  country profiles and cross-country comparisons of creative firms (source in `docs/`)
- [Working paper (PDF)](paper/wipo_sample/working_paper.pdf): descriptive evidence prepared for the
  report (bar charts) and, in the appendix, the regression evidence behind the report's technical appendix

Author: Hossein Tohidimehr

## What it does

1. **Scores policy texts.** 8,155 policy measures reported by 134 countries to the UNESCO
   2005 Convention monitoring platform are embedded with a multilingual sentence-transformer
   (`paraphrase-multilingual-MiniLM-L12-v2`) and compared with written definitions of six
   channels: intellectual property; brand development; market access; digital capabilities;
   finance; sustainability and certification.
2. **Finds creative firms.** ITC's creative-industry ISIC list (Rev. 3.1, 4 and 5) is applied
   to the World Bank Enterprise Surveys: 10,493 creative firms, 72 countries, surveyed 2021+.
3. **Links and compares.** Each firm is matched to its country's latest UNESCO report before
   the survey (at most four years old). Creative firms in above- and below-median exposure
   country-waves are compared, raw and after controls (weighted OLS, country-clustered SEs).
   Results are associations, not causal effects.

## Repository map

| Folder | Contents |
|---|---|
| `code/semantic/python/` | Policy-text cleaning, chunking and six-channel scoring |
| `code/empirical/stata/` | `reproduce_chapter4_main.do`: sample, descriptives and regressions in one file |
| `code/empirical/R/` | Earlier report figures and the full outcome scan |
| `code/wipo_sample/` | Figures and LaTeX tables for the PDFs, built from the Stata CSVs |
| `data/semantic_scores/` | Channel definitions and policy scores (record, long and country-year) |
| `data/mappings/` | Creative-industry ISIC codes and country/channel crosswalks |
| `output/tables/` | Stata results (CSV) |
| `paper/wipo_sample/` | Working paper: LaTeX source, figures, tables and PDF |
| `docs/` | Explorer web page, its aggregated data (JS and CSV) and social media cards |
| `code/dashboard/` | Builds the explorer data and the social media cards |

## Reproduce

Inputs not stored here (all public):

- WBES comprehensive microdata: <https://www.enterprisesurveys.org> → save as
  `data/raw/wbes/New_Comprehensive_April_29_2026.dta`
- UNESCO policy records: <https://www.unesco.org/creativity/en/policy-monitoring-platform> →
  `data/raw/project_raw/dce002.csv` (only needed to re-score the texts)
- World Development Indicators (GDP per capita, population, electricity access) →
  `data/external/WB_WDI.dta`

```bash
python3 code/semantic/python/run_unesco_smeco2027_six_channel_scores_transformer.py  # optional
stata-mp -b do code/empirical/stata/reproduce_chapter4_main.do
Rscript code/wipo_sample/make_wipo_figures.R
Rscript code/wipo_sample/make_wipo_tables.R
Rscript code/dashboard/build_dashboard_data.R
Rscript code/dashboard/make_social_cards.R
cd paper/wipo_sample && latexmk -xelatex working_paper.tex
```

Tools: Python (sentence-transformers, NumPy), Stata 17+, R (ggplot2), Plotly, XeLaTeX.
