# Appendix tables (LaTeX) for the WIPO work sample, generated from the Stata results.
# Run from the project root: Rscript code/wipo_sample/make_wipo_tables.R
suppressPackageStartupMessages(library(dplyr))
out <- "paper/wipo_sample/tables"; dir.create(out, recursive = TRUE, showWarnings = FALSE)
reg <- read.csv("output/wipo_sample_20261120/figures/figure_data_regressions.csv")
reg$label <- gsub("%", "\\%", reg$label, fixed = TRUE)
families <- c("Overall policy count", "Intellectual property", "Brand development and positioning",
  "Market leverage and access", "Digital capabilities and commerce",
  "Finance and financial capability", "Sustainability, standards and certification")
codes <- c(h2="h2", any_exports="d3b, d3c", h5="h5", r8="r8", e33="e33", f1="f1", d40a="d40a",
  d14a="d14a", k35="k35", l11a="l11a", finance_obstacle_any="k30", k4="k4", k21="k21", c43="c43", ge8d="ge8d")
num <- function(x, d = 1) formatC(x, format = "f", digits = ifelse(abs(x) < 0.05 & x != 0, 2, d), big.mark = ",")
sgn <- function(x) ifelse(x > 0, paste0("$+$", num(x)), paste0("$-$", num(abs(x))))
unit <- function(u) ifelse(u == "days", "days", "pp")
pfmt <- function(p) ifelse(p < 0.001, "$<$0.001", formatC(p, format = "f", digits = 3))
stars <- function(p) ifelse(p < .01, "$^{***}$", ifelse(p < .05, "$^{**}$", ifelse(p < .10, "$^{*}$", "")))
L <- c("\\begin{tabular}{@{}l r r c r r r@{}}", "\\toprule",
       "Outcome & Estimate & Std.\\ error & 90\\% CI & $p$ & Firms & Countries \\\\", "\\midrule")
for (fam in families) {
  r <- reg[reg$family == fam, ]
  L <- c(L, sprintf("\\multicolumn{7}{@{}l}{\\textbf{%s}} \\\\", fam))
  for (i in seq_len(nrow(r))) with(r[i, ], {
    L <<- c(L, sprintf("\\quad %s & %s %s%s & %s & [%s, %s] & %s & %s & %d \\\\",
      label, sgn(est), unit(unit), stars(p_value), num(se), gsub("^-", "\\$-\\$", num(lo)), gsub("^-", "\\$-\\$", num(hi)), pfmt(p_value),
      formatC(n, big.mark = ",", format = "d"), countries))
  })
  if (fam != tail(families, 1)) L <- c(L, "\\addlinespace")
}
L <- c(L, "\\bottomrule", "\\end{tabular}")
writeLines(L, file.path(out, "tab_regressions.tex"))
# Variable definitions
d <- unique(reg[, c("outcome", "label", "unit")])
V <- c("\\begin{tabular}{@{}l l l@{}}", "\\toprule", "Outcome (as labelled in figures) & WBES item & Unit in tables \\\\", "\\midrule")
for (i in seq_len(nrow(d))) V <- c(V, sprintf("%s & \\texttt{%s} & %s \\\\", d$label[i], gsub("_", "\\\\_", codes[[d$outcome[i]]]),
  c(share = "share of firms; differences in pp", pct = "percent; differences in pp", days = "days")[[d$unit[i]]]))
V <- c(V, "\\bottomrule", "\\end{tabular}")
writeLines(V, file.path(out, "tab_variables.tex"))
cat("Tables written to", out, "\n")
