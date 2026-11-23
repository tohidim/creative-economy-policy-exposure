# Writes paper/wipo_sample/tables/channel_definitions.tex (Appendix B of the working paper)
# from the channel definitions used for semantic scoring.
# Run from the project root: Rscript code/wipo_sample/make_channel_definitions.R
d <- read.csv("data/semantic_scores/smeco2027_six_channels_transformer/smeco2027_six_channel_definitions.csv")
esc <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x)
  gsub("\\s+", " ", trimws(x))
}
out <- unlist(lapply(seq_len(nrow(d)), function(i) c(
  sprintf("\\subsection*{B.%d\\quad %s}", i, esc(d$channel_label[i])),
  sprintf("\\emph{%s}", esc(d$role_in_theory[i])), "",
  esc(d$refined_definition[i]), "",
  sprintf("\\textbf{Terms that signal the channel:} %s.", esc(d$include_when_text_mentions[i])), "",
  sprintf("\\textbf{Excluded or down-weighted:} %s.", esc(d$exclude_or_downweight_when_text_mentions[i])), "")))
writeLines(out, "paper/wipo_sample/tables/channel_definitions.tex")
cat("Wrote", nrow(d), "channel definitions\n")
