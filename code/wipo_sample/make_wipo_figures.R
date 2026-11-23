# Figures for the WIPO work sample (Chapter 4 main text + appendix).
# Reads only saved results:
#   output/tables/chapter4_stata_descriptives.csv   (weighted means, Stata)
#   output/tables/chapter4_stata_regressions.csv    (adjusted coefficients, Stata)
#   output/reports/chapter4_stata_aligned_full_regression_scan.xlsx (outcome SDs)
#   data/semantic_scores/.../unesco_smeco2027_six_channel_scores_long.csv
#   output/figures/figure_2_*.csv, figure_3_*.csv
# Run from the project root:  Rscript code/wipo_sample/make_wipo_figures.R

suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tidyr); library(patchwork); library(scales)
})

out_dir <- "output/wipo_sample_20261120/figures"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Design tokens ---------------------------------------------------------
col_high  <- "#2a78d6"   # above-median exposure (focus series)
col_low   <- "#a3a29b"   # below-median exposure (context, neutral)
ink       <- "#0b0b0b"
ink_2     <- "#52514e"
ink_3     <- "#8a8984"
grid_col  <- "#e6e5e1"
base_font <- "Helvetica Neue"

theme_ws <- function(size = 9) {
  theme_minimal(base_size = size, base_family = base_font) +
    theme(
      text = element_text(colour = ink),
      axis.text = element_text(colour = ink_2, size = size - 1),
      axis.title = element_text(colour = ink_2, size = size - 1),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(colour = grid_col, linewidth = 0.3),
      strip.text = element_text(colour = ink, face = "bold", hjust = 0, size = size),
      plot.title = element_text(face = "bold", size = size + 1, hjust = 0),
      plot.subtitle = element_text(colour = ink_2, size = size - 0.5, hjust = 0),
      plot.title.position = "plot",
      legend.position = "top", legend.justification = "left",
      legend.title = element_blank(), legend.text = element_text(colour = ink_2),
      plot.margin = margin(4, 6, 4, 4)
    )
}

save_fig <- function(p, name, w, h) {
  # PNG at 400 dpi via ragg (system fonts render reliably; cairo_pdf is unavailable here).
  ggsave(file.path(out_dir, paste0(name, ".png")), p, width = w, height = h, dpi = 400, bg = "white",
         device = ragg::agg_png)
}

# ---- Outcome dictionary ----------------------------------------------------
outcomes <- tribble(
  ~outcome,               ~label,                                        ~unit,   ~scale,
  "h2",                   "New-to-market products or services",          "share", 100,
  "any_exports",          "Any direct or indirect exports",              "share", 100,
  "h5",                   "Introduced an improved process",              "share", 100,
  "r8",                   "Performance bonuses for production targets",  "share", 100,
  "e33",                  "Pricing power",                                "share", 100,
  "f1",                   "Capacity utilization (%)",                     "pct",   1,
  "d40a",                 "Import clearance, all border agencies (days)", "days",  1,
  "d14a",                 "Import customs clearance (days)",              "days",  1,
  "k35",                  "Days to receive e-payments",                   "days",  1,
  "l11a",                 "Production workers trained (%)",               "pct",   1,
  "finance_obstacle_any", "Reports any finance obstacle",                 "share", 100,
  "k4",                   "Purchased fixed assets",                       "share", 100,
  "k21",                  "Externally certified financial statements",    "share", 100,
  "c43",                  "On-site solar panels",                         "share", 100,
  "ge8d",                 "Energy-management measures",                   "share", 100
)

families <- c(
  "Overall policy count", "Intellectual property", "Brand development and positioning",
  "Market leverage and access", "Digital capabilities and commerce",
  "Finance and financial capability", "Sustainability, standards and certification"
)

desc <- read.csv("output/tables/chapter4_stata_descriptives.csv") |>
  left_join(outcomes, by = "outcome") |>
  mutate(value = weighted_mean * scale)

reg <- read.csv("output/tables/chapter4_stata_regressions.csv") |>
  left_join(outcomes, by = "outcome") |>
  mutate(
    est = coefficient * scale, se = std_error * scale,
    lo = est - 1.645 * se, hi = est + 1.645 * se,
    unit_lab = ifelse(unit == "days", " days", " pp"),
    stars = case_when(p_value < .01 ~ "***", p_value < .05 ~ "**", p_value < .10 ~ "*", TRUE ~ ""),
    est_lab = paste0(ifelse(est > 0, "+", "−"), formatC(abs(est), format = "f", digits = 1), unit_lab, stars)
  )

# Outcome SDs (same canonical specification) for standardized effects.
scan <- readxl::read_excel("output/reports/chapter4_stata_aligned_full_regression_scan.xlsx", "All regressions")
sds <- scan |>
  filter(spec_id == "wb_region_with_log_employment_no_domestic", !is.na(model_weighted_sd)) |>
  mutate(outcome = sub("^wbes__", "", outcome)) |>
  group_by(outcome) |> summarise(sd = first(model_weighted_sd), .groups = "drop")
reg <- reg |> left_join(sds, by = "outcome") |>
  mutate(z = coefficient / sd, z_lo = (coefficient - 1.645 * std_error) / sd,
         z_hi = (coefficient + 1.645 * std_error) / sd)
stopifnot(!any(is.na(reg$sd)))
write.csv(reg, file.path(out_dir, "figure_data_regressions.csv"), row.names = FALSE)
write.csv(desc, file.path(out_dir, "figure_data_descriptives.csv"), row.names = FALSE)

# ---- Figure 1: distribution of channel attention ---------------------------
long <- read.csv("data/semantic_scores/smeco2027_six_channels_transformer/unesco_smeco2027_six_channel_scores_long.csv")
meds <- long |> group_by(channel_title) |> summarise(med = median(bounded_score), n = n(), .groups = "drop") |>
  arrange(med)
long$channel_title <- factor(long$channel_title, levels = meds$channel_title)
meds$channel_title <- factor(meds$channel_title, levels = meds$channel_title)

p1 <- ggplot(long, aes(bounded_score, channel_title)) +
  geom_violin(fill = col_high, alpha = 0.18, colour = col_high, linewidth = 0.3, scale = "width", width = 0.8) +
  geom_boxplot(width = 0.14, outlier.shape = NA, fill = "white", colour = ink_2, linewidth = 0.3) +
  geom_point(data = meds, aes(med, channel_title), colour = col_high, size = 1.8) +
  geom_text(data = meds, aes(med, channel_title, label = formatC(med, format = "f", digits = 2)),
            nudge_y = 0.32, size = 2.7, colour = ink, family = base_font) +
  scale_x_continuous(limits = c(0.45, 0.9), breaks = seq(0.5, 0.9, 0.1)) +
  labs(x = "Semantic alignment score (0–1, higher = closer to the channel definition)", y = NULL,
       title = "Policy texts align most with sustainability and least with market access",
       subtitle = "Score distribution across 8,155 UNESCO policy records; dots and labels mark medians") +
  theme_ws()
save_fig(p1, "fig01_channel_attention", 6.3, 3.4)

# ---- Figure 2: policy observations by income group -------------------------
f2 <- read.csv("output/figures/figure_2_policy_observations_by_income.csv")
f2$ylab <- sprintf("%s\n%d countries, %d country-years", f2$income_group, f2$countries, f2$country_years)
f2$ylab <- factor(f2$ylab, levels = rev(f2$ylab[match(c("Low income", "Middle income", "High income"), f2$income_group)]))
p2 <- ggplot(f2, aes(average_policy_count, ylab)) +
  geom_col(fill = col_high, width = 0.55) +
  geom_text(aes(label = sprintf("%.1f", average_policy_count)), hjust = -0.15, size = 3, family = base_font, colour = ink) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(x = "Average UNESCO policy records per reporting country-year", y = NULL,
       title = "Richer countries record more cultural policies",
       subtitle = "A coverage and reporting-capacity signal, not a measure of policy quality") +
  theme_ws() + theme(panel.grid.major.x = element_blank(), axis.text.x = element_blank())
save_fig(p2, "fig02_income_groups", 6.3, 2.5)

# ---- Figure 3: policy volume over time -------------------------------------
f3 <- read.csv("output/figures/figure_3_policy_volume_over_time.csv") |>
  mutate(cycle = ifelse(year %in% c(2012, 2016, 2020, 2024), "Main quadrennial reporting year", "Other year"))
p3 <- ggplot(f3, aes(factor(year), average_policy_count, fill = cycle)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.0f", average_policy_count)), vjust = -0.4, size = 2.6, family = base_font, colour = ink) +
  geom_text(aes(y = 0, label = paste0("n=", countries)), vjust = 1.6, size = 2.2, family = base_font, colour = ink_3) +
  scale_fill_manual(values = c("Main quadrennial reporting year" = col_high, "Other year" = "#a9c8ef")) +
  scale_y_continuous(expand = expansion(mult = c(0.08, 0.1))) +
  labs(x = NULL, y = "Average records per reporting country",
       title = "Recorded policy volume has risen across reporting cycles",
       subtitle = "Average UNESCO policy records per reporting country; n = reporting countries in that year") +
  theme_ws() + theme(panel.grid.major.x = element_blank(),
                     panel.grid.major.y = element_line(colour = grid_col, linewidth = 0.3))
save_fig(p3, "fig03_policy_volume", 6.3, 3.0)

# ---- Figures 4-10: low vs high exposure bar charts, one figure per channel -------------
channel_titles <- c(
  "Overall policy count" = "Creative firms in high-policy-count countries innovate and export more",
  "Intellectual property" = "Where policy stresses IP, creative firms upgrade processes and reward output",
  "Brand development and positioning" = "Where policy stresses branding, creative firms report more pricing power",
  "Market leverage and access" = "Where policy stresses market access, imports clear faster",
  "Digital capabilities and commerce" = "Where policy stresses digital, e-payments arrive faster and training is wider",
  "Finance and financial capability" = "Where policy stresses finance, creative firms face fewer finance obstacles",
  "Sustainability, standards and certification" = "Where policy stresses sustainability, more creative firms use solar and upgrade processes"
)

channel_fig <- function(fam, idx) {
  # Bar charts as prepared for the report: weighted means, below vs above median exposure.
  d <- desc |> filter(family == fam) |>
    mutate(group = factor(ifelse(exposure_group == "High", "Above median", "Below median"),
                          levels = c("Below median", "Above median")))
  lv <- reg$label[reg$family == fam]
  d$label <- factor(d$label, levels = lv)
  d$txt <- ifelse(d$unit == "days", sprintf("%.1f", d$value), sprintf("%.1f%%", d$value))
  d$panel_unit <- ifelse(d$unit == "days", "days", "%")
  n_out <- length(lv)
  r <- reg |> filter(family == fam); r$label <- factor(r$label, levels = lv)
  bars <- ggplot(d, aes(group, value, fill = group)) +
    geom_col(width = 0.62) +
    geom_text(aes(label = txt), vjust = -0.5, size = 2.8, family = base_font, colour = ink) +
    facet_wrap(~label, nrow = 1, scales = "free_y", labeller = label_wrap_gen(22)) +
    scale_fill_manual(values = c("Below median" = col_low, "Above median" = col_high), guide = "none") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.16)), labels = label_number(accuracy = 1)) +
    labs(x = NULL, y = "Weighted mean") +
    theme_ws() + theme(panel.grid.major.x = element_blank(),
                       panel.grid.major.y = element_line(colour = grid_col, linewidth = 0.3),
                       strip.text = element_text(size = 8.5), panel.spacing.x = unit(14, "pt"))
  coef <- ggplot(r, aes(est, 1)) +
    geom_vline(xintercept = 0, colour = ink_3, linewidth = 0.3) +
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, colour = ink_2, linewidth = 0.6) +
    geom_point(colour = col_high, size = 2.4) +
    geom_text(aes(label = est_lab), vjust = -1.1, size = 2.7, family = base_font, colour = ink) +
    facet_wrap(~label, nrow = 1, scales = "free_x", labeller = as_labeller(function(x) rep("", length(x)))) +
    scale_x_continuous(expand = expansion(mult = 0.2), breaks = scales::breaks_pretty(4)) +
    scale_y_continuous(limits = c(0.7, 1.6)) +
    labs(x = "Adjusted difference, above minus below median (90% CI)", y = "Regression") +
    theme_ws() + theme(axis.text.y = element_blank(), strip.text = element_blank(),
                       panel.spacing.x = unit(14, "pt"), axis.title.y = element_text(colour = ink_2))
  # Title and explanation live in the LaTeX caption, so the figure stays compact.
  p <- (bars / coef) + plot_layout(heights = c(2.3, 1))
  save_fig(p, sprintf("fig%02d_%s", idx, gsub("[^a-z]+", "_", tolower(sub(",.*", "", fam)))),
           ifelse(n_out == 2, 5.6, 6.3), 2.95)
}
for (i in seq_along(families)) channel_fig(families[i], i + 3)

# ---- Appendix Figure A1: all standardized coefficients ---------------------
ra <- reg |> mutate(family = factor(family, levels = families),
                    row = paste(family, label)) |> arrange(family, desc(z))
ra$row <- factor(ra$row, levels = rev(ra$row))
pa <- ggplot(ra, aes(z, row)) +
  geom_vline(xintercept = 0, colour = ink_3, linewidth = 0.3) +
  geom_errorbarh(aes(xmin = z_lo, xmax = z_hi), height = 0, colour = ink_2, linewidth = 0.6) +
  geom_point(colour = col_high, size = 2.4) +
  facet_grid(family ~ ., scales = "free_y", space = "free_y", switch = "y",
             labeller = label_wrap_gen(22)) +
  scale_y_discrete(labels = function(x) sub(paste0("^(", paste(families, collapse = "|"), ") "), "", x)) +
  labs(x = "Standardized coefficient (in outcome SDs), 90% CI", y = NULL,
       title = "All 17 headline associations in one view",
       subtitle = "Each estimate from a separate weighted regression with the full control set; SEs clustered by country") +
  theme_ws() + theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 0, hjust = 1, size = 7.5),
                     panel.spacing.y = unit(3, "pt"), panel.grid.major.x = element_line(colour = grid_col, linewidth = 0.3))
save_fig(pa, "figA1_all_coefficients", 6.5, 5.6)

cat("Figures written to", out_dir, "\n")
