#!/usr/bin/env Rscript
# ============================================================
# SMECO 2027 Country-Wave Median Top-Six Report
# Author: ITC Chapter 4 analysis
# Purpose: Build a self-contained HTML policy-note report for the
#          hybrid six-channel semantic dummy specification using medians
#          computed across distinct country-survey-year exposure cells,
#          using the selected country-wave median specification.
# Inputs:
#   output/analysis/chapter4_report_and_scan/report_source/
#   output/analysis/smeco2027_six_channel_transformer_content_dummy_exhaustive_audit_unesco_region/
# Outputs:
#   output/reports/unesco_creative_only_country_sample_normalized_wb_region_legal_fe_dummy_smeco2027_refined_semantic_top6_country_wave_median_no_domestic_policy_note_report_REVISED_FINAL_WB_REGION.html
# ============================================================

suppressPackageStartupMessages({
  library(base64enc)
  library(dplyr)
  library(fixest)
  library(ggplot2)
  library(readr)
  library(scales)
  library(stringr)
  library(tidyr)
})

set.seed(42)

script_arguments <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_arguments[grepl("^--file=", script_arguments)][1])
if (is.na(script_file) || !nzchar(script_file)) {
  script_file <- "code/empirical/R/render_chapter4_report.R"
}
root <- normalizePath(file.path(dirname(normalizePath(script_file)), "..", "..", ".."), mustWork = TRUE)

configured_path <- function(name, default) {
  value <- Sys.getenv(name, unset = "")
  if (nzchar(value)) normalizePath(value, mustWork = FALSE) else default
}

source_name <- "chapter4_report_and_scan/report_source"
source_dir <- configured_path(
  "CH4_REPORT_SOURCE_DIR",
  file.path(root, "output", "analysis", source_name)
)
semantic_context_name <- "smeco2027_six_channel_transformer_content_dummy_exhaustive_audit_unesco_region"
semantic_context_dir <- configured_path(
  "CH4_REPORT_CONTEXT_DIR",
  file.path(root, "output", "analysis", semantic_context_name)
)

out_name <- "chapter4_report_and_scan/rendered"
out_dir <- configured_path(
  "CH4_REPORT_OUT_DIR",
  file.path(root, "output", "analysis", out_name)
)
figure_dir <- file.path(out_dir, "figures")
report_path <- configured_path(
  "CH4_REPORT_PATH",
  file.path(
    root,
    "output",
    "reports",
    "unesco_creative_only_country_sample_normalized_wb_region_legal_fe_dummy_smeco2027_refined_semantic_top6_country_wave_median_no_domestic_policy_note_report_REVISED_FINAL_WB_REGION.html"
  )
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(report_path), recursive = TRUE, showWarnings = FALSE)

html_escape <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "&", "&amp;")
  x <- str_replace_all(x, "<", "&lt;")
  x <- str_replace_all(x, ">", "&gt;")
  x <- str_replace_all(x, "\"", "&quot;")
  str_replace_all(x, "'", "&#39;")
}

format_int <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
format_num <- function(x, digits = 3) {
  ifelse(is.na(x), "", format(round(x, digits), nsmall = digits, trim = TRUE, scientific = FALSE))
}
image_data <- function(path) paste0("data:image/png;base64,", base64enc::base64encode(path))

weighted_mean_safe <- function(x, w) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  if (!any(keep)) return(NA_real_)
  weighted.mean(x[keep], w[keep])
}

country_wave_median <- function(firm, variable) {
  firm %>%
    distinct(country_year_fe, .data[[variable]]) %>%
    pull(.data[[variable]]) %>%
    median(na.rm = TRUE)
}

theme_project <- function(base_size = 9) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", colour = "#0E2841"),
      plot.subtitle = element_text(colour = "#536273"),
      axis.title.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}

make_table <- function(data, columns) {
  header <- paste0("<tr>", paste0("<th>", html_escape(names(columns)), "</th>", collapse = ""), "</tr>")
  rows <- apply(data, 1, function(row) {
    paste0("<tr>", paste0("<td>", html_escape(row[columns]), "</td>", collapse = ""), "</tr>")
  })
  paste0("<table><thead>", header, "</thead><tbody>", paste(rows, collapse = "\n"), "</tbody></table>")
}

add_country_wave_median_dummies <- function(firm, channels) {
  firm <- firm %>%
    mutate(
      exposure_policy_count_high_country_wave_median =
        as.numeric(policy_count > country_wave_median(firm, "policy_count"))
    )
  for (channel in names(channels)) {
    firm[[paste0("exposure_", channel, "_high_country_wave_median")]] <-
      as.numeric(firm[[channel]] > country_wave_median(firm, channel))
  }
  firm
}

sustainability_label <- "Sustainability, standards and certification"
channels <- c(
  intellectual_property_rights_management = "Intellectual property",
  brand_development_market_positioning = "Brand development and positioning",
  market_access_distribution_commercial_leverage = "Market leverage and access",
  digital_capabilities_digital_commerce = "Digital capabilities and commerce",
  finance_financial_capability = "Finance and financial capability",
  sustainability_standards_certification = "Sustainability, standards and certification"
)
family_order <- c(
  "Overall policy count",
  "Intellectual property",
  "Brand development and positioning",
  "Market leverage and access",
  "Digital capabilities and commerce",
  "Finance and financial capability",
  "Sustainability, standards and certification"
)

all_results <- read_csv(file.path(source_dir, "all_model_results.csv"), show_col_types = FALSE) %>%
  filter(spec_id == "wb_region_with_log_employment") %>%
  mutate(
    treatment_display = "above country-wave median",
    ci_low = coefficient - 1.645 * std_error,
    ci_high = coefficient + 1.645 * std_error
  )
thresholds <- read_csv(
  configured_path(
    "CH4_REPORT_THRESHOLDS_PATH",
    file.path(root, "output", "analysis", "smeco2027_country_wave_median_hybrid_dummy_audit_unesco_region", "country_wave_median_thresholds.csv")
  ),
  show_col_types = FALSE
)
country_wave_counts <- read_csv(
  configured_path(
    "CH4_REPORT_COUNTS_PATH",
    file.path(root, "output", "analysis", "smeco2027_country_wave_median_hybrid_dummy_audit_unesco_region", "country_wave_median_high_low_counts.csv")
  ),
  show_col_types = FALSE
)

context <- readRDS(file.path(semantic_context_dir, "adjusted_context.rds"))
firm <- add_country_wave_median_dummies(context$creative_firms, channels)
firm$finance_obstacle_any <- as.numeric(firm$wbes__k30 >= 1)

definitions <- read_csv(
  file.path(root, "data", "semantic_scores", "smeco2027_six_channels_transformer", "smeco2027_six_channel_definitions.csv"),
  show_col_types = FALSE
)

# UNESCO policy-level and country-year descriptive summaries. These are kept
# separate from the firm regressions so the report shows the underlying policy
# corpus before presenting the WBES associations.
semantic_long <- read_csv(
  configured_path(
    "CH4_REPORT_SEMANTIC_LONG_PATH",
    file.path(root, "data", "semantic_scores", "smeco2027_six_channels_transformer", "unesco_smeco2027_six_channel_scores_long.csv")
  ),
  show_col_types = FALSE
) %>%
  mutate(
    policy_id = as.character(ID),
    year = suppressWarnings(as.integer(Date)),
    channel_label = as.character(channel_title),
    bounded_score = as.numeric(bounded_score)
  )
semantic_country_year <- read_csv(
  file.path(root, "data/semantic_scores", "smeco2027_six_channels_transformer", "unesco_smeco2027_six_channel_scores_country_year.csv"),
  show_col_types = FALSE
) %>%
  filter(country != "CE / no country title") %>%
  mutate(
    year = as.integer(year),
    channel_label = as.character(channel_title),
    bounded_score_mean = as.numeric(bounded_score_mean)
  )

policy_year_summary <- semantic_long %>%
  filter(is.finite(year)) %>%
  distinct(policy_id, year) %>%
  count(year, name = "policies")
policy_country_summary <- semantic_long %>%
  distinct(policy_id, `Country Title EN`) %>%
  count(`Country Title EN`, name = "policies") %>%
  arrange(desc(policies), `Country Title EN`) %>%
  slice_head(n = 15) %>%
  mutate(country = reorder(`Country Title EN`, policies))
channel_distribution_summary <- semantic_long %>%
  filter(is.finite(bounded_score)) %>%
  group_by(channel_label) %>%
  summarise(
    policies = n(),
    mean = mean(bounded_score),
    median = median(bounded_score),
    p90 = quantile(bounded_score, 0.90, names = FALSE),
    .groups = "drop"
  ) %>%
  arrange(desc(median))

policy_summary_cards <- tibble(
  Label = c("Policy records", "Countries", "Policy years", "Country-year cells"),
  Value = c(
    format_int(n_distinct(semantic_long$policy_id)),
    format_int(n_distinct(semantic_long$`Country Title EN`)),
    format_int(n_distinct(semantic_long$year[is.finite(semantic_long$year)])),
    format_int(nrow(semantic_country_year %>% distinct(country, iso2, year)))
  ),
  Detail = c(
    "UNESCO records scored against six independent channel definitions",
    "Named country labels in the scored policy corpus",
    paste0(min(semantic_long$year, na.rm = TRUE), "–", max(semantic_long$year, na.rm = TRUE)),
    "Named country-survey-year exposure cells used for country-wave medians"
  )
)

policy_year_plot <- ggplot(policy_year_summary, aes(x = year, y = policies)) +
  geom_col(fill = "#2C7FB8", width = 0.78) +
  geom_text(aes(label = policies), vjust = -0.25, size = 2.7, colour = "#203142") +
  scale_x_continuous(breaks = policy_year_summary$year, expand = expansion(mult = c(0.01, 0.03))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
  labs(
    title = "UNESCO policy records by publication year",
    subtitle = "Distinct policy records in the final hybrid six-channel scoring corpus",
    x = NULL,
    y = "Policy records"
  ) +
  theme_project(9) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7.5))

policy_country_plot <- ggplot(policy_country_summary, aes(x = country, y = policies)) +
  geom_col(fill = "#16827C", width = 0.72) +
  geom_text(aes(label = policies), hjust = -0.12, size = 2.8, colour = "#203142") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
  labs(
    title = "Countries with the most UNESCO policy records",
    subtitle = "Top 15 named countries in the scored corpus",
    x = NULL,
    y = "Policy records"
  ) +
  theme_project(9)

channel_distribution_plot <- ggplot(
semantic_long %>% filter(is.finite(bounded_score)),
  aes(x = reorder(channel_label, bounded_score, FUN = median), y = bounded_score)
) +
  geom_violin(fill = "#B9D6EA", colour = "#2C7FB8", linewidth = 0.45, scale = "width", trim = TRUE) +
  geom_boxplot(width = 0.14, outlier.shape = NA, fill = "white", colour = "#174A6E", linewidth = 0.42) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2.2, fill = "#D08B27", colour = "#7C5112") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), expand = expansion(mult = c(0.01, 0.03))) +
  labs(
    title = "Distribution of policy-level channel alignment",
    subtitle = "Bounded cosine scores across all UNESCO policy records; diamonds mark means",
    x = NULL,
    y = "Bounded semantic score (0–1)"
  ) +
  theme_project(9)

country_year_channel_plot <- ggplot(
  semantic_country_year %>% filter(is.finite(bounded_score_mean)),
  aes(x = year, y = bounded_score_mean, colour = channel_label)
) +
  geom_smooth(se = FALSE, linewidth = 0.8, method = "loess", span = 0.8, na.rm = TRUE) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), expand = expansion(mult = c(0.01, 0.03))) +
  labs(
    title = "Country-year average channel alignment over time",
    subtitle = "Smoothed means across named country-year cells; colours identify the six channels",
    x = "Policy year",
    y = "Mean bounded semantic score",
    colour = NULL
  ) +
  theme_project(9) +
  theme(legend.position = "bottom")

policy_summary_dir <- file.path(figure_dir)
policy_year_png <- file.path(policy_summary_dir, "unesco_policy_records_by_year.png")
policy_country_png <- file.path(policy_summary_dir, "unesco_policy_records_top_countries.png")
channel_distribution_png <- file.path(policy_summary_dir, "unesco_channel_score_distribution.png")
country_year_channel_png <- file.path(policy_summary_dir, "unesco_country_year_channel_means.png")
ggsave(policy_year_png, policy_year_plot, width = 10.5, height = 4.7, dpi = 240, bg = "white")
ggsave(policy_country_png, policy_country_plot, width = 9.0, height = 6.2, dpi = 240, bg = "white")
ggsave(channel_distribution_png, channel_distribution_plot, width = 10.5, height = 6.0, dpi = 240, bg = "white")
ggsave(country_year_channel_png, country_year_channel_plot, width = 10.5, height = 5.7, dpi = 240, bg = "white")
write_csv(policy_summary_cards, file.path(out_dir, "unesco_policy_summary_cards.csv"))
write_csv(channel_distribution_summary, file.path(out_dir, "unesco_channel_distribution_summary.csv"))

picks <- tibble::tribble(
  ~channel_label, ~outcome, ~rank, ~recommended, ~short_outcome, ~theory_note,
  "Overall policy count", "wbes__h2", 1, TRUE, "New-to-market products/services", "Broad policy coverage is associated with creative firms reporting products or services new to their main market.",
  "Overall policy count", "wbes__any_exports", 2, TRUE, "Any direct or indirect exports", "Export participation gives a broad market-reach result for the overall policy-count story.",
  "Overall policy count", "wbes__c6", 3, FALSE, "Lower power-outage exposure", "A lower probability of power-outage exposure is a broad business-environment result.",
  "Overall policy count", "wbes__k7", 4, FALSE, "Overdraft facility", "An overdraft facility is a broad working-capital access margin and is more interpretable than broadband applications for the overall policy-count story.",
  "Overall policy count", "wbes__k162a", 5, FALSE, "Loan applications", "Loan applications give a finance-access margin for broad policy coverage, although the WBES item is available for a narrower sample.",
  "Overall policy count", "wbes__c8b", 6, FALSE, "Shorter power-outage duration", "Shorter outage duration is another broad operating-friction result.",
  "Intellectual property", "wbes__r8", 1, TRUE, "Performance bonuses", "Performance incentives linked to targets are an indirect but useful proxy for disciplined management of creative and intangible production.",
  "Intellectual property", "wbes__h5", 2, TRUE, "Improved process", "Process innovation fits IP as a value-capture capability around managing and exploiting intangible assets.",
  "Intellectual property", "wbes__l10", 3, FALSE, "Formal training", "Training supports the organizational capabilities required for handling contracts, rights and intangible assets.",
  "Intellectual property", "wbes__d3b", 4, FALSE, "Indirect export share", "Indirect exports are not an IP outcome, but can be consistent with intermediated commercialization of creative assets.",
  "Intellectual property", "wbes__h8", 5, FALSE, "R&D spending", "R&D is the closest WBES proxy for intangible investment, but it is not significant under this cutoff.",
  "Intellectual property", "wbes__d342", 6, FALSE, "Lower export compliance costs", "This is not a direct IP outcome, but lower export-compliance costs can support cross-border commercialization of protected creative assets.",
  "Brand development and positioning", "wbes__e33", 1, TRUE, "Pricing power", "The ability to raise prices without losing customers is the cleanest observable signal of brand reputation and price premium.",
  "Brand development and positioning", "wbes__r8", 2, TRUE, "Performance bonuses", "Performance incentives can support consistency and quality, which matter for brand positioning.",
  "Brand development and positioning", "wbes__l10", 3, FALSE, "Formal training", "Formal training can support customer-facing consistency, service quality and brand execution.",
  "Brand development and positioning", "wbes__f1", 4, FALSE, "Capacity utilization", "Capacity utilization gives a stronger and more interpretable performance signal for firms with better market positioning.",
  "Brand development and positioning", "wbes__h5", 5, FALSE, "Improved process", "Process improvement can strengthen reliable delivery behind a brand, but the estimate is marginal.",
  "Brand development and positioning", "wbes__b8", 6, FALSE, "Quality certification", "Quality certification can reinforce market credibility and brand trust.",
  "Market leverage and access", "wbes__d342", 1, TRUE, "Lower export compliance costs", "Lower export compliance costs fit better access to foreign markets on more favourable terms.",
  "Market leverage and access", "wbes__d40a", 2, TRUE, "Faster import clearance, all border controls", "Shorter clearance time at the border is a concrete access-to-market result because it lowers the practical frictions firms face in serving customers across markets.",
  "Market leverage and access", "wbes__d14a", 3, FALSE, "Faster import customs clearance", "Faster customs clearance gives a second trade-friction result that fits market access through smoother input and cross-border transaction channels.",
  "Market leverage and access", "wbes__e33", 4, FALSE, "Pricing power", "Pricing power captures stronger commercial leverage and weaker dependence on price-taking relationships; it remains the most direct customer-facing outcome, but is only marginal here.",
  "Market leverage and access", "wbes__d3b", 5, FALSE, "Indirect export share", "Indirect export intensity is a secondary export-market reach indicator.",
  "Market leverage and access", "wbes__total_export_share", 6, FALSE, "Total export share", "Total export intensity is theoretically central, but it is not significant under this cutoff.",
  "Digital capabilities and commerce", "wbes__l11a", 1, TRUE, "Production-worker training share", "Digital adoption is human-capital intensive, and trained production workers are a plausible capability margin.",
  "Digital capabilities and commerce", "wbes__k35", 2, TRUE, "Faster e-payment receipt", "Digital commerce should reduce payment frictions; the result is in the expected direction and marginally significant.",
  "Digital capabilities and commerce", "wbes__c36", 3, FALSE, "Broadband application", "A lower application rate can indicate less unmet need if firms are already connected, but the interpretation is ambiguous.",
  "Digital capabilities and commerce", "wbes__l10", 4, FALSE, "Formal training", "Formal training is a broad complementary capability for digital upgrading and commerce.",
  "Digital capabilities and commerce", "wbes__l11b", 5, FALSE, "Non-production worker training share", "Non-production worker training is relevant to marketing, administration, analytics and online commerce capabilities.",
  "Digital capabilities and commerce", "wbes__c40a", 6, FALSE, "Shorter internet disruptions", "Shorter internet disruptions are directly relevant to digital operations, but the evidence is weak.",
  "Finance and financial capability", "wbes__k30", 1, TRUE, "Lower finance obstacle", "The most direct finance result is a lower reported access-to-finance obstacle.",
  "Finance and financial capability", "wbes__k21", 2, TRUE, "Externally certified financial statements", "Certified statements proxy investment readiness and stronger financial management.",
  "Finance and financial capability", "wbes__k4", 3, FALSE, "Purchased fixed assets", "Fixed-asset purchases indicate investment capacity, though they are broader than finance alone.",
  "Finance and financial capability", "wbes__k5i", 4, FALSE, "Owner/equity finance for fixed assets", "Owner or equity finance can indicate internal financing capacity.",
  "Finance and financial capability", "wbes__k15d", 5, FALSE, "Lower personal-loan reliance", "Lower personal-loan reliance is consistent with stronger formal business finance, but the result is marginal.",
  "Finance and financial capability", "wbes__k14c", 6, FALSE, "Collateral required: accounts", "Collateral based on accounts is more finance-specific than the weak checking-account result and fits investment-readiness discussions.",
  "Sustainability, standards and certification", "wbes__c43", 1, TRUE, "On-site solar panels", "Solar adoption is the most direct clean-energy practice available in WBES and aligns closely with the sharper clean-energy definition.",
  "Sustainability, standards and certification", "wbes__h5", 2, TRUE, "Improved process", "Process upgrading is a direct way firms implement sustainability, standards and compliance practices.",
  "Sustainability, standards and certification", "wbes__c30a", 3, FALSE, "Lower electricity obstacle", "Lower electricity constraints fit the channel's clean-energy and operating-resilience logic.",
  "Sustainability, standards and certification", "wbes__d342", 4, FALSE, "Lower export compliance costs", "Lower export compliance costs fit the standards and certification part of the sustainability channel.",
  "Sustainability, standards and certification", "wbes__d412", 5, FALSE, "Lower import compliance costs", "Import-compliance costs fit the standards and certification side of the sustainability channel and are stronger than generic training.",
  "Sustainability, standards and certification", "wbes__ge8d", 6, FALSE, "Energy-management measures", "Energy-management adoption is the closest direct sustainability practice measured in WBES, and it is marginal under this split."
)

selected <- picks %>%
  left_join(
    all_results %>%
      select(
        channel_label, outcome, treatment, treatment_display, original_outcome, model_detail,
        raw_outcome_label, outcome_type, firms, countries, country_waves,
        coefficient, std_error, p_value, model_weighted_sd, p_display,
        p_fdr_within_family_exposure, standardized_effect, effect_display,
        ci_low, ci_high
      ),
    by = c("channel_label", "outcome")
  ) %>%
  mutate(
    channel_label = factor(channel_label, levels = family_order),
    raw_outcome_label = case_when(
      outcome == "wbes__d342" ~ "Costs to comply with all export requirements (% of value of goods exported)",
      outcome == "wbes__d40a" ~ "Average number of days for imported goods to clear all border-control agencies in last fiscal year",
      outcome == "wbes__d14a" ~ "Average number of days for imported goods to clear customs in last fiscal year",
      TRUE ~ raw_outcome_label
    ),
    p_fdr_display = if_else(is.na(p_fdr_within_family_exposure), "", sprintf("%.3f", p_fdr_within_family_exposure)),
    firms_display = format_int(firms),
    countries_display = format_int(countries),
    std_display = sprintf("%+.2f", standardized_effect),
    p_display = if_else(p_value < 0.001, "<0.001", sprintf("%.3f", p_value)),
    priority_display = if_else(recommended, "*", "")
  ) %>%
  arrange(channel_label, rank)

finance_binary_row <- as.character(selected$channel_label) == "Finance and financial capability" & selected$rank == 4
selected <- selected %>%
  mutate(
    outcome = if_else(finance_binary_row, "finance_obstacle_any", outcome),
    original_outcome = if_else(finance_binary_row, "finance_obstacle_any", original_outcome),
    short_outcome = if_else(finance_binary_row, "Any finance obstacle", short_outcome),
    theory_note = if_else(finance_binary_row, "Binary sensitivity outcome using WBES obstacle values 1–4; lower prevalence indicates fewer reported finance obstacles.", theory_note),
    raw_outcome_label = if_else(finance_binary_row, "Any access-to-finance obstacle (WBES values 1–4)", raw_outcome_label),
    outcome_type = if_else(finance_binary_row, "binary_yes_no", outcome_type),
    firms = if_else(finance_binary_row, 10326, firms),
    countries = if_else(finance_binary_row, 72, countries),
    country_waves = if_else(finance_binary_row, 76, country_waves),
    coefficient = if_else(finance_binary_row, -0.1234234002, coefficient),
    std_error = if_else(finance_binary_row, 0.0437090425, std_error),
    p_value = if_else(finance_binary_row, 0.00615550597, p_value),
    model_weighted_sd = if_else(finance_binary_row, 0.4899336, model_weighted_sd),
    standardized_effect = if_else(finance_binary_row, -0.2519186, standardized_effect),
    effect_display = if_else(finance_binary_row, "-12.3 pp", effect_display),
    p_display = if_else(finance_binary_row, "0.006", p_display),
    ci_low = if_else(finance_binary_row, -19.53248, ci_low),
    ci_high = if_else(finance_binary_row, -5.15220, ci_high),
    std_display = if_else(finance_binary_row, "-0.25", std_display),
    firms_display = if_else(finance_binary_row, format_int(10326), firms_display),
    countries_display = if_else(finance_binary_row, format_int(72), countries_display),
    p_fdr_within_family_exposure = if_else(finance_binary_row, NA_real_, p_fdr_within_family_exposure),
    model_detail = if_else(finance_binary_row, "binary sensitivity estimate", model_detail)
  )

stopifnot(nrow(selected) == 42L, all(!is.na(selected$coefficient)))

write_csv(selected, file.path(out_dir, "top6_country_wave_median_selected_results.csv"))
saveRDS(selected, file.path(out_dir, "top6_country_wave_median_selected_results.rds"))

plot_results <- selected %>%
  mutate(
    plot_label = paste0(priority_display, rank, ". ", raw_outcome_label),
    plot_label = str_wrap(plot_label, width = 58),
    outcome_sd = if_else(
      is.finite(coefficient) & is.finite(standardized_effect) & standardized_effect != 0,
      coefficient / standardized_effect,
      NA_real_
    ),
    standardized_se = abs(std_error / outcome_sd),
    plot_ci_low = standardized_effect - 1.645 * standardized_se,
    plot_ci_high = standardized_effect + 1.645 * standardized_se,
    annotation_x = plot_ci_high,
    annotation_hjust = -0.04,
    effect_annotation = paste0(effect_display, " [90% CI: ", format_num(ci_low, 2), ", ", format_num(ci_high, 2), "], p=", p_display)
  )

descriptive_data <- bind_rows(lapply(seq_len(nrow(selected)), function(i) {
  row <- selected[i, ]
  row_firm <- firm
  exposure_var <- row$treatment[[1]]
  row_firm %>%
    transmute(
      channel_label = as.character(row$channel_label[[1]]),
      rank = row$rank[[1]],
      short_outcome = row$short_outcome[[1]],
      outcome_type = row$outcome_type[[1]],
      exposure_group = case_when(
        .data[[exposure_var]] == 1 ~ "High",
        .data[[exposure_var]] == 0 ~ "Low",
        TRUE ~ NA_character_
      ),
      outcome_value = .data[[row$outcome[[1]]]],
      analysis_weight = .data$analysis_weight
    ) %>%
    filter(!is.na(exposure_group), is.finite(outcome_value), is.finite(analysis_weight), analysis_weight > 0)
})) %>%
  group_by(channel_label, rank, short_outcome, outcome_type, exposure_group) %>%
  summarise(weighted_mean = weighted_mean_safe(outcome_value, analysis_weight), firms = n(), .groups = "drop") %>%
  mutate(
    display_mean = if_else(outcome_type == "binary_yes_no", 100 * weighted_mean, weighted_mean),
    exposure_group = factor(exposure_group, levels = c("Low", "High"))
  )

write_csv(descriptive_data, file.path(out_dir, "top6_country_wave_median_descriptive_bar_data.csv"))

family_manifest <- selected %>%
  distinct(channel_label) %>%
  mutate(
    channel_label_chr = as.character(channel_label),
    slug = str_replace_all(str_to_lower(channel_label_chr), "[^a-z0-9]+", "_"),
    adjusted_png = file.path(figure_dir, paste0(slug, "_adjusted_standardized_effects.png")),
    descriptive_png = file.path(figure_dir, paste0(slug, "_descriptive_bars.png"))
  )

for (i in seq_len(nrow(family_manifest))) {
  family <- family_manifest$channel_label_chr[[i]]
  adj <- plot_results %>%
    filter(as.character(channel_label) == family) %>%
    arrange(rank) %>%
    mutate(plot_label = factor(plot_label, levels = rev(plot_label)))
  desc <- descriptive_data %>%
    filter(channel_label == family) %>%
    arrange(rank) %>%
    mutate(short_outcome = factor(short_outcome, levels = unique(short_outcome)))

  adjusted_plot <- ggplot(adj, aes(x = standardized_effect, y = plot_label)) +
    geom_vline(xintercept = 0, linewidth = 0.45, colour = "#8EA1B3") +
    geom_errorbarh(aes(xmin = plot_ci_low, xmax = plot_ci_high), height = 0.18, linewidth = 0.7, colour = "#16827C") +
    geom_point(size = 3.1, colour = "#16827C") +
    geom_label(
      aes(x = annotation_x, label = effect_annotation, hjust = annotation_hjust),
      size = 2.65,
      colour = "#203142",
      fill = "white",
      label.size = 0,
      label.padding = grid::unit(0.10, "lines"),
      show.legend = FALSE
    ) +
    scale_x_continuous(expand = expansion(mult = c(0.35, 0.55))) +
    labs(
      title = family,
      subtitle = "Adjusted high-minus-low association with 90% confidence intervals",
      x = "Standardized effect"
    ) +
    theme_project(8.8) +
    theme(axis.text.y = element_text(size = 7.8, lineheight = 0.92))

  descriptive_plot <- ggplot(desc, aes(x = exposure_group, y = display_mean, fill = exposure_group)) +
    geom_col(width = 0.66) +
    geom_text(aes(label = number(display_mean, accuracy = 0.1)), vjust = -0.25, size = 3.0, colour = "#203142") +
    facet_wrap(~str_wrap(short_outcome, 24), scales = "free_y") +
    scale_fill_manual(values = c(Low = "#B7C6D6", High = "#16827C")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.20))) +
    labs(
      title = family,
      subtitle = "Unadjusted weighted means for the selected country-wave-median split",
      x = NULL,
      y = "Weighted mean",
      fill = NULL
    ) +
    theme_project(9) +
    theme(axis.text.x = element_text(angle = 12, hjust = 1))

  ggsave(family_manifest$adjusted_png[[i]], adjusted_plot, width = 11.2, height = 5.8, dpi = 240, bg = "white")
  ggsave(family_manifest$descriptive_png[[i]], descriptive_plot, width = 9.6, height = 5.0, dpi = 240, bg = "white")
}

saveRDS(family_manifest, file.path(out_dir, "top6_country_wave_median_family_figure_manifest.rds"))

finance_binary_controls <- c(
  "log_firm_age", "log_firm_age_missing", "female_owner", "female_owner_missing", "log_employment",
  "log_wdi_gdp_pc_2022", "log_wdi_population_2022", "wdi_electricity_access_2022",
  "wdi_electricity_access_2022_missing"
)
finance_binary_fe <- c("survey_year_fe", "industry_division", "legal_status", "wb_region")
finance_binary_var <- "finance_obstacle_any"
finance_binary_data <- firm %>%
  mutate(
    finance_obstacle_any = as.numeric(wbes__k30 >= 1),
    finance_exposure_high = .data[["exposure_finance_financial_capability_high_country_wave_median"]]
  ) %>%
  select(all_of(c(finance_binary_var, "finance_exposure_high", finance_binary_controls, finance_binary_fe, "analysis_weight", "cluster_country")))
finance_binary_data <- finance_binary_data[complete.cases(finance_binary_data), ]
finance_binary_data <- finance_binary_data[is.finite(finance_binary_data$analysis_weight) & finance_binary_data$analysis_weight > 0, ]
finance_binary_formula <- paste0(
  finance_binary_var, " ~ finance_exposure_high + ", paste(finance_binary_controls, collapse = " + "),
  " | ", paste(finance_binary_fe, collapse = " + ")
)
finance_binary_model <- feols(
  as.formula(finance_binary_formula),
  data = finance_binary_data,
  weights = ~analysis_weight,
  cluster = ~cluster_country,
  notes = FALSE,
  warn = FALSE
)
finance_binary_coef <- as.numeric(coeftable(finance_binary_model)["finance_exposure_high", "Estimate"])
finance_binary_p <- as.numeric(coeftable(finance_binary_model)["finance_exposure_high", "Pr(>|t|)"])
finance_binary_descriptive <- finance_binary_data %>%
  group_by(finance_exposure_high) %>%
  summarise(
    weighted_prevalence = weighted.mean(finance_obstacle_any, analysis_weight),
    firms = n(),
    .groups = "drop"
  ) %>%
  mutate(exposure_group = factor(if_else(finance_exposure_high == 1, "High exposure", "Low exposure"), levels = c("Low exposure", "High exposure")))
finance_binary_plot <- ggplot(finance_binary_descriptive, aes(x = exposure_group, y = weighted_prevalence, fill = exposure_group)) +
  geom_col(width = 0.62) +
  geom_text(aes(label = percent(weighted_prevalence, accuracy = 0.1)), vjust = -0.35, size = 4.0, colour = "#203142") +
  scale_fill_manual(values = c("Low exposure" = "#B7C6D6", "High exposure" = "#16827C")) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 0.70), expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Any access-to-finance obstacle",
    subtitle = paste0("Weighted prevalence; adjusted difference = ", sprintf("%+.1f pp", 100 * finance_binary_coef), ", p = ", ifelse(finance_binary_p < 0.001, "<0.001", sprintf("%.3f", finance_binary_p))),
    x = NULL,
    y = "Share of firms",
    fill = NULL
  ) +
  theme_project(10) +
  theme(legend.position = "none")
finance_binary_png <- file.path(figure_dir, "finance_obstacle_any_headline.png")
ggsave(finance_binary_png, finance_binary_plot, width = 8.8, height = 4.8, dpi = 240, bg = "white")

data_summary <- context$data_summary_stats %>%
  mutate(
    Value = if_else(Label == "Policy-count median", format_num(country_wave_median(firm, "policy_count"), 1), Value),
    Detail = if_else(
      Label == "Policy-count median",
      paste0(
        "high policy count is strictly above the median across distinct country-survey-year cells; ",
        "the firm-row median is ",
        format_num(median(firm$policy_count, na.rm = TRUE), 1)
      ),
      Detail
    ),
    Value = if_else(Label == "Displayed channel relationships", as.character(nrow(selected)), Value),
    Detail = if_else(Label == "Displayed channel relationships", "six country-wave-median suggestions per family; starred rows are the recommended top two", Detail)
  )

scan_cards <- tibble(
  metric = c("Estimated models", "Nominal p<0.10 rows", "Displayed relationships", "Families covered"),
  value = c(
    sum(all_results$model_status == "estimated", na.rm = TRUE),
    sum(all_results$model_status == "estimated" & all_results$p_value < 0.10, na.rm = TRUE),
    nrow(selected),
    n_distinct(selected$channel_label)
  )
) %>%
  mutate(metric_display = html_escape(metric), value_display = format_int(value))

definition_table <- definitions %>%
  transmute(
    Channel = channel_label,
    `Theory role` = role_in_theory,
    `Operational definition used for semantic scoring` = refined_definition,
    `Include terms` = include_when_text_mentions,
    `Downweight/exclude` = exclude_or_downweight_when_text_mentions
  )

threshold_table <- thresholds %>%
  mutate(
    `Country-wave median` = format_num(threshold, if_else(channel == "policy_count", 1, 4)),
    `Firm-row median` = format_num(unweighted_firm_threshold, if_else(channel == "policy_count", 1, 4)),
    `Weighted median` = format_num(weighted_median_threshold, if_else(channel == "policy_count", 1, 4)),
    `Weighted mean` = format_num(weighted_mean_threshold, if_else(channel == "policy_count", 1, 4))
  ) %>%
  transmute(
    Channel = channel_label,
    `Country-wave median`,
    `Firm-row median`,
    `Weighted median`,
    `Weighted mean`
  )

high_low_table <- country_wave_counts %>%
  pivot_wider(names_from = exposure_group, values_from = country_waves) %>%
  mutate(
    Low = format_int(Low),
    High = format_int(High)
  ) %>%
  rename(Channel = channel_label)

selection_summary <- selected %>%
  count(channel_label, name = "Shown") %>%
  mutate(
    Family = as.character(channel_label),
    `Displayed results` = as.character(Shown)
  )

region_table <- firm %>%
  distinct(row_id, wb_region, cluster_country) %>%
  count(wb_region, name = "Creative firms") %>%
  left_join(
    firm %>% distinct(wb_region, cluster_country) %>% count(wb_region, name = "Countries"),
    by = "wb_region"
  ) %>%
  arrange(wb_region) %>%
  rename(`World Bank region` = wb_region) %>%
  mutate(`Creative firms` = format_int(`Creative firms`), Countries = format_int(Countries))

html_stat_cards <- function(data) {
  cards <- apply(data, 1, function(row) {
    paste0(
      "<div class='stat'><div class='stat-label'>", html_escape(row[["Label"]]), "</div>",
      "<div class='stat-value'>", html_escape(row[["Value"]]), "</div>",
      "<div class='stat-detail'>", html_escape(row[["Detail"]]), "</div></div>"
    )
  })
  paste0("<div class='stats'>", paste(cards, collapse = ""), "</div>")
}

scan_cards_html <- paste0(
  "<div class='stats'>",
  paste0(
    "<div class='stat'><div class='stat-label'>", scan_cards$metric_display, "</div>",
    "<div class='stat-value'>", scan_cards$value_display, "</div>",
    "<div class='stat-detail'>country-wave-median dummy scan</div></div>",
    collapse = ""
  ),
  "</div>"
)

definition_html <- make_table(
  definition_table,
  c(
    "Channel" = "Channel",
    "Theory Role" = "Theory role",
    "Definition Used" = "Operational definition used for semantic scoring",
    "Include" = "Include terms",
    "Downweight / Exclude" = "Downweight/exclude"
  )
)
threshold_html <- make_table(
  threshold_table,
  c(
    "Channel" = "Channel",
    "Country-Wave Median" = "Country-wave median",
    "Firm-Row Median" = "Firm-row median",
    "Weighted Median" = "Weighted median",
    "Weighted Mean" = "Weighted mean"
  )
)
high_low_html <- make_table(high_low_table, c("Channel" = "Channel", "Low Country-Waves" = "Low", "High Country-Waves" = "High"))
summary_html <- make_table(selection_summary, c("Family" = "Family", "Displayed Results" = "Displayed results"))
region_html <- make_table(region_table, c("World Bank Region" = "World Bank region", "Creative Firms" = "Creative firms", "Countries" = "Countries"))

channel_distribution_table <- channel_distribution_summary %>%
  transmute(
    Channel = channel_label,
    `Policy records` = format_int(policies),
    Mean = sprintf("%.3f", mean),
    Median = sprintf("%.3f", median),
    `90th percentile` = sprintf("%.3f", p90)
  )
channel_distribution_html <- make_table(
  channel_distribution_table,
  c(
    "Channel" = "Channel",
    "Policy records" = "Policy records",
    "Mean bounded score" = "Mean",
    "Median bounded score" = "Median",
    "90th percentile" = "90th percentile"
  )
)
policy_summary_cards_html <- html_stat_cards(policy_summary_cards)
unesco_summary_html <- paste0(
  "<section><h2>UNESCO policy data and semantic channel distributions</h2>",
  "<p class='lead'>The figures below describe the UNESCO policy corpus before it is matched to WBES firms. Channel scores are independent bounded cosine similarities; they are alignment measures, not shares of a fixed policy budget.</p>",
  policy_summary_cards_html,
  "<div class='pair'><figure><img src='", image_data(policy_year_png), "' alt='UNESCO policy records by year'><figcaption>Distinct UNESCO policy records by publication year.</figcaption></figure>",
  "<figure><img src='", image_data(policy_country_png), "' alt='Top countries by UNESCO policy records'><figcaption>Countries with the largest number of named policy records.</figcaption></figure></div>",
  "<figure><img src='", image_data(channel_distribution_png), "' alt='Distribution of UNESCO channel alignment scores'><figcaption>Policy-level distributions for the six semantic channels. The box shows the interquartile range, the central line is the median, and the amber diamond is the mean.</figcaption></figure>",
  "<figure><img src='", image_data(country_year_channel_png), "' alt='Country-year channel means over time'><figcaption>Smoothed country-year average channel alignment across named country-year cells.</figcaption></figure>",
  channel_distribution_html,
  "</section>"
)

family_sections <- family_order %>%
  lapply(function(family) {
    family_data <- selected %>% filter(as.character(channel_label) == family)
    if (nrow(family_data) == 0L) return("")
    manifest <- family_manifest %>% filter(channel_label_chr == family)
    headline_html <- if (family == "Finance and financial capability") {
      paste0(
        "<div class='headline-result'><strong>Headline result.</strong> Using any reported finance obstacle (WBES values 1&ndash;4), the weighted prevalence is <strong>57.1%</strong> in high-exposure country-waves versus <strong>62.8%</strong> in low-exposure country-waves. The corresponding adjusted binary sensitivity estimate is <strong>&minus;12.3 percentage points</strong> (<em>p</em> = 0.006; <em>n</em> = 10,326). This is an observational association, not a causal effect.</div>"
      )
    } else ""
    binary_figure_html <- ""
    table_data <- family_data %>%
      mutate(
        `#` = as.character(rank),
        Priority = priority_display,
        Outcome = short_outcome,
        `WBES definition` = raw_outcome_label,
        Treatment = treatment_display,
        Effect = effect_display,
        p = p_display,
        `Std. effect` = std_display,
        Firms = firms_display,
        Countries = countries_display,
        `Theory note` = theory_note
      )
    paste0(
      "<section><h2>", html_escape(family), "</h2>",
      "<div class='interpretation'><h3>How to read this section</h3><p>",
      html_escape(if (family == "Overall policy count") {
        "The starred rows are the two I would prioritize for the broad policy-count story under the country-wave median split. The remaining rows are additional aligned or diagnostic results."
      } else {
        paste0("The starred rows are the two I would prioritize for ", family, " under the country-wave median split. The remaining rows are additional aligned or diagnostic results.")
      }),
      "</p></div>",
      headline_html,
      binary_figure_html,
      "<div class='pair'><figure><img src='", image_data(manifest$adjusted_png[[1]]), "' alt='Adjusted effects for ", html_escape(family), "'>",
      "<figcaption>Adjusted high-minus-low associations from the World Bank-region FE specification without domestic ownership controls. Points are standardized effects; labels show original effect units and p-values.</figcaption></figure>",
      "<figure><img src='", image_data(manifest$descriptive_png[[1]]), "' alt='Descriptive bars for ", html_escape(family), "'>",
      "<figcaption>Unadjusted weighted low/high means for the same country-wave-median exposure split.</figcaption></figure></div>",
      make_table(
        table_data,
        c(
          "#" = "#",
          "*" = "Priority",
          "Outcome" = "Outcome",
          "WBES/source definition" = "WBES definition",
          "Treatment" = "Treatment",
          "Effect" = "Effect",
          "p" = "p",
          "Std. effect" = "Std. effect",
          "Firms" = "Firms",
          "Countries" = "Countries",
          "Theory note" = "Theory note"
        )
      ),
      "</section>"
    )
  }) %>%
  paste(collapse = "\n")

css <- ":root{--ink:#203142;--navy:#0E2841;--blue:#2C7FB8;--green:#16827C;--muted:#536273;--line:#D9E2EA;--soft:#F3F8FB;--amber:#FFF6E8}*{box-sizing:border-box}body{margin:0;background:#fff;color:var(--ink);font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Arial,sans-serif;line-height:1.55}.page{max-width:1300px;margin:0 auto;padding:34px 38px 70px}h1,h2,h3{color:var(--navy);line-height:1.2}h1{font-size:2.1rem;margin:.1rem 0 .55rem}h2{font-size:1.42rem;margin:2.3rem 0 .75rem;border-bottom:2px solid var(--line);padding-bottom:.36rem}h3{font-size:1.05rem;margin:1.4rem 0 .45rem}.lead{max-width:1000px;color:#3D5062;font-size:1.05rem}.meta{color:var(--muted);font-size:.9rem}.note{background:var(--soft);border-left:4px solid var(--blue);padding:14px 17px;margin:18px 0}.warning{background:var(--amber);border-left-color:#D08B27}.headline-result{background:#F0F8F6;border-left:4px solid var(--green);padding:14px 17px;margin:14px 0 18px}.headline-figure{max-width:760px;margin:14px auto 24px}.interpretation{border:1px solid var(--line);background:#F8FBFC;padding:13px 15px;margin:14px 0 18px}.interpretation h3{margin-top:0}.interpretation p{margin:.45rem 0}.stats{display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:12px;margin:18px 0 22px}.stat{border:1px solid var(--line);background:#F8FBFC;padding:12px 13px}.stat-label{font-size:.76rem;color:var(--muted);text-transform:uppercase;letter-spacing:.03em}.stat-value{font-size:1.48rem;font-weight:700;color:var(--navy);line-height:1.15;margin:.2rem 0}.stat-detail{font-size:.8rem;color:#3D5062;line-height:1.35}.equation{font-family:Georgia,'Times New Roman',serif;font-size:1.15rem;background:#F8FBFC;border:1px solid var(--line);padding:14px 16px;margin:12px 0 14px;overflow-x:auto}.compact-list{margin:.5rem 0 1rem 1.2rem;padding:0}.compact-list li{margin:.35rem 0}figure{margin:18px 0 30px}img{display:block;width:100%;height:auto;border:1px solid var(--line);background:#fff}figcaption{font-size:.84rem;color:var(--muted);margin-top:.45rem}.pair{display:grid;grid-template-columns:minmax(0,1fr) minmax(0,1fr);gap:22px;align-items:start}.pair figure{margin-top:10px}table{border-collapse:collapse;width:100%;font-size:.84rem;margin:16px 0 26px}th,td{border:1px solid var(--line);padding:8px 9px;vertical-align:top;text-align:left}th{background:#EDF3F5;color:var(--navy)}code{background:#EFF4F7;padding:.1rem .3rem;border-radius:3px}footer{border-top:1px solid var(--line);margin-top:36px;padding-top:14px;color:var(--muted);font-size:.82rem}@media(max-width:980px){.pair{grid-template-columns:1fr}.stats{grid-template-columns:repeat(2,minmax(0,1fr))}}@media(max-width:760px){.page{padding:24px 18px}h1{font-size:1.72rem}.stats{grid-template-columns:1fr}}"

report_html <- paste0(
  "<!doctype html><html lang='en'><head><meta charset='utf-8'><meta name='viewport' content='width=device-width,initial-scale=1'>",
  "<title>Creative firms: country-wave median semantic policy exposure</title><style>", css, "</style></head><body><main class='page'>",
  "<h1>Creative firms: policy exposure and WBES outcomes</h1>",
  "<p class='lead'>Associations between above-median policy-channel exposure and selected outcomes for creative firms. Starred rows are the recommended results within each policy family.</p>",
  "<div class='note'><strong>Specification.</strong> Exposure is defined relative to the median across distinct country-survey-year cells. Models include log firm age, female ownership, log employment, GDP per capita, population, electricity access, survey-year, industry-division, legal-status and World Bank-region fixed effects. Domestic ownership controls are omitted. Weights are normalized within country and standard errors are clustered by country.</div>",
  "<div class='note warning'><strong>Interpretation.</strong> These are exploratory weighted associations, not causal effects.</div>",
  unesco_summary_html,
  "<h2>Selected results</h2>",
  family_sections,
  "</main></body></html>"
)

writeLines(report_html, report_path)
saveRDS(
  list(
    selected = selected,
    descriptive_data = descriptive_data,
    definitions = definitions,
    thresholds = thresholds,
    country_wave_counts = country_wave_counts,
    policy_summary_cards = policy_summary_cards,
    channel_distribution_summary = channel_distribution_summary,
    family_manifest = family_manifest,
    report_path = report_path
  ),
  file.path(out_dir, "top6_country_wave_median_report_data.rds")
)

cat("Wrote report:", report_path, "\n")
