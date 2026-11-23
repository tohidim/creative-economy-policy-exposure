#!/usr/bin/env Rscript
# ============================================================
# Chapter 4 HTML report and complete regression scan.
# Author: ITC Chapter 4 analysis
# Purpose: Use the canonical sample and specification for the historical HTML
#          report structure, while exporting every outcome ×
#          exposure × specification regression before the top-six selection.
# Outputs: corrected HTML report, full regression CSV/RDS, selected-results
#          CSV/RDS, thresholds, context, and an Excel-ready data bundle.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(fixest)
  library(readr)
  library(stringr)
  library(tidyr)
})

set.seed(42)
setFixest_notes(FALSE)
setFixest_nthreads(0)

script_arguments <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_arguments[grepl("^--file=", script_arguments)][1])
if (is.na(script_file) || !nzchar(script_file)) script_file <- "code/empirical/R/run_chapter4_report_and_scan.R"
root <- normalizePath(file.path(dirname(normalizePath(script_file)), "..", "..", ".."), mustWork = TRUE)
setwd(root)
project_path <- function(...) file.path(root, ...)
require_file <- function(path, label = path) {
  if (!file.exists(path)) stop("Missing ", label, ": ", path, call. = FALSE)
  path
}

out_dir <- project_path("output", "analysis", "chapter4_report_and_scan")
source_dir <- file.path(out_dir, "report_source")
context_dir <- file.path(out_dir, "report_context")
render_dir <- file.path(out_dir, "rendered")
dir.create(source_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(context_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(render_dir, recursive = TRUE, showWarnings = FALSE)

# Inputs relocated on 2026-11-06 from 99_archive/minimalist_cleanup_20260922/ to data/.
report_inputs <- project_path("data", "report_inputs")
cleaned_outcomes_path <- file.path(report_inputs, "wbes_eligible_outcomes_clean.csv.gz")
outcome_ledger_path <- file.path(report_inputs, "outcome_eligibility_ledger.csv")
old_context_path <- file.path(report_inputs, "adjusted_context.rds")
semantic_long_path <- project_path("data", "semantic_scores", "smeco2027_six_channels_transformer", "unesco_smeco2027_six_channel_scores_long.csv")
semantic_country_year_path <- project_path("data", "semantic_scores", "smeco2027_six_channels_transformer", "unesco_smeco2027_six_channel_scores_country_year.csv")
invisible(lapply(list(cleaned_outcomes_path, outcome_ledger_path, old_context_path, semantic_long_path, semantic_country_year_path), function(x) require_file(x)))

message("Building the canonical analysis sample")
source(project_path("code", "empirical", "R", "run_chapter4_main_analysis.R"), local = new.env(parent = globalenv()))
firm <- readRDS(project_path("output", "analysis", "chapter4_main_analysis", "chapter4_main_sample.rds"))
firm <- firm %>% mutate(row_id = as.character(idstd))

# Keep the long channel names used by the existing report renderer while
# preserving the exact channel values and exposure indicators used in the analysis.
firm <- firm %>% mutate(
  intellectual_property_rights_management = ip_focus,
  brand_development_market_positioning = brand_focus,
  market_access_distribution_commercial_leverage = market_focus,
  digital_capabilities_digital_commerce = digital_focus,
  finance_financial_capability = finance_focus,
  sustainability_standards_certification = sustain_focus,
  intellectual_property_rights_management_high = ip_focus_high,
  brand_development_market_positioning_high = brand_focus_high,
  market_access_distribution_commercial_leverage_high = market_focus_high,
  digital_capabilities_digital_commerce_high = digital_focus_high,
  finance_financial_capability_high = finance_focus_high,
  sustainability_standards_certification_high = sustain_focus_high
)
semantic_regions <- read_csv(semantic_country_year_path, show_col_types = FALSE) %>%
  transmute(policy_iso2 = as.character(iso2), policy_report_year = as.integer(year), regional_group = as.character(regional_group)) %>%
  distinct(policy_iso2, policy_report_year, .keep_all = TRUE)
firm <- firm %>% left_join(semantic_regions, by = c("policy_iso2", "policy_report_year"))
firm <- firm %>% mutate(wdi_electricity_access_2022_missing = wdi_elec_missing)

ledger <- read_csv(outcome_ledger_path, show_col_types = FALSE)
eligible_outcomes <- ledger %>% filter(codebook_eligible) %>% pull(outcome)
cleaned_outcomes <- read_csv(cleaned_outcomes_path, col_types = cols(.default = col_double())) %>%
  select(idstd, any_of(eligible_outcomes)) %>%
  mutate(idstd = as.character(idstd)) %>%
  rename_with(~ paste0("wbes__", .x), -idstd)
firm <- firm %>%
  left_join(cleaned_outcomes, by = "idstd") %>%
  mutate(
    wbes__total_export_share = total_export_share,
    wbes__any_exports = any_exports
  )

outcome_metadata <- readRDS(old_context_path)$full_scan_results %>%
  filter(outcome %in% names(firm)) %>%
  distinct(outcome, .keep_all = TRUE) %>%
  bind_rows(tibble(
    outcome = "finance_obstacle_any",
    original_outcome = "finance_obstacle_any",
    raw_outcome_label = "Any access-to-finance obstacle",
    outcome_type = "binary_yes_no"
  )) %>%
  distinct(outcome, .keep_all = TRUE) %>%
  filter(outcome %in% names(firm))

channels <- c(
  intellectual_property_rights_management = "Intellectual property",
  brand_development_market_positioning = "Brand development and positioning",
  market_access_distribution_commercial_leverage = "Market leverage and access",
  digital_capabilities_digital_commerce = "Digital capabilities and commerce",
  finance_financial_capability = "Finance and financial capability",
  sustainability_standards_certification = "Sustainability, standards and certification"
)
treatments <- bind_rows(
  tibble(scope = "policy_count", channel = NA_character_, channel_label = "Overall policy count", treatment = "policy_count_high"),
  bind_rows(lapply(names(channels), function(channel) tibble(scope = "channel", channel = channel, channel_label = unname(channels[[channel]]), treatment = paste0(gsub("_focus", "", channel), "_high"))))
)
# The exposure-column names above are the short analysis names.
treatments$treatment <- c("policy_count_high", "ip_focus_high", "brand_focus_high", "market_focus_high", "digital_focus_high", "finance_focus_high", "sustain_focus_high")

specs <- tibble(
  spec_id = c("wb_region_with_log_employment", "wb_region_no_log_employment", "unesco_region_with_log_employment", "unesco_region_no_log_employment", "no_region_with_log_employment", "no_region_no_log_employment"),
  spec_label = c("World Bank region FE + log employment", "World Bank region FE, no log employment", "UNESCO region FE + log employment", "UNESCO region FE, no log employment", "No region FE + log employment", "No region FE, no log employment"),
  fixed_effects = list(
    c("survey_year_fe", "industry_division", "legal_status", "wb_region"),
    c("survey_year_fe", "industry_division", "legal_status", "wb_region"),
    c("survey_year_fe", "industry_division", "legal_status", "regional_group"),
    c("survey_year_fe", "industry_division", "legal_status", "regional_group"),
    c("survey_year_fe", "industry_division", "legal_status"),
    c("survey_year_fe", "industry_division", "legal_status")
  ),
  controls = list(
    c("log_firm_age", "log_firm_age_missing", "female_owner", "female_owner_missing", "log_employment"),
    c("log_firm_age", "log_firm_age_missing", "female_owner", "female_owner_missing"),
    c("log_firm_age", "log_firm_age_missing", "female_owner", "female_owner_missing", "log_employment"),
    c("log_firm_age", "log_firm_age_missing", "female_owner", "female_owner_missing"),
    c("log_firm_age", "log_firm_age_missing", "female_owner", "female_owner_missing", "log_employment"),
    c("log_firm_age", "log_firm_age_missing", "female_owner", "female_owner_missing")
  )
)
country_controls <- c("log_wdi_gdp_pc_2022", "log_wdi_population_2022", "wdi_electricity_access_2022", "wdi_elec_missing")
weighted_sd <- function(x, w) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  if (sum(keep) < 2) return(NA_real_)
  mu <- weighted.mean(x[keep], w[keep])
  sqrt(weighted.mean((x[keep] - mu)^2, w[keep]))
}
format_p <- function(x) ifelse(is.na(x), "", ifelse(x < .001, "<0.001", sprintf("%.3f", x)))
format_effect <- function(coef, type, label) {
  label <- str_to_lower(coalesce(label, ""))
  if (!is.finite(coef)) return("")
  if (!is.na(type) && type == "binary_yes_no" && abs(coef) <= 1) return(sprintf("%+.1f pp", 100 * coef))
  if (str_detect(label, "share|percent|percentage|%")) return(sprintf("%+.1f pp", coef))
  if (str_detect(label, "days|duration")) return(sprintf("%+.1f days", coef))
  if (str_detect(label, "hours")) return(sprintf("%+.1f hours", coef))
  sprintf("%+.2f", coef)
}

run_one <- function(outcome_row, treatment_row, spec_row) {
  controls <- spec_row$controls[[1]]
  fixed_effects <- spec_row$fixed_effects[[1]]
  required <- unique(c(outcome_row$outcome, treatment_row$treatment, controls, country_controls, fixed_effects, "analysis_weight", "cluster_country", "country_year_fe"))
  d <- firm %>% select(all_of(required)) %>% filter(if_all(everything(), ~ !is.na(.x)), is.finite(analysis_weight), analysis_weight > 0)
  base <- tibble(
    spec_id = spec_row$spec_id, spec_label = spec_row$spec_label, scope = treatment_row$scope, channel = treatment_row$channel, channel_label = treatment_row$channel_label,
    treatment = treatment_row$treatment, outcome = outcome_row$outcome, original_outcome = outcome_row$original_outcome, raw_outcome_label = outcome_row$raw_outcome_label, outcome_type = outcome_row$outcome_type,
    firms = nrow(d), countries = n_distinct(d$cluster_country), country_waves = n_distinct(d$country_year_fe), treatment_values = n_distinct(d[[treatment_row$treatment]]), outcome_values = n_distinct(d[[outcome_row$outcome]]),
    coefficient = NA_real_, std_error = NA_real_, p_value = NA_real_, model_weighted_sd = NA_real_, standardized_effect = NA_real_, effect_display = NA_character_, p_display = NA_character_, dropped_variables = NA_character_
  )
  if (nrow(d) < 500 || base$countries < 20 || base$treatment_values < 2 || base$outcome_values < 2) return(mutate(base, model_status = "not_estimated", model_detail = "insufficient_sample_or_variation"))
  f <- as.formula(paste0(outcome_row$outcome, " ~ ", treatment_row$treatment, " + ", paste(c(controls, country_controls), collapse = " + "), " | ", paste(fixed_effects, collapse = " + ")))
  model <- tryCatch(feols(f, data = d, weights = ~analysis_weight, cluster = ~cluster_country, notes = FALSE, warn = FALSE), error = function(e) e)
  if (inherits(model, "error")) return(mutate(base, model_status = "not_estimated", model_detail = conditionMessage(model)))
  ct <- as.data.frame(coeftable(model))
  if (!treatment_row$treatment %in% rownames(ct)) return(mutate(base, model_status = "not_estimated", model_detail = "treatment_dropped"))
  b <- as.numeric(ct[treatment_row$treatment, "Estimate"])
  se <- as.numeric(ct[treatment_row$treatment, "Std. Error"])
  p <- 2 * pt(-abs(b / se), df = base$countries - 1)
  sd_y <- weighted_sd(d[[outcome_row$outcome]], d$analysis_weight)
  mutate(base, model_status = "estimated", model_detail = "estimated", coefficient = b, std_error = se, p_value = p, model_weighted_sd = sd_y, standardized_effect = b / sd_y, effect_display = format_effect(b, outcome_row$outcome_type, outcome_row$raw_outcome_label), p_display = format_p(p), dropped_variables = "")
}

grid <- tidyr::crossing(spec_index = seq_len(nrow(specs)), treatment_index = seq_len(nrow(treatments)), outcome_index = seq_len(nrow(outcome_metadata)))
scan_rds <- file.path(source_dir, "all_model_results.rds")
if (file.exists(scan_rds)) {
  all_results <- readRDS(scan_rds)
  if (nrow(all_results) != nrow(grid)) stop("Cached full scan has an unexpected number of rows.")
  message("Reusing completed full scan: ", nrow(all_results), " models")
} else {
  message("Running complete scan: ", nrow(grid), " models")
  results <- vector("list", nrow(grid))
  for (i in seq_len(nrow(grid))) {
    g <- grid[i, ]
    results[[i]] <- run_one(outcome_metadata[g$outcome_index, ], treatments[g$treatment_index, ], specs[g$spec_index, ])
    if (i %% 500 == 0) message("completed ", i, " / ", nrow(grid))
  }
  all_results <- bind_rows(results) %>%
    group_by(spec_id, scope, channel_label) %>%
    mutate(p_fdr_within_family_exposure = if_else(model_status == "estimated", p.adjust(p_value, "BH"), NA_real_)) %>%
    ungroup() %>%
    mutate(significant_10 = model_status == "estimated" & p_value < .10, significant_05 = model_status == "estimated" & p_value < .05)
  write_csv(all_results, file.path(source_dir, "all_model_results.csv"))
  saveRDS(all_results, scan_rds)
}

weighted_median <- function(x, w) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  if (!any(keep)) return(NA_real_)
  o <- order(x[keep]); x <- x[keep][o]; w <- w[keep][o]
  x[which(cumsum(w) >= sum(w) / 2)[1]]
}
thresholds <- bind_rows(lapply(c("policy_count", names(channels)), function(channel) {
  label <- if (channel == "policy_count") "Overall policy count" else unname(channels[[channel]])
  values <- firm %>% distinct(country_year_fe, .data[[channel]]) %>% pull(.data[[channel]])
  tibble(channel = channel, channel_label = label, threshold = median(values, na.rm = TRUE), unweighted_firm_threshold = median(firm[[channel]], na.rm = TRUE), weighted_median_threshold = weighted_median(firm[[channel]], firm$analysis_weight), weighted_mean_threshold = weighted.mean(values, rep(1, length(values)), na.rm = TRUE))
}))
country_wave_counts <- bind_rows(lapply(c("policy_count", names(channels)), function(channel) {
  label <- if (channel == "policy_count") "Overall policy count" else unname(channels[[channel]])
  exposure <- if (channel == "policy_count") "policy_count_high" else paste0(channel, "_high")
  firm %>% distinct(country_year_fe, .data[[exposure]]) %>% transmute(channel_label = label, exposure_group = if_else(.data[[exposure]] == 1, "High", "Low"), country_year_fe) %>% filter(!is.na(exposure_group)) %>% count(channel_label, exposure_group, name = "country_waves")
}))
write_csv(thresholds, file.path(context_dir, "country_wave_median_thresholds.csv"))
write_csv(country_wave_counts, file.path(context_dir, "country_wave_median_high_low_counts.csv"))

data_summary_stats <- tibble(Label = c("Creative firms", "Countries", "Country-survey-year cells", "Policy-count median", "Displayed channel relationships"), Value = c(as.character(nrow(firm)), as.character(n_distinct(firm$cluster_country)), as.character(n_distinct(firm$country_year_fe)), NA_character_, NA_character_), Detail = c("WBES firms in the canonical sample", "Countries represented in the 72-country eligible sample", "Distinct country-wave exposure cells", "", ""))
saveRDS(list(creative_firms = firm, full_scan_results = outcome_metadata, data_summary_stats = data_summary_stats), file.path(context_dir, "adjusted_context.rds"))
saveRDS(list(sample = list(firms = nrow(firm), countries = n_distinct(firm$cluster_country), country_waves = n_distinct(firm$country_year_fe)), outcomes = nrow(outcome_metadata), models = nrow(all_results), all_results = all_results), file.path(out_dir, "run_manifest.rds"))

report_path <- project_path("output", "reports", "unesco_creative_only_country_sample_normalized_wb_region_legal_fe_dummy_smeco2027_refined_semantic_top6_country_wave_median_no_domestic_policy_note_report_REVISED_FINAL_WB_REGION.html")
Sys.setenv(CH4_REPORT_SOURCE_DIR = source_dir, CH4_REPORT_CONTEXT_DIR = context_dir, CH4_REPORT_OUT_DIR = render_dir, CH4_REPORT_PATH = report_path, CH4_REPORT_THRESHOLDS_PATH = file.path(context_dir, "country_wave_median_thresholds.csv"), CH4_REPORT_COUNTS_PATH = file.path(context_dir, "country_wave_median_high_low_counts.csv"), CH4_REPORT_SEMANTIC_LONG_PATH = semantic_long_path)
source(project_path("code", "empirical", "R", "render_chapter4_report.R"), local = new.env(parent = globalenv()))
message("Wrote HTML report: ", report_path)
