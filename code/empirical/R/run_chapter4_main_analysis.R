#!/usr/bin/env Rscript
# ============================================================
# Chapter 4 main analysis.
# Author: ITC Chapter 4 analysis
# Purpose: Rebuild the canonical sample, descriptive comparisons, and 17 main
#          weighted fixed-effect regressions from the study inputs.
# Inputs: raw WBES, active ITC code list, crosswalk workbook, 72-country
#         mapping, WDI controls, and country-year semantic scores.
# Outputs: comparison tables and saved analysis objects under
#           output/analysis/chapter4_main_analysis/.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(fixest)
  library(haven)
  library(readr)
  library(readxl)
  library(stringr)
  library(tidyr)
})

set.seed(42)
setFixest_notes(FALSE)
setFixest_nthreads(0)

script_arguments <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_arguments[grepl("^--file=", script_arguments)][1])
if (is.na(script_file) || !nzchar(script_file)) script_file <- "code/empirical/R/run_chapter4_main_analysis.R"
root <- normalizePath(file.path(dirname(normalizePath(script_file)), "..", "..", ".."), mustWork = TRUE)
setwd(root)
project_path <- function(...) file.path(root, ...)
require_file <- function(path, label = path) {
  if (!file.exists(path)) stop("Missing ", label, ": ", path, call. = FALSE)
  path
}

out_dir <- project_path("output", "analysis", "chapter4_main_analysis")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

raw_path <- project_path("data", "raw", "wbes", "New_Comprehensive_April_29_2026.dta")
code_path <- project_path("data", "mappings", "itc_creative_isic_codes_rev5_rev4_rev31_only.csv")
crosswalk_path <- project_path("data", "mappings", "chapter4_crosswalks.xlsx")
country_mapping_path <- project_path("data", "external", "analysis_inputs", "creative_only_wb_region_country_mapping.csv")
wdi_path <- project_path("data", "external", "WB_WDI.dta")
score_path <- project_path("data", "semantic_scores", "smeco2027_six_channels_transformer", "unesco_smeco2027_six_channel_scores_country_year.csv")
invisible(lapply(list(raw_path = raw_path, code_path = code_path, crosswalk_path = crosswalk_path, country_mapping_path = country_mapping_path, wdi_path = wdi_path, score_path = score_path), function(x) require_file(x)))

norm_key <- function(x) {
  x <- stringi::stri_trans_nfd(tolower(as.character(x)))
  x <- stringr::str_replace_all(x, "\\p{M}", "")
  stringr::str_replace_all(x, "[^A-Za-z0-9]", "")
}
stata_median <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  as.numeric(stats::quantile(x, probs = 0.5, type = 2, names = FALSE, na.rm = TRUE))
}
stata_percentile <- function(x, p) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  as.numeric(stats::quantile(x, probs = p, type = 2, names = FALSE, na.rm = TRUE))
}
weighted_mean_stata <- function(x, w) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  if (!any(keep)) return(NA_real_)
  weighted.mean(x[keep], w[keep])
}
weighted_sd <- function(x, w) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  if (sum(keep) < 2) return(NA_real_)
  m <- weighted_mean_stata(x[keep], w[keep])
  sqrt(weighted_mean_stata((x[keep] - m)^2, w[keep]))
}

aliases <- read_excel(crosswalk_path, sheet = "country_aliases") %>%
  filter(source_system == "wbes") %>%
  transmute(unesco_key = norm_key(source_key), canonical_unesco_key = norm_key(canonical_key))
eligible_countries <- read_csv(country_mapping_path, show_col_types = FALSE) %>%
  transmute(unesco_key = norm_key(`WBES country`), iso2 = as.character(ISO2)) %>%
  distinct(unesco_key, .keep_all = TRUE)
eligible_countries <- eligible_countries %>%
  left_join(aliases, by = "unesco_key") %>%
  transmute(unesco_key = coalesce(canonical_unesco_key, unesco_key), iso2) %>%
  distinct(unesco_key, .keep_all = TRUE)

iso_map <- read_excel(crosswalk_path, sheet = "iso2_iso3") %>%
  transmute(iso2 = as.character(iso2), iso3 = as.character(iso3)) %>%
  distinct(iso2, .keep_all = TRUE)
wb_regions <- read_excel(crosswalk_path, sheet = "wb_regions") %>%
  transmute(region_code = as.integer(region_code), wb_region = as.character(region_label))
channel_map <- read_excel(crosswalk_path, sheet = "semantic_channels") %>%
  transmute(channel = as.character(channel), channel_short = as.character(channel_short), family = as.character(family))

wdi <- read_dta(wdi_path) %>%
  transmute(
    iso3 = as.character(ISO_code),
    log_wdi_gdp_pc_2022 = if_else(WDI_GDPpc2022_current > 0, log(as.numeric(WDI_GDPpc2022_current)), NA_real_),
    log_wdi_population_2022 = if_else(WDI_population > 0, log(as.numeric(WDI_population)), NA_real_),
    wdi_electricity_access_2022 = as.numeric(WDI_accesstoelectricity)
  ) %>%
  mutate(
    wdi_elec_missing = as.numeric(is.na(wdi_electricity_access_2022)),
    wdi_electricity_access_2022 = if_else(is.na(wdi_electricity_access_2022), stata_median(wdi_electricity_access_2022), wdi_electricity_access_2022)
  ) %>%
  distinct(iso3, .keep_all = TRUE)

creative_codes <- read_csv(code_path, show_col_types = FALSE)
creative_rev4 <- creative_codes %>%
  select(isic_rev4_codes) %>%
  separate_rows(isic_rev4_codes, sep = ";") %>%
  transmute(isic_rev4_code = suppressWarnings(as.integer(str_trim(isic_rev4_codes)))) %>%
  filter(is.finite(isic_rev4_code)) %>% distinct()
creative_rev31 <- creative_codes %>%
  select(isic_rev31_codes) %>%
  separate_rows(isic_rev31_codes, sep = ";") %>%
  transmute(isic_rev31_code = suppressWarnings(as.integer(str_trim(isic_rev31_codes)))) %>%
  filter(is.finite(isic_rev31_code)) %>% distinct()

raw_cols <- c("idstd", "country", "region", "wt", "d1a2", "d1a2_v4", "d2", "n3", "l1", "l2", "l6", "n2a", "d3b", "d3c", "b1", "b2a", "b3", "b4", "b5", "h2", "h5", "r8", "e33", "f1", "d40a", "d14a", "k35", "l11a", "k30", "k21", "k4", "c43", "ge8d")
raw_source <- if (file.exists("/private/tmp/ch4_wbes_for_count.dta")) "/private/tmp/ch4_wbes_for_count.dta" else raw_path
raw <- read_dta(raw_source, col_select = all_of(raw_cols))
clean_numeric <- function(x) { y <- as.numeric(x); y[y < 0] <- NA_real_; y }
numeric_raw <- setdiff(raw_cols, c("idstd", "country"))
raw[numeric_raw] <- lapply(raw[numeric_raw], clean_numeric)
raw <- raw %>%
  mutate(
    idstd = as.character(idstd),
    survey_year = suppressWarnings(as.integer(substr(as.character(country), nchar(as.character(country)) - 3, nchar(as.character(country))))),
    wbes_country_name = substr(as.character(country), 1, nchar(as.character(country)) - 4),
    unesco_key = norm_key(wbes_country_name)
  ) %>%
  left_join(aliases, by = "unesco_key") %>%
  mutate(unesco_key = coalesce(canonical_unesco_key, unesco_key)) %>%
  select(-canonical_unesco_key) %>%
  inner_join(eligible_countries, by = "unesco_key")

binary_vars <- c("h2", "h5", "r8", "e33", "k21", "k4", "c43", "ge8d")
raw[binary_vars] <- lapply(raw[binary_vars], function(x) {
  x[x == 2] <- 0
  x[!(x %in% c(0, 1))] <- NA_real_
  x
})

raw <- raw %>%
  mutate(
    isic_rev4_code = if_else(d1a2_v4 > 0 & d1a2_v4 <= 9999, round(d1a2_v4), NA_real_),
    isic_rev31_code = if_else(is.na(isic_rev4_code) & d1a2 > 0 & d1a2 <= 99999, round(d1a2), NA_real_)
  ) %>%
  left_join(creative_rev4 %>% mutate(creative_rev4 = 1), by = "isic_rev4_code") %>%
  left_join(creative_rev31 %>% mutate(creative_rev31 = 1), by = "isic_rev31_code") %>%
  mutate(
    creative = if_else(!is.na(isic_rev4_code), coalesce(creative_rev4, 0), coalesce(creative_rev31, 0)),
    log_employment_raw = if_else(l1 > 0, log(l1), NA_real_),
    log_employment = log_employment_raw,
    firm_age_years = if_else(!is.na(survey_year) & !is.na(b5), survey_year - b5, NA_real_),
    firm_age_years = if_else(firm_age_years < 0 | firm_age_years > 250, NA_real_, firm_age_years),
    log_firm_age_raw = if_else(!is.na(firm_age_years), log1p(firm_age_years), NA_real_),
    female_owner_raw = case_when(b4 == 1 ~ 1, b4 == 2 ~ 0, TRUE ~ NA_real_),
    log_firm_age_missing = as.numeric(is.na(log_firm_age_raw)),
    female_owner_missing = as.numeric(is.na(female_owner_raw)),
    log_firm_age = log_firm_age_raw,
    female_owner = female_owner_raw,
    legal_status = if_else(b1 %in% 1:6, as.character(b1), "missing"),
    industry_code = coalesce(isic_rev4_code, isic_rev31_code),
    industry_division = if_else(!is.na(industry_code), substr(sprintf("%04d", as.integer(industry_code)), 1, 2), "missing"),
    region_code = coalesce(as.integer(region), -99L)
  ) %>%
  left_join(wb_regions, by = "region_code") %>%
  mutate(
    total_export_share = if_else(!is.na(d3b) & !is.na(d3c), d3b + d3c, NA_real_),
    any_exports = if_else(!is.na(total_export_share), as.numeric(total_export_share > 0), NA_real_),
    finance_obstacle_any = if_else(!is.na(k30), as.numeric(k30 >= 1), NA_real_)
  )

raw <- raw %>% filter(survey_year >= 2021, creative == 1, wt > 0, !is.na(wt))
emp_p1 <- stata_percentile(raw$log_employment_raw, 0.01)
emp_p99 <- stata_percentile(raw$log_employment_raw, 0.99)
med_age <- stata_median(raw$log_firm_age_raw)
med_female <- stata_median(raw$female_owner_raw)
raw <- raw %>%
  mutate(
    log_employment = if_else(!is.na(log_employment), pmin(pmax(log_employment, emp_p1), emp_p99), NA_real_),
    log_firm_age = coalesce(log_firm_age, med_age),
    female_owner = coalesce(female_owner, med_female)
  )

score_long <- read_csv(score_path, show_col_types = FALSE) %>%
  select(country, iso2, year, channel, policy_count, bounded_score_mean) %>%
  rename(policy_country = country) %>%
  filter(policy_country != "CE / no country title") %>%
  inner_join(channel_map %>% select(channel, channel_short), by = "channel") %>%
  distinct(iso2, year, channel, .keep_all = TRUE)

matched <- raw %>%
  select(idstd, iso2, survey_year) %>%
  inner_join(score_long, by = "iso2", relationship = "many-to-many") %>%
  filter(year < survey_year, year >= survey_year - 4) %>%
  arrange(idstd, channel, desc(year)) %>%
  group_by(idstd, channel) %>%
  slice_head(n = 1) %>%
  ungroup()
eligible_ids <- matched %>% count(idstd, name = "n_channels") %>% filter(n_channels == 6) %>% pull(idstd)
if (!length(eligible_ids)) stop("No firms matched to all six channels.")
matched <- matched %>% filter(idstd %in% eligible_ids)
score_meta <- matched %>% arrange(idstd, channel, desc(year)) %>% group_by(idstd) %>% slice_head(n = 1) %>% ungroup() %>% transmute(idstd, policy_country, policy_iso2 = iso2, policy_report_year = year, policy_count)
score_wide <- matched %>% select(idstd, channel_short, bounded_score_mean) %>% pivot_wider(names_from = channel_short, values_from = bounded_score_mean) %>% rename(ip_focus = ip, brand_focus = brand, market_focus = market, digital_focus = digital, finance_focus = finance, sustain_focus = sustain)

firm <- raw %>%
  filter(idstd %in% eligible_ids) %>%
  left_join(score_wide, by = "idstd") %>%
  left_join(score_meta, by = "idstd") %>%
  mutate(
    cluster_country = coalesce(policy_country, wbes_country_name),
    country_year_fe = paste0(cluster_country, "_", survey_year),
    survey_year_fe = as.character(survey_year)
  ) %>%
  left_join(iso_map, by = "iso2") %>%
  left_join(wdi, by = "iso3") %>%
  group_by(cluster_country) %>%
  mutate(analysis_weight = wt / sum(wt, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(analysis_weight > 0, !is.na(analysis_weight), !is.na(policy_count))

exposure_map <- c(policy_count = "policy_count_high", ip_focus = "ip_focus_high", brand_focus = "brand_focus_high", market_focus = "market_focus_high", digital_focus = "digital_focus_high", finance_focus = "finance_focus_high", sustain_focus = "sustain_focus_high")
firm <- firm %>% arrange(country_year_fe)
for (x in names(exposure_map)) {
  cell_values <- firm %>% distinct(country_year_fe, .data[[x]]) %>% pull(.data[[x]])
  threshold <- stata_median(cell_values)
  firm[[exposure_map[[x]]]] <- if_else(!is.na(firm[[x]]), as.numeric(firm[[x]] > threshold), NA_real_)
}

saveRDS(firm, file.path(out_dir, "chapter4_main_sample.rds"))
write_csv(tibble(measure = c("firms", "countries", "country_waves"), value = c(nrow(firm), n_distinct(firm$cluster_country), n_distinct(firm$country_year_fe))), file.path(out_dir, "chapter4_sample_summary.csv"))

descriptive_spec <- tribble(
  ~family, ~outcome, ~exposure,
  "Overall policy count", "h2", "policy_count_high",
  "Overall policy count", "any_exports", "policy_count_high",
  "Intellectual property", "h5", "ip_focus_high",
  "Intellectual property", "r8", "ip_focus_high",
  "Brand development and positioning", "e33", "brand_focus_high",
  "Brand development and positioning", "r8", "brand_focus_high",
  "Brand development and positioning", "f1", "brand_focus_high",
  "Market leverage and access", "d40a", "market_focus_high",
  "Market leverage and access", "d14a", "market_focus_high",
  "Digital capabilities and commerce", "k35", "digital_focus_high",
  "Digital capabilities and commerce", "l11a", "digital_focus_high",
  "Finance and financial capability", "finance_obstacle_any", "finance_focus_high",
  "Finance and financial capability", "k4", "finance_focus_high",
  "Finance and financial capability", "k21", "finance_focus_high",
  "Sustainability, standards and certification", "c43", "sustain_focus_high",
  "Sustainability, standards and certification", "ge8d", "sustain_focus_high",
  "Sustainability, standards and certification", "h5", "sustain_focus_high"
)
descriptive_results <- bind_rows(lapply(seq_len(nrow(descriptive_spec)), function(i) {
  s <- descriptive_spec[i, ]
  bind_rows(lapply(c("High", "Low"), function(group) {
    high <- group == "High"
    d <- firm %>% filter(.data[[s$exposure]] == as.numeric(high), !is.na(.data[[s$outcome]]), !is.na(analysis_weight))
    tibble(family = s$family, outcome = s$outcome, exposure_group = group, weighted_mean = weighted_mean_stata(d[[s$outcome]], d$analysis_weight), firms = nrow(d))
  }))
})) %>% arrange(family, outcome, exposure_group)
write_csv(descriptive_results, file.path(out_dir, "chapter4_descriptives.csv"))
saveRDS(descriptive_results, file.path(out_dir, "chapter4_descriptives.rds"))

controls <- c("log_firm_age", "log_firm_age_missing", "female_owner", "female_owner_missing", "log_employment", "log_wdi_gdp_pc_2022", "log_wdi_population_2022", "wdi_electricity_access_2022", "wdi_elec_missing")
run_regression <- function(s) {
  required <- unique(c(s$outcome, s$exposure, controls, "survey_year_fe", "industry_division", "legal_status", "wb_region", "analysis_weight", "cluster_country", "country_year_fe"))
  d <- firm %>% select(all_of(required)) %>% filter(if_all(everything(), ~ !is.na(.x)), analysis_weight > 0, is.finite(analysis_weight))
  f <- as.formula(paste0(s$outcome, " ~ ", s$exposure, " + ", paste(controls, collapse = " + "), " | survey_year_fe + industry_division + legal_status + wb_region"))
  model <- feols(f, data = d, weights = ~analysis_weight, cluster = ~cluster_country, notes = FALSE, warn = FALSE)
  ct <- as.data.frame(coeftable(model))
  b <- as.numeric(ct[s$exposure, "Estimate"])
  se <- as.numeric(ct[s$exposure, "Std. Error"])
  model_countries <- n_distinct(d$cluster_country)
  tibble(family = s$family, outcome = s$outcome, coefficient = b, std_error = se, p_value = 2 * pt(-abs(b / se), df = model_countries - 1), n = nrow(d), countries = model_countries, country_waves = n_distinct(d$country_year_fe))
}
regression_results <- bind_rows(lapply(seq_len(nrow(descriptive_spec)), function(i) run_regression(descriptive_spec[i, ]))) %>% arrange(family, outcome)
write_csv(regression_results, file.path(out_dir, "chapter4_regressions.csv"))
saveRDS(regression_results, file.path(out_dir, "chapter4_regressions.rds"))

manifest <- list(
  raw_source = raw_source,
  inputs = list(raw = raw_path, code = code_path, crosswalk = crosswalk_path, country_mapping = country_mapping_path, wdi = wdi_path, semantic_scores = score_path),
  sample = list(firms = nrow(firm), countries = n_distinct(firm$cluster_country), country_waves = n_distinct(firm$country_year_fe)),
  descriptive_rows = nrow(descriptive_results),
  regression_rows = nrow(regression_results),
  seed = 42
)
saveRDS(manifest, file.path(out_dir, "run_manifest.rds"))
message("Chapter 4 main analysis complete: ", nrow(firm), " firms, ", n_distinct(firm$cluster_country), " countries, ", n_distinct(firm$country_year_fe), " country-waves")
