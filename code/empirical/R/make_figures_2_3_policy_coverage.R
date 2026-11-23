# ============================================================
# Chapter 4 main text: Figures 2 and 3 (UNESCO policy coverage)
#   Figure 2. UNESCO policy observations by income group
#   Figure 3. Overall UNESCO policy volume over time
# Extracted from the August 2026 script
# build_chapter4_six_channel_policy_firm_performance_document.R so that the
# two figures can be rebuilt from inside this project folder.
#
# Inputs:  data/semantic_scores/smeco2027_six_channels_transformer/
#            unesco_smeco2027_six_channel_scores_country_year.csv (policy_count)
#          data/external/worldbank_country_lending_groups_fy2026.html
# Outputs: output/figures/figure_2_policy_observations_by_income.{png,csv}
#          output/figures/figure_3_policy_volume_over_time.{png,csv}
# Run from the project root:
#   Rscript code/empirical/R/make_figures_2_3_policy_coverage.R
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(rvest)
  library(scales)
  library(stringr)
  library(tidyr)
})

script_args <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", grep("^--file=", script_args, value = TRUE)[1])
script_file <- gsub("~\\+~", " ", script_file)
if (is.na(script_file) || !nzchar(script_file)) script_file <- "code/empirical/R/make_figures_2_3_policy_coverage.R"
root <- normalizePath(file.path(dirname(normalizePath(script_file)), "..", "..", ".."), mustWork = TRUE)

scores_path <- file.path(root, "data", "semantic_scores", "smeco2027_six_channels_transformer",
                         "unesco_smeco2027_six_channel_scores_country_year.csv")
income_path <- file.path(root, "data", "external", "worldbank_country_lending_groups_fy2026.html")
out_dir <- file.path(root, "output", "figures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

normalize_country_key <- function(x) {
  x %>%
    as.character() %>%
    str_to_lower() %>%
    str_trim() %>%
    iconv(to = "ASCII//TRANSLIT") %>%
    str_replace_all(fixed("&"), "and") %>%
    str_replace_all("\\bthe\\b", "") %>%
    str_replace_all("[^a-z0-9]+", "")
}
format_number <- function(x, accuracy = 0.1) scales::number(x, accuracy = accuracy, big.mark = ",")

theme_chapter4 <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.title = element_text(face = "bold", colour = "#1F2937", margin = margin(b = 4)),
      plot.subtitle = element_text(colour = "#4B5563", margin = margin(b = 8)),
      axis.title = element_text(colour = "#374151"),
      axis.text = element_text(colour = "#374151"),
      axis.text.x = element_text(margin = margin(t = 4)),
      axis.text.y = element_text(margin = margin(r = 4)),
      legend.position = "bottom",
      legend.title = element_blank(),
      strip.text = element_text(face = "bold", hjust = 0),
      plot.margin = margin(8, 18, 8, 12)
    )
}

# World Bank FY2026 income classification ----
income_levels <- c("Low income", "Lower-middle income", "Upper-middle income", "High income")
income_tables <- read_html(income_path) %>% html_table(fill = TRUE)
if (length(income_tables) < 12L) stop("Income-group source does not contain the expected World Bank tables.", call. = FALSE)
flatten_income_table <- function(table, group) {
  table %>%
    pivot_longer(everything(), values_to = "country") %>%
    transmute(country = str_squish(str_replace_all(country, "\\n", " ")), income_group = group) %>%
    filter(!is.na(country), country != "")
}
income_lookup <- bind_rows(
  flatten_income_table(income_tables[[9]], "Low income"),
  flatten_income_table(income_tables[[10]], "Lower-middle income"),
  flatten_income_table(income_tables[[11]], "Upper-middle income"),
  flatten_income_table(income_tables[[12]], "High income"),
  tibble::tribble(
    ~country, ~income_group,
    "Bolivia (Plurinational State of)", "Lower-middle income",
    "Egypt", "Lower-middle income",
    "Lao People's Democratic Republic", "Lower-middle income",
    "Netherlands (Kingdom of the)", "High income",
    "Republic of Korea", "High income",
    "Republic of Moldova", "Upper-middle income",
    "Saint Lucia", "Upper-middle income",
    "Saint Vincent and the Grenadines", "Upper-middle income",
    "Slovakia", "High income",
    "State of Palestine", "Lower-middle income",
    "United Kingdom of Great Britain and Northern Ireland", "High income",
    "United Republic of Tanzania", "Lower-middle income",
    "Venezuela (Bolivarian Republic of)", "Upper-middle income"
  )
) %>%
  mutate(country_key = normalize_country_key(country), income_group = factor(income_group, levels = income_levels)) %>%
  distinct(country_key, .keep_all = TRUE) %>%
  select(country_key, income_group)

# Country-year policy counts ----
policy_country_year <- read_csv(scores_path, show_col_types = FALSE) %>%
  distinct(country, iso2, year, policy_count) %>%
  mutate(country_key = normalize_country_key(country)) %>%
  left_join(income_lookup, by = "country_key") %>%
  filter(!is.na(income_group))

policy_income_plot_data <- policy_country_year %>%
  mutate(
    income_group = case_when(
      as.character(income_group) %in% c("Lower-middle income", "Upper-middle income") ~ "Middle income",
      TRUE ~ as.character(income_group)
    ),
    income_group = factor(income_group, levels = c("Low income", "Middle income", "High income"))
  ) %>%
  group_by(income_group) %>%
  summarise(countries = n_distinct(country), country_years = n(),
            average_policy_count = mean(policy_count), .groups = "drop") %>%
  mutate(label = paste0(format_number(average_policy_count), "\n", countries, " countries; ", country_years, " country-years"))

policy_volume_overall_year <- policy_country_year %>%
  group_by(year) %>%
  summarise(average_policy_count = mean(policy_count), country_years = n(),
            countries = n_distinct(country), .groups = "drop") %>%
  arrange(year)

# Figures ----
figure_2 <- policy_income_plot_data %>%
  ggplot(aes(x = income_group, y = average_policy_count)) +
  geom_col(width = 0.62, fill = "#2C7FB8") +
  geom_text(aes(label = label), vjust = -0.18, size = 3.0, lineheight = 0.92, colour = "#1F2937") +
  scale_y_continuous(labels = number_format(accuracy = 1), expand = expansion(mult = c(0, 0.25))) +
  labs(
    title = "Policy observations are more numerous in higher-income groups",
    subtitle = "Bars show the average number of UNESCO policy entries per reporting country-year.",
    x = NULL, y = "Average policy entries per country-year"
  ) +
  theme_chapter4(12) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 20, hjust = 1))

figure_3 <- policy_volume_overall_year %>%
  ggplot(aes(x = year, y = average_policy_count)) +
  geom_line(linewidth = 1.0, colour = "#2C7FB8") +
  geom_point(size = 2.15, colour = "#2C7FB8") +
  scale_x_continuous(breaks = policy_volume_overall_year$year) +
  scale_y_continuous(labels = number_format(accuracy = 0.1)) +
  labs(
    title = "Overall policy volume rises in recent reporting years",
    subtitle = "Line shows the average number of UNESCO policy entries per reporting country-year.",
    x = NULL, y = "Average policy entries"
  ) +
  theme_chapter4(12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(out_dir, "figure_2_policy_observations_by_income.png"), figure_2, width = 7.2, height = 4.5, dpi = 300, bg = "white")
ggsave(file.path(out_dir, "figure_3_policy_volume_over_time.png"), figure_3, width = 7.2, height = 4.7, dpi = 300, bg = "white")
write_csv(policy_income_plot_data, file.path(out_dir, "figure_2_policy_observations_by_income.csv"))
write_csv(policy_volume_overall_year, file.path(out_dir, "figure_3_policy_volume_over_time.csv"))

print(policy_income_plot_data %>% select(-label))
print(policy_volume_overall_year, n = Inf)
