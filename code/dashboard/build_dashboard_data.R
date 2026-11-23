# Data for the Creative Economy Policy Explorer (docs/index.html).
# Writes docs/data.js (window.EXPLORER_DATA) with aggregates only, no firm-level records.
# Run from the project root: Rscript code/dashboard/build_dashboard_data.R
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(jsonlite); library(countrycode) })

# ---- 1. Policy focus by country and report year ---------------------------
sc <- read.csv("data/semantic_scores/smeco2027_six_channels_transformer/unesco_smeco2027_six_channel_scores_country_year.csv") |>
  filter(country != "CE / no country title", iso2 != "")
sc$iso2[sc$country == "Namibia"] <- "NA"   # read.csv turns "NA" into missing
sc <- sc |> filter(!is.na(iso2))
short <- c(intellectual_property_rights_management = "ip", brand_development_market_positioning = "brand",
           market_access_distribution_commercial_leverage = "market", digital_capabilities_digital_commerce = "digital",
           finance_financial_capability = "finance", sustainability_standards_certification = "sustain")
sc <- sc |> filter(channel %in% names(short)) |> mutate(ch = short[channel])
sc$iso3 <- countrycode(sc$iso2, "iso2c", "iso3c", warn = FALSE)
sc$iso3[sc$iso2 == "XK"] <- "XKX"
policy <- sc |>
  group_by(iso3, country, region = regional_group, year) |>
  summarise(n = first(policy_count), .groups = "drop") |>
  left_join(sc |> select(iso3, year, ch, bounded_score_mean) |>
              pivot_wider(names_from = ch, values_from = bounded_score_mean), by = c("iso3", "year")) |>
  filter(!is.na(iso3)) |> arrange(iso3, year)

# ---- 2. Creative vs other firms, latest WBES wave per country (2019+) -----
codes <- read.csv("data/mappings/itc_creative_isic_codes_rev5_rev4_rev31_only.csv")
r4 <- unique(na.omit(as.integer(unlist(strsplit(as.character(codes$isic_rev4_codes), ";")))))
r31 <- unique(na.omit(as.integer(unlist(strsplit(as.character(codes$isic_rev31_codes), ";")))))

wb_file <- Sys.glob(c(file.path(Sys.getenv("TMPDIR"), "wbes.dta"), "data/raw/wbes/New_Comprehensive_April_29_2026.dta"))[1]
w <- haven::read_dta(wb_file, col_select = c(country, wt, d1a2, d1a2_v4, h1, h5, h8, c22b, b8, k8, k30,
                                             b4, b7a, d3b, d3c)) |> haven::zap_labels()
w <- w |> mutate(
  survey_year = as.integer(substr(country, nchar(country) - 3, nchar(country))),
  wbes_name = trimws(substr(country, 1, nchar(country) - 4)),
  rev4 = ifelse(!is.na(d1a2_v4) & d1a2_v4 > 0 & d1a2_v4 <= 9999, round(d1a2_v4), NA),
  rev31 = ifelse(is.na(rev4) & !is.na(d1a2) & d1a2 > 0, round(d1a2), NA),
  creative = ifelse(!is.na(rev4), rev4 %in% r4, ifelse(!is.na(rev31), rev31 %in% r31, NA))
) |> filter(!is.na(creative), !is.na(wt), wt > 0, survey_year >= 2019)

yn <- function(v) ifelse(v == 1, 1, ifelse(v == 2, 0, NA))
w <- w |> mutate(
  new_product = yn(h1), process = yn(h5), rnd = yn(h8), website = yn(c22b), quality_cert = yn(b8),
  credit = yn(k8), finance_major = ifelse(k30 >= 0 & k30 <= 4, as.numeric(k30 >= 3), NA),
  female_owner = yn(b4), female_manager = yn(b7a),
  exports = ifelse(!is.na(d3b) & !is.na(d3c) & d3b >= 0 & d3c >= 0, as.numeric(d3b + d3c > 0), NA)
)
latest <- w |> group_by(wbes_name) |> summarise(latest = max(survey_year), .groups = "drop")
w <- w |> inner_join(latest, by = "wbes_name") |> filter(survey_year == latest)
w$iso3 <- countrycode(w$wbes_name, "country.name", "iso3c", warn = FALSE,
                      custom_match = c("Kosovo" = "XKX", "West Bank And Gaza" = "PSE", "West Bank and Gaza" = "PSE", "SouthAfrica" = "ZAF"))
unmatched <- unique(w$wbes_name[is.na(w$iso3)])
if (length(unmatched)) message("Unmatched WBES names dropped: ", paste(unmatched, collapse = ", "))
w <- w |> filter(!is.na(iso3))

inds <- c("new_product", "process", "rnd", "website", "exports", "quality_cert", "credit",
          "finance_major", "female_owner", "female_manager")
firms <- w |> pivot_longer(all_of(inds), names_to = "ind", values_to = "v") |> filter(!is.na(v)) |>
  group_by(iso3, survey_year, ind, creative) |>
  summarise(share = weighted.mean(v, wt), n = n(), .groups = "drop") |>
  mutate(grp = ifelse(creative, "c", "o")) |> select(-creative) |>
  pivot_wider(names_from = grp, values_from = c(share, n))
ncre <- w |> group_by(iso3) |> summarise(n_creative = sum(creative), n_all = n(), .groups = "drop")

# ---- 3. Country table ------------------------------------------------------
countries <- tibble(iso3 = unique(c(policy$iso3, firms$iso3))) |>
  mutate(name = countrycode(iso3, "iso3c", "cldr.short.en", warn = FALSE,
                            custom_match = c(XKX = "Kosovo", TWN = "Taiwan, China", HKG = "Hong Kong, China",
                                             MAC = "Macao, China", PSE = "West Bank and Gaza",
                                             COG = "Congo", COD = "DR Congo", TUR = "Türkiye", KOR = "Republic of Korea")),
         wb_region = countrycode(iso3, "iso3c", "region", warn = FALSE, custom_match = c(XKX = "Europe & Central Asia"))) |>
  left_join(policy |> distinct(iso3, region), by = "iso3") |>
  left_join(ncre, by = "iso3") |> arrange(name)

out <- list(
  generated = format(Sys.Date()),
  channels = list(list(id = "ip", label = "Intellectual property"), list(id = "brand", label = "Brand development and positioning"),
                  list(id = "market", label = "Market leverage and access"), list(id = "digital", label = "Digital capabilities and commerce"),
                  list(id = "finance", label = "Finance and financial capability"), list(id = "sustain", label = "Sustainability, standards and certification")),
  indicators = list(
    list(id = "new_product", label = "Introduced new products or services (3 yrs)"),
    list(id = "process", label = "Introduced a new or improved process (3 yrs)"),
    list(id = "rnd", label = "Spent on R&D (last fiscal year)"),
    list(id = "website", label = "Has its own website"),
    list(id = "exports", label = "Exports directly or indirectly"),
    list(id = "quality_cert", label = "Internationally recognized quality certification"),
    list(id = "credit", label = "Has a line of credit or bank loan"),
    list(id = "finance_major", label = "Finance is a major or severe obstacle"),
    list(id = "female_owner", label = "At least one female owner"),
    list(id = "female_manager", label = "Female top manager")),
  countries = countries,
  policy = policy |> mutate(across(c(ip, brand, market, digital, finance, sustain), ~round(.x, 4))),
  firms = firms |> mutate(across(starts_with("share"), ~round(.x, 4)))
)
dir.create("docs", showWarnings = FALSE)
writeLines(paste0("window.EXPLORER_DATA = ", toJSON(out, dataframe = "rows", na = "null", auto_unbox = TRUE), ";"),
           "docs/data.js")
cat("Countries:", nrow(countries), "| policy rows:", nrow(policy), "| firm-indicator rows:", nrow(firms),
    "| countries with firm data:", length(unique(firms$iso3)), "\n")

# ---- 4. Downloadable CSVs (aggregates) -------------------------------------
write.csv(policy |> left_join(countries |> select(iso3, name), by = "iso3") |>
            select(iso3, name, unesco_region = region, report_year = year, measures = n, ip, brand, market, digital, finance, sustain),
          "docs/policy_focus_by_country_year.csv", row.names = FALSE)
write.csv(firms |> left_join(countries |> select(iso3, name, wb_region), by = "iso3") |>
            select(iso3, name, wb_region, survey_year, indicator = ind, share_creative = share_c, n_creative = n_c,
                   share_other = share_o, n_other = n_o),
          "docs/creative_firms_by_country.csv", row.names = FALSE)
