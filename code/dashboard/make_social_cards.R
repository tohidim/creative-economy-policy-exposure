# Three square social-media cards (1080 x 1080 px) for the Creative Economy Policy Explorer.
# Run from the project root after build_dashboard_data.R:  Rscript code/dashboard/make_social_cards.R
suppressPackageStartupMessages({ library(ggplot2); library(dplyr) })
out <- "docs/social"; dir.create(out, recursive = TRUE, showWarnings = FALSE)
col_high <- "#2a78d6"; col_low <- "#a3a29b"; ink <- "#0b0b0b"; ink2 <- "#52514e"; bg <- "#fcfcfb"
f <- "Helvetica Neue"

card_theme <- theme_void(base_family = f) + theme(
  plot.background = element_rect(fill = bg, colour = NA),
  plot.title = element_text(face = "bold", size = 30, colour = ink, lineheight = 1.05, margin = margin(b = 10)),
  plot.subtitle = element_text(size = 17, colour = ink2, lineheight = 1.15, margin = margin(b = 26)),
  plot.caption = element_text(size = 12.5, colour = ink2, hjust = 0, lineheight = 1.2, margin = margin(t = 26)),
  plot.title.position = "plot", plot.caption.position = "plot",
  plot.margin = margin(60, 60, 50, 60))
save_card <- function(p, name) ggsave(file.path(out, name), p, width = 1080, height = 1080, units = "px",
                                      dpi = 100, device = ragg::agg_png)
src <- "Source: UNESCO 2005 Convention policy monitoring platform; World Bank Enterprise Surveys."

# Card 1: finance obstacle, creative firms by finance-policy exposure (paper, Stata results)
d1 <- read.csv("output/tables/chapter4_stata_descriptives.csv") |>
  filter(outcome == "finance_obstacle_any") |>
  mutate(g = factor(ifelse(exposure_group == "High", "Countries where policy\nfocuses more on finance", "Countries where policy\nfocuses less on finance"),
                    levels = c("Countries where policy\nfocuses less on finance", "Countries where policy\nfocuses more on finance")),
         v = 100 * weighted_mean)
p1 <- ggplot(d1, aes(g, v, fill = exposure_group)) +
  geom_col(width = 0.55) +
  geom_text(aes(label = sprintf("%.0f%%", v)), vjust = -0.5, size = 13, family = f, fontface = "bold", colour = ink) +
  scale_fill_manual(values = c(High = col_high, Low = col_low), guide = "none") +
  scale_y_continuous(limits = c(0, 75), expand = c(0, 0)) +
  labs(title = "Creative firms face fewer finance\nobstacles where cultural policy\nfocuses on finance",
       subtitle = "Share of creative-industry firms that report access to finance\nas an obstacle, 72 countries",
       caption = paste0("Below vs above median focus on finance in countries' latest UNESCO policy report. After firm and country\ncontrols the gap is 12 percentage points. Associations, not causal effects.\n", src)) +
  card_theme + theme(axis.text.x = element_text(size = 17, colour = ink2, lineheight = 1.1, margin = margin(t = 10)))
save_card(p1, "card1_finance.png")

# Card 2: recorded policy volume by reporting cycle
d2 <- read.csv("output/figures/figure_3_policy_volume_over_time.csv") |> filter(year %in% c(2012, 2016, 2020, 2024))
p2 <- ggplot(d2, aes(factor(year), average_policy_count)) +
  geom_col(width = 0.6, fill = col_high) +
  geom_text(aes(label = round(average_policy_count)), vjust = -0.5, size = 13, family = f, fontface = "bold", colour = ink) +
  geom_text(aes(y = 0, label = paste0(countries, " countries")), vjust = 1.9, size = 5.5, family = f, colour = ink2) +
  scale_y_continuous(limits = c(-2.5, 38), expand = c(0, 0)) +
  labs(title = "Countries now report more than three\ntimes as many cultural policies\nas in 2012",
       subtitle = "Average number of measures per country in each UNESCO\nquadrennial reporting year",
       caption = paste0("Part of the rise reflects more countries reporting, not only more policies.\n", src)) +
  card_theme + theme(axis.text.x = element_text(size = 19, colour = ink, face = "bold", margin = margin(t = 26)))
save_card(p2, "card2_policy_volume.png")

# Card 3: creative firms vs other firms, new products, countries with >= 30 creative firms
d3 <- read.csv("docs/creative_firms_by_country.csv") |>
  filter(indicator == "new_product", n_creative >= 30, !is.na(share_other)) |>
  mutate(ahead = share_creative > share_other) |> arrange(desc(ahead), desc(share_creative - share_other))
n_ahead <- sum(d3$ahead); n_all <- nrow(d3)
cols <- 13
d3 <- d3 |> mutate(i = row_number() - 1, x = i %% cols, y = -(i %/% cols))
p3 <- ggplot(d3, aes(x, y, colour = ahead)) +
  geom_point(size = 17) +
  scale_colour_manual(values = c(`TRUE` = col_high, `FALSE` = col_low), guide = "none") +
  coord_equal(clip = "off") +
  labs(title = sprintf("In %d of %d countries, creative firms\nare more likely to launch new\nproducts than other firms", n_ahead, n_all),
       subtitle = "Each dot is a country. Blue: creative firms ahead. Grey: other firms\nahead or level. Latest Enterprise Survey, 2019 or later",
       caption = paste0("Share of firms that introduced new products or services in the last three years, survey-weighted.\nCountries with at least 30 creative-industry firms in the survey.\n", src)) +
  card_theme
save_card(p3, "card3_new_products.png")

writeLines(c(
  "# Post captions", "",
  "## card1_finance.png",
  "Where governments' cultural policies put more weight on finance, fewer creative firms say access to finance holds them back: 57% against 63%, and a 12-point gap after controlling for firm size, age and country income. Policy focus is measured by a semantic analysis of 8,155 policy measures reported to UNESCO. #CreativeEconomy #SMEs", "",
  "## card2_policy_volume.png",
  "Countries reported an average of 9 cultural policy measures to UNESCO in 2012 and 32 in 2024. More countries report too, so part of the rise is better coverage. Explore the data by country in the Creative Economy Policy Explorer. #CreativeEconomy #CulturalPolicy", "",
  "## card3_new_products.png",
  sprintf("Creative-industry firms are more likely than other firms to launch new products or services in %d of %d countries with enough survey data. Compare any country in the Creative Economy Policy Explorer. #CreativeEconomy #Innovation", n_ahead, n_all)
), file.path(out, "captions.md"))
cat("Cards written to", out, "| card 3:", n_ahead, "of", n_all, "\n")
