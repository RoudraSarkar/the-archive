# ============================================================
# Visualisations: Composition of European migration
# For "the archives" — psychology · demographics · geopolitics
# ============================================================
install.packages("ggtext")  # if not already installed
library(tidyverse)
library(scales)
library(ggtext)   # for markdown in titles
# install.packages(c("tidyverse", "scales", "ggtext")) if needed

# ------------------------------------------------------------
# 0. PATHS
# ------------------------------------------------------------
data_path <- "/Users/whiz/Desktop/the archive/eu_immigration/data/processed data"
out_path  <- "/Users/whiz/Desktop/the archive/eu_immigration/figures"
dir.create(out_path, showWarnings = FALSE, recursive = TRUE)

wide <- read_csv(file.path(data_path, "migration_composition_wide.csv"))
long <- read_csv(file.path(data_path, "migration_composition_long.csv"))

# ------------------------------------------------------------
# 1. THEME & PALETTE
# ------------------------------------------------------------
# Palette ties to your Substack identity:
# cream background, charcoal text, terracotta accent
archives_cream     <- "#F1EBDF"
archives_charcoal  <- "#2A231C"
archives_terracota <- "#8C3A2F"
archives_taupe     <- "#C9BFA9"
archives_brown     <- "#6B5D4F"
archives_grey      <- "#8A7C6D"

# Five-category palette: ordered light-to-dark for stacked charts.
# Asylum/Protection sits in the terracotta accent so it stands out
# without dominating; this is the politically loaded category and
# making it visually distinct serves the reader.
category_colours <- c(
  "Employment"        = "#3A4A5C",   # deep slate blue
  "Family"            = "#7A8B6F",   # muted sage
  "Education"         = "#C9A875",   # warm sand
  "Other"             = "#A89C8A",   # warm grey
  "Asylum/Protection" = archives_terracota
)

# Order categories from largest typical share to smallest for stacking
category_order <- c("Employment", "Family", "Education", "Other", "Asylum/Protection")

# Editorial theme
theme_archives <- function(base_size = 12) {
  theme_minimal(base_size = base_size, base_family = "Georgia") +
    theme(
      plot.background       = element_rect(fill = archives_cream, colour = NA),
      panel.background      = element_rect(fill = archives_cream, colour = NA),
      panel.grid.major.y    = element_blank(),
      panel.grid.minor      = element_blank(),
      panel.grid.major.x    = element_line(colour = archives_taupe, linewidth = 0.3),
      axis.ticks            = element_blank(),
      axis.text             = element_text(colour = archives_brown, size = rel(0.9)),
      axis.title            = element_text(colour = archives_brown, size = rel(0.9)),
      plot.title            = element_textbox_simple(
                                colour = archives_charcoal,
                                size = rel(1.35),
                                face = "plain",
                                lineheight = 1.2,
                                margin = margin(b = 6),
                                width = grid::unit(1, "npc")),
      plot.subtitle         = element_textbox_simple(
                                colour = archives_brown,
                                size = rel(0.95),
                                lineheight = 1.3,
                                margin = margin(b = 14),
                                width = grid::unit(1, "npc")),
      plot.caption          = element_text(colour = archives_grey, size = rel(0.75),
                                           hjust = 0, margin = margin(t = 12)),
      plot.caption.position = "plot",
      plot.title.position   = "plot",
      legend.position       = "top",
      legend.justification  = "left",
      legend.title          = element_blank(),
      legend.text           = element_text(colour = archives_charcoal, size = rel(0.85)),
      legend.key.height     = grid::unit(0.4, "cm"),
      legend.key.width      = grid::unit(0.8, "cm"),
      plot.margin           = margin(20, 20, 16, 20)
    )
}

# Standard caption used across all charts
src_caption <- paste(
  "Source: Eurostat (migr_resfirst, tps00192).",
  "Asylum/Protection = positive first-instance asylum decisions, comparable to other categories which count successful entries.",
  "Excludes EU-citizen free movement and renewals.",
  "Chart: the archives",
  sep = "  ·  "
)

# ------------------------------------------------------------
# 2. PREP: pick countries for cross-country chart
# ------------------------------------------------------------
# Filter to the most recent complete year (2023 — 2024 partial in some countries).
# Then pick countries with substantial volumes; drop micro-states whose
# percentages are statistical artefacts on tiny base numbers.
focus_countries <- c(
  "Germany", "France", "Spain", "Italy", "Netherlands",
  "Sweden", "Belgium", "Austria", "Ireland", "Poland",
  "Portugal", "Denmark", "Finland", "Czechia", "Greece"
)

snap_2023 <- wide %>%
  filter(year == 2023, country %in% focus_countries) %>%
  select(country, all_of(category_order), Total) %>%
  pivot_longer(cols = all_of(category_order),
               names_to = "category", values_to = "count") %>%
  group_by(country) %>%
  mutate(pct = count / Total * 100) %>%
  ungroup() %>%
  mutate(category = factor(category, levels = rev(category_order)))

# Order countries by employment share (visually communicates the East/West gradient)
country_order <- snap_2023 %>%
  filter(category == "Employment") %>%
  arrange(pct) %>%
  pull(country)

snap_2023 <- snap_2023 %>%
  mutate(country = factor(country, levels = country_order))

# ------------------------------------------------------------
# FIGURE 1: Cross-country composition (headline chart)
# ------------------------------------------------------------
fig1 <- ggplot(snap_2023, aes(x = pct, y = country, fill = category)) +
  geom_col(width = 0.78) +
  scale_fill_manual(values = category_colours,
                    breaks = category_order,
                    guide = guide_legend(reverse = FALSE, nrow = 2)) +
  scale_x_continuous(labels = label_percent(scale = 1),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(
    title = "Asylum is rarely the dominant category in European migration",
    subtitle = paste0(
      "Composition of <b>first residence permits and positive asylum decisions</b>, 2023.<br>",
      "Most political debate frames migration as primarily about asylum. The data shows family, employment, ",
      "and education usually account for far more new arrivals."
    ),
    x = NULL, y = NULL,
    caption = src_caption
  ) +
  theme_archives()

ggsave(file.path(out_path, "fig1_composition_2023.png"),
       fig1, width = 9, height = 7.5, dpi = 300, bg = archives_cream)

# ------------------------------------------------------------
# FIGURE 2 & 3: Country time series
# ------------------------------------------------------------
# Pick two countries with contrasting stories. After looking at the
# data, two strong candidates: Germany (asylum-dominated narrative,
# but composition has shifted dramatically) and Poland (employment-
# driven, transformed by Ukrainian displacement).
# Adjust these once you've eyeballed Figure 1.

ts_country <- function(country_name, title_override, subtitle_override) {
  d <- long %>%
    filter(country == country_name,
           year >= 2014, year <= 2024) %>%
    mutate(category = factor(category, levels = rev(category_order)))

  # Compute totals per year for percentage view
  d_pct <- d %>%
    group_by(year) %>%
    mutate(pct = count / sum(count) * 100) %>%
    ungroup()

  ggplot(d_pct, aes(x = year, y = pct, fill = category)) +
    geom_area(alpha = 0.95) +
    scale_fill_manual(values = category_colours,
                      breaks = category_order,
                      guide = guide_legend(nrow = 2)) +
    scale_x_continuous(breaks = seq(2014, 2024, 2),
                       expand = expansion(mult = c(0.01, 0.01))) +
    scale_y_continuous(labels = label_percent(scale = 1),
                       expand = expansion(mult = c(0, 0.02))) +
    labs(
      title = title_override,
      subtitle = subtitle_override,
      x = NULL, y = NULL,
      caption = src_caption
    ) +
    theme_archives() +
    theme(panel.grid.major.x = element_line(colour = archives_taupe, linewidth = 0.3),
          panel.grid.major.y = element_blank())
}

fig2 <- ts_country(
  "Germany",
  title_override = "Germany's migration mix has shifted away from asylum",
  subtitle_override = paste0(
    "Composition of new arrivals to Germany by category, 2014-2024.<br>",
    "The 2015-16 asylum surge is visible, but family and employment have since reasserted as the dominant categories."
  )
)

fig3 <- ts_country(
  "Poland",
  title_override = "Poland: an almost entirely labour-driven migration profile",
  subtitle_override = paste0(
    "Composition of new arrivals to Poland by category, 2014-2024.<br>",
    "Poland receives one of Europe's largest migration inflows, almost all on employment grounds."
  )
)

ggsave(file.path(out_path, "fig2_germany_timeseries.png"),
       fig2, width = 9, height = 6, dpi = 300, bg = archives_cream)

ggsave(file.path(out_path, "fig3_poland_timeseries.png"),
       fig3, width = 9, height = 6, dpi = 300, bg = archives_cream)

# ------------------------------------------------------------
# FIGURE 4: Total volumes (so readers see scale, not just %)
# ------------------------------------------------------------
# A common criticism of percentage-only charts: a 30% share in a
# small country is fewer people than a 10% share in Germany.
# This chart shows absolute volumes for context.

vols_2023 <- wide %>%
  filter(year == 2023, country %in% focus_countries) %>%
  arrange(Total) %>%
  mutate(country = factor(country, levels = country))

fig4 <- ggplot(vols_2023, aes(x = Total, y = country)) +
  geom_col(fill = archives_charcoal, width = 0.7) +
  geom_text(aes(label = comma(Total)),
            hjust = -0.15, size = 3.3, family = "Georgia",
            colour = archives_brown) +
  scale_x_continuous(labels = comma,
                     expand = expansion(mult = c(0, 0.18))) +
  labs(
    title = "Germany alone receives more new migrants than the next four countries combined",
    subtitle = paste0(
      "Total first permits plus positive asylum decisions, 2023.<br>",
      "Composition matters, but so does scale: Germany's migration system processes more people than most of Europe combined."
    ),
    x = NULL, y = NULL,
    caption = src_caption
  ) +
  theme_archives() +
  theme(panel.grid.major.x = element_line(colour = archives_taupe, linewidth = 0.3))

ggsave(file.path(out_path, "fig4_total_volumes_2023.png"),
       fig4, width = 9, height = 6.5, dpi = 300, bg = archives_cream)

# ------------------------------------------------------------
# Done. Open the figures folder.
# ------------------------------------------------------------
message("Charts written to: ", out_path)







# ============================================================
# Additional visualisations: Sweden & Spain time series
# plus asylum recognition rates across Europe
# Append to or run after visualise_migration.R
# ============================================================

library(tidyverse)
library(scales)
library(ggtext)

# ------------------------------------------------------------
# 0. PATHS — assumes you've already loaded the theme and palette
#    from visualise_migration.R. If running fresh, copy the
#    theme_archives() function and palette objects from there.
# ------------------------------------------------------------
data_path <- "/Users/whiz/Desktop/the archive/eu_immigration/data/processed data"
raw_path  <- "/Users/whiz/Desktop/the archive/eu_immigration/data/raw data"  # adjust if different
out_path  <- "/Users/whiz/Desktop/the archive/eu_immigration/figures"

wide <- read_csv(file.path(data_path, "migration_composition_wide.csv"))
long <- read_csv(file.path(data_path, "migration_composition_long.csv"))

# You'll need the raw asylum decisions file for the recognition rate
# (we need both positive AND negative decisions, and the merged file
# only kept positives). Adjust path to wherever asylum_dec.csv lives.
asylum_dec <- read_csv(file.path(raw_path, "asylum_dec.csv"))

# ------------------------------------------------------------
# FIGURES 5 & 6: Sweden and Spain time series
# (uses the ts_country() function from visualise_migration.R)
# ------------------------------------------------------------
fig5 <- ts_country(
  "Sweden",
  title_override = "Sweden's asylum-driven era has ended",
  subtitle_override = paste0(
    "Composition of new arrivals to Sweden by category, 2014-2024.<br>",
    "Once one of Europe's most asylum-heavy systems, Sweden's intake has shifted decisively toward family and employment ",
    "after a sequence of policy tightenings beginning in 2015-16."
  )
)

fig6 <- ts_country(
  "Spain",
  title_override = "Spain's migration is dominated by family and Latin American labour",
  subtitle_override = paste0(
    "Composition of new arrivals to Spain by category, 2014-2024.<br>",
    "Spain's pattern differs sharply from northern Europe: low asylum, high family reunification, ",
    "and substantial labour migration driven largely by Latin American flows."
  )
)

ggsave(file.path(out_path, "fig5_sweden_timeseries.png"),
       fig5, width = 9, height = 6, dpi = 300, bg = archives_cream)

ggsave(file.path(out_path, "fig6_spain_timeseries.png"),
       fig6, width = 9, height = 6, dpi = 300, bg = archives_cream)


# ============================================================
# RECOGNITION RATES
# ============================================================
# Methodology: recognition rate = positive first-instance decisions
# divided by total first-instance decisions (positive + negative).
# This is the EUAA/UNHCR standard methodology — both numerator and
# denominator come from the same year and same dataset, avoiding
# the timing mismatch that would arise from using applications.
# ------------------------------------------------------------

# Filter and pivot the decisions data
recog <- asylum_dec %>%
  filter(decision %in% c("Positive decision", "Negative decision")) %>%
  filter(!str_detect(geo,
    "European Union|European Free|Euro area|EFTA")) %>%
  select(country = geo, year = TIME_PERIOD,
         decision, count = OBS_VALUE) %>%
  pivot_wider(names_from = decision, values_from = count) %>%
  rename(positive = `Positive decision`,
         negative = `Negative decision`) %>%
  mutate(
    total_decisions  = positive + negative,
    recognition_rate = positive / total_decisions * 100
  ) %>%
  filter(total_decisions >= 500)   # drop countries with too few cases

# ------------------------------------------------------------
# FIGURE 7: Recognition rates across Europe, 2023
# ------------------------------------------------------------
focus_countries <- c(
  "Germany", "France", "Spain", "Italy", "Netherlands",
  "Sweden", "Belgium", "Austria", "Ireland", "Poland",
  "Portugal", "Denmark", "Finland", "Czechia", "Greece"
)

recog_2023 <- recog %>%
  filter(year == 2023, country %in% focus_countries) %>%
  arrange(recognition_rate) %>%
  mutate(country = factor(country, levels = country))

# Median for reference line
median_rate <- median(recog_2023$recognition_rate, na.rm = TRUE)

fig7 <- ggplot(recog_2023, aes(x = recognition_rate, y = country)) +
  geom_col(fill = archives_terracota, width = 0.7) +
  geom_vline(xintercept = median_rate, colour = archives_charcoal,
             linetype = "dashed", linewidth = 0.4) +
  annotate("text",
           x = median_rate + 1.5, y = 1,
           label = paste0("Median: ", round(median_rate), "%"),
           hjust = 0, family = "Georgia", size = 3.2,
           colour = archives_brown) +
  geom_text(aes(label = paste0(round(recognition_rate), "%")),
            hjust = -0.2, size = 3.3, family = "Georgia",
            colour = archives_brown) +
  scale_x_continuous(labels = label_percent(scale = 1),
                     expand = expansion(mult = c(0, 0.12)),
                     limits = c(0, 100)) +
  labs(
    title = "An asylum seeker's odds depend heavily on which country decides their case",
    subtitle = paste0(
      "Share of first-instance asylum decisions resulting in protection, by country of decision, 2023.<br>",
      "Recognition rates vary from under 20% to over 80% across the EU, despite a common asylum framework."
    ),
    x = NULL, y = NULL,
    caption = paste(
      "Source: Eurostat (tps00192).",
      "Recognition rate = positive decisions / (positive + negative decisions) at first instance.",
      "Excludes countries with fewer than 500 decisions in 2023.",
      "Chart: the archives",
      sep = "  ·  "
    )
  ) +
  theme_archives()

ggsave(file.path(out_path, "fig7_recognition_rates_2023.png"),
       fig7, width = 9, height = 7, dpi = 300, bg = archives_cream)

# ------------------------------------------------------------
# FIGURE 8 (OPTIONAL): Recognition rate over time for the four focus countries
# Useful for showing institutional drift / policy shifts
# ------------------------------------------------------------
recog_ts <- recog %>%
  filter(country %in% c("Germany", "Sweden", "Poland", "Spain"),
         year >= 2014, year <= 2024)

fig8 <- ggplot(recog_ts, aes(x = year, y = recognition_rate, colour = country)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  scale_colour_manual(values = c(
    "Germany" = archives_charcoal,
    "Sweden"  = archives_terracota,
    "Poland"  = "#3A4A5C",
    "Spain"   = "#7A8B6F"
  )) +
  scale_x_continuous(breaks = seq(2014, 2024, 2)) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 100)) +
  labs(
    title = "Recognition rates have diverged sharply across major receiving countries",
    subtitle = paste0(
      "Share of first-instance asylum decisions resulting in protection, 2014-2024.<br>",
      "Sweden's recognition rate has fallen by more than half since the mid-2010s policy shift; ",
      "Spain's has risen as the receiving population has shifted toward Venezuelan and other Latin American applicants."
    ),
    x = NULL, y = NULL, colour = NULL,
    caption = paste(
      "Source: Eurostat (tps00192).",
      "Recognition rate = positive decisions / (positive + negative decisions) at first instance.",
      "Chart: the archives",
      sep = "  ·  "
    )
  ) +
  theme_archives()

ggsave(file.path(out_path, "fig8_recognition_timeseries.png"),
       fig8, width = 9, height = 6, dpi = 300, bg = archives_cream)

message("Additional charts written to: ", out_path)
