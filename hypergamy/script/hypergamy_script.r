# ============================================================
# Visualisations: A Generation of Change — four trends
# For "the archives" — psychology · demographics · geopolitics
# ============================================================
# Replicates theme_archives() exactly as used in the EU migration
# piece. Produces 5 publication-ready PNGs (9in wide, 300 dpi,
# cream background, Georgia serif).
#
# install.packages(c("tidyverse", "scales", "ggtext"))
library(tidyverse)
library(scales)
library(ggtext)

# ------------------------------------------------------------
# 0. PATHS  — edit these two if your folders differ
# ------------------------------------------------------------
data_path <- "/Users/whiz/Desktop/the archive/hypergamy/data/processed data"
out_path  <- "/Users/whiz/Desktop/the archive/hypergamy/figures"
dir.create(out_path, showWarnings = FALSE, recursive = TRUE)

# The two CSVs bundled alongside this script. Put them in data_path,
# or change these lines to point wherever you save them.
panel <- read_csv(file.path(data_path, "trends_data.csv"))
wvs   <- read_csv(file.path(data_path, "wvs_jobs.csv"))

# ------------------------------------------------------------
# 1. THEME & PALETTE  (identical to the migration piece)
# ------------------------------------------------------------
archives_cream     <- "#F1EBDF"
archives_charcoal  <- "#2A231C"
archives_terracota <- "#8C3A2F"
archives_taupe     <- "#C9BFA9"
archives_brown     <- "#6B5D4F"
archives_grey      <- "#8A7C6D"

# Categorical line palette. Terracotta is RESERVED for the salient
# series in each chart (the country the surrounding text is about),
# exactly as Asylum/Protection was singled out in the migration piece.
line_palette <- c(
  "#3A4A5C",  # deep slate blue
  "#7A8B6F",  # muted sage
  "#C9A875",  # warm sand
  "#A89C8A",  # warm grey
  "#6B5D4F",  # brown
  archives_terracota
)

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
                                colour = archives_charcoal, size = rel(1.35),
                                face = "plain", lineheight = 1.2,
                                margin = margin(b = 6),
                                width = grid::unit(1, "npc")),
      plot.subtitle         = element_textbox_simple(
                                colour = archives_brown, size = rel(0.95),
                                lineheight = 1.3, margin = margin(b = 14),
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

cap_edu <- paste(
  "Source: Eurostat (EDAT_LFSE_03); OECD attainment database for non-EU members and Japan.",
  "Tertiary = ISCED 2011 levels 5-8, ages 25-34.",
  "Chart: the archives", sep = "  ·  ")
cap_tfr <- paste(
  "Source: UN World Population Prospects 2024.",
  "Chart: the archives", sep = "  ·  ")
cap_mar <- paste(
  "Source: Eurostat (DEMO_NIND).",
  "Mean age at first marriage, women.",
  "Chart: the archives", sep = "  ·  ")
cap_wvs <- paste(
  "Source: World Values Survey Time-Series 1981-2022.",
  "Survey waves are irregular; lines interpolate between them.",
  "Chart: the archives", sep = "  ·  ")

# ============================================================
# FIGURE 1 — The educational reversal (six countries)
# ============================================================
sel1 <- c("KOR","JPN","SWE","USA","ITA","DEU")
d1 <- panel %>%
  filter(ISO3 %in% sel1) %>%
  select(ISO3, country, Year, hypergamy_gap) %>%
  drop_na(hypergamy_gap)

# Terracotta reserved for South Korea (the country the text opens on)
col1 <- c(
  "South Korea"   = archives_terracota,
  "Japan"         = "#3A4A5C",
  "Sweden"        = "#7A8B6F",
  "United States" = "#C9A875",
  "Italy"         = "#A89C8A",
  "Germany"       = archives_brown
)

fig1 <- ggplot(d1, aes(Year, hypergamy_gap, colour = country)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             colour = archives_grey, linewidth = 0.4) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = col1) +
  scale_x_continuous(breaks = seq(1992, 2024, 8),
                     expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(labels = label_number(suffix = " pp")) +
  labs(
    title = "The great educational reversal",
    subtitle = paste0(
      "Female minus male tertiary attainment, ages 25-34, <b>1992-2024</b>.<br>",
      "Every country crosses from male advantage or parity into a female lead — ",
      "but Sweden climbs steeply while Japan barely moves."),
    x = NULL, y = NULL, colour = NULL, caption = cap_edu
  ) +
  theme_archives()

ggsave(file.path(out_path, "fig1_education_reversal.png"),
       fig1, width = 9, height = 6, dpi = 300, bg = archives_cream)

# ============================================================
# FIGURE 2 — Small multiples: same gap, twelve countries
# ============================================================
sel2 <- c("SVN","LVA","NOR","POL","SWE","ITA","USA","JPN","GBR","DEU","KOR","MEX")
d2 <- panel %>%
  filter(ISO3 %in% sel2) %>%
  select(ISO3, country, Year, hypergamy_gap) %>%
  drop_na(hypergamy_gap)

# Order facets by most recent gap (largest first)
ord2 <- d2 %>% group_by(country) %>%
  filter(Year == max(Year)) %>%
  arrange(desc(hypergamy_gap)) %>% pull(country)
d2 <- d2 %>% mutate(country = factor(country, levels = ord2))

fig2 <- ggplot(d2, aes(Year, hypergamy_gap)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             colour = archives_grey, linewidth = 0.3) +
  geom_area(fill = "#7A8B6F", alpha = 0.25) +
  geom_line(colour = "#3A4A5C", linewidth = 0.8) +
  facet_wrap(~country, ncol = 4) +
  scale_x_continuous(breaks = c(2000, 2020)) +
  scale_y_continuous(labels = label_number(suffix = " pp")) +
  labs(
    title = "Same direction, twelve different journeys",
    subtitle = paste0(
      "Female minus male tertiary attainment, ages 25-34. ",
      "Dashed line = <b>parity</b>. Panels ordered by 2024 gap.<br>",
      "The Baltics and Nordics open the widest gaps; Japan and Korea, ",
      "despite the fertility headlines, sit toward the narrower end."),
    x = NULL, y = NULL, caption = cap_edu
  ) +
  theme_archives() +
  theme(strip.text = element_text(colour = archives_charcoal,
                                  family = "Georgia", size = rel(0.85)),
        panel.spacing = grid::unit(0.9, "lines"))

ggsave(file.path(out_path, "fig2_reversal_small_multiples.png"),
       fig2, width = 9, height = 7, dpi = 300, bg = archives_cream)

# ============================================================
# FIGURE 3 — People marrying later
# ============================================================
sel3 <- c("SWE","ESP","ITA","HUN","POL")
d3 <- panel %>%
  filter(ISO3 %in% sel3) %>%
  select(ISO3, country, Year, mafm_F) %>%
  drop_na(mafm_F)

col3 <- c(
  "Sweden"  = "#3A4A5C",
  "Spain"   = "#7A8B6F",
  "Italy"   = archives_terracota,   # reserved: the steepest climber, text focus
  "Hungary" = "#C9A875",
  "Poland"  = "#A89C8A"
)

fig3 <- ggplot(d3, aes(Year, mafm_F, colour = country)) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = col3) +
  scale_x_continuous(breaks = seq(1992, 2024, 8)) +
  scale_y_continuous(labels = label_number(suffix = " yrs")) +
  labs(
    title = "People started marrying later — everywhere",
    subtitle = paste0(
      "Mean age at first marriage, <b>women</b>, 1990s-2024.<br>",
      "Whether a country started low or high, the line goes up. ",
      "Sweden and Spain now exceed 35."),
    x = NULL, y = NULL, colour = NULL, caption = cap_mar
  ) +
  theme_archives()

ggsave(file.path(out_path, "fig3_marriage_age.png"),
       fig3, width = 9, height = 6, dpi = 300, bg = archives_cream)

# ============================================================
# FIGURE 4 — Birth rates fell, but not in lockstep
# ============================================================
sel4 <- c("KOR","JPN","ITA","USA","SWE","FRA")
d4 <- panel %>%
  filter(ISO3 %in% sel4) %>%
  select(ISO3, country, Year, TFR) %>%
  drop_na(TFR)

col4 <- c(
  "South Korea"   = archives_terracota,  # reserved: the extreme case
  "Japan"         = "#3A4A5C",
  "Italy"         = "#7A8B6F",
  "United States" = "#C9A875",
  "Sweden"        = "#A89C8A",
  "France"        = archives_brown
)

fig4 <- ggplot(d4, aes(Year, TFR, colour = country)) +
  geom_hline(yintercept = 2.1, linetype = "dashed",
             colour = archives_grey, linewidth = 0.4) +
  annotate("text", x = 1993, y = 2.16, label = "replacement \u2248 2.1",
           hjust = 0, family = "Georgia", size = 3,
           colour = archives_grey) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = col4) +
  scale_x_continuous(breaks = seq(1992, 2024, 8)) +
  labs(
    title = "Birth rates fell — but not in lockstep",
    subtitle = paste0(
      "Total fertility rate, <b>1992-2024</b>.<br>",
      "Korea drops almost without interruption; Japan stays low but flat; ",
      "France, Sweden and the US rose through the 2000s before turning down."),
    x = NULL, y = NULL, colour = NULL, caption = cap_tfr
  ) +
  theme_archives()

ggsave(file.path(out_path, "fig4_fertility_trends.png"),
       fig4, width = 9, height = 6, dpi = 300, bg = archives_cream)

# ============================================================
# FIGURE 5 — Attitudes shifted too (WVS)
# ============================================================
sel5 <- c("KOR","JPN","DEU","USA","SWE")
nm5  <- c(KOR="South Korea", JPN="Japan", DEU="Germany",
          USA="United States", SWE="Sweden")
d5 <- wvs %>%
  filter(ISO3 %in% sel5) %>%
  drop_na(norm_jobs) %>%
  mutate(country = nm5[ISO3],
         pct = norm_jobs * 100) %>%
  arrange(country, Year)

col5 <- c(
  "South Korea"   = archives_terracota,  # reserved: the against-trend case
  "Japan"         = "#3A4A5C",
  "Germany"       = "#7A8B6F",
  "United States" = "#C9A875",
  "Sweden"        = "#A89C8A"
)

fig5 <- ggplot(d5, aes(Year, pct, colour = country)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  scale_colour_manual(values = col5) +
  scale_x_continuous(breaks = seq(1990, 2020, 6)) +
  scale_y_continuous(labels = label_number(suffix = "%"),
                     limits = c(0, 56)) +
  labs(
    title = "Attitudes about gender shifted too",
    subtitle = paste0(
      "Share agreeing <b>\u201cmen should have more right to a job than women\u201d</b>, ",
      "by WVS wave.<br>Agreement fell in most sampled countries; ",
      "South Korea runs against the trend in its most recent wave."),
    x = NULL, y = NULL, colour = NULL, caption = cap_wvs
  ) +
  theme_archives()

ggsave(file.path(out_path, "fig5_wvs_attitudes.png"),
       fig5, width = 9, height = 6, dpi = 300, bg = archives_cream)

message("All 5 figures written to: ", out_path)