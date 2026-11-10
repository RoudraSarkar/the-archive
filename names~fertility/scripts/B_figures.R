# =============================================================================
# B_figures.R  —  VISUALISATION ONLY.
#
# Reads out/*.csv and nothing else. If a figure needs a number that isn't in
# out/, that number belongs in A_analysis.R — do not compute it here.
#
#
# Run:  source("scripts/B_figures.R")   (working dir = project root)
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})
source("theme_archives.R")   # archives_pal, archives_categorical,
                             # theme_archives(), scale_*_archives(), save_archives()

OUT <- "out"; FIG <- "figures"
dir.create(FIG, showWarnings = FALSE, recursive = TRUE)

have <- function(f) file.exists(file.path(OUT, f))
grab <- function(f) fread(file.path(OUT, f), showProgress = FALSE)
skip <- function(fig, why) message("  SKIP ", fig, " - ", why)

CAP <- function(src) archives_caption(
  paste0("Source: ", src), "Chart: the archives")

TERRA <- archives_pal$terracotta
GREY  <- archives_pal$grey
TAUPE <- archives_pal$taupe

# ---------------------------------------------------------------------------
# Figure 1 — the hook. How common was the most common name?
# ---------------------------------------------------------------------------
if (have("index_headline.csv")) {
  d <- grab("index_headline.csv")
  pick <- c("France", "Norway", "USA")
  d1 <- d[country %chin% pick & !is.na(top1_cov)]
  p1 <- ggplot(d1, aes(year, 100 * top1_cov, colour = country)) +
    geom_line(linewidth = 0.7) +
    scale_colour_manual(values = c(France = TERRA,
                                   Norway = archives_categorical[["slate"]],
                                   USA    = archives_categorical[["warm_grey"]])) +
    scale_x_continuous(breaks = seq(1880, 2020, 20)) +
    labs(title = "One name used to belong to one child in twenty",
         subtitle = paste("Share of babies of a given sex receiving that year's",
                          "<b>single most common name</b>"),
         x = NULL, y = "% of births", caption = CAP("National name registries")) +
    theme_archives()
  save_archives(p1, file.path(FIG, "fig01_hook_top1.png"))
} else skip("fig01", "index_headline.csv missing - run A first")

# ---------------------------------------------------------------------------
# Figure 2 — the core index, faceted. Grey spaghetti behind for context;
# each panel's own country in terracotta. Two colours, palette untroubled.
# ---------------------------------------------------------------------------
if (have("index_headline.csv")) {
  d <- grab("index_headline.csv")
  bg <- d[, .(year, index, bg = country)]           # every country, no facet var
  p2 <- ggplot() +
    geom_line(data = bg, aes(year, 100 * index, group = bg),
              colour = TAUPE, linewidth = 0.35) +
    geom_line(data = d, aes(year, 100 * index),
              colour = TERRA, linewidth = 0.8) +
    facet_wrap(~ country, ncol = 3) +
    scale_x_continuous(breaks = seq(1880, 2020, 40)) +
    labs(title = "Naming concentration fell everywhere - but not at the same time",
         subtitle = paste("Share of births receiving a <b>top-ten name</b>.",
                          "Other countries shown in grey behind."),
         x = NULL, y = "% of births", caption = CAP("National name registries")) +
    theme_archives()
  save_archives(p2, file.path(FIG, "fig02_index_facets.png"), height = 6.4)
} else skip("fig02", "index_headline.csv missing")

# ---------------------------------------------------------------------------
# Figure 3 — do the measures agree? Three series, one country.
# ---------------------------------------------------------------------------
if (have("index_headline.csv")) {
  d <- grab("index_headline.csv")[country == "Norway"]
  m <- melt(d[, .(year,
                  `Top-10 share of all births, %` = 100 * index,
                  `Top-10 within the top 25, %`   = 100 * top10_c25,
                  `Zipf exponent (scale-free)`    = zipf)],
            id.vars = "year", variable.name = "measure", na.rm = TRUE)
  p3 <- ggplot(m, aes(year, value, colour = measure)) +
    geom_line(linewidth = 0.7) +
    facet_wrap(~ measure, scales = "free_y", ncol = 1) +
    scale_colour_archives() +
    guides(colour = "none") +
    labs(title = "The story is not an artefact of the measure",
         subtitle = paste("Three measures, Norway 1880-2025. The middle one is",
                          "comparable across sources but keeps only a third of the range."),
         x = NULL, y = NULL, caption = CAP("Statistics Norway, table 10467")) +
    theme_archives()
  save_archives(p3, file.path(FIG, "fig03_measure_agreement.png"), height = 6.4)
} else skip("fig03", "index_headline.csv missing")

# ---------------------------------------------------------------------------
# Figure 4 — the sex split
# ---------------------------------------------------------------------------
if (have("index_by_country_year_sex.csv")) {
  d <- grab("index_by_country_year_sex.csv")
  d[, Sex := fifelse(sex == "F", "Girls", "Boys")]
  p4 <- ggplot(d, aes(year, 100 * index, colour = Sex)) +
    geom_line(linewidth = 0.6) +
    facet_wrap(~ country, ncol = 3) +
    scale_colour_manual(values = c(Girls = TERRA,
                                   Boys  = archives_categorical[["slate"]])) +
    scale_x_continuous(breaks = seq(1880, 2020, 40)) +
    labs(title = "Girls' names have always been the more varied",
         subtitle = "Top-ten share by sex. The headline index averages the two.",
         x = NULL, y = "% of births", caption = CAP("National name registries")) +
    theme_archives()
  save_archives(p4, file.path(FIG, "fig04_sex_split.png"), height = 6.4)
} else skip("fig04", "index_by_country_year_sex.csv missing")

# ---------------------------------------------------------------------------
# Figure 5 — break-year scatter. Above the 45 degree line means naming moved first.
# ---------------------------------------------------------------------------
if (have("breakpoints.csv")) {
  b <- grab("breakpoints.csv")
  if (uniqueN(b$series) >= 2) {
    # Only countries whose naming series is long enough for a break to be
    # dated freely. Denmark starts 1985 and England & Wales 1996, so their
    # breaks sit against the edge of the data, not against history.
    if ("censored" %in% names(b)) b <- b[censored == FALSE]
    if ("tier" %in% names(b))     b <- b[tier == "deep"]
    w <- dcast(b[, .(v = min(break_year)), by = .(country, series)],
               country ~ series, value.var = "v")
    if (all(c("naming","fertility") %in% names(w))) {
      p5 <- ggplot(w, aes(naming, fertility)) +
        geom_abline(slope = 1, intercept = 0, colour = TAUPE, linewidth = 0.4) +
        geom_point(colour = TERRA, size = 2.6) +
        geom_text(aes(label = country), family = "Georgia", size = 3.2,
                  colour = archives_pal$brown, vjust = -1.1) +
        labs(title = "Which moved first?",
             subtitle = paste("Dated independently on each series.",
                              "<b>Below the line</b> = naming broke first."),
             x = "Break in naming concentration",
             y = "Break in fertility",
             caption = CAP("Name registries; HFD, HFC and UN WPP")) +
        theme_archives()
      save_archives(p5, file.path(FIG, "fig05_break_scatter.png"))
    } else skip("fig05", "need both naming and fertility breaks")
  } else skip("fig05", "only one series in breakpoints.csv - fertility data missing?")
} else skip("fig05", "breakpoints.csv missing")

# ---------------------------------------------------------------------------
# Figure 6 — the money figure. Local projections.
# ---------------------------------------------------------------------------
if (have("lead_lag.csv")) {
  lp <- grab("lead_lag.csv")
  if ("spec" %in% names(lp)) {
    p6 <- ggplot(lp, aes(h, beta, colour = spec, fill = spec)) +
      geom_hline(yintercept = 0, colour = archives_pal$brown, linewidth = 0.4) +
      geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.25, colour = NA) +
      geom_line(linewidth = 0.9) +
      scale_colour_manual(values = c(`country FE only` = archives_categorical[["warm_grey"]],
                                     `year FE + lags`  = TERRA)) +
      scale_fill_manual(values   = c(`country FE only` = archives_categorical[["warm_grey"]],
                                     `year FE + lags`  = TERRA)) +
      labs(title = "The apparent effect is a shared trend",
           subtitle = paste("Change in fertility <b>h years after</b> a one-point change in",
                            "naming concentration.<br>Grey: country effects only. Terracotta:",
                            "with year effects, which absorb whatever hit every country at once."),
           x = "Years after the naming change", y = "Effect on TFR",
           caption = CAP("Local projections on first differences; HFD, HFC, UN WPP")) +
      theme_archives()
    save_archives(p6, file.path(FIG, "fig06_lead_lag.png"))
  } else {
  p6 <- ggplot(lp, aes(h, beta)) +
    geom_hline(yintercept = 0, colour = archives_pal$brown, linewidth = 0.4) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = TAUPE, alpha = 0.55) +
    geom_line(colour = TERRA, linewidth = 0.9) +
    labs(title = "Does naming move before fertility?",
         subtitle = paste("Change in fertility <b>h years after</b> a one-point",
                          "change in naming concentration. 95% bands, clustered by country."),
         x = "Years after the naming change", y = "Effect on TFR",
         caption = CAP("Local projections on first differences; HFD, HFC, UN WPP")) +
    theme_archives()
  save_archives(p6, file.path(FIG, "fig06_lead_lag.png"))
  }
} else skip("fig06", "lead_lag.csv missing - fertility data not loaded")

# ---------------------------------------------------------------------------
# Figure 7 — robustness. Depth sensitivity, and what the covered-total
# shortcut costs (measured against Norway's true all-births percentages).
# ---------------------------------------------------------------------------
if (have("robustness_depth_sensitivity.csv")) {
  d <- grab("robustness_depth_sensitivity.csv")
  d <- d[, .(share = mean(share, na.rm = TRUE)), by = .(country, year, top_k)]
  d[, Depth := factor(paste0("Top ", top_k), levels = paste0("Top ", c(5,10,25,50)))]
  p7 <- ggplot(d[country %chin% c("France","Norway","USA","Ireland")],
               aes(year, 100 * share, colour = Depth)) +
    geom_line(linewidth = 0.6) +
    facet_wrap(~ country, ncol = 2) +
    scale_colour_archives() +
    labs(title = "The shape does not depend on where you cut",
         subtitle = "Concentration measured at four different depths",
         x = NULL, y = "% of births", caption = CAP("National name registries")) +
    theme_archives()
  save_archives(p7, file.path(FIG, "fig07_robustness_depth.png"), height = 6)
} else skip("fig07", "robustness_depth_sensitivity.csv missing")

if (have("robustness_norway_calibration.csv")) {
  n <- grab("robustness_norway_calibration.csv")
  n <- n[, .(top10_true = mean(top10_true), top10_covered = mean(top10_covered),
             coverage = mean(coverage)), by = year]
  m <- melt(n[, .(year, `True (all births)` = 100 * top10_true,
                  `Covered total only`      = 100 * top10_covered)],
            id.vars = "year", variable.name = "basis")
  p7b <- ggplot(m, aes(year, value, colour = basis)) +
    geom_line(linewidth = 0.7) +
    scale_colour_manual(values = c(`True (all births)` = TERRA,
                                   `Covered total only` = archives_categorical[["slate"]])) +
    labs(title = "What the missing tail costs us",
         subtitle = paste("Norway publishes shares of <b>all</b> births, so it can",
                          "price the shortcut every other country forces on us."),
         x = NULL, y = "Top-ten share, %",
         caption = CAP("Statistics Norway, table 10467")) +
    theme_archives()
  save_archives(p7b, file.path(FIG, "fig07b_norway_calibration.png"))
} else skip("fig07b", "robustness_norway_calibration.csv missing")

# ---------------------------------------------------------------------------
# Figure 8 — the modern plateau. Exactly four countries: the palette's limit.
# ---------------------------------------------------------------------------
if (have("index_headline.csv")) {
  d <- grab("index_headline.csv")[year >= 1980 &
        country %chin% c("Denmark","England & Wales","Norway","Scotland")]
  p8 <- ggplot(d, aes(year, 100 * index, colour = country)) +
    geom_line(linewidth = 0.75) +
    scale_colour_archives() +
    labs(title = "And then it stopped",
         subtitle = "Top-ten share since 1980. The fall flattens out in the 2010s.",
         x = NULL, y = "% of births", caption = CAP("National name registries")) +
    theme_archives()
  save_archives(p8, file.path(FIG, "fig08_plateau.png"))
} else skip("fig08", "index_headline.csv missing")


# ---------------------------------------------------------------------------
# Figure 9 — THE CENTRAL CHART. The two series, together, per country.
# Both indexed to 100 at the first year of overlap so one axis carries both;
# ---------------------------------------------------------------------------
if (have("series_joined.csv")) {
  sj <- grab("series_joined.csv")[tier == "deep"]
  m <- melt(sj[, .(country, year,
                   `Naming concentration` = idx100,
                   `Total fertility`      = tfr100)],
            id.vars = c("country","year"), variable.name = "series",
            na.rm = TRUE)
  p9 <- ggplot(m, aes(year, value, colour = series)) +
    geom_line(linewidth = 0.75)
  if (have("breakpoints.csv")) {
    bk <- grab("breakpoints.csv")
    if ("censored" %in% names(bk)) bk <- bk[censored == FALSE]
    bk <- bk[country %chin% unique(m$country)]
    if (nrow(bk))
      p9 <- p9 + geom_vline(data = bk, aes(xintercept = break_year, linetype = series),
                            colour = archives_pal$brown, linewidth = 0.3, alpha = 0.7,
                            show.legend = FALSE)
  }
  p9 <- p9 +
    facet_wrap(~ country, ncol = 2, scales = "free_x") +
    scale_colour_manual(values = c(`Naming concentration` = TERRA,
                                   `Total fertility` = archives_categorical[["slate"]])) +
    labs(title = "Do names move before babies?",
         subtitle = paste("Both series indexed to 100 at the first year they overlap.",
                          "Vertical rules mark<br>the breaks each series was dated at,",
                          "independently."),
         x = NULL, y = "Index, first overlapping year = 100",
         caption = CAP("Name registries; HFD, HFC and UN WPP")) +
    theme_archives()
  save_archives(p9, file.path(FIG, "fig09_two_series.png"), height = 7)
} else skip("fig09", "series_joined.csv missing - re-run A")

# ---------------------------------------------------------------------------
# Figure 10 —
# ---------------------------------------------------------------------------
if (have("event_time.csv")) {
  et <- grab("event_time.csv")
  avg <- et[, .(tfr100 = mean(tfr100, na.rm = TRUE)), by = rel]
  p10 <- ggplot() +
    geom_vline(xintercept = 0, colour = archives_pal$brown, linewidth = 0.4) +
    geom_line(data = et, aes(rel, tfr100, group = country),
              colour = TAUPE, linewidth = 0.45) +
    geom_line(data = avg, aes(rel, tfr100), colour = TERRA, linewidth = 1) +
    labs(title = "Align anything on anything and it looks like a cause",
         subtitle = paste("Fertility, with each country re-centred on the year its",
                          "<b>naming</b> concentration broke.<br>Grey: countries.",
                          "Terracotta: the average. Compare the FT's smartphone chart."),
         x = "Years from the naming break", y = "Fertility, indexed",
         caption = CAP("Name registries; HFD, HFC and UN WPP")) +
    theme_archives()
  save_archives(p10, file.path(FIG, "fig10_event_time.png"))
} else skip("fig10", "event_time.csv missing - needs breakpoints, so strucchange")

message("\nFigures written to ", FIG, "/: ",
        paste(list.files(FIG, pattern = "png$"), collapse = ", "))
