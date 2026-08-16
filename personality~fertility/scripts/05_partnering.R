## ---------------------------------------------------------------------------
## 05_partnering.R -- the partnering beat, self-contained
##
## Everything for this section in one file: the quintile aggregation, the
## figures, and the CSVs behind them. Run it on its own after 03_models.R.
## Depends only on 00_setup.R and theme_archives.R, both already in place.
##
## Outcome throughout: the share of months between ages 16 and 50 spent living
## with a partner. Marriage and cohabitation pooled -- marriage alone would
## track a changing institution rather than a changing behaviour.
## ---------------------------------------------------------------------------

source("00_setup.R")
source(file.path(PROJ, "theme_archives.R"))
check_georgia()

library(ggplot2)

FIG <- file.path(PROJ, "figures")
dir.create(FIG, showWarnings = FALSE, recursive = TRUE)

an <- readRDS(file.path(DIR$cache, "analysis.rds"))
M  <- readRDS(file.path(DIR$cache, "models.rds"))

SRC <- archives_caption("NCDS 1958 cohort, UK Data Service SN 6137, 9418, 6940")

trait_order <- c("Extraversion", "Agreeableness", "Conscientiousness",
                 "Emotional stability", "Openness / intellect")
ord <- function(x) factor(x, levels = rev(trait_order))
sex_cols <- c(Men   = unname(archives_categorical["slate"]),
              Women = unname(archives_categorical["sage"]))

## Restrict to people whose partnership histories actually run to age 50.
## Everything below uses this one sample so the figures are mutually consistent.
ap <- an %>% dplyr::filter(!is.na(pship_share))
message("Partnering sample: ", nrow(ap), " (",
        sum(ap$sex == "Men"), " men, ", sum(ap$sex == "Women"), " women)")

## ===========================================================================
## 1. AGGREGATE BY TRAIT QUINTILE
##    Quintiles are formed within sex, so a "top quintile" man is compared with
##    other men. Pooling would confound the trait gradient with the sex gap in
##    trait means, which is large for agreeableness in particular.
## ===========================================================================
pq <- ap %>%
  tidyr::pivot_longer(dplyr::all_of(paste0("bf_", trait_levels)),
                      names_to = "trait", values_to = "score") %>%
  dplyr::mutate(trait = sub("^bf_", "", trait),
                label = unname(trait_labels[trait])) %>%
  dplyr::group_by(sex, trait) %>%
  dplyr::mutate(q = dplyr::ntile(score, 5)) %>%
  dplyr::group_by(sex, trait, label, q) %>%
  dplyr::summarise(
    n              = dplyr::n(),
    pct_partnered  = 100 * mean(pship_share),
    se_partnered   = 100 * stats::sd(pship_share) / sqrt(dplyr::n()),
    years_partnered = mean(pship_share) * (AGE_END - AGE_START),
    pct_never      = 100 * mean(ever_partnered == 0),
    se_never       = 100 * sqrt(mean(ever_partnered == 0) *
                                (1 - mean(ever_partnered == 0)) / dplyr::n()),
    .groups = "drop"
  ) %>%
  dplyr::mutate(label = factor(label, levels = trait_order))

readr::write_csv(pq, file.path(DIR$out, "05_partnering_by_quintile.csv"))

qlab <- c("Low", "", "", "", "High")   # middles dropped: the axis title says quintile

## ===========================================================================
## FIG 14 -- share of adult life partnered, by trait quintile
## ===========================================================================
f14 <- ggplot(pq, aes(x = q, y = pct_partnered, colour = sex)) +
  geom_ribbon(aes(ymin = pct_partnered - 1.96 * se_partnered,
                  ymax = pct_partnered + 1.96 * se_partnered, fill = sex),
              alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_colour_manual(values = sex_cols) +
  scale_fill_manual(values = sex_cols, guide = "none") +
  scale_x_continuous(breaks = 1:5, labels = qlab,
                     expand = expansion(mult = 0.14)) +
  facet_wrap(~ label, nrow = 1) +
  labs(
    title = "How much of adult life each trait buys you in a partnership",
    subtitle = paste0("Share of months between ages 16 and 50 spent living with ",
                      "a partner, by within-sex quintile of<br>each trait. ",
                      "Shaded bands are 95% intervals."),
    x = "Quintile of trait", y = "% of adult life partnered", caption = SRC
  ) +
  theme_archives(grid = "y") +
  theme(panel.spacing.x = unit(1.4, "lines"))
save_archives(f14, file.path(FIG, "14_partnered_years_by_quintile.png"),
              width = 11, height = 4.6)

## ===========================================================================
## FIG 15 -- never partnered, by trait quintile
##    Small base rates (6.0% of men, 4.0% of women), so the intervals are wide.
##    Kept because the never-partnered tail is the group the piece is about.
## ===========================================================================
f15 <- ggplot(pq, aes(x = q, y = pct_never, colour = sex)) +
  geom_ribbon(aes(ymin = pmax(0, pct_never - 1.96 * se_never),
                  ymax = pct_never + 1.96 * se_never, fill = sex),
              alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_colour_manual(values = sex_cols) +
  scale_fill_manual(values = sex_cols, guide = "none") +
  scale_x_continuous(breaks = 1:5, labels = qlab,
                     expand = expansion(mult = 0.14)) +
  facet_wrap(~ label, nrow = 1) +
  labs(
    title = "Who never lives with anyone",
    subtitle = paste0("Percentage who never formed a co-residential union by 50, ",
                      "by within-sex quintile of each trait.<br>Base rates are ",
                      "low - 6.0% of men and 4.0% of women - so the intervals ",
                      "are wide."),
    x = "Quintile of trait", y = "% never partnered by 50", caption = SRC
  ) +
  theme_archives(grid = "y") +
  theme(panel.spacing.x = unit(1.4, "lines"))
save_archives(f15, file.path(FIG, "15_never_partnered_by_quintile.png"),
              width = 11, height = 4.6)

## ===========================================================================
## FIG 16 -- the agreeableness reversal
##    The one place in the whole analysis where a trait's sign flips by sex.
##    Deciles rather than quintiles, since this is a single-panel close-up.
## ===========================================================================
agree_dec <- ap %>%
  dplyr::group_by(sex) %>%
  dplyr::mutate(dec = dplyr::ntile(bf_A, 10)) %>%
  dplyr::group_by(sex, dec) %>%
  dplyr::summarise(pct = 100 * mean(pship_share),
                   se  = 100 * stats::sd(pship_share) / sqrt(dplyr::n()),
                   n   = dplyr::n(), .groups = "drop")

f16 <- ggplot(agree_dec, aes(x = dec, y = pct, colour = sex)) +
  geom_ribbon(aes(ymin = pct - 1.96 * se, ymax = pct + 1.96 * se, fill = sex),
              alpha = 0.15, colour = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.2) +
  scale_colour_manual(values = sex_cols) +
  scale_fill_manual(values = sex_cols, guide = "none") +
  scale_x_continuous(breaks = c(1, 5, 10),
                     labels = c("Least\nagreeable", "Middle", "Most\nagreeable")) +
  labs(
    title = "Agreeableness is the one trait that reverses by sex",
    subtitle = paste0("Share of adult life partnered, by decile of agreeableness. ",
                      "<b>Disagreeable men spend more of life<br>partnered; ",
                      "agreeable women do.</b> Every other trait points the same ",
                      "way for both sexes."),
    x = NULL, y = "% of adult life partnered", caption = SRC
  ) +
  theme_archives(grid = "y")
save_archives(f16, file.path(FIG, "16_agreeableness_reversal.png"))

## ===========================================================================
## FIG 17 -- partnering against fertility, one point per trait
##    The argument of the piece in a single frame: for men the traits line up
##    along the diagonal, so partnering is the whole story. For women they
##    scatter off it, so something else is at work.
## ===========================================================================
xy <- M$headline %>%
  dplyr::filter(model == "M1",
                outcome_label %in% c("Biological children",
                                     "Share of years partnered")) %>%
  dplyr::select(outcome_label, sample, label, estimate) %>%
  tidyr::pivot_wider(names_from = outcome_label, values_from = estimate) %>%
  dplyr::rename(children = `Biological children`,
                partnered = `Share of years partnered`)

f17 <- ggplot(xy, aes(x = partnered, y = children)) +
  geom_hline(yintercept = 0, colour = archives_pal$taupe, linewidth = 0.4) +
  geom_vline(xintercept = 0, colour = archives_pal$taupe, linewidth = 0.4) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.6,
              colour = archives_pal$taupe, formula = y ~ x) +
  geom_point(size = 3, colour = archives_pal$terracotta) +
  geom_label_archives(aes(label = label), vjust = -0.9, size = 3) +
  facet_wrap(~ sample) +
  scale_x_continuous(expand = expansion(mult = 0.18)) +
  scale_y_continuous(expand = expansion(mult = 0.22)) +
  labs(
    title = "For men, a trait matters for children exactly as much as it matters for partnering",
    subtitle = paste0("Each point is one trait. Horizontal: percentage points of ",
                      "adult life partnered per SD.<br>Vertical: children per SD. ",
                      "<b>Men's traits sit close to the line; women's do not.</b>"),
    x = "Effect on share of adult life partnered (pp per SD)",
    y = "Effect on number of children (per SD)",
    caption = SRC
  ) +
  theme_archives(grid = "both")
save_archives(f17, file.path(FIG, "17_partnering_vs_fertility.png"), height = 5.8)

## ===========================================================================
## 2. A NUMBER FOR THE PROSE
##    Top-versus-bottom quintile gaps, in years rather than percentage points,
##    which is the unit a reader can hold onto.
## ===========================================================================
gap <- pq %>%
  dplyr::filter(q %in% c(1, 5)) %>%
  dplyr::select(sex, label, q, years_partnered) %>%
  tidyr::pivot_wider(names_from = q, values_from = years_partnered,
                     names_prefix = "q") %>%
  dplyr::mutate(gap_years = q5 - q1) %>%
  dplyr::arrange(sex, dplyr::desc(abs(gap_years)))

print(gap, n = 20)
readr::write_csv(gap, file.path(DIR$out, "05_quintile_gap_years.csv"))

message("05_partnering.R done. Figures 14-17 in ", FIG)
