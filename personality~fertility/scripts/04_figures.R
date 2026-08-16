## ---------------------------------------------------------------------------
## 04_figures.R -- every chart, house style
##
## Figures land in ../figures/. Nothing here computes a new estimate; all
## modelling is in 03_models.R. The two "observed" figures (7, 8) bin the
## analysis frame directly so the raw pattern can be checked against the OLS.
##
## Palette note: the house rotation carries men and women. Terracotta is
## reserved for the loaded category and is used in exactly one place -- the
## after-partnering estimate in the mediation figure, which is the finding.
## ---------------------------------------------------------------------------

source("00_setup.R")
source(file.path(PROJ, "theme_archives.R"))
check_georgia()

library(ggplot2)

FIG <- file.path(PROJ, "figures")
dir.create(FIG, showWarnings = FALSE, recursive = TRUE)

an <- readRDS(file.path(DIR$cache, "analysis.rds"))
M  <- readRDS(file.path(DIR$cache, "models.rds"))

SRC  <- archives_caption("NCDS 1958 cohort, UK Data Service SN 6137, 9418, 6940")
SRC2 <- archives_caption("NCDS 1958 cohort, UK Data Service SN 6137, 9418")

## Fixed trait order everywhere, so figures can be read against each other
trait_order <- c("Extraversion", "Agreeableness", "Conscientiousness",
                 "Emotional stability", "Openness / intellect")
ord <- function(x) factor(x, levels = rev(trait_order))

sex_cols <- c(Men   = unname(archives_categorical["slate"]),
              Women = unname(archives_categorical["sage"]))

## ---------------------------------------------------------------------------
## Shared coefficient-plot builder: traits on y, estimate on x, CI whiskers,
## zero reference line, men and women dodged.
## ---------------------------------------------------------------------------
coef_plot <- function(d, title, subtitle, xlab, caption = SRC) {
  ggplot(d, aes(x = estimate, y = ord(label), colour = sample)) +
    geom_vline(xintercept = 0, colour = archives_pal$charcoal,
               linewidth = 0.4, linetype = "solid") +
    geom_linerange(aes(xmin = conf.low, xmax = conf.high),
                   position = position_dodge(width = 0.55), linewidth = 0.7) +
    geom_point(position = position_dodge(width = 0.55), size = 2.6) +
    scale_colour_manual(values = sex_cols) +
    labs(title = title, subtitle = subtitle, x = xlab, y = NULL,
         caption = caption) +
    theme_archives(grid = "x")
}

## ===========================================================================
## FIG 1 -- fertility, unadjusted
## ===========================================================================
d1 <- M$headline %>%
  dplyr::filter(outcome_label == "Biological children", model == "M1")

f1 <- coef_plot(
  d1,
  "Which traits go with having more children",
  paste0("Change in number of biological children by age 50, per standard ",
         "deviation of each trait.<br>Unadjusted. Horizontal bars are 95% ",
         "confidence intervals."),
  "Children per SD of trait"
)
save_archives(f1, file.path(FIG, "01_fertility_unadjusted.png"))

## ===========================================================================
## FIG 2 -- childlessness, unadjusted
## ===========================================================================
d2 <- M$headline %>%
  dplyr::filter(outcome_label == "Childless (pp)", model == "M1")

f2 <- coef_plot(
  d2,
  "Which traits go with never having children",
  paste0("Change in the probability of being childless at 50, in percentage ",
         "points, per standard deviation of each trait.<br>Unadjusted. ",
         "20.6% of men and 16.8% of women in this cohort had no biological ",
         "children."),
  "Percentage points per SD of trait"
)
save_archives(f2, file.path(FIG, "02_childlessness_unadjusted.png"))

## ===========================================================================
## FIG 3 -- partnering
## ===========================================================================
d3 <- M$headline %>%
  dplyr::filter(outcome_label == "Share of years partnered", model == "M1")

f3 <- coef_plot(
  d3,
  "Which traits go with spending more of adult life partnered",
  paste0("Change in the share of months between ages 16 and 50 spent living ",
         "with a partner, in percentage<br>points, per standard deviation of ",
         "each trait. Marriage and cohabitation pooled. Unadjusted."),
  "Percentage points of adult life per SD of trait"
)
save_archives(f3, file.path(FIG, "03_partnering_unadjusted.png"))

## ===========================================================================
## FIG 4 -- THE MEDIATION FIGURE
## Before and after adjusting for time spent partnered. This is the finding:
## for men the association collapses, for women it does not.
## ===========================================================================
d4 <- M$headline %>%
  dplyr::filter(outcome_label == "Biological children", model %in% c("M1", "M3")) %>%
  dplyr::mutate(model = factor(model, levels = c("M1", "M3"),
                               labels = c("Before", "After partnering")))

f4 <- ggplot(d4, aes(x = estimate, y = ord(label))) +
  geom_vline(xintercept = 0, colour = archives_pal$charcoal, linewidth = 0.4) +
  geom_line(aes(group = label), colour = archives_pal$taupe, linewidth = 1.1) +
  geom_point(aes(colour = model), size = 2.8) +
  scale_colour_manual(values = c("Before" = unname(archives_categorical["slate"]),
                                 "After partnering" = archives_pal$terracotta)) +
  facet_wrap(~ sample) +
  labs(
    title = "For men, personality predicts children only by predicting partnership",
    subtitle = paste0("Children per standard deviation of each trait, before and ",
                      "after adjusting for the share of adult<br>life spent living ",
                      "with a partner. <b>Men's extraversion effect all but ",
                      "disappears. Women's conscientiousness<br>effect gets ",
                      "stronger.</b>"),
    x = "Children per SD of trait", y = NULL, caption = SRC
  ) +
  theme_archives(grid = "x")
save_archives(f4, file.path(FIG, "04_mediation_children.png"), height = 6.0)

## ===========================================================================
## FIG 5 -- same mediation view for childlessness
## ===========================================================================
d5 <- M$headline %>%
  dplyr::filter(outcome_label == "Childless (pp)", model %in% c("M1", "M3")) %>%
  dplyr::mutate(model = factor(model, levels = c("M1", "M3"),
                               labels = c("Before", "After partnering")))

f5 <- ggplot(d5, aes(x = estimate, y = ord(label))) +
  geom_vline(xintercept = 0, colour = archives_pal$charcoal, linewidth = 0.4) +
  geom_line(aes(group = label), colour = archives_pal$taupe, linewidth = 1.1) +
  geom_point(aes(colour = model), size = 2.8) +
  scale_colour_manual(values = c("Before" = unname(archives_categorical["slate"]),
                                 "After partnering" = archives_pal$terracotta)) +
  facet_wrap(~ sample) +
  labs(
    title = "The same pattern in childlessness",
    subtitle = paste0("Percentage-point change in the probability of childlessness ",
                      "per standard deviation of each trait,<br>before and after ",
                      "adjusting for time spent partnered."),
    x = "Percentage points per SD of trait", y = NULL, caption = SRC
  ) +
  theme_archives(grid = "x")
save_archives(f5, file.path(FIG, "05_mediation_childlessness.png"), height = 6.0)

## ===========================================================================
## FIG 6 -- is the sex difference statistically real?
## Interaction terms from the pooled trait x sex model.
## ===========================================================================
inter <- dplyr::bind_rows(
  M$nkids     %>% dplyr::mutate(outcome_label = "Biological children", scale = 1),
  M$childless %>% dplyr::mutate(outcome_label = "Childlessness (pp)",  scale = 100)
) %>%
  dplyr::filter(sample == "Pooled, trait x sex", model == "M1",
                grepl("^z_.*:sex", term)) %>%
  dplyr::mutate(
    trait = sub("^z_", "", sub(":sex.*$", "", term)),
    label = unname(trait_labels[trait]),
    dplyr::across(c(estimate, conf.low, conf.high), ~ .x * scale)
  )

f6 <- ggplot(inter, aes(x = estimate, y = ord(label))) +
  geom_vline(xintercept = 0, colour = archives_pal$charcoal, linewidth = 0.4) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high),
                 linewidth = 0.7, colour = archives_pal$terracotta) +
  geom_point(size = 2.6, colour = archives_pal$terracotta) +
  facet_wrap(~ outcome_label, scales = "free_x") +
  labs(
    title = "Where men and women genuinely differ",
    subtitle = paste0("Women's slope minus men's slope, per standard deviation ",
                      "of each trait. Intervals that clear zero<br>mark traits ",
                      "that operate differently by sex rather than differing by ",
                      "chance."),
    x = "Difference in slope (women - men)", y = NULL, caption = SRC2
  ) +
  theme_archives(grid = "x")
save_archives(f6, file.path(FIG, "06_sex_difference.png"), height = 5.2)

## ===========================================================================
## FIG 7 -- observed childlessness by trait quintile
## The raw pattern, so a reader can check the regression is not doing anything
## exotic. Quintiles are formed within sex.
## ===========================================================================
quint <- an %>%
  tidyr::pivot_longer(dplyr::all_of(paste0("bf_", trait_levels)),
                      names_to = "trait", values_to = "score") %>%
  dplyr::mutate(trait = sub("^bf_", "", trait),
                label = unname(trait_labels[trait])) %>%
  dplyr::group_by(sex, trait) %>%
  dplyr::mutate(q = dplyr::ntile(score, 5)) %>%
  dplyr::group_by(sex, trait, label, q) %>%
  dplyr::summarise(n = dplyr::n(),
                   pct_childless = 100 * mean(childless),
                   se = 100 * sqrt(mean(childless) * (1 - mean(childless)) / dplyr::n()),
                   mean_kids = mean(nkids),
                   se_kids = stats::sd(nkids) / sqrt(dplyr::n()),
                   .groups = "drop") %>%
  dplyr::mutate(label = factor(label, levels = trait_order))

f7 <- ggplot(quint, aes(x = q, y = pct_childless, colour = sex)) +
  geom_ribbon(aes(ymin = pct_childless - 1.96 * se,
                  ymax = pct_childless + 1.96 * se, fill = sex),
              alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_colour_manual(values = sex_cols) +
  scale_fill_manual(values = sex_cols, guide = "none") +
  scale_x_continuous(breaks = 1:5, labels = c("Low", "", "", "", "High"),
                     expand = expansion(mult = 0.14)) +
  facet_wrap(~ label, nrow = 1) +
  labs(
    title = "Childlessness across the range of each trait",
    subtitle = paste0("Percentage childless at 50, by within-sex quintile of ",
                      "each trait. Shaded bands are 95% intervals."),
    x = "Quintile of trait", y = "% childless at 50", caption = SRC2
  ) +
  theme_archives(grid = "y") +
  theme(panel.spacing.x = unit(1.4, "lines"))
save_archives(f7, file.path(FIG, "07_childless_by_quintile.png"),
              width = 11, height = 4.6)

## ===========================================================================
## FIG 8 -- observed number of children by trait quintile
## ===========================================================================
f8 <- ggplot(quint, aes(x = q, y = mean_kids, colour = sex)) +
  geom_ribbon(aes(ymin = mean_kids - 1.96 * se_kids,
                  ymax = mean_kids + 1.96 * se_kids, fill = sex),
              alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_colour_manual(values = sex_cols) +
  scale_fill_manual(values = sex_cols, guide = "none") +
  scale_x_continuous(breaks = 1:5, labels = c("Low", "", "", "", "High"),
                     expand = expansion(mult = 0.14)) +
  facet_wrap(~ label, nrow = 1) +
  labs(
    title = "Number of children across the range of each trait",
    subtitle = "Mean biological children at 50, by within-sex quintile of each trait.",
    x = "Quintile of trait", y = "Mean children", caption = SRC2
  ) +
  theme_archives(grid = "y") +
  theme(panel.spacing.x = unit(1.4, "lines"))
save_archives(f8, file.path(FIG, "08_children_by_quintile.png"),
              width = 11, height = 4.6)

## ===========================================================================
## FIG 9 -- the extraversion gradient in partnering, men vs women
## The single clearest bivariate picture behind the mediation story.
## ===========================================================================
grad <- an %>%
  dplyr::filter(!is.na(pship_share)) %>%
  dplyr::group_by(sex) %>%
  dplyr::mutate(dec = dplyr::ntile(bf_E, 10)) %>%
  dplyr::group_by(sex, dec) %>%
  dplyr::summarise(share = 100 * mean(pship_share),
                   se = 100 * stats::sd(pship_share) / sqrt(dplyr::n()),
                   n = dplyr::n(), .groups = "drop")

f9 <- ggplot(grad, aes(x = dec, y = share, colour = sex)) +
  geom_ribbon(aes(ymin = share - 1.96 * se, ymax = share + 1.96 * se, fill = sex),
              alpha = 0.15, colour = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.2) +
  scale_colour_manual(values = sex_cols) +
  scale_fill_manual(values = sex_cols, guide = "none") +
  scale_x_continuous(breaks = c(1, 5, 10),
                     labels = c("Least\nextraverted", "Middle", "Most\nextraverted")) +
  labs(
    title = "Extraversion buys men partnered years. It does much less for women.",
    subtitle = paste0("Share of months between 16 and 50 spent living with a ",
                      "partner, by decile of extraversion."),
    x = NULL, y = "% of adult life partnered", caption = SRC
  ) +
  theme_archives(grid = "y")
save_archives(f9, file.path(FIG, "09_extraversion_partnering_gradient.png"))

## ===========================================================================
## FIG 10 -- descriptive: parity distribution
## ===========================================================================
f10 <- M$parity %>%
  dplyr::mutate(parity_lab = factor(parity, levels = 0:4,
                                    labels = c("0", "1", "2", "3", "4+"))) %>%
  ggplot(aes(x = parity_lab, y = 100 * share, fill = sex)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.68) +
  scale_fill_manual(values = sex_cols) +
  labs(
    title = "How many children the 1958 cohort actually had",
    subtitle = paste0("Distribution of biological children at age 50. ",
                      "<b>One man in five had none, against one woman in six.</b>"),
    x = "Biological children", y = "% of cohort", caption = SRC2
  ) +
  theme_archives(grid = "y")
save_archives(f10, file.path(FIG, "10_parity_distribution.png"))

## ===========================================================================
## FIG 11 -- descriptive: where men and women sit on each trait
## Context for every other figure: the sexes do not start from the same place.
## ===========================================================================
f11 <- M$desc_traits %>%
  dplyr::mutate(label = factor(label, levels = trait_order)) %>%
  ggplot(aes(x = mean, y = ord(as.character(label)), colour = sex)) +
  geom_linerange(aes(xmin = mean - 1.96 * sd / sqrt(n),
                     xmax = mean + 1.96 * sd / sqrt(n)),
                 position = position_dodge(width = 0.5), linewidth = 0.8) +
  geom_point(position = position_dodge(width = 0.5), size = 2.8) +
  scale_colour_manual(values = sex_cols) +
  labs(
    title = "Where the sexes sit on each trait",
    subtitle = paste0("Mean score on each 10-50 scale. Women score markedly ",
                      "higher on agreeableness and lower on<br>emotional ",
                      "stability; extraversion and openness are close to level."),
    x = "Mean score (10-50 scale)", y = NULL, caption = SRC2
  ) +
  theme_archives(grid = "x")
save_archives(f11, file.path(FIG, "11_trait_means_by_sex.png"), height = 5.0)

## ===========================================================================
## FIG 12 -- the full ladder, for the methods appendix
## Shows what education adds and what partnering takes away, all three models.
## ===========================================================================
d12 <- M$headline %>%
  dplyr::filter(outcome_label == "Biological children") %>%
  dplyr::mutate(model = factor(model, levels = c("M1", "M2", "M3"),
                               labels = c("Traits only",
                                          "+ education",
                                          "+ partnering")))

## The three estimates for a trait sit on one horizontal track, so the eye reads
## movement rather than three unrelated dots. The confidence interval is drawn
## for the final estimate only -- that is the one the reader is being asked to
## judge against zero, and three overlapping intervals per trait would be
## unreadable. Full intervals for every model are in 03_fertility_count.csv.
f12 <- ggplot(d12, aes(x = estimate, y = ord(label))) +
  geom_vline(xintercept = 0, colour = archives_pal$charcoal, linewidth = 0.4) +
  geom_line(aes(group = label), colour = archives_pal$taupe, linewidth = 1.1) +
  geom_linerange(data = dplyr::filter(d12, model == "+ partnering"),
                 aes(xmin = conf.low, xmax = conf.high),
                 colour = archives_pal$terracotta, linewidth = 0.7) +
  geom_point(aes(colour = model), size = 2.8) +
  scale_colour_manual(values = c(
    "Traits only"   = unname(archives_categorical["slate"]),
    "+ education"   = unname(archives_categorical["warm_grey"]),
    "+ partnering"  = archives_pal$terracotta)) +
  facet_wrap(~ sample) +
  labs(
    title = "What survives adjustment",
    subtitle = paste0("The same association, estimated three ways: on its own, ",
                      "then holding education constant, then<br>also holding ",
                      "constant the share of adult life spent partnered. ",
                      "<b>Men's extraversion effect travels<br>almost all the way ",
                      "to zero. Women's conscientiousness effect moves the other ",
                      "way.</b> Interval shown<br>is for the final estimate."),
    x = "Children per SD of trait", y = NULL, caption = SRC
  ) +
  theme_archives(grid = "x")
save_archives(f12, file.path(FIG, "12_model_ladder.png"), height = 6.2)

## ===========================================================================
## FIG 13 -- appendix: who is in the analytic sample
## ===========================================================================
attr_d <- readr::read_csv(file.path(DIR$out, "02_attrition.csv"),
                          show_col_types = FALSE) %>%
  tidyr::pivot_longer(c(pct_resp_33, pct_resp_42),
                      names_to = "wave", values_to = "pct") %>%
  dplyr::mutate(wave = dplyr::recode(wave,
                                     pct_resp_33 = "Responded at 33",
                                     pct_resp_42 = "Responded at 42"))

f13 <- ggplot(attr_d, aes(x = pct, y = stage, fill = wave)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.66) +
  scale_fill_manual(values = unname(archives_categorical[c("slate", "sand")])) +
  labs(
    title = "Who makes it into the analytic sample",
    subtitle = paste0("The 8,429 people analysed are markedly more likely to ",
                      "have responded at earlier sweeps than<br>those who dropped ",
                      "out. Associations here describe survivors, not the whole ",
                      "1958 cohort."),
    x = "% responding", y = NULL, caption = archives_caption("NCDS 1958 cohort, UK Data Service SN 5560")
  ) +
  theme_archives(grid = "x")
save_archives(f13, file.path(FIG, "13_attrition.png"), height = 4.4)

message("04_figures.R done. ", length(list.files(FIG, pattern = "\\.png$")),
        " figures in ", FIG)
