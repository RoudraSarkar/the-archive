## ---------------------------------------------------------------------------
## 03_models.R -- descriptive OLS, association not causation
##
## Premise 1: does personality predict time spent partnered?
## Premise 2: does personality predict biological fertility, and how much of
##            that runs through partnering?
##
## Three-model ladder for each outcome:
##   M1  traits only
##   M2  + education
##   M3  + share of adult years partnered   (fertility outcomes only --
##          for the partnering outcome this term IS the outcome)
##
## All coefficients are per one within-cohort SD of the trait. Standard errors
## are HC1 throughout, because two of the three outcomes are binary or bounded.
## ---------------------------------------------------------------------------

source("00_setup.R")

an <- readRDS(file.path(DIR$cache, "analysis.rds"))
Z  <- paste0("z_", trait_levels)

## ===========================================================================
## 1. DESCRIPTIVES
## ===========================================================================
desc_traits <- an %>%
  tidyr::pivot_longer(dplyr::all_of(paste0("bf_", trait_levels)),
                      names_to = "trait", values_to = "score") %>%
  dplyr::mutate(trait = sub("^bf_", "", trait)) %>%
  dplyr::group_by(trait, sex) %>%
  dplyr::summarise(n = dplyr::n(), mean = mean(score), sd = stats::sd(score),
                   .groups = "drop") %>%
  dplyr::mutate(label = trait_labels[trait])

desc_outcomes <- an %>%
  dplyr::group_by(sex) %>%
  dplyr::summarise(
    n              = dplyr::n(),
    mean_children  = mean(nkids),
    pct_childless  = 100 * mean(childless),
    pct_3plus      = 100 * mean(nkids >= 3),
    mean_pship_share = mean(pship_share, na.rm = TRUE),
    pct_never_partnered = 100 * mean(ever_partnered == 0, na.rm = TRUE),
    .groups = "drop"
  )

## Parity distribution, for the descriptive chart in the next script
parity <- an %>%
  dplyr::mutate(parity = pmin(nkids, 4)) %>%
  dplyr::count(sex, parity) %>%
  dplyr::group_by(sex) %>%
  dplyr::mutate(share = n / sum(n)) %>%
  dplyr::ungroup()

print(desc_outcomes)

## ===========================================================================
## 2. MODEL MACHINERY
## ===========================================================================
rhs_m1 <- paste(Z, collapse = " + ")
rhs_m2 <- paste(rhs_m1, "+ nvq")
rhs_m3 <- paste(rhs_m2, "+ pship_share")

## Pooled models carry a sex main effect; sex-specific models do not.
fit_ladder <- function(outcome, data, sample_label, include_sex,
                       ladder = c("M1", "M2", "M3")) {
  rhs <- c(M1 = rhs_m1, M2 = rhs_m2, M3 = rhs_m3)[ladder]
  purrr::imap_dfr(rhs, function(r, nm) {
    f <- stats::as.formula(
      paste(outcome, "~", if (include_sex) paste(r, "+ sex") else r))
    m <- stats::lm(f, data = data)
    tidy_hc1(m, nm, outcome, sample_label)
  })
}

## Formal test of whether a trait's slope differs by sex: interact every trait
## with sex in one pooled model.
fit_interaction <- function(outcome, data, ladder_rhs, model_name) {
  f <- stats::as.formula(
    paste(outcome, "~ (", paste(Z, collapse = " + "), ") * sex",
          if (grepl("nvq", ladder_rhs)) "+ nvq" else "",
          if (grepl("pship_share", ladder_rhs)) "+ pship_share" else ""))
  m <- stats::lm(f, data = data)
  tidy_hc1(m, model_name, outcome, "Pooled, trait x sex")
}

## ===========================================================================
## 3. PREMISE 1 -- PARTNERING
##    Outcome: share of months age 16-50 spent in a co-residential union.
##    Marriage and cohabitation are pooled deliberately; marriage alone would
##    measure a changing institution rather than a changing behaviour.
## ===========================================================================
an_p <- an %>% dplyr::filter(!is.na(pship_share))

res_pship <- dplyr::bind_rows(
  fit_ladder("pship_share", an_p, "Pooled", include_sex = TRUE,  ladder = c("M1", "M2")),
  fit_ladder("pship_share", dplyr::filter(an_p, sex == "Men"),   "Men",
             include_sex = FALSE, ladder = c("M1", "M2")),
  fit_ladder("pship_share", dplyr::filter(an_p, sex == "Women"), "Women",
             include_sex = FALSE, ladder = c("M1", "M2")),
  fit_interaction("pship_share", an_p, rhs_m1, "M1"),
  fit_interaction("pship_share", an_p, rhs_m2, "M2")
)

## Never-partnered as a binary, since that tail is the story
res_neverp <- dplyr::bind_rows(
  fit_ladder("ever_partnered", an_p, "Pooled", include_sex = TRUE,  ladder = c("M1", "M2")),
  fit_ladder("ever_partnered", dplyr::filter(an_p, sex == "Men"),   "Men",
             include_sex = FALSE, ladder = c("M1", "M2")),
  fit_ladder("ever_partnered", dplyr::filter(an_p, sex == "Women"), "Women",
             include_sex = FALSE, ladder = c("M1", "M2"))
)

## ===========================================================================
## 4. PREMISE 2 -- FERTILITY
##    Outcome A: number of biological children (OLS on the count -- descriptive,
##               and the coefficient reads directly as children per SD).
##    Outcome B: childlessness (linear probability model, coefficients in
##               percentage points once multiplied by 100).
##
##    M2 - M1 shows how much of the raw association is education.
##    M3 - M2 shows how much of what remains runs through partnering.
## ===========================================================================
res_nkids <- dplyr::bind_rows(
  fit_ladder("nkids", an, "Pooled", include_sex = TRUE),
  fit_ladder("nkids", dplyr::filter(an, sex == "Men"),   "Men",   include_sex = FALSE),
  fit_ladder("nkids", dplyr::filter(an, sex == "Women"), "Women", include_sex = FALSE),
  fit_interaction("nkids", an, rhs_m1, "M1"),
  fit_interaction("nkids", an, rhs_m3, "M3")
)

res_childless <- dplyr::bind_rows(
  fit_ladder("childless", an, "Pooled", include_sex = TRUE),
  fit_ladder("childless", dplyr::filter(an, sex == "Men"),   "Men",   include_sex = FALSE),
  fit_ladder("childless", dplyr::filter(an, sex == "Women"), "Women", include_sex = FALSE),
  fit_interaction("childless", an, rhs_m1, "M1"),
  fit_interaction("childless", an, rhs_m3, "M3")
)

## ===========================================================================
## 5. ROBUSTNESS
## ===========================================================================
## (a) Fertility topped up to age 55 using total own children ever had. Men can
##     still father children after 50, and that asymmetry runs in the same
##     direction as the male under-reporting problem.
an_55 <- an %>% dplyr::filter(!is.na(nkids_55))
rob_55 <- dplyr::bind_rows(
  fit_ladder("nkids_55", dplyr::filter(an_55, sex == "Men"),   "Men, to 55",
             include_sex = FALSE, ladder = c("M1", "M3")),
  fit_ladder("nkids_55", dplyr::filter(an_55, sex == "Women"), "Women, to 55",
             include_sex = FALSE, ladder = c("M1", "M3"))
)

## (b) Excluding people whose fertility count disagrees between the two sources
an_agree <- an %>% dplyr::filter(is.na(nkids_sweep8) | nkids == nkids_sweep8)
rob_agree <- dplyr::bind_rows(
  fit_ladder("nkids", dplyr::filter(an_agree, sex == "Men"),   "Men, sources agree",
             include_sex = FALSE, ladder = c("M1")),
  fit_ladder("nkids", dplyr::filter(an_agree, sex == "Women"), "Women, sources agree",
             include_sex = FALSE, ladder = c("M1"))
)

## ===========================================================================
## 6. HEADLINE TABLE
## ===========================================================================
## One row per trait per sex per outcome, traits only (M1) and fully adjusted
## (M3 where it exists), for the chart in the next script.
headline <- dplyr::bind_rows(
  res_nkids     %>% dplyr::mutate(outcome_label = "Biological children"),
  res_childless %>% dplyr::mutate(outcome_label = "Childless (pp)"),
  res_pship     %>% dplyr::mutate(outcome_label = "Share of years partnered")
) %>%
  dplyr::filter(term %in% Z, sample %in% c("Men", "Women")) %>%
  dplyr::mutate(
    trait = sub("^z_", "", term),
    label = trait_labels[trait],
    ## childlessness and partnering shares read better as percentage points
    scale = dplyr::if_else(outcome_label == "Biological children", 1, 100),
    dplyr::across(c(estimate, std.error, conf.low, conf.high), ~ .x * scale)
  ) %>%
  dplyr::select(outcome_label, model, sample, trait, label,
                estimate, std.error, conf.low, conf.high, p.value, n, r.squared)

print(headline %>% dplyr::filter(model == "M1"), n = 40)

## ===========================================================================
## 7. SAVE
## ===========================================================================
saveRDS(list(pship = res_pship, neverp = res_neverp, nkids = res_nkids,
             childless = res_childless, rob_55 = rob_55, rob_agree = rob_agree,
             headline = headline, desc_traits = desc_traits,
             desc_outcomes = desc_outcomes, parity = parity),
        file.path(DIR$cache, "models.rds"))

readr::write_csv(headline,      file.path(DIR$out, "03_headline.csv"))
readr::write_csv(res_nkids,     file.path(DIR$out, "03_fertility_count.csv"))
readr::write_csv(res_childless, file.path(DIR$out, "03_childlessness.csv"))
readr::write_csv(res_pship,     file.path(DIR$out, "03_partnering.csv"))
readr::write_csv(res_neverp,    file.path(DIR$out, "03_ever_partnered.csv"))
readr::write_csv(desc_traits,   file.path(DIR$out, "03_desc_traits.csv"))
readr::write_csv(desc_outcomes, file.path(DIR$out, "03_desc_outcomes.csv"))
readr::write_csv(parity,        file.path(DIR$out, "03_parity.csv"))

message("03_models.R done. Tables in ", DIR$out)
