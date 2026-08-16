## ---------------------------------------------------------------------------
## 02_derive.R -- build the analysis frame
##   * rebuild the Big Five from the 50 IPIP items (the deposited scores are
##     flagged by CLS as incorrectly derived)
##   * biological fertility at 50, kept clear of step/adopted children
##   * share of adult months (age 16-50) spent in a co-residential union
##   * education, and an attrition audit against the full cohort
## ---------------------------------------------------------------------------

source("00_setup.R")

ncds8 <- readRDS(file.path(DIR$cache, "ncds8.rds"))
fert  <- readRDS(file.path(DIR$cache, "fert.rds"))
pship <- readRDS(file.path(DIR$cache, "pship.rds"))
resp  <- readRDS(file.path(DIR$cache, "resp.rds"))
educ  <- readRDS(file.path(DIR$cache, "educ.rds"))

audit <- list()   # collects everything worth reporting in the piece

## ===========================================================================
## 1. SEX
## ===========================================================================
sex_lab <- tolower(as.character(haven::as_factor(ncds8$ND8SEX)))
sex_vec <- dplyr::case_when(
  stringr::str_detect(sex_lab, "^m")      ~ "Men",
  stringr::str_detect(sex_lab, "^f|^w")   ~ "Women",
  TRUE                                    ~ NA_character_
)
if (all(is.na(sex_vec))) {
  warning("ND8SEX carried no usable value labels; falling back to 1 = Men, 2 = Women")
  sex_vec <- dplyr::case_when(zap_neg(ncds8$ND8SEX) == 1 ~ "Men",
                              zap_neg(ncds8$ND8SEX) == 2 ~ "Women",
                              TRUE ~ NA_character_)
}
base <- tibble::tibble(id = ncds8$id,
                       sex = factor(sex_vec, levels = c("Men", "Women")))

## Cross-check against the sex recorded in the harmonised fertility file
sex_check <- base %>%
  dplyr::left_join(
    fert %>% dplyr::transmute(
      id,
      sex_fert = dplyr::case_when(
        stringr::str_detect(tolower(as.character(haven::as_factor(sex))), "^m") ~ "Men",
        stringr::str_detect(tolower(as.character(haven::as_factor(sex))), "^f|^w") ~ "Women",
        TRUE ~ NA_character_)),
    by = "id") %>%
  dplyr::summarise(mismatch = sum(!is.na(sex_fert) & as.character(sex) != sex_fert))
audit$sex_mismatch <- sex_check$mismatch
message("Sex mismatches between SN 6137 and SN 9418: ", audit$sex_mismatch)

## ===========================================================================
## 2. PERSONALITY -- rebuild the IPIP-50
## ===========================================================================
items <- ncds8 %>%
  dplyr::select(id, dplyr::all_of(ipip_key$var)) %>%
  dplyr::mutate(dplyr::across(-id, zap_neg))

item_rng <- range(unlist(items[-1]), na.rm = TRUE)
message("Observed IPIP item range: ", item_rng[1], "-", item_rng[2])
## The NCDS self-completion uses a 5-point accuracy scale. Stop rather than
## silently mis-reverse if the deposit ever changes.
stopifnot(item_rng[1] >= 1, item_rng[2] <= 5)
REV_BASE <- item_rng[1] + item_rng[2]        # 6 for a 1-5 scale

rev_vars <- ipip_key$var[ipip_key$reverse]
items_scored <- items %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(rev_vars), ~ REV_BASE - .x))

## Score each trait as the mean of its available items, rescaled to the 10-50
## sum metric, requiring at least 8 of 10 items answered.
MIN_ITEMS <- 8L
score_trait <- function(tr) {
  vs <- ipip_key$var[ipip_key$trait == tr]
  M  <- as.matrix(items_scored[, vs])
  n_ok <- rowSums(!is.na(M))
  out <- rowMeans(M, na.rm = TRUE) * 10
  out[n_ok < MIN_ITEMS] <- NA_real_
  out
}
big5 <- tibble::tibble(id = items_scored$id)
for (tr in trait_levels) big5[[paste0("bf_", tr)]] <- score_trait(tr)

## Reliability of each rebuilt scale
audit$alpha <- stats::setNames(
  purrr::map_dbl(trait_levels,
                 ~ alpha_raw(items_scored[, ipip_key$var[ipip_key$trait == .x]])),
  trait_levels)
print(round(audit$alpha, 3))

## How badly do the deposited scores diverge? CLS flagged ND8* as incorrectly
## derived; this is the check that tells us whether the re-deposit fixed it.
deposited <- ncds8 %>%
  dplyr::transmute(id,
                   dep_E  = zap_neg(ND8EXT), dep_A  = zap_neg(ND8AGR),
                   dep_C  = zap_neg(ND8CON), dep_ES = zap_neg(ND8EMO),
                   dep_O  = zap_neg(ND8INT))
cmp <- big5 %>% dplyr::left_join(deposited, by = "id")
audit$deposited_cor <- stats::setNames(
  purrr::map_dbl(trait_levels,
                 ~ stats::cor(cmp[[paste0("bf_", .x)]], cmp[[paste0("dep_", .x)]],
                              use = "complete.obs")),
  trait_levels)
audit$deposited_meandiff <- stats::setNames(
  purrr::map_dbl(trait_levels,
                 ~ mean(cmp[[paste0("bf_", .x)]] - cmp[[paste0("dep_", .x)]],
                        na.rm = TRUE)),
  trait_levels)
message("Correlation of rebuilt vs deposited scores:")
print(round(audit$deposited_cor, 4))
message("Mean difference (rebuilt - deposited):")
print(round(audit$deposited_meandiff, 3))

## ===========================================================================
## 3. FERTILITY
## ===========================================================================
fert50 <- fert %>%
  dplyr::transmute(
    id,
    cmbyear    = zap_neg(cmbyear),
    cmbmonth   = zap_neg(cmbmonth),
    in_50      = zap_neg(ncdssurvey_50),
    intyear_50 = zap_neg(intyear_50),
    intmonth_50 = zap_neg(intmonth_50),
    nkids      = zap_neg(biochild_tot_50),
    any_kids   = zap_neg(anybiochildren_50),
    nkids_hh   = zap_neg(biochildhh_total_50),
    nkids_nonhh = zap_neg(biochildnonhh_total_50),
    n_step     = zap_neg(step_tot_50),
    n_adopt    = zap_neg(adopt_tot_50),
    age_first_birth = zap_neg(cmageybirth_eldest_50),
    partner_50 = zap_neg(partner_50)
  )

## Sweep-8 fertility count from the age-50 deposit, as an independent read on
## the same quantity. These are derived differently; disagreement is
## informative rather than fatal.
fert_alt <- ncds8 %>% dplyr::transmute(id, nkids_sweep8 = zap_neg(ND8NCHTT))

## Age-55 top-up: total own children ever had, reported at 55. Used only in a
## robustness check, because men can still have children after 50.
fert_55 <- educ %>% dplyr::transmute(id, nkids_55 = zap_neg(ND9TOTCE))

## ===========================================================================
## 4. PARTNERING -- share of months age 16-50 in a co-residential union
## ===========================================================================
birth <- fert50 %>%
  dplyr::transmute(
    id,
    birth_cmc = dplyr::if_else(is.na(cmbyear) | is.na(cmbmonth),
                               cmc(1958, 3),          # NCDS: born 3-9 March 1958
                               cmc(cmbyear, cmbmonth)),
    birth_imputed = is.na(cmbyear) | is.na(cmbmonth)
  )
audit$birth_imputed <- sum(birth$birth_imputed)

## Verify the century-month convention rather than trusting it: age at the
## age-50 interview should sit close to 50.
age_check <- fert50 %>%
  dplyr::left_join(birth, by = "id") %>%
  dplyr::filter(!is.na(intyear_50), !is.na(intmonth_50)) %>%
  dplyr::mutate(age_at_int = (cmc(intyear_50, intmonth_50) - birth_cmc) / 12)
audit$median_age_at_50 <- stats::median(age_check$age_at_int, na.rm = TRUE)
message("Median age at the 'age 50' interview: ",
        round(audit$median_age_at_50, 2))
stopifnot(abs(audit$median_age_at_50 - 50) < 2)

spells <- pship %>%
  dplyr::transmute(
    id,
    pstart  = zap_neg(PSTCMC),
    pend    = zap_neg(PENDCMC),
    lastint = zap_neg(LSTSWCMC),
    n_pships = zap_neg(NUMPSHPS),
    ptype   = zap_neg(PTYPE),
    case_err = zap_neg(CERRORS)
  ) %>%
  dplyr::left_join(birth, by = "id") %>%
  dplyr::mutate(
    w_start = birth_cmc + AGE_START * 12,
    w_end   = birth_cmc + AGE_END   * 12,
    ## an open spell runs to the last interview, capped at the window
    end_eff = dplyr::if_else(is.na(pend), pmin(lastint, w_end, na.rm = TRUE), pend),
    overlap = dplyr::case_when(
      !is.na(n_pships) & n_pships == 0            ~ 0,      # never partnered
      is.na(pstart) | is.na(end_eff)              ~ NA_real_,
      TRUE ~ pmax(0, pmin(end_eff, w_end) - pmax(pstart, w_start))
    )
  )

partnering <- spells %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(
    months_partnered = sum(overlap, na.rm = TRUE),
    n_spells_dated   = sum(!is.na(pstart)),
    n_pships         = dplyr::first(n_pships),
    spell_dates_missing = any(is.na(overlap)),
    case_err         = if (all(is.na(case_err))) NA_real_ else max(case_err, na.rm = TRUE),
    covered_to_50    = any(!is.na(lastint) & !is.na(w_end) & lastint >= w_end),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    months_partnered = pmin(months_partnered, WINDOW_MONTHS),
    pship_share      = months_partnered / WINDOW_MONTHS,
    ever_partnered   = as.integer(months_partnered > 0),
    ## the outcome is only meaningful if the histories actually run to age 50
    pship_share      = dplyr::if_else(covered_to_50, pship_share, NA_real_)
  )

audit$pship_no_coverage <- sum(!partnering$covered_to_50)
audit$pship_dates_missing <- sum(partnering$spell_dates_missing)

## ===========================================================================
## 5. EDUCATION
## ===========================================================================
education <- educ %>%
  dplyr::transmute(
    id,
    nvq_num = zap_neg(ND9HNVQ),
    nvq = factor(nvq_num, levels = sort(unique(stats::na.omit(nvq_num))))
  )

## ===========================================================================
## 6. ASSEMBLE
## ===========================================================================
an_full <- base %>%
  dplyr::left_join(big5, by = "id") %>%
  dplyr::left_join(fert50 %>% dplyr::select(-cmbyear, -cmbmonth), by = "id") %>%
  dplyr::left_join(fert_alt, by = "id") %>%
  dplyr::left_join(fert_55, by = "id") %>%
  dplyr::left_join(partnering %>% dplyr::select(id, pship_share, ever_partnered,
                                                n_pships, covered_to_50),
                   by = "id") %>%
  dplyr::left_join(education %>% dplyr::select(id, nvq), by = "id") %>%
  dplyr::mutate(
    childless = dplyr::if_else(is.na(nkids), NA_integer_, as.integer(nkids == 0)),
    has_big5  = !dplyr::if_any(dplyr::all_of(paste0("bf_", trait_levels)), is.na)
  )

## Disagreement between the two fertility measures, worth a line in the piece
audit$fert_disagree <- an_full %>%
  dplyr::filter(!is.na(nkids), !is.na(nkids_sweep8)) %>%
  dplyr::summarise(n = dplyr::n(),
                   disagree = sum(nkids != nkids_sweep8),
                   pct = 100 * mean(nkids != nkids_sweep8))
print(audit$fert_disagree)

## --- analytic sample --------------------------------------------------------
## Fertility models: personality + biological children, both observed.
an <- an_full %>% dplyr::filter(has_big5, !is.na(nkids), !is.na(sex))

## Z-scores are computed on the analytic sample, pooling men and women, so that
## a coefficient reads as "per SD of the trait in this cohort" and the sex
## difference in trait means is preserved rather than differenced away.
for (tr in trait_levels) an[[paste0("z_", tr)]] <- zscore(an[[paste0("bf_", tr)]])

message("Analytic sample: ", nrow(an), " (",
        sum(an$sex == "Men"), " men, ", sum(an$sex == "Women"), " women)")

## ===========================================================================
## 7. ATTRITION AUDIT
## ===========================================================================
## Who ends up in the analytic sample, relative to the full 18,558-member
## cohort frame. Complete cases with the loss documented, not modelled away.
cohort <- resp %>%
  dplyr::transmute(
    id,
    sex_birth = dplyr::case_when(zap_neg(n622) == 1 ~ "Men",
                                 zap_neg(n622) == 2 ~ "Women",
                                 TRUE ~ NA_character_),
    ethnic   = as.character(haven::as_factor(ethnicid)),
    resp_33  = as.integer(zap_neg(outcme05) == 1),
    resp_42  = as.integer(zap_neg(outcme06) == 1),
    resp_50  = as.integer(zap_neg(outcme08) == 1)
  ) %>%
  dplyr::mutate(
    stage = dplyr::case_when(
      id %in% an$id                                  ~ "3. In analytic sample",
      id %in% an_full$id                             ~ "2. At age 50, dropped",
      TRUE                                           ~ "1. Never reached age 50"
    )
  )

attrition <- cohort %>%
  dplyr::group_by(stage) %>%
  dplyr::summarise(
    n            = dplyr::n(),
    pct_women    = 100 * mean(sex_birth == "Women", na.rm = TRUE),
    pct_resp_33  = 100 * mean(resp_33, na.rm = TRUE),
    pct_resp_42  = 100 * mean(resp_42, na.rm = TRUE),
    .groups = "drop"
  )
print(attrition)

## Why cases are lost between the age-50 sweep and the analytic sample
dropout_reasons <- an_full %>%
  dplyr::summarise(
    at_age_50            = dplyr::n(),
    missing_big5         = sum(!has_big5),
    missing_fertility    = sum(is.na(nkids)),
    missing_either       = sum(!has_big5 | is.na(nkids)),
    missing_education    = sum(is.na(nvq)),
    missing_pship_share  = sum(is.na(pship_share))
  )
print(dropout_reasons)

## ===========================================================================
## 8. SAVE
## ===========================================================================
saveRDS(an,      file.path(DIR$cache, "analysis.rds"))
saveRDS(an_full, file.path(DIR$cache, "analysis_full.rds"))
saveRDS(audit,   file.path(DIR$cache, "audit.rds"))

readr::write_csv(attrition,        file.path(DIR$out, "02_attrition.csv"))
readr::write_csv(dropout_reasons,  file.path(DIR$out, "02_dropout_reasons.csv"))
readr::write_csv(
  tibble::tibble(trait = trait_levels,
                 label = unname(trait_labels[trait_levels]),
                 alpha = as.numeric(audit$alpha),
                 cor_with_deposited = as.numeric(audit$deposited_cor),
                 mean_diff = as.numeric(audit$deposited_meandiff)),
  file.path(DIR$out, "02_scale_diagnostics.csv"))

message("02_derive.R done.")

