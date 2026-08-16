## ---------------------------------------------------------------------------
## 01_read.R -- read the five UKDS Stata deposits, keep only what is needed,
##              harmonise the ID, cache as .rds
##
## Nothing here transforms values. All recoding happens in 02_derive.R so the
## raw extract stays auditable.
## ---------------------------------------------------------------------------

source("00_setup.R")

## Every file uses NCDSID as the linking key, but the case differs between
## deposits and some are stored with trailing whitespace.
clean_id <- function(x) toupper(stringr::str_trim(as.character(x)))

## --- SN 6137: age 50 sweep --------------------------------------------------
## The personality inventory, the sweep-8 fertility counts and the sweep-8
## partnership status. 4,285 variables in the deposit; we take ~70.
ncds8 <- haven::read_dta(
  file.path(DIR$s6137, "ncds_2008_followup.dta"),
  col_select = dplyr::all_of(c(
    "NCDSID", "ND8SEX",
    ipip_key$var,                                        # the 50 IPIP items
    "ND8EXT", "ND8AGR", "ND8CON", "ND8EMO", "ND8INT",    # deposited (suspect) scores
    "ND8NCHTT", "ND8NCHHH", "ND8NCHAB", "ND8OCHTT",      # fertility at 50
    "ND8MS", "ND8COHAB", "ND8SPPHH", "ND8ALHH",          # partnership at 50
    "ND8NS3P", "ND8ECACT"                                # adult class / activity
  ))
) %>%
  dplyr::rename(id = NCDSID) %>%
  dplyr::mutate(id = clean_id(id))

## --- SN 9418: harmonised fertility histories --------------------------------
## Biological children kept separate from step, adopted and fostered children.
## NCDS coverage runs ages 23-50; there is no _55 or _62 block in this file.
fert <- haven::read_dta(
  file.path(DIR$s9418, "harmonised_fertility_histories_ncds.dta")
) %>%
  dplyr::rename(id = ncdsid) %>%
  dplyr::mutate(id = clean_id(id))

## --- SN 6940: partnership histories (spell level) ---------------------------
## One row per co-residential partnership, marriage and cohabitation both.
pship <- haven::read_dta(
  file.path(DIR$s6940, "ncds_partnership_histories_v2.dta")
) %>%
  dplyr::rename(id = NCDSID) %>%
  dplyr::mutate(id = clean_id(id))

## --- SN 5560: response and outcomes -----------------------------------------
## The full 18,558-member cohort frame. Used only to document who is missing
## from the analytic sample.
resp <- haven::read_dta(
  file.path(DIR$s5560, "ncds_response.dta")
) %>%
  dplyr::rename(id = ncdsid) %>%
  dplyr::mutate(id = clean_id(id))

## --- SN 7669: age 55 derived ------------------------------------------------
## Education (highest NVQ level accumulated to 2013) is not available as a
## derived variable in the age-50 deposit, so it comes from here. ND9TOTCE is
## a fertility top-up: total own children ever had, reported at 55.
educ <- haven::read_dta(
  file.path(DIR$s7669, "ncds_2013_derived.dta"),
  col_select = dplyr::all_of(c("NCDSID", "ND9HNVQ", "ND9HACHQ",
                               "ND9HQFLG", "ND9TOTCE", "ND9PARTP"))
) %>%
  dplyr::rename(id = NCDSID) %>%
  dplyr::mutate(id = clean_id(id))

## --- sanity checks on the link ----------------------------------------------
stopifnot(
  !anyDuplicated(ncds8$id),
  !anyDuplicated(fert$id),
  !anyDuplicated(resp$id),
  !anyDuplicated(educ$id)
)

link_report <- tibble::tibble(
  source = c("ncds8 (SN 6137)", "fert (SN 9418)", "pship (SN 6940)",
             "resp (SN 5560)", "educ (SN 7669)"),
  rows   = c(nrow(ncds8), nrow(fert), nrow(pship), nrow(resp), nrow(educ)),
  people = c(dplyr::n_distinct(ncds8$id), dplyr::n_distinct(fert$id),
             dplyr::n_distinct(pship$id), dplyr::n_distinct(resp$id),
             dplyr::n_distinct(educ$id)),
  in_resp = c(sum(ncds8$id %in% resp$id), sum(unique(fert$id) %in% resp$id),
              sum(unique(pship$id) %in% resp$id), NA_integer_,
              sum(educ$id %in% resp$id))
)
print(link_report)

## Every analysis ID should exist in the cohort frame. If this fails the ID
## cleaning above is wrong, not the data.
stopifnot(all(ncds8$id %in% resp$id))

## --- cache ------------------------------------------------------------------
saveRDS(ncds8, file.path(DIR$cache, "ncds8.rds"))
saveRDS(fert,  file.path(DIR$cache, "fert.rds"))
saveRDS(pship, file.path(DIR$cache, "pship.rds"))
saveRDS(resp,  file.path(DIR$cache, "resp.rds"))
saveRDS(educ,  file.path(DIR$cache, "educ.rds"))
readr::write_csv(link_report, file.path(DIR$out, "01_link_report.csv"))

message("01_read.R done. ", nrow(ncds8), " age-50 respondents cached.")
