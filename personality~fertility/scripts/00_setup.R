## ---------------------------------------------------------------------------
## 00_setup.R -- paths, packages, helpers, IPIP-50 scoring key
## The Archives: personality, partnering and fertility in the 1958 cohort (NCDS)
##
## Run every script with scripts/ as the working directory.
## ---------------------------------------------------------------------------

PROJ <- normalizePath(file.path(getwd(), ".."))

DIR <- list(
  s6137 = file.path(PROJ, "UKDA-6137-stata", "stata", "stata13_se"),  # age 50 sweep
  s9418 = file.path(PROJ, "UKDA-9418-stata", "stata", "stata13"),     # harmonised fertility
  s6940 = file.path(PROJ, "UKDA-6940-stata", "stata", "stata13"),     # partnership histories
  s5560 = file.path(PROJ, "UKDA-5560-stata", "stata", "stata13"),     # response / outcomes
  s7669 = file.path(PROJ, "UKDA-7669-stata", "stata", "stata13"),     # age 55 derived
  cache = file.path(PROJ, "cache"),
  out   = file.path(PROJ, "output")
)
invisible(lapply(DIR[c("cache", "out")], dir.create,
                 showWarnings = FALSE, recursive = TRUE))

suppressPackageStartupMessages({
  library(haven); library(dplyr); library(tidyr); library(purrr)
  library(stringr); library(tibble); library(readr)
})

## --- helpers ---------------------------------------------------------------

## CLS uses negative codes for every flavour of missing (-1 not applicable,
## -8 don't know, -9 refused, and sweep-specific variants). Strip the labels
## and send every negative to NA. Nothing in this analysis is legitimately
## negative, so this is safe -- but it is also the single most consequential
## line in the pipeline, so 02_derive.R reports what it removed.
zap_neg <- function(x) {
  x <- as.numeric(haven::zap_labels(x))
  x[!is.na(x) & x < 0] <- NA
  x
}

zscore <- function(x) as.numeric(scale(x))

## CLS century months: January 1900 = 1. Verified against the age-50 interview
## dates in 02_derive.R rather than assumed.
cmc <- function(year, month) (year - 1900) * 12 + month

## Cronbach's alpha on complete cases of an item matrix
alpha_raw <- function(M) {
  M <- as.matrix(M)
  M <- M[stats::complete.cases(M), , drop = FALSE]
  k <- ncol(M)
  k / (k - 1) * (1 - sum(apply(M, 2, stats::var)) / stats::var(rowSums(M)))
}

## Heteroskedasticity-consistent (HC1) standard errors, no extra dependencies.
## Needed because half the outcomes here are binary (linear probability models).
hc1_se <- function(m) {
  X <- stats::model.matrix(m)
  u <- stats::residuals(m)
  n <- nrow(X); k <- ncol(X)
  bread <- solve(crossprod(X))
  meat  <- crossprod(X * u)
  sqrt(diag(bread %*% meat %*% bread) * n / (n - k))
}

## Tidy an lm with HC1 SEs
tidy_hc1 <- function(m, model_name, outcome, sample_label) {
  b  <- stats::coef(m)
  se <- hc1_se(m)
  tibble::tibble(
    model    = model_name,
    outcome  = outcome,
    sample   = sample_label,
    term     = names(b),
    estimate = as.numeric(b),
    std.error = as.numeric(se),
    statistic = as.numeric(b / se),
    p.value   = 2 * stats::pt(abs(as.numeric(b / se)),
                              df = stats::df.residual(m), lower.tail = FALSE),
    conf.low  = as.numeric(b - 1.96 * se),
    conf.high = as.numeric(b + 1.96 * se),
    n         = stats::nobs(m),
    r.squared = summary(m)$r.squared
  )
}

## --- IPIP-50 scoring key ---------------------------------------------------
## Item suffixes run A..Z then AA..XX in questionnaire order (50 items), giving
## variables N8SCQ2A ... N8SCQ2XX in the age-50 file.
##
## Goldberg's IPIP-50 cycles Extraversion, Agreeableness, Conscientiousness,
## Emotional Stability, Intellect in blocks of five. The reverse-keyed positions
## below were read off the NCDS item wording itself, not assumed -- e.g. position
## 6 is "I don't talk a lot" (reversed extraversion), position 19 is "I seldom
## feel blue" (positive emotional stability).
##
## This matters because the deposited ND8EXT / ND8AGR / ND8CON / ND8EMO / ND8INT
## were flagged by CLS as incorrectly derived. 02_derive.R rebuilds them from
## the items and reports how far the deposited versions diverge.

ipip_suffix <- c(LETTERS, paste0(LETTERS[1:24], LETTERS[1:24]))
stopifnot(length(ipip_suffix) == 50L)

ipip_key <- tibble::tibble(
  pos   = 1:50,
  var   = paste0("N8SCQ2", ipip_suffix),
  trait = rep(c("E", "A", "C", "ES", "O"), times = 10)
) %>%
  dplyr::mutate(
    reverse = pos %in% c(
      6, 16, 26, 36, 46,                 # extraversion
      2, 12, 22, 32,                     # agreeableness
      8, 18, 28, 38,                     # conscientiousness
      4, 14, 24, 29, 34, 39, 44, 49,     # emotional stability
      10, 20, 30                         # openness / intellect
    )
  )

stopifnot(sum(ipip_key$reverse) == 24L,
          all(table(ipip_key$trait) == 10L))

trait_levels <- c("E", "A", "C", "ES", "O")
trait_labels <- c(E  = "Extraversion",
                  A  = "Agreeableness",
                  C  = "Conscientiousness",
                  ES = "Emotional stability",
                  O  = "Openness / intellect")

## Exposure window for the partnering outcome: age 16 to age 50
AGE_START <- 16L
AGE_END   <- 50L
WINDOW_MONTHS <- (AGE_END - AGE_START) * 12L   # 408

message("00_setup.R loaded. Project root: ", PROJ)
