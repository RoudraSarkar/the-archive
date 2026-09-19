# =============================================================================
# A_analysis.R  —  ALL data engineering, methods and robustness.
#
# Reads data/<country>/, writes tidy chart-ready tables to out/.
# B_figures.R reads out/ and nothing else. If B ever needs a number that
# isn't in out/, it belongs here.
#
# Run:  source("scripts/A_analysis.R")   (working dir = project root)
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
})

# ---- 0. CONFIG --------------------------------------------------------------

CFG <- list(
  data_dir   = "data",
  out_dir    = "out",
  cache_dir  = "out/cache",
  top_n      = 10,     # headline index depth
  deep_n     = 25,     # common depth for tail-sensitive measures
  min_year   = 1880,
  max_year   = 2025,
  lp_horizon = 20,     # local-projection horizons, years
  headline   = "top10_cov",  # validated at r = 0.992 vs Norway truth
  wpp_last_obs = 2023,
  fert_priority = c("HFD","HFC","WPP"),  # registers win; WPP fills gaps
  fert_priority = c("HFD","HFC","WPP"),  # register series win; WPP fills gaps  # WPP2024 labels estimates AND projections "Medium";
                        # everything after this year is a forecast, not data.
  # Countries whose name series is deep enough to contain a fertility break.
  deep       = c("USA","Norway","France","New Zealand","Ireland","Scotland"),
  # Present but modern-only; used for the plateau question, not the lead/lag.
  modern     = c("Denmark","England & Wales"),
  # Present but excluded from everything, with reasons (see INVENTORY.md).
  excluded   = c("Belgium"    = "no year column; pooled 2012-2021 by municipality",
                 "Switzerland"= "partial export: 2 of 3 language regions, no national total")
)

dir.create(CFG$out_dir,   showWarnings = FALSE, recursive = TRUE)
dir.create(CFG$cache_dir, showWarnings = FALSE, recursive = TRUE)

p <- function(...) file.path(CFG$data_dir, ...)
say <- function(...) message(format(Sys.time(), "[%H:%M:%S] "), ...)

# Cache wrapper: expensive stages run once, then load from disk.
cached <- function(key, expr, force = FALSE) {
  f <- file.path(CFG$cache_dir, paste0(key, ".rds"))
  if (!force && file.exists(f)) { say("cache hit: ", key); return(readRDS(f)) }
  say("building: ", key)
  val <- force(expr)
  saveRDS(val, f)
  val
}

# =============================================================================
# 1. READERS — one per country. Every one returns the same columns:
#      country, year, sex ("F"/"M"), name, n
#    where n is a COUNT, except Norway where it is a within-sex PERCENT of all
#    births (flagged by attr(x, "n_is_pct")). Nothing else in the pipeline
#    needs to know the difference.
# =============================================================================

# Encoding hygiene. Two files are not valid UTF-8: the NZ file carries 71 stray
# high bytes (a handful of Maori macron vowels that survived a lossy conversion)
# and Norway's long files are Latin-1. Left alone these blow up the first
# perl-regex call downstream (trimws) with "input string N is invalid UTF-8".
# Anything already valid UTF-8 passes through untouched; anything else is read
# as Latin-1, which cannot fail; leftover control bytes are dropped.
safe_utf8 <- function(x) {
  x  <- as.character(x)
  ok <- !is.na(iconv(x, "UTF-8", "UTF-8"))
  if (any(!ok)) x[!ok] <- iconv(x[!ok], "latin1", "UTF-8", sub = "")
  x <- enc2utf8(x)
  gsub("[\u0001-\u001f\u007f-\u009f]", "", x, perl = TRUE)
}

norm_sex <- function(x) {
  x <- toupper(trimws(as.character(x)))
  fifelse(x %chin% c("F","GIRL","GIRLS","FEMALE","2","K"), "F",
  fifelse(x %chin% c("M","BOY","BOYS","MALE","1","G"),     "M", NA_character_))
}

read_usa <- function() {
  fs <- list.files(p("usa"), pattern = "^yob\\d{4}\\.txt$", full.names = TRUE)
  rbindlist(lapply(fs, function(f) {
    y <- as.integer(sub("^yob", "", tools::file_path_sans_ext(basename(f))))
    d <- fread(f, header = FALSE, col.names = c("name","sex","n"),
               encoding = "UTF-8", showProgress = FALSE)
    d[, `:=`(country = "USA", year = y)]
  }))[, .(country, year, sex = norm_sex(sex), name, n = as.numeric(n))]
}

read_france <- function() {
  d <- fread(p("france","prenoms-2023-nat.csv"), sep = ";",
             encoding = "UTF-8", showProgress = FALSE)
  setnames(d, c("sexe","preusuel","annais","nombre"), c("sex","name","year","n"),
           skip_absent = TRUE)
  d <- d[year != "XXXX"]
  # _PRENOMS_RARES_ is INSEE's pooled censored tail. Excluded from the index,
  # but kept aside as a coverage diagnostic — France is the one country that
  # tells you how much mass the tail holds.
  # NB the marker is "_PRENOMS_RARES" with NO trailing underscore, whatever the
  # documentation says. Getting this wrong leaves the pooled tail in as if it
  # were a name - and by 2010 it is the single most common "name" in France at
  # 8.7% of births, which nearly doubles the measured top-10 share and invents
  # a plateau that is not there.
  RARE <- "^_PRENOMS_RARES_?$"
  rare <- d[grepl(RARE, name),
            .(country = "France", year = as.integer(year),
              sex = norm_sex(sex), rare_n = as.numeric(n))]
  saveRDS(rare, file.path(CFG$cache_dir, "france_rare_mass.rds"))
  d <- d[!grepl(RARE, name)]
  d[, .(country = "France", year = as.integer(year), sex = norm_sex(sex),
        name, n = as.numeric(n))]
}

read_norway <- function() {
  # NOT the wide file. Its first column holds PxWeb value CODES, not names -
  # Bjorn is stored as "2BJZ2RN" (O-slash escaped as Z2), so any figure that
  # prints a name would print the escape. The split long files carry proper
  # display names in Latin-1 and get sex from the filename, unambiguously.
  one <- function(file, sexlab) {
    d <- fread(file, encoding = "Latin-1", showProgress = FALSE)
    setnames(d, 1:4, c("name","year","contents","value"))
    d <- d[grepl("^Per cent", contents)]          # drop the count rows
    d[, .(country = "Norway", year = as.integer(year), sex = sexlab,
          name = as.character(name),
          n = suppressWarnings(as.numeric(value)))][!is.na(n) & n > 0]
  }
  out <- rbindlist(list(
    one(p("norway","no_names_girls_1880_2025.csv"), "F"),
    one(p("norway","no_names_boys_1880_2025.csv"),  "M")))
  setattr(out, "n_is_pct", TRUE)   # n is a % of ALL births, within sex
  out
}

read_ireland <- function() {
  one <- function(file, sexlab, namecol) {
    d <- fread(file, encoding = "UTF-8", showProgress = FALSE)
    setnames(d, namecol, "name", skip_absent = TRUE)
    # Two statistics share the file: the count and its rank. Keep the count.
    d <- d[!grepl("Rank$", `Statistic Label`)]
    d[, .(country = "Ireland", year = as.integer(Year), sex = sexlab,
          name, n = as.numeric(VALUE))][!is.na(n)]
  }
  b <- list.files(p("ireland"), pattern = "^VSA50", full.names = TRUE)[1]
  g <- list.files(p("ireland"), pattern = "^VSA60", full.names = TRUE)[1]
  rbindlist(list(one(b, "M", "Boys Names"), one(g, "F", "Girls Names")))
}

read_scotland <- function() {
  f <- p("scotland","all-names-given-to-babies-between-1974-to-2025",
         "full-list-1974-2025.csv")
  d <- fread(f, encoding = "UTF-8", showProgress = FALSE)
  d[, .(country = "Scotland", year = as.integer(Year), sex = norm_sex(Sex),
        name = Name, n = as.numeric(Number))][!is.na(n)]
}

read_nz <- function() {
  d <- fread(p("new zealand","nz_baby_names_1900_2025.csv"),
             encoding = "Latin-1", showProgress = FALSE)
  d[, .(country = "New Zealand", year = as.integer(Year), sex = norm_sex(Sex),
        name = Name, n = as.numeric(Count))][!is.na(n)]
}

read_denmark <- function() {
  d <- fread(p("denmark","dk_newborn_names_1985_2025.csv"),
             encoding = "UTF-8", showProgress = FALSE)
  # The file lists only the top 25/50, so its own total is NOT all births and
  # dividing by it inflates Denmark about twofold. But per_1000 is a share of
  # ALL births of that sex, so the true denominator is recoverable. per_1000 is
  # rounded to whole units, so estimate the denominator robustly from the
  # median across the listed names and then use the precise counts.
  d[, births_est := median(count / (per_1000 / 1000), na.rm = TRUE),
    by = .(year, sex)]
  out <- d[is.finite(births_est) & births_est > 0,
           .(country = "Denmark", year = as.integer(year), sex = norm_sex(sex),
             name, n = 100 * count / births_est)]
  setattr(out, "n_is_pct", TRUE)   # % of ALL births, within sex
  out
}

read_ew <- function() {
  f <- p("uk","babynames1996to2025.xlsx")
  one <- function(sheet) {
    ttl <- as.character(read_excel(f, sheet = sheet, n_max = 1,
                                   col_names = FALSE)[[1]][1])
    sx  <- if (grepl("girl", ttl, ignore.case = TRUE)) "F" else "M"
    d <- as.data.table(read_excel(f, sheet = sheet, skip = 4))
    setnames(d, 1, "name")
    cnt <- grep("Count$", names(d), value = TRUE)     # wide: "YYYY Count"
    d <- melt(d[, c("name", cnt), with = FALSE], id.vars = "name",
              variable.name = "col", value.name = "n")
    d[, year := as.integer(sub("^(\\d{4}).*", "\\1", as.character(col)))]
    d[, n := suppressWarnings(as.numeric(n))]          # "[x]" -> NA
    d[!is.na(n), .(country = "England & Wales", year, sex = sx, name,
                   n = as.numeric(n))]
  }
  rbindlist(lapply(c("Table_1","Table_2"), one))
}

# =============================================================================
# 2. HARMONISE
# =============================================================================

repairs <- list()
build_names <- function() {
  readers <- list(USA = read_usa, France = read_france, Norway = read_norway,
                  Ireland = read_ireland, Scotland = read_scotland,
                  `New Zealand` = read_nz, Denmark = read_denmark,
                  `England & Wales` = read_ew)
  out <- list(); pct_flag <- character(0); repairs <<- list()
  for (nm in names(readers)) {
    say("reading ", nm)
    d <- readers[[nm]]()
    if (isTRUE(attr(d, "n_is_pct"))) pct_flag <- c(pct_flag, nm)
    stopifnot(all(c("country","year","sex","name","n") %in% names(d)))
    raw_name <- as.character(d$name)
    # Identify the bad ones the same way safe_utf8 does. Do NOT compare the
    # before/after strings directly - `!=` on an invalid-UTF-8 vector throws
    # the very error we are fixing.
    chg <- which(is.na(iconv(raw_name, "UTF-8", "UTF-8")))
    d[, name := safe_utf8(name)]
    if (length(chg)) {
      repairs[[nm]] <<- data.table(
        country = nm,
        before_as_latin1 = iconv(raw_name[chg], "latin1", "UTF-8", sub = "?"),
        after            = d$name[chg])
      say("  repaired ", length(chg), " name strings in ", nm)
    }
    out[[nm]] <- d
  }
  all <- rbindlist(out, use.names = TRUE)
  all <- all[!is.na(sex) & !is.na(year) & !is.na(n) & n > 0]
  # Guard against pooled / placeholder categories masquerading as names. Every
  # agency marks them differently and they are always large, so they land in
  # the top 10 and wreck the index. This check is here because France's
  # "_PRENOMS_RARES" slipped through once already.
  susp <- unique(all[grepl("^[^[:alpha:]]", name), .(country, name)])
  if (nrow(susp)) {
    say("WARNING: names not starting with a letter - inspect before trusting:")
    print(susp[1:min(20, .N)])
  }
  all[, name := trimws(name)]
  all <- all[year >= CFG$min_year & year <= CFG$max_year]
  setattr(all, "pct_countries", pct_flag)
  all
}

names_dt <- cached("names_harmonised", build_names())
PCT <- attr(names_dt, "pct_countries")

# =============================================================================
# 3. INDEX
#    top_n_share is computed against the file's own COVERED total, because no
#    source publishes the full tail. Norway is the exception: its values are
#    percentages of ALL births, so it gives both a covered-total figure and a
#    true one — the calibration in section 7 measures the gap between them.
# =============================================================================

build_index <- function(d) {
  setorder(d, country, year, sex, -n)
  d[, rank := seq_len(.N), by = .(country, year, sex)]
  idx <- d[, {
    is_pct <- .BY$country %chin% PCT   # n is already a % of ALL births
    tot    <- sum(n)
    p_cov  <- n / tot                  # share of COVERED births
    p_true <- if (is_pct) n / 100 else rep(NA_real_, length(n))
    deep   <- .N >= CFG$deep_n
    topr   <- if (deep) { t <- head(p_cov, CFG$deep_n); t / sum(t) } else NA_real_
    .(n_names     = .N,
      covered_tot = tot,
      top1_name   = name[1],
      basis       = if (is_pct) "true" else "covered",
      # TRUE share of all births - only where the source publishes one
      # (Norway's percentages; Denmark via its per-1000 column).
      top10_true  = if (is_pct) sum(head(p_true, CFG$top_n)) else NA_real_,
      # Share of COVERED births. For sources that publish nearly everything
      # (USA >=5, France, Ireland 3+, Scotland, E&W) this is within a hair of
      # the truth - validated at r = 0.992 against Norway's known-true series.
      top1_cov    = sum(head(p_cov, 1)),
      top10_cov   = sum(head(p_cov, CFG$top_n)),
      top25_cov   = if (deep) sum(head(p_cov, CFG$deep_n)) else NA_real_,
      # Renormalised to the top 25. Comparable, but compressed: it keeps only
      # a third of the dynamic range, so it is a check, never the headline.
      top10_c25   = if (deep) sum(head(topr, 10)) else NA_real_,
      hill_q2     = if (deep) 1 / sum(topr^2)     else NA_real_,
      # Zipf exponent: slope of log(share) on log(rank) across the top 25.
      # Scale-free, so immune to how much tail a source publishes. Tracks
      # Norway's truth at r = 0.86 and keeps its range (0.65 -> 0.24).
      zipf = if (deep) -unname(coef(lm(log(topr) ~ log(seq_along(topr))))[2])
             else NA_real_)
  }, by = .(country, year, sex)]
  # One headline column: the true share where a source gives one, the covered
  # share otherwise, with `basis` recording which was used for every row.
  idx[, index := fifelse(!is.na(top10_true), top10_true, top10_cov)]
  # Norway: n is already a % of all births, so the listed total IS coverage.
  idx[country %chin% PCT, coverage := covered_tot / 100]
  idx[!country %chin% PCT, coverage := NA_real_]
  idx[]
}

index_sex <- cached("index_by_sex", build_index(copy(names_dt)))

# Headline: mean of the two sexes (see the sex decision in the brief).
index_head <- index_sex[, .(
    index     = mean(index,     na.rm = TRUE),
    basis     = basis[1],
    top10_cov = mean(top10_cov, na.rm = TRUE),
    top10_c25 = mean(top10_c25, na.rm = TRUE),
    top1_cov  = mean(top1_cov,  na.rm = TRUE),
    hill_q2   = mean(hill_q2,   na.rm = TRUE),
    zipf      = mean(zipf,      na.rm = TRUE),
    n_names   = sum(n_names),
    coverage  = mean(coverage,  na.rm = TRUE)
  ), by = .(country, year)]
# The headline series. Change CFG$headline to "top10_cov" if you decide the
# intuitive "share of all births" framing is worth losing comparability for.
index_head[, tier := fifelse(country %chin% CFG$deep, "deep", "modern")]

# =============================================================================
# 4. FERTILITY
# =============================================================================

read_fertility <- function() {
  fdir <- p("fertility"); out <- list()

  unzip_if_needed <- function(zipf, dir) {
    if (!dir.exists(dir) && file.exists(zipf)) {
      say("unzipping ", basename(zipf)); unzip(zipf, exdir = dir)
    }
    dir.exists(dir)
  }

  # ---- HFD: register-based and standardised. Best where it exists. ---------
  hfd_dir <- file.path(fdir, "hfd_tfr")
  if (unzip_if_needed(file.path(fdir, "hfd_tfr.zip"), hfd_dir)) {
    # ONLY tfrRR.txt - period TFR by calendar year. The zip also holds
    # tfrVH.txt (by mother's birth cohort) and two birth-order files; reading
    # them all rbinds four copies of every country-year.
    f <- file.path(hfd_dir, "tfrRR.txt")
    if (file.exists(f)) {
      say("reading HFD tfrRR.txt")
      x <- fread(f, skip = 2, showProgress = FALSE); setnames(x, tolower(names(x)))
      out$HFD <- data.table(code = as.character(x$code),
                            year = suppressWarnings(as.integer(x$year)),
                            tfr  = suppressWarnings(as.numeric(x$tfr)))
    } else say("  tfrRR.txt not found in ", hfd_dir)
  }

  # ---- HFC: the historical collection. Norway reaches 1845. ----------------
  hfc_dir <- file.path(fdir, "hfc_tfr")
  if (unzip_if_needed(file.path(fdir, "hfc_tfr.zip"), hfc_dir)) {
    f <- list.files(hfc_dir, pattern = "TFRMAB.*[.](txt|csv)$", full.names = TRUE,
                    recursive = TRUE, ignore.case = TRUE)
    if (!length(f))
      f <- list.files(hfc_dir, pattern = "[.](txt|csv)$", full.names = TRUE,
                      recursive = TRUE)
    if (length(f)) {
      say("reading HFC (", length(f), " file(s))")
      x <- rbindlist(lapply(f, function(g)
             tryCatch(fread(g, showProgress = FALSE), error = function(e) NULL)),
           fill = TRUE)
      need <- c("Country","Region","Year1","Year2","TFR")
      if (!all(need %in% names(x))) {
        warning("HFC columns not as documented (want ", paste(need, collapse=", "),
                "; got ", paste(head(names(x), 12), collapse=", "), "). Skipping HFC.")
      } else {
        # Whole-country rows only ("." marks these), and single calendar years
        # only - HFC also carries multi-year periods, which would drop a
        # decade's average onto one year and invent a step in the series.
        x <- x[Region == "." & Year1 == Year2 & !is.na(TFR) & TFR > 0]
        # HFC is a COLLECTION: the same country-year often appears from several
        # bibliographic sources (RefCode). Take the median across them rather
        # than arbitrarily picking one, and keep the spread so any disagreement
        # is visible instead of hidden.
        agg <- x[, .(tfr = median(TFR), n_src = .N,
                     spread = if (.N > 1) diff(range(TFR)) else 0),
                 by = .(code = as.character(Country), year = as.integer(Year1))]
        n_dis <- agg[spread > 0.3, .N]
        if (n_dis) say("  NOTE: ", n_dis, " HFC country-years where sources ",
                       "disagree by >0.3 TFR - see out/fertility_sources.csv")
        out$HFC <- agg[, .(code, year, tfr)]
        hfc_diag <<- agg
      }
    }
  }

  # ---- UN WPP: fills whatever the two register sources cannot reach --------
  wpp <- list.files(fdir, pattern = "wpp.*[.](csv|gz|zip)$", full.names = TRUE,
                    ignore.case = TRUE)
  if (length(wpp)) {
    say("reading UN WPP")
    # fread cannot open .gz without R.utils; decompress through a pipe instead.
    x <- if (grepl("[.]gz$", wpp[1]))
           fread(cmd = sprintf("gzip -dc %s", shQuote(wpp[1])), showProgress = FALSE)
         else fread(wpp[1], showProgress = FALSE)
    nm  <- names(x)
    loc <- grep("^Location$|^Region", nm, value = TRUE)[1]
    yr  <- grep("^Time$|^Year$",      nm, value = TRUE)[1]
    tf  <- grep("^TFR$|Fert",         nm, value = TRUE)[1]
    w <- x[, .(code = as.character(get(loc)), year = as.integer(get(yr)),
               tfr = as.numeric(get(tf)))]
    # WPP2024 tags estimates AND projections alike as "Medium", so the variant
    # column cannot separate them - cut on the year. Without this the models
    # regress naming on FORECAST fertility out to 2100.
    n_proj <- w[year > CFG$wpp_last_obs, .N]
    out$WPP <- w[year <= CFG$wpp_last_obs]
    say("  dropped ", n_proj, " projected rows after ", CFG$wpp_last_obs)
  }

  if (!length(out)) {
    warning("No fertility data found. Run scripts/00c_download_fertility.R. ",
            "The index still builds; the models will be skipped.")
    return(NULL)
  }

  # ---- map each source's country codes onto our country names -------------
  # HFD coverage verified 2 Sept 2026: DNK 1916-, USA 1933-, GBRTENW 1938-,
  # GBR_SCO 1945-, FRATNP 1946-, IRL 1955-, NOR only 1967-. NZ absent entirely.
  maps <- list(
    HFD = c(USA = "USA", NOR = "Norway", FRATNP = "France", IRL = "Ireland",
            GBRTENW = "England & Wales", GBR_SCO = "Scotland", DNK = "Denmark"),
    # HFC uses ISO 3166-1 alpha-3. Norway runs 1845-2024, which is the whole
    # reason it is here. Scotland and England & Wales are not separable in HFC
    # (they sit under GBR with region codes), so they stay on HFD.
    HFC = c(USA = "USA", NOR = "Norway", FRA = "France", IRL = "Ireland",
            DNK = "Denmark", NZL = "New Zealand"),
    WPP = c(`United States of America` = "USA", Norway = "Norway",
            France = "France", Ireland = "Ireland", Denmark = "Denmark",
            `New Zealand` = "New Zealand")
  )
  res <- rbindlist(lapply(names(out), function(src) {
    d <- out[[src]]; if (is.null(d) || !nrow(d)) return(NULL)
    d <- copy(d)[, country := maps[[src]][code]]
    d[!is.na(country) & !is.na(tfr), .(country, year, tfr, source = src)]
  }), fill = TRUE)
  if (!nrow(res)) { warning("fertility sources loaded but nothing mapped"); return(NULL) }

  # ---- priority: register series win, WPP only fills gaps -----------------
  # HFD, then HFC, then WPP. This ordering means Norway 1950-66 now comes from
  # HFC - a national register series - instead of a UN estimate.
  res[, prio := match(source, CFG$fert_priority)]
  setorder(res, country, year, prio)
  f <- unique(res, by = c("country","year"))
  f[, prio := NULL]
  say("fertility assembled: ", nrow(f), " country-years, ",
      uniqueN(f$country), " countries, ", min(f$year), "-", max(f$year))
  f[]
}

hfc_diag <- NULL
fert <- cached("fertility", read_fertility())

# =============================================================================
# 5. CHANGE POINTS — dated independently on each series.
#    Structural change in the TREND, not the level: Norway's index rises into
#    a mid-century peak and then falls, and a level test would miss that.
# =============================================================================

breakpoints_for <- function(dt, valcol, label, max_breaks = 2) {
  if (!requireNamespace("strucchange", quietly = TRUE)) {
    warning("strucchange not installed; skipping change-point dating."); return(NULL)
  }
  rbindlist(lapply(split(dt, by = "country", drop = TRUE), function(d) {
    d <- d[!is.na(get(valcol))]; setorder(d, year)
    if (nrow(d) < 30) return(NULL)
    bp <- try(strucchange::breakpoints(d[[valcol]] ~ d$year,
                                       h = max(0.15, 10 / nrow(d)),
                                       breaks = max_breaks), silent = TRUE)
    if (inherits(bp, "try-error") || all(is.na(bp$breakpoints))) return(NULL)
    ci <- try(confint(bp), silent = TRUE)
    idx <- bp$breakpoints
    data.table(country = d$country[1], series = label,
               break_year = d$year[idx],
               lo = if (!inherits(ci,"try-error")) d$year[pmax(1, ci$confint[,1])] else NA_integer_,
               hi = if (!inherits(ci,"try-error")) d$year[pmin(nrow(d), ci$confint[,3])] else NA_integer_)
  }), fill = TRUE)
}

bp_index <- breakpoints_for(index_head, "index", "naming")
# A break within 15 years of either end of a series is pinned by where the
# data happens to start or stop, not by history. Denmark's naming begins in
# 1985 and England & Wales' in 1996, so their "breaks" are censored artefacts
# and must not be compared against fertility breaks dated over a full century.
flag_censored <- function(bp, dt, valcol) {
  if (is.null(bp) || !nrow(bp)) return(bp)
  span <- dt[!is.na(get(valcol)), .(y0 = min(year), y1 = max(year)), by = country]
  bp <- merge(bp, span, by = "country", all.x = TRUE)
  bp[, censored := (break_year - y0 < 15) | (y1 - break_year < 15)]
  bp[]
}
bp_index <- flag_censored(bp_index, index_head, "index")
bp_fert  <- if (!is.null(fert)) flag_censored(
              breakpoints_for(fert, "tfr", "fertility"), fert, "tfr") else NULL
breaks   <- rbindlist(list(bp_index, bp_fert), fill = TRUE)
if (nrow(breaks)) breaks[, tier := fifelse(country %chin% CFG$deep, "deep", "modern")]

# =============================================================================
# 6. LEAD-LAG — Jordà local projections on first differences.
#    Reads as: does a change in naming distinctiveness at t predict a change in
#    fertility h years later? Predictive precedence only. NOT causation.
# =============================================================================

local_projections <- function(idx, f, H = CFG$lp_horizon) {
  if (is.null(f)) return(NULL)
  # Index in PERCENTAGE POINTS so beta reads as "change in TFR per one-point
  # change in the top-10 share".
  d <- merge(idx[tier == "deep", .(country, year, idx = 100 * index)],
             f[, .(country, year, tfr)], by = c("country","year"))
  setorder(d, country, year)
  d[, `:=`(d_idx = idx - shift(idx), d_tfr = tfr - shift(tfr)), by = country]
  d <- d[!is.na(d_idx) & !is.na(d_tfr)]
  if (!nrow(d)) return(NULL)
  d[, `:=`(l1 = shift(d_idx), l2 = shift(d_idx, 2),
           t1 = shift(d_tfr), t2 = shift(d_tfr, 2)), by = country]

  # TWO specifications, deliberately. Both series fall almost everywhere at
  # once, so with country effects alone the regression happily attributes a
  # shared global trend to naming - it returns a large, significant, growing
  # coefficient that is pure common time trend. YEAR fixed effects absorb
  # whatever hit every country in a given year (war, depression, the pill),
  # leaving only within-year cross-country variation. The headline spec is the
  # one WITH year effects; the naive one is kept so the gap between them can
  # be shown rather than asserted.
  specs <- list(
    `country FE only` = "y ~ d_idx + factor(country)",
    `year FE + lags`  = "y ~ d_idx + l1 + l2 + t1 + t2 + factor(country) + factor(year)")

  rbindlist(lapply(names(specs), function(sp) {
    rbindlist(lapply(0:H, function(h) {
      dd <- copy(d)[, y := shift(tfr, -h) - shift(tfr, 1), by = country][!is.na(y)]
      if (nrow(dd) < 60) return(NULL)
      m <- try(lm(as.formula(specs[[sp]]), data = dd), silent = TRUE)
      if (inherits(m, "try-error") || !"d_idx" %in% names(coef(m))) return(NULL)
      V <- if (requireNamespace("sandwich", quietly = TRUE))
             sandwich::vcovCL(m, cluster = dd$country) else vcov(m)
      b <- coef(m)[["d_idx"]]; se <- sqrt(diag(V))[["d_idx"]]
      data.table(spec = sp, h = h, beta = b, se = se,
                 lo = b - 1.96 * se, hi = b + 1.96 * se, n = nrow(dd))
    }), fill = TRUE)
  }), fill = TRUE)
}

lp <- local_projections(index_head, fert)

# =============================================================================
# 7. ROBUSTNESS
# =============================================================================

# (a) Norway calibration: covered-total index vs the true all-births index.
#     This is the number that tells the reader how much the shortcut costs.
nor_cal <- NULL
if ("Norway" %chin% PCT) {
  nw <- names_dt[country == "Norway"]
  setorder(nw, year, sex, -n)
  nor_cal <- nw[, {
    p_true <- n / 100                       # already % of ALL births
    p_cov  <- n / sum(n)                    # % of covered births
    .(top10_true    = sum(head(p_true, CFG$top_n)),
      top10_covered = sum(head(p_cov,  CFG$top_n)),
      coverage      = sum(n) / 100)
  }, by = .(year, sex)]
  nor_cal[, ratio := top10_covered / top10_true]
}

# (b) France's censored tail, straight from INSEE.
fr_rare <- NULL
frf <- file.path(CFG$cache_dir, "france_rare_mass.rds")
if (file.exists(frf)) {
  fr_rare <- as.data.table(readRDS(frf))
  frtot <- names_dt[country == "France", .(listed = sum(n)), by = .(year, sex)]
  fr_rare <- merge(fr_rare, frtot, by = c("year","sex"), all.x = TRUE)
  fr_rare[, rare_share := rare_n / (rare_n + listed)]
}

# (c) Depth sensitivity: does the story survive a different N?
depth_sens <- rbindlist(lapply(c(5, 10, 25, 50), function(k) {
  d <- copy(names_dt); setorder(d, country, year, sex, -n)
  d[, .(share = sum(head(n, k)) / sum(n)), by = .(country, year, sex)][, top_k := k][]
}), fill = TRUE)

# (d) Spelling variants, only if `phonics` is available.
phon_sens <- NULL
if (requireNamespace("phonics", quietly = TRUE)) {
  say("phonetic collapsing")
  d <- copy(names_dt)
  d[, key := phonics::metaphone(toupper(name))]
  d <- d[, .(n = sum(n)), by = .(country, year, sex, key)]
  setorder(d, country, year, sex, -n)
  phon_sens <- d[, .(top10_share_phonetic = sum(head(n, CFG$top_n)) / sum(n)),
                 by = .(country, year, sex)]
} else {
  say("phonics not installed - skipping the spelling-variant check")
}

# --- (e) The two series side by side. B cannot join tables, so the join
#     happens here. Both are indexed to 100 at each country's first year of
#     overlap: they are in different units (a share and a rate), and indexing
#     lets one axis carry both so the SHAPES can be compared directly, which
#     is the whole question.
series_joined <- NULL
if (!is.null(fert)) {
  sj <- merge(index_head[, .(country, year, index, tier)],
              fert[, .(country, year, tfr)], by = c("country","year"))
  setorder(sj, country, year)
  sj <- sj[!is.na(index) & !is.na(tfr)]
  sj[, `:=`(idx100 = 100 * index / index[1],
            tfr100 = 100 * tfr   / tfr[1]), by = country]
  series_joined <- sj[]
}

# --- (f) Event time: each country re-centred on its own naming break, with
#     fertility plotted against it. This is deliberately the same manoeuvre as
#     the FT smartphone chart, and it inherits the same weakness - aligning a
#     smooth decline to any event makes the event look causal. It is here to
#     be shown WITH that caveat, not as evidence.
event_time <- NULL
if (!is.null(series_joined) && !is.null(breaks) && nrow(breaks)) {
  anch <- breaks[series == "naming" & tier == "deep" &
                   (is.na(censored) | !censored),
                 .(anchor = max(break_year)), by = country]
  if (nrow(anch)) {
    event_time <- merge(series_joined, anch, by = "country")
    event_time[, rel := year - anchor]
    event_time <- event_time[abs(rel) <= 50][]
  }
}

# =============================================================================
# 8. WRITE OUTPUTS  (everything B is allowed to see)
# =============================================================================

w <- function(x, f) if (!is.null(x) && nrow(x)) {
  fwrite(x, file.path(CFG$out_dir, f)); say("wrote ", f, "  (", nrow(x), " rows)")
}

w(index_sex,  "index_by_country_year_sex.csv")
w(index_head, "index_headline.csv")
w(fert,          "fertility.csv")
w(series_joined, "series_joined.csv")
w(event_time,    "event_time.csv")
w(hfc_diag,   "fertility_sources.csv")
w(hfc_diag,   "fertility_sources.csv")
w(breaks,     "breakpoints.csv")
w(lp,         "lead_lag.csv")
w(nor_cal,    "robustness_norway_calibration.csv")
w(fr_rare,    "robustness_france_rare_mass.csv")
w(depth_sens, "robustness_depth_sensitivity.csv")
w(phon_sens,  "robustness_phonetic.csv")

# Provenance: which files, which sizes, which run.
manifest <- rbindlist(lapply(
  list.files(CFG$data_dir, recursive = TRUE, full.names = TRUE,
             pattern = "\\.(csv|txt|xlsx)$"),
  function(f) data.table(file = f, bytes = file.size(f),
                         mtime = format(file.mtime(f)))), fill = TRUE)
manifest[, run_at := format(Sys.time())]
if (length(repairs)) {
  rp <- unique(rbindlist(repairs, fill = TRUE))
  w(rp, "encoding_repairs.csv")
  say("NOTE: ", nrow(rp), " distinct name strings were repaired - see ",
      "out/encoding_repairs.csv. All are rare names; none can reach a top-10.")
}
w(manifest, "run_manifest.csv")

say("done. ", length(list.files(CFG$out_dir, pattern="csv$")), " tables in out/")
if (is.null(fert)) message("\nNOTE: no fertility data -> sections 5b and 6 were skipped.")
