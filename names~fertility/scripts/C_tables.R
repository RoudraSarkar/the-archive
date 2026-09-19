# =============================================================================
# C_tables.R  —  TABLES AS IMAGES.
#
# Substack has no table block and does not read Markdown, so a pasted pipe
# table arrives as a row of pipes and dashes. The reliable route is to render
# each table as a PNG in house style and insert it exactly like a figure.
#
# Same contract as B_figures.R: reads out/*.csv and nothing else. Numbers are
# never typed in here — the model table is read from out/lead_lag.csv so the
# article can never drift from the run. The sources table IS typed in, because
# it is documentation of provenance rather than an output of the pipeline.
#
# Exports narrower than the charts (7.2in, not 9in). Substack scales every
# image to one column width, so a narrower export means larger apparent text
# on a phone. Do not widen this without checking it on a phone.
#
# Run:  source("scripts/C_tables.R")   (working dir = project root)
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})
source("theme_archives.R")

OUT <- "out"; FIG <- "figures"
dir.create(FIG, showWarnings = FALSE, recursive = TRUE)

TAB_W <- 7.2   # inches
ROW_H <- 0.30  # inches per body row, before title/caption furniture

# ---------------------------------------------------------------------------
# archives_table() — a table drawn as a plot, in house style.
#
# No vertical rules, no zebra striping, no box. One rule under the header and
# one under the last row, both taupe. First column charcoal, the rest brown.
#
#   df       data.frame/data.table of character columns, already formatted
#   headers  column labels, same length as ncol(df)
#   widths   relative column widths, recycled to sum 1 (default: equal)
#   aligns   per column, "l" or "r" (default: first "l", rest "r")
# ---------------------------------------------------------------------------
archives_table <- function(df, headers = names(df),
                           title = NULL, subtitle = NULL, caption = NULL,
                           widths = NULL, aligns = NULL,
                           base_size = 12) {

  df <- as.data.frame(lapply(as.data.frame(df), as.character),
                      stringsAsFactors = FALSE)
  nc <- ncol(df); nr <- nrow(df)
  stopifnot(length(headers) == nc)

  if (is.null(widths)) widths <- rep(1 / nc, nc)
  widths <- widths / sum(widths)
  if (is.null(aligns)) aligns <- c("l", rep("r", nc - 1))
  stopifnot(length(widths) == nc, length(aligns) == nc)

  # column edges in 0..1, then the anchor each column's text hangs off
  right <- cumsum(widths); left <- right - widths
  PAD   <- 0.012
  x_at  <- ifelse(aligns == "l", left + PAD, right - PAD)
  hj    <- ifelse(aligns == "l", 0, 1)

  p <- archives_pal

  cells <- do.call(rbind, lapply(seq_len(nc), function(j) {
    data.frame(x = x_at[j], y = -seq_len(nr), hj = hj[j],
               lab = df[[j]],
               col = if (j == 1) p$charcoal else p$brown,
               stringsAsFactors = FALSE)
  }))
  head_df <- data.frame(x = x_at, y = 0, hj = hj, lab = headers,
                        stringsAsFactors = FALSE)

  ggplot() +
    # header
    geom_text(data = head_df,
              aes(x, y, label = lab, hjust = hj),
              family = "Georgia", fontface = "bold", colour = p$charcoal,
              size = base_size * 0.30) +
    # body
    geom_text(data = cells,
              aes(x, y, label = lab, hjust = hj), colour = cells$col,
              family = "Georgia", size = base_size * 0.29) +
    # rules: under the header, and under the last row
    annotate("segment", x = 0, xend = 1, y = -0.5, yend = -0.5,
             colour = p$taupe, linewidth = 0.45) +
    annotate("segment", x = 0, xend = 1, y = -nr - 0.5, yend = -nr - 0.5,
             colour = p$taupe, linewidth = 0.45) +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-nr - 0.75, 0.62), expand = c(0, 0)) +
    labs(title = title, subtitle = subtitle, caption = caption,
         x = NULL, y = NULL) +
    theme_archives(base_size = base_size, grid = "none") +
    # theme_archives sets axis.title.x/.y explicitly, and a child element beats
    # a blanked parent — so both children have to be blanked by name or the
    # default "x" and "y" from the aes mapping print on the plot.
    theme(axis.title   = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text    = element_blank(),
          axis.text.x  = element_blank(),
          axis.text.y  = element_blank(),
          plot.margin  = margin(t = 16, r = 10, b = 10, l = 10))
}

# height that fits the rows plus the title/caption furniture
tab_height <- function(nr, has_sub = TRUE) {
  0.95 + (if (has_sub) 0.28 else 0) + nr * ROW_H
}

# ---------------------------------------------------------------------------
# Table 1 — the sources table. Provenance, so it is written out by hand.
# ---------------------------------------------------------------------------
sources <- data.table(
  country = c("United States", "Norway", "France", "New Zealand",
              "Ireland", "Scotland", "Denmark", "England & Wales"),
  from    = c("1880", "1880", "1900", "1900",
              "1964", "1974", "1985", "1996"),
  source  = c("Social Security Administration", "Statistics Norway",
              "INSEE", "Dept of Internal Affairs",
              "Central Statistics Office", "National Records of Scotland",
              "Danmarks Statistik", "Office for National Statistics"))

t1 <- archives_table(
  sources,
  headers  = c("Country", "Names from", "Register"),
  widths   = c(0.30, 0.18, 0.52),
  aligns   = c("l", "l", "l"),
  title    = "Eight registers, 145 years",
  subtitle = "First year of usable annual name counts, by country",
  caption  = "Table: the archives")

save_archives(t1, file.path(FIG, "tab01_sources.png"),
              width = TAB_W, height = tab_height(nrow(sources)))

# ---------------------------------------------------------------------------
# Table 2 — the lead-lag result, read straight from the run.
# ---------------------------------------------------------------------------
HORIZONS <- c(10, 15, 20)

ll_file <- file.path(OUT, "lead_lag.csv")
if (!file.exists(ll_file)) {
  message("  SKIP tab02 - out/lead_lag.csv not found; run A_analysis.R first")
} else {
  ll <- fread(ll_file, showProgress = FALSE)

  # Typographic minus (U+2212), not a hyphen. Explicit + on the point estimate
  # only — signing the interval bounds as well just adds noise.
  minus <- function(x) gsub("-", "\u2212", x, fixed = TRUE)
  fmt <- function(b, lo, hi)
    minus(sprintf("%+.3f  [%.3f, %.3f]", b, lo, hi))

  # NB: argument names must not collide with column names, or the column
  # wins the comparison and the filter silently matches everything.
  cell <- function(sp, hh) {
    r <- ll[spec == sp & h == hh]
    if (!nrow(r)) return(NA_character_)
    fmt(r$beta[1], r$lo[1], r$hi[1])
  }

  specs <- unique(ll$spec)
  stopifnot(length(specs) == 2)

  tab2 <- data.table(
    horizon = paste(HORIZONS, "years"),
    a = vapply(HORIZONS, function(hh) cell(specs[1], hh), character(1)),
    b = vapply(HORIZONS, function(hh) cell(specs[2], hh), character(1)))

  t2 <- archives_table(
    tab2,
    headers  = c("Horizon", "Country effects only", "With year effects"),
    widths   = c(0.20, 0.40, 0.40),
    aligns   = c("l", "r", "r"),
    title    = "The result depends entirely on the controls",
    # Keep each subtitle line under ~70 visible characters. At 7.2in there is
    # no room for more, and ggtext will not wrap it for you.
    subtitle = paste0("Change in fertility on naming concentration, ",
                      "<b>h</b> years earlier.<br>",
                      "Coefficient and 95% interval; SEs clustered by country."),
    caption  = paste("Data: HFD, HFC and UN WPP 2024",
                     "national name registers",
                     "Table: the archives", sep = " \u00b7 "))

  save_archives(t2, file.path(FIG, "tab02_lead_lag.png"),
                width = TAB_W, height = tab_height(nrow(tab2)) + 0.25)
}

message("done - 2 tables in ", FIG, "/")
