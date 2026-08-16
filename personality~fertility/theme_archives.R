# ---------------------------------------------------------------------------
# theme_archives() — house style for *the archives*
#
# Built to the specification in dimorphism/cowork_handoff_primate_dimorphism.md.
# Georgia serif, cream ground, terracotta reserved for the loaded category.
# ---------------------------------------------------------------------------

library(ggplot2)
library(ggtext)
library(grid)

## ---- palette --------------------------------------------------------------

archives_pal <- list(
  cream     = "#F1EBDF",  # panel and plot background
  charcoal  = "#2A231C",  # primary text
  brown     = "#6B5D4F",  # axis text, subtitle
  grey      = "#8A7C6D",  # captions
  taupe     = "#C9BFA9",  # gridlines
  terracotta= "#8C3A2F"   # ACCENT — the loaded category only. Never in rotation.
)

# Categorical rotation. Terracotta is deliberately excluded.
archives_categorical <- c(
  slate     = "#3A4A5C",
  sage      = "#7A8B6F",
  sand      = "#C9A875",
  warm_grey = "#A89C8A"
)

#' Discrete colour/fill scales using the house rotation
scale_colour_archives <- function(...) {
  ggplot2::discrete_scale("colour", palette = function(n) {
    if (n > length(archives_categorical)) {
      warning("More categories than the house rotation has colours (",
              length(archives_categorical), "). Collapse categories rather ",
              "than extending the palette.", call. = FALSE)
    }
    unname(archives_categorical)[seq_len(n)]
  }, ...)
}
scale_color_archives <- scale_colour_archives
scale_fill_archives <- function(...) {
  ggplot2::discrete_scale("fill", palette = function(n) {
    unname(archives_categorical)[seq_len(n)]
  }, ...)
}

## ---- the theme ------------------------------------------------------------

#' @param base_size    base point size
#' @param base_family  font family; Georgia per house style
#' @param grid         one of "both", "x", "y", "none". The bar-chart convention
#'                     is "x" (vertical gridlines only). Scatters usually "both".
theme_archives <- function(base_size = 12,
                           base_family = "Georgia",
                           grid = c("both", "x", "y", "none")) {

  grid <- match.arg(grid)
  p <- archives_pal

  th <- theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      # --- ground ---
      plot.background  = element_rect(fill = p$cream, colour = NA),
      panel.background = element_rect(fill = p$cream, colour = NA),
      panel.border     = element_blank(),

      # --- type ---
      plot.title = element_text(
        family = base_family, face = "plain", colour = p$charcoal,
        size = base_size * 1.55, hjust = 0,
        margin = margin(b = 4)
      ),
      # Subtitle renders HTML: use <b>…</b> for emphasis.
      plot.subtitle = element_markdown(
        family = base_family, colour = p$brown,
        size = base_size * 1.0, hjust = 0, lineheight = 1.35,
        margin = margin(b = 16)
      ),
      plot.caption = element_text(
        family = base_family, colour = p$grey,
        size = base_size * 0.72, hjust = 0,
        margin = margin(t = 14)
      ),
      plot.title.position   = "plot",
      plot.caption.position = "plot",

      axis.title   = element_text(family = base_family, colour = p$brown,
                                  size = base_size * 0.82),
      axis.title.x = element_text(margin = margin(t = 8)),
      axis.title.y = element_text(margin = margin(r = 8), angle = 90),
      axis.text    = element_text(family = base_family, colour = p$brown,
                                  size = base_size * 0.8),

      # --- no ticks ---
      axis.ticks        = element_blank(),
      axis.ticks.length = unit(0, "pt"),

      # --- gridlines ---
      panel.grid.major = element_line(colour = p$taupe, linewidth = 0.3),
      panel.grid.minor = element_blank(),

      # --- legend: top, left-justified, untitled ---
      legend.position      = "top",
      legend.justification = "left",
      legend.title         = element_blank(),
      legend.text          = element_text(family = base_family, colour = p$brown,
                                          size = base_size * 0.82),
      legend.key           = element_blank(),
      legend.background    = element_blank(),
      legend.margin        = margin(b = 6, l = 0),
      legend.box.margin    = margin(l = -6),

      strip.text = element_text(family = base_family, colour = p$charcoal,
                                size = base_size * 0.88, hjust = 0,
                                margin = margin(b = 5)),

      plot.margin = margin(t = 18, r = 22, b = 14, l = 18)
    )

  # gridline convention
  th <- th + switch(grid,
    both = theme(),
    x    = theme(panel.grid.major.y = element_blank()),
    y    = theme(panel.grid.major.x = element_blank()),
    none = theme(panel.grid.major = element_blank())
  )

  th
}

## ---- helpers --------------------------------------------------------------

#' House caption: dot-separated, always ends "Chart: the archives"
#' @examples archives_caption("Statistics Finland, tables 14mp, 12dm, 11rd")
archives_caption <- function(...) {
  bits <- c(paste0("Data: ", paste(c(...), collapse = " · ")),
            "Chart: the archives")
  paste(bits, collapse = " · ")
}

#' Georgia data labels in brown, per house style
geom_label_archives <- function(..., family = "Georgia",
                                colour = archives_pal$brown, size = 3.1) {
  ggplot2::geom_text(..., family = family, colour = colour, size = size)
}

#' Export at house specification: 9in wide, 300 dpi, cream ground, PNG.
#' Uses ragg if available — it handles system fonts (Georgia) far more
#' reliably than the default png device.
save_archives <- function(plot, file, width = 9, height = 5.6, dpi = 300) {
  dev <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else NULL
  ggplot2::ggsave(
    filename = file, plot = plot,
    width = width, height = height, dpi = dpi, units = "in",
    bg = archives_pal$cream,
    device = dev
  )
  message("wrote ", file, "  (", width, " x ", height, " in @ ", dpi, " dpi)")
  invisible(file)
}

#' Check Georgia is actually available. Silent failure to a fallback font is
#' the most common way house style quietly breaks.
check_georgia <- function() {
  ok <- FALSE
  if (requireNamespace("systemfonts", quietly = TRUE)) {
    m <- systemfonts::match_font("Georgia")
    ok <- !is.null(m$path) && grepl("georgia", basename(m$path), ignore.case = TRUE)
  }
  if (!ok) {
    warning("Georgia did not resolve — ggplot will silently substitute a ",
            "fallback and the charts will not match house style. ",
            "Install systemfonts + ragg, or register the font with ",
            "systemfonts::register_font().", call. = FALSE)
  }
  invisible(ok)
}
