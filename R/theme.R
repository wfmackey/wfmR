#' wfmR ggplot2 theme
#'
#' A transparent-background theme based on the course-note chart convention:
#' minimal grid, muted axes, strong titles, and clean figure notes/captions.
#'
#' @param base_size Base font size.
#' @param base_family Base font family. Defaults to `"Source Serif Pro"`,
#'   the text font used by the bundled Quarto PDF styles.
#' @param grid If `FALSE`, remove major grid lines.
#'
#' @details
#' Install Source Serif Pro as a system font to match the bundled Quarto PDF
#' documents. Graphics devices can silently substitute another font when it
#' is unavailable. Set `base_family` to use a different installed font.
#'
#' @return A ggplot2 theme.
#' @export
theme_wfm <- function(base_size = 11, base_family = "Source Serif Pro", grid = TRUE) {
  chart_text <- "#2F2F33"
  muted_text <- "#5E5966"
  major_grid <- if (isTRUE(grid)) {
    ggplot2::element_line(colour = "#E6DAD1", linewidth = 0.35)
  } else {
    ggplot2::element_blank()
  }

  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        colour = chart_text,
        margin = ggplot2::margin(b = 4)
      ),
      plot.subtitle = ggplot2::element_text(
        colour = chart_text,
        margin = ggplot2::margin(b = 8)
      ),
      plot.caption = ggplot2::element_text(
        colour = chart_text,
        size = ggplot2::rel(0.82),
        hjust = 0,
        margin = ggplot2::margin(t = 8)
      ),
      axis.title = ggplot2::element_text(colour = chart_text),
      axis.title.y = ggplot2::element_text(
        angle = 90,
        margin = ggplot2::margin(r = 6)
      ),
      axis.title.y.right = ggplot2::element_text(
        angle = -90,
        margin = ggplot2::margin(l = 6)
      ),
      axis.text = ggplot2::element_text(colour = chart_text),
      axis.text.y = ggplot2::element_text(
        hjust = 1,
        margin = ggplot2::margin(r = 0)
      ),
      axis.text.y.right = ggplot2::element_text(
        hjust = 0,
        margin = ggplot2::margin(l = 0)
      ),
      axis.text.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 3)
      ),
      panel.grid.major = major_grid,
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = NA, colour = NA),
      panel.background = ggplot2::element_rect(fill = NA, colour = NA),
      legend.background = ggplot2::element_rect(fill = NA, colour = NA),
      legend.box.background = ggplot2::element_rect(fill = NA, colour = NA),
      strip.background = ggplot2::element_rect(fill = NA, colour = NA),
      strip.text = ggplot2::element_text(colour = chart_text),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(colour = muted_text),
      legend.position = "bottom",
      plot.title.position = "plot"
    )
}

#' wfmR y-axis scale
#'
#' A small wrapper around [ggplot2::scale_y_continuous()] with tighter default
#' expansion for the wfmR chart style.
#'
#' @param ... Arguments passed to [ggplot2::scale_y_continuous()].
#' @param expand Expansion applied to the y-axis. Defaults to a smaller
#'   expansion than ggplot2's default.
#'
#' @return A ggplot2 position scale.
#' @export
scale_y_wfm <- function(...,
                        expand = ggplot2::expansion(mult = c(0.01, 0.035))) {
  ggplot2::scale_y_continuous(..., expand = expand)
}

#' Set wfmR as the active ggplot2 theme
#'
#' @inheritParams theme_wfm
#'
#' @return The previous ggplot2 theme, invisibly.
#' @export
wfm_set_theme <- function(base_size = 11, base_family = "Source Serif Pro", grid = TRUE) {
  old <- ggplot2::theme_set(theme_wfm(base_size, base_family, grid))
  invisible(old)
}

wfm_palette_function <- function(reverse = FALSE,
                                 palette = c(
	                                   "primary",
	                                   "wong",
	                                   "colourblind",
	                                   "colorblind",
	                                   "publication",
	                                   "rosepine",
	                                   "base",
                                   "sunset",
                                   "sunset_ruby",
                                   "semantic"
                                 )) {
  palette <- match.arg(palette)
  force(reverse)
  force(palette)

  function(n) {
    pal <- if (identical(palette, "semantic")) {
      wfm_colours(semantic = TRUE)
    } else {
      wfm_palette(reverse = reverse, palette = palette)
    }

    if (n > length(pal)) {
      warning(
        "Requested ",
        n,
        " colours, but the selected wfmR palette has ",
        length(pal),
        "; colours will be recycled.",
        call. = FALSE
      )
    }

    unname(rep_len(pal, n))
  }
}

#' wfmR ggplot2 scales
#'
#' @param ... Arguments passed to [ggplot2::discrete_scale()].
#' @param reverse If `TRUE`, reverse the palette order.
#' @param palette Discrete palette name. Use [wfm_palette_options()] to list
#'   available palettes. `"semantic"` returns object/parameter/welfare/method
#'   colours.
#' @param aesthetics ggplot2 aesthetics to scale.
#'
#' @return A ggplot2 scale.
#' @export
scale_colour_wfm <- function(...,
                             reverse = FALSE,
                             palette = c(
	                               "primary",
	                               "wong",
	                               "colourblind",
	                               "colorblind",
	                               "publication",
	                               "rosepine",
	                               "base",
                               "sunset",
                               "sunset_ruby",
                               "semantic"
                             ),
                             aesthetics = "colour") {
  palette <- match.arg(palette)
  ggplot2::discrete_scale(
    aesthetics = aesthetics,
    scale_name = "wfm",
    palette = wfm_palette_function(reverse, palette),
    ...
  )
}

#' @rdname scale_colour_wfm
#' @export
scale_color_wfm <- scale_colour_wfm

#' @rdname scale_colour_wfm
#' @export
scale_fill_wfm <- function(...,
                           reverse = FALSE,
                           palette = c(
	                             "primary",
	                             "wong",
	                             "colourblind",
	                             "colorblind",
	                             "publication",
	                             "rosepine",
	                             "base",
                             "sunset",
                             "sunset_ruby",
                             "semantic"
                           ),
                           aesthetics = "fill") {
  palette <- match.arg(palette)
  ggplot2::discrete_scale(
    aesthetics = aesthetics,
    scale_name = "wfm",
    palette = wfm_palette_function(reverse, palette),
    ...
  )
}

#' wfmR sequential ggplot2 scales
#'
#' @param ... Arguments passed to [ggplot2::scale_colour_gradientn()] or
#'   [ggplot2::scale_fill_gradientn()].
#' @param palette Sequential palette name. Use [wfm_palette_options()] to list
#'   available palettes.
#' @param n Number of colours sampled from the palette.
#' @param reverse If `TRUE`, reverse the palette.
#' @param aesthetics ggplot2 aesthetics to scale.
#'
#' @return A ggplot2 scale.
#' @export
scale_colour_wfm_seq <- function(...,
                                 palette = "blue_teal_yellow",
                                 n = 256,
                                 reverse = FALSE,
                                 aesthetics = "colour") {
  ggplot2::scale_colour_gradientn(
    colours = wfm_sequential_palette(name = palette, n = n, reverse = reverse),
    aesthetics = aesthetics,
    ...
  )
}

#' @rdname scale_colour_wfm_seq
#' @export
scale_color_wfm_seq <- scale_colour_wfm_seq

#' @rdname scale_colour_wfm_seq
#' @export
scale_fill_wfm_seq <- function(...,
                               palette = "blue_teal_yellow",
                               n = 256,
                               reverse = FALSE,
                               aesthetics = "fill") {
  ggplot2::scale_fill_gradientn(
    colours = wfm_sequential_palette(name = palette, n = n, reverse = reverse),
    aesthetics = aesthetics,
    ...
  )
}

#' wfmR diverging ggplot2 scales
#'
#' @param ... Arguments passed to [ggplot2::scale_colour_gradientn()] or
#'   [ggplot2::scale_fill_gradientn()].
#' @param palette Diverging palette name. Use [wfm_palette_options()] to list
#'   available palettes.
#' @param midpoint Data value mapped to the neutral midpoint.
#' @inheritParams scale_colour_wfm_seq
#'
#' @return A ggplot2 scale.
#' @export
scale_colour_wfm_div <- function(...,
                                 palette = "teal_salmon",
                                 midpoint = 0,
                                 n = 257,
                                 reverse = FALSE,
                                 aesthetics = "colour") {
  ggplot2::scale_colour_gradientn(
    colours = wfm_diverging_palette(name = palette, n = n, reverse = reverse),
    values = seq(0, 1, length.out = n),
    rescaler = wfm_midpoint_rescaler(midpoint),
    aesthetics = aesthetics,
    ...
  )
}

#' @rdname scale_colour_wfm_div
#' @export
scale_color_wfm_div <- scale_colour_wfm_div

#' @rdname scale_colour_wfm_div
#' @export
scale_fill_wfm_div <- function(...,
                               palette = "teal_salmon",
                               midpoint = 0,
                               n = 257,
                               reverse = FALSE,
                               aesthetics = "fill") {
  ggplot2::scale_fill_gradientn(
    colours = wfm_diverging_palette(name = palette, n = n, reverse = reverse),
    values = seq(0, 1, length.out = n),
    rescaler = wfm_midpoint_rescaler(midpoint),
    aesthetics = aesthetics,
    ...
  )
}

wfm_midpoint_rescaler <- function(midpoint = 0) {
  force(midpoint)
  function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
    scales::rescale_mid(x, to = to, from = from, mid = midpoint)
  }
}
