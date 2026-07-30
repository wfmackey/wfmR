#' Build a Quarto chart caption
#'
#' Used by [install_chart_chunk_options()] so chunks can declare
#' `chart-title` and `chart-subtitle` directly.
#'
#' @param title Chart title.
#' @param subtitle Optional chart subtitle.
#'
#' @param style Caption style. `"nice"` puts title and subtitle on separate
#'   lines for Grattan-style chart headings. `"academic"` uses a plain formal
#'   caption; in LaTeX/PDF the subtitle is handled with notes/source by the
#'   bundled filter.
#'
#' @return A markdown caption string, or `NULL`.
#' @export
chart_caption <- function(title,
                          subtitle = NULL,
                          style = c("nice", "academic")) {
  style <- match.arg(style)

  if (is.null(title) || !nzchar(title)) {
    return(NULL)
  }

  sentence <- function(x) {
    if (grepl("[.!?]$", x)) x else paste0(x, ".")
  }

  if (identical(style, "academic")) {
    title_line <- sentence(title)

    if (requireNamespace("knitr", quietly = TRUE) && knitr::is_latex_output()) {
      return(title_line)
    }

    if (is.null(subtitle) || !nzchar(subtitle)) {
      return(title_line)
    }

    return(paste0(title_line, " ", subtitle))
  }

  title_line <- paste0("**", title, "**")
  if (is.null(subtitle) || !nzchar(subtitle)) {
    return(title_line)
  }

  if (requireNamespace("knitr", quietly = TRUE) && knitr::is_html_output()) {
    paste0(
      title_line,
      "  \n",
      "<span class=\"chart-subtitle\">",
      subtitle,
      "</span>"
    )
  } else {
    paste0(title_line, "  \n", subtitle)
  }
}

#' Install chart-title chunk options for knitr
#'
#' Allows Quarto chunks to use `chart-title`, `chart-subtitle`,
#' `chart-notes`, and `chart-source`. The title/subtitle are translated to the
#' normal Quarto figure caption. Notes/source are handled by the bundled
#' `chart-notes.lua` filter in the wfmR Quarto extension.
#'
#' @inheritParams chart_caption
#' @param default_caption_location Default value for `fig-cap-location`.
#'
#' @return `NULL`, invisibly.
#' @export
install_chart_chunk_options <- function(style = c("nice", "academic"),
                                        default_caption_location = NULL) {
  style <- match.arg(style)

  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Package `knitr` is required to install chart chunk options.", call. = FALSE)
  }

  if (is.null(default_caption_location)) {
    default_caption_location <- if (identical(style, "academic")) "bottom" else "top"
  }

  knitr::opts_hooks$set(
    `chart-title` = function(options) {
      options$fig.cap <- chart_caption(
        options[["chart-title"]],
        options[["chart-subtitle"]],
        style = style
      )

      if (is.null(options[["fig-cap-location"]]) &&
          !is.null(default_caption_location)) {
        options[["fig-cap-location"]] <- default_caption_location
      }

      options
    }
  )

  invisible(NULL)
}

#' Set wfmR chart defaults for a Quarto document
#'
#' This is the one-line setup helper intended for Quarto setup chunks. It sets
#' the ggplot2 theme, installs chart metadata chunk options, and configures
#' transparent SVG/PDF output depending on the active Quarto format.
#'
#' @param base_size Base chart font size. If `NULL`, uses 12 for LaTeX/PDF and
#'   11 otherwise.
#' @param base_family Base chart font family.
#' @param set_theme If `TRUE`, set [theme_wfm()] as the active ggplot2 theme.
#' @param style Chart caption style. `"nice"` is the default and places chart
#'   headings above figures; `"academic"` uses formal below-figure captions.
#' @param transparent If `TRUE`, request transparent graphics backgrounds.
#' @param default_caption_location Default value for `fig-cap-location`.
#'
#' @return A list of the settings applied, invisibly.
#' @export
wfm_chart_defaults <- function(base_size = NULL,
                               base_family = "",
                               set_theme = TRUE,
                               style = c("nice", "academic"),
                               transparent = TRUE,
                               default_caption_location = NULL) {
  style <- match.arg(style)
  has_knitr <- requireNamespace("knitr", quietly = TRUE)

  if (is.null(base_size)) {
    base_size <- if (has_knitr && knitr::is_latex_output()) 12 else 11
  }

  if (isTRUE(set_theme)) {
    wfm_set_theme(base_size = base_size, base_family = base_family)
  }

  if (has_knitr) {
    device <- if (knitr::is_html_output()) {
      "svg"
    } else if (knitr::is_latex_output()) {
      "pdf"
    } else {
      "png"
    }

    dev_args <- if (isTRUE(transparent)) list(bg = "transparent") else NULL
    knitr::opts_chunk$set(dev = device, dev.args = dev_args)
    install_chart_chunk_options(style = style, default_caption_location = default_caption_location)
  }

  invisible(list(
    base_size = base_size,
    base_family = base_family,
    style = style,
    transparent = transparent,
    knitr = has_knitr
  ))
}
