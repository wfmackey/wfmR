#' Save a plot with wfmR defaults
#'
#' Defaults to transparent backgrounds and uses `ragg` for bitmap output when
#' it is available.
#'
#' @param filename Output filename.
#' @param plot Plot to save. Defaults to [ggplot2::last_plot()].
#' @param width,height Plot dimensions.
#' @param units Units for `width` and `height`.
#' @param dpi Resolution for bitmap output.
#' @param bg Plot background passed to [ggplot2::ggsave()].
#' @param device Optional graphics device.
#' @param ... Additional arguments passed to [ggplot2::ggsave()].
#'
#' @return The filename, invisibly.
#' @export
wfm_save_plot <- function(filename,
                          plot = ggplot2::last_plot(),
                          width = 7,
                          height = 4.5,
                          units = "in",
                          dpi = 320,
                          bg = "transparent",
                          device = NULL,
                          ...) {
  if (is.null(device) && requireNamespace("ragg", quietly = TRUE)) {
    ext <- tolower(tools::file_ext(filename))
    device <- switch(
      ext,
      png = ragg::agg_png,
      jpg = ragg::agg_jpeg,
      jpeg = ragg::agg_jpeg,
      tiff = ragg::agg_tiff,
      NULL
    )
  }

  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    width = width,
    height = height,
    units = units,
    dpi = dpi,
    bg = bg,
    device = device,
    ...
  )

  invisible(filename)
}
