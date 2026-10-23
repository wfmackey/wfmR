#' Save a plot at wfmR document dimensions
#'
#' Saves PDF, SVG and PNG files with the same dimensions. A file extension in
#' `path` selects a single format instead. Use `width = "full"` or
#' `width = "half"` for the text width of the bundled wfmR Quarto PDF documents,
#' or supply numeric dimensions.
#'
#' @param plot A single ggplot object. Defaults to [ggplot2::last_plot()].
#'   Export components of a patchwork composition separately.
#' @param path Output path including the file stem, for example
#'   `"figures/cars"`. Missing directories are created. For `wfm_save_plot()`,
#'   a suffix such as `.png` selects that format and overrides `formats`.
#'   For `wfm_save_all()`, the final suffix is removed before saving all formats.
#' @param width Plot width: `"full"` (143.96 mm), `"half"` (71.98 mm), or a
#'   positive number in `units`.
#' @param height Plot height in `units`. If `NULL`, uses `width * 4.5 / 7`.
#' @param units Units for numeric dimensions: `"in"`, `"cm"`, `"mm"` or `"px"`.
#'   Preset widths are converted to these units, using `dpi` for pixels.
#' @param dpi Bitmap resolution in dots per inch. Defaults to 320.
#' @param background Background colour: `NULL` (transparent), a hex code in
#'   `#RRGGBB` or `#RRGGBBAA` form, or a wfmR colour name such as `"salmon"`
#'   or `"teal"`. See [wfm_colours()] for names. Exported colour constants such
#'   as `wfm_teal20` also work. This replaces the outer plot background;
#'   panel backgrounds and other theme settings are retained.
#' @param device Optional graphics device override, for a single format only.
#' @param ... Additional arguments passed to [ggplot2::ggsave()].
#' @param formats File formats to save when `path` has no suffix. Defaults to
#'   `c("pdf", "svg", "png")`.
#'
#' @details
#' Both bundled PDF styles use A4 paper with 1.3-inch left and right margins:
#' `210 - 2 * 1.3 * 25.4 = 143.96` mm of text width. Half width is exactly half
#' of that; allow for any gutter in a multi-column layout by supplying a numeric
#' width. The default height preserves the original helper's 7:4.5 aspect ratio.
#' HTML layouts are responsive; these presets specify the exported figure size,
#' not a fixed browser width. Use numeric dimensions if document margins differ.
#'
#' SVG uses `svglite`; PNG uses `ragg` when installed. PDF uses Cairo when
#' available, otherwise the standard R PDF device. Existing output files are
#' overwritten, as with [ggplot2::ggsave()]. A suffix can also select another
#' format supported by `ggsave()`, such as `.jpg` or `.tiff`; some formats cannot
#' retain transparency.
#'
#' Layer data functions and statistical transformations are evaluated once per
#' call, so all formats use the same data and computed positions. The original
#' plot is not changed.
#'
#' @return A named character vector of output paths, invisibly.
#' @seealso [wfm_export_plot_data()], [wfm_save_all()]
#' @export
#'
#' @examples
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'   ggplot2::geom_point() + theme_wfm()
#' wfm_save_plot(p, path = file.path(tempdir(), "cars"))
#' wfm_save_plot(p, path = file.path(tempdir(), "cars-half.png"),
#'               width = "half", height = 55, units = "mm",
#'               background = "salmon")
wfm_save_plot <- function(plot = ggplot2::last_plot(),
                          path,
                          width = "full",
                          height = NULL,
                          units = "in",
                          dpi = 320,
                          background = NULL,
                          device = NULL,
                          ...,
                          formats = c("pdf", "svg", "png")) {
  stem <- wfm_export_stem(path)
  extension <- tolower(tools::file_ext(path))
  if (nzchar(extension)) {
    supported <- c("pdf", "svg", "png", "jpg", "jpeg", "tif", "tiff",
                   "bmp", "eps", "ps", "tex", "emf", "wmf")
    if (!extension %in% supported) {
      stop("Unsupported plot file extension: .", extension, call. = FALSE)
    }
    files <- stats::setNames(path, extension)
  } else {
    if (!is.character(formats) || !length(formats) || anyNA(formats) ||
        any(!formats %in% c("pdf", "svg", "png")) || anyDuplicated(formats)) {
      stop("`formats` must contain unique values from 'pdf', 'svg' and 'png'.",
           call. = FALSE)
    }
    formats <- unname(formats)
    files <- stats::setNames(paste0(stem, ".", formats), formats)
  }
  wfm_save_plot_files(
    plot, files, width, height, units, dpi, background, device, ...
  )
}

#' Save a plot and its chart data
#'
#' Exports PDF, SVG, PNG and CSV files with a common file stem. Any final file
#' suffix in `path` is ignored: `"figures/cars.png"` produces `cars.pdf`,
#' `cars.svg`, `cars.png` and `cars.csv` in `figures/`. The CSV contains the
#' source columns used by the plot; see [wfm_export_plot_data()] for the
#' treatment of layers, facets and statistical transformations.
#'
#' @param plot A single ggplot object. Export components of a patchwork
#'   composition separately.
#' @inheritParams wfm_save_plot
#' @return A named character vector of `pdf`, `svg`, `png` and `csv` paths,
#'   invisibly.
#' @seealso [wfm_save_plot()], [wfm_export_plot_data()]
#' @export
#'
#' @examples
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'   ggplot2::geom_point() + theme_wfm()
#' wfm_save_all(p, path = file.path(tempdir(), "cars.png"),
#'              background = "#FEFCFA")
wfm_save_all <- function(plot,
                         path,
                         width = "full",
                         height = NULL,
                         units = "in",
                         dpi = 320,
                         background = NULL,
                         ...) {
  stem <- wfm_export_stem(path)
  formats <- c("pdf", "svg", "png", "csv")
  files <- stats::setNames(paste0(stem, ".", formats), formats)
  # Validate chart data before writing any graphics files.
  plot <- wfm_materialise_plot_data(plot)
  data <- wfm_plot_data(plot)
  wfm_save_plot_files(
    plot, files[c("pdf", "svg", "png")], width, height, units, dpi,
    background, ...
  )
  wfm_write_plot_data(data, files[["csv"]])
  invisible(files)
}

wfm_save_plot_files <- function(plot, files, width, height, units, dpi,
                                background, device = NULL, ...) {
  retired <- intersect(names(list(...)), c("name", "filename", "bg"))
  if (length(retired)) {
    stop("Use `path` and `background` instead of `name`, `filename` or `bg`.",
         call. = FALSE)
  }
  if (!is.null(device) && length(files) != 1L) {
    stop("Use `device` only when saving a single format.", call. = FALSE)
  }
  background <- wfm_export_background(background)
  dimensions <- wfm_plot_dimensions(width, height, units, dpi)
  plot <- wfm_materialise_plot_data(plot)
  # Paint the background once on the device, including when it has alpha.
  plot <- plot + ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "transparent", colour = NA)
  )
  built <- ggplot2::ggplot_build(plot)
  # Reuse statistics and positions, but measure text on each output device.
  rendered <- grid::recordGrob({
    grid::grid.draw(ggplot2::ggplot_gtable(built))
  }, list(built = built))
  wfm_export_dir(files[[1]])
  for (format in names(files)) {
    ggplot2::ggsave(
      filename = files[[format]], plot = rendered,
      width = dimensions$width, height = dimensions$height,
      units = units, dpi = dimensions$dpi, bg = background,
      device = wfm_plot_device(format, device), ...
    )
  }
  invisible(files)
}

wfm_export_background <- function(background) {
  if (is.null(background)) return("transparent")
  wfm_export_string(background, "background")
  colours <- wfm_colours()
  if (background %in% names(colours)) return(unname(colours[[background]]))
  if (grepl("^#([[:xdigit:]]{6}|[[:xdigit:]]{8})$", background)) {
    return(background)
  }
  stop("`background` must be NULL, a #RRGGBB or #RRGGBBAA hex code, ",
       "or a wfmR colour name from `wfm_colours()`.", call. = FALSE)
}

wfm_plot_dimensions <- function(width, height, units, dpi) {
  if (!is.character(units) || length(units) != 1L || is.na(units) ||
      !units %in% c("in", "cm", "mm", "px")) {
    stop("`units` must be 'in', 'cm', 'mm' or 'px'.", call. = FALSE)
  }
  if (is.character(dpi) && length(dpi) == 1L && !is.na(dpi)) {
    dpi <- switch(dpi, screen = 72, print = 300, retina = 320, dpi)
  }
  wfm_export_positive(dpi, "dpi")
  if (is.character(width)) {
    if (length(width) != 1L || is.na(width) || !width %in% c("full", "half")) {
      stop("`width` must be 'full', 'half' or a positive number.", call. = FALSE)
    }
    full <- 210 / 25.4 - 2 * 1.3
    width <- if (width == "half") full / 2 else full
    width <- width * switch(units, "in" = 1, cm = 2.54, mm = 25.4, px = dpi)
  }
  wfm_export_positive(width, "width")
  if (is.null(height)) height <- width * 4.5 / 7
  wfm_export_positive(height, "height")
  list(width = width, height = height, dpi = dpi)
}

wfm_plot_device <- function(format, device = NULL) {
  if (!is.null(device)) return(device)
  format <- tolower(format)
  if (format == "svg") return(svglite::svglite)
  if (format == "pdf" && capabilities("cairo")) return(grDevices::cairo_pdf)
  if (requireNamespace("ragg", quietly = TRUE)) {
    return(switch(format,
      png = ragg::agg_png,
      jpg = ragg::agg_jpeg,
      jpeg = ragg::agg_jpeg,
      tif = ragg::agg_tiff,
      tiff = ragg::agg_tiff,
      NULL
    ))
  }
  NULL
}

wfm_export_stem <- function(path) {
  wfm_export_string(path, "path")
  if (grepl("[/\\\\]$", path) || basename(path) %in% c(".", "..") ||
      dir.exists(path)) {
    stop("`path` must include a file stem, not just a directory.", call. = FALSE)
  }
  if (grepl("^[.](pdf|svg|png|jpg|jpeg|tif|tiff|bmp|eps|ps|tex|emf|wmf|csv)$",
            basename(path), ignore.case = TRUE)) {
    stop("`path` must include a file stem before its extension.", call. = FALSE)
  }
  stem <- tools::file_path_sans_ext(path)
  if (!nzchar(basename(stem)) || basename(stem) %in% c(".", "..")) {
    stop("`path` must include a file stem before its extension.", call. = FALSE)
  }
  stem
}

wfm_export_file <- function(path, extension) {
  paste0(wfm_export_stem(path), ".", extension)
}

wfm_export_string <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    stop("`", name, "` must be a non-empty string.", call. = FALSE)
  }
}

wfm_export_positive <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x <= 0) {
    stop("`", name, "` must be a finite positive number.", call. = FALSE)
  }
}

wfm_export_dir <- function(filename) {
  directory <- dirname(filename)
  if (!dir.exists(directory) &&
      !dir.create(directory, recursive = TRUE, showWarnings = FALSE)) {
    stop("Could not create output directory: ", directory, call. = FALSE)
  }
}
