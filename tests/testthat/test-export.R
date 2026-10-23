export_test_plot <- function() {
  ggplot2::ggplot(
    data.frame(x = 1:4, y = c(2, 5, 3, 7), unused = letters[1:4]),
    ggplot2::aes(x, y)
  ) +
    ggplot2::geom_point() +
    ggplot2::labs(title = "Export check") +
    theme_wfm()
}

export_svg_inches <- function(file) {
  svg <- paste(readLines(file, warn = FALSE), collapse = "\n")
  dimensions <- vapply(c("width", "height"), function(dimension) {
    pattern <- paste0(dimension, "=['\"]([0-9.]+)pt['\"]")
    value <- regmatches(svg, regexpr(pattern, svg))
    as.numeric(sub(pattern, "\\1", value)) / 72
  }, numeric(1))
  unname(dimensions)
}

export_png_pixels <- function(file) {
  header <- readBin(file, what = "raw", n = 24)
  expect_identical(header[1:8], as.raw(c(137, 80, 78, 71, 13, 10, 26, 10)))
  expect_identical(rawToChar(header[13:16]), "IHDR")
  c(
    sum(as.numeric(header[17:20]) * 256^(3:0)),
    sum(as.numeric(header[21:24]) * 256^(3:0))
  )
}

export_pdf_inches <- function(file) {
  bytes <- readBin(file, what = "raw", n = file.info(file)$size)
  expect_identical(rawToChar(bytes[1:5]), "%PDF-")
  bytes[bytes == as.raw(0)] <- as.raw(32)
  pdf <- rawToChar(bytes)
  pattern <- "/MediaBox[[:space:]]*\\[[^]]+\\]"
  box <- regmatches(pdf, regexpr(pattern, pdf, useBytes = TRUE))
  # Recent Cairo versions compress page dictionaries into object streams.
  if (!length(box)) return(NULL)
  values <- as.numeric(strsplit(gsub("[^0-9. -]", "", box), " +")[[1]])
  values <- values[!is.na(values)]
  (values[3:4] - values[1:2]) / 72
}

test_that("export helpers use paths and default to a transparent background", {
  for (helper in list(wfm_save_plot, wfm_save_all)) {
    arguments <- formals(helper)
    expect_true("path" %in% names(arguments))
    expect_true("background" %in% names(arguments))
    expect_null(arguments$background)
    expect_false(any(c("name", "filename", "bg") %in% names(arguments)))
  }
  expect_true("path" %in% names(formals(wfm_export_plot_data)))
  expect_false("name" %in% names(formals(wfm_export_plot_data)))
})

test_that("save_plot exports three files at the full document width", {
  dir <- withr::local_tempdir()
  p <- export_test_plot()
  result <- withVisible(wfm_save_plot(p, path = file.path(dir, "full")))
  paths <- result$value
  width <- 210 / 25.4 - 2.6
  dimensions <- c(width, width * 4.5 / 7)

  expect_false(result$visible)
  expect_named(paths, c("pdf", "svg", "png"))
  expect_identical(basename(unname(paths)), paste0("full.", names(paths)))
  expect_true(all(file.exists(paths)))
  expect_equal(export_svg_inches(paths[["svg"]]), dimensions, tolerance = 0.001)
  pdf_dimensions <- export_pdf_inches(paths[["pdf"]])
  if (!is.null(pdf_dimensions)) {
    expect_equal(pdf_dimensions, dimensions, tolerance = 0.02)
  }
  expect_true(all(abs(export_png_pixels(paths[["png"]]) - dimensions * 320) <= 1))
  expect_match(paste(readLines(paths[["svg"]]), collapse = "\n"), "Export check")
})

test_that("half width uses half the document width and accepts a chosen height", {
  dir <- withr::local_tempdir()
  paths <- wfm_save_plot(
    export_test_plot(), path = file.path(dir, "half"),
    width = "half", height = 2.5, formats = "svg"
  )

  expect_named(paths, "svg")
  expect_equal(
    export_svg_inches(paths[["svg"]]),
    c((210 / 25.4 - 2.6) / 2, 2.5),
    tolerance = 0.001
  )
  expect_identical(list.files(dir), "half.svg")
})

test_that("numeric dimensions keep their physical size in supported units", {
  dir <- withr::local_tempdir()
  dimensions <- list("in" = c(4, 3), cm = c(10.16, 7.62), mm = c(101.6, 76.2),
                     px = c(800, 600))

  for (unit in names(dimensions)) {
    paths <- wfm_save_plot(
      export_test_plot(), path = file.path(dir, paste0("size-", unit)),
      width = dimensions[[unit]][1], height = dimensions[[unit]][2],
      units = unit, dpi = 200, formats = "svg"
    )
    expect_equal(export_svg_inches(paths[["svg"]]), c(4, 3), tolerance = 0.001)
  }
})

test_that("preset widths remain physical widths when height uses other units", {
  dir <- withr::local_tempdir()
  paths <- wfm_save_plot(
    export_test_plot(), path = file.path(dir, "metric"),
    width = "full", height = 50.8, units = "mm", formats = "svg"
  )

  expect_equal(
    export_svg_inches(paths[["svg"]]), c(210 / 25.4 - 2.6, 2),
    tolerance = 0.001
  )
})

test_that("save_plot creates directories and accepts a single custom device", {
  dir <- withr::local_tempdir()
  paths <- wfm_save_plot(
    export_test_plot(), path = file.path(dir, "output", "nested", "chart"),
    width = 4, height = 3, formats = "svg", device = svglite::svglite
  )

  expect_true(file.exists(file.path(dir, "output", "nested", "chart.svg")))
  expect_equal(export_svg_inches(paths[["svg"]]), c(4, 3), tolerance = 0.001)
})

test_that("a path suffix selects a single plot format", {
  dir <- withr::local_tempdir()
  file <- file.path(dir, "chart.png")
  result <- withVisible(wfm_save_plot(export_test_plot(), path = file))

  expect_false(result$visible)
  expect_identical(result$value, c(png = file))
  dimensions <- (210 / 25.4 - 2.6) * c(1, 4.5 / 7)
  expect_true(all(abs(export_png_pixels(file) - dimensions * 320) <= 1))
  expect_identical(list.files(dir), "chart.png")
})

test_that("path suffixes are case insensitive and retain explicit dimensions", {
  dir <- withr::local_tempdir()
  file <- file.path(dir, "chart.SVG")
  result <- withVisible(wfm_save_plot(
    export_test_plot(), path = file, width = 4, height = 3
  ))

  expect_false(result$visible)
  expect_named(result$value, "svg")
  expect_equal(export_svg_inches(result$value[["svg"]]), c(4, 3),
               tolerance = 0.001)
  expect_length(list.files(dir), 1)
  expect_identical(tolower(list.files(dir)), "chart.svg")
})

test_that("positional paths work and suffixes select the output format", {
  dir <- withr::local_tempdir()
  file <- file.path(dir, "chart.svg")
  result <- withVisible(wfm_save_plot(
    export_test_plot(), file, width = 4, height = 3, formats = "pdf"
  ))

  expect_false(result$visible)
  expect_identical(result$value, c(svg = file))
  expect_equal(export_svg_inches(file), c(4, 3), tolerance = 0.001)
  expect_identical(list.files(dir), "chart.svg")
})

test_that("NULL backgrounds export transparent corners without changing the plot", {
  skip_if_not_installed("png")
  dir <- withr::local_tempdir()
  p <- export_test_plot() + ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "#112233", colour = NA),
    panel.background = ggplot2::element_rect(fill = "#ABCDEF", colour = NA)
  )
  original_theme <- p$theme
  files <- wfm_save_plot(p, path = file.path(dir, "transparent"),
                         width = 2, height = 2, dpi = 72)
  raster <- png::readPNG(files[["png"]])
  svg <- paste(readLines(files[["svg"]], warn = FALSE), collapse = "\n")

  expect_equal(dim(raster)[3], 4)
  expect_equal(raster[1, 1, 4], 0)
  expect_match(svg, "fill: none;")
  expect_match(svg, "fill: #ABCDEF;", fixed = TRUE)
  expect_false(grepl("fill: #112233;", svg, fixed = TRUE))
  expect_equal(p$theme, original_theme)
})

test_that("background accepts hex and palette names without doubling alpha", {
  skip_if_not_installed("png")
  dir <- withr::local_tempdir()
  p <- export_test_plot() + ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "#112233", colour = NA)
  )
  original_theme <- p$theme
  backgrounds <- list(
    opaque = "#E76F51", alpha = "#E76F5180", palette = "blue"
  )
  colours <- c(opaque = "#E76F51", alpha = "#E76F5180", palette = "#286983")

  for (name in names(backgrounds)) {
    file <- wfm_save_plot(
      p, path = file.path(dir, paste0(name, ".png")),
      width = 2, height = 2, dpi = 72, background = backgrounds[[name]]
    )
    raster <- png::readPNG(file[["png"]])
    expected <- as.numeric(grDevices::col2rgb(colours[[name]], alpha = TRUE)) / 255
    actual <- raster[1, 1, ]
    if (length(actual) == 3) actual <- c(actual, 1)
    # PNG unpremultiplication can round RGB channels at partial transparency.
    expect_lte(max(abs(actual[1:3] - expected[1:3])), 2 / 255)
    expect_equal(actual[4], expected[4], tolerance = 1 / 255)
  }
  expect_equal(p$theme, original_theme)
})

test_that("save_all passes the selected background to each graphics format", {
  skip_if_not_installed("png")
  dir <- withr::local_tempdir()
  paths <- wfm_save_all(
    export_test_plot(), path = file.path(dir, "background.png"),
    width = 2, height = 2, dpi = 72, background = "salmon"
  )
  raster <- png::readPNG(paths[["png"]])
  colour <- unname(wfm_colours()[["salmon"]])
  expected <- as.numeric(grDevices::col2rgb(colour)) / 255
  svg <- paste(readLines(paths[["svg"]], warn = FALSE), collapse = "\n")

  expect_equal(raster[1, 1, 1:3], expected, tolerance = 1 / 255)
  expect_match(svg, paste0("fill: ", toupper(colour), ";"), fixed = TRUE)
  expect_true(all(file.exists(paths)))
})

test_that("invalid backgrounds fail before creating files", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "nested", "chart")

  for (background in list(NA_character_, character(), c("blue", "teal"),
                           12, TRUE, "not-a-colour", "#GGHHII")) {
    expect_error(wfm_save_plot(export_test_plot(), path = path,
                               background = background))
    expect_error(wfm_save_all(export_test_plot(), path = path,
                              background = background))
  }
  expect_false(dir.exists(dirname(path)))
})

test_that("save_plot rejects invalid paths, dimensions, formats and devices", {
  dir <- withr::local_tempdir()
  p <- export_test_plot()
  path <- file.path(dir, "chart")

  for (invalid in c("", " ", ".", "..", dir, paste0(dir, "/"),
                    file.path(dir, ".png"), file.path(dir, "chart.bad"),
                    file.path(dir, "chart.csv"))) {
    expect_error(wfm_save_plot(p, path = invalid))
  }
  expect_error(wfm_save_plot(p, path = NA_character_))
  expect_error(wfm_save_plot(p, path = c("a", "b")))
  expect_error(wfm_save_plot(p, path = path, width = "third"))
  expect_error(wfm_save_plot(p, path = path, width = 0))
  expect_error(wfm_save_plot(p, path = path, width = c(4, 5)))
  expect_error(wfm_save_plot(p, path = path, height = -1))
  expect_error(wfm_save_plot(p, path = path, height = Inf))
  expect_error(wfm_save_plot(p, path = path, units = "pt"))
  expect_error(wfm_save_plot(p, path = path, formats = "jpg"))
  expect_error(wfm_save_plot(p, path = path, formats = character()))
  expect_error(wfm_save_plot(p, path = path, device = grDevices::pdf))
  expect_error(wfm_save_plot(list(), path = path))
  expect_length(list.files(dir, recursive = TRUE), 0)
})

test_that("save_all exports graphics and only the chart's source columns", {
  dir <- withr::local_tempdir()
  result <- withVisible(wfm_save_all(
    export_test_plot(), path = file.path(dir, "chart"), width = 4, height = 3
  ))
  paths <- result$value

  expect_false(result$visible)
  expect_named(paths, c("pdf", "svg", "png", "csv"))
  expect_true(all(file.exists(paths)))
  data <- utils::read.csv(paths[["csv"]])
  expect_true(all(c("x", "y") %in% names(data)))
  expect_false("unused" %in% names(data))
  expect_equal(data$x, 1:4)
  expect_equal(data$y, c(2, 5, 3, 7))
  expect_equal(export_svg_inches(paths[["svg"]]), c(4, 3), tolerance = 0.001)
})

test_that("save_all ignores the supplied suffix and retains the filename stem", {
  dir <- withr::local_tempdir()
  stems <- c("cars.v1", "cars.png", "cars.pdf")
  suffixes <- c("png", "CSV", "other")
  for (i in seq_along(suffixes)) {
    path <- file.path(dir, "folder.with.dots", paste0(stems[i], ".", suffixes[i]))
    paths <- wfm_save_all(export_test_plot(), path = path, width = 2, height = 2,
                          dpi = 72)

    expect_named(paths, c("pdf", "svg", "png", "csv"))
    expect_identical(
      basename(unname(paths)), paste0(stems[i], ".", names(paths))
    )
    expect_true(all(file.exists(paths)))
    expect_identical(dirname(unname(paths)), rep(dirname(path), 4))
  }
  expect_length(list.files(dir, recursive = TRUE), 12)
})

test_that("save_all checks chart data before creating graphic files", {
  dir <- withr::local_tempdir()
  p <- ggplot2::ggplot(data.frame(y = 1:3), ggplot2::aes(missing_x, y)) +
    ggplot2::geom_point()

  expect_error(wfm_save_all(p, path = file.path(dir, "invalid")))
  expect_length(list.files(dir, recursive = TRUE), 0)
})

test_that("save_plot calculates statistics once for all output formats", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  devices <- grDevices::dev.list()
  active <- grDevices::dev.cur()
  calls <- 0L
  p <- ggplot2::ggplot(
    data.frame(x = rep(1, 4), y = c(2, 5, 3, 7)),
    ggplot2::aes(x, y)
  ) + ggplot2::stat_summary(
    fun = function(y) {
      calls <<- calls + 1L
      mean(y)
    },
    geom = "point"
  )

  paths <- wfm_save_plot(p, path = "summary", width = 4, height = 3)

  expect_identical(calls, 1L)
  expect_true(all(file.exists(paths)))
  expect_named(paths, c("pdf", "svg", "png"))
  expect_identical(grDevices::dev.list(), devices)
  expect_identical(grDevices::dev.cur(), active)
  expect_false(file.exists("Rplots.pdf"))
})
