test_that("CSV contains only source columns needed by mapped aesthetics", {
  data <- data.frame(x = c(1, 1, 2), y = c(4, 4, 7), g = c("a", "a", "b"),
                     unused = c("private", "private", "private"))
  p <- ggplot2::ggplot(data, ggplot2::aes(x, log(y), colour = g)) +
    ggplot2::geom_point()
  path <- file.path(tempdir(), "wfm-export-data", "nested")
  file <- wfm_export_plot_data(p, path = file.path(path, "source"))
  expect_identical(file, file.path(path, "source.csv"))
  result <- utils::read.csv(file)
  expect_equal(result, data[c("x", "y", "g")])
  expect_false(any(grepl("private", readLines(file))))

  remade <- ggplot2::ggplot(result, ggplot2::aes(x, log(y), colour = g)) +
    ggplot2::geom_point()
  expect_equal(ggplot2::ggplot_build(remade)$data,
               ggplot2::ggplot_build(p)$data)
})

test_that("CSV export replaces the final suffix and preserves the directory", {
  dir <- withr::local_tempdir()
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()

  suffixes <- c("csv", "CSV", "png", "other")
  for (i in seq_along(suffixes)) {
    path <- file.path(dir, "folder.with.dots", paste0("cars-", i, ".", suffixes[i]))
    result <- withVisible(wfm_export_plot_data(p, path = path))
    expected <- file.path(dirname(path), paste0("cars-", i, ".csv"))

    expect_false(result$visible)
    expect_identical(result$value, expected)
    restored <- utils::read.csv(expected)
    expect_named(restored, c("wt", "mpg"))
    expect_equal(restored$wt, mtcars$wt)
    expect_equal(restored$mpg, mtcars$mpg)
  }
  expect_length(list.files(dir, recursive = TRUE), 4)
})

test_that("CSV export rejects paths without a filename", {
  dir <- withr::local_tempdir()
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()

  for (path in c("", " ", ".", "..", dir, paste0(dir, "/"),
                 file.path(dir, ".csv"))) {
    expect_error(wfm_export_plot_data(p, path = path))
  }
  expect_error(wfm_export_plot_data(p, path = NA_character_))
  expect_error(wfm_export_plot_data(p, path = c("a", "b")))
  expect_length(list.files(dir, recursive = TRUE), 0)
})

test_that("shared layer data export once and include every used variable", {
  data <- data.frame(x = 1:3, y = 2:4, lower = 1:3, upper = 3:5,
                     label = letters[1:3], unused = 0)
  p <- ggplot2::ggplot(data, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper)) +
    ggplot2::geom_text(ggplot2::aes(label = label))
  expect_equal(wfm_plot_data(p), data[setdiff(names(data), "unused")])
})

test_that("inherited mappings and fixed overrides follow effective aesthetics", {
  data <- data.frame(x = 1:2, y = 4:5, alt = 7:8, unused = 9:10)
  p <- ggplot2::ggplot(data, ggplot2::aes(x, y, colour = unused)) +
    ggplot2::geom_point(ggplot2::aes(y = alt), colour = "red")
  expect_equal(wfm_plot_data(p), data[c("x", "alt")])

  p <- ggplot2::ggplot(data, ggplot2::aes(x, y, colour = unused)) +
    ggplot2::geom_point(ggplot2::aes(alt, x), inherit.aes = FALSE)
  expect_equal(wfm_plot_data(p), data[c("alt", "x")])
})

test_that("tidy pronouns, transformed mappings and non-syntactic names work", {
  data <- data.frame(x = 1:3, y = 2:4, offset = 101:103, unused = 0)
  data[["group label"]] <- letters[1:3]
  y_var <- "y"
  offset <- 10
  p <- ggplot2::ggplot(data, ggplot2::aes(
    .data$x, log(.data[[y_var]]) + .env$offset,
    group = .data[["group label"]]
  )) + ggplot2::geom_point()
  expect_equal(wfm_plot_data(p), data[c("x", "y", "group label")])

  p <- ggplot2::ggplot(data, ggplot2::aes(x, .env$offset)) +
    ggplot2::geom_point()
  expect_named(wfm_plot_data(p), "x")
})

test_that("statistic inputs preserve duplicates, weights and missing rows", {
  data <- data.frame(x = c(1, 1, 2, NA), w = c(2, 3, 4, 5),
                     g = c("a", "a", "b", "b"), count = 99, unused = 0)
  p <- ggplot2::ggplot(data, ggplot2::aes(
    x, y = ggplot2::after_stat(count), weight = w, group = g
  )) + ggplot2::geom_histogram(binwidth = 1) +
    ggplot2::coord_cartesian(xlim = c(1, 2))
  expect_equal(wfm_plot_data(p), data[c("x", "w", "g")])

  summary <- ggplot2::ggplot(data, ggplot2::aes(g, x)) +
    ggplot2::stat_summary(fun = mean, geom = "point", na.rm = TRUE)
  expect_equal(wfm_plot_data(summary), data[c("g", "x")])
})

test_that("weighted statistics can be remade from the exported observations", {
  data <- data.frame(x = c(1, 1, 2, 2, 3), weight = c(1, 2, 3, 1, 1),
                     group = c("a", "a", "a", "b", "b"), unused = 99)
  make_plot <- function(d) {
    ggplot2::ggplot(d, ggplot2::aes(x, weight = weight, fill = group)) +
      ggplot2::geom_histogram(binwidth = 1, boundary = 0.5)
  }
  original <- make_plot(data)
  filename <- wfm_export_plot_data(original, path = file.path(tempdir(), "weighted"))
  restored <- make_plot(utils::read.csv(filename))
  expect_equal(ggplot2::ggplot_build(restored)$data,
               ggplot2::ggplot_build(original)$data)
})

test_that("staged and computed aesthetics exclude derived variables", {
  data <- data.frame(x = 1:3, y = 4:6, g = letters[1:3],
                     count = 88, colour = "unused", unused = 0)
  p <- ggplot2::ggplot(data, ggplot2::aes(
    x, y = ggplot2::stage(start = y, after_stat = count),
    colour = g, fill = ggplot2::after_scale(colour)
  )) + ggplot2::geom_point()
  expect_equal(wfm_plot_data(p), data[c("x", "y", "g")])

  p <- ggplot2::ggplot(data, ggplot2::aes(x, y = ..count.. / sum(..count..))) +
    ggplot2::geom_histogram()
  expect_named(wfm_plot_data(p), "x")
})

test_that("facets retain their source variables without duplicate source rows", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() + ggplot2::facet_grid(cyl ~ gear)
  result <- wfm_plot_data(p)
  expect_named(result, c("wt", "mpg", "cyl", "gear"))
  expect_equal(nrow(result), nrow(mtcars))

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() + ggplot2::facet_wrap(ggplot2::vars(factor(cyl)))
  expect_named(wfm_plot_data(p), c("wt", "mpg", "cyl"))
})

test_that("separate data sources preserve layer and original-row provenance", {
  data <- data.frame(x = c(1, 1, 2), y = c(2, 2, 4), unused = 0)
  labels <- data.frame(x = 2, y = 5, label = "top", secret = "omit")
  p <- ggplot2::ggplot(data, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::geom_text(data = labels, ggplot2::aes(label = label))
  result <- wfm_plot_data(p)
  expect_named(result, c(".wfm_layer", ".wfm_row", "x", "y", "label"))
  expect_equal(result$.wfm_layer, c(1L, 1L, 1L, 2L))
  expect_equal(result$.wfm_row, c(1L, 2L, 3L, 1L))
  expect_equal(result$x, c(1, 1, 2, 2))
  expect_equal(result$label, c(NA, NA, NA, "top"))
})

test_that("layer data functions and formulas export their returned rows", {
  data <- data.frame(x = 1:4, y = 5:8, unused = "secret")
  p <- ggplot2::ggplot(data, ggplot2::aes(x, y)) +
    ggplot2::geom_point(data = function(d) d[d$x <= 2, ]) +
    ggplot2::geom_line(data = ~ subset(.x, x >= 3))
  result <- wfm_plot_data(p)
  expect_equal(result$.wfm_layer, c(1L, 1L, 2L, 2L))
  expect_equal(result$x, 1:4)
  expect_named(result, c(".wfm_layer", ".wfm_row", "x", "y"))
})

test_that("saving all formats evaluates layer functions once without mutation", {
  calls <- 0L
  layer_data <- function(data) {
    calls <<- calls + 1L
    data$y <- data$y + calls
    data
  }
  data <- data.frame(x = 1:3, y = 4:6, unused = "omit")
  p <- ggplot2::ggplot(data, ggplot2::aes(x, y)) +
    ggplot2::geom_point(data = layer_data) +
    ggplot2::theme_minimal(base_family = "sans")
  files <- wfm_save_all(
    p, path = file.path(tempfile("wfm-materialised-"), "materialised"),
    width = 2, height = 2, dpi = 72
  )
  expect_identical(calls, 1L)
  expect_true(all(file.exists(files)))
  expect_equal(utils::read.csv(files[["csv"]])$y, 5:7)
  expect_identical(get("data", envir = p$layers[[1]], inherits = FALSE), layer_data)
  expect_equal(p$data, data)
  expect_null(p$layers[[1]]$computed_mapping)
})

test_that("facet-only plot data preserve panels absent from all layer data", {
  data <- data.frame(x = 1:3, y = 4:6, f = letters[1:3], unused = "omit")
  p <- ggplot2::ggplot(data, ggplot2::aes(x, y)) +
    ggplot2::geom_point(data = data[1, ]) + ggplot2::facet_wrap(~ f)
  result <- wfm_plot_data(p)
  expect_equal(result$.wfm_layer, c(1L, 0L, 0L, 0L))
  expect_equal(result$f, c("a", "a", "b", "c"))
  expect_equal(result$x, c(1L, NA, NA, NA))
})

test_that("filtered layers and all facet panels can be remade from CSV", {
  data <- data.frame(x = 1:4, y = 5:8, f = letters[1:4], unused = "omit")
  p <- ggplot2::ggplot(data, ggplot2::aes(x, y)) +
    ggplot2::geom_point(data = ~ subset(.x, x <= 2)) +
    ggplot2::geom_text(data = data[3, ], ggplot2::aes(label = y)) +
    ggplot2::facet_wrap(~ f)
  filename <- wfm_export_plot_data(p, path = file.path(tempdir(), "facets"))
  restored <- utils::read.csv(filename)
  remade <- ggplot2::ggplot(subset(restored, .wfm_layer == 0),
                           ggplot2::aes(x, y)) +
    ggplot2::geom_point(data = subset(restored, .wfm_layer == 1)) +
    ggplot2::geom_text(data = subset(restored, .wfm_layer == 2),
                       ggplot2::aes(label = y)) +
    ggplot2::facet_wrap(~ f)
  built <- ggplot2::ggplot_build(p)
  rebuilt <- ggplot2::ggplot_build(remade)
  expect_equal(rebuilt$data, built$data)
  expect_equal(rebuilt$layout$layout, built$layout$layout)
})

test_that("annotations use their source positions without inherited plot data", {
  data <- data.frame(x = 1:3, y = 2:4, unused = "omit")
  p <- ggplot2::ggplot(data, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::annotate("text", x = 2, y = 4, label = "fixed in plot code") +
    ggplot2::geom_hline(yintercept = 2.5)
  result <- wfm_plot_data(p)
  expect_equal(result$.wfm_layer, c(1L, 1L, 1L, 2L, 3L))
  expect_named(result, c(".wfm_layer", ".wfm_row", "x", "y", "yintercept"))
  expect_equal(result$yintercept, c(NA, NA, NA, NA, 2.5))
})

test_that("constant mappings preserve counts without exporting unused data", {
  data <- data.frame(unused = letters[1:3])
  p <- ggplot2::ggplot(data, ggplot2::aes(x = "all")) + ggplot2::geom_bar()
  expect_equal(wfm_plot_data(p), data.frame(.wfm_row = 1:3))
})

test_that("unsafe mappings fail before writing a partial CSV", {
  data <- data.frame(x = 1:3, y = 2:4)
  external <- 10:12
  p <- ggplot2::ggplot(data, ggplot2::aes(x, .env$external)) +
    ggplot2::geom_point()
  path <- tempfile("wfm-unsafe-")
  expect_error(wfm_export_plot_data(p, file.path(path, "unsafe")),
               "External mapped vectors")
  expect_false(dir.exists(path))

  p <- ggplot2::ggplot(data, ggplot2::aes(x, get("y"))) + ggplot2::geom_point()
  expect_error(wfm_plot_data(p), "Dynamic column lookups")
  helper <- function(d) d$y
  p <- ggplot2::ggplot(data, ggplot2::aes(x, helper(.data))) + ggplot2::geom_point()
  expect_error(wfm_plot_data(p), "Whole-data")
  p <- ggplot2::ggplot(data, ggplot2::aes(x, missing_column)) +
    ggplot2::geom_point()
  expect_error(wfm_plot_data(p), "Source column `missing_column` is missing")

  p <- ggplot2::ggplot(data, ggplot2::aes(!!external, y)) + ggplot2::geom_point()
  expect_error(wfm_plot_data(p), "Injected aesthetic vectors")
  p <- ggplot2::ggplot(data, ggplot2::aes(x, y)) +
    ggplot2::geom_point(size = external)
  expect_error(wfm_plot_data(p), "Vector-valued fixed aesthetics")
})

test_that("CSV rejects ambiguous columns and incompatible layer types", {
  data <- data.frame(x = 1:2, y = 2:3)
  data$.wfm_layer <- 1:2
  p <- ggplot2::ggplot(data, ggplot2::aes(x, .wfm_layer)) + ggplot2::geom_point()
  expect_error(wfm_plot_data(p), "reserved source columns")

  data$y <- I(list(1, 2))
  p <- ggplot2::ggplot(data, ggplot2::aes(x, y)) + ggplot2::geom_point()
  expect_error(wfm_plot_data(p), "CSV cannot preserve")

  p <- ggplot2::ggplot(data.frame(x = 1, y = 2), ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::geom_point(data = data.frame(x = "a", y = 3))
  expect_error(wfm_plot_data(p), "incompatible types")
  expect_error(wfm_plot_data(data.frame()), "ggplot object")
  expect_error(wfm_plot_data(ggplot2::ggplot()), "at least one layer")
})

test_that("composite plots cannot silently export only their final component", {
  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 2:4), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  class(p) <- c("patchwork", class(p))
  path <- tempfile("wfm-composite-")

  expect_error(wfm_export_plot_data(p, file.path(path, "composite")), "Patchwork")
  expect_error(wfm_save_plot(p, file.path(path, "composite")), "Patchwork")
  expect_error(wfm_save_all(p, file.path(path, "composite")), "Patchwork")
  expect_false(dir.exists(path))
})
