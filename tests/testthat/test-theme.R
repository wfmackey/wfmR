test_that("wfm scales use the expected palette", {
  pal_fun <- wfmR:::wfm_palette_function()
  publication_fun <- wfmR:::wfm_palette_function(palette = "publication")
  colorblind_fun <- wfmR:::wfm_palette_function(palette = "colorblind")

  expect_equal(pal_fun(3), unname(wfm_palette(3)))
  expect_equal(publication_fun(3), unname(wfm_palette(3, palette = "colourblind")))
  expect_equal(colorblind_fun(3), unname(wfm_palette(3, palette = "colourblind")))
  expect_warning(pal_fun(9), "recycled")
})

test_that("theme_wfm returns a ggplot theme", {
  thm <- theme_wfm()
  margin_values <- function(x) unname(as.numeric(x))

  expect_s3_class(thm, "theme")
  expect_identical(thm$text$family, "Source Serif Pro")
  expect_equal(thm$plot.title$colour, "#2F2F33")
  expect_equal(thm$axis.text$colour, "#2F2F33")
  expect_equal(thm$axis.title.y$angle, 90)
  expect_equal(margin_values(thm$axis.title.y$margin)[2], 6)
  expect_equal(thm$axis.title.y.right$angle, -90)
  expect_equal(margin_values(thm$axis.text.y$margin)[2], 0)
  expect_equal(thm$plot.title.position, "plot")
})

test_that("wfm theme helpers retain an explicit font family", {
  old_theme <- ggplot2::theme_get()
  withr::defer(ggplot2::theme_set(old_theme))

  expect_identical(theme_wfm(base_family = "serif")$text$family, "serif")
  result <- withVisible(wfm_set_theme())
  expect_false(result$visible)
  expect_identical(result$value, old_theme)
  expect_identical(ggplot2::theme_get()$text$family, "Source Serif Pro")
  wfm_set_theme(base_family = "sans")
  expect_identical(ggplot2::theme_get()$text$family, "sans")
})

test_that("chart defaults use font-aware devices for each document format", {
  skip_if_not_installed("knitr")
  old_theme <- ggplot2::theme_get()
  old_knit <- knitr::opts_knit$get()
  old_chunk <- knitr::opts_chunk$get()
  old_hooks <- knitr::opts_hooks$get()
  withr::defer({
    ggplot2::theme_set(old_theme)
    knitr::opts_knit$restore(old_knit)
    knitr::opts_chunk$restore(old_chunk)
    knitr::opts_hooks$restore(old_hooks)
  })

  knitr::opts_knit$set(rmarkdown.pandoc.to = "html")
  settings <- wfm_chart_defaults()
  expect_identical(settings$base_family, "Source Serif Pro")
  expect_equal(settings$base_size, 11)
  expect_identical(ggplot2::theme_get()$text$family, "Source Serif Pro")
  expect_identical(knitr::opts_chunk$get("dev"), "svglite")
  expect_identical(knitr::opts_chunk$get("dev.args"), list(bg = "transparent"))

  knitr::opts_knit$set(rmarkdown.pandoc.to = "latex")
  if (capabilities("cairo")) {
    settings <- wfm_chart_defaults()
    expect_equal(settings$base_size, 12)
    expect_identical(settings$base_family, "Source Serif Pro")
    expect_identical(knitr::opts_chunk$get("dev"), "cairo_pdf")
  } else {
    expect_error(wfm_chart_defaults(), "Cairo graphics support is required")
  }

  knitr::opts_knit$set(rmarkdown.pandoc.to = "docx")
  wfm_chart_defaults()
  expected <- if (requireNamespace("ragg", quietly = TRUE)) "ragg_png" else "png"
  expect_identical(knitr::opts_chunk$get("dev"), expected)
})

test_that("chart defaults honour an explicit font and font size", {
  skip_if_not_installed("knitr")
  old_theme <- ggplot2::theme_get()
  old_knit <- knitr::opts_knit$get()
  old_chunk <- knitr::opts_chunk$get()
  old_hooks <- knitr::opts_hooks$get()
  withr::defer({
    ggplot2::theme_set(old_theme)
    knitr::opts_knit$restore(old_knit)
    knitr::opts_chunk$restore(old_chunk)
    knitr::opts_hooks$restore(old_hooks)
  })

  knitr::opts_knit$set(rmarkdown.pandoc.to = "html")
  settings <- wfm_chart_defaults(base_family = "serif", base_size = 9)
  expect_identical(settings$base_family, "serif")
  expect_equal(settings$base_size, 9)
  expect_identical(ggplot2::theme_get()$text$family, "serif")
  expect_equal(ggplot2::theme_get()$text$size, 9)
})

test_that("scale_y_wfm uses tighter default expansion", {
  scale <- scale_y_wfm()

  expect_s3_class(scale, "ScaleContinuousPosition")
  expect_equal(scale$expand, c(0.01, 0, 0.035, 0))
})

test_that("wfm colour demo returns a ggplot", {
  plot <- wfm_colour_demo()

  expect_s3_class(plot, "ggplot")
})

test_that("continuous wfm scales accept palette and legend names", {
  seq_scale <- scale_fill_wfm_seq(palette = "blue_teal_yellow", name = "Height")
  div_scale <- scale_colour_wfm_div(palette = "teal_salmon", midpoint = 0, name = "Signal")

  expect_s3_class(seq_scale, "ScaleContinuous")
  expect_s3_class(div_scale, "ScaleContinuous")
})
