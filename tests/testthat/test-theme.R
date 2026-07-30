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
  expect_equal(thm$plot.title$colour, "#2F2F33")
  expect_equal(thm$axis.text$colour, "#2F2F33")
  expect_equal(thm$axis.title.y$angle, 90)
  expect_equal(margin_values(thm$axis.title.y$margin)[2], 6)
  expect_equal(thm$axis.title.y.right$angle, -90)
  expect_equal(margin_values(thm$axis.text.y$margin)[2], 0)
  expect_equal(thm$plot.title.position, "plot")
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
