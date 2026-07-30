test_that("base palette is stable and named", {
  colours <- wfm_colours()
  pal <- wfm_palette()

  expect_named(
    pal,
    c("salmon", "teal", "purple", "lime", "yellow", "blue", "pink")
  )
  expect_equal(colours, pal)
  expect_equal(unname(pal["teal"]), wfm_teal)
  expect_equal(unname(pal["salmon"]), wfm_salmon)
  expect_equal(unname(pal["yellow"]), wfm_yellow)
  expect_equal(unname(wfm_colours("blue", palette = "wong")), "#0072B2")
  expect_true(all(grDevices::col2rgb(pal) >= 0))
})

test_that("default colour constants are exported and stable", {
  expect_equal(wfm_teal, "#56949F")
  expect_equal(wfm_salmon, "#D7827E")
  expect_equal(wfm_yellow, "#EA9D34")
  expect_equal(wfm_pink, "#B4637A")
  expect_equal(wfm_lime, "#8B9A5A")
  expect_equal(wfm_blue, "#286983")
  expect_equal(wfm_green, "#6F8F72")
  expect_equal(wfm_purple, "#907AA9")
})

test_that("tints move colours towards the background", {
  expect_equal(wfm_tint("#000000", amount = 0), "#000000")
  expect_equal(wfm_tint("#000000", amount = 1), "#FFFFFF")

  tints <- wfm_tints(wfm_colours("teal", "salmon"), strength = c(90, 70))
  expect_named(
    tints,
    c("teal90", "salmon90", "teal70", "salmon70")
  )
  expect_true(all(grDevices::col2rgb(tints) >= 0))
  expect_equal(wfm_teal90, unname(wfm_tints(wfm_colours("teal"), strength = 90)))
  expect_equal(wfm_teal10, unname(wfm_tints(wfm_colours("teal"), strength = 10)))
  expect_true(mean(grDevices::col2rgb(wfm_teal10)) > mean(grDevices::col2rgb(wfm_teal90)))
})

test_that("semantic definition colours match the course-note source", {
  defs <- wfm_definition_colours()

  expect_equal(defs$name, c("object", "parameter", "welfare", "method"))
  expect_equal(defs$colour, unname(c(wfm_blue, wfm_salmon, wfm_green, wfm_purple)))
  expect_equal(defs$tint, unname(c(wfm_blue10, wfm_salmon10, wfm_green10, wfm_purple10)))
  expect_equal(defs$text, unname(c(wfm_blue, wfm_salmon, wfm_green, wfm_purple)))
})

test_that("primary palette colour-vision diagnostics stay explicit", {
  report <- wfm_colour_blind_report(min_distance = 10, severity = 10)

  expect_setequal(report$cvd, c("none", "deutan", "protan", "tritan"))
  expect_false(all(report$passes))
  expect_equal(report$colour_1[report$cvd == "deutan"], "teal")
  expect_equal(report$colour_2[report$cvd == "deutan"], "purple")
  expect_equal(report$colour_1[report$cvd == "protan"], "blue")
  expect_equal(report$colour_2[report$cvd == "protan"], "pink")
  expect_equal(report$colour_1[report$cvd == "tritan"], "salmon")
  expect_equal(report$colour_2[report$cvd == "tritan"], "yellow")
  expect_false(wfm_is_colour_blind_safe(wfm_palette(3), min_distance = 10, severity = 10))
})

test_that("palette options expose discrete and continuous choices", {
  options <- wfm_palette_options()

  expect_true(all(c("primary", "wong", "colourblind", "minou") %in% options$name))
  expect_true(all(c("blue_teal_yellow", "blue_green_yellow", "purple_pink_salmon", "teal_lime_yellow", "blue_purple_pink") %in% options$name))
  expect_true(all(c("teal_salmon", "blue_yellow", "green_pink", "purple_lime", "blue_salmon") %in% options$name))
  expect_false(any(c("sunset", "sunset_ruby", "ink_ruby", "violet_ruby", "sky_brickrose", "ink_amber", "amber_light", "orange_light", "blue_coral", "darkblue_orange") %in% options$name))
  expect_setequal(unique(options$type), c("discrete", "sequential", "diverging"))
})

test_that("Minou palette is available in its source order", {
  expected <- c(
    teal = "#00798C",
    red = "#D1495B",
    yellow = "#EDAE49",
    green = "#66A182",
    navy = "#2E4057",
    grey = "#8D96A3"
  )

  expect_equal(wfm_palette(palette = "minou"), expected)
  expect_equal(wfm_colours(palette = "minou"), expected)
  expect_equal(wfm_palette(3, palette = "minou"), expected[1:3])
})

test_that("old exploration palette names still resolve as aliases", {
  expect_equal(wfm_palette(palette = "sunset"), wfm_palette(palette = "primary"))
  expect_equal(wfm_palette(palette = "sunset_ruby"), wfm_palette(palette = "primary"))
  expect_equal(wfm_palette(palette = "rosepine"), wfm_palette(palette = "primary"))
  expect_equal(wfm_palette(palette = "publication"), wfm_palette(palette = "colourblind"))
  expect_equal(wfm_sequential_palette("amber_light", n = 5), wfm_sequential_palette("blue_teal_yellow", n = 5))
  expect_equal(wfm_sequential_palette("orange_light", n = 5), wfm_sequential_palette("blue_teal_yellow", n = 5))
  expect_equal(wfm_diverging_palette("violet_ruby", n = 5), wfm_diverging_palette("green_pink", n = 5))
  expect_equal(wfm_diverging_palette("blue_coral", n = 5), wfm_diverging_palette("teal_salmon", n = 5))
  expect_equal(wfm_diverging_palette("ink_amber", n = 5), wfm_diverging_palette("blue_yellow", n = 5))
  expect_equal(wfm_diverging_palette("darkblue_orange", n = 5), wfm_diverging_palette("blue_yellow", n = 5))
})

test_that("Wong palette remains available as a colour-blind-friendly option", {
  report <- wfm_colour_blind_report(
    wfm_palette(palette = "colourblind"),
    min_distance = 8,
    severity = 10
  )

  expect_true(all(report$passes))
  expect_equal(wfm_palette(palette = "colourblind"), wfm_palette(palette = "wong"))
})

test_that("continuous ramps can use Lab and OKLab interpolation", {
  colours <- c(wfm_blue, wfm_yellow)
  lab <- wfm_colour_ramp(colours, n = 7, space = "Lab")
  oklab <- wfm_colour_ramp(colours, n = 7, space = "OKLab")

  expect_length(lab, 7)
  expect_length(oklab, 7)
  expect_equal(lab[c(1, 7)], colours)
  expect_equal(oklab[c(1, 7)], colours)
  expect_equal(wfm_colour_ramp(colours, n = 3, reverse = TRUE)[1], "#EA9D34")
})

test_that("sequential scales have ordered lightness structure", {
  options <- subset(wfm_palette_options("sequential"), type == "sequential")

  for (name in options$name) {
    colours <- wfm_sequential_palette(name, n = 256)
    lab <- farver::decode_colour(colours, to = "lab")
    lightness <- lab[, "l"]

    expect_length(colours, 256)
    expect_true(diff(range(lightness)) >= 35, info = name)
    expect_true(min(diff(lightness)) >= -0.5, info = name)
  }
})

test_that("diverging scales have neutral midpoint and separated endpoints", {
  options <- subset(wfm_palette_options("diverging"), type == "diverging")

  for (name in options$name) {
    colours <- wfm_diverging_palette(name, n = 257)
    endpoints <- colours[c(1, 129, 257)]
    lightness <- farver::decode_colour(endpoints, to = "lab")[, "l"]

    expect_length(colours, 257)
    expect_true(lightness[2] >= max(lightness[c(1, 3)]) + 20, info = name)
    expect_true(
      wfm_colour_distances(endpoints[c(1, 3)], cvd = "none")[1, 2] >= 10,
      info = name
    )
  }
})

test_that("wfm_colourblind() swaps the primary palette for the Wong palette", {
  old <- getOption("wfmR.colourblind")
  on.exit(options(wfmR.colourblind = old), add = TRUE)

  options(wfmR.colourblind = NULL)
  expect_false(wfm_colourblind())
  expect_equal(wfm_palette(), wfm_colours(palette = "wong"))
  expect_equal(wfm_palette(palette = "primary"), wfm_colours(palette = "wong"))
  expect_equal(wfm_palette(palette = "minou"), wfm_colours(palette = "minou"))
  expect_equal(unname(wfm_colours("teal")), wfm_teal)

  expect_true(wfm_colourblind(FALSE))
  expect_equal(wfm_palette(), wfm_colours(palette = "primary"))
  expect_error(wfm_colourblind(NA), "TRUE or FALSE")
})

test_that("wfm scales follow the colourblind switch at draw time", {
  old <- getOption("wfmR.colourblind")
  on.exit(options(wfmR.colourblind = old), add = TRUE)

  p <- ggplot2::ggplot(
    data.frame(g = c("a", "b"), y = c(1, 2)),
    ggplot2::aes(g, y, fill = g)
  ) +
    ggplot2::geom_col() +
    scale_fill_wfm()

  options(wfmR.colourblind = TRUE)
  fills <- unique(ggplot2::ggplot_build(p)$data[[1]]$fill)
  expect_setequal(fills, unname(wfm_colours(palette = "wong"))[1:2])

  options(wfmR.colourblind = FALSE)
  fills <- unique(ggplot2::ggplot_build(p)$data[[1]]$fill)
  expect_setequal(fills, unname(wfm_colours(palette = "primary"))[1:2])
})
