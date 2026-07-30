test_that("bundled Quarto extension exists", {
  extension <- wfm_quarto_extension_dir()
  academic_extension <- wfm_quarto_extension_dir("academic")

  expect_true(dir.exists(extension))
  expect_true(dir.exists(academic_extension))
  expect_true(file.exists(file.path(extension, "_extension.yml")))
  expect_true(file.exists(file.path(academic_extension, "_extension.yml")))
  expect_true(file.exists(file.path(extension, "chart-dark-mode.html")))
  expect_true(file.exists(file.path(academic_extension, "chart-dark-mode.html")))
  expect_true(file.exists(file.path(extension, "_brand.yml")))
  expect_true(file.exists(file.path(academic_extension, "_brand.yml")))
  expect_true(file.exists(file.path(extension, "html.scss")))
  expect_true(file.exists(file.path(academic_extension, "html.scss")))
  expect_true(file.exists(file.path(extension, "revealjs.scss")))
  expect_true(file.exists(file.path(extension, "chart-reveal-layout.html")))
  expect_true(file.exists(file.path(extension, "section-slides.lua")))
  expect_true(file.exists(file.path(extension, "latex-header.tex")))
  expect_true(file.exists(file.path(academic_extension, "latex-header.tex")))
  expect_true(file.exists(file.path(extension, "fonts", "newsreader", "Newsreader-Variable.woff2")))
  expect_true(file.exists(file.path(extension, "fonts", "newsreader", "Newsreader-Italic-Variable.woff2")))
  expect_true(file.exists(file.path(extension, "fonts", "newsreader", "OFL.txt")))
  expect_true(file.exists(file.path(academic_extension, "fonts", "newsreader", "Newsreader-Variable.woff2")))

  extension_yml <- readLines(file.path(extension, "_extension.yml"), warn = FALSE)
  academic_yml <- readLines(file.path(academic_extension, "_extension.yml"), warn = FALSE)
  expect_true(sum(grepl("fig-format: svg", extension_yml, fixed = TRUE)) >= 2)
  expect_true(sum(grepl("fig-cap-location: top", extension_yml, fixed = TRUE)) >= 2)
  expect_true(sum(grepl("fig-cap-location: bottom", academic_yml, fixed = TRUE)) >= 2)
  expect_true(any(grepl("tbl-cap-location: top", academic_yml, fixed = TRUE)))
  expect_true(any(grepl("chart-dark-mode.html", extension_yml, fixed = TRUE)))
  expect_true(any(grepl("chart-dark-mode.html", academic_yml, fixed = TRUE)))
  expect_true(any(grepl("chart-reveal-layout.html", extension_yml, fixed = TRUE)))
  expect_true(any(grepl("section-slides.lua", extension_yml, fixed = TRUE)))
  expect_true(any(grepl("- brand", extension_yml, fixed = TRUE)))
  expect_true(any(grepl("- brand", academic_yml, fixed = TRUE)))
  expect_false(any(grepl("dark-mode-svg.html", extension_yml, fixed = TRUE)))
})

test_that("chart captions support nice and academic styles", {
  nice_caption <- chart_caption("Title", "Subtitle", style = "nice")
  expect_true(grepl("Title", nice_caption, fixed = TRUE))
  expect_true(grepl("Subtitle", nice_caption, fixed = TRUE))
  expect_true(grepl("\n", nice_caption, fixed = TRUE))

  expect_equal(
    chart_caption("Title", "Subtitle", style = "academic"),
    "Title. Subtitle"
  )
})

test_that("use_wfmr_doc creates a starter document and extension", {
  dir <- withr::local_tempdir()
  file <- file.path(dir, "analysis.qmd")

  out <- use_wfmr_doc(file, format = "html", title = "Analysis", open = FALSE)

  expect_equal(out, file)
  expect_true(file.exists(file))
  expect_true(file.exists(file.path(dir, "_extensions", "wfmr", "_extension.yml")))
  expect_true(file.exists(file.path(dir, "_extensions", "wfmr-academic", "_extension.yml")))

  qmd <- readLines(file, warn = FALSE)
  expect_true(any(grepl("brand: \"_extensions/wfmr/_brand.yml\"", qmd, fixed = TRUE)))
  expect_true(any(grepl("wfmr-html", qmd, fixed = TRUE)))
  expect_true(any(grepl("wfm_chart_defaults()", qmd, fixed = TRUE)))
})

test_that("use_wfmr_doc can create academic documents", {
  dir <- withr::local_tempdir()
  file <- use_wfmr_doc(
    file.path(dir, "paper.qmd"),
    format = "all",
    style = "academic",
    title = "Paper",
    open = FALSE
  )

  qmd <- readLines(file, warn = FALSE)
  expect_true(any(grepl("brand: \"_extensions/wfmr-academic/_brand.yml\"", qmd, fixed = TRUE)))
  expect_true(any(grepl("wfmr-academic-html", qmd, fixed = TRUE)))
  expect_true(any(grepl("wfmr-academic-pdf", qmd, fixed = TRUE)))
  expect_true(any(grepl("wfm_chart_defaults(style = \"academic\")", qmd, fixed = TRUE)))
})

test_that("use_wfmr_doc can request HTML serif typography", {
  dir <- withr::local_tempdir()
  file <- use_wfmr_doc(
    file.path(dir, "analysis.qmd"),
    format = "html",
    title = "Analysis",
    serif = TRUE,
    open = FALSE
  )

  qmd <- readLines(file, warn = FALSE)
  expect_true(any(grepl("mainfont:", qmd, fixed = TRUE)))
  expect_true(any(grepl("Newsreader", qmd, fixed = TRUE)))
})

test_that("use_wfmr_doc can create revealjs documents", {
  dir <- withr::local_tempdir()
  file <- use_wfmr_doc(
    file.path(dir, "slides.qmd"),
    format = "revealjs",
    open = FALSE
  )

  qmd <- readLines(file, warn = FALSE)
  expect_true(any(grepl("wfmr-revealjs", qmd, fixed = TRUE)))
  expect_true(any(grepl("## Overview", qmd, fixed = TRUE)))
  expect_true(any(grepl("## Results {.section-blue}", qmd, fixed = TRUE)))
})
