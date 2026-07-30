#' Locate the bundled wfmR Quarto extension
#'
#' @param style Extension style. `"nice"` locates the default `wfmr`
#'   extension; `"academic"` locates the `wfmr-academic` extension.
#'
#' @return Path to the installed Quarto extension directory.
#' @export
wfm_quarto_extension_dir <- function(style = c("nice", "academic")) {
  style <- match.arg(style)
  extension <- if (identical(style, "academic")) "wfmr-academic" else "wfmr"

  system.file(
    "quarto",
    "_extensions",
    extension,
    package = "wfmR",
    mustWork = TRUE
  )
}

wfm_quarto_extensions_dir <- function() {
  system.file("quarto", "_extensions", package = "wfmR", mustWork = TRUE)
}

#' Install the bundled wfmR Quarto extension into a project
#'
#' Copies the package's Quarto extension folders into a Quarto project or
#' document folder so documents can use `wfmr-html`, `wfmr-pdf`,
#' `wfmr-academic-html`, `wfmr-academic-pdf`, and `wfmr-revealjs`. The
#' extensions include Quarto `_brand.yml` files for the HTML and revealjs
#' colour system.
#'
#' @param path Target project directory.
#' @param overwrite If `TRUE`, replace an existing `_extensions/wfmr` folder.
#' @param style Extension style to install. `"all"` installs both the default
#'   and academic extension folders.
#'
#' @return Paths to the copied extension directories, invisibly.
#' @export
use_wfmr_extension <- function(path = ".",
                               overwrite = FALSE,
                               style = c("all", "nice", "academic")) {
  style <- match.arg(style)
  target_root <- normalizePath(path, mustWork = FALSE)
  extension_parent <- file.path(target_root, "_extensions")
  extension_names <- switch(
    style,
    all = c("wfmr", "wfmr-academic"),
    nice = "wfmr",
    academic = "wfmr-academic"
  )

  dir.create(extension_parent, recursive = TRUE, showWarnings = FALSE)
  targets <- file.path(extension_parent, extension_names)
  source_parent <- wfm_quarto_extensions_dir()

  for (i in seq_along(extension_names)) {
    target <- targets[[i]]
    if (dir.exists(target)) {
      if (!isTRUE(overwrite)) {
        next
      }
      unlink(target, recursive = TRUE, force = TRUE)
    }

    source <- file.path(source_parent, extension_names[[i]])
    ok <- file.copy(source, extension_parent, recursive = TRUE)

    if (!isTRUE(ok)) {
      stop("Could not copy the wfmR Quarto extension to ", target, call. = FALSE)
    }
  }

  invisible(targets)
}

#' Create a starter Quarto document using wfmR defaults
#'
#' This is the main usability helper. It creates a `.qmd` file and installs the
#' bundled Quarto extension beside it. Render the result with Quarto in the
#' usual way.
#'
#' @param path File path for the new `.qmd`, or a directory in which `index.qmd`
#'   should be created.
#' @param format One of `"html"`, `"pdf"`, `"revealjs"`, or `"all"`.
#' @param style Document style. `"nice"` uses `wfmr-html`/`wfmr-pdf`;
#'   `"academic"` uses `wfmr-academic-html`/`wfmr-academic-pdf`.
#' @param title Optional document title.
#' @param serif If `TRUE`, add an HTML `mainfont` setting so prose uses the
#'   bundled serif typography option.
#' @param overwrite If `TRUE`, replace an existing document and extension.
#' @param open If `TRUE`, open the created document in the default editor.
#'
#' @return Path to the created `.qmd`, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' use_wfmr_doc("analysis.qmd", format = "html")
#' use_wfmr_doc("serif-analysis.qmd", format = "html", serif = TRUE)
#' use_wfmr_doc("slides.qmd", format = "revealjs")
#' }
use_wfmr_doc <- function(path = "index.qmd",
                         format = c("html", "pdf", "revealjs", "all"),
                         style = c("nice", "academic"),
                         title = NULL,
                         serif = FALSE,
                         overwrite = FALSE,
                         open = interactive()) {
  format <- match.arg(format)
  style <- match.arg(style)
  path <- normalizePath(path, mustWork = FALSE)

  if (!grepl("[.]qmd$", path, ignore.case = TRUE)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    file <- file.path(path, "index.qmd")
  } else {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    file <- path
  }

  if (file.exists(file) && !isTRUE(overwrite)) {
    stop("File already exists: ", file, call. = FALSE)
  }

  if (is.null(title)) {
    title <- tools::file_path_sans_ext(basename(file))
    title <- gsub("[-_]+", " ", title)
  }

  use_wfmr_extension(dirname(file), overwrite = overwrite)
  writeLines(
    wfm_starter_qmd(title = title, format = format, style = style, serif = serif),
    file
  )

  if (isTRUE(open)) {
    utils::file.edit(file)
  }

  invisible(file)
}

wfm_html_serif_stack <- "Newsreader, Georgia, Cambria, Iowan Old Style, Palatino Linotype, Palatino, serif"

wfm_format_name <- function(format, style = c("nice", "academic")) {
  style <- match.arg(style)

  if (identical(format, "revealjs")) {
    return("wfmr-revealjs")
  }

  if (identical(style, "academic")) {
    paste0("wfmr-academic-", format)
  } else {
    paste0("wfmr-", format)
  }
}

wfm_chart_defaults_call <- function(style = c("nice", "academic")) {
  style <- match.arg(style)

  if (identical(style, "academic")) {
    "wfm_chart_defaults(style = \"academic\")"
  } else {
    "wfm_chart_defaults()"
  }
}

wfm_brand_path <- function(style = c("nice", "academic")) {
  style <- match.arg(style)
  extension <- if (identical(style, "academic")) "wfmr-academic" else "wfmr"
  paste0("_extensions/", extension, "/_brand.yml")
}

wfm_starter_qmd <- function(title, format, style = c("nice", "academic"), serif = FALSE) {
  style <- match.arg(style)
  html_format <- wfm_format_name("html", style)
  pdf_format <- wfm_format_name("pdf", style)

  format_lines <- switch(
    format,
    html = if (isTRUE(serif)) {
      c(
        "format:",
        paste0("  ", html_format, ":"),
        "    fig-format: svg",
        paste0("    mainfont: \"", wfm_html_serif_stack, "\""),
        "    linestretch: 1.58"
      )
    } else {
      c(
        "format:",
        paste0("  ", html_format, ":"),
        "    fig-format: svg"
      )
    },
    pdf = c("format:", paste0("  ", pdf_format, ": default")),
    revealjs = c("format:", "  wfmr-revealjs: default"),
    all = if (isTRUE(serif)) {
      c(
        "format:",
        paste0("  ", html_format, ":"),
        "    fig-format: svg",
        paste0("    mainfont: \"", wfm_html_serif_stack, "\""),
        "    linestretch: 1.58",
        paste0("  ", pdf_format, ": default"),
        "  wfmr-revealjs: default"
      )
    } else {
      c(
        "format:",
        paste0("  ", html_format, ":"),
        "    fig-format: svg",
        paste0("  ", pdf_format, ": default"),
        "  wfmr-revealjs: default"
      )
    }
  )

  body <- if (identical(format, "revealjs")) {
    c(
      "## Overview",
      "",
      "- One idea per slide.",
      "- Use `theme_wfm()` and `scale_colour_wfm()` for charts.",
      "",
      "## Results {.section-blue}",
      "",
      "A section divider with a built-in palette background.",
      "",
      "## Example chart",
      "",
      "```{r}",
      "#| label: fig-example",
      "#| chart-title: \"Example chart\"",
      "#| chart-subtitle: \"A small built-in dataset using wfmR defaults\"",
      "#| chart-notes: \"Bars use the wfmR primary palette.\"",
      "#| chart-source: \"R datasets package.\"",
      "mtcars |>",
      "  transform(cyl = factor(cyl)) |>",
      "  ggplot2::ggplot(ggplot2::aes(cyl, mpg, fill = cyl)) +",
      "  ggplot2::geom_boxplot(width = 0.6, alpha = 0.9) +",
      "  scale_fill_wfm() +",
      "  scale_y_wfm() +",
      "  ggplot2::labs(x = \"Cylinders\", y = NULL, subtitle = \"Miles per gallon\")",
      "```"
    )
  } else {
    c(
      "## Overview",
      "",
      "Start writing here.",
      "",
      "```{r}",
      "#| label: fig-example",
      "#| chart-title: \"Example chart\"",
      "#| chart-subtitle: \"A small built-in dataset using wfmR defaults\"",
      "#| chart-notes: \"Bars use the wfmR primary palette.\"",
      "#| chart-source: \"R datasets package.\"",
      "mtcars |>",
      "  transform(cyl = factor(cyl)) |>",
      "  ggplot2::ggplot(ggplot2::aes(cyl, mpg, fill = cyl)) +",
      "  ggplot2::geom_boxplot(width = 0.6, alpha = 0.9) +",
      "  scale_fill_wfm() +",
      "  scale_y_wfm() +",
      "  ggplot2::labs(x = \"Cylinders\", y = NULL, subtitle = \"Miles per gallon\")",
      "```"
    )
  }

  c(
	    "---",
	    paste0("title: ", wfm_yaml_string(title)),
	    "author: \"Will Mackey\"",
	    paste0("brand: ", wfm_yaml_string(wfm_brand_path(style))),
	    format_lines,
    "execute:",
    "  warning: false",
    "  message: false",
    "---",
    "",
    "```{r}",
    "#| label: setup",
    "#| include: false",
    "library(wfmR)",
    wfm_chart_defaults_call(style),
    "```",
    "",
    body
  )
}

wfm_yaml_string <- function(x) {
  paste0("\"", gsub("\"", "\\\\\"", x), "\"")
}
