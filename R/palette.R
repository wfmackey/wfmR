#' wfmR colours
#'
#' The default chart palette is the primary wfmR palette: a soft Rose
#' Pine-inspired sequence of salmon, teal, purple, moss, gold, blue,
#' and pink. The original Wong/Colour Universal Design palette remains
#' available as `palette = "wong"` or `palette = "colourblind"`. The
#' six-colour Minou palette is available as `palette = "minou"`. A single
#' switch, [wfm_colourblind()], substitutes the Wong palette wherever the
#' primary palette would be used.
#'
#' @param ... Optional colour names to select.
#' @param semantic If `TRUE`, return the four semantic colours used for
#'   definition tags: object, parameter, welfare, and method.
#' @param palette Colour set name. `"all"` returns the named package colours.
#'   Use [wfm_palette_options()] to list chart palettes.
#'
#' @return A named character vector of hex colours.
#' @export
#'
#' @examples
#' wfm_colours()
#' wfm_colours("teal", "salmon", "yellow")
#' wfm_colours("blue", "orange", palette = "wong")
#' wfm_colours("teal", "red", palette = "minou")
wfm_colours <- function(..., semantic = FALSE, palette = "all") {
  keys <- c(...)
  pal <- if (semantic) {
    wfm_semantic_colours
  } else if (identical(palette, "all")) {
    wfm_base_colours
  } else {
    wfm_get_discrete_palette(palette)
  }

  if (length(keys) == 0) {
    return(pal)
  }

  missing <- setdiff(keys, names(pal))
  if (length(missing) > 0) {
    stop(
      "Unknown colour name: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  pal[keys]
}

#' Return the wfmR chart palette
#'
#' @param n Number of colours to return. If `NULL`, returns the full palette.
#' @param reverse If `TRUE`, reverse the palette order.
#' @inheritParams wfm_colours
#'
#' @return A named character vector of hex colours.
#' @export
#'
#' @examples
#' wfm_palette(palette = "minou")
wfm_palette <- function(n = NULL, reverse = FALSE, palette = "primary") {
  pal <- wfm_get_discrete_palette(palette)
  if (isTRUE(reverse)) {
    pal <- rev(pal)
  }

  if (is.null(n)) {
    return(pal)
  }

  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 1) {
    stop("`n` must be a positive whole number.", call. = FALSE)
  }

  n <- as.integer(n)
  if (n > length(pal)) {
    warning(
      "The selected wfmR palette has ",
      length(pal),
      " colours; colours will be recycled.",
      call. = FALSE
    )
  }

  stats::setNames(rep_len(unname(pal), n), rep_len(names(pal), n))
}

#' Switch the chart palette to the colourblind-safe palette
#'
#' One switch for colourblind-safe output. While it is on, every request
#' for the primary chart palette -- including the defaults of
#' [wfm_palette()], [scale_colour_wfm()], and [scale_fill_wfm()] --
#' returns the Wong/Colour Universal Design palette instead. One line in
#' a setup chunk therefore converts a whole document.
#'
#' The switch stores its state in the `wfmR.colourblind` option, so
#' `options(wfmR.colourblind = TRUE)` in a script or `.Rprofile` does the
#' same thing. Because the wfm scales resolve their palette when a chart
#' is drawn, the switch also applies to plot objects built before it was
#' turned on.
#'
#' Palettes requested by another name (`"wong"`, `"colourblind"`,
#' `"minou"`) are returned as asked. The named colour lookup
#' [wfm_colours()], the exported colour constants, and the sequential and
#' diverging scales are unchanged; for a diverging scale under
#' colour-vision deficiency, prefer `palette = "green_pink"`.
#'
#' @param on If `TRUE`, use the Wong palette wherever the primary palette
#'   would be used. If `FALSE`, restore the primary palette.
#'
#' @return The previous value of the switch, invisibly.
#' @export
#'
#' @examples
#' wfm_colourblind()
#' wfm_palette()
#' wfm_colourblind(FALSE)
#' wfm_palette()
wfm_colourblind <- function(on = TRUE) {
  if (!is.logical(on) || length(on) != 1 || is.na(on)) {
    stop("`on` must be TRUE or FALSE.", call. = FALSE)
  }

  old <- isTRUE(getOption("wfmR.colourblind"))
  options(wfmR.colourblind = on)
  invisible(old)
}

#' @rdname wfm_colourblind
#' @export
wfm_colorblind <- wfm_colourblind

#' List available wfmR palettes
#'
#' @param type Palette family to list.
#'
#' @return A data frame with palette names, types, generators, and notes.
#' @export
wfm_palette_options <- function(type = c("all", "discrete", "sequential", "diverging")) {
  type <- match.arg(type)
  wanted <- if (identical(type, "all")) {
    c("discrete", "sequential", "diverging")
  } else {
    type
  }

  rows <- list()

  if ("discrete" %in% wanted) {
    rows <- c(
      rows,
      lapply(names(wfm_discrete_palettes), function(name) {
        spec <- wfm_discrete_palettes[[name]]
        data.frame(
          type = "discrete",
          name = name,
          generator = "fixed",
          colours = paste(unname(spec$colours), collapse = ", "),
          colour_count = length(spec$colours),
          notes = spec$notes,
          stringsAsFactors = FALSE
        )
      })
    )
  }

  if ("sequential" %in% wanted) {
    rows <- c(rows, wfm_palette_option_rows(wfm_sequential_palettes, "sequential"))
  }

  if ("diverging" %in% wanted) {
    rows <- c(rows, wfm_palette_option_rows(wfm_diverging_palettes, "diverging"))
  }

  do.call(rbind, rows)
}

#' Tint colours towards a background
#'
#' @param colour A character vector of colours.
#' @param amount Numeric tint amount between 0 and 1. `0` returns the original
#'   colour and `1` returns the background.
#' @param background Background colour to tint towards.
#'
#' @return A character vector of hex colours.
#' @export
#'
#' @examples
#' wfm_tint("#286983", amount = 0.85)
wfm_tint <- function(colour, amount = 0.85, background = "#FFFFFF") {
  if (!is.numeric(amount) || anyNA(amount) || any(amount < 0 | amount > 1)) {
    stop("`amount` must be between 0 and 1.", call. = FALSE)
  }

  colour_rgb <- grDevices::col2rgb(colour)
  background_rgb <- grDevices::col2rgb(background)

  if (ncol(background_rgb) == 1L && ncol(colour_rgb) > 1L) {
    background_rgb <- background_rgb[, rep(1L, ncol(colour_rgb)), drop = FALSE]
  }

  if (ncol(background_rgb) != ncol(colour_rgb)) {
    stop("`background` must have length 1 or match `colour`.", call. = FALSE)
  }

  amount <- rep_len(amount, ncol(colour_rgb))
  mixed <- sweep(colour_rgb, 2L, 1 - amount, `*`) +
    sweep(background_rgb, 2L, amount, `*`)

  out <- grDevices::rgb(
    mixed[1L, ],
    mixed[2L, ],
    mixed[3L, ],
    maxColorValue = 255
  )
  names(out) <- names(colour)
  out
}

#' Generate named tints for a palette
#'
#' @param colours A character vector of colours, or names from [wfm_colours()].
#' @param strength Numeric vector of original-colour strength percentages.
#'   `90` means 90% original colour and 10% background; `10` is very pale.
#' @param amount Deprecated compatibility argument: numeric vector of tint
#'   amounts between 0 and 1, where `1` is the background.
#' @param background Background colour to tint towards.
#'
#' @return A named character vector of tinted hex colours.
#' @export
wfm_tints <- function(colours = wfm_colours(),
                      strength = seq(90, 10, by = -10),
                      amount = NULL,
                      background = "#FFFFFF") {
  if (is.character(colours) && all(colours %in% names(wfm_base_colours))) {
    colours <- wfm_colours(colours)
  }

  if (!is.null(amount)) {
    if (!is.numeric(amount) || anyNA(amount) || any(amount < 0 | amount > 1)) {
      stop("`amount` must be between 0 and 1.", call. = FALSE)
    }
    strength <- round((1 - amount) * 100)
  }

  if (!is.numeric(strength) || anyNA(strength) || any(strength < 0 | strength > 100)) {
    stop("`strength` must be between 0 and 100.", call. = FALSE)
  }

  if (is.null(names(colours)) || any(names(colours) == "")) {
    names(colours) <- paste0("colour_", seq_along(colours))
  }

  amount <- 1 - strength / 100

  out <- unlist(
    lapply(amount, function(x) wfm_tint(colours, amount = x, background = background)),
    use.names = FALSE
  )

  tint_names <- unlist(
    lapply(strength, function(x) paste0(names(colours), x)),
    use.names = FALSE
  )
  stats::setNames(out, tint_names)
}

#' Semantic definition colours
#'
#' @return A data frame with semantic keys, base colours, text colours, and
#'   subtle background tints.
#' @export
wfm_definition_colours <- function() {
  data.frame(
    name = names(wfm_semantic_colours),
    colour = unname(wfm_semantic_colours),
    tint = unname(wfm_semantic_tints),
    text = unname(wfm_semantic_text),
    stringsAsFactors = FALSE
  )
}

#' Plot the wfmR palette and tints
#'
#' @param include_tints If `TRUE`, include tinted versions of each base colour.
#' @param amount Tint strengths to display when `include_tints = TRUE`.
#'
#' @return A ggplot object.
#' @export
wfm_colour_demo <- function(include_tints = TRUE, amount = c(80, 60, 40)) {
  base <- wfm_colours()
  strengths <- if (isTRUE(include_tints)) c(100, amount) else 100
  dodge_width <- if (isTRUE(include_tints)) 0.9 else 0.72
  bar_width <- if (isTRUE(include_tints)) 0.22 else 0.72

  dat <- do.call(
    rbind,
    lapply(names(base), function(colour) {
      values <- c(base[colour], wfm_tints(base[colour], strength = strengths[-1]))
      names(values) <- paste0(colour, ifelse(strengths == 100, "", strengths))

      data.frame(
        colour = tools::toTitleCase(colour),
        strength = factor(strengths, levels = strengths),
        value = unname(values),
        stringsAsFactors = FALSE
      )
    })
  )
  dat$colour <- factor(dat$colour, levels = tools::toTitleCase(names(base)))

  ggplot2::ggplot(
    dat,
    ggplot2::aes(
      x = .data$colour,
      y = 1,
      fill = .data$value,
      group = .data$strength
    )
  ) +
    ggplot2::geom_col(
      position = ggplot2::position_dodge(width = dodge_width),
      width = bar_width
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::labs(x = NULL, y = NULL) +
    theme_wfm(base_size = 11) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
}

#' Colour distances under colour-vision-deficiency simulation
#'
#' @param colours Character vector of colours.
#' @param cvd Colour vision mode: none, deutan, protan, or tritan.
#' @param severity Integer severity from 0 to 10.
#'
#' @return A square matrix of CIEDE2000 colour distances.
#' @export
wfm_colour_distances <- function(colours = wfm_palette(),
                                 cvd = c("none", "deutan", "protan", "tritan"),
                                 severity = 10) {
  cvd <- match.arg(cvd)
  if (is.null(names(colours)) || any(names(colours) == "")) {
    names(colours) <- paste0("colour_", seq_along(colours))
  }

  severity <- as.integer(severity)
  if (!severity %in% 0:10) {
    stop("`severity` must be an integer from 0 to 10.", call. = FALSE)
  }

  simulated <- switch(
    cvd,
    none = colours,
    deutan = colorspace::simulate_cvd(
      colours,
      colorspace::deutanomaly_cvd[[as.character(severity)]]
    ),
    protan = colorspace::simulate_cvd(
      colours,
      colorspace::protanomaly_cvd[[as.character(severity)]]
    ),
    tritan = colorspace::simulate_cvd(
      colours,
      colorspace::tritanomaly_cvd[[as.character(severity)]]
    )
  )

  lab <- farver::decode_colour(simulated, to = "lab")
  distances <- farver::compare_colour(
    lab,
    lab,
    from_space = "lab",
    method = "CIE2000"
  )
  dimnames(distances) <- list(names(colours), names(colours))
  distances
}

#' Check colour-blind distinguishability
#'
#' @param colours Character vector of colours.
#' @param min_distance Minimum CIEDE2000 distance required for each pair.
#' @param severity Integer severity from 0 to 10.
#'
#' @return A data frame with the minimum pairwise distance for each simulated
#'   vision mode.
#' @export
wfm_colour_blind_report <- function(colours = wfm_palette(),
                                    min_distance = 8,
                                    severity = 10) {
  modes <- c("none", "deutan", "protan", "tritan")
  rows <- lapply(modes, function(mode) {
    distances <- wfm_colour_distances(colours, cvd = mode, severity = severity)
    distances[lower.tri(distances, diag = TRUE)] <- NA_real_
    min_value <- min(distances, na.rm = TRUE)
    index <- which(distances == min_value, arr.ind = TRUE)[1L, ]

    data.frame(
      cvd = mode,
      min_distance = unname(min_value),
      colour_1 = rownames(distances)[index[1L]],
      colour_2 = colnames(distances)[index[2L]],
      passes = unname(min_value) >= min_distance,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

#' @rdname wfm_colour_blind_report
#' @return `wfm_is_colour_blind_safe()` returns a single logical value.
#' @export
wfm_is_colour_blind_safe <- function(colours = wfm_palette(),
                                     min_distance = 8,
                                     severity = 10) {
  all(wfm_colour_blind_report(colours, min_distance, severity)$passes)
}

#' Generate a continuous wfmR colour ramp
#'
#' @param colours Character vector of colours to interpolate through.
#' @param n Number of colours to return.
#' @param space Interpolation space. `OKLab` is the newer perceptual
#'   interpolation option. `Lab` uses the same CIELAB family as ggplot2 and
#'   scales continuous gradients.
#' @param reverse If `TRUE`, reverse the output ramp.
#'
#' @return A character vector of hex colours.
#' @export
wfm_colour_ramp <- function(colours,
                            n = 256,
                            space = c("OKLab", "Lab"),
                            reverse = FALSE) {
  space <- match.arg(space)
  n <- wfm_check_n(n)
  colours <- wfm_check_colours(colours)

  out <- switch(
    space,
    Lab = scales::colour_ramp(unname(colours))(seq(0, 1, length.out = n)),
    OKLab = wfm_oklab_ramp(unname(colours), n)
  )

  if (isTRUE(reverse)) {
    out <- rev(out)
  }

  out
}

#' Generate a wfmR sequential palette
#'
#' @param name Sequential palette name. Use [wfm_palette_options()] to list
#'   available palettes.
#' @param n Number of colours to return.
#' @param reverse If `TRUE`, reverse the palette.
#'
#' @return A character vector of hex colours.
#' @export
wfm_sequential_palette <- function(name = "blue_teal_yellow", n = 256, reverse = FALSE) {
  wfm_continuous_palette(name, n, reverse, wfm_sequential_palettes, "sequential")
}

#' Generate a wfmR diverging palette
#'
#' @inheritParams wfm_sequential_palette
#'
#' @return A character vector of hex colours.
#' @export
wfm_diverging_palette <- function(name = "teal_salmon", n = 256, reverse = FALSE) {
  wfm_continuous_palette(name, n, reverse, wfm_diverging_palettes, "diverging")
}

#' wfmR default colour constants
#'
#' @format Character scalars containing hex colours.
#'
#' @name wfm_colour_constants
#'
#' @examples
#' wfm_teal
#' wfm_teal90
NULL

#' @rdname wfm_colour_constants
#' @export
wfm_teal <- "#56949F"

#' @rdname wfm_colour_constants
#' @export
wfm_salmon <- "#D7827E"

#' @rdname wfm_colour_constants
#' @export
wfm_yellow <- "#EA9D34"

#' @rdname wfm_colour_constants
#' @export
wfm_pink <- "#B4637A"

#' @rdname wfm_colour_constants
#' @export
wfm_lime <- "#8B9A5A"

#' @rdname wfm_colour_constants
#' @export
wfm_blue <- "#286983"

#' @rdname wfm_colour_constants
#' @export
wfm_green <- "#6F8F72"

#' @rdname wfm_colour_constants
#' @export
wfm_purple <- "#907AA9"

#' @rdname wfm_colour_constants
#' @export
wfm_teal90 <- unname(wfm_tint(wfm_teal, amount = 0.10))

#' @rdname wfm_colour_constants
#' @export
wfm_teal80 <- unname(wfm_tint(wfm_teal, amount = 0.20))

#' @rdname wfm_colour_constants
#' @export
wfm_teal70 <- unname(wfm_tint(wfm_teal, amount = 0.30))

#' @rdname wfm_colour_constants
#' @export
wfm_teal60 <- unname(wfm_tint(wfm_teal, amount = 0.40))

#' @rdname wfm_colour_constants
#' @export
wfm_teal50 <- unname(wfm_tint(wfm_teal, amount = 0.50))

#' @rdname wfm_colour_constants
#' @export
wfm_teal40 <- unname(wfm_tint(wfm_teal, amount = 0.60))

#' @rdname wfm_colour_constants
#' @export
wfm_teal30 <- unname(wfm_tint(wfm_teal, amount = 0.70))

#' @rdname wfm_colour_constants
#' @export
wfm_teal20 <- unname(wfm_tint(wfm_teal, amount = 0.80))

#' @rdname wfm_colour_constants
#' @export
wfm_teal10 <- unname(wfm_tint(wfm_teal, amount = 0.90))

#' @rdname wfm_colour_constants
#' @export
wfm_salmon90 <- unname(wfm_tint(wfm_salmon, amount = 0.10))

#' @rdname wfm_colour_constants
#' @export
wfm_salmon80 <- unname(wfm_tint(wfm_salmon, amount = 0.20))

#' @rdname wfm_colour_constants
#' @export
wfm_salmon70 <- unname(wfm_tint(wfm_salmon, amount = 0.30))

#' @rdname wfm_colour_constants
#' @export
wfm_salmon60 <- unname(wfm_tint(wfm_salmon, amount = 0.40))

#' @rdname wfm_colour_constants
#' @export
wfm_salmon50 <- unname(wfm_tint(wfm_salmon, amount = 0.50))

#' @rdname wfm_colour_constants
#' @export
wfm_salmon40 <- unname(wfm_tint(wfm_salmon, amount = 0.60))

#' @rdname wfm_colour_constants
#' @export
wfm_salmon30 <- unname(wfm_tint(wfm_salmon, amount = 0.70))

#' @rdname wfm_colour_constants
#' @export
wfm_salmon20 <- unname(wfm_tint(wfm_salmon, amount = 0.80))

#' @rdname wfm_colour_constants
#' @export
wfm_salmon10 <- unname(wfm_tint(wfm_salmon, amount = 0.90))

#' @rdname wfm_colour_constants
#' @export
wfm_yellow90 <- unname(wfm_tint(wfm_yellow, amount = 0.10))

#' @rdname wfm_colour_constants
#' @export
wfm_yellow80 <- unname(wfm_tint(wfm_yellow, amount = 0.20))

#' @rdname wfm_colour_constants
#' @export
wfm_yellow70 <- unname(wfm_tint(wfm_yellow, amount = 0.30))

#' @rdname wfm_colour_constants
#' @export
wfm_yellow60 <- unname(wfm_tint(wfm_yellow, amount = 0.40))

#' @rdname wfm_colour_constants
#' @export
wfm_yellow50 <- unname(wfm_tint(wfm_yellow, amount = 0.50))

#' @rdname wfm_colour_constants
#' @export
wfm_yellow40 <- unname(wfm_tint(wfm_yellow, amount = 0.60))

#' @rdname wfm_colour_constants
#' @export
wfm_yellow30 <- unname(wfm_tint(wfm_yellow, amount = 0.70))

#' @rdname wfm_colour_constants
#' @export
wfm_yellow20 <- unname(wfm_tint(wfm_yellow, amount = 0.80))

#' @rdname wfm_colour_constants
#' @export
wfm_yellow10 <- unname(wfm_tint(wfm_yellow, amount = 0.90))

#' @rdname wfm_colour_constants
#' @export
wfm_pink90 <- unname(wfm_tint(wfm_pink, amount = 0.10))

#' @rdname wfm_colour_constants
#' @export
wfm_pink80 <- unname(wfm_tint(wfm_pink, amount = 0.20))

#' @rdname wfm_colour_constants
#' @export
wfm_pink70 <- unname(wfm_tint(wfm_pink, amount = 0.30))

#' @rdname wfm_colour_constants
#' @export
wfm_pink60 <- unname(wfm_tint(wfm_pink, amount = 0.40))

#' @rdname wfm_colour_constants
#' @export
wfm_pink50 <- unname(wfm_tint(wfm_pink, amount = 0.50))

#' @rdname wfm_colour_constants
#' @export
wfm_pink40 <- unname(wfm_tint(wfm_pink, amount = 0.60))

#' @rdname wfm_colour_constants
#' @export
wfm_pink30 <- unname(wfm_tint(wfm_pink, amount = 0.70))

#' @rdname wfm_colour_constants
#' @export
wfm_pink20 <- unname(wfm_tint(wfm_pink, amount = 0.80))

#' @rdname wfm_colour_constants
#' @export
wfm_pink10 <- unname(wfm_tint(wfm_pink, amount = 0.90))

#' @rdname wfm_colour_constants
#' @export
wfm_lime90 <- unname(wfm_tint(wfm_lime, amount = 0.10))

#' @rdname wfm_colour_constants
#' @export
wfm_lime80 <- unname(wfm_tint(wfm_lime, amount = 0.20))

#' @rdname wfm_colour_constants
#' @export
wfm_lime70 <- unname(wfm_tint(wfm_lime, amount = 0.30))

#' @rdname wfm_colour_constants
#' @export
wfm_lime60 <- unname(wfm_tint(wfm_lime, amount = 0.40))

#' @rdname wfm_colour_constants
#' @export
wfm_lime50 <- unname(wfm_tint(wfm_lime, amount = 0.50))

#' @rdname wfm_colour_constants
#' @export
wfm_lime40 <- unname(wfm_tint(wfm_lime, amount = 0.60))

#' @rdname wfm_colour_constants
#' @export
wfm_lime30 <- unname(wfm_tint(wfm_lime, amount = 0.70))

#' @rdname wfm_colour_constants
#' @export
wfm_lime20 <- unname(wfm_tint(wfm_lime, amount = 0.80))

#' @rdname wfm_colour_constants
#' @export
wfm_lime10 <- unname(wfm_tint(wfm_lime, amount = 0.90))

#' @rdname wfm_colour_constants
#' @export
wfm_blue90 <- unname(wfm_tint(wfm_blue, amount = 0.10))

#' @rdname wfm_colour_constants
#' @export
wfm_blue80 <- unname(wfm_tint(wfm_blue, amount = 0.20))

#' @rdname wfm_colour_constants
#' @export
wfm_blue70 <- unname(wfm_tint(wfm_blue, amount = 0.30))

#' @rdname wfm_colour_constants
#' @export
wfm_blue60 <- unname(wfm_tint(wfm_blue, amount = 0.40))

#' @rdname wfm_colour_constants
#' @export
wfm_blue50 <- unname(wfm_tint(wfm_blue, amount = 0.50))

#' @rdname wfm_colour_constants
#' @export
wfm_blue40 <- unname(wfm_tint(wfm_blue, amount = 0.60))

#' @rdname wfm_colour_constants
#' @export
wfm_blue30 <- unname(wfm_tint(wfm_blue, amount = 0.70))

#' @rdname wfm_colour_constants
#' @export
wfm_blue20 <- unname(wfm_tint(wfm_blue, amount = 0.80))

#' @rdname wfm_colour_constants
#' @export
wfm_blue10 <- unname(wfm_tint(wfm_blue, amount = 0.90))

#' @rdname wfm_colour_constants
#' @export
wfm_green90 <- unname(wfm_tint(wfm_green, amount = 0.10))

#' @rdname wfm_colour_constants
#' @export
wfm_green80 <- unname(wfm_tint(wfm_green, amount = 0.20))

#' @rdname wfm_colour_constants
#' @export
wfm_green70 <- unname(wfm_tint(wfm_green, amount = 0.30))

#' @rdname wfm_colour_constants
#' @export
wfm_green60 <- unname(wfm_tint(wfm_green, amount = 0.40))

#' @rdname wfm_colour_constants
#' @export
wfm_green50 <- unname(wfm_tint(wfm_green, amount = 0.50))

#' @rdname wfm_colour_constants
#' @export
wfm_green40 <- unname(wfm_tint(wfm_green, amount = 0.60))

#' @rdname wfm_colour_constants
#' @export
wfm_green30 <- unname(wfm_tint(wfm_green, amount = 0.70))

#' @rdname wfm_colour_constants
#' @export
wfm_green20 <- unname(wfm_tint(wfm_green, amount = 0.80))

#' @rdname wfm_colour_constants
#' @export
wfm_green10 <- unname(wfm_tint(wfm_green, amount = 0.90))

#' @rdname wfm_colour_constants
#' @export
wfm_purple90 <- unname(wfm_tint(wfm_purple, amount = 0.10))

#' @rdname wfm_colour_constants
#' @export
wfm_purple80 <- unname(wfm_tint(wfm_purple, amount = 0.20))

#' @rdname wfm_colour_constants
#' @export
wfm_purple70 <- unname(wfm_tint(wfm_purple, amount = 0.30))

#' @rdname wfm_colour_constants
#' @export
wfm_purple60 <- unname(wfm_tint(wfm_purple, amount = 0.40))

#' @rdname wfm_colour_constants
#' @export
wfm_purple50 <- unname(wfm_tint(wfm_purple, amount = 0.50))

#' @rdname wfm_colour_constants
#' @export
wfm_purple40 <- unname(wfm_tint(wfm_purple, amount = 0.60))

#' @rdname wfm_colour_constants
#' @export
wfm_purple30 <- unname(wfm_tint(wfm_purple, amount = 0.70))

#' @rdname wfm_colour_constants
#' @export
wfm_purple20 <- unname(wfm_tint(wfm_purple, amount = 0.80))

#' @rdname wfm_colour_constants
#' @export
wfm_purple10 <- unname(wfm_tint(wfm_purple, amount = 0.90))

wfm_base_colours <- c(
  salmon = wfm_salmon,
  teal = wfm_teal,
  purple = wfm_purple,
  lime = wfm_lime,
  yellow = wfm_yellow,
  blue = wfm_blue,
  pink = wfm_pink
)

wfm_chart_colours <- wfm_base_colours

wfm_wong_colours <- c(
  pink = "#CC79A7",
  sky = "#56B4E9",
  orange = "#D55E00",
  blue = "#0072B2",
  green = "#009E73",
  gold = "#E69F00",
  brown = "#7A5A00",
  black = "#1A1A1A"
)

wfm_minou_colours <- c(
  teal = "#00798C",
  red = "#D1495B",
  yellow = "#EDAE49",
  green = "#66A182",
  navy = "#2E4057",
  grey = "#8D96A3"
)

wfm_discrete_palettes <- list(
  primary = list(
    colours = wfm_chart_colours,
    notes = "Default soft Rose Pine-inspired wfmR chart sequence."
  ),
  wong = list(
    colours = wfm_wong_colours,
    notes = "Original Wong/Colour Universal Design palette."
  ),
  colourblind = list(
    colours = wfm_wong_colours,
    notes = "Colour-blind-friendly publication backup based on the Wong palette."
  ),
  minou = list(
    colours = wfm_minou_colours,
    notes = "Six-colour Minou categorical palette."
  )
)

wfm_sequential_palettes <- list(
  blue_teal_yellow = list(
    generator = "anchors",
    colours = c(wfm_blue, wfm_teal, wfm_yellow10),
    space = "OKLab",
    notes = "Default blue-to-teal-to-pale-yellow sequential scale."
  ),
  blue_green_yellow = list(
    generator = "anchors",
    colours = c(wfm_blue, wfm_green, wfm_lime, wfm_yellow10),
    space = "OKLab",
    notes = "Blue-to-green-to-pale-yellow scale for ordered intensity."
  ),
  purple_pink_salmon = list(
    generator = "anchors",
    colours = c(wfm_purple, wfm_pink, wfm_salmon, wfm_yellow10),
    space = "OKLab",
    notes = "Purple-to-pink-to-salmon scale with a pale warm endpoint."
  ),
  teal_lime_yellow = list(
    generator = "anchors",
    colours = c(wfm_teal, wfm_green, wfm_lime, wfm_yellow10),
    space = "OKLab",
    notes = "Teal-to-lime-to-pale-yellow scale for positive quantities."
  ),
  blue_purple_pink = list(
    generator = "anchors",
    colours = c(wfm_blue, wfm_purple, wfm_pink, wfm_salmon10),
    space = "OKLab",
    notes = "Blue-to-purple-to-pink scale for dense heatmaps."
  )
)

wfm_diverging_palettes <- list(
  teal_salmon = list(
    generator = "anchors",
    colours = c(wfm_teal, wfm_teal20, "#F7F7F2", wfm_salmon20, wfm_salmon),
    space = "OKLab",
    balanced = TRUE,
    notes = "Default teal-to-salmon diverging scale."
  ),
  blue_yellow = list(
    generator = "anchors",
    colours = c(wfm_blue, wfm_blue20, "#F7F7F2", wfm_yellow20, wfm_yellow),
    space = "OKLab",
    balanced = TRUE,
    notes = "Blue-to-yellow diverging scale with high endpoint separation."
  ),
  green_pink = list(
    generator = "anchors",
    colours = c(wfm_green, wfm_green20, "#F7F7F2", wfm_pink20, wfm_pink),
    space = "OKLab",
    balanced = TRUE,
    notes = "Green-to-pink diverging scale; clearer than teal/pink under CVD simulation."
  ),
  purple_lime = list(
    generator = "anchors",
    colours = c(wfm_purple, wfm_purple20, "#F7F7F2", wfm_lime20, wfm_lime),
    space = "OKLab",
    balanced = TRUE,
    notes = "Purple-to-lime diverging scale."
  ),
  blue_salmon = list(
    generator = "anchors",
    colours = c(wfm_blue, wfm_blue20, "#F7F7F2", wfm_salmon20, wfm_salmon),
    space = "OKLab",
    balanced = TRUE,
    notes = "Blue-to-salmon diverging scale."
  )
)

wfm_semantic_colours <- c(
  object = wfm_blue,
  parameter = wfm_salmon,
  welfare = wfm_green,
  method = wfm_purple
)

wfm_semantic_tints <- c(
  object = wfm_blue10,
  parameter = wfm_salmon10,
  welfare = wfm_green10,
  method = wfm_purple10
)

wfm_semantic_text <- c(
  object = wfm_blue,
  parameter = wfm_salmon,
  welfare = wfm_green,
  method = wfm_purple
)

wfm_get_discrete_palette <- function(palette = "primary") {
  palette <- wfm_match_palette_name(palette, names(wfm_discrete_palettes), "discrete")
  if (identical(palette, "primary") && isTRUE(getOption("wfmR.colourblind"))) {
    palette <- "colourblind"
  }
  wfm_discrete_palettes[[palette]]$colours
}

wfm_match_palette_name <- function(name, choices, type) {
  if (identical(name, "base") && "primary" %in% choices) {
    name <- "primary"
  }

  if (!is.character(name) || length(name) != 1 || is.na(name)) {
    stop("`name` must be a single palette name.", call. = FALSE)
  }

  aliases <- c(
	    sunset = "primary",
	    sunset_ruby = "primary",
	    rosepine = "primary",
	    rose_pine = "primary",
	    publication = "colourblind",
	    colorblind = "colourblind",
	    colour_blind = "colourblind",
	    color_blind = "colourblind",
	    blue = "blue_teal_yellow",
    dusk = "blue_teal_yellow",
    fire = "purple_pink_salmon",
    rose_heat = "purple_pink_salmon",
    yellow_light = "blue_teal_yellow",
    amber_light = "blue_teal_yellow",
    orange_light = "blue_teal_yellow",
    purple_pink = "green_pink",
    darkblue_pink = "blue_salmon",
    lightblue_orange = "teal_salmon",
    lightblue_pink = "green_pink",
    darkblue_yellow = "blue_yellow",
    sunset_magma = "teal_salmon",
    violet_ruby = "green_pink",
    ink_ruby = "blue_salmon",
    blue_coral = "teal_salmon",
    sky_brickrose = "green_pink",
    ink_amber = "blue_yellow",
    darkblue_orange = "blue_yellow"
  )
  if (name %in% names(aliases) && aliases[[name]] %in% choices) {
    name <- aliases[[name]]
  }

  matched <- pmatch(name, choices, duplicates.ok = FALSE)
  if (is.na(matched)) {
    stop(
      "Unknown ",
      type,
      " palette: ",
      name,
      ". Available palettes are: ",
      paste(choices, collapse = ", "),
      call. = FALSE
    )
  }

  choices[[matched]]
}

wfm_palette_option_rows <- function(specs, type) {
  lapply(names(specs), function(name) {
    spec <- specs[[name]]
    data.frame(
      type = type,
      name = name,
      generator = spec$generator,
      colours = paste(unname(spec$colours), collapse = ", "),
      colour_count = length(spec$colours),
      notes = spec$notes,
      stringsAsFactors = FALSE
    )
  })
}

wfm_continuous_palette <- function(name, n, reverse, specs, type) {
  name <- wfm_match_palette_name(name, names(specs), type)
  n <- wfm_check_n(n)
  spec <- specs[[name]]

  if (identical(spec$generator, "hcl") && identical(type, "sequential")) {
    out <- do.call(colorspace::sequential_hcl, c(list(n = n), spec$args))
  } else if (identical(spec$generator, "hcl") && identical(type, "diverging")) {
    out <- do.call(colorspace::diverging_hcl, c(list(n = n), spec$args))
  } else if (identical(spec$generator, "anchors")) {
    out <- wfm_colour_ramp(spec$colours, n = n, space = spec$space)
  } else {
    stop("Unknown palette generator: ", spec$generator, call. = FALSE)
  }

  if (isTRUE(reverse)) {
    out <- rev(out)
  }

  out
}

wfm_check_n <- function(n) {
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 1) {
    stop("`n` must be a positive whole number.", call. = FALSE)
  }

  as.integer(n)
}

wfm_check_colours <- function(colours) {
  if (!is.character(colours) || length(colours) < 2) {
    stop("`colours` must be a character vector with at least two colours.", call. = FALSE)
  }

  grDevices::col2rgb(colours)
  colours
}

wfm_oklab_ramp <- function(colours, n) {
  rgb <- farver::decode_colour(colours, to = "rgb")
  oklab <- farver::convert_colour(rgb, from = "rgb", to = "oklab")
  x <- seq(0, 1, length.out = nrow(oklab))
  xout <- seq(0, 1, length.out = n)

  interpolated <- vapply(
    seq_len(ncol(oklab)),
    function(index) stats::approx(x, oklab[, index], xout = xout)$y,
    numeric(n)
  )

  rgb_out <- farver::convert_colour(interpolated, from = "oklab", to = "rgb")
  rgb_out <- pmin(pmax(rgb_out, 0), 255)
  farver::encode_colour(rgb_out, from = "rgb")
}
