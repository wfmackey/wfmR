#' Export the source data used by a plot
#'
#' Select the source columns referenced by a plot's aesthetics and facets and
#' write them to a UTF-8 CSV file. This includes grouping, weights, labels and
#' interval limits, as well as `x` and `y`.
#'
#' @param plot A single ggplot object. Patchwork compositions are not supported;
#'   export their component plots separately.
#' @param path Output path including the file stem, for example
#'   `"figures/cars"` or `"figures/cars.csv"`. Any final suffix is replaced
#'   with `.csv`. Missing directories are created.
#'
#' @details
#' The export contains inputs to the plot's statistics, with original column
#' names and duplicate observations preserved. For example, a histogram exports
#' its observations, not its bin counts. Transformations inside `aes()` export
#' their source columns. `after_stat()` and `after_scale()` variables are not
#' source columns. Missing observations and observations removed by scales or
#' coordinates are retained because they can affect statistics.
#'
#' Layer mappings respect `inherit.aes` and fixed aesthetic overrides. Layer
#' data functions and formulas are evaluated before selecting columns, so their
#' returned data are exported. If all layers use the same data frame, its used
#' columns are exported once. Otherwise, each layer has its own rows, with
#' `.wfm_layer` identifying the layer number and `.wfm_row` identifying its
#' original row. Missing columns are filled with `NA`. Layer `0` contains
#' plot-level facet data when those data differ from every layer's data. A plot
#' with only constant mappings uses `.wfm_row` to preserve observation counts.
#'
#' Keep the plotting code alongside the CSV: it contains transformations,
#' statistics, scales, fixed annotations and other settings. CSV does not retain
#' factor levels, ordering or R column classes. Environment scalars (for example
#' `.env$offset`) also remain in the plotting code. External vectors, list or
#' matrix columns, dynamic lookups such as `get()`, and whole-data expressions
#' such as `helper(.data)` are rejected. Vector-valued fixed aesthetics and
#' injected vectors in `aes()` are also rejected. Put their results in mapped
#' columns first. Dependencies hidden inside arbitrary helper functions cannot
#' be inferred; helper functions should take their data columns explicitly.
#'
#' @return The CSV filename, invisibly.
#' @export
#'
#' @examples
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, colour = factor(cyl))) +
#'   ggplot2::geom_point()
#' wfm_export_plot_data(p, path = file.path(tempdir(), "fuel-economy.csv"))
wfm_export_plot_data <- function(plot, path) {
  filename <- wfm_export_file(path, "csv")
  data <- wfm_plot_data(plot)
  wfm_write_plot_data(data, filename)
}

wfm_write_plot_data <- function(data, filename) {
  wfm_export_dir(filename)
  utils::write.csv(data, filename, row.names = FALSE, fileEncoding = "UTF-8")
  invisible(filename)
}

wfm_check_single_plot <- function(plot) {
  if (!inherits(plot, "ggplot")) {
    stop("`plot` must be a ggplot object.", call. = FALSE)
  }
  if (inherits(plot, "patchwork")) {
    stop("Patchwork compositions are not supported. Export each plot separately.",
         call. = FALSE)
  }
}

wfm_materialise_plot_data <- function(plot) {
  wfm_check_single_plot(plot)
  layers <- lapply(plot$layers, function(layer) {
    data <- layer$data
    if (is.function(data)) {
      data <- layer$layer_data(plot$data)
    }
    ggplot2::ggproto(NULL, layer, data = data)
  })
  plot$layers <- layers
  plot
}

wfm_plot_data <- function(plot) {
  wfm_check_single_plot(plot)
  if (length(plot$layers) == 0L) {
    stop("`plot` must contain at least one layer.", call. = FALSE)
  }

  facets <- c(plot$facet$params$facets, plot$facet$params$rows,
              plot$facet$params$cols)
  sources <- lapply(seq_along(plot$layers), function(i) {
    layer <- plot$layers[[i]]
    data <- layer$layer_data(plot$data)
    if (is.null(data) || inherits(data, "waiver")) {
      data <- data.frame()
    }
    if (!is.data.frame(data)) {
      stop("Data for layer ", i, " must be a data frame.", call. = FALSE)
    }
    wfm_check_plot_column_names(data)
    fixed_vectors <- vapply(layer$aes_params, function(x) length(x) > 1L,
                            logical(1))
    if (any(fixed_vectors)) {
      wfm_plot_data_error("Vector-valued fixed aesthetics", paste("layer", i))
    }

    mapping <- if (isTRUE(layer$inherit.aes)) as.list(plot$mapping) else list()
    for (aesthetic in names(layer$mapping)) {
      mapping[aesthetic] <- list(layer$mapping[[aesthetic]])
    }
    mapping <- mapping[setdiff(names(mapping), names(layer$aes_params))]
    columns <- wfm_mapping_columns(mapping, data, paste("layer", i))
    columns <- union(columns, wfm_mapping_columns(
      facets, data, paste("facets in layer", i), missing_ok = TRUE
    ))
    list(data = data, columns = columns, layer = i)
  })

  # Global data can supply facet panels even when no layer inherits those data.
  if (length(facets) && is.data.frame(plot$data)) {
    facet_data <- plot$data
    rownames(facet_data) <- NULL
    same <- vapply(sources, function(x) identical(x$data, facet_data), logical(1))
    if (!any(same)) {
      columns <- wfm_mapping_columns(
        facets, facet_data, "plot-level facets", missing_ok = TRUE
      )
      if (length(columns)) {
        wfm_check_plot_column_names(facet_data)
        sources <- c(sources, list(list(
          data = facet_data, columns = columns, layer = 0L
        )))
      }
    }
  }

  shared <- all(vapply(sources, function(x) {
    identical(x$data, sources[[1L]]$data)
  }, logical(1)))
  if (shared) {
    columns <- unique(unlist(lapply(sources, `[[`, "columns"), use.names = FALSE))
    out <- wfm_select_plot_columns(sources[[1L]]$data, columns)
    if (!length(columns)) {
      out$.wfm_row <- seq_len(nrow(out))
    }
    return(out)
  }

  pieces <- lapply(sources, function(source) {
    out <- wfm_select_plot_columns(source$data, source$columns)
    out$.wfm_layer <- rep.int(source$layer, nrow(out))
    out$.wfm_row <- seq_len(nrow(out))
    out[c(".wfm_layer", ".wfm_row", source$columns)]
  })
  tryCatch(
    do.call(vctrs::vec_rbind, pieces),
    vctrs_error_incompatible_type = function(e) {
      stop(
        "Layers have incompatible types for the same source column. ",
        "Rename or convert those columns before exporting.\n", conditionMessage(e),
        call. = FALSE
      )
    }
  )
}

wfm_check_plot_column_names <- function(data) {
  if (anyNA(names(data)) || any(names(data) == "") || anyDuplicated(names(data))) {
    stop("Plot data must have unique, non-empty column names.", call. = FALSE)
  }
}

wfm_select_plot_columns <- function(data, columns) {
  reserved <- intersect(columns, c(".wfm_layer", ".wfm_row"))
  if (length(reserved)) {
    stop("Rename reserved source columns before exporting: ",
         paste(reserved, collapse = ", "), ".", call. = FALSE)
  }
  out <- as.data.frame(data[columns], optional = TRUE)
  invalid <- vapply(out, function(x) {
    is.list(x) || !is.null(dim(x)) || !is.atomic(x)
  }, logical(1))
  if (any(invalid)) {
    stop("CSV cannot preserve these source columns: ",
         paste(names(out)[invalid], collapse = ", "),
         ". Convert list or matrix columns to ordinary columns first.",
         call. = FALSE)
  }
  rownames(out) <- NULL
  out
}

wfm_mapping_columns <- function(mapping, data, context, missing_ok = FALSE) {
  columns <- lapply(mapping, function(value) {
    expr <- rlang::get_expr(value)
    env <- if (rlang::is_quosure(value)) rlang::quo_get_env(value) else baseenv()
    expr <- wfm_start_mapping(expr)
    tryCatch(
      wfm_expr_columns(expr, names(data), env, context),
      wfm_missing_plot_column = function(e) {
        if (missing_ok) character() else stop(e)
      }
    )
  })
  unique(unlist(columns, use.names = FALSE))
}

wfm_call_name <- function(expr) {
  if (!is.call(expr)) return("")
  head <- expr[[1L]]
  if (is.symbol(head)) return(as.character(head))
  if (is.call(head) && as.character(head[[1L]]) %in% c("::", ":::")) {
    return(as.character(head[[3L]]))
  }
  ""
}

wfm_start_mapping <- function(expr) {
  if (wfm_call_name(expr) == "stage") {
    args <- as.list(expr)[-1L]
    nms <- names(args)
    if ("start" %in% nms) return(wfm_start_mapping(args[["start"]]))
    if (length(args) && (is.null(nms) || nms[[1L]] == "")) {
      return(wfm_start_mapping(args[[1L]]))
    }
    return(NULL)
  }
  if (wfm_call_name(expr) %in% c("after_scale", "from_theme")) return(NULL)
  if (wfm_is_stat_mapping(expr)) return(NULL)
  expr
}

wfm_is_stat_mapping <- function(expr) {
  if (is.symbol(expr)) return(grepl("^\\.\\..+\\.\\.$", as.character(expr)))
  if (!is.call(expr)) return(FALSE)
  if (wfm_call_name(expr) %in% c("after_stat", "stat")) return(TRUE)
  any(vapply(as.list(expr)[-1L], wfm_is_stat_mapping, logical(1)))
}

wfm_expr_columns <- function(expr, columns, env, context) {
  if (rlang::is_quosure(expr)) {
    return(wfm_expr_columns(rlang::get_expr(expr), columns,
                            rlang::quo_get_env(expr), context))
  }
  if (is.symbol(expr)) {
    name <- as.character(expr)
    if (name %in% c(".data", ".env")) {
      wfm_plot_data_error("Whole-data or whole-environment expressions", context)
    }
    if (name %in% columns) return(name)
    if (exists(name, envir = env, inherits = TRUE)) {
      wfm_check_external_plot_value(get(name, envir = env, inherits = TRUE), context)
      return(character())
    }
    rlang::abort(paste0("Source column `", name, "` is missing in ", context, "."),
                 class = "wfm_missing_plot_column")
  }
  if (!is.call(expr)) {
    if ((!is.null(expr) && !is.atomic(expr)) || !is.null(dim(expr)) ||
        length(expr) > 1L) {
      wfm_plot_data_error("Injected aesthetic vectors", context)
    }
    return(character())
  }

  name <- wfm_call_name(expr)
  if (name %in% c("get", "mget", "eval", "eval_tidy", "parse", "do.call")) {
    wfm_plot_data_error("Dynamic column lookups", context)
  }
  if (name %in% c("$", "[[") && length(expr) >= 3L) {
    target <- expr[[2L]]
    if (is.symbol(target) && as.character(target) %in% c(".data", ".env")) {
      key <- if (name == "$") as.character(expr[[3L]]) else {
        tryCatch(rlang::eval_bare(expr[[3L]], env), error = function(e) NULL)
      }
      if (!is.character(key) || length(key) != 1L || is.na(key)) {
        wfm_plot_data_error("Non-scalar or unresolved column lookups", context)
      }
      if (as.character(target) == ".data") {
        if (key %in% columns) return(key)
        rlang::abort(paste0("Source column `", key, "` is missing in ", context, "."),
                     class = "wfm_missing_plot_column")
      }
      if (!exists(key, envir = env, inherits = TRUE)) {
        stop("Environment value `", key, "` is missing in ", context, ".",
             call. = FALSE)
      }
      wfm_check_external_plot_value(get(key, envir = env, inherits = TRUE), context)
      return(character())
    }
    if (name == "$") {
      return(wfm_expr_columns(target, columns, env, context))
    }
  }
  if (name %in% c("::", ":::")) return(character())
  unique(unlist(lapply(as.list(expr)[-1L], wfm_expr_columns,
                      columns = columns, env = env, context = context),
                use.names = FALSE))
}

wfm_check_external_plot_value <- function(value, context) {
  if (is.function(value) || is.null(value)) return(invisible(NULL))
  if (is.atomic(value) && is.null(dim(value)) && length(value) == 1L) {
    return(invisible(NULL))
  }
  wfm_plot_data_error("External mapped vectors or objects", context)
}

wfm_plot_data_error <- function(what, context) {
  stop(what, " cannot be exported safely in ", context,
       ". Calculate the values in ordinary data columns and map those columns.",
       call. = FALSE)
}
