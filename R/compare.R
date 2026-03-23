#' Compare multiple core inflation measures
#'
#' Takes multiple `ik_core` objects and facilitates side-by-side comparison.
#' Produces summary statistics and a multi-line chart of all measures.
#'
#' @param ... One or more objects of class `"ik_core"`, as returned by
#'   [ik_core()].
#' @param labels Character vector or `NULL`. Labels for each measure. If
#'   `NULL`, labels are derived from each object's method name.
#'
#' @return An S3 object of class `"ik_comparison"` with elements:
#' \describe{
#'   \item{measures}{List of `ik_core` objects.}
#'   \item{labels}{Character vector of labels.}
#' }
#'
#' @export
#' @examples
#' data <- ik_sample_data("components")
#' core_tm <- ik_core(data, method = "trimmed_mean")
#' core_wm <- ik_core(data, method = "weighted_median")
#' core_ex <- ik_core(data, method = "exclusion", exclude = c("Food", "Transport"))
#'
#' comp <- ik_compare(core_tm, core_wm, core_ex)
#' print(comp)
#' plot(comp)
ik_compare <- function(..., labels = NULL) {
  measures <- list(...)

  if (length(measures) == 0L) {
    cli_abort("At least one {.cls ik_core} object must be provided.")
  }

  for (i in seq_along(measures)) {
    if (!inherits(measures[[i]], "ik_core")) {
      cli_abort(
        "Argument {i} is not an {.cls ik_core} object."
      )
    }
  }

  if (is.null(labels)) {
    labels <- vapply(measures, function(m) {
      switch(m$method,
        trimmed_mean = paste0("Trimmed Mean (", m$trim * 100, "%)"),
        weighted_median = "Weighted Median",
        exclusion = paste0("Exclusion (", paste(m$exclude, collapse = ", "), ")"),
        asymmetric_trim = paste0("Asymmetric Trim (", m$trim_lower * 100, "%-", m$trim_upper * 100, "%)")
      )
    }, character(1))
  }

  if (length(labels) != length(measures)) {
    cli_abort(
      "{.arg labels} must have the same length as the number of measures ({length(measures)})."
    )
  }

  structure(
    list(
      measures = measures,
      labels = labels
    ),
    class = "ik_comparison"
  )
}

#' @export
print.ik_comparison <- function(x, ...) {
  cli_h1("Core Inflation Comparison")

  cli_h2("Summary Statistics")

  for (i in seq_along(x$measures)) {
    m <- x$measures[[i]]
    mean_val <- mean(m$core$core_inflation, na.rm = TRUE)
    sd_val <- sd(m$core$core_inflation, na.rm = TRUE)
    cli_bullets(c(
      "*" = "{x$labels[i]}: mean = {fmt_pct(mean_val)}, SD = {round(sd_val, 5)}"
    ))
  }

  # Headline for reference
  hl_mean <- mean(x$measures[[1]]$headline$headline, na.rm = TRUE)
  cli_bullets(c(
    "*" = "Headline: mean = {fmt_pct(hl_mean)}"
  ))

  invisible(x)
}

#' @export
plot.ik_comparison <- function(x, ...) {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  n_measures <- length(x$measures)

  # Collect all values for ylim
  all_vals <- unlist(lapply(x$measures, function(m) m$core$core_inflation))
  headline <- x$measures[[1]]$headline$headline
  all_vals <- c(all_vals, headline)
  ylim <- range(all_vals, na.rm = TRUE)
  ylim <- ylim + c(-1, 1) * diff(ylim) * 0.1

  # Colours
  cols <- c("#2166AC", "#B2182B", "#4DAF4A", "#984EA3", "#FF7F00",
            "#A65628", "#F781BF", "#999999")
  if (n_measures > length(cols)) {
    cols <- rep_len(cols, n_measures)
  }

  dates <- as.Date(x$measures[[1]]$core$date)

  par(mar = c(4, 4, 3, 1))
  # Plot headline first (grey)
  plot(
    dates, headline,
    type = "l",
    col = adjustcolor("grey60", 0.6),
    lwd = 1.5,
    ylim = ylim,
    xlab = "Date",
    ylab = "Inflation",
    main = "Core Inflation Measures Comparison",
    ...
  )

  # Overlay core measures
  for (i in seq_along(x$measures)) {
    m <- x$measures[[i]]
    lines(as.Date(m$core$date), m$core$core_inflation,
          col = cols[i], lwd = 2)
  }

  grid(col = "grey90")
  legend(
    "topright",
    legend = c("Headline", x$labels),
    col = c("grey60", cols[seq_len(n_measures)]),
    lwd = c(1.5, rep(2, n_measures)),
    bty = "n",
    cex = 0.8
  )

  invisible(x)
}
