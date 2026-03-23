#' Compute core inflation measures
#'
#' Estimates core (underlying) inflation using one of three standard methods:
#' trimmed mean, weighted median, or exclusion-based. These measures aim to
#' strip out transitory price movements and reveal the persistent trend.
#'
#' @param data A data.frame containing component-level inflation data.
#' @param method Character. One of `"trimmed_mean"`, `"weighted_median"`,
#'   `"exclusion"`, or `"asymmetric_trim"`.
#' @param trim Numeric. Fraction to trim from each tail (for `"trimmed_mean"`
#'   only). Default `0.08` (8% symmetric trim, following the Cleveland Fed).
#' @param trim_lower Numeric. Fraction to trim from the lower tail (for
#'   `"asymmetric_trim"` only). Default `0.24`, matching the Dallas Fed
#'   trimmed mean PCE.
#' @param trim_upper Numeric. Fraction to trim from the upper tail (for
#'   `"asymmetric_trim"` only). Default `0.31`, matching the Dallas Fed
#'   trimmed mean PCE.
#' @param exclude Character vector. Items to exclude (for `"exclusion"` only).
#' @param date_col Character. Name of the date column. Default `"date"`.
#' @param item_col Character. Name of the item/component column. Default
#'   `"item"`.
#' @param change_col Character. Name of the price change column. Default
#'   `"price_change"`.
#' @param weight_col Character. Name of the weight column. Default `"weight"`.
#'
#' @return An S3 object of class `"ik_core"` with elements:
#' \describe{
#'   \item{core}{data.frame with columns: date, core_inflation.}
#'   \item{method}{Character. The method used.}
#'   \item{trim}{Numeric. The trim fraction (if applicable).}
#'   \item{exclude}{Character vector. Excluded items (if applicable).}
#'   \item{headline}{data.frame with columns: date, headline.}
#' }
#'
#' @references
#' Bryan, M. F. and Cecchetti, S. G. (1993). "The Consumer Price Index as a
#' Measure of Inflation." NBER Working Paper No. 4505.
#'
#' Bryan, M. F. and Cecchetti, S. G. (1994). "Measuring Core Inflation." In
#' Monetary Policy, University of Chicago Press.
#'
#' @export
#' @examples
#' data <- ik_sample_data("components")
#'
#' # Trimmed mean (default)
#' core_tm <- ik_core(data, method = "trimmed_mean")
#' print(core_tm)
#'
#' # Weighted median
#' core_wm <- ik_core(data, method = "weighted_median")
#' print(core_wm)
#'
#' # Exclusion-based
#' core_ex <- ik_core(data, method = "exclusion", exclude = c("Food", "Transport"))
#' print(core_ex)
ik_core <- function(data,
                    method = c("trimmed_mean", "weighted_median", "exclusion",
                               "asymmetric_trim"),
                    trim = 0.08,
                    trim_lower = 0.24,
                    trim_upper = 0.31,
                    exclude = NULL,
                    date_col = "date",
                    item_col = "item",
                    change_col = "price_change",
                    weight_col = "weight") {
  method <- match.arg(method)
  validate_data(data, c(date_col, item_col, change_col, weight_col))

  df <- data.frame(
    date = data[[date_col]],
    item = data[[item_col]],
    weight = data[[weight_col]],
    price_change = data[[change_col]],
    stringsAsFactors = FALSE
  )

  # Warn if weights do not sum to approximately 1 within any date group
  dates <- sort(unique(df$date))
  for (d in dates) {
    w_sum <- sum(df$weight[df$date == d])
    if (abs(w_sum - 1) > 0.01) {
      cli_warn(
        "Weights do not sum to 1 for date {d} (sum = {round(w_sum, 4)}). Normalising internally."
      )
      break
    }
  }

  # Headline
  headline_vals <- vapply(dates, function(d) {
    sub <- df[df$date == d, ]
    weighted.mean(sub$price_change, sub$weight)
  }, numeric(1))

  # Core
  core_vals <- switch(method,
    trimmed_mean = .core_trimmed_mean(df, dates, trim),
    weighted_median = .core_weighted_median(df, dates),
    exclusion = .core_exclusion(df, dates, exclude),
    asymmetric_trim = .core_asymmetric_trim(df, dates, trim_lower, trim_upper)
  )

  structure(
    list(
      core = data.frame(
        date = dates,
        core_inflation = core_vals,
        stringsAsFactors = FALSE
      ),
      method = method,
      trim = trim,
      trim_lower = if (method == "asymmetric_trim") trim_lower else NULL,
      trim_upper = if (method == "asymmetric_trim") trim_upper else NULL,
      exclude = exclude,
      headline = data.frame(
        date = dates,
        headline = headline_vals,
        stringsAsFactors = FALSE
      )
    ),
    class = "ik_core"
  )
}

#' @noRd
.core_trimmed_mean <- function(df, dates, trim) {
  validate_scalar(trim, "trim")
  if (trim <= 0 || trim >= 0.5) {
    cli_abort("{.arg trim} must be between 0 and 0.5 (exclusive).")
  }

  vapply(dates, function(d) {
    sub <- df[df$date == d, ]
    ord <- order(sub$price_change)
    sub <- sub[ord, ]

    # Normalise weights
    w <- sub$weight / sum(sub$weight)
    cum_w <- cumsum(w)

    # Keep items where cumulative weight is within trimmed bounds
    keep <- cum_w > trim & (cum_w - w) < (1 - trim)

    if (!any(keep)) {
      return(weighted.mean(sub$price_change, sub$weight))
    }

    # Adjust edge weights
    w_adj <- w
    for (i in which(keep)) {
      lower_excess <- max(0, trim - (cum_w[i] - w[i]))
      upper_excess <- max(0, cum_w[i] - (1 - trim))
      w_adj[i] <- w[i] - lower_excess - upper_excess
    }
    w_adj[!keep] <- 0

    weighted.mean(sub$price_change, w_adj)
  }, numeric(1))
}

#' @noRd
.core_weighted_median <- function(df, dates) {
  vapply(dates, function(d) {
    sub <- df[df$date == d, ]
    ord <- order(sub$price_change)
    sub <- sub[ord, ]

    w <- sub$weight / sum(sub$weight)
    cum_w <- cumsum(w)

    # Find first item where cumulative weight >= 0.5
    idx <- which(cum_w >= 0.5)[1]
    sub$price_change[idx]
  }, numeric(1))
}

#' @noRd
.core_exclusion <- function(df, dates, exclude) {
  if (is.null(exclude) || length(exclude) == 0L) {
    cli_abort("{.arg exclude} must be a non-empty character vector for the exclusion method.")
  }

  all_items <- unique(df$item)
  missing <- setdiff(exclude, all_items)
  if (length(missing) > 0L) {
    cli_warn("Items not found in data: {.field {missing}}.")
  }

  vapply(dates, function(d) {
    sub <- df[df$date == d & !df$item %in% exclude, ]
    if (nrow(sub) == 0L) {
      cli_abort("No items remain after exclusion for date {d}.")
    }
    # Reweight proportionally
    weighted.mean(sub$price_change, sub$weight)
  }, numeric(1))
}

#' Asymmetric trimmed mean (Dallas Fed style)
#' @noRd
.core_asymmetric_trim <- function(df, dates, trim_lower, trim_upper) {
  validate_scalar(trim_lower, "trim_lower")
  validate_scalar(trim_upper, "trim_upper")
  if (trim_lower <= 0 || trim_lower >= 1) {
    cli_abort("{.arg trim_lower} must be between 0 and 1 (exclusive).")
  }
  if (trim_upper <= 0 || trim_upper >= 1) {
    cli_abort("{.arg trim_upper} must be between 0 and 1 (exclusive).")
  }
  if (trim_lower + trim_upper >= 1) {
    cli_abort("Sum of {.arg trim_lower} and {.arg trim_upper} must be less than 1.")
  }

  vapply(dates, function(d) {
    sub <- df[df$date == d, ]
    ord <- order(sub$price_change)
    sub <- sub[ord, ]

    # Normalise weights
    w <- sub$weight / sum(sub$weight)
    cum_w <- cumsum(w)

    # Keep items where cumulative weight is within asymmetric trimmed bounds
    keep <- cum_w > trim_lower & (cum_w - w) < (1 - trim_upper)

    if (!any(keep)) {
      return(weighted.mean(sub$price_change, sub$weight))
    }

    # Adjust edge weights
    w_adj <- w
    for (i in which(keep)) {
      lower_excess <- max(0, trim_lower - (cum_w[i] - w[i]))
      upper_excess <- max(0, cum_w[i] - (1 - trim_upper))
      w_adj[i] <- w[i] - lower_excess - upper_excess
    }
    w_adj[!keep] <- 0

    weighted.mean(sub$price_change, w_adj)
  }, numeric(1))
}

#' @export
print.ik_core <- function(x, ...) {
  cli_h1("Core Inflation Measure")

  method_label <- switch(x$method,
    trimmed_mean = paste0("Trimmed Mean (", x$trim * 100, "%)"),
    weighted_median = "Weighted Median",
    exclusion = paste0("Exclusion (", paste(x$exclude, collapse = ", "), ")"),
    asymmetric_trim = paste0("Asymmetric Trim (lower ",
                             x$trim_lower * 100, "%, upper ",
                             x$trim_upper * 100, "%)")
  )

  mean_core <- mean(x$core$core_inflation, na.rm = TRUE)
  mean_hl <- mean(x$headline$headline, na.rm = TRUE)

  cli_bullets(c(
    "*" = "Method: {method_label}",
    "*" = "Mean core inflation: {fmt_pct(mean_core)}",
    "*" = "Mean headline inflation: {fmt_pct(mean_hl)}",
    "*" = "Observations: {nrow(x$core)}"
  ))
  invisible(x)
}

#' @export
plot.ik_core <- function(x, ...) {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  dates <- as.Date(x$core$date)
  core <- x$core$core_inflation
  headline <- x$headline$headline

  ylim <- range(c(core, headline), na.rm = TRUE)
  ylim <- ylim + c(-1, 1) * diff(ylim) * 0.1

  method_label <- switch(x$method,
    trimmed_mean = paste0("Trimmed Mean (", x$trim * 100, "%)"),
    weighted_median = "Weighted Median",
    exclusion = "Exclusion-Based",
    asymmetric_trim = "Asymmetric Trim"
  )

  par(mar = c(4, 4, 3, 1))
  plot(
    dates, headline,
    type = "l",
    col = adjustcolor("grey50", 0.7),
    lwd = 1.5,
    ylim = ylim,
    xlab = "Date",
    ylab = "Inflation",
    main = paste("Core vs Headline Inflation:", method_label),
    ...
  )
  lines(dates, core, col = "#2166AC", lwd = 2)
  grid(col = "grey90")
  legend(
    "topright",
    legend = c("Headline", "Core"),
    col = c("grey50", "#2166AC"),
    lwd = c(1.5, 2),
    bty = "n"
  )

  invisible(x)
}
