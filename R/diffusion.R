#' Compute an inflation diffusion index
#'
#' Measures the breadth of price increases across CPI components. For each
#' period, computes the (weighted or unweighted) fraction of items with price
#' changes exceeding a threshold. A diffusion index above 0.5 indicates that
#' more than half of items are experiencing above-threshold price increases.
#'
#' @param data A data.frame containing component-level inflation data.
#' @param threshold Character. The threshold rule: `"zero"` (price_change > 0),
#'   `"mean"` (above the cross-sectional mean), or `"target"` (above an
#'   annualised target rate).
#' @param target Numeric. Annualised inflation target (for `"target"` threshold
#'   only). Default `0.02` (2%).
#' @param weighted Logical. If `TRUE` (default), compute the weighted fraction.
#'   If `FALSE`, compute the simple (unweighted) fraction.
#' @param date_col Character. Name of the date column. Default `"date"`.
#' @param item_col Character. Name of the item/component column. Default
#'   `"item"`.
#' @param change_col Character. Name of the price change column. Default
#'   `"price_change"`.
#' @param weight_col Character. Name of the weight column. Default `"weight"`.
#'
#' @return An S3 object of class `"ik_diffusion"` with elements:
#' \describe{
#'   \item{diffusion}{data.frame with columns: date, diffusion_index.}
#'   \item{threshold}{Character. The threshold type used.}
#'   \item{weighted}{Logical. Whether weights were used.}
#' }
#'
#' @export
#' @examples
#' data <- ik_sample_data("components")
#'
#' # Fraction of items with rising prices
#' d <- ik_diffusion(data, threshold = "zero")
#' print(d)
#' plot(d)
ik_diffusion <- function(data,
                         threshold = c("zero", "mean", "target"),
                         target = 0.02,
                         weighted = TRUE,
                         date_col = "date",
                         item_col = "item",
                         change_col = "price_change",
                         weight_col = "weight") {
  threshold <- match.arg(threshold)
  validate_data(data, c(date_col, item_col, change_col, weight_col))

  df <- data.frame(
    date = data[[date_col]],
    item = data[[item_col]],
    weight = data[[weight_col]],
    price_change = data[[change_col]],
    stringsAsFactors = FALSE
  )

  # Convert annualised target to monthly rate for "target" threshold
  target_monthly <- (1 + target)^(1 / 12) - 1

  dates <- sort(unique(df$date))

  diffusion_vals <- vapply(dates, function(d) {
    sub <- df[df$date == d, ]

    above <- switch(threshold,
      zero = sub$price_change > 0,
      mean = sub$price_change > mean(sub$price_change),
      target = sub$price_change > target_monthly
    )

    if (weighted) {
      sum(sub$weight[above]) / sum(sub$weight)
    } else {
      mean(above)
    }
  }, numeric(1))

  structure(
    list(
      diffusion = data.frame(
        date = dates,
        diffusion_index = diffusion_vals,
        stringsAsFactors = FALSE
      ),
      threshold = threshold,
      weighted = weighted
    ),
    class = "ik_diffusion"
  )
}

#' @export
print.ik_diffusion <- function(x, ...) {
  cli_h1("Inflation Diffusion Index")

  threshold_label <- switch(x$threshold,
    zero = "Price change > 0",
    mean = "Price change > cross-sectional mean",
    target = "Price change > annualised target"
  )

  mean_diff <- mean(x$diffusion$diffusion_index, na.rm = TRUE)
  latest_diff <- tail(x$diffusion$diffusion_index, 1)

  cli_bullets(c(
    "*" = "Threshold: {threshold_label}",
    "*" = "Weighted: {x$weighted}",
    "*" = "Mean diffusion: {round(mean_diff, 3)}",
    "*" = "Latest diffusion: {round(latest_diff, 3)}",
    "*" = "Observations: {nrow(x$diffusion)}"
  ))
  invisible(x)
}

#' @export
plot.ik_diffusion <- function(x, ...) {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  dates <- as.Date(x$diffusion$date)
  vals <- x$diffusion$diffusion_index

  par(mar = c(4, 4, 3, 1))
  plot(
    dates, vals,
    type = "l",
    col = "#2166AC",
    lwd = 2,
    ylim = c(0, 1),
    xlab = "Date",
    ylab = "Diffusion Index",
    main = "Inflation Diffusion Index",
    ...
  )
  abline(h = 0.5, lty = 2, col = "grey50")
  grid(col = "grey90")

  invisible(x)
}
