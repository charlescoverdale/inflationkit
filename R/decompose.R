#' Decompose inflation into weighted component contributions
#'
#' Computes the weighted contribution of each CPI component to headline
#' inflation. The contribution of item i is `weight_i * price_change_i`, and
#' headline inflation is the sum of all contributions for each period.
#'
#' @param data A data.frame containing component-level inflation data.
#' @param date_col Character. Name of the date column. Default `"date"`.
#' @param item_col Character. Name of the item/component column. Default
#'   `"item"`.
#' @param change_col Character. Name of the price change column. Default
#'   `"price_change"`.
#' @param weight_col Character. Name of the weight column. Default `"weight"`.
#'
#' @return An S3 object of class `"ik_decomposition"` with elements:
#' \describe{
#'   \item{contributions}{data.frame with columns: date, item, weight,
#'     price_change, contribution.}
#'   \item{headline}{data.frame with columns: date, headline_inflation.}
#' }
#'
#' @export
#' @examples
#' data <- ik_sample_data("components")
#' decomp <- ik_decompose(data)
#' print(decomp)
#' plot(decomp)
ik_decompose <- function(data,
                         date_col = "date",
                         item_col = "item",
                         change_col = "price_change",
                         weight_col = "weight") {
  validate_data(data, c(date_col, item_col, change_col, weight_col))

  # Standardise column names
  df <- data.frame(
    date = data[[date_col]],
    item = data[[item_col]],
    weight = data[[weight_col]],
    price_change = data[[change_col]],
    stringsAsFactors = FALSE
  )

  # Normalise weights within each date group so they sum to 1
  dates <- unique(df$date)
  for (d in dates) {
    idx <- df$date == d
    w_sum <- sum(df$weight[idx])
    if (w_sum > 0) {
      df$weight[idx] <- df$weight[idx] / w_sum
    }
  }

  # Compute contributions
  df$contribution <- df$weight * df$price_change

  # Compute headline
  headline_vals <- vapply(dates, function(d) {
    sum(df$contribution[df$date == d])
  }, numeric(1))

  headline <- data.frame(
    date = dates,
    headline_inflation = headline_vals,
    stringsAsFactors = FALSE
  )

  structure(
    list(
      contributions = df,
      headline = headline
    ),
    class = "ik_decomposition"
  )
}

#' @export
print.ik_decomposition <- function(x, ...) {
  cli_h1("Inflation Decomposition")
  dates <- x$headline$date
  items <- unique(x$contributions$item)
  mean_hl <- mean(x$headline$headline_inflation, na.rm = TRUE)

  cli_bullets(c(
    "*" = "Period: {min(dates)} to {max(dates)}",
    "*" = "Number of items: {length(items)}",
    "*" = "Mean headline inflation: {fmt_pct(mean_hl)}"
  ))
  invisible(x)
}

#' @export
plot.ik_decomposition <- function(x, ...) {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  contrib <- x$contributions
  dates <- sort(unique(contrib$date))
  items <- unique(contrib$item)

  # If many dates, show the most recent 24 periods
  if (length(dates) > 24L) {
    dates <- tail(dates, 24L)
    contrib <- contrib[contrib$date %in% dates, ]
  }

  # Build contribution matrix: items x dates
  mat <- matrix(0, nrow = length(items), ncol = length(dates))
  rownames(mat) <- items
  for (i in seq_along(items)) {
    for (j in seq_along(dates)) {
      vals <- contrib$contribution[
        contrib$item == items[i] & contrib$date == dates[j]
      ]
      if (length(vals) > 0L) mat[i, j] <- vals[1]
    }
  }

  # Colour palette
  n_items <- length(items)
  cols <- grDevices::hcl.colors(n_items, palette = "Set 2")

  par(mar = c(5, 4, 3, 8), xpd = TRUE)
  barplot(
    mat,
    beside = FALSE,
    col = cols,
    border = NA,
    names.arg = format(dates, "%Y-%m"),
    las = 2,
    cex.names = 0.7,
    main = "Inflation Decomposition: Component Contributions",
    ylab = "Contribution to Inflation",
    ...
  )

  legend(
    "topright",
    inset = c(-0.25, 0),
    legend = items,
    fill = cols,
    border = NA,
    cex = 0.7,
    bty = "n"
  )

  invisible(x)
}
