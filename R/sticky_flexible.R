#' Decompose inflation into sticky and flexible components
#'
#' Splits CPI components into sticky-price and flexible-price categories based
#' on a user-provided classification, then computes separate weighted inflation
#' measures for each group. This follows the Atlanta Fed methodology.
#'
#' @param data A data.frame containing component-level inflation data.
#' @param classification A named logical vector or a data.frame. If a named
#'   logical vector, names correspond to item names and `TRUE` indicates sticky.
#'   If a data.frame, it must have columns `item` (character) and `sticky`
#'   (logical).
#' @param date_col Character. Name of the date column. Default `"date"`.
#' @param item_col Character. Name of the item/component column. Default
#'   `"item"`.
#' @param change_col Character. Name of the price change column. Default
#'   `"price_change"`.
#' @param weight_col Character. Name of the weight column. Default `"weight"`.
#'
#' @return An S3 object of class `"ik_sticky_flex"` with elements:
#' \describe{
#'   \item{result}{data.frame with columns: date, sticky, flexible, headline.}
#'   \item{classification}{Named logical vector mapping items to sticky/flexible.}
#' }
#'
#' @references
#' Bils, M. and Klenow, P. J. (2004). "Some Evidence on the Importance of
#' Sticky Prices." Journal of Political Economy, 112(5), 947-985.
#'
#' @export
#' @examples
#' data <- ik_sample_data("components")
#' # Classify items
#' class_vec <- c(
#'   Food = FALSE, Housing = TRUE, Transport = FALSE,
#'   Clothing = FALSE, Health = TRUE, Education = TRUE,
#'   Communication = TRUE, Recreation = FALSE,
#'   Restaurants = TRUE, Other = FALSE
#' )
#' sf <- ik_sticky_flexible(data, classification = class_vec)
#' print(sf)
#' plot(sf)
ik_sticky_flexible <- function(data,
                               classification,
                               date_col = "date",
                               item_col = "item",
                               change_col = "price_change",
                               weight_col = "weight") {
  validate_data(data, c(date_col, item_col, change_col, weight_col))

  # Normalise classification to named logical vector
  if (is.data.frame(classification)) {
    if (!all(c("item", "sticky") %in% names(classification))) {
      cli_abort(
        "{.arg classification} data.frame must have columns {.field item} and {.field sticky}."
      )
    }
    class_vec <- setNames(classification$sticky, classification$item)
  } else if (is.logical(classification) && !is.null(names(classification))) {
    class_vec <- classification
  } else {
    cli_abort(
      paste0(
        "{.arg classification} must be a named logical vector or a data.frame ",
        "with columns {.field item} and {.field sticky}."
      )
    )
  }

  df <- data.frame(
    date = data[[date_col]],
    item = data[[item_col]],
    weight = data[[weight_col]],
    price_change = data[[change_col]],
    stringsAsFactors = FALSE
  )

  all_items <- unique(df$item)
  classified_items <- intersect(all_items, names(class_vec))

  if (length(classified_items) == 0L) {
    cli_abort("No items in {.arg data} match the classification.")
  }

  missing_items <- setdiff(all_items, names(class_vec))
  if (length(missing_items) > 0L) {
    cli_warn(
      "Items not in classification (excluded): {.field {missing_items}}."
    )
  }

  # Filter to classified items only
  df <- df[df$item %in% classified_items, ]
  sticky_items <- names(class_vec[class_vec])
  flex_items <- names(class_vec[!class_vec])

  dates <- sort(unique(df$date))

  sticky_vals <- numeric(length(dates))
  flex_vals <- numeric(length(dates))
  headline_vals <- numeric(length(dates))

  for (i in seq_along(dates)) {
    d <- dates[i]
    sub <- df[df$date == d, ]
    headline_vals[i] <- weighted.mean(sub$price_change, sub$weight)

    sub_sticky <- sub[sub$item %in% sticky_items, ]
    if (nrow(sub_sticky) > 0L) {
      sticky_vals[i] <- weighted.mean(
        sub_sticky$price_change, sub_sticky$weight
      )
    }

    sub_flex <- sub[sub$item %in% flex_items, ]
    if (nrow(sub_flex) > 0L) {
      flex_vals[i] <- weighted.mean(sub_flex$price_change, sub_flex$weight)
    }
  }

  result <- data.frame(
    date = dates,
    sticky = sticky_vals,
    flexible = flex_vals,
    headline = headline_vals,
    stringsAsFactors = FALSE
  )

  structure(
    list(
      result = result,
      classification = class_vec
    ),
    class = "ik_sticky_flex"
  )
}

#' @export
print.ik_sticky_flex <- function(x, ...) {
  cli_h1("Sticky vs Flexible Price Inflation")

  n_sticky <- sum(x$classification)
  n_flex <- sum(!x$classification)
  mean_sticky <- mean(x$result$sticky, na.rm = TRUE)
  mean_flex <- mean(x$result$flexible, na.rm = TRUE)
  corr <- cor(x$result$sticky, x$result$flexible, use = "complete.obs")

  cli_bullets(c(
    "*" = "Sticky items: {n_sticky}",
    "*" = "Flexible items: {n_flex}",
    "*" = "Mean sticky inflation: {fmt_pct(mean_sticky)}",
    "*" = "Mean flexible inflation: {fmt_pct(mean_flex)}",
    "*" = "Correlation: {round(corr, 3)}",
    "*" = "Observations: {nrow(x$result)}"
  ))
  invisible(x)
}

#' @export
plot.ik_sticky_flex <- function(x, ...) {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  dates <- as.Date(x$result$date)
  ylim <- range(
    c(x$result$sticky, x$result$flexible), na.rm = TRUE
  )
  ylim <- ylim + c(-1, 1) * diff(ylim) * 0.1

  par(mar = c(4, 4, 3, 1))
  plot(
    dates, x$result$sticky,
    type = "l",
    col = "#B2182B",
    lwd = 2,
    ylim = ylim,
    xlab = "Date",
    ylab = "Inflation",
    main = "Sticky vs Flexible Price Inflation",
    ...
  )
  lines(dates, x$result$flexible, col = "#2166AC", lwd = 2)
  grid(col = "grey90")
  legend(
    "topright",
    legend = c("Sticky", "Flexible"),
    col = c("#B2182B", "#2166AC"),
    lwd = 2,
    bty = "n"
  )

  invisible(x)
}
