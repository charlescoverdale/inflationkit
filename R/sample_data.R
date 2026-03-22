#' Generate sample inflation data
#'
#' Creates synthetic data for testing and demonstrating inflationkit functions.
#' Two types are available: component-level CPI data and headline macro data.
#'
#' @param type Character. Either `"components"` for item-level CPI data with
#'   weights and price changes, or `"headline"` for quarterly macro data with
#'   inflation, output gap, and unemployment.
#'
#' @return A data.frame. For `"components"`: columns `date`, `item`, `weight`,
#'   `price_change` (120 months, 10 items, 1200 rows). For `"headline"`:
#'   columns `date`, `inflation`, `output_gap`, `unemployment` (80 quarterly
#'   observations).
#'
#' @export
#' @examples
#' comp <- ik_sample_data("components")
#' head(comp)
#'
#' macro <- ik_sample_data("headline")
#' head(macro)
ik_sample_data <- function(type = c("components", "headline")) {
  type <- match.arg(type)

  if (type == "components") {
    return(.sample_components())
  }

  .sample_headline()
}

#' @noRd
.sample_components <- function() {
  old_seed <- .Random.seed
  on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  set.seed(42L)

  items <- c(
    "Food", "Housing", "Transport", "Clothing", "Health",
    "Education", "Communication", "Recreation", "Restaurants", "Other"
  )

  weights <- c(0.15, 0.25, 0.12, 0.06, 0.07,
               0.04, 0.03, 0.10, 0.08, 0.10)

  n_months <- 120L
  dates <- seq.Date(
    from = as.Date("2015-01-01"),
    by = "month",
    length.out = n_months
  )

  # Mean monthly price changes (annualised equivalent roughly 2-4%)

  mean_changes <- c(0.003, 0.004, 0.002, 0.001, 0.003,
                    0.003, -0.001, 0.002, 0.003, 0.002)

  # Volatilities vary by component

  vol <- c(0.004, 0.003, 0.008, 0.006, 0.003,
           0.002, 0.003, 0.004, 0.003, 0.003)

  rows <- vector("list", length(items))
  for (i in seq_along(items)) {
    pc <- rnorm(n_months, mean = mean_changes[i], sd = vol[i])
    rows[[i]] <- data.frame(
      date = dates,
      item = items[i],
      weight = weights[i],
      price_change = round(pc, 6),
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' @noRd
.sample_headline <- function() {
  old_seed <- .Random.seed
  on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  set.seed(42L)

  n_quarters <- 80L
  dates <- seq.Date(
    from = as.Date("2005-01-01"),
    by = "quarter",
    length.out = n_quarters
  )

  # Generate correlated macro series
  inflation <- numeric(n_quarters)
  output_gap <- numeric(n_quarters)
  unemployment <- numeric(n_quarters)

  inflation[1] <- 2.5

output_gap[1] <- 0.0
  unemployment[1] <- 5.5

  for (t in 2:n_quarters) {
    output_gap[t] <- 0.7 * output_gap[t - 1] + rnorm(1, 0, 0.5)
    unemployment[t] <- 5.5 - 0.4 * output_gap[t] + rnorm(1, 0, 0.3)
    inflation[t] <- 0.6 * inflation[t - 1] + 0.3 * 2.0 -
      0.15 * (unemployment[t] - 5.5) + rnorm(1, 0, 0.3)
  }

  # Clamp to realistic ranges
  inflation <- pmax(pmin(inflation, 6), 0.5)
  output_gap <- pmax(pmin(output_gap, 4), -4)
  unemployment <- pmax(pmin(unemployment, 10), 3)

  data.frame(
    date = dates,
    inflation = round(inflation, 3),
    output_gap = round(output_gap, 3),
    unemployment = round(unemployment, 3),
    stringsAsFactors = FALSE
  )
}
