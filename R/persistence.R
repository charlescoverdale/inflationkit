#' Measure inflation persistence
#'
#' Estimates the degree of persistence in an inflation series using one of
#' three methods: sum of AR coefficients, half-life, or largest autoregressive
#' root.
#'
#' @param x Numeric vector. An inflation time series.
#' @param method Character. One of `"sum_ar"`, `"half_life"`, or
#'   `"largest_root"`.
#' @param ar_order Integer or `NULL`. The AR order to fit. If `NULL`, order is
#'   selected automatically using the information criterion specified by `ic`.
#' @param max_order Integer. Maximum AR order to consider when selecting
#'   automatically. Default `12`.
#' @param ic Character. Information criterion for order selection: `"bic"` or
#'   `"aic"`. Default `"bic"`.
#'
#' @return An S3 object of class `"ik_persistence"` with elements:
#' \describe{
#'   \item{value}{Numeric. The persistence measure.}
#'   \item{method}{Character. The method used.}
#'   \item{ar_order}{Integer. The AR order fitted.}
#'   \item{ar_coefficients}{Numeric vector. The estimated AR coefficients.}
#'   \item{interpretation}{Character. A plain-language interpretation
#'     ("High persistence", "Moderate persistence", or "Low persistence").}
#' }
#'
#' @references
#' Andrews, D. W. K. and Chen, H.-Y. (1994). "Approximately Median-Unbiased
#' Estimation of Autoregressive Models." Journal of Business and Economic
#' Statistics, 12(2), 187-204.
#'
#' Marques, C. R. (2004). "Inflation Persistence: Facts or Artefacts?" ECB
#' Working Paper No. 371.
#'
#' @export
#' @examples
#' data <- ik_sample_data("headline")
#' p <- ik_persistence(data$inflation, method = "sum_ar")
#' print(p)
#'
#' p_hl <- ik_persistence(data$inflation, method = "half_life")
#' print(p_hl)
ik_persistence <- function(x,
                           method = c("sum_ar", "half_life", "largest_root"),
                           ar_order = NULL,
                           max_order = 12L,
                           ic = c("bic", "aic")) {
  method <- match.arg(method)
  ic <- match.arg(ic)
  validate_numeric_vector(x, "x")

  if (length(x) < 10L) {
    cli_abort("{.arg x} must have at least 10 observations.")
  }

  if (!is.null(ar_order)) {
    validate_positive_integer(ar_order, "ar_order")
  }

  # Select AR order if not provided
  p <- if (is.null(ar_order)) {
    .select_ar_order(x, max_order = max_order, ic = ic)
  } else {
    ar_order
  }

  # Fit AR(p) model
  ar_coefs <- .fit_ar(x, p)

  # Compute persistence measure
  result <- switch(method,
    sum_ar = {
      val <- sum(ar_coefs)
      list(value = val, ar_coefficients = ar_coefs)
    },
    half_life = {
      # Use AR(1) coefficient
      rho <- if (p >= 1L) ar_coefs[1] else 0
      if (rho <= 0 || rho >= 1) {
        val <- if (rho >= 1) Inf else 0
      } else {
        val <- -log(2) / log(abs(rho))
      }
      list(value = val, ar_coefficients = ar_coefs)
    },
    largest_root = {
      if (p == 0L) {
        val <- 0
      } else {
        # Companion form polynomial roots
        poly_coefs <- c(1, -ar_coefs)
        roots <- polyroot(poly_coefs)
        val <- max(Mod(roots))
      }
      list(value = val, ar_coefficients = ar_coefs)
    }
  )

  # Interpretation
  interpretation <- if (method == "half_life") {
    if (result$value > 12) {
      "High persistence (half-life > 12 periods)"
    } else if (result$value > 4) {
      "Moderate persistence (half-life 4 to 12 periods)"
    } else {
      "Low persistence (half-life < 4 periods)"
    }
  } else if (method == "sum_ar") {
    if (result$value > 0.8) {
      "High persistence (sum of AR coefficients > 0.8)"
    } else if (result$value > 0.5) {
      "Moderate persistence (sum of AR coefficients 0.5 to 0.8)"
    } else {
      "Low persistence (sum of AR coefficients < 0.5)"
    }
  } else {
    if (result$value > 0.9) {
      "High persistence (largest root > 0.9)"
    } else if (result$value > 0.7) {
      "Moderate persistence (largest root 0.7 to 0.9)"
    } else {
      "Low persistence (largest root < 0.7)"
    }
  }

  structure(
    list(
      value = result$value,
      method = method,
      ar_order = p,
      ar_coefficients = result$ar_coefficients,
      interpretation = interpretation
    ),
    class = "ik_persistence"
  )
}

#' @noRd
.select_ar_order <- function(x, max_order, ic) {
  n <- length(x)
  max_order <- min(max_order, n - 2L)

  if (max_order < 1L) return(1L)

  ic_vals <- rep(Inf, max_order)
  for (p in seq_len(max_order)) {
    if (n <= p + 1L) next
    coefs <- .fit_ar(x, p)
    # Compute residuals
    resid <- .ar_residuals(x, coefs)
    k <- p + 1L  # p coefficients + intercept
    rss <- sum(resid^2)
    n_eff <- length(resid)
    sigma2 <- rss / n_eff

    ic_vals[p] <- if (ic == "bic") {
      n_eff * log(sigma2) + k * log(n_eff)
    } else {
      n_eff * log(sigma2) + 2 * k
    }
  }

  which.min(ic_vals)
}

#' @noRd
.fit_ar <- function(x, p) {
  if (p == 0L) return(numeric(0))

  n <- length(x)
  if (n <= p) {
    cli_abort("Series too short for AR({p}) model.")
  }

  # Build design matrix
  y <- x[(p + 1):n]
  X <- matrix(NA_real_, nrow = length(y), ncol = p)
  for (j in seq_len(p)) {
    X[, j] <- x[(p + 1 - j):(n - j)]
  }

  fit <- lm(y ~ X)
  coefs <- unname(coef(fit)[-1])  # Exclude intercept
  coefs[is.na(coefs)] <- 0
  coefs
}

#' @noRd
.ar_residuals <- function(x, coefs) {
  p <- length(coefs)
  if (p == 0L) return(x - mean(x))

  n <- length(x)
  y <- x[(p + 1):n]
  X <- matrix(NA_real_, nrow = length(y), ncol = p)
  for (j in seq_len(p)) {
    X[, j] <- x[(p + 1 - j):(n - j)]
  }

  fit <- lm(y ~ X)
  unname(residuals(fit))
}

#' @export
print.ik_persistence <- function(x, ...) {
  cli_h1("Inflation Persistence")

  method_label <- switch(x$method,
    sum_ar = "Sum of AR Coefficients",
    half_life = "Half-Life",
    largest_root = "Largest Autoregressive Root"
  )

  cli_bullets(c(
    "*" = "Method: {method_label}",
    "*" = "AR order: {x$ar_order}",
    "*" = "Persistence value: {round(x$value, 4)}",
    "*" = "{x$interpretation}"
  ))
  invisible(x)
}
