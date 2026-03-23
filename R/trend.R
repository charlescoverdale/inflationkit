#' Estimate trend inflation
#'
#' Extracts the trend component from an inflation series using one of four
#' methods: Hodrick-Prescott filter, Beveridge-Nelson decomposition,
#' exponential smoothing, or centred moving average.
#'
#' @param x Numeric vector. An inflation time series.
#' @param method Character. One of `"hp"`, `"beveridge_nelson"`,
#'   `"exponential_smooth"`, or `"moving_average"`.
#' @param frequency Character. Data frequency: `"quarterly"` (default),
#'   `"monthly"`, or `"annual"`. Used to set the HP filter lambda (when
#'   `lambda = NULL`) and moving average window (when `window = NULL`).
#' @param lambda Numeric or `NULL`. Smoothing parameter for the HP filter. If
#'   `NULL`, defaults based on `frequency`: 6.25 for annual, 1600 for
#'   quarterly (Hodrick and Prescott, 1997), or 14400 for monthly
#'   (Backus and Kehoe, 1992).
#' @param window Integer or `NULL`. Window size for the moving average method.
#'   Defaults to 4 for quarterly, 12 for monthly, or 3 for annual.
#'
#' @return An S3 object of class `"ik_trend"` with elements:
#' \describe{
#'   \item{trend}{Numeric vector. The estimated trend component.}
#'   \item{cycle}{Numeric vector. The cyclical component (original minus
#'     trend).}
#'   \item{method}{Character. The method used.}
#'   \item{lambda}{Numeric. HP filter lambda (if applicable).}
#'   \item{window}{Integer. Moving average window (if applicable).}
#'   \item{alpha}{Numeric. Exponential smoothing parameter (if applicable).}
#'   \item{original}{Numeric vector. The original series.}
#' }
#'
#' @references
#' Hodrick, R. J. and Prescott, E. C. (1997). "Postwar U.S. Business Cycles:
#' An Empirical Investigation." Journal of Money, Credit and Banking, 29(1),
#' 1-16.
#'
#' Ravn, M. O. and Uhlig, H. (2002). "On Adjusting the Hodrick-Prescott
#' Filter for the Frequency of Observations." Review of Economics and
#' Statistics, 84(2), 371-376.
#'
#' @export
#' @examples
#' data <- ik_sample_data("headline")
#' tr <- ik_trend(data$inflation, method = "hp")
#' print(tr)
#' plot(tr)
#'
#' tr_ma <- ik_trend(data$inflation, method = "moving_average", window = 4)
#' print(tr_ma)
ik_trend <- function(x,
                     method = c("hp", "beveridge_nelson",
                                "exponential_smooth", "moving_average"),
                     frequency = c("quarterly", "monthly", "annual"),
                     lambda = NULL,
                     window = NULL) {
  method <- match.arg(method)
  frequency <- match.arg(frequency)
  validate_numeric_vector(x, "x")
  n <- length(x)

  if (n < 6L) {
    cli_abort("{.arg x} must have at least 6 observations.")
  }

  result <- switch(method,
    hp = .trend_hp(x, lambda, frequency),
    beveridge_nelson = .trend_bn(x),
    exponential_smooth = .trend_es(x),
    moving_average = .trend_ma(x, window, frequency)
  )

  structure(
    list(
      trend = result$trend,
      cycle = x - result$trend,
      method = method,
      lambda = result$lambda,
      window = result$window,
      alpha = result$alpha,
      original = x
    ),
    class = "ik_trend"
  )
}

#' HP filter: solve (I + lambda * K'K)^{-1} x
#' @noRd
.trend_hp <- function(x, lambda, frequency) {
  n <- length(x)

  if (is.null(lambda)) {
    lambda <- switch(frequency,
      annual = 6.25,
      quarterly = 1600,
      monthly = 14400
    )
  }

  validate_scalar(lambda, "lambda")

  # Build second-difference matrix K (n-2 x n)
  K <- matrix(0, nrow = n - 2L, ncol = n)
  for (i in seq_len(n - 2L)) {
    K[i, i] <- 1
    K[i, i + 1L] <- -2
    K[i, i + 2L] <- 1
  }

  # Solve: trend = (I + lambda * K'K)^{-1} x
  I_n <- diag(n)
  A <- I_n + lambda * crossprod(K)
  trend <- solve(A, x)

  list(trend = as.numeric(trend), lambda = lambda, window = NULL, alpha = NULL)
}

#' Beveridge-Nelson decomposition (Morley 2002)
#'
#' Computes the BN trend by fitting an AR(p) to first differences and
#' calculating expected future cumulative changes via the companion form.
#' @noRd
.trend_bn <- function(x) {
  n <- length(x)

  # Step 1: Compute first differences
  dx <- diff(x)
  n_dx <- length(dx)

  # Step 2: Fit AR(p) to first differences (select p by BIC)
  p <- .select_ar_order(dx, max_order = min(12L, n_dx - 3L), ic = "bic")

  if (p == 0L) {
    # No dynamics in differences: trend is a random walk, cycle is zero
    trend <- x
    return(list(trend = trend, lambda = NULL, window = NULL, alpha = NULL))
  }

  ar_coefs <- .fit_ar(dx, p)

  # Step 3: Check for unit root in differences (sum of AR coefs near 1)
  sum_phi <- sum(ar_coefs)
  if (abs(1 - sum_phi) < 1e-10) {
    trend <- x
    return(list(trend = trend, lambda = NULL, window = NULL, alpha = NULL))
  }

  # Step 4: Build companion form matrix A (p x p)
  A <- matrix(0, nrow = p, ncol = p)
  A[1, ] <- ar_coefs
  if (p > 1L) {
    for (i in 2:p) {
      A[i, i - 1L] <- 1
    }
  }

  # Step 5: Compute (I - A)^{-1} A for expected future cumulative change
  I_p <- diag(p)
  ImA_inv <- solve(I_p - A)
  G <- ImA_inv %*% A  # G maps state vector to expected future cumulative change

  # e1 selects the first element: expected_change = e1' G s_t
  # which equals G[1, ] %*% s_t

  # Step 6: Get residuals from the AR fit on differences
  resid_dx <- .ar_residuals(dx, ar_coefs)

  # Step 7: Compute BN trend for each time point
  trend <- rep(NA_real_, n)

  # We need p lagged residuals to form the state vector.
  # Residuals are available from index (p+1) of dx onward.
  # That corresponds to original series indices (p+2) onward.
  # For earlier indices, use NA.

  # Pad residuals: resid_dx corresponds to dx[(p+1):n_dx], i.e. x[(p+2):n]
  # We need residuals aligned to dx indices
  resid_full <- rep(NA_real_, n_dx)
  resid_full[(p + 1L):n_dx] <- resid_dx

  for (t in seq_len(n)) {
    # t indexes x[t]; corresponding dx index is t-1 (dx[t-1] = x[t] - x[t-1])
    # State vector at time t uses dx residuals at dx indices (t-1), (t-2), ..., (t-p)
    dx_idx <- t - 1L  # most recent dx index
    if (dx_idx < p) {
      trend[t] <- NA_real_
      next
    }

    # Build state vector: [resid at dx_idx, resid at dx_idx-1, ..., resid at dx_idx-p+1]
    state_indices <- dx_idx:(dx_idx - p + 1L)
    if (any(state_indices < 1L) || any(is.na(resid_full[state_indices]))) {
      trend[t] <- NA_real_
      next
    }

    s_t <- resid_full[state_indices]
    expected_cum_change <- sum(G[1, ] * s_t)
    trend[t] <- x[t] + expected_cum_change
  }

  list(trend = as.numeric(trend), lambda = NULL, window = NULL, alpha = NULL)
}

#' Exponential smoothing with optimised alpha
#' @noRd
.trend_es <- function(x) {
  n <- length(x)

  # Optimise alpha to minimise MSE (one-step-ahead forecast errors)
  obj_fn <- function(alpha) {
    s <- numeric(n)
    s[1] <- x[1]
    for (t in 2:n) {
      s[t] <- alpha * x[t] + (1 - alpha) * s[t - 1]
    }
    # MSE of one-step-ahead forecasts
    errors <- x[2:n] - s[1:(n - 1)]
    mean(errors^2)
  }

  opt <- optim(
    par = 0.3,
    fn = obj_fn,
    method = "Brent",
    lower = 0.01,
    upper = 0.99
  )
  alpha <- opt$par

  # Compute smoothed series with optimal alpha
  trend <- numeric(n)
  trend[1] <- x[1]
  for (t in 2:n) {
    trend[t] <- alpha * x[t] + (1 - alpha) * trend[t - 1]
  }

  list(trend = trend, lambda = NULL, window = NULL, alpha = alpha)
}

#' Centred moving average
#' @noRd
.trend_ma <- function(x, window, frequency) {
  n <- length(x)

  if (is.null(window)) {
    window <- switch(frequency,
      annual = 3L,
      quarterly = 4L,
      monthly = 12L
    )
  }

  validate_positive_integer(window, "window")

  if (window > n) {
    cli_abort("{.arg window} ({window}) cannot exceed series length ({n}).")
  }

  trend <- rep(NA_real_, n)
  half <- floor(window / 2)

  for (t in seq_len(n)) {
    lo <- max(1L, t - half)
    hi <- min(n, t + half)
    # For even windows, adjust to keep it centred
    if (window %% 2 == 0L) {
      hi <- min(n, t + half - 1L)
    }
    trend[t] <- mean(x[lo:hi])
  }

  list(trend = trend, lambda = NULL, window = window, alpha = NULL)
}

#' @export
print.ik_trend <- function(x, ...) {
  cli_h1("Trend Inflation")

  method_label <- switch(x$method,
    hp = paste0("Hodrick-Prescott (lambda = ", x$lambda, ")"),
    beveridge_nelson = "Beveridge-Nelson Decomposition",
    exponential_smooth = paste0("Exponential Smoothing (alpha = ",
                                round(x$alpha, 4), ")"),
    moving_average = paste0("Moving Average (window = ", x$window, ")")
  )

  mean_trend <- mean(x$trend, na.rm = TRUE)
  cycle_vol <- sd(x$cycle, na.rm = TRUE)

  cli_bullets(c(
    "*" = "Method: {method_label}",
    "*" = "Mean trend: {round(mean_trend, 4)}",
    "*" = "Cycle volatility (SD): {round(cycle_vol, 4)}",
    "*" = "Observations: {length(x$original)}"
  ))
  invisible(x)
}

#' @export
plot.ik_trend <- function(x, ...) {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  n <- length(x$original)
  idx <- seq_len(n)

  ylim <- range(c(x$original, x$trend), na.rm = TRUE)
  ylim <- ylim + c(-1, 1) * diff(ylim) * 0.1

  method_label <- switch(x$method,
    hp = "Hodrick-Prescott",
    beveridge_nelson = "Beveridge-Nelson",
    exponential_smooth = "Exponential Smoothing",
    moving_average = "Moving Average"
  )

  par(mar = c(4, 4, 3, 1))
  plot(
    idx, x$original,
    type = "l",
    col = adjustcolor("grey50", 0.7),
    lwd = 1.5,
    ylim = ylim,
    xlab = "Period",
    ylab = "Inflation",
    main = paste("Trend Inflation:", method_label),
    ...
  )
  lines(idx, x$trend, col = "#B2182B", lwd = 2.5)
  grid(col = "grey90")
  legend(
    "topright",
    legend = c("Original", "Trend"),
    col = c("grey50", "#B2182B"),
    lwd = c(1.5, 2.5),
    bty = "n"
  )

  invisible(x)
}
