#' Estimate trend inflation
#'
#' Extracts the trend component from an inflation series using one of four
#' methods: Hodrick-Prescott filter, Beveridge-Nelson decomposition,
#' exponential smoothing, or centred moving average.
#'
#' @param x Numeric vector. An inflation time series.
#' @param method Character. One of `"hp"`, `"beveridge_nelson"`,
#'   `"exponential_smooth"`, or `"moving_average"`.
#' @param lambda Numeric or `NULL`. Smoothing parameter for the HP filter. If
#'   `NULL`, defaults to 1600 for quarterly data (length <= 200) or 14400 for
#'   monthly data.
#' @param window Integer or `NULL`. Window size for the moving average method.
#'   Defaults to 4 for quarterly or 12 for monthly.
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
                     lambda = NULL,
                     window = NULL) {
  method <- match.arg(method)
  validate_numeric_vector(x, "x")
  n <- length(x)

  if (n < 6L) {
    cli_abort("{.arg x} must have at least 6 observations.")
  }

  result <- switch(method,
    hp = .trend_hp(x, lambda),
    beveridge_nelson = .trend_bn(x),
    exponential_smooth = .trend_es(x),
    moving_average = .trend_ma(x, window)
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
.trend_hp <- function(x, lambda) {
  n <- length(x)

  if (is.null(lambda)) {
    # Heuristic: quarterly if n <= 200, monthly otherwise
    lambda <- if (n <= 200) 1600 else 14400
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

#' Beveridge-Nelson decomposition
#' @noRd
.trend_bn <- function(x) {
  n <- length(x)

  # Fit AR model (order by BIC)
  p <- .select_ar_order(x, max_order = min(12L, n - 3L), ic = "bic")
  ar_coefs <- .fit_ar(x, p)

  if (p == 0L) {
    # No dynamics; trend = cumulative sum of mean
    trend <- rep(mean(x), n)
    return(list(trend = trend, lambda = NULL, window = NULL, alpha = NULL))
  }

  # BN trend: tau_t = x_t + sum of expected future changes
  # For AR(p): long-run impact = 1 / (1 - sum(phi))
  sum_phi <- sum(ar_coefs)

  if (abs(1 - sum_phi) < 1e-10) {
    # Unit root case: trend follows a random walk
    trend <- x
  } else {
    # BN decomposition: trend = x + psi(1)^{-1} * cumulative revision
    # Simplified: compute AR residuals, accumulate
    resid <- .ar_residuals(x, ar_coefs)
    long_run <- 1 / (1 - sum_phi)

    # Pad residuals to match x length
    resid_full <- c(rep(0, p), resid)

    # BN trend = x + long_run multiplier * cumulative adjustment
    cum_resid <- cumsum(resid_full)
    trend <- x - (long_run - 1) * resid_full
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
.trend_ma <- function(x, window) {
  n <- length(x)

  if (is.null(window)) {
    window <- if (n <= 200) 4L else 12L
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
