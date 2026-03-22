#' Evaluate inflation forecasts
#'
#' Runs standard forecast evaluation tests. The bias test (Mincer-Zarnowitz)
#' checks whether forecasts are unbiased. The efficiency test (Nordhaus) checks
#' whether forecast errors are autocorrelated. The Diebold-Mariano test
#' compares predictive accuracy of two competing forecasts.
#'
#' @param actual Numeric vector. Realised inflation values.
#' @param forecast Numeric vector. Forecast values, same length as `actual`.
#' @param test Character. One of `"bias"`, `"efficiency"`, or `"dm"`.
#' @param forecast2 Numeric vector or `NULL`. Second forecast series (required
#'   for `"dm"` test), same length as `actual`.
#' @param horizon Integer. Forecast horizon (used for Newey-West bandwidth in
#'   the DM test). Default `1`.
#' @param alternative Character. Alternative hypothesis for the DM test:
#'   `"two.sided"`, `"less"`, or `"greater"`.
#'
#' @return An S3 object of class `"ik_forecast_eval"` with elements:
#' \describe{
#'   \item{test}{Character. The test performed.}
#'   \item{statistic}{Numeric. The test statistic.}
#'   \item{p_value}{Numeric. The p-value.}
#'   \item{coefficients}{Named numeric vector (for bias and efficiency tests).}
#'   \item{conclusion}{Character. A plain-language summary of the result.}
#' }
#'
#' @references
#' Diebold, F. X. and Mariano, R. S. (1995). "Comparing Predictive Accuracy."
#' Journal of Business and Economic Statistics, 13(3), 253-263.
#'
#' @export
#' @examples
#' data <- ik_sample_data("headline")
#' actual <- data$inflation[5:80]
#' forecast1 <- data$inflation[4:79] + rnorm(76, 0, 0.2)
#' forecast2 <- data$inflation[4:79] + rnorm(76, 0, 0.4)
#'
#' # Mincer-Zarnowitz bias test
#' bias <- ik_forecast_eval(actual, forecast1, test = "bias")
#' print(bias)
#'
#' # Diebold-Mariano test
#' dm <- ik_forecast_eval(actual, forecast1, test = "dm", forecast2 = forecast2)
#' print(dm)
ik_forecast_eval <- function(actual,
                             forecast,
                             test = c("bias", "efficiency", "dm"),
                             forecast2 = NULL,
                             horizon = 1L,
                             alternative = c("two.sided", "less", "greater")) {
  test <- match.arg(test)
  alternative <- match.arg(alternative)
  validate_numeric_vector(actual, "actual")
  validate_numeric_vector(forecast, "forecast")

  if (length(actual) != length(forecast)) {
    cli_abort("{.arg actual} and {.arg forecast} must have the same length.")
  }

  if (test == "dm") {
    if (is.null(forecast2)) {
      cli_abort("{.arg forecast2} is required for the Diebold-Mariano test.")
    }
    validate_numeric_vector(forecast2, "forecast2")
    if (length(forecast2) != length(actual)) {
      cli_abort("{.arg forecast2} must have the same length as {.arg actual}.")
    }
  }

  result <- switch(test,
    bias = .test_bias(actual, forecast),
    efficiency = .test_efficiency(actual, forecast),
    dm = .test_dm(actual, forecast, forecast2, horizon, alternative)
  )

  structure(result, class = "ik_forecast_eval")
}

#' Mincer-Zarnowitz bias test: actual ~ alpha + beta * forecast
#' H0: alpha = 0 and beta = 1
#' @noRd
.test_bias <- function(actual, forecast) {
  fit <- lm(actual ~ forecast)
  summ <- summary(fit)
  coefs <- coef(fit)
  se <- summ$coefficients[, 2]

  alpha <- unname(coefs[1])
  beta <- unname(coefs[2])

  # Joint F-test: H0: alpha = 0, beta = 1
  # Wald test
  R <- matrix(c(1, 0, 0, 1), nrow = 2)
  r <- c(0, 1)
  diff_vec <- coefs - r
  V <- vcov(fit)
  wald_stat <- as.numeric(t(diff_vec) %*% solve(V) %*% diff_vec) / 2
  p_val <- pf(wald_stat, df1 = 2, df2 = fit$df.residual, lower.tail = FALSE)

  conclusion <- if (p_val < 0.05) {
    "Reject H0: forecasts appear biased (alpha != 0 or beta != 1)."
  } else {
    "Cannot reject H0: no evidence of forecast bias."
  }

  list(
    test = "Mincer-Zarnowitz Bias Test",
    statistic = wald_stat,
    p_value = p_val,
    coefficients = c(alpha = alpha, beta = beta),
    conclusion = conclusion
  )
}

#' Nordhaus efficiency test: e_t ~ gamma * e_{t-1}
#' H0: gamma = 0 (forecast errors are not autocorrelated)
#' @noRd
.test_efficiency <- function(actual, forecast) {
  errors <- actual - forecast
  n <- length(errors)

  if (n < 4L) {
    cli_abort("Need at least 4 observations for the efficiency test.")
  }

  e_t <- errors[2:n]
  e_lag <- errors[1:(n - 1)]

  fit <- lm(e_t ~ e_lag)
  summ <- summary(fit)
  gamma <- unname(coef(fit)[2])
  t_stat <- summ$coefficients[2, 3]
  p_val <- summ$coefficients[2, 4]

  conclusion <- if (p_val < 0.05) {
    "Reject H0: forecast errors are autocorrelated (inefficient forecasts)."
  } else {
    "Cannot reject H0: no evidence of forecast inefficiency."
  }

  list(
    test = "Nordhaus Efficiency Test",
    statistic = t_stat,
    p_value = p_val,
    coefficients = c(gamma = gamma),
    conclusion = conclusion
  )
}

#' Diebold-Mariano test
#' @noRd
.test_dm <- function(actual, forecast1, forecast2, horizon, alternative) {
  e1 <- actual - forecast1
  e2 <- actual - forecast2
  n <- length(e1)

  # Loss differential (squared error loss)
  d <- e1^2 - e2^2
  d_bar <- mean(d)

  # Newey-West HAC variance estimate
  # Bandwidth = horizon - 1
  bw <- max(0L, horizon - 1L)
  gamma_0 <- var(d) * (n - 1) / n  # population variance

  if (bw > 0L) {
    gamma_sum <- 0
    for (k in seq_len(bw)) {
      w_k <- 1 - k / (bw + 1)  # Bartlett kernel
      gamma_k <- sum((d[(k + 1):n] - d_bar) * (d[1:(n - k)] - d_bar)) / n
      gamma_sum <- gamma_sum + 2 * w_k * gamma_k
    }
    V_d <- gamma_0 + gamma_sum
  } else {
    V_d <- gamma_0
  }

  se_d <- sqrt(V_d / n)

  if (se_d < 1e-15) {
    cli_abort("Loss differentials are constant; DM test is undefined.")
  }

  dm_stat <- d_bar / se_d

  p_val <- switch(alternative,
    two.sided = 2 * pnorm(abs(dm_stat), lower.tail = FALSE),
    less = pnorm(dm_stat),
    greater = pnorm(dm_stat, lower.tail = FALSE)
  )

  conclusion <- if (p_val < 0.05) {
    if (d_bar < 0) {
      "Forecast 1 is significantly more accurate than Forecast 2."
    } else {
      "Forecast 2 is significantly more accurate than Forecast 1."
    }
  } else {
    "No significant difference in predictive accuracy between the two forecasts."
  }

  list(
    test = "Diebold-Mariano Test",
    statistic = dm_stat,
    p_value = p_val,
    coefficients = c(mean_loss_diff = d_bar),
    conclusion = conclusion
  )
}

#' @export
print.ik_forecast_eval <- function(x, ...) {
  cli_h1("Forecast Evaluation")

  cli_bullets(c(
    "*" = "Test: {x$test}",
    "*" = "Test statistic: {round(x$statistic, 4)}",
    "*" = "p-value: {round(x$p_value, 4)}",
    "*" = "{x$conclusion}"
  ))

  if (!is.null(x$coefficients)) {
    coef_str <- paste(
      names(x$coefficients), "=", round(x$coefficients, 4),
      collapse = ", "
    )
    cli_bullets(c("*" = "Coefficients: {coef_str}"))
  }

  invisible(x)
}
