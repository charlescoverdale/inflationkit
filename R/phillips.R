#' Estimate a Phillips curve
#'
#' Fits a Phillips curve relating inflation to an economic slack measure
#' (output gap or unemployment rate). Supports traditional, expectations-
#' augmented, and hybrid specifications.
#'
#' @param inflation Numeric vector. Inflation rate series.
#' @param slack Numeric vector. Slack measure (output gap or unemployment rate),
#'   same length as `inflation`.
#' @param expectations Numeric vector or `NULL`. Inflation expectations series,
#'   required for `"expectations_augmented"` and `"hybrid"` types. Must be the
#'   same length as `inflation`.
#' @param type Character. Phillips curve specification: `"traditional"`,
#'   `"expectations_augmented"`, or `"hybrid"`.
#' @param lags Integer. Number of lagged inflation terms to include. Default
#'   `4`.
#' @param robust_se Logical or character. If `FALSE`, use OLS standard errors.
#'   If `TRUE` or `"HC1"`, compute HC1 heteroskedasticity-robust standard
#'   errors. If `"HAC"`, compute Newey-West heteroskedasticity and
#'   autocorrelation consistent standard errors with automatic bandwidth
#'   selection (Newey and West, 1994). Default `FALSE`.
#'
#' @return An S3 object of class `"ik_phillips"` with elements:
#' \describe{
#'   \item{coefficients}{Named numeric vector of estimated coefficients.}
#'   \item{std_errors}{Named numeric vector of standard errors.}
#'   \item{p_values}{Named numeric vector of p-values.}
#'   \item{r_squared}{Numeric. R-squared of the regression.}
#'   \item{type}{Character. The Phillips curve type.}
#'   \item{slope_estimate}{Numeric. The estimated slope on the slack variable.}
#'   \item{n_obs}{Integer. Number of observations used.}
#'   \item{residuals}{Numeric vector. Regression residuals.}
#' }
#'
#' @export
#' @examples
#' data <- ik_sample_data("headline")
#' pc <- ik_phillips(data$inflation, data$unemployment, type = "traditional")
#' print(pc)
#' plot(pc)
ik_phillips <- function(inflation,
                        slack,
                        expectations = NULL,
                        type = c("traditional", "expectations_augmented",
                                 "hybrid"),
                        lags = 4L,
                        robust_se = FALSE) {
  type <- match.arg(type)
  validate_numeric_vector(inflation, "inflation")
  validate_numeric_vector(slack, "slack")

  if (length(inflation) != length(slack)) {
    cli_abort("{.arg inflation} and {.arg slack} must have the same length.")
  }

  if (type %in% c("expectations_augmented", "hybrid")) {
    if (is.null(expectations)) {
      cli_abort(
        "{.arg expectations} is required for type {.val {type}}."
      )
    }
    validate_numeric_vector(expectations, "expectations")
    if (length(expectations) != length(inflation)) {
      cli_abort(
        "{.arg expectations} must have the same length as {.arg inflation}."
      )
    }
  }

  validate_positive_integer(lags, "lags")
  n <- length(inflation)

  if (n <= lags + 2L) {
    cli_abort("Series too short for {lags} lags. Need at least {lags + 3} observations.")
  }

  # Build design matrix
  # Effective sample starts at lags + 1
  start <- lags + 1L
  y <- inflation[start:n]

  # Lagged inflation
  lag_mat <- matrix(NA_real_, nrow = length(y), ncol = lags)
  for (j in seq_len(lags)) {
    lag_mat[, j] <- inflation[(start - j):(n - j)]
  }
  colnames(lag_mat) <- paste0("inflation_lag", seq_len(lags))

  # Slack (contemporaneous)
  slack_vec <- slack[start:n]

  # Build formula data
  reg_data <- data.frame(y = y, slack = slack_vec, lag_mat)

  formula_str <- "y ~ slack"
  for (j in seq_len(lags)) {
    formula_str <- paste0(formula_str, " + inflation_lag", j)
  }

  if (type == "expectations_augmented") {
    reg_data$expectations <- expectations[start:n]
    formula_str <- paste0(formula_str, " + expectations")
  } else if (type == "hybrid") {
    reg_data$expectations <- expectations[start:n]
    formula_str <- paste0(formula_str, " + expectations")
  }

  fit <- lm(as.formula(formula_str), data = reg_data)
  summ <- summary(fit)

  # Standard errors
  # Backward compatibility: TRUE maps to "HC1"
  if (is.logical(robust_se)) {
    robust_se <- if (robust_se) "HC1" else FALSE
  }

  if (identical(robust_se, "HC1")) {
    se <- .hc1_se(fit)
    coefs <- coef(fit)
    t_stats <- coefs / se
    p_vals <- 2 * pt(abs(t_stats), df = fit$df.residual, lower.tail = FALSE)
  } else if (identical(robust_se, "HAC")) {
    se <- .hac_se(fit)
    coefs <- coef(fit)
    t_stats <- coefs / se
    p_vals <- 2 * pt(abs(t_stats), df = fit$df.residual, lower.tail = FALSE)
  } else {
    se <- summ$coefficients[, 2]
    p_vals <- summ$coefficients[, 4]
  }

  coefs <- coef(fit)
  slope_est <- unname(coefs["slack"])

  structure(
    list(
      coefficients = coefs,
      std_errors = se,
      p_values = p_vals,
      r_squared = summ$r.squared,
      type = type,
      slope_estimate = slope_est,
      n_obs = length(y),
      residuals = unname(residuals(fit)),
      .slack = slack_vec,
      .inflation = y
    ),
    class = "ik_phillips"
  )
}

#' HC1 robust standard errors (no external dependency)
#' @noRd
.hc1_se <- function(fit) {
  X <- model.matrix(fit)
  e <- residuals(fit)
  n <- length(e)
  k <- ncol(X)

  # HC1 adjustment factor
  hc1_factor <- n / (n - k)

  # Meat: X' diag(e^2) X
  meat <- crossprod(X, X * (e^2))
  bread <- solve(crossprod(X))

  V <- hc1_factor * bread %*% meat %*% bread
  se <- sqrt(diag(V))
  names(se) <- names(coef(fit))
  se
}

#' Newey-West HAC standard errors (no external dependency)
#'
#' Uses Bartlett kernel with automatic bandwidth selection following
#' Newey and West (1994): floor(4 * (n / 100)^(2/9)).
#' @noRd
.hac_se <- function(fit) {
  X <- model.matrix(fit)
  e <- residuals(fit)
  n <- length(e)
  k <- ncol(X)

  # Automatic bandwidth (Newey-West 1994 plug-in)
  L <- floor(4 * (n / 100)^(2 / 9))

  # Compute S = sum of weighted autocovariance matrices (Bartlett kernel)
  # Gamma_0
  S <- crossprod(X * e)

  for (j in seq_len(L)) {
    w_j <- 1 - j / (L + 1)  # Bartlett kernel weight
    # Gamma_j = sum_{t=j+1}^{n} (e_t * x_t) (e_{t-j} * x_{t-j})'
    Gamma_j <- crossprod(X[(j + 1):n, , drop = FALSE] * e[(j + 1):n],
                         X[1:(n - j), , drop = FALSE] * e[1:(n - j)])
    S <- S + w_j * (Gamma_j + t(Gamma_j))
  }

  bread <- solve(crossprod(X))
  V <- bread %*% S %*% bread
  se <- sqrt(diag(V))
  names(se) <- names(coef(fit))
  se
}

#' @export
print.ik_phillips <- function(x, ...) {
  cli_h1("Phillips Curve Estimation")

  type_label <- switch(x$type,
    traditional = "Traditional",
    expectations_augmented = "Expectations-Augmented",
    hybrid = "Hybrid"
  )

  slope_pval <- x$p_values["slack"]
  sig <- if (slope_pval < 0.01) "***" else if (slope_pval < 0.05) "**" else
    if (slope_pval < 0.1) "*" else ""

 cli_bullets(c(
    "*" = "Type: {type_label}",
    "*" = "Slope estimate: {round(x$slope_estimate, 4)} (p = {round(slope_pval, 4)}) {sig}",
    "*" = "R-squared: {round(x$r_squared, 4)}",
    "*" = "Observations: {x$n_obs}"
  ))
  invisible(x)
}

#' @export
plot.ik_phillips <- function(x, ...) {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  slack <- x$.slack
  inflation <- x$.inflation

  par(mar = c(4, 4, 3, 1))
  plot(
    slack, inflation,
    pch = 19,
    col = adjustcolor("#2166AC", 0.6),
    cex = 0.8,
    xlab = "Slack",
    ylab = "Inflation",
    main = paste("Phillips Curve:", x$type),
    ...
  )

  # Fitted line (bivariate for display)
  fit_simple <- lm(inflation ~ slack)
  abline(fit_simple, col = "#B2182B", lwd = 2)
  grid(col = "grey90")

  invisible(x)
}
