# test-phillips.R

test_that("ik_phillips returns correct class", {
  data <- ik_sample_data("headline")
  pc <- ik_phillips(data$inflation, data$unemployment, type = "traditional")
  expect_s3_class(pc, "ik_phillips")
})

test_that("ik_phillips returns expected elements", {
  data <- ik_sample_data("headline")
  pc <- ik_phillips(data$inflation, data$unemployment, type = "traditional")
  expected_names <- c("coefficients", "std_errors", "p_values", "r_squared",
                      "type", "slope_estimate", "n_obs", "residuals",
                      ".slack", ".inflation")
  expect_true(all(expected_names %in% names(pc)))
})

test_that("slope is negative for data with negative Phillips curve relationship", {
  set.seed(42)
  n <- 100
  slack <- rnorm(n, 0, 1)
  inflation <- 2 - 0.5 * slack + rnorm(n, 0, 0.1)
  pc <- ik_phillips(inflation, slack, type = "traditional", lags = 1L)
  # Slope should be near -0.5
  expect_true(pc$slope_estimate < 0)
  expect_equal(pc$slope_estimate, -0.5, tolerance = 0.2)
})

test_that("r_squared is between 0 and 1", {
  data <- ik_sample_data("headline")
  pc <- ik_phillips(data$inflation, data$unemployment, type = "traditional")
  expect_true(pc$r_squared >= 0 && pc$r_squared <= 1)
})

test_that("n_obs is correct given lags", {
  data <- ik_sample_data("headline")
  pc <- ik_phillips(data$inflation, data$unemployment, type = "traditional",
                    lags = 4L)
  expect_equal(pc$n_obs, length(data$inflation) - 4L)
})

test_that("coefficients include slack and lag terms", {
  data <- ik_sample_data("headline")
  pc <- ik_phillips(data$inflation, data$unemployment, type = "traditional",
                    lags = 2L)
  expect_true("slack" %in% names(pc$coefficients))
  expect_true("inflation_lag1" %in% names(pc$coefficients))
  expect_true("inflation_lag2" %in% names(pc$coefficients))
})

test_that("ik_phillips validates unequal lengths", {
  expect_error(ik_phillips(1:10, 1:5, type = "traditional"))
})

test_that("ik_phillips validates non-numeric input", {
  expect_error(ik_phillips("not numeric", 1:10, type = "traditional"))
  expect_error(ik_phillips(1:10, "not numeric", type = "traditional"))
})

test_that("ik_phillips errors with too few observations", {
  expect_error(
    ik_phillips(1:5, 1:5, type = "traditional", lags = 4L)
  )
})

test_that("expectations_augmented type requires expectations", {
  data <- ik_sample_data("headline")
  expect_error(
    ik_phillips(data$inflation, data$unemployment,
                type = "expectations_augmented")
  )
})

test_that("expectations_augmented type works with expectations", {
  data <- ik_sample_data("headline")
  exp_data <- data$inflation + rnorm(nrow(data), 0, 0.1)
  pc <- ik_phillips(data$inflation, data$unemployment,
                    expectations = exp_data,
                    type = "expectations_augmented")
  expect_s3_class(pc, "ik_phillips")
  expect_true("expectations" %in% names(pc$coefficients))
})

test_that("hybrid type works", {
  data <- ik_sample_data("headline")
  exp_data <- data$inflation + rnorm(nrow(data), 0, 0.1)
  pc <- ik_phillips(data$inflation, data$unemployment,
                    expectations = exp_data,
                    type = "hybrid")
  expect_s3_class(pc, "ik_phillips")
  expect_equal(pc$type, "hybrid")
})

test_that("robust_se option works", {
  data <- ik_sample_data("headline")
  pc_ols <- ik_phillips(data$inflation, data$unemployment,
                        type = "traditional", robust_se = FALSE)
  pc_rob <- ik_phillips(data$inflation, data$unemployment,
                        type = "traditional", robust_se = TRUE)
  # Both should produce results; SEs may differ
  expect_true(is.numeric(pc_rob$std_errors))
  expect_equal(length(pc_ols$std_errors), length(pc_rob$std_errors))
})

test_that("HAC standard errors produce different results from OLS", {
  # Generate data with autocorrelated errors
  set.seed(42)
  n <- 100
  slack <- rnorm(n, 0, 1)
  # AR(1) errors induce autocorrelation
  e <- numeric(n)
  e[1] <- rnorm(1)
  for (t in 2:n) e[t] <- 0.7 * e[t - 1] + rnorm(1)
  inflation <- 2 - 0.5 * slack + e

  pc_ols <- ik_phillips(inflation, slack, type = "traditional",
                        lags = 1L, robust_se = FALSE)
  pc_hac <- ik_phillips(inflation, slack, type = "traditional",
                        lags = 1L, robust_se = "HAC")

  # HAC SEs should differ from OLS SEs
  expect_true(is.numeric(pc_hac$std_errors))
  expect_false(all(abs(pc_ols$std_errors - pc_hac$std_errors) < 1e-10))
})

test_that("robust_se = TRUE still works (backward compatibility)", {
  data <- ik_sample_data("headline")
  pc <- ik_phillips(data$inflation, data$unemployment,
                    type = "traditional", robust_se = TRUE)
  expect_s3_class(pc, "ik_phillips")
  expect_true(is.numeric(pc$std_errors))
})

test_that("robust_se = 'HC1' gives same results as robust_se = TRUE", {
  data <- ik_sample_data("headline")
  pc_true <- ik_phillips(data$inflation, data$unemployment,
                         type = "traditional", robust_se = TRUE)
  pc_hc1 <- ik_phillips(data$inflation, data$unemployment,
                         type = "traditional", robust_se = "HC1")
  expect_equal(pc_true$std_errors, pc_hc1$std_errors, tolerance = 1e-10)
})

test_that("type is stored correctly", {
  data <- ik_sample_data("headline")
  pc <- ik_phillips(data$inflation, data$unemployment, type = "traditional")
  expect_equal(pc$type, "traditional")
})

test_that("print.ik_phillips runs without error", {
  data <- ik_sample_data("headline")
  pc <- ik_phillips(data$inflation, data$unemployment, type = "traditional")
  expect_no_error(capture.output(print(pc)))
})

test_that("print.ik_phillips returns invisibly", {
  data <- ik_sample_data("headline")
  pc <- ik_phillips(data$inflation, data$unemployment, type = "traditional")
  out <- capture.output(result <- print(pc))
  expect_identical(result, pc)
})

test_that("plot.ik_phillips runs without error", {
  data <- ik_sample_data("headline")
  pc <- ik_phillips(data$inflation, data$unemployment, type = "traditional")
  expect_no_error(plot(pc))
})

test_that("plot.ik_phillips returns invisibly", {
  data <- ik_sample_data("headline")
  pc <- ik_phillips(data$inflation, data$unemployment, type = "traditional")
  result <- plot(pc)
  expect_identical(result, pc)
})
