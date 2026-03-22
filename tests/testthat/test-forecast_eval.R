# test-forecast_eval.R

test_that("ik_forecast_eval returns correct class", {
  actual <- rnorm(50, 2, 0.5)
  forecast <- actual + rnorm(50, 0, 0.2)
  result <- ik_forecast_eval(actual, forecast, test = "bias")
  expect_s3_class(result, "ik_forecast_eval")
})

test_that("bias test returns expected elements", {
  actual <- rnorm(50, 2, 0.5)
  forecast <- actual + rnorm(50, 0, 0.2)
  result <- ik_forecast_eval(actual, forecast, test = "bias")
  expect_true(all(c("test", "statistic", "p_value", "coefficients",
                     "conclusion") %in% names(result)))
})

test_that("bias test: near-perfect forecast has alpha near 0 and beta near 1", {
  set.seed(42)
  actual <- rnorm(200, 2, 0.5)
  forecast <- actual + rnorm(200, 0, 0.01)  # near-perfect forecast
  result <- ik_forecast_eval(actual, forecast, test = "bias")
  expect_equal(result$coefficients["alpha"], c(alpha = 0), tolerance = 0.1)
  expect_equal(result$coefficients["beta"], c(beta = 1), tolerance = 0.1)
})

test_that("bias test: biased forecast is detected", {
  set.seed(42)
  actual <- rnorm(100, 2, 0.5)
  forecast <- actual + 2 + rnorm(100, 0, 0.1)  # systematic bias
  result <- ik_forecast_eval(actual, forecast, test = "bias")
  # With large constant bias, should reject unbiasedness
  expect_true(result$p_value < 0.05)
})

test_that("efficiency test returns expected elements", {
  actual <- rnorm(50, 2, 0.5)
  forecast <- actual + rnorm(50, 0, 0.2)
  result <- ik_forecast_eval(actual, forecast, test = "efficiency")
  expect_true("gamma" %in% names(result$coefficients))
})

test_that("efficiency test: random errors should pass", {
  set.seed(42)
  actual <- rnorm(100, 2, 0.5)
  forecast <- actual + rnorm(100, 0, 0.2)  # iid errors
  result <- ik_forecast_eval(actual, forecast, test = "efficiency")
  # With iid errors, gamma should be near 0
  expect_equal(unname(result$coefficients["gamma"]), 0, tolerance = 0.3)
})

test_that("DM test requires forecast2", {
  actual <- rnorm(50, 2, 0.5)
  forecast <- actual + rnorm(50, 0, 0.2)
  expect_error(ik_forecast_eval(actual, forecast, test = "dm"))
})

test_that("DM test: identical forecasts give error (constant loss differentials)", {
  set.seed(42)
  actual <- rnorm(50, 2, 0.5)
  forecast1 <- actual + rnorm(50, 0, 0.2)
  forecast2 <- forecast1  # identical
  expect_error(
    ik_forecast_eval(actual, forecast1, test = "dm", forecast2 = forecast2),
    "constant"
  )
})

test_that("DM test: nearly identical forecasts give stat near 0", {
  set.seed(42)
  actual <- rnorm(100, 2, 0.5)
  forecast1 <- actual + rnorm(100, 0, 0.3)
  forecast2 <- forecast1 + rnorm(100, 0, 1e-6)  # nearly identical
  result <- ik_forecast_eval(actual, forecast1, test = "dm",
                             forecast2 = forecast2)
  expect_equal(result$statistic, 0, tolerance = 1)
})

test_that("DM test: better forecast is identified", {
  set.seed(42)
  actual <- rnorm(100, 2, 0.5)
  forecast1 <- actual + rnorm(100, 0, 0.1)  # more accurate
  forecast2 <- actual + rnorm(100, 0, 1.0)  # less accurate
  result <- ik_forecast_eval(actual, forecast1, test = "dm",
                             forecast2 = forecast2)
  # forecast1 has smaller errors, so mean loss diff should be negative
  expect_true(result$coefficients["mean_loss_diff"] < 0)
})

test_that("DM test: alternative hypothesis options work", {
  set.seed(42)
  actual <- rnorm(50, 2, 0.5)
  f1 <- actual + rnorm(50, 0, 0.2)
  f2 <- actual + rnorm(50, 0, 0.3)
  r1 <- ik_forecast_eval(actual, f1, test = "dm", forecast2 = f2,
                          alternative = "two.sided")
  r2 <- ik_forecast_eval(actual, f1, test = "dm", forecast2 = f2,
                          alternative = "less")
  r3 <- ik_forecast_eval(actual, f1, test = "dm", forecast2 = f2,
                          alternative = "greater")
  expect_true(is.numeric(r1$p_value))
  expect_true(is.numeric(r2$p_value))
  expect_true(is.numeric(r3$p_value))
})

test_that("ik_forecast_eval validates unequal lengths", {
  expect_error(ik_forecast_eval(1:10, 1:5, test = "bias"))
})

test_that("ik_forecast_eval validates non-numeric input", {
  expect_error(ik_forecast_eval("not numeric", 1:10, test = "bias"))
  expect_error(ik_forecast_eval(1:10, "not numeric", test = "bias"))
})

test_that("DM test validates forecast2 length", {
  expect_error(
    ik_forecast_eval(1:10, 1:10, test = "dm", forecast2 = 1:5)
  )
})

test_that("test name is stored in result", {
  actual <- rnorm(50, 2, 0.5)
  forecast <- actual + rnorm(50, 0, 0.2)
  r_bias <- ik_forecast_eval(actual, forecast, test = "bias")
  r_eff <- ik_forecast_eval(actual, forecast, test = "efficiency")
  expect_equal(r_bias$test, "Mincer-Zarnowitz Bias Test")
  expect_equal(r_eff$test, "Nordhaus Efficiency Test")
})

test_that("print.ik_forecast_eval runs without error", {
  actual <- rnorm(50, 2, 0.5)
  forecast <- actual + rnorm(50, 0, 0.2)
  result <- ik_forecast_eval(actual, forecast, test = "bias")
  expect_no_error(capture.output(print(result)))
})

test_that("print.ik_forecast_eval returns invisibly", {
  actual <- rnorm(50, 2, 0.5)
  forecast <- actual + rnorm(50, 0, 0.2)
  result <- ik_forecast_eval(actual, forecast, test = "bias")
  out <- capture.output(ret <- print(result))
  expect_identical(ret, result)
})
