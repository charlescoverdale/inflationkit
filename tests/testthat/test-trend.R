# test-trend.R

test_that("ik_trend returns correct class", {
  data <- ik_sample_data("headline")
  tr <- ik_trend(data$inflation, method = "hp")
  expect_s3_class(tr, "ik_trend")
})

test_that("ik_trend returns expected elements", {
  data <- ik_sample_data("headline")
  tr <- ik_trend(data$inflation, method = "hp")
  expected <- c("trend", "cycle", "method", "lambda", "window", "alpha", "original")
  expect_true(all(expected %in% names(tr)))
})

test_that("cycle equals original minus trend", {
  data <- ik_sample_data("headline")
  tr <- ik_trend(data$inflation, method = "hp")
  expect_equal(tr$cycle, tr$original - tr$trend, tolerance = 1e-10)
})

test_that("HP filter: trend of constant series is constant", {
  x <- rep(3.0, 50)
  tr <- ik_trend(x, method = "hp")
  expect_equal(tr$trend, x, tolerance = 1e-6)
})

test_that("HP filter: lambda is stored", {
  data <- ik_sample_data("headline")
  tr <- ik_trend(data$inflation, method = "hp", lambda = 1600)
  expect_equal(tr$lambda, 1600)
})

test_that("HP filter: lambda defaults for quarterly data", {
  x <- rnorm(80)
  tr <- ik_trend(x, method = "hp")
  expect_equal(tr$lambda, 1600)
})

test_that("HP filter: lambda defaults for monthly data", {
  x <- rnorm(300)
  tr <- ik_trend(x, method = "hp", frequency = "monthly")
  expect_equal(tr$lambda, 14400)
})

test_that("HP filter: lambda defaults for annual data", {
  x <- rnorm(50)
  tr <- ik_trend(x, method = "hp", frequency = "annual")
  expect_equal(tr$lambda, 6.25)
})

test_that("HP filter: trend has same length as original", {
  data <- ik_sample_data("headline")
  tr <- ik_trend(data$inflation, method = "hp")
  expect_length(tr$trend, length(data$inflation))
})

test_that("moving_average with known values", {
  # Simple sequence; MA(3) of [1,2,3,4,5] centred should average nearby values
  x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  tr <- ik_trend(x, method = "moving_average", window = 3)
  # Window 3, half = 1. For t=2: mean(x[1:3]) = 2
  expect_equal(tr$trend[2], mean(x[1:3]), tolerance = 1e-10)
  # For t=5: mean(x[4:6]) = 5
  expect_equal(tr$trend[5], mean(x[4:6]), tolerance = 1e-10)
})

test_that("moving_average stores window", {
  x <- rnorm(50)
  tr <- ik_trend(x, method = "moving_average", window = 6)
  expect_equal(tr$window, 6)
})

test_that("moving_average trend has same length as original", {
  x <- rnorm(50)
  tr <- ik_trend(x, method = "moving_average", window = 4)
  expect_length(tr$trend, length(x))
})

test_that("exponential_smooth works", {
  data <- ik_sample_data("headline")
  tr <- ik_trend(data$inflation, method = "exponential_smooth")
  expect_s3_class(tr, "ik_trend")
  expect_true(!is.null(tr$alpha))
  expect_true(tr$alpha > 0 && tr$alpha < 1)
})

test_that("exponential_smooth: trend of constant series is constant", {
  x <- rep(5.0, 30)
  tr <- ik_trend(x, method = "exponential_smooth")
  expect_equal(tr$trend, x, tolerance = 1e-4)
})

test_that("beveridge_nelson works", {
  data <- ik_sample_data("headline")
  tr <- ik_trend(data$inflation, method = "beveridge_nelson")
  expect_s3_class(tr, "ik_trend")
  expect_length(tr$trend, length(data$inflation))
})

test_that("BN decomposition: trend of random walk is close to series itself", {
  set.seed(42)
  n <- 200
  # Pure random walk: differences are i.i.d.
  x <- cumsum(rnorm(n, 0, 1))
  tr <- ik_trend(x, method = "beveridge_nelson")

  # For a random walk, BN trend should be very close to the series
  # (cycle should be small)
  non_na <- !is.na(tr$trend)
  expect_true(sum(non_na) > n / 2)  # most values should be non-NA
  cycle_sd <- sd(tr$cycle[non_na])
  series_sd <- sd(x[non_na])
  # Cycle volatility should be much smaller than the series itself

  expect_true(cycle_sd < series_sd)
})

test_that("BN decomposition: trend + cycle = original for non-NA values", {
  data <- ik_sample_data("headline")
  tr <- ik_trend(data$inflation, method = "beveridge_nelson")

  non_na <- !is.na(tr$trend)
  expect_true(any(non_na))
  # trend + cycle should equal original
  expect_equal(
    tr$trend[non_na] + tr$cycle[non_na],
    tr$original[non_na],
    tolerance = 1e-10
  )
})

test_that("method is stored correctly", {
  x <- rnorm(50)
  expect_equal(ik_trend(x, method = "hp")$method, "hp")
  expect_equal(ik_trend(x, method = "moving_average")$method, "moving_average")
  expect_equal(ik_trend(x, method = "exponential_smooth")$method, "exponential_smooth")
  expect_equal(ik_trend(x, method = "beveridge_nelson")$method, "beveridge_nelson")
})

test_that("ik_trend errors with too few observations", {
  expect_error(ik_trend(1:3, method = "hp"))
})

test_that("ik_trend validates numeric input", {
  expect_error(ik_trend("not numeric", method = "hp"))
  expect_error(ik_trend(c(1, 2, NA, 4, 5, 6, 7), method = "hp"))
})

test_that("moving_average errors when window exceeds series length", {
  expect_error(ik_trend(1:6, method = "moving_average", window = 10))
})

test_that("original is stored", {
  x <- rnorm(30)
  tr <- ik_trend(x, method = "hp")
  expect_equal(tr$original, x)
})

test_that("print.ik_trend runs without error", {
  data <- ik_sample_data("headline")
  tr <- ik_trend(data$inflation, method = "hp")
  expect_no_error(capture.output(print(tr)))
})

test_that("print.ik_trend returns invisibly", {
  data <- ik_sample_data("headline")
  tr <- ik_trend(data$inflation, method = "hp")
  out <- capture.output(result <- print(tr))
  expect_identical(result, tr)
})

test_that("plot.ik_trend runs without error", {
  data <- ik_sample_data("headline")
  tr <- ik_trend(data$inflation, method = "hp")
  expect_no_error(plot(tr))
})

test_that("plot.ik_trend returns invisibly", {
  data <- ik_sample_data("headline")
  tr <- ik_trend(data$inflation, method = "hp")
  result <- plot(tr)
  expect_identical(result, tr)
})
