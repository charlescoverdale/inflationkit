# test-persistence.R

test_that("ik_persistence returns correct class", {
  data <- ik_sample_data("headline")
  p <- ik_persistence(data$inflation, method = "sum_ar")
  expect_s3_class(p, "ik_persistence")
})

test_that("ik_persistence returns expected elements", {
  data <- ik_sample_data("headline")
  p <- ik_persistence(data$inflation, method = "sum_ar")
  expect_true(all(c("value", "method", "ar_order", "ar_coefficients",
                     "interpretation") %in% names(p)))
})

test_that("sum_ar on known AR(1) process recovers persistence near rho", {
  set.seed(42)
  n <- 500
  rho <- 0.8
  x <- numeric(n)
  x[1] <- 0
  for (t in 2:n) x[t] <- rho * x[t - 1] + rnorm(1, 0, 1)

  p <- ik_persistence(x, method = "sum_ar", ar_order = 1L)
  # With AR(1), sum_ar = first AR coefficient, should be near 0.8

  expect_equal(p$value, rho, tolerance = 0.1)
  expect_length(p$ar_coefficients, 1L)
})

test_that("half_life is computed correctly for AR(1)", {
  set.seed(42)
  n <- 500
  rho <- 0.7
  x <- numeric(n)
  x[1] <- 0
  for (t in 2:n) x[t] <- rho * x[t - 1] + rnorm(1, 0, 1)

  p <- ik_persistence(x, method = "half_life", ar_order = 1L)
  # half_life = -log(2)/log(rho), rho ~0.7 => hl ~1.94
  expected_hl <- -log(2) / log(rho)
  expect_equal(p$value, expected_hl, tolerance = 0.5)
})

test_that("largest_root works", {
  data <- ik_sample_data("headline")
  p <- ik_persistence(data$inflation, method = "largest_root", ar_order = 2L)
  expect_true(is.numeric(p$value))
  expect_true(p$value >= 0)
})

test_that("ar_order is selected automatically when NULL", {
  data <- ik_sample_data("headline")
  p <- ik_persistence(data$inflation, method = "sum_ar", ar_order = NULL)
  expect_true(p$ar_order >= 1L)
  expect_length(p$ar_coefficients, p$ar_order)
})

test_that("AIC and BIC can select different orders", {
  data <- ik_sample_data("headline")
  p_bic <- ik_persistence(data$inflation, method = "sum_ar", ic = "bic")
  p_aic <- ik_persistence(data$inflation, method = "sum_ar", ic = "aic")
  # Both should run without error; orders may differ
  expect_true(p_bic$ar_order >= 1L)
  expect_true(p_aic$ar_order >= 1L)
})

test_that("ik_persistence errors with too few observations", {
  expect_error(ik_persistence(1:5, method = "sum_ar"))
})

test_that("ik_persistence validates numeric input", {
  expect_error(ik_persistence("not numeric", method = "sum_ar"))
  expect_error(ik_persistence(c(1, 2, NA, 4, 5, 6, 7, 8, 9, 10), method = "sum_ar"))
})

test_that("interpretation labels are correct for sum_ar", {
  data <- ik_sample_data("headline")
  p <- ik_persistence(data$inflation, method = "sum_ar")
  expect_true(grepl("persistence", p$interpretation, ignore.case = TRUE))
})

test_that("interpretation labels are correct for half_life", {
  data <- ik_sample_data("headline")
  p <- ik_persistence(data$inflation, method = "half_life")
  expect_true(grepl("persistence", p$interpretation, ignore.case = TRUE))
})

test_that("method is stored correctly", {
  data <- ik_sample_data("headline")
  expect_equal(
    ik_persistence(data$inflation, method = "sum_ar")$method, "sum_ar"
  )
  expect_equal(
    ik_persistence(data$inflation, method = "half_life")$method, "half_life"
  )
  expect_equal(
    ik_persistence(data$inflation, method = "largest_root")$method, "largest_root"
  )
})

test_that("print.ik_persistence runs without error", {
  data <- ik_sample_data("headline")
  p <- ik_persistence(data$inflation, method = "sum_ar")
  expect_no_error(capture.output(print(p)))
})

test_that("print.ik_persistence returns invisibly", {
  data <- ik_sample_data("headline")
  p <- ik_persistence(data$inflation, method = "sum_ar")
  out <- capture.output(result <- print(p))
  expect_identical(result, p)
})

test_that("ar_order validation rejects non-positive integer", {
  data <- ik_sample_data("headline")
  expect_error(ik_persistence(data$inflation, method = "sum_ar", ar_order = 0L))
  expect_error(ik_persistence(data$inflation, method = "sum_ar", ar_order = -1L))
})
