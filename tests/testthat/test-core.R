# test-core.R

test_that("ik_core returns correct class", {
  data <- ik_sample_data("components")
  core <- ik_core(data, method = "trimmed_mean")
  expect_s3_class(core, "ik_core")
})

test_that("ik_core returns expected elements", {
  data <- ik_sample_data("components")
  core <- ik_core(data, method = "trimmed_mean")
  expect_true(all(c("core", "method", "trim", "exclude", "headline") %in% names(core)))
})

test_that("core data.frame has correct columns", {
  data <- ik_sample_data("components")
  core <- ik_core(data, method = "trimmed_mean")
  expect_equal(names(core$core), c("date", "core_inflation"))
  expect_equal(names(core$headline), c("date", "headline"))
})

test_that("trimmed_mean with 50% trim approaches weighted median", {
  # With a large symmetric trim, trimmed mean should be close to the median
  df <- data.frame(
    date = rep(as.Date("2020-01-01"), 5),
    item = c("A", "B", "C", "D", "E"),
    weight = rep(0.2, 5),
    price_change = c(0.01, 0.02, 0.03, 0.04, 0.05),
    stringsAsFactors = FALSE
  )
  core_tm <- ik_core(df, method = "trimmed_mean", trim = 0.4)
  core_wm <- ik_core(df, method = "weighted_median")
  # With equal weights and symmetric trim near 50%, result should be near median (0.03)
  expect_equal(core_tm$core$core_inflation, 0.03, tolerance = 0.01)
  expect_equal(core_wm$core$core_inflation, 0.03, tolerance = 1e-10)
})

test_that("weighted_median returns correct value for known case", {
  # 3 items: A(w=0.3, pc=0.01), B(w=0.4, pc=0.05), C(w=0.3, pc=0.10)
  # Sorted by pc: A(0.01, cumw=0.3), B(0.05, cumw=0.7), C(0.10, cumw=1.0)
  # First cumw >= 0.5 is B, so weighted median = 0.05
  df <- data.frame(
    date = rep(as.Date("2020-01-01"), 3),
    item = c("A", "B", "C"),
    weight = c(0.3, 0.4, 0.3),
    price_change = c(0.01, 0.05, 0.10),
    stringsAsFactors = FALSE
  )
  core <- ik_core(df, method = "weighted_median")
  expect_equal(core$core$core_inflation, 0.05)
})

test_that("exclusion method removes specified items", {
  df <- data.frame(
    date = rep(as.Date("2020-01-01"), 4),
    item = c("Food", "Housing", "Transport", "Other"),
    weight = c(0.3, 0.3, 0.2, 0.2),
    price_change = c(0.10, 0.02, 0.08, 0.01),
    stringsAsFactors = FALSE
  )
  core <- ik_core(df, method = "exclusion", exclude = c("Food", "Transport"))
  # Only Housing and Other remain: weighted.mean(c(0.02, 0.01), c(0.3, 0.2))
  expected <- weighted.mean(c(0.02, 0.01), c(0.3, 0.2))
  expect_equal(core$core$core_inflation, expected, tolerance = 1e-10)
})

test_that("exclusion method errors when exclude is NULL", {
  data <- ik_sample_data("components")
  expect_error(ik_core(data, method = "exclusion", exclude = NULL))
})

test_that("exclusion method errors when exclude is empty", {
  data <- ik_sample_data("components")
  expect_error(ik_core(data, method = "exclusion", exclude = character(0)))
})

test_that("exclusion method warns about unknown items", {
  data <- ik_sample_data("components")
  expect_warning(
    ik_core(data, method = "exclusion", exclude = c("Food", "NonExistent"))
  )
})

test_that("trimmed_mean errors for invalid trim", {
  data <- ik_sample_data("components")
  expect_error(ik_core(data, method = "trimmed_mean", trim = 0))
  expect_error(ik_core(data, method = "trimmed_mean", trim = 0.5))
  expect_error(ik_core(data, method = "trimmed_mean", trim = -0.1))
})

test_that("asymmetric_trim with known data gives expected result", {
  # 5 equal-weight items, sorted price changes
  df <- data.frame(
    date = rep(as.Date("2020-01-01"), 5),
    item = c("A", "B", "C", "D", "E"),
    weight = rep(0.2, 5),
    price_change = c(0.01, 0.02, 0.03, 0.04, 0.05),
    stringsAsFactors = FALSE
  )
  # Trim lower 20%, upper 20% => keep middle 60% (items B, C, D)
  core <- ik_core(df, method = "asymmetric_trim",
                  trim_lower = 0.2, trim_upper = 0.2)
  # With equal weights, result should be mean of B, C, D
  expected <- mean(c(0.02, 0.03, 0.04))
  expect_equal(core$core$core_inflation, expected, tolerance = 0.01)
})

test_that("asymmetric_trim with Dallas Fed defaults runs", {
  data <- ik_sample_data("components")
  core <- ik_core(data, method = "asymmetric_trim")
  expect_s3_class(core, "ik_core")
  expect_equal(core$method, "asymmetric_trim")
  expect_equal(core$trim_lower, 0.24)
  expect_equal(core$trim_upper, 0.31)
})

test_that("weight normalization warning is issued when weights do not sum to 1", {
  df <- data.frame(
    date = rep(as.Date("2020-01-01"), 3),
    item = c("A", "B", "C"),
    weight = c(0.6, 0.8, 0.6),  # sum = 2.0
    price_change = c(0.01, 0.02, 0.03),
    stringsAsFactors = FALSE
  )
  expect_warning(
    ik_core(df, method = "trimmed_mean"),
    "Weights do not sum to 1"
  )
})

test_that("ik_core validates missing columns", {
  df <- data.frame(date = Sys.Date(), item = "A")
  expect_error(ik_core(df))
})

test_that("method is stored correctly", {
  data <- ik_sample_data("components")
  expect_equal(ik_core(data, method = "trimmed_mean")$method, "trimmed_mean")
  expect_equal(ik_core(data, method = "weighted_median")$method, "weighted_median")
  expect_equal(
    ik_core(data, method = "exclusion", exclude = "Food")$method, "exclusion"
  )
})

test_that("core and headline have same number of dates", {
  data <- ik_sample_data("components")
  core <- ik_core(data, method = "trimmed_mean")
  expect_equal(nrow(core$core), nrow(core$headline))
})

test_that("print.ik_core runs without error", {
  data <- ik_sample_data("components")
  core <- ik_core(data, method = "trimmed_mean")
  expect_no_error(capture.output(print(core)))
})

test_that("print.ik_core returns invisibly", {
  data <- ik_sample_data("components")
  core <- ik_core(data, method = "weighted_median")
  out <- capture.output(result <- print(core))
  expect_identical(result, core)
})

test_that("plot.ik_core runs without error", {
  data <- ik_sample_data("components")
  core <- ik_core(data, method = "trimmed_mean")
  expect_no_error(plot(core))
})

test_that("plot.ik_core returns invisibly", {
  data <- ik_sample_data("components")
  core <- ik_core(data, method = "trimmed_mean")
  result <- plot(core)
  expect_identical(result, core)
})
