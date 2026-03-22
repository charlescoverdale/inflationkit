# test-compare.R

test_that("ik_compare returns correct class", {
  data <- ik_sample_data("components")
  core_tm <- ik_core(data, method = "trimmed_mean")
  core_wm <- ik_core(data, method = "weighted_median")
  comp <- ik_compare(core_tm, core_wm)
  expect_s3_class(comp, "ik_comparison")
})

test_that("ik_compare returns expected elements", {
  data <- ik_sample_data("components")
  core_tm <- ik_core(data, method = "trimmed_mean")
  core_wm <- ik_core(data, method = "weighted_median")
  comp <- ik_compare(core_tm, core_wm)
  expect_true(all(c("measures", "labels") %in% names(comp)))
})

test_that("ik_compare stores correct number of measures", {
  data <- ik_sample_data("components")
  core_tm <- ik_core(data, method = "trimmed_mean")
  core_wm <- ik_core(data, method = "weighted_median")
  core_ex <- ik_core(data, method = "exclusion", exclude = c("Food", "Transport"))
  comp <- ik_compare(core_tm, core_wm, core_ex)
  expect_length(comp$measures, 3L)
  expect_length(comp$labels, 3L)
})

test_that("ik_compare auto-generates labels from methods", {
  data <- ik_sample_data("components")
  core_tm <- ik_core(data, method = "trimmed_mean")
  core_wm <- ik_core(data, method = "weighted_median")
  comp <- ik_compare(core_tm, core_wm)
  expect_true(grepl("Trimmed Mean", comp$labels[1]))
  expect_equal(comp$labels[2], "Weighted Median")
})

test_that("ik_compare accepts custom labels", {
  data <- ik_sample_data("components")
  core_tm <- ik_core(data, method = "trimmed_mean")
  core_wm <- ik_core(data, method = "weighted_median")
  comp <- ik_compare(core_tm, core_wm, labels = c("TM", "WM"))
  expect_equal(comp$labels, c("TM", "WM"))
})

test_that("ik_compare errors with wrong number of labels", {
  data <- ik_sample_data("components")
  core_tm <- ik_core(data, method = "trimmed_mean")
  core_wm <- ik_core(data, method = "weighted_median")
  expect_error(ik_compare(core_tm, core_wm, labels = c("TM")))
})

test_that("ik_compare errors with no arguments", {
  expect_error(ik_compare())
})

test_that("ik_compare errors with non-ik_core object", {
  data <- ik_sample_data("components")
  core_tm <- ik_core(data, method = "trimmed_mean")
  expect_error(ik_compare(core_tm, "not an ik_core"))
})

test_that("ik_compare works with single measure", {
  data <- ik_sample_data("components")
  core_tm <- ik_core(data, method = "trimmed_mean")
  comp <- ik_compare(core_tm)
  expect_length(comp$measures, 1L)
})

test_that("print.ik_comparison runs without error", {
  data <- ik_sample_data("components")
  core_tm <- ik_core(data, method = "trimmed_mean")
  core_wm <- ik_core(data, method = "weighted_median")
  comp <- ik_compare(core_tm, core_wm)
  expect_no_error(capture.output(print(comp)))
})

test_that("print.ik_comparison returns invisibly", {
  data <- ik_sample_data("components")
  core_tm <- ik_core(data, method = "trimmed_mean")
  core_wm <- ik_core(data, method = "weighted_median")
  comp <- ik_compare(core_tm, core_wm)
  out <- capture.output(result <- print(comp))
  expect_identical(result, comp)
})

test_that("plot.ik_comparison runs without error", {
  data <- ik_sample_data("components")
  core_tm <- ik_core(data, method = "trimmed_mean")
  core_wm <- ik_core(data, method = "weighted_median")
  comp <- ik_compare(core_tm, core_wm)
  expect_no_error(plot(comp))
})

test_that("plot.ik_comparison returns invisibly", {
  data <- ik_sample_data("components")
  core_tm <- ik_core(data, method = "trimmed_mean")
  core_wm <- ik_core(data, method = "weighted_median")
  comp <- ik_compare(core_tm, core_wm)
  result <- plot(comp)
  expect_identical(result, comp)
})
