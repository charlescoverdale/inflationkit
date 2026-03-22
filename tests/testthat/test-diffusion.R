# test-diffusion.R

test_that("ik_diffusion returns correct class", {
  data <- ik_sample_data("components")
  d <- ik_diffusion(data, threshold = "zero")
  expect_s3_class(d, "ik_diffusion")
})

test_that("ik_diffusion returns expected elements", {
  data <- ik_sample_data("components")
  d <- ik_diffusion(data, threshold = "zero")
  expect_true(all(c("diffusion", "threshold", "weighted") %in% names(d)))
})

test_that("diffusion data.frame has correct columns", {
  data <- ik_sample_data("components")
  d <- ik_diffusion(data, threshold = "zero")
  expect_equal(names(d$diffusion), c("date", "diffusion_index"))
})

test_that("diffusion index is between 0 and 1", {
  data <- ik_sample_data("components")
  d <- ik_diffusion(data, threshold = "zero")
  expect_true(all(d$diffusion$diffusion_index >= 0))
  expect_true(all(d$diffusion$diffusion_index <= 1))
})

test_that("unweighted diffusion with known data", {
  # 4 items, 2 positive, 2 negative => unweighted fraction = 0.5
  df <- data.frame(
    date = rep(as.Date("2020-01-01"), 4),
    item = c("A", "B", "C", "D"),
    weight = rep(0.25, 4),
    price_change = c(0.05, 0.03, -0.02, -0.01),
    stringsAsFactors = FALSE
  )
  d <- ik_diffusion(df, threshold = "zero", weighted = FALSE)
  expect_equal(d$diffusion$diffusion_index, 0.5)
})

test_that("weighted diffusion with known data", {
  # A(w=0.5, pc=0.05), B(w=0.1, pc=0.03), C(w=0.3, pc=-0.02), D(w=0.1, pc=-0.01)
  # Items above zero: A(0.5) + B(0.1) = 0.6
  df <- data.frame(
    date = rep(as.Date("2020-01-01"), 4),
    item = c("A", "B", "C", "D"),
    weight = c(0.5, 0.1, 0.3, 0.1),
    price_change = c(0.05, 0.03, -0.02, -0.01),
    stringsAsFactors = FALSE
  )
  d <- ik_diffusion(df, threshold = "zero", weighted = TRUE)
  expect_equal(d$diffusion$diffusion_index, 0.6, tolerance = 1e-10)
})

test_that("threshold 'mean' works correctly", {
  # 3 items with pc: 0.01, 0.02, 0.06; mean = 0.03
  # Items above mean: only 0.06 => unweighted 1/3
  df <- data.frame(
    date = rep(as.Date("2020-01-01"), 3),
    item = c("A", "B", "C"),
    weight = rep(1/3, 3),
    price_change = c(0.01, 0.02, 0.06),
    stringsAsFactors = FALSE
  )
  d <- ik_diffusion(df, threshold = "mean", weighted = FALSE)
  expect_equal(d$diffusion$diffusion_index, 1/3, tolerance = 1e-10)
})

test_that("threshold 'target' works", {
  data <- ik_sample_data("components")
  d <- ik_diffusion(data, threshold = "target", target = 0.02)
  expect_s3_class(d, "ik_diffusion")
  expect_equal(d$threshold, "target")
})

test_that("all positive changes give diffusion index of 1", {
  df <- data.frame(
    date = rep(as.Date("2020-01-01"), 3),
    item = c("A", "B", "C"),
    weight = c(0.3, 0.3, 0.4),
    price_change = c(0.01, 0.02, 0.03),
    stringsAsFactors = FALSE
  )
  d <- ik_diffusion(df, threshold = "zero", weighted = FALSE)
  expect_equal(d$diffusion$diffusion_index, 1)
})

test_that("all negative changes give diffusion index of 0", {
  df <- data.frame(
    date = rep(as.Date("2020-01-01"), 3),
    item = c("A", "B", "C"),
    weight = c(0.3, 0.3, 0.4),
    price_change = c(-0.01, -0.02, -0.03),
    stringsAsFactors = FALSE
  )
  d <- ik_diffusion(df, threshold = "zero", weighted = FALSE)
  expect_equal(d$diffusion$diffusion_index, 0)
})

test_that("weighted flag is stored", {
  data <- ik_sample_data("components")
  d1 <- ik_diffusion(data, threshold = "zero", weighted = TRUE)
  d2 <- ik_diffusion(data, threshold = "zero", weighted = FALSE)
  expect_true(d1$weighted)
  expect_false(d2$weighted)
})

test_that("print.ik_diffusion runs without error", {
  data <- ik_sample_data("components")
  d <- ik_diffusion(data, threshold = "zero")
  expect_no_error(capture.output(print(d)))
})

test_that("print.ik_diffusion returns invisibly", {
  data <- ik_sample_data("components")
  d <- ik_diffusion(data, threshold = "zero")
  out <- capture.output(result <- print(d))
  expect_identical(result, d)
})

test_that("plot.ik_diffusion runs without error", {
  data <- ik_sample_data("components")
  d <- ik_diffusion(data, threshold = "zero")
  expect_no_error(plot(d))
})

test_that("plot.ik_diffusion returns invisibly", {
  data <- ik_sample_data("components")
  d <- ik_diffusion(data, threshold = "zero")
  result <- plot(d)
  expect_identical(result, d)
})
