# test-decompose.R

test_that("ik_decompose returns correct class", {
  data <- ik_sample_data("components")
  decomp <- ik_decompose(data)
  expect_s3_class(decomp, "ik_decomposition")
})

test_that("ik_decompose returns contributions and headline", {
  data <- ik_sample_data("components")
  decomp <- ik_decompose(data)
  expect_true("contributions" %in% names(decomp))
  expect_true("headline" %in% names(decomp))
})

test_that("contributions have correct columns", {
  data <- ik_sample_data("components")
  decomp <- ik_decompose(data)
  expect_equal(
    names(decomp$contributions),
    c("date", "item", "weight", "price_change", "contribution")
  )
})

test_that("headline has correct columns", {
  data <- ik_sample_data("components")
  decomp <- ik_decompose(data)
  expect_equal(names(decomp$headline), c("date", "headline_inflation"))
})

test_that("contribution equals weight * price_change", {
  data <- ik_sample_data("components")
  decomp <- ik_decompose(data)
  expected <- decomp$contributions$weight * decomp$contributions$price_change
  expect_equal(decomp$contributions$contribution, expected)
})

test_that("headline equals sum of contributions per date", {
  data <- ik_sample_data("components")
  decomp <- ik_decompose(data)

  for (i in seq_len(nrow(decomp$headline))) {
    d <- decomp$headline$date[i]
    sub <- decomp$contributions[decomp$contributions$date == d, ]
    expect_equal(
      decomp$headline$headline_inflation[i],
      sum(sub$contribution),
      tolerance = 1e-10
    )
  }
})

test_that("ik_decompose works with known simple data", {
  df <- data.frame(
    date = rep(as.Date("2020-01-01"), 3),
    item = c("A", "B", "C"),
    weight = c(0.5, 0.3, 0.2),
    price_change = c(0.02, 0.04, -0.01),
    stringsAsFactors = FALSE
  )
  decomp <- ik_decompose(df)

  expected_headline <- 0.5 * 0.02 + 0.3 * 0.04 + 0.2 * (-0.01)
  expect_equal(decomp$headline$headline_inflation, expected_headline)
})

test_that("ik_decompose preserves number of rows", {
  data <- ik_sample_data("components")
  decomp <- ik_decompose(data)
  expect_equal(nrow(decomp$contributions), nrow(data))
})

test_that("weight normalization: weights summing to 2 give same headline as weights summing to 1", {
  df1 <- data.frame(
    date = rep(as.Date("2020-01-01"), 3),
    item = c("A", "B", "C"),
    weight = c(0.5, 0.3, 0.2),
    price_change = c(0.02, 0.04, -0.01),
    stringsAsFactors = FALSE
  )
  df2 <- data.frame(
    date = rep(as.Date("2020-01-01"), 3),
    item = c("A", "B", "C"),
    weight = c(1.0, 0.6, 0.4),  # doubled weights (sum = 2)
    price_change = c(0.02, 0.04, -0.01),
    stringsAsFactors = FALSE
  )
  decomp1 <- ik_decompose(df1)
  decomp2 <- ik_decompose(df2)
  expect_equal(
    decomp1$headline$headline_inflation,
    decomp2$headline$headline_inflation,
    tolerance = 1e-10
  )
})

test_that("ik_decompose validates missing columns", {
  df <- data.frame(date = Sys.Date(), item = "A", weight = 0.5)
  expect_error(ik_decompose(df))
})

test_that("ik_decompose validates non-data.frame input", {
  expect_error(ik_decompose("not a data frame"))
})

test_that("ik_decompose accepts custom column names", {
  df <- data.frame(
    date = rep(as.Date("2020-01-01"), 2),
    item = c("A", "B"),
    w = c(0.6, 0.4),
    pc = c(0.01, 0.03),
    stringsAsFactors = FALSE
  )
  decomp <- ik_decompose(
    df, date_col = "date", item_col = "item",
    change_col = "pc", weight_col = "w"
  )
  expect_s3_class(decomp, "ik_decomposition")
  expect_equal(nrow(decomp$headline), 1L)
})

test_that("print.ik_decomposition runs without error", {
  data <- ik_sample_data("components")
  decomp <- ik_decompose(data)
  expect_no_error(capture.output(print(decomp)))
})

test_that("print.ik_decomposition returns invisibly", {
  data <- ik_sample_data("components")
  decomp <- ik_decompose(data)
  out <- capture.output(result <- print(decomp))
  expect_identical(result, decomp)
})

test_that("plot.ik_decomposition runs without error", {
  data <- ik_sample_data("components")
  decomp <- ik_decompose(data)
  expect_no_error(plot(decomp))
})

test_that("plot.ik_decomposition returns invisibly", {
  data <- ik_sample_data("components")
  decomp <- ik_decompose(data)
  result <- plot(decomp)
  expect_identical(result, decomp)
})
