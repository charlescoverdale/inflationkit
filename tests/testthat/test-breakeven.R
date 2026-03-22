# test-breakeven.R

test_that("ik_breakeven returns correct class", {
  be <- ik_breakeven(nominal_yield = 4.5, real_yield = 2.0)
  expect_s3_class(be, "ik_breakeven")
})

test_that("ik_breakeven returns expected elements", {
  be <- ik_breakeven(nominal_yield = 4.5, real_yield = 2.0)
  expect_true(all(c("breakeven", "maturity") %in% names(be)))
})

test_that("simple breakeven is nominal minus real", {
  be <- ik_breakeven(nominal_yield = 4.5, real_yield = 2.0)
  expect_equal(be$breakeven, 2.5)
})

test_that("breakeven with maturity returns data.frame", {
  be <- ik_breakeven(
    nominal_yield = c(4.2, 4.5, 4.8),
    real_yield = c(1.8, 2.0, 2.3),
    maturity = c(2, 5, 10)
  )
  expect_s3_class(be$breakeven, "data.frame")
  expect_equal(names(be$breakeven), c("maturity", "nominal", "real", "breakeven"))
  expect_equal(nrow(be$breakeven), 3L)
})

test_that("breakeven spread is correct with maturity", {
  be <- ik_breakeven(
    nominal_yield = c(4.2, 4.5, 4.8, 5.0),
    real_yield = c(1.8, 2.0, 2.3, 2.5),
    maturity = c(2, 5, 10, 30)
  )
  expected <- c(4.2 - 1.8, 4.5 - 2.0, 4.8 - 2.3, 5.0 - 2.5)
  expect_equal(be$breakeven$breakeven, expected)
})

test_that("maturity is NULL when not provided", {
  be <- ik_breakeven(nominal_yield = 4.5, real_yield = 2.0)
  expect_null(be$maturity)
})

test_that("maturity is stored when provided", {
  be <- ik_breakeven(
    nominal_yield = c(4.2, 4.5),
    real_yield = c(1.8, 2.0),
    maturity = c(2, 5)
  )
  expect_equal(be$maturity, c(2, 5))
})

test_that("ik_breakeven validates unequal lengths", {
  expect_error(ik_breakeven(nominal_yield = c(4.2, 4.5), real_yield = 2.0))
})

test_that("ik_breakeven validates non-numeric input", {
  expect_error(ik_breakeven(nominal_yield = "not numeric", real_yield = 2.0))
  expect_error(ik_breakeven(nominal_yield = 4.5, real_yield = "not numeric"))
})

test_that("ik_breakeven validates maturity length", {
  expect_error(
    ik_breakeven(
      nominal_yield = c(4.2, 4.5), real_yield = c(1.8, 2.0),
      maturity = c(2, 5, 10)
    )
  )
})

test_that("breakeven with zero spread", {
  be <- ik_breakeven(nominal_yield = 3.0, real_yield = 3.0)
  expect_equal(be$breakeven, 0)
})

test_that("breakeven with negative spread (deflation expectation)", {
  be <- ik_breakeven(nominal_yield = 1.0, real_yield = 2.0)
  expect_equal(be$breakeven, -1.0)
})

test_that("print.ik_breakeven runs without error (with maturity)", {
  be <- ik_breakeven(
    nominal_yield = c(4.2, 4.5), real_yield = c(1.8, 2.0),
    maturity = c(2, 5)
  )
  expect_no_error(capture.output(print(be)))
})

test_that("print.ik_breakeven runs without error (without maturity)", {
  be <- ik_breakeven(nominal_yield = 4.5, real_yield = 2.0)
  expect_no_error(capture.output(print(be)))
})

test_that("print.ik_breakeven returns invisibly", {
  be <- ik_breakeven(nominal_yield = 4.5, real_yield = 2.0)
  out <- capture.output(result <- print(be))
  expect_identical(result, be)
})

test_that("plot.ik_breakeven runs without error (with maturity)", {
  be <- ik_breakeven(
    nominal_yield = c(4.2, 4.5, 4.8), real_yield = c(1.8, 2.0, 2.3),
    maturity = c(2, 5, 10)
  )
  expect_no_error(plot(be))
})

test_that("plot.ik_breakeven warns when no maturity", {
  be <- ik_breakeven(nominal_yield = 4.5, real_yield = 2.0)
  expect_warning(plot(be))
})

test_that("plot.ik_breakeven returns invisibly", {
  be <- ik_breakeven(
    nominal_yield = c(4.2, 4.5), real_yield = c(1.8, 2.0),
    maturity = c(2, 5)
  )
  result <- plot(be)
  expect_identical(result, be)
})
