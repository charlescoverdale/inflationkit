# test-utils.R

# --- validate_numeric_vector ---

test_that("validate_numeric_vector accepts valid input", {
  expect_no_error(inflationkit:::validate_numeric_vector(c(1, 2, 3), "x"))
})

test_that("validate_numeric_vector rejects non-numeric", {
  expect_error(inflationkit:::validate_numeric_vector("not numeric", "x"))
  expect_error(inflationkit:::validate_numeric_vector(TRUE, "x"))
})

test_that("validate_numeric_vector rejects NAs", {
  expect_error(inflationkit:::validate_numeric_vector(c(1, NA, 3), "x"))
})

# --- validate_positive_integer ---

test_that("validate_positive_integer accepts valid input", {
  expect_no_error(inflationkit:::validate_positive_integer(1L, "n"))
  expect_no_error(inflationkit:::validate_positive_integer(5, "n"))
})

test_that("validate_positive_integer rejects zero", {
  expect_error(inflationkit:::validate_positive_integer(0, "n"))
})

test_that("validate_positive_integer rejects negative", {
  expect_error(inflationkit:::validate_positive_integer(-1, "n"))
})

test_that("validate_positive_integer rejects non-integer", {
  expect_error(inflationkit:::validate_positive_integer(1.5, "n"))
})

test_that("validate_positive_integer rejects vector", {
  expect_error(inflationkit:::validate_positive_integer(c(1, 2), "n"))
})

test_that("validate_positive_integer rejects NA", {
  expect_error(inflationkit:::validate_positive_integer(NA, "n"))
})

# --- validate_scalar ---

test_that("validate_scalar accepts valid input", {
  expect_no_error(inflationkit:::validate_scalar(3.14, "x"))
  expect_no_error(inflationkit:::validate_scalar(0, "x"))
  expect_no_error(inflationkit:::validate_scalar(-1.5, "x"))
})

test_that("validate_scalar rejects non-numeric", {
  expect_error(inflationkit:::validate_scalar("a", "x"))
})

test_that("validate_scalar rejects vector", {
  expect_error(inflationkit:::validate_scalar(c(1, 2), "x"))
})

test_that("validate_scalar rejects NA", {
  expect_error(inflationkit:::validate_scalar(NA_real_, "x"))
})

test_that("validate_scalar rejects Inf", {
  expect_error(inflationkit:::validate_scalar(Inf, "x"))
  expect_error(inflationkit:::validate_scalar(-Inf, "x"))
})

# --- validate_data ---

test_that("validate_data accepts valid data.frame", {
  df <- data.frame(date = Sys.Date(), item = "A", weight = 0.5,
                   price_change = 0.02)
  expect_no_error(
    inflationkit:::validate_data(df, c("date", "item", "weight", "price_change"))
  )
})

test_that("validate_data rejects non-data.frame", {
  expect_error(inflationkit:::validate_data("not a df", c("a")))
  expect_error(inflationkit:::validate_data(list(a = 1), c("a")))
})

test_that("validate_data rejects missing columns", {
  df <- data.frame(date = Sys.Date(), item = "A")
  expect_error(
    inflationkit:::validate_data(df, c("date", "item", "weight"))
  )
})

test_that("validate_data rejects non-numeric weight/price_change", {
  df <- data.frame(date = Sys.Date(), item = "A", weight = "not_numeric",
                   price_change = 0.02, stringsAsFactors = FALSE)
  expect_error(
    inflationkit:::validate_data(df, c("date", "item", "weight", "price_change"))
  )
})

test_that("validate_data allows non-numeric date and item columns", {
  df <- data.frame(date = "2020-01-01", item = "A", weight = 0.5,
                   price_change = 0.02, stringsAsFactors = FALSE)
  expect_no_error(
    inflationkit:::validate_data(df, c("date", "item", "weight", "price_change"))
  )
})

# --- fmt_pct ---

test_that("fmt_pct formats correctly", {
  expect_equal(inflationkit:::fmt_pct(0.05), "5%")
  expect_equal(inflationkit:::fmt_pct(0), "0%")
  expect_equal(inflationkit:::fmt_pct(1), "100%")
  expect_equal(inflationkit:::fmt_pct(0.1234), "12.34%")
})

test_that("fmt_pct handles negative values", {
  expect_equal(inflationkit:::fmt_pct(-0.05), "-5%")
})

# --- fmt_pp ---

test_that("fmt_pp formats correctly", {
  expect_equal(inflationkit:::fmt_pp(0.02), "2 pp")
  expect_equal(inflationkit:::fmt_pp(0), "0 pp")
  expect_equal(inflationkit:::fmt_pp(0.1234), "12.34 pp")
})

test_that("fmt_pp handles negative values", {
  expect_equal(inflationkit:::fmt_pp(-0.03), "-3 pp")
})
