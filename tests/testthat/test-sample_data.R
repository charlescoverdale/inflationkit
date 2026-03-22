# test-sample_data.R

test_that("ik_sample_data('components') returns correct structure", {
  comp <- ik_sample_data("components")
  expect_s3_class(comp, "data.frame")
  expect_equal(names(comp), c("date", "item", "weight", "price_change"))
})

test_that("ik_sample_data('components') has correct dimensions", {
  comp <- ik_sample_data("components")
  # 120 months x 10 items = 1200 rows

  expect_equal(nrow(comp), 1200L)
  expect_equal(length(unique(comp$item)), 10L)
  expect_equal(length(unique(comp$date)), 120L)
})

test_that("ik_sample_data('components') has 10 expected items", {
  comp <- ik_sample_data("components")
  expected_items <- c(
    "Food", "Housing", "Transport", "Clothing", "Health",
    "Education", "Communication", "Recreation", "Restaurants", "Other"
  )
  expect_equal(sort(unique(comp$item)), sort(expected_items))
})

test_that("ik_sample_data('components') weights sum to 1", {
  comp <- ik_sample_data("components")
  # Weights are constant per item; take one date
  one_date <- comp[comp$date == comp$date[1], ]
  expect_equal(sum(one_date$weight), 1)
})

test_that("ik_sample_data('components') has numeric columns", {
  comp <- ik_sample_data("components")
  expect_true(is.numeric(comp$weight))
  expect_true(is.numeric(comp$price_change))
  expect_true(inherits(comp$date, "Date"))
  expect_true(is.character(comp$item))
})

test_that("ik_sample_data('components') is reproducible", {
  comp1 <- ik_sample_data("components")
  comp2 <- ik_sample_data("components")
  expect_equal(comp1, comp2)
})

test_that("ik_sample_data('components') does not alter global RNG state", {
  set.seed(123)
  r1 <- runif(1)
  set.seed(123)
  ik_sample_data("components")
  r2 <- runif(1)
  expect_equal(r1, r2)
})

test_that("ik_sample_data('headline') returns correct structure", {
  hl <- ik_sample_data("headline")
  expect_s3_class(hl, "data.frame")
  expect_equal(names(hl), c("date", "inflation", "output_gap", "unemployment"))
})

test_that("ik_sample_data('headline') has 80 quarterly observations", {
  hl <- ik_sample_data("headline")
  expect_equal(nrow(hl), 80L)
})

test_that("ik_sample_data('headline') has numeric columns", {
  hl <- ik_sample_data("headline")
  expect_true(is.numeric(hl$inflation))
  expect_true(is.numeric(hl$output_gap))
  expect_true(is.numeric(hl$unemployment))
  expect_true(inherits(hl$date, "Date"))
})

test_that("ik_sample_data('headline') values are in realistic ranges", {
  hl <- ik_sample_data("headline")
  expect_true(all(hl$inflation >= 0.5 & hl$inflation <= 6))
  expect_true(all(hl$output_gap >= -4 & hl$output_gap <= 4))
  expect_true(all(hl$unemployment >= 3 & hl$unemployment <= 10))
})

test_that("ik_sample_data('headline') is reproducible", {
  hl1 <- ik_sample_data("headline")
  hl2 <- ik_sample_data("headline")
  expect_equal(hl1, hl2)
})

test_that("ik_sample_data rejects invalid type", {
  expect_error(ik_sample_data("invalid"))
})

test_that("ik_sample_data default type is 'components'", {
  comp <- ik_sample_data()
  expect_equal(nrow(comp), 1200L)
})
