# test-sticky_flexible.R

test_that("ik_sticky_flexible returns correct class", {
  data <- ik_sample_data("components")
  class_vec <- c(
    Food = FALSE, Housing = TRUE, Transport = FALSE,
    Clothing = FALSE, Health = TRUE, Education = TRUE,
    Communication = TRUE, Recreation = FALSE,
    Restaurants = TRUE, Other = FALSE
  )
  sf <- ik_sticky_flexible(data, classification = class_vec)
  expect_s3_class(sf, "ik_sticky_flex")
})

test_that("ik_sticky_flexible returns expected elements", {
  data <- ik_sample_data("components")
  class_vec <- c(
    Food = FALSE, Housing = TRUE, Transport = FALSE,
    Clothing = FALSE, Health = TRUE, Education = TRUE,
    Communication = TRUE, Recreation = FALSE,
    Restaurants = TRUE, Other = FALSE
  )
  sf <- ik_sticky_flexible(data, classification = class_vec)
  expect_true(all(c("result", "classification") %in% names(sf)))
})

test_that("result data.frame has correct columns", {
  data <- ik_sample_data("components")
  class_vec <- c(
    Food = FALSE, Housing = TRUE, Transport = FALSE,
    Clothing = FALSE, Health = TRUE, Education = TRUE,
    Communication = TRUE, Recreation = FALSE,
    Restaurants = TRUE, Other = FALSE
  )
  sf <- ik_sticky_flexible(data, classification = class_vec)
  expect_equal(names(sf$result), c("date", "sticky", "flexible", "headline"))
})

test_that("sticky and flexible are computed correctly with known data", {
  df <- data.frame(
    date = rep(as.Date("2020-01-01"), 3),
    item = c("A", "B", "C"),
    weight = c(0.5, 0.3, 0.2),
    price_change = c(0.02, 0.04, 0.06),
    stringsAsFactors = FALSE
  )
  class_vec <- c(A = TRUE, B = FALSE, C = TRUE)
  sf <- ik_sticky_flexible(df, classification = class_vec)

  # Sticky: A(w=0.5, pc=0.02) and C(w=0.2, pc=0.06)
  exp_sticky <- weighted.mean(c(0.02, 0.06), c(0.5, 0.2))
  # Flexible: B(w=0.3, pc=0.04)
  exp_flexible <- 0.04

  expect_equal(sf$result$sticky, exp_sticky, tolerance = 1e-10)
  expect_equal(sf$result$flexible, exp_flexible, tolerance = 1e-10)
})

test_that("classification as data.frame works", {
  data <- ik_sample_data("components")
  class_df <- data.frame(
    item = c("Food", "Housing", "Transport", "Clothing", "Health",
             "Education", "Communication", "Recreation", "Restaurants", "Other"),
    sticky = c(FALSE, TRUE, FALSE, FALSE, TRUE,
               TRUE, TRUE, FALSE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  sf <- ik_sticky_flexible(data, classification = class_df)
  expect_s3_class(sf, "ik_sticky_flex")
})

test_that("classification data.frame missing columns errors", {
  data <- ik_sample_data("components")
  class_df <- data.frame(item = c("Food"), other = TRUE)
  expect_error(ik_sticky_flexible(data, classification = class_df))
})

test_that("invalid classification type errors", {
  data <- ik_sample_data("components")
  expect_error(ik_sticky_flexible(data, classification = c(TRUE, FALSE)))
})

test_that("warns about unclassified items", {
  data <- ik_sample_data("components")
  # Only classify 3 of 10 items
  class_vec <- c(Food = TRUE, Housing = FALSE, Transport = TRUE)
  expect_warning(ik_sticky_flexible(data, classification = class_vec))
})

test_that("errors when no items match classification", {
  data <- ik_sample_data("components")
  class_vec <- c(NonExistent1 = TRUE, NonExistent2 = FALSE)
  expect_error(ik_sticky_flexible(data, classification = class_vec))
})

test_that("classification is stored correctly", {
  data <- ik_sample_data("components")
  class_vec <- c(Food = FALSE, Housing = TRUE, Transport = FALSE,
                 Clothing = FALSE, Health = TRUE, Education = TRUE,
                 Communication = TRUE, Recreation = FALSE,
                 Restaurants = TRUE, Other = FALSE)
  sf <- ik_sticky_flexible(data, classification = class_vec)
  expect_equal(sf$classification, class_vec)
})

test_that("print.ik_sticky_flex runs without error", {
  data <- ik_sample_data("components")
  class_vec <- c(Food = FALSE, Housing = TRUE, Transport = FALSE,
                 Clothing = FALSE, Health = TRUE, Education = TRUE,
                 Communication = TRUE, Recreation = FALSE,
                 Restaurants = TRUE, Other = FALSE)
  sf <- ik_sticky_flexible(data, classification = class_vec)
  expect_no_error(capture.output(print(sf)))
})

test_that("print.ik_sticky_flex returns invisibly", {
  data <- ik_sample_data("components")
  class_vec <- c(Food = FALSE, Housing = TRUE, Transport = FALSE,
                 Clothing = FALSE, Health = TRUE, Education = TRUE,
                 Communication = TRUE, Recreation = FALSE,
                 Restaurants = TRUE, Other = FALSE)
  sf <- ik_sticky_flexible(data, classification = class_vec)
  out <- capture.output(result <- print(sf))
  expect_identical(result, sf)
})

test_that("plot.ik_sticky_flex runs without error", {
  data <- ik_sample_data("components")
  class_vec <- c(Food = FALSE, Housing = TRUE, Transport = FALSE,
                 Clothing = FALSE, Health = TRUE, Education = TRUE,
                 Communication = TRUE, Recreation = FALSE,
                 Restaurants = TRUE, Other = FALSE)
  sf <- ik_sticky_flexible(data, classification = class_vec)
  expect_no_error(plot(sf))
})

test_that("plot.ik_sticky_flex returns invisibly", {
  data <- ik_sample_data("components")
  class_vec <- c(Food = FALSE, Housing = TRUE, Transport = FALSE,
                 Clothing = FALSE, Health = TRUE, Education = TRUE,
                 Communication = TRUE, Recreation = FALSE,
                 Restaurants = TRUE, Other = FALSE)
  sf <- ik_sticky_flexible(data, classification = class_vec)
  result <- plot(sf)
  expect_identical(result, sf)
})
