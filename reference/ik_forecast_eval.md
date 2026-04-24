# Evaluate inflation forecasts

Runs standard forecast evaluation tests. The bias test
(Mincer-Zarnowitz) checks whether forecasts are unbiased. The efficiency
test (Nordhaus) checks whether forecast errors are autocorrelated. The
Diebold-Mariano test compares predictive accuracy of two competing
forecasts.

## Usage

``` r
ik_forecast_eval(
  actual,
  forecast,
  test = c("bias", "efficiency", "dm"),
  forecast2 = NULL,
  horizon = 1L,
  alternative = c("two.sided", "less", "greater")
)
```

## Arguments

- actual:

  Numeric vector. Realised inflation values.

- forecast:

  Numeric vector. Forecast values, same length as `actual`.

- test:

  Character. One of `"bias"`, `"efficiency"`, or `"dm"`.

- forecast2:

  Numeric vector or `NULL`. Second forecast series (required for `"dm"`
  test), same length as `actual`.

- horizon:

  Integer. Forecast horizon (used for Newey-West bandwidth in the DM
  test). Default `1`.

- alternative:

  Character. Alternative hypothesis for the DM test: `"two.sided"`,
  `"less"`, or `"greater"`.

## Value

An S3 object of class `"ik_forecast_eval"` with elements:

- test:

  Character. The test performed.

- statistic:

  Numeric. The test statistic.

- p_value:

  Numeric. The p-value.

- coefficients:

  Named numeric vector (for bias and efficiency tests).

- conclusion:

  Character. A plain-language summary of the result.

## References

Diebold, F. X. and Mariano, R. S. (1995). "Comparing Predictive
Accuracy." Journal of Business and Economic Statistics, 13(3), 253-263.

## Examples

``` r
set.seed(42)
data <- ik_sample_data("headline")
actual <- data$inflation[5:80]
forecast1 <- data$inflation[4:79] + rnorm(76, 0, 0.2)
forecast2 <- data$inflation[4:79] + rnorm(76, 0, 0.4)

# Mincer-Zarnowitz bias test
bias <- ik_forecast_eval(actual, forecast1, test = "bias")
print(bias)
#> 
#> ── Forecast Evaluation ─────────────────────────────────────────────────────────
#> • Test: Mincer-Zarnowitz Bias Test
#> • Test statistic: 19.5379
#> • p-value: 0
#> • Reject H0: forecasts appear biased (alpha != 0 or beta != 1).
#> • Coefficients: alpha = 0.7846, beta = 0.4673

# Diebold-Mariano test
dm <- ik_forecast_eval(actual, forecast1, test = "dm", forecast2 = forecast2)
print(dm)
#> 
#> ── Forecast Evaluation ─────────────────────────────────────────────────────────
#> • Test: Diebold-Mariano Test
#> • Test statistic: -3.3516
#> • p-value: 8e-04
#> • Forecast 1 is significantly more accurate than Forecast 2.
#> • Coefficients: mean_loss_diff = -0.126
```
