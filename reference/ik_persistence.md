# Measure inflation persistence

Estimates the degree of persistence in an inflation series using one of
three methods: sum of AR coefficients, half-life, or largest
autoregressive root.

## Usage

``` r
ik_persistence(
  x,
  method = c("sum_ar", "half_life", "largest_root"),
  ar_order = NULL,
  max_order = 12L,
  ic = c("bic", "aic")
)
```

## Arguments

- x:

  Numeric vector. An inflation time series.

- method:

  Character. One of `"sum_ar"`, `"half_life"`, or `"largest_root"`.

- ar_order:

  Integer or `NULL`. The AR order to fit. If `NULL`, order is selected
  automatically using the information criterion specified by `ic`.

- max_order:

  Integer. Maximum AR order to consider when selecting automatically.
  Default `12`.

- ic:

  Character. Information criterion for order selection: `"bic"` or
  `"aic"`. Default `"bic"`.

## Value

An S3 object of class `"ik_persistence"` with elements:

- value:

  Numeric. The persistence measure.

- method:

  Character. The method used.

- ar_order:

  Integer. The AR order fitted.

- ar_coefficients:

  Numeric vector. The estimated AR coefficients.

- interpretation:

  Character. A plain-language interpretation ("High persistence",
  "Moderate persistence", or "Low persistence").

## References

Andrews, D. W. K. and Chen, H.-Y. (1994). "Approximately Median-Unbiased
Estimation of Autoregressive Models." Journal of Business and Economic
Statistics, 12(2), 187-204.

Marques, C. R. (2004). "Inflation Persistence: Facts or Artefacts?" ECB
Working Paper No. 371.

## Examples

``` r
data <- ik_sample_data("headline")
p <- ik_persistence(data$inflation, method = "sum_ar")
print(p)
#> 
#> ── Inflation Persistence ───────────────────────────────────────────────────────
#> • Method: Sum of AR Coefficients
#> • AR order: 1
#> • Persistence value: 0.647
#> • Moderate persistence (sum of AR coefficients 0.5 to 0.8)

p_hl <- ik_persistence(data$inflation, method = "half_life")
print(p_hl)
#> 
#> ── Inflation Persistence ───────────────────────────────────────────────────────
#> • Method: Half-Life
#> • AR order: 1
#> • Persistence value: 1.592
#> • Low persistence (half-life < 4 periods)
```
