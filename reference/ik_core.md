# Compute core inflation measures

Estimates core (underlying) inflation using one of three standard
methods: trimmed mean, weighted median, or exclusion-based. These
measures aim to strip out transitory price movements and reveal the
persistent trend.

## Usage

``` r
ik_core(
  data,
  method = c("trimmed_mean", "weighted_median", "exclusion", "asymmetric_trim"),
  trim = 0.08,
  trim_lower = 0.24,
  trim_upper = 0.31,
  exclude = NULL,
  date_col = "date",
  item_col = "item",
  change_col = "price_change",
  weight_col = "weight"
)
```

## Arguments

- data:

  A data.frame containing component-level inflation data.

- method:

  Character. One of `"trimmed_mean"`, `"weighted_median"`,
  `"exclusion"`, or `"asymmetric_trim"`.

- trim:

  Numeric. Fraction to trim from each tail (for `"trimmed_mean"` only).
  Default `0.08` (8% symmetric trim, following the Cleveland Fed).

- trim_lower:

  Numeric. Fraction to trim from the lower tail (for `"asymmetric_trim"`
  only). Default `0.24`, matching the Dallas Fed trimmed mean PCE.

- trim_upper:

  Numeric. Fraction to trim from the upper tail (for `"asymmetric_trim"`
  only). Default `0.31`, matching the Dallas Fed trimmed mean PCE.

- exclude:

  Character vector. Items to exclude (for `"exclusion"` only).

- date_col:

  Character. Name of the date column. Default `"date"`.

- item_col:

  Character. Name of the item/component column. Default `"item"`.

- change_col:

  Character. Name of the price change column. Default `"price_change"`.

- weight_col:

  Character. Name of the weight column. Default `"weight"`.

## Value

An S3 object of class `"ik_core"` with elements:

- core:

  data.frame with columns: date, core_inflation.

- method:

  Character. The method used.

- trim:

  Numeric. The trim fraction (if applicable).

- exclude:

  Character vector. Excluded items (if applicable).

- headline:

  data.frame with columns: date, headline.

## References

Bryan, M. F. and Cecchetti, S. G. (1993). "The Consumer Price Index as a
Measure of Inflation." NBER Working Paper No. 4505.

Bryan, M. F. and Cecchetti, S. G. (1994). "Measuring Core Inflation." In
Monetary Policy, University of Chicago Press.

## Examples

``` r
data <- ik_sample_data("components")

# Trimmed mean (default)
core_tm <- ik_core(data, method = "trimmed_mean")
print(core_tm)
#> 
#> ── Core Inflation Measure ──────────────────────────────────────────────────────
#> • Method: Trimmed Mean (8%)
#> • Mean core inflation: 0.27%
#> • Mean headline inflation: 0.26%
#> • Observations: 120

# Weighted median
core_wm <- ik_core(data, method = "weighted_median")
print(core_wm)
#> 
#> ── Core Inflation Measure ──────────────────────────────────────────────────────
#> • Method: Weighted Median
#> • Mean core inflation: 0.27%
#> • Mean headline inflation: 0.26%
#> • Observations: 120

# Exclusion-based
core_ex <- ik_core(data, method = "exclusion", exclude = c("Food", "Transport"))
print(core_ex)
#> 
#> ── Core Inflation Measure ──────────────────────────────────────────────────────
#> • Method: Exclusion (Food, Transport)
#> • Mean core inflation: 0.26%
#> • Mean headline inflation: 0.26%
#> • Observations: 120
```
