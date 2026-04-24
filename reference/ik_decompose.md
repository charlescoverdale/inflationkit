# Decompose inflation into weighted component contributions

Computes the weighted contribution of each CPI component to headline
inflation. The contribution of item i is `weight_i * price_change_i`,
and headline inflation is the sum of all contributions for each period.

## Usage

``` r
ik_decompose(
  data,
  date_col = "date",
  item_col = "item",
  change_col = "price_change",
  weight_col = "weight"
)
```

## Arguments

- data:

  A data.frame containing component-level inflation data.

- date_col:

  Character. Name of the date column. Default `"date"`.

- item_col:

  Character. Name of the item/component column. Default `"item"`.

- change_col:

  Character. Name of the price change column. Default `"price_change"`.

- weight_col:

  Character. Name of the weight column. Default `"weight"`.

## Value

An S3 object of class `"ik_decomposition"` with elements:

- contributions:

  data.frame with columns: date, item, weight, price_change,
  contribution.

- headline:

  data.frame with columns: date, headline_inflation.

## Examples

``` r
data <- ik_sample_data("components")
decomp <- ik_decompose(data)
print(decomp)
#> 
#> ── Inflation Decomposition ─────────────────────────────────────────────────────
#> • Period: 2015-01-01 to 2024-12-01
#> • Number of items: 10
#> • Mean headline inflation: 0.26%
plot(decomp)
```
