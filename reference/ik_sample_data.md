# Generate sample inflation data

Creates synthetic data for testing and demonstrating inflationkit
functions. Two types are available: component-level CPI data and
headline macro data.

## Usage

``` r
ik_sample_data(type = c("components", "headline"))
```

## Arguments

- type:

  Character. Either `"components"` for item-level CPI data with weights
  and price changes, or `"headline"` for quarterly macro data with
  inflation, output gap, and unemployment.

## Value

A data.frame. For `"components"`: columns `date`, `item`, `weight`,
`price_change` (120 months, 10 items, 1200 rows). For `"headline"`:
columns `date`, `inflation`, `output_gap`, `unemployment` (80 quarterly
observations).

## Examples

``` r
comp <- ik_sample_data("components")
head(comp)
#>         date item weight price_change
#> 1 2015-01-01 Food   0.15     0.008484
#> 2 2015-02-01 Food   0.15     0.000741
#> 3 2015-03-01 Food   0.15     0.004453
#> 4 2015-04-01 Food   0.15     0.005531
#> 5 2015-05-01 Food   0.15     0.004617
#> 6 2015-06-01 Food   0.15     0.002576

macro <- ik_sample_data("headline")
head(macro)
#>         date inflation output_gap unemployment
#> 1 2005-01-01     2.500      0.000        5.500
#> 2 2005-04-01     2.275      0.685        5.056
#> 3 2005-07-01     1.963      0.796        5.303
#> 4 2005-10-01     2.466      1.313        4.946
#> 5 2006-01-01     2.760      0.888        5.536
#> 6 2006-04-01     2.224     -0.073        5.446
```
