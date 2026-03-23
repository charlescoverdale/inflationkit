# inflationkit

<!-- badges: start -->
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

**A toolkit for analysing inflation: decompose price indices into component contributions, compute core inflation measures, estimate persistence, fit Phillips curves, and extract trends.**

## Installation

Install from GitHub:

```r
# install.packages("devtools")
devtools::install_github("charlescoverdale/inflationkit")
```

```r
library(inflationkit)

# Built-in sample data: 10 CPI components, 120 months
d <- ik_sample_data("components")
head(d)
#>         date    item weight price_change
#> 1 2015-01-01    Food   0.15     0.003218
#> 2 2015-01-01 Housing   0.25     0.004274
#> 3 2015-01-01 Transport 0.12    0.001893
#> ...

# What is driving inflation?
decomp <- ik_decompose(d)
plot(decomp)
```


## Why inflationkit?

Inflation is the rate at which prices rise across an economy. Central banks target low, stable inflation (typically around 2% per year) and adjust interest rates to keep it there. But "inflation" is not a single number: it is a weighted average of price changes across hundreds of categories, from food and housing to transport and clothing. Understanding which components are pushing prices up (or down) is essential for diagnosing what is happening in an economy and predicting what comes next.

The problem is that headline inflation is noisy. Food and energy prices swing wildly from month to month due to weather, supply shocks, and global commodity markets. Central banks need to see through this noise to the underlying trend. This is why they compute "core" inflation measures: the trimmed mean strips out the most extreme price changes, the weighted median takes the middle of the distribution, and exclusion-based core simply drops volatile items like food and energy. Each method makes different assumptions about what counts as noise, and they can give different signals.

Beyond core measures, economists study inflation persistence (how quickly inflation returns to normal after a shock), the Phillips curve (the relationship between inflation and unemployment), diffusion (how broadly prices are rising across categories), and trend extraction (separating the permanent component from transitory shocks). These are standard tools in every central bank research department, yet there is no general-purpose R package for any of them. The only CRAN package with "inflation" in its name (`Inflation`) is Brazil-specific and has not been updated since 2017. `inflationkit` fills this gap. You bring CPI data from any country, and the package handles the analysis.


## Examples

### What is driving inflation?

Decompose headline inflation into the weighted contribution of each component. This tells you how many percentage points each category (food, housing, transport, etc.) is adding to the total.

```r
d <- ik_sample_data("components")
decomp <- ik_decompose(d)
decomp
#> -- Inflation Decomposition --
#> * Period: 2015-01-01 to 2024-12-01
#> * Number of items: 10
#> * Mean headline inflation: 0.25%

plot(decomp)  # Stacked bar chart of contributions over time
```

### How noisy is headline inflation?

Compare four core inflation measures side by side. The trimmed mean and weighted median are distribution-based (they let the data decide what to exclude each month). The exclusion method and asymmetric trim use fixed rules.

```r
d <- ik_sample_data("components")

# Cleveland Fed style: symmetric 8% trim
tm <- ik_core(d, method = "trimmed_mean", trim = 0.08)

# Weighted median: 50th percentile of price changes
wm <- ik_core(d, method = "weighted_median")

# Exclusion: drop food and transport
ex <- ik_core(d, method = "exclusion", exclude = c("Food", "Transport"))

# Dallas Fed style: asymmetric trim (24% lower, 31% upper)
at <- ik_core(d, method = "asymmetric_trim")

# Compare all four on one chart
comp <- ik_compare(tm, wm, ex, at,
                   labels = c("Trimmed mean", "Weighted median",
                              "Ex food & transport", "Asymmetric trim"))
plot(comp)
```

### How persistent is inflation?

Measure how quickly inflation returns to its mean after a shock. High persistence means rate hikes take longer to bring inflation down.

```r
d <- ik_sample_data("headline")

pers <- ik_persistence(d$inflation, method = "sum_ar")
pers
#> -- Inflation Persistence (Sum of AR Coefficients) --
#> * AR order: 4 (selected by BIC)
#> * Persistence value: 0.72
#> * Moderate persistence (sum of AR coefficients 0.5 to 0.8)

# Half-life: how many quarters until a shock decays by half?
pers_hl <- ik_persistence(d$inflation, method = "half_life")
pers_hl
#> -- Inflation Persistence (Half-Life) --
#> * Persistence value: 3.2 periods
```

### Is there a Phillips curve?

Estimate the relationship between inflation and unemployment. A negative slope means higher unemployment is associated with lower inflation.

```r
d <- ik_sample_data("headline")

pc <- ik_phillips(d$inflation, d$unemployment, type = "traditional", lags = 4)
pc
#> -- Phillips Curve (Traditional) --
#> * Slope estimate: -0.12 (p = 0.03)
#> * R-squared: 0.45
#> * Observations: 76

# Robust standard errors (Newey-West HAC)
pc_hac <- ik_phillips(d$inflation, d$unemployment,
                      type = "traditional", lags = 4, robust_se = "HAC")

plot(pc)  # Scatter plot with fitted line
```

### What is the inflation trend?

Separate the permanent trend from transitory shocks using the Hodrick-Prescott filter or Beveridge-Nelson decomposition.

```r
d <- ik_sample_data("headline")

hp <- ik_trend(d$inflation, method = "hp")
hp
#> -- Trend Inflation (Hodrick-Prescott) --
#> * Lambda: 1600
#> * Mean trend: 2.8
#> * Cycle volatility (SD): 0.5

plot(hp)  # Original series with trend overlay

# Beveridge-Nelson decomposition (ARIMA-based)
bn <- ik_trend(d$inflation, method = "beveridge_nelson")
plot(bn)
```

### Are these inflation forecasts any good?

Test whether forecasts are unbiased (Mincer-Zarnowitz) or compare two competing forecasts (Diebold-Mariano).

```r
actual <- c(2.1, 2.3, 2.5, 2.2, 2.8, 3.0, 2.7, 2.4)
forecast1 <- c(2.0, 2.2, 2.3, 2.1, 2.5, 2.8, 2.6, 2.3)
forecast2 <- c(1.8, 2.0, 2.6, 2.3, 2.4, 3.2, 2.5, 2.1)

# Bias test
bias <- ik_forecast_eval(actual, forecast1, test = "bias")
bias
#> -- Forecast Evaluation (Mincer-Zarnowitz Bias Test) --
#> * p-value: 0.42
#> * Cannot reject H0: no evidence of forecast bias.

# Diebold-Mariano: is forecast1 better than forecast2?
dm <- ik_forecast_eval(actual, forecast1, test = "dm", forecast2 = forecast2)
dm
#> -- Forecast Evaluation (Diebold-Mariano Test) --
#> * p-value: 0.08
#> * No significant difference in predictive accuracy.
```


## Where do I get CPI data?

`inflationkit` is a pure computation package. It does not download data. You supply data frames with component-level price changes and weights. Common sources:

| Source | Coverage | Free? | How to get into R |
|--------|----------|-------|-------------------|
| ONS (UK CPI) | United Kingdom | Yes | [ons](https://cran.r-project.org/package=ons) |
| BLS (US CPI) | United States | Yes | [fred](https://cran.r-project.org/package=fred) or blscrapeR |
| ECB (HICP) | Euro area | Yes | [readecb](https://cran.r-project.org/package=readecb) |
| OECD | 38 countries | Yes | [readoecd](https://cran.r-project.org/package=readoecd) |
| World Bank | 200+ countries | Yes | WDI package |
| Statistics offices | Any country | Varies | Download CSV from national statistics website |

Or use the built-in sample data: `ik_sample_data("components")` and `ik_sample_data("headline")`.


## Conventions

- **price_change** values are period-on-period rates (e.g., `0.003` = 0.3% monthly inflation).
- **weights** should sum to 1 within each period. If they do not, functions normalise them internally.
- Positive values mean prices are rising.


## Functions

| Function | Description |
|----------|-------------|
| `ik_decompose()` | Weighted contributions of CPI components to headline inflation |
| `ik_core()` | Core inflation (trimmed mean, weighted median, exclusion, asymmetric trim) |
| `ik_sticky_flexible()` | Sticky vs flexible price decomposition (Atlanta Fed methodology) |
| `ik_persistence()` | Inflation persistence (sum of AR coefficients, half-life, largest root) |
| `ik_diffusion()` | Diffusion index (fraction of items with rising prices) |
| `ik_phillips()` | Phillips curve estimation (traditional, expectations-augmented, hybrid) |
| `ik_breakeven()` | Breakeven inflation from nominal and real bond yields |
| `ik_trend()` | Trend extraction (HP filter, Beveridge-Nelson, exponential smoothing, moving average) |
| `ik_forecast_eval()` | Forecast evaluation (Mincer-Zarnowitz bias, Nordhaus efficiency, Diebold-Mariano) |
| `ik_compare()` | Compare multiple core inflation measures side by side |
| `ik_sample_data()` | Built-in sample CPI and macro data for examples |

All functions return S3 objects with `print()` and `plot()` methods.


## Academic references

The methods implemented in this package are based on:

- Bryan, M.F. & Cecchetti, S.G. (1994). "Measuring Core Inflation." In *Monetary Policy*, ed. N.G. Mankiw, University of Chicago Press, 195-215. [doi:10.1016/0304-3932(94)90030-2](https://doi.org/10.1016/0304-3932(94)90030-2)
- Bils, M. & Klenow, P.J. (2004). "Some Evidence on the Importance of Sticky Prices." *Journal of Political Economy*, 112(5), 947-985.
- Marques, C.R. (2004). "Inflation Persistence: Facts or Artefacts?" ECB Working Paper No. 371.
- Beveridge, S. & Nelson, C.R. (1981). "A New Approach to Decomposition of Economic Time Series." *Journal of Monetary Economics*, 7(2), 151-174.
- Diebold, F.X. & Mariano, R.S. (1995). "Comparing Predictive Accuracy." *Journal of Business & Economic Statistics*, 13(3), 253-263.


## Related packages

| Package | Description |
|---------|-------------|
| [inflateR](https://github.com/charlescoverdale/inflateR) | Adjust monetary values for inflation |
| [ons](https://github.com/charlescoverdale/ons) | UK Office for National Statistics data (includes CPI) |
| [fred](https://github.com/charlescoverdale/fred) | Federal Reserve Economic Data (includes US CPI) |
| [readecb](https://github.com/charlescoverdale/readecb) | European Central Bank data (includes HICP) |


## Issues

Found a bug or have a feature request? Open an issue at <https://github.com/charlescoverdale/inflationkit/issues>.


## Keywords

r, r-package, inflation, cpi, core-inflation, trimmed-mean, weighted-median, phillips-curve, macroeconomics, monetary-policy, price-index, beveridge-nelson, inflation-persistence
