# inflationkit

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/inflationkit)](https://CRAN.R-project.org/package=inflationkit)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/inflationkit)](https://CRAN.R-project.org/package=inflationkit)
[![Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/inflationkit)](https://CRAN.R-project.org/package=inflationkit)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

A technical working paper for this package can be found [here](https://charlescoverdale.github.io/files/coverdale_inflationkit_2026.pdf).

An R package for inflation analysis. Decompose headline inflation into component contributions, compute core measures (trimmed mean, weighted median, exclusion-based), estimate how persistent inflation is, fit Phillips curves, and extract the underlying trend from noisy price data.

## Installation

```r
install.packages("inflationkit")

# Or install the development version from GitHub
# install.packages("devtools")
devtools::install_github("charlescoverdale/inflationkit")
```

```r
library(inflationkit)

# Built-in sample data: 10 CPI components, 120 months
d <- ik_sample_data("components")
head(d)
#>        date item weight price_change
#>  2015-01-01 Food   0.15     0.008484
#>  2015-02-01 Food   0.15     0.000741
#>  2015-03-01 Food   0.15     0.004453
#> ...

# What is driving inflation?
decomp <- ik_decompose(d)
plot(decomp)
```


## Why inflationkit?

Headline inflation is noisy. Food and energy prices swing month to month, making it hard to tell whether inflation is genuinely rising or just reacting to a temporary shock. Central banks solve this by computing "core" measures (trimmed means, weighted medians, exclusion-based indices) and studying persistence, diffusion, and trend. These are standard tools in every central bank research department, but there is no general-purpose R package for any of them. The only CRAN package with "inflation" in its name (`Inflation`) is Brazil-specific and has not been updated since 2017.

`inflationkit` fills this gap. You bring CPI component data from any country, and the package handles decomposition, core measures, persistence estimation, Phillips curves, trend extraction, and forecast evaluation. Functions accept any column names, so you can plug in your own dataset without reshaping it first.


## Examples

### What is driving inflation?

Decompose headline inflation into the weighted contribution of each component. This tells you how many percentage points each category (food, housing, transport, etc.) is adding to the total.

```r
d <- ik_sample_data("components")
decomp <- ik_decompose(d)
decomp
#> -- Inflation Decomposition -----------------------------------------------------
#> * Period: 2015-01-01 to 2024-12-01
#> * Number of items: 10
#> * Mean headline inflation: 0.26%

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
#> -- Inflation Persistence -------------------------------------------------------
#> * Method: Sum of AR Coefficients
#> * AR order: 1
#> * Persistence value: 0.647
#> * Moderate persistence (sum of AR coefficients 0.5 to 0.8)

# Half-life: how many quarters until a shock decays by half?
pers_hl <- ik_persistence(d$inflation, method = "half_life")
pers_hl
#> -- Inflation Persistence -------------------------------------------------------
#> * Method: Half-Life
#> * AR order: 1
#> * Persistence value: 1.592
#> * Low persistence (half-life < 4 periods)
```

### Is there a Phillips curve?

Estimate the relationship between inflation and unemployment. A negative slope means higher unemployment is associated with lower inflation.

```r
d <- ik_sample_data("headline")

pc <- ik_phillips(d$inflation, d$unemployment, type = "traditional", lags = 4)
pc
#> -- Phillips Curve Estimation ---------------------------------------------------
#> * Type: Traditional
#> * Slope estimate: -0.1475 (p = 0.0801) *
#> * R-squared: 0.4342
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
#> -- Trend Inflation -------------------------------------------------------------
#> * Method: Hodrick-Prescott (lambda = 1600)
#> * Mean trend: 1.526
#> * Cycle volatility (SD): 0.3405
#> * Observations: 80

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
#> -- Forecast Evaluation ---------------------------------------------------------
#> * Test: Mincer-Zarnowitz Bias Test
#> * Test statistic: 18.8543
#> * p-value: 0.0026
#> * Reject H0: forecasts appear biased (alpha != 0 or beta != 1).
#> * Coefficients: alpha = -0.179, beta = 1.14

# Diebold-Mariano: is forecast1 better than forecast2?
dm <- ik_forecast_eval(actual, forecast1, test = "dm", forecast2 = forecast2)
dm
#> -- Forecast Evaluation ---------------------------------------------------------
#> * Test: Diebold-Mariano Test
#> * Test statistic: -2.6306
#> * p-value: 0.0085
#> * Forecast 1 is significantly more accurate than Forecast 2.
#> * Coefficients: mean_loss_diff = -0.0387
```


## What data do I need?

Most functions take a data frame with CPI components. Each row is one item in one period. The default column names are `date`, `item`, `weight`, and `price_change`:

| Column | What it is |
|--------|-----------|
| `date` | Date of the observation (e.g. `2024-01-01`) |
| `item` | Component name (e.g. `"Food"`, `"Housing"`, `"Transport"`) |
| `weight` | CPI basket weight as a proportion (0.15 = 15%). Should sum to 1 within each date. If they do not, functions normalise them internally. |
| `price_change` | Period-on-period price change as a decimal (0.003 = 0.3% monthly inflation). Positive means prices are rising. |

If your data uses different column names, every function has `date_col`, `item_col`, `weight_col`, and `change_col` arguments so you can map them:

```r
# Your data has columns: month, category, share, pct_change
ik_decompose(my_data,
             date_col = "month", item_col = "category",
             weight_col = "share", change_col = "pct_change")
```

Some functions (`ik_persistence`, `ik_phillips`, `ik_trend`, `ik_forecast_eval`) take simple numeric vectors instead of a data frame.

### Option 1: Use the built-in sample data

The package includes sample datasets so you can try everything immediately:

```r
library(inflationkit)

# Component-level data: 10 CPI items, 120 months
d <- ik_sample_data("components")
head(d)
#>        date item weight price_change
#>  2015-01-01 Food   0.15     0.008484
#>  2015-02-01 Food   0.15     0.000741
#>  2015-03-01 Food   0.15     0.004453
#>  2015-04-01 Food   0.15     0.005531
#>  2015-05-01 Food   0.15     0.004617
#>  2015-06-01 Food   0.15     0.002576

# Headline data: monthly inflation + unemployment
h <- ik_sample_data("headline")
head(h)
#>        date inflation output_gap unemployment
#>  2005-01-01     2.500      0.000        5.500
#>  2005-04-01     2.275      0.685        5.056
#>  2005-07-01     1.963      0.796        5.303
#>  2005-10-01     2.466      1.313        4.946
#>  2006-01-01     2.760      0.888        5.536
#>  2006-04-01     2.224     -0.073        5.446
```

### Option 2: Pull UK CPI data from the ONS

```r
# 1. Install the data package (one time)
install.packages("ons")

# 2. Download CPI component series
library(ons)
cpi <- ons_cpi()   # Returns monthly CPI index by component

# 3. Compute month-on-month price changes and reshape for inflationkit
library(inflationkit)
# Your data frame needs: date, item, weight, price_change
# See ons package documentation for component weights
```

### Option 3: Pull US CPI data from FRED

```r
# 1. Install the data package (one time)
install.packages("fred")

# 2. Get a free API key from https://fred.stlouisfed.org/docs/api/api_key.html
library(fred)
fred_set_key("your_api_key_here")

# 3. Download CPI sub-indices
food    <- fred_series("CPIFABSL")    # Food
housing <- fred_series("CPIHOSSL")    # Housing
transport <- fred_series("CPITRNSL")  # Transportation

# 4. Compute month-on-month changes, assign weights, and stack into
#    a data frame with columns: date, item, weight, price_change
```

### Where to find CPI data

| Source | Coverage | How to get into R |
|--------|----------|-------------------|
| ONS (UK CPI) | United Kingdom | [ons](https://cran.r-project.org/package=ons) |
| BLS (US CPI) | United States | [fred](https://cran.r-project.org/package=fred) |
| ECB (HICP) | Euro area | [readecb](https://cran.r-project.org/package=readecb) |
| OECD | 38 countries | [readoecd](https://cran.r-project.org/package=readoecd) |
| World Bank | 200+ countries | WDI package |
| Statistics offices | Any country | Download CSV from national statistics website |


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

- Bryan, M.F. & Cecchetti, S.G. (1994). "Measuring Core Inflation." In *Monetary Policy*, ed. N.G. Mankiw, University of Chicago Press, 195-219. (NBER Working Paper [w4303](https://www.nber.org/papers/w4303))
- Bils, M. & Klenow, P.J. (2004). "Some Evidence on the Importance of Sticky Prices." *Journal of Political Economy*, 112(5), 947-985. [doi:10.1086/422559](https://doi.org/10.1086/422559)
- Marques, C.R. (2004). "Inflation Persistence: Facts or Artefacts?" ECB Working Paper No. 371.
- Beveridge, S. & Nelson, C.R. (1981). "A New Approach to Decomposition of Economic Time Series." *Journal of Monetary Economics*, 7(2), 151-174.
- Diebold, F.X. & Mariano, R.S. (1995). "Comparing Predictive Accuracy." *Journal of Business & Economic Statistics*, 13(3), 253-263.


## Related packages

| Package | Description |
|---------|-------------|
| [inflateR](https://github.com/charlescoverdale/inflateR) | Adjust monetary values for inflation (simpler companion) |
| [ons](https://github.com/charlescoverdale/ons) | UK Office for National Statistics data (CPI, RPI, GDP deflator) |
| [fred](https://github.com/charlescoverdale/fred) | Federal Reserve Economic Data (includes US CPI) |
| [readecb](https://github.com/charlescoverdale/readecb) | European Central Bank data (includes HICP) |
| [readoecd](https://github.com/charlescoverdale/readoecd) | OECD international price data |
| [nowcast](https://github.com/charlescoverdale/nowcast) | Economic nowcasting (inflation nowcasts) |
| [mpshock](https://github.com/charlescoverdale/mpshock) | Monetary policy shock series |


## Issues

Found a bug or have a feature request? Open an issue at <https://github.com/charlescoverdale/inflationkit/issues>.


## Keywords

r, r-package, inflation, cpi, core-inflation, trimmed-mean, weighted-median, phillips-curve, macroeconomics, monetary-policy, price-index, beveridge-nelson, inflation-persistence
