# inflationkit

<!-- badges: start -->
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

**Inflation decomposition, core measures, and trend estimation for R.**

To our knowledge, `inflationkit` is the first open-source R package for comprehensive inflation analysis. The only CRAN incumbent (`Inflation`) is Brazil-specific and has not been updated since 2017.

## Why does this package exist?

Every central bank publishes core inflation measures. The Atlanta Fed computes sticky vs flexible price inflation. The Cleveland Fed publishes a trimmed mean and weighted median. The ECB has its "supercore" indicator. Academic economists estimate Phillips curves, measure inflation persistence, and extract trend inflation using state-space models.

Yet there is no standard tool for any of this. Analysts either use proprietary internal systems (at central banks) or write one-off scripts that reimplement the same methods from scratch. The result is duplicated effort, inconsistent implementations, and analyses that are hard to reproduce.

The core analyses in inflation research are well-established but non-trivial to implement correctly:

- **CPI decomposition.** Headline inflation is a weighted average of hundreds of component price changes. Understanding *what is driving inflation* requires computing each component's weighted contribution. This sounds simple but requires careful handling of weight normalisation and base effects.

- **Core inflation measures.** Headline CPI is noisy because of volatile items like food and energy. Core measures try to extract the underlying trend. The trimmed mean (Bryan and Cecchetti, 1994) removes the tails of the cross-sectional price change distribution. The weighted median takes the 50th percentile. Exclusion-based core removes pre-specified items. Each method makes different assumptions about what "noise" means.

- **Sticky vs flexible prices.** Some prices change frequently (petrol, fresh food); others are sticky (rent, insurance, education). The Atlanta Fed decomposes CPI into these two groups. Sticky-price inflation is a better predictor of future inflation because it reflects pricing decisions that incorporate expectations.

- **Inflation persistence.** How quickly does inflation return to its mean after a shock? This matters for monetary policy: high persistence means rate hikes take longer to work. Measured via the sum of autoregressive coefficients (Marques, 2004).

- **Phillips curve.** The relationship between inflation and economic slack (output gap or unemployment). Estimating this correctly requires controlling for expectations and lagged inflation. The slope of the Phillips curve has been a central debate in macroeconomics for decades.

- **Trend inflation.** Separating the permanent component of inflation from transitory shocks. The Hodrick-Prescott filter is simple but has known endpoint problems. The Beveridge-Nelson decomposition uses the ARIMA structure of the data.

`inflationkit` puts all of these methods into clean, tested R functions. You bring CPI component data from any source, and the package handles the analysis.

## Installation

Install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("charlescoverdale/inflationkit")
```

## Quick start

The package includes built-in sample data so you can try everything immediately.

```r
library(inflationkit)

# Built-in sample: 10 CPI components, 120 months of data
d <- ik_sample_data("components")
head(d)
#>         date    item weight price_change
#> 1 2014-01-01    Food  0.135      0.00218
#> 2 2014-01-01 Housing  0.330      0.00274
#> 3 2014-01-01 Transport 0.105    0.00189
#> ...
```

### Conventions

- **price_change** values are period-on-period rates (e.g., `0.003` = 0.3% monthly inflation)
- **weights** sum to 1 within each period
- Positive values mean prices are rising

## Examples

### Decompose headline inflation

What is driving inflation? Compute each component's weighted contribution.

```r
d <- ik_sample_data("components")
decomp <- ik_decompose(d)
decomp
#> -- CPI Decomposition --
#> * Period: 2014-01-01 to 2023-12-01 (120 periods)
#> * Components: 10
#> * Mean headline inflation: 0.25% per period

plot(decomp)  # Stacked bar chart of contributions
```

### Core inflation measures

Strip out the noise from headline CPI. Compare trimmed mean, weighted median, and exclusion-based core.

```r
d <- ik_sample_data("components")

# Trimmed mean: remove top and bottom 8% by weight (Cleveland Fed standard)
tm <- ik_core(d, method = "trimmed_mean", trim = 0.08)

# Weighted median: 50th percentile of price changes
wm <- ik_core(d, method = "weighted_median")

# Exclusion: remove food and energy
ex <- ik_core(d, method = "exclusion", exclude = c("Food", "Transport"))

# Asymmetric trim: Dallas Fed style (24% lower, 31% upper)
at <- ik_core(d, method = "asymmetric_trim")

# Compare all four
comp <- ik_compare(tm, wm, ex, at,
                   labels = c("Trimmed mean", "Weighted median",
                              "Ex food & transport", "Asymmetric trim"))
plot(comp)
```

### Inflation persistence

How quickly does inflation return to its mean after a shock?

```r
d <- ik_sample_data("headline")

pers <- ik_persistence(d$inflation, method = "sum_ar")
pers
#> -- Inflation Persistence (Sum of AR Coefficients) --
#> * AR order: 4 (selected by BIC)
#> * Persistence: 0.72
#> * Interpretation: Moderate persistence
```

### Phillips curve

Estimate the relationship between inflation and economic slack.

```r
d <- ik_sample_data("headline")

pc <- ik_phillips(d$inflation, d$output_gap, type = "traditional", lags = 4)
pc
#> -- Phillips Curve (Traditional) --
#> * Slope: -0.12 (p = 0.03)
#> * R-squared: 0.45
#> * Observations: 76

# With Newey-West HAC standard errors (robust to serial correlation)
pc_hac <- ik_phillips(d$inflation, d$output_gap, type = "traditional",
                      lags = 4, robust_se = "HAC")

plot(pc)  # Scatter plot with fitted line
```

### Trend inflation

Extract the permanent component of inflation using the Hodrick-Prescott filter.

```r
d <- ik_sample_data("headline")

trend <- ik_trend(d$inflation, method = "hp")
trend
#> -- Inflation Trend (Hodrick-Prescott Filter) --
#> * Lambda: 1600
#> * Mean trend: 2.8%
#> * Cycle volatility (SD): 0.5 pp

plot(trend)  # Original series with trend overlay
```

### Breakeven inflation

Compute market-implied inflation expectations from nominal and real yields.

```r
be <- ik_breakeven(
  nominal_yield = c(0.042, 0.040, 0.043),
  real_yield = c(0.018, 0.015, 0.020),
  maturity = c(5, 10, 30)
)
be
#> -- Breakeven Inflation --
#>   maturity nominal  real breakeven
#> 1        5  0.0420 0.018    0.0240
#> 2       10  0.0400 0.015    0.0250
#> 3       30  0.0430 0.020    0.0230

plot(be)  # Breakeven term structure
```

### Forecast evaluation

Test whether inflation forecasts are unbiased and efficient.

```r
actual <- c(2.1, 2.3, 2.5, 2.2, 2.8, 3.0, 2.7, 2.4)
forecast1 <- c(2.0, 2.2, 2.3, 2.1, 2.5, 2.8, 2.6, 2.3)

bias <- ik_forecast_eval(actual, forecast1, test = "bias")
bias
#> -- Forecast Evaluation (Bias Test) --
#> * Alpha: 0.15 (p = 0.42)
#> * Beta: 0.94 (p = 0.00)
#> * Conclusion: Cannot reject unbiasedness
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

Or use the built-in sample data: `ik_sample_data("components")`.

## Functions

| Function | Description |
|----------|-------------|
| `ik_decompose()` | Weighted contributions of CPI components to headline |
| `ik_core()` | Core inflation (trimmed mean, weighted median, exclusion, asymmetric trim) |
| `ik_sticky_flexible()` | Sticky vs flexible price decomposition |
| `ik_persistence()` | Inflation persistence (sum of AR, half-life, largest root) |
| `ik_diffusion()` | Diffusion index (fraction of items with rising prices) |
| `ik_phillips()` | Phillips curve estimation |
| `ik_breakeven()` | Breakeven inflation from nominal and real yields |
| `ik_trend()` | Trend extraction (HP filter, Beveridge-Nelson, exponential smooth) |
| `ik_forecast_eval()` | Forecast evaluation (bias, efficiency, Diebold-Mariano) |
| `ik_compare()` | Compare multiple core inflation measures |
| `ik_sample_data()` | Built-in sample CPI and macro data |

## Academic references

The package implements methods from:

- Bryan, M.F. & Cecchetti, S.G. (1993). "Measuring Core Inflation." NBER Working Paper No. 4303.
- Bryan, M.F. & Cecchetti, S.G. (1994). "Measuring Core Inflation." In *Monetary Policy*, ed. N.G. Mankiw, 195-215.
- Bils, M. & Klenow, P.J. (2004). "Some Evidence on the Importance of Sticky Prices." *Journal of Political Economy*, 112(5), 947-985.
- Marques, C.R. (2004). "Inflation Persistence: Facts or Artefacts?" ECB Working Paper No. 371.
- Diebold, F.X. & Mariano, R.S. (1995). "Comparing Predictive Accuracy." *Journal of Business & Economic Statistics*, 13(3), 253-263.
- Beveridge, S. & Nelson, C.R. (1981). "A New Approach to Decomposition of Economic Time Series." *Journal of Monetary Economics*, 7(2), 151-174.

## Related packages

| Package | Description | CRAN |
|---------|-------------|------|
| [inflateR](https://github.com/charlescoverdale/inflateR) | Adjust monetary values for inflation | [![CRAN](https://www.r-pkg.org/badges/version/inflateR)](https://cran.r-project.org/package=inflateR) |
| [ons](https://github.com/charlescoverdale/ons) | UK Office for National Statistics data (includes CPI) | [![CRAN](https://www.r-pkg.org/badges/version/ons)](https://cran.r-project.org/package=ons) |
| [fred](https://github.com/charlescoverdale/fred) | Federal Reserve Economic Data (includes US CPI) | [![CRAN](https://www.r-pkg.org/badges/version/fred)](https://cran.r-project.org/package=fred) |
| [readecb](https://github.com/charlescoverdale/readecb) | European Central Bank data (includes HICP) | [![CRAN](https://www.r-pkg.org/badges/version/readecb)](https://cran.r-project.org/package=readecb) |

r, r-package, inflation, cpi, core-inflation, trimmed-mean, phillips-curve, macroeconomics, monetary-policy, price-index
