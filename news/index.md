# Changelog

## inflationkit 0.1.0

CRAN release: 2026-03-26

- Initial release.
- CPI component decomposition into weighted contributions via
  [`ik_decompose()`](https://charlescoverdale.github.io/inflationkit/reference/ik_decompose.md).
- Core inflation measures (trimmed mean, weighted median,
  exclusion-based, asymmetric trim) via
  [`ik_core()`](https://charlescoverdale.github.io/inflationkit/reference/ik_core.md),
  following Bryan and Cecchetti (1994).
- Sticky vs flexible price decomposition via
  [`ik_sticky_flexible()`](https://charlescoverdale.github.io/inflationkit/reference/ik_sticky_flexible.md),
  based on Atlanta Fed methodology (Bils and Klenow, 2004).
- Inflation persistence estimation (sum of AR coefficients, half-life
  via companion matrix, largest root) via
  [`ik_persistence()`](https://charlescoverdale.github.io/inflationkit/reference/ik_persistence.md).
- Diffusion indices via
  [`ik_diffusion()`](https://charlescoverdale.github.io/inflationkit/reference/ik_diffusion.md).
- Phillips curve estimation (traditional, expectations-augmented,
  hybrid) via
  [`ik_phillips()`](https://charlescoverdale.github.io/inflationkit/reference/ik_phillips.md).
  Newey-West HAC standard errors via `robust_se = "HAC"`.
- Breakeven inflation from nominal and real yields via
  [`ik_breakeven()`](https://charlescoverdale.github.io/inflationkit/reference/ik_breakeven.md).
- Trend inflation extraction (Hodrick-Prescott filter with
  frequency-based lambda, Beveridge-Nelson decomposition, exponential
  smoothing, moving average) via
  [`ik_trend()`](https://charlescoverdale.github.io/inflationkit/reference/ik_trend.md).
- Forecast evaluation (Mincer-Zarnowitz bias, Nordhaus efficiency,
  Diebold-Mariano test) via
  [`ik_forecast_eval()`](https://charlescoverdale.github.io/inflationkit/reference/ik_forecast_eval.md).
- Side-by-side comparison of core inflation measures via
  [`ik_compare()`](https://charlescoverdale.github.io/inflationkit/reference/ik_compare.md).
- Built-in sample data via
  [`ik_sample_data()`](https://charlescoverdale.github.io/inflationkit/reference/ik_sample_data.md).
