# inflationkit 0.2.0

## Bug fixes

* Fixed `ik_persistence(method = "largest_root")` to return values in [0,1] for
  stationary processes.
* Rewrote `ik_trend(method = "beveridge_nelson")` to correctly implement the BN
  decomposition (Morley 2002): fits AR to first differences and uses companion
  form matrix.
* Added weight normalization in `ik_decompose()` so weights that don't sum to 1
  are handled correctly.

## New features

* Dallas Fed asymmetric trimmed mean via `ik_core(method = "asymmetric_trim")`
  with `trim_lower` and `trim_upper` parameters (defaults: 24% lower, 31%
  upper).
* Newey-West HAC standard errors for Phillips curve via
  `ik_phillips(robust_se = "HAC")` with automatic bandwidth selection.
* Weight normalization warning in `ik_core()` when weights don't sum to
  approximately 1.

## Documentation

* Added note to `ik_breakeven()` about inflation risk premium and liquidity
  premium limitations.

# inflationkit 0.1.0

* Initial release.
* CPI component decomposition into weighted contributions via `ik_decompose()`.
* Core inflation measures (trimmed mean, weighted median, exclusion-based)
  via `ik_core()`, following Bryan and Cecchetti (1994).
* Sticky vs flexible price decomposition via `ik_sticky_flexible()`, based on
  Atlanta Fed methodology (Bils and Klenow, 2004).
* Inflation persistence estimation (sum of AR coefficients, half-life, largest
  root) via `ik_persistence()`.
* Diffusion indices via `ik_diffusion()`.
* Phillips curve estimation (traditional, expectations-augmented, hybrid) via
  `ik_phillips()`.
* Breakeven inflation from nominal and real yields via `ik_breakeven()`.
* Trend inflation extraction (Hodrick-Prescott filter, Beveridge-Nelson
  decomposition, exponential smoothing, moving average) via `ik_trend()`.
* Forecast evaluation (Mincer-Zarnowitz bias, Nordhaus efficiency,
  Diebold-Mariano test) via `ik_forecast_eval()`.
* Side-by-side comparison of core inflation measures via `ik_compare()`.
* Built-in sample data via `ik_sample_data()`.
