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
