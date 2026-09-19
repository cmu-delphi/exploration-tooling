# R/eval/

Evaluation and visualisation utilities.

## Files

- **scoring.R** — `evaluate_predictions`: scores forecasts against truth using `scoringutils` (WIS, AE, interval coverage at 50%/90%).
- **plotting.R** — Quantile fan plots (`plot_forecasts`, `apply_view_defaults`, `get_default_truth_data`), season phase helpers (`Mean`, `GeoMean`, `compute_peak_season`, `classify_phase`).
