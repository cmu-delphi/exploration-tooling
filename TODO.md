# TODO

See the dated files in `notes/`.


Done. scripts/reports/calibration_qt.html is rendered with everything visible (paged tables), and the sweep is saved to cache/calibration/sweep.rds for follow-up. Here's what the numbers say — worth knowing before you review:

Your question — window or the 0.1 constant? The multiplier is the control knob; the window barely matters.
- Averaged per horizon, the WIS-improvement spread across multipliers is 8–63 percentage points; across windows it's 1.5–5. Windows 20/50/Inf are nearly indistinguishable (and in season 2024-25, 50 and Inf are identical — fewer than 50 observed rounds); only window 8 behaves noticeably differently, and not clearly better.
- So the ratcheting-learning-rate mechanism we documented is real but immaterial at this data size, exactly what the earlier single-series check hinted.

The interesting finding: the paper's mult = 0.1 overpays for calibration. At the defaults (carry/50/0.1), calibration error improves 70–85% but WIS degrades −3.6% (h0) to −22.9% (h3). Dropping to mult = 0.03 gets equal or better calibration error at horizons ≥ 1 (e.g. h2: 0.026 vs 0.031) at a fraction of the WIS cost (−2 to −6%). mult = 0.01 is near WIS-parity but converges too slowly — its first-live-season calibration error is bad (0.07–0.10). The caveat on 0.03 is the same in milder form: season 2024-25 is worse than 0.1 (0.052 vs 0.041), and it only dominates once warmed up (2025-26: better on both metrics at h ≥ 1). The tradeoff is really convergence speed vs steady-state sharpness cost.

Two more things for the collaborator email: horizon −1 calibration improves WIS outright (+4%) — calibration is free there; and carry-vs-reset across the off-season is a minor effect (carry slightly better at h3, otherwise a wash).

The notebook now has: WIS-labeled headline + per-season split, the PP plots (per-location spaghetti + pooled), rolling calibration-vs-WIS tradeoff, the factorial sweep with the four reads above, and the offset diagnostics. Ball's in your court to review and send; after that it's the windowed_seasonal_extra_sources retrospective and integration design. One thing that finding sharpens for that next step: with mult ≈ 0.03 the tracker needs most of a season to warm up, so the burn-in/state-carrying question becomes the central integration design issue.
