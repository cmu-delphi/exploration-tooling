rscript := env_var_or_default("RSCRIPT", "Rscript")

# Render calibration experiment notebooks (all, or the named ones, e.g. e00_vintage_backtest) and the index.
calibration-experiments *notebooks:
    {{rscript}} scripts/calibration/render_experiments.R {{notebooks}}
