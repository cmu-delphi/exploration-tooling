# R isn't installed on the host; run recipes inside the distrobox: `distrobox enter rocker -- just <recipe>`.
rscript := env_var_or_default("RSCRIPT", "Rscript")

# Render calibration experiment notebooks (all, or the named ones, e.g. e18_ref_op_bridge) and the index.
calibration-experiments *notebooks:
    {{rscript}} scripts/calibration/render_experiments.R {{notebooks}}
