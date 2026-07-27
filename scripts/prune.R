#!/usr/bin/env Rscript
# Prune a project's store, companion to scripts/run.R. Selects the project via
# TAR_RUN_PROJECT and passes script/store explicitly, because relying on
# TAR_PROJECT is fragile: an .Renviron that sets it overrides the shell
# environment on every R start, including tar_prune's callr subprocess.
tar_project <- Sys.getenv("TAR_RUN_PROJECT", "flu_hosp_prod")
cli::cli_inform("Pruning project {tar_project}...")
targets::tar_prune(
  script = targets::tar_config_get("script", project = tar_project),
  store = targets::tar_config_get("store", project = tar_project)
)
