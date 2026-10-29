set shell := ["bash", "-euo", "pipefail", "-c"]

bucket := "s3://forecasting-team-data/2024"
today := `date +%F`
diseases := "covid flu rsv"
stores := "covid_hosp_prod covid_hosp_evaluation covid_hosp_explore flu_hosp_prod flu_hosp_evaluation flu_hosp_explore rsv_hosp_prod rsv_hosp_evaluation rsv_hosp_explore"
# S3 sync covers aux_data and every store except rsv_hosp_evaluation.
synced := "covid_hosp_prod flu_hosp_prod rsv_hosp_prod covid_hosp_evaluation flu_hosp_evaluation covid_hosp_explore flu_hosp_explore rsv_hosp_explore"
load_all := "suppressPackageStartupMessages(source(here::here('R', 'load_all.R')))"
# Nested calls reuse this binary and Justfile.
rejust := quote(just_executable()) + " -f " + quote(justfile())

default:
    @{{rejust}} --list

[group('setup-sync-clean')]
install:
    Rscript -e "install.packages(c('renv', 'pak', 'rspm')); renv::restore()"

[group('deprecated')]
test:
    Rscript -e "testthat::test_dir('tests/testthat')"

# ---- Pipelines ----

# Output is tee'd to local_logs/<project>/<run id>/runner.log, next to the run's worker logs.
# Evaluation env knobs (EVALUATION_N_DATES, EVALUATION_DATES, ...) pass through; see AGENTS.md.
# Run one targets pipeline without publishing, e.g. `just run flu evaluation`. kind: prod | evaluation | explore.
run disease kind="prod":
    #!/usr/bin/env bash
    set -euo pipefail
    export TAR_RUN_PROJECT={{disease}}_hosp_{{kind}} TAR_RUN_ID=$(date +%Y%m%dT%H%M%S)
    dir=local_logs/$TAR_RUN_PROJECT/$TAR_RUN_ID
    mkdir -p "$dir"
    Rscript scripts/run.R 2>&1 | tee -a "$dir/runner.log"

# Prod pipelines (one disease or all three), then publish once.
[group('production')]
prod disease="all":
    for d in {{ if disease == "all" { diseases } else { disease } }}; do {{rejust}} run "$d" prod; done
    {{rejust}} publish-netlify

# Historical replay + scoring in the disease's own evaluation store.
[group('exploration')]
eval disease: (run disease "evaluation")

# Exploration sweeps (one disease, or all three and then publish).
[group('exploration')]
explore disease="all":
    for d in {{ if disease == "all" { diseases } else { disease } }}; do {{rejust}} run "$d" explore; done
    {{ if disease == "all" { rejust + " publish-netlify" } else { "" } }}

# One-off comparison of prod vs explore scores.
[group('deprecated')]
prod-compare disease:
    Rscript scripts/one_offs/render_prod_explore_comparison.R {{disease}}

# The project goes through TAR_RUN_PROJECT because an .Renviron TAR_PROJECT would override a shell export.
# Prune a store via scripts/prune.R, e.g. `just prune flu_hosp_evaluation`; default prunes all nine.
[group('setup-sync-clean')]
prune project="all":
    for p in {{ if project == "all" { stores } else { project } }}; do TAR_RUN_PROJECT="$p" Rscript scripts/prune.R; done

# Show errors from a project's last pipeline run.
[group('debugging')]
errors project="flu_hosp_prod":
    Rscript -e "{{load_all}}; get_targets_errors(project = '{{project}}')"

# ---- Prod monitoring ----

# Summarize today's systemd prod run: freshness polls, pipelines, site, netlify.
[group('production')]
status:
    @Rscript scripts/prod_status.R

# Print the latest run's log directory and runner.log path. See notes/run-logs.md.
[group('debugging')]
logs project:
    #!/usr/bin/env bash
    set -euo pipefail
    run_dir=$(ls -d local_logs/{{project}}/*/ 2>/dev/null | sort | tail -n 1 || true)
    [ -n "$run_dir" ] || { echo "no runs logged for {{project}} in local_logs/" >&2; exit 1; }
    echo "run dir: ${run_dir%/}"
    if [ -e "${run_dir}runner.log" ]; then echo "runner:  ${run_dir}runner.log"; else echo "runner:  (none; started without the run recipe)"; fi

# Follow the latest run's runner log and worker logs as one stream, including logs created later.
[group('debugging')]
logs-follow project:
    @scripts/tail_logs.sh {{project}}

# Print when the CDC Socrata datasets last updated. Fail-soft: one bad fetch doesn't stop the rest.
[group('debugging')]
check-socrata-updates:
    @echo "NHSN Regular Update:"
    -@curl -s https://data.cdc.gov/api/views/ua7e-t2fy | python3 -c "import sys, json; print(json.load(sys.stdin)['rowsUpdatedAt'])" | xargs -I {} date -d "@{}"
    @echo "NHSN Prelim Update:"
    -@curl -s https://data.cdc.gov/api/views/mpgq-jmmr | python3 -c "import sys, json; print(json.load(sys.stdin)['rowsUpdatedAt'])" | xargs -I {} date -d "@{}"
    @echo "NSSP Update:"
    -@curl -s https://data.cdc.gov/api/views/rdmq-nq56 | python3 -c "import sys, json; print(json.load(sys.stdin)['rowsUpdatedAt'])" | xargs -I {} date -d "@{}"

# ---- Oracle: behavior-preserving golden capture + compare ----

# Pinned so captures are reproducible week-to-week instead of tracking Sys.Date().
oracle_reference_date := "2026-06-24"

# Capture a golden, e.g. `just oracle-capture flu_hosp_prod refactored`; n limits evaluation replays to the last n dates.
[group('debugging')]
oracle-capture project label n="":
    mkdir -p cache/logs
    TAR_RUN_PROJECT={{project}} ORACLE_LABEL={{label}} FORECAST_REFERENCE_DATE={{oracle_reference_date}} EVALUATION_N_DATES={{n}} \
        Rscript scripts/oracle/capture.R 2>&1 | tee -a cache/logs/oracle_capture

# Exits non-zero when anything differs; that is the signal, not an error.
# Diff two captured labels of one project, e.g. `just oracle-compare flu_hosp_prod baseline refactored`.
[group('debugging')]
oracle-compare project a b:
    Rscript scripts/oracle/compare.R {{project}}:{{a}} {{project}}:{{b}}

# ---- Submission ----

_hub_dir disease:
    @echo {{ if disease == "covid" { "../covid19-forecast-hub" } else if disease == "flu" { "../FluSight-forecast-hub" } else if disease == "rsv" { "../rsv-forecast-hub" } else { error("unknown disease: " + disease) } }}

_hub_repo disease:
    @echo {{ if disease == "covid" { "CDCgov/covid19-forecast-hub" } else if disease == "flu" { "cdcepi/FluSight-forecast-hub" } else if disease == "rsv" { "CDCgov/rsv-forecast-hub" } else { error("unknown disease: " + disease) } }}

# Commit this week's forecast in the disease's hub checkout.
[group('production')]
commit disease:
    ./scripts/commit-script.sh "$({{rejust}} _hub_dir {{disease}})"

# Other staged or dirty files stay out of the commit. Refuses to run off main, so a push can't carry
# unrelated branch commits. Rebases onto origin first because origin usually has new commits.
# Commit only the disease's weights files and push them to main.
[group('production')]
push-weights disease:
    [ "$(git branch --show-current)" = main ] || { echo "push-weights {{disease}}: not on main"; exit 1; }
    files="pipelines/{{disease}}_geo_exclusions.csv pipelines/{{disease}}_nssp_geo_exclusions.csv"; \
        git diff --quiet HEAD -- $files || git commit -m "{{disease}} weights {{today}}" -- $files
    git pull --rebase --autostash origin main
    git push origin main

# Weights go before the PR so a failed PR step can't skip them.
# Commit the forecast, push the weights, and open the hub PR (one disease or all); `--dry-run` skips the weights push.
[group('production')]
submit disease="all" *flags:
    for d in {{ if disease == "all" { diseases } else { disease } }}; do \
        {{rejust}} commit "$d"; \
        [[ "{{flags}}" == *--dry-run* ]] || {{rejust}} push-weights "$d"; \
        (cd "$({{rejust}} _hub_dir "$d")" && gh pr create --title "CMU-TimeSeries {{today}}" --repo "$({{rejust}} _hub_repo "$d")" {{flags}}); \
    done

# Keeps the committed hosp rows and takes only the NSSP rows from the working file.
# One-off covid submission fix, e.g. `just update-only-nssp-submission 2025-09-13`.
[group('deprecated')]
update-only-nssp-submission date:
    #!/usr/bin/env bash
    set -euo pipefail
    cd ../covid19-forecast-hub
    git pull --rebase --autostash origin main
    f=model-output/CMU-TimeSeries/{{date}}-CMU-TimeSeries.csv
    { head -n 1 "$f"; git show "HEAD:$f" | grep "wk inc covid hosp"; grep "wk inc covid prop ed visits" "$f"; } > "$f.tmp"
    mv "$f.tmp" "$f"

# ---- Data sync ----

# NWSS wastewater export via the Python tool.
[group('deprecated')]
get-nwss:
    mkdir -p aux_data/nwss_covid_data aux_data/nwss_flu_data
    cd scripts/nwss_export_tool && ../../.venv/bin/python nwss_covid_export.py && ../../.venv/bin/python nwss_influenza_export.py

# Mirror S3 into local dirs (--delete). dir: aux_data, a store name, or all.
[group('setup-sync-clean')]
pull dir="all":
    for d in {{ if dir == "all" { "aux_data " + synced } else { dir } }}; do aws s3 sync "{{bucket}}/$d/" "$d/" --delete; done

# Mirror local stores into S3 (--delete). dir: a store name, or all.
[group('setup-sync-clean')]
push dir="all":
    for d in {{ if dir == "all" { synced } else { dir } }}; do aws s3 sync "$d/" "{{bucket}}/$d/" --delete; done

# ---- Reports site ----

# Rebuild the report index and deploy it.
[group('production')]
publish-netlify: update-site netlify

[group('production')]
sync-reports:
    aws s3 sync rendered_reports/ {{bucket}}/reports/
    aws s3 sync {{bucket}}/reports/ rendered_reports/

[group('production')]
update-site: sync-reports
    Rscript -e "{{load_all}}; update_site()"
    aws s3 sync rendered_reports/ {{bucket}}/reports/

[group('production')]
netlify:
    netlify deploy --dir=rendered_reports --prod

# Shiny dashboard over the scores.
[group('deprecated')]
dashboard:
    Rscript scripts/app/dashboard.R

# Render the 2025 one-off writeups into rendered_reports/.
[group('deprecated')]
summary-reports:
    Rscript -e "rmarkdown::render('reports/writeups/revision/revision_summary_report_2025.Rmd', output_file = here::here('rendered_reports', 'revision_summary_2025.html'))"
    Rscript -e "rmarkdown::render('reports/writeups/decreasing_forecasters.Rmd', output_file = here::here('rendered_reports', 'decreasing_forecasters.html'))"
    Rscript -e "rmarkdown::render('reports/writeups/season_summary_2025.Rmd', output_file = here::here('rendered_reports', 'season_summary_2025.html'))"
    Rscript -e "rmarkdown::render('reports/writeups/first_day_wrong.Rmd', output_file = here::here('rendered_reports', 'first_day_wrong.html'))"

# Render calibration experiment notebooks (all, or the named ones, e.g. e18_ref_op_bridge) and the index.
[group('exploration')]
calibration-experiments *notebooks:
    Rscript scripts/calibration/render_experiments.R {{notebooks}}

# Build the sleepcycle EDA data targets and render rendered_reports/sleepcycle_cough.html.
sleepcycle-eda:
    TAR_PROJECT=sleepcycle_eda Rscript -e 'targets::tar_make()'
