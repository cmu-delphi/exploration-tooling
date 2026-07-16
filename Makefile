.PHONY: all test test-forecasters run sync download upload dashboard \
	prod-rsv prod-rsv-backtest prune-rsv-prod \
	commit-rsv submit-rsv submit-rsv-dry \
	pull-rsv-prod push-rsv-prod get-rsv-prod-errors \
	eval-flu prune-flu-evaluation \
	pull-flu-evaluation push-flu-evaluation get-flu-evaluation-errors \
	oracle-capture oracle-compare

# Long-running recipes tee to cache/logs/. pipefail so a failing Rscript isn't
# masked by tee's exit status.
SHELL := /bin/bash
.SHELLFLAGS := -o pipefail -c

current_date:=$(shell date +%F)

install:
	Rscript -e "install.packages(c('renv', 'pak', 'rspm')); renv::restore()"

test:
	Rscript -e "testthat::test_dir('tests/testthat')"

run:
	Rscript scripts/run.R

cache/logs:
	mkdir -p cache/logs

prod-covid: | cache/logs
	export TAR_RUN_PROJECT=covid_hosp_prod; Rscript scripts/run.R 2>&1 | tee -a cache/logs/prod_covid

prod-flu: | cache/logs
	export TAR_RUN_PROJECT=flu_hosp_prod; Rscript scripts/run.R 2>&1 | tee -a cache/logs/prod_flu

prod-rsv: | cache/logs
	export TAR_RUN_PROJECT=rsv_hosp_prod; Rscript scripts/run.R 2>&1 | tee -a cache/logs/prod_rsv

prod: prod-covid prod-flu update-site netlify

prod-covid-backtest:
	export BACKTEST_MODE=TRUE; export TAR_RUN_PROJECT=covid_hosp_prod; Rscript scripts/run.R

prod-rsv-backtest:
	export BACKTEST_MODE=TRUE; export TAR_RUN_PROJECT=rsv_hosp_prod; Rscript scripts/run.R

prod-backtest: prod-covid-backtest prod-rsv-backtest

# Flu's historical replay is its own targets project (separate store), not a
# BACKTEST_MODE flag on prod. Set EVALUATION_N_DATES=<n> to replay only the
# last n forecast dates.
eval-flu: | cache/logs
	export TAR_RUN_PROJECT=flu_hosp_evaluation; Rscript scripts/run.R 2>&1 | tee -a cache/logs/eval_flu

explore-covid:
	export TAR_RUN_PROJECT=covid_hosp_explore; Rscript scripts/run.R

explore-flu:
	export TAR_RUN_PROJECT=flu_hosp_explore; Rscript scripts/run.R

explore: explore-covid explore-flu update-site netlify

# ---- Oracle: behavior-preserving golden capture + compare ----
# Pinned reference date so captures are reproducible week-to-week instead of
# tracking Sys.Date(). Requires FORECAST_REFERENCE_DATE support in the pipeline.
ORACLE_REFERENCE_DATE := 2026-06-24

# Capture a golden. project=<targets project> label=<subdir>. For the
# flu_hosp_evaluation project also pass n=<EVALUATION_N_DATES> to replay only
# the last n dates.
# e.g. make oracle-capture project=flu_hosp_prod label=refactored
oracle-capture: | cache/logs
	@if [ -z "$(project)" ] || [ -z "$(label)" ]; then \
		echo "Usage: make oracle-capture project=flu_hosp_prod label=refactored [n=3]"; exit 1; fi
	export TAR_RUN_PROJECT=$(project); export ORACLE_LABEL=$(label); \
	export FORECAST_REFERENCE_DATE=$(ORACLE_REFERENCE_DATE); export EVALUATION_N_DATES=$(n); \
	Rscript scripts/oracle/capture.R 2>&1 | tee -a cache/logs/oracle_capture

# Diff two captured labels of one project. project=<p> a=<label> b=<label>.
# NOTE: exits non-zero when anything differs -- that is the signal, not an error.
# e.g. make oracle-compare project=flu_hosp_prod a=baseline b=refactored
oracle-compare:
	@if [ -z "$(project)" ] || [ -z "$(a)" ] || [ -z "$(b)" ]; then \
		echo "Usage: make oracle-compare project=flu_hosp_prod a=baseline b=refactored"; exit 1; fi
	Rscript scripts/oracle/compare.R $(project):$(a) $(project):$(b)

prune: prune-covid-prod prune-flu-prod prune-flu-evaluation prune-rsv-prod prune-covid-explore prune-flu-explore

# scripts/prune.R selects the project via TAR_RUN_PROJECT (like run.R): an
# .Renviron that sets TAR_PROJECT overrides shell exports on every R start,
# so `export TAR_PROJECT=...` is not a reliable way to pick a project.
# NB: the flu script ignores BACKTEST_MODE (mode dispatches on the project
# name), so pruning flu_hosp_prod drops any historical replay targets left in
# the prod store from the pre-split era — they belong to flu_hosp_evaluation now.
prune-covid-prod:
	export TAR_RUN_PROJECT=covid_hosp_prod; export BACKTEST_MODE=TRUE; Rscript scripts/prune.R

prune-flu-prod:
	export TAR_RUN_PROJECT=flu_hosp_prod; Rscript scripts/prune.R

prune-flu-evaluation:
	export TAR_RUN_PROJECT=flu_hosp_evaluation; Rscript scripts/prune.R

prune-rsv-prod:
	export TAR_RUN_PROJECT=rsv_hosp_prod; export BACKTEST_MODE=TRUE; Rscript scripts/prune.R

prune-covid-explore:
	export TAR_RUN_PROJECT=covid_hosp_explore; Rscript scripts/prune.R

prune-flu-explore:
	export TAR_RUN_PROJECT=flu_hosp_explore; Rscript scripts/prune.R

commit-covid:
	./scripts/commit-script.sh '../covid19-forecast-hub'

commit-flu:
	./scripts/commit-script.sh '../FluSight-forecast-hub'

commit-rsv:
	./scripts/commit-script.sh '../rsv-forecast-hub'

submit-covid: commit-covid
	cd ../covid19-forecast-hub; \
	gh pr create --title "CMU-TimeSeries $(current_date)" --repo CDCgov/covid19-forecast-hub

submit-flu: commit-flu
	cd ../FluSight-forecast-hub; \
	gh pr create --title "CMU-TimeSeries $(current_date)" --repo cdcepi/FluSight-forecast-hub

submit-rsv: commit-rsv
	cd ../rsv-forecast-hub; \
	gh pr create --title "CMU-TimeSeries $(current_date)" --repo CDCgov/rsv-forecast-hub

submit-covid-dry: commit-covid
	cd ../covid19-forecast-hub; \
	gh pr create --title "CMU-TimeSeries $(current_date)" --repo CDCgov/covid19-forecast-hub --dry-run

submit-flu-dry: commit-flu
	cd ../FluSight-forecast-hub; \
	gh pr create --title "CMU-TimeSeries $(current_date)" --repo cdcepi/FluSight-forecast-hub --dry-run

submit-rsv-dry: commit-rsv
	cd ../rsv-forecast-hub; \
	gh pr create --title "CMU-TimeSeries $(current_date)" --repo CDCgov/rsv-forecast-hub --dry-run

submit: submit-covid submit-flu submit-rsv

get-nwss:
	mkdir -p aux_data/nwss_covid_data; \
	mkdir -p aux_data/nwss_flu_data; \
	. .venv/bin/activate; \
	cd scripts/nwss_export_tool/; \
	python nwss_covid_export.py; \
	python nwss_influenza_export.py

pull-aux-data:
	aws s3 sync s3://forecasting-team-data/2024/aux_data/ aux_data/ --delete

pull-covid-prod:
	aws s3 sync s3://forecasting-team-data/2024/covid_hosp_prod/ covid_hosp_prod/ --delete

pull-flu-prod:
	aws s3 sync s3://forecasting-team-data/2024/flu_hosp_prod/ flu_hosp_prod/ --delete

pull-flu-evaluation:
	aws s3 sync s3://forecasting-team-data/2024/flu_hosp_evaluation/ flu_hosp_evaluation/ --delete

pull-rsv-prod:
	aws s3 sync s3://forecasting-team-data/2024/rsv_hosp_prod/ rsv_hosp_prod/ --delete

pull-covid-explore:
	aws s3 sync s3://forecasting-team-data/2024/covid_hosp_explore/ covid_hosp_explore/ --delete

pull-flu-explore:
	aws s3 sync s3://forecasting-team-data/2024/flu_hosp_explore/ flu_hosp_explore/ --delete

pull: pull-aux-data pull-covid-prod pull-flu-prod pull-flu-evaluation pull-rsv-prod pull-covid-explore pull-flu-explore

download: pull

push-covid-prod:
	aws s3 sync covid_hosp_prod/ s3://forecasting-team-data/2024/covid_hosp_prod/ --delete

push-flu-prod:
	aws s3 sync flu_hosp_prod/ s3://forecasting-team-data/2024/flu_hosp_prod/ --delete

push-flu-evaluation:
	aws s3 sync flu_hosp_evaluation/ s3://forecasting-team-data/2024/flu_hosp_evaluation/ --delete

push-rsv-prod:
	aws s3 sync rsv_hosp_prod/ s3://forecasting-team-data/2024/rsv_hosp_prod/ --delete

push-covid-explore:
	aws s3 sync covid_hosp_explore/ s3://forecasting-team-data/2024/covid_hosp_explore/ --delete

push-flu-explore:
	aws s3 sync flu_hosp_explore/ s3://forecasting-team-data/2024/flu_hosp_explore/ --delete

push: push-covid-prod push-flu-prod push-flu-evaluation push-rsv-prod push-covid-explore push-flu-explore

upload: push

dashboard:
	Rscript scripts/dashboard.R

sync-reports:
	aws s3 sync reports/ s3://forecasting-team-data/2024/reports/; \
	aws s3 sync s3://forecasting-team-data/2024/reports/ reports/

update-site: sync-reports
	Rscript -e "suppressPackageStartupMessages(source(here::here('R', 'load_all.R'))); update_site()"

netlify:
	netlify deploy --dir=reports --prod

get-flu-prod-errors:
	Rscript -e "suppressPackageStartupMessages(source(here::here('R', 'load_all.R'))); get_targets_errors(project = 'flu_hosp_prod')"

get-flu-evaluation-errors:
	Rscript -e "suppressPackageStartupMessages(source(here::here('R', 'load_all.R'))); get_targets_errors(project = 'flu_hosp_evaluation')"

get-covid-prod-errors:
	Rscript -e "suppressPackageStartupMessages(source(here::here('R', 'load_all.R'))); get_targets_errors(project = 'covid_hosp_prod')"

get-rsv-prod-errors:
	Rscript -e "suppressPackageStartupMessages(source(here::here('R', 'load_all.R'))); get_targets_errors(project = 'rsv_hosp_prod')"

summary-reports:
	Rscript -e "rmarkdown::render('scripts/reports/revision_summary_report_2025.Rmd', output_file = here::here('reports', 'revision_summary_2025.html'))"; \
	Rscript -e "rmarkdown::render('scripts/reports/decreasing_forecasters.Rmd', output_file = here::here('reports', 'decreasing_forecasters.html'))"; \
	Rscript -e "rmarkdown::render('scripts/reports/season_summary_2025.Rmd', output_file = here::here('reports', 'season_summary_2025.html'))"; \
	Rscript -e "rmarkdown::render('scripts/reports/first_day_wrong.Rmd', output_file = here::here('reports', 'first_day_wrong.html'))";

season-summary-2025-talk:
	quarto render scripts/reports/season_2025_talk/season_summary_2025_presentation.qmd --to revealjs --output-dir "../../../reports"; \
	cp scripts/reports/season_2025_talk/tachyons-minimal.css reports/; \
	cp -r scripts/reports/season_2025_talk/gfx reports/

season-summary-2025-talk-preview:
	quarto preview scripts/reports/season_2025_talk/season_summary_2025_presentation.qmd

check-socrata-updates:
	echo "NHSN Regular Update:"
	@curl -s https://data.cdc.gov/api/views/ua7e-t2fy | python3 -c "import sys, json; data=json.load(sys.stdin); print(data['rowsUpdatedAt'])" | xargs -I {} date -d "@{}"
	echo "NHSN Prelim Update:"
	@curl -s https://data.cdc.gov/api/views/mpgq-jmmr | python3 -c "import sys, json; data=json.load(sys.stdin); print(data['rowsUpdatedAt'])" | xargs -I {} date -d "@{}"
	echo "NSSP Update:"
	@curl -s https://data.cdc.gov/api/views/rdmq-nq56 | python3 -c "import sys, json; data=json.load(sys.stdin); print(data['rowsUpdatedAt'])" | xargs -I {} date -d "@{}"

check-nssp-socrata-github-diff:
	Rscript -e "suppressPackageStartupMessages(source(here::here('R', 'load_all.R'))); check_nssp_socrata_github_diff()"

update-only-nssp-submission:
	@if [ -z "$$date" ]; then \
		echo "Usage: make update-only-nssp-submission date=2025-09-13"; \
		exit 1; \
	fi; \
	cd ../covid19-forecast-hub; \
	git pull --rebase --autostash origin main; \
	head -n 1 model-output/CMU-TimeSeries/$$date-CMU-TimeSeries.csv > temp1.csv; \
	git show HEAD:model-output/CMU-TimeSeries/$$date-CMU-TimeSeries.csv | grep "wk inc covid hosp" > temp2.csv; \
	cat model-output/CMU-TimeSeries/$$date-CMU-TimeSeries.csv | grep "wk inc covid prop ed visits" > temp3.csv; \
	cat temp1.csv temp2.csv temp3.csv > model-output/CMU-TimeSeries/$$date-CMU-TimeSeries.csv; \
	rm temp1.csv temp2.csv temp3.csv; \
	cd -
