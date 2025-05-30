.PHONY: all test test-forecasters run sync download upload dashboard

current_date:=$(shell date +%F)

install:
	Rscript -e "install.packages(c('renv', 'pak', 'rspm')); renv::restore()"

test:
	Rscript -e "testthat::test_dir('tests/testthat')"

run:
	Rscript scripts/run.R

prod-covid:
	export TAR_RUN_PROJECT=covid_hosp_prod; Rscript scripts/run.R

prod-flu:
	export TAR_RUN_PROJECT=flu_hosp_prod; Rscript scripts/run.R

prod: prod-covid prod-flu update-site netlify

prod-covid-backtest:
	export BACKTEST_MODE=TRUE; export TAR_RUN_PROJECT=covid_hosp_prod; Rscript scripts/run.R

prod-flu-backtest:
	export BACKTEST_MODE=TRUE; export TAR_RUN_PROJECT=flu_hosp_prod; Rscript scripts/run.R

prod-backtest: prod-covid-backtest prod-flu-backtest

explore-covid:
	export TAR_RUN_PROJECT=covid_hosp_explore; Rscript scripts/run.R

explore-flu:
	export TAR_RUN_PROJECT=flu_hosp_explore; Rscript scripts/run.R

explore: explore-covid explore-flu update-site netlify

prune: prune-covid-prod prune-flu-prod prune-covid-explore prune-flu-explore

prune-covid-prod:
	export TAR_PROJECT=covid_hosp_prod; export BACKTEST_MODE=TRUE; Rscript -e "targets::tar_prune()"

prune-flu-prod:
	export TAR_PROJECT=flu_hosp_prod; export BACKTEST_MODE=TRUE; Rscript -e "targets::tar_prune()"

prune-covid-explore:
	export TAR_PROJECT=covid_hosp_explore; Rscript -e "targets::tar_prune()"

prune-flu-explore:
	export TAR_PROJECT=flu_hosp_explore; Rscript -e "targets::tar_prune()"

commit-covid:
	cd ../covid19-forecast-hub; \
	git pull --rebase --autostash origin main; \
	git add model-output/CMU-TimeSeries/*; \
	git add model-output/CMU-climate_baseline/*; \
	git commit -am "CMU-Delphi submission $(current_date)"; \
	git push --force delphi main

commit-flu:
	cd ../FluSight-forecast-hub; \
	git pull --rebase --autostash origin main; \
	git add model-output/CMU-TimeSeries/*; \
	git add model-output/CMU-climate_baseline/*; \
	git commit -am "CMU-Delphi submission $(current_date)"; \
	git push --force delphi main

submit-covid: commit-covid
	cd ../covid19-forecast-hub; \
	gh pr create --title "CMU-TimeSeries $(current_date)" --repo CDCgov/covid19-forecast-hub

submit-flu: commit-flu
	cd ../FluSight-forecast-hub; \
	gh pr create --title "CMU-TimeSeries $(current_date)" --repo cdcepi/FluSight-forecast-hub

submit-covid-dry: commit-covid
	cd ../covid19-forecast-hub; \
	gh pr create --title "CMU-TimeSeries $(current_date)" --repo CDCgov/covid19-forecast-hub --dry-run

submit-flu-dry: commit-flu
	cd ../FluSight-forecast-hub; \
	gh pr create --title "CMU-TimeSeries $(current_date)" --repo cdcepi/FluSight-forecast-hub --dry-run

submit: submit-covid submit-flu

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

pull-covid-explore:
	aws s3 sync s3://forecasting-team-data/2024/covid_hosp_explore/ covid_hosp_explore/ --delete

pull-flu-explore:
	aws s3 sync s3://forecasting-team-data/2024/flu_hosp_explore/ flu_hosp_explore/ --delete

pull: pull-aux-data pull-covid-prod pull-flu-prod pull-covid-explore pull-flu-explore

download: pull

push-covid-prod:
	aws s3 sync covid_hosp_prod/ s3://forecasting-team-data/2024/covid_hosp_prod/ --delete

push-flu-prod:
	aws s3 sync flu_hosp_prod/ s3://forecasting-team-data/2024/flu_hosp_prod/ --delete

push-covid-explore:
	aws s3 sync covid_hosp_explore/ s3://forecasting-team-data/2024/covid_hosp_explore/ --delete

push-flu-explore:
	aws s3 sync flu_hosp_explore/ s3://forecasting-team-data/2024/flu_hosp_explore/ --delete

push: push-covid-prod push-flu-prod push-covid-explore push-flu-explore

upload: push

dashboard:
	Rscript scripts/dashboard.R

sync-reports:
	aws s3 sync reports/ s3://forecasting-team-data/2024/reports/; \
	aws s3 sync s3://forecasting-team-data/2024/reports/ reports/

update-site: sync-reports
	Rscript -e "suppressPackageStartupMessages(source(here::here('R', 'load_all.R'))); update_site()" > cache/update_site_log.txt

netlify:
	netlify deploy --dir=reports --prod

get-flu-prod-errors:
	Rscript -e "suppressPackageStartupMessages(source(here::here('R', 'load_all.R'))); get_targets_errors(project = 'flu_hosp_prod')"

get-covid-prod-errors:
	Rscript -e "suppressPackageStartupMessages(source(here::here('R', 'load_all.R'))); get_targets_errors(project = 'covid_hosp_prod')"

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
