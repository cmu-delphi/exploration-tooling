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

prod-backtest-covid:
	export BACKTEST_MODE=TRUE; export TAR_RUN_PROJECT=covid_hosp_prod; Rscript scripts/run.R

prod-backtest-flu:
	export BACKTEST_MODE=TRUE; export TAR_RUN_PROJECT=flu_hosp_prod; Rscript scripts/run.R

prod-backtest: prod-backtest-covid prod-backtest-flu

explore-covid:
	export TAR_RUN_PROJECT=covid_hosp_explore; Rscript scripts/run.R

explore-flu:
	export TAR_RUN_PROJECT=flu_hosp_explore; Rscript scripts/run.R

explore: explore-covid explore-flu update-site netlify

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

sync:
	Rscript -e "source('R/sync_aws.R'); sync_aws()"

pull:
	Rscript -e "source('R/sync_aws.R'); sync_aws(direction = 'download')"

download:pull

push:
	Rscript -e "source('R/sync_aws.R'); sync_aws(direction = 'upload')"

upload: push

dashboard:
	Rscript scripts/dashboard.R

update-site:
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
	quarto render scripts/reports/season_2025_talk/season_summary_2025_presentation.qmd --to html --output-dir "../../../reports"; \
	cp scripts/reports/season_2025_talk/tachyons-minimal.css reports/; \
	cp -r scripts/reports/season_2025_talk/gfx reports/

season-summary-2025-talk-preview:
	quarto preview scripts/reports/season_2025_talk/season_summary_2025_presentation.qmd
