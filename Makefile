current_date:=$(shell date +%F)

install:
	Rscript -e "install.packages(c('renv', 'pak', 'rspm'))"
	Rscript -e "renv::restore()"

.PHONY: all test test-forecasters run run-nohup sync download upload dashboard

test:
	Rscript -e "testthat::test_dir('tests/testthat')"

run:
	Rscript scripts/run.R

prod-covid:
	export TAR_RUN_PROJECT=covid_hosp_prod; \
	Rscript scripts/run_prod.R

prod-flu:
	export TAR_RUN_PROJECT=flu_hosp_prod; \
	Rscript scripts/run_prod.R

prod: prod-covid prod-flu update_site netlify

submit-covid:
	cd ../covid19-forecast-hub; \
	git pull origin main; \
	git add model-output/CMU-TimeSeries/*; \
	git add model-output/CMU-climate_baseline/*; \
	git commit -am "CMU-Delphi submission $(current_date)"; \
	git push delphi main; \
	gh pr create --title "CMU-TimeSeries $(current_date)" --repo cdcgov/covid19-forecast-hub

submit-flu:
	cd ../FluSight-forecast-hub; \
	git pull origin main; \
	git add model-output/CMU-TimeSeries/*; \
	git add model-output/CMU-climate_baseline/*; \
	git commit -am "CMU-Delphi submission $(current_date)"; \
	git push delphi main; \
	gh pr create --title "CMU-TimeSeries $(current_date)" --repo cdcepi/FluSight-forecast-hub

submit-covid-dry:
	cd ../covid19-forecast-hub; \
	git pull origin main; \
	git add model-output/CMU-TimeSeries/*; \
	git add model-output/CMU-climate_baseline/*; \
	git commit -am "CMU-Delphi submission $(current_date)"; \
	git push delphi main; \
	gh pr create --title "CMU-TimeSeries $(current_date)" --repo cdcgov/covid19-forecast-hub --dry-run

submit-flu-dry:
	cd ../FluSight-forecast-hub; \
	git pull origin main; \
	git add model-output/CMU-TimeSeries/*; \
	git add model-output/CMU-climate_baseline/*; \
	git commit -am "CMU-Delphi submission $(current_date)"; \
	git push delphi main; \
	gh pr create --title "CMU-TimeSeries $(current_date)" --repo cdcepi/FluSight-forecast-hub --dry-run

submit: submit-covid submit-flu

get_nwss:
	mkdir -p aux_data/nwss_covid_data; \
	mkdir -p aux_data/nwss_flu_data; \
	. .venv/bin/activate; \
	cd scripts/nwss_export_tool/; \
	python nwss_covid_export.py; \
	python nwss_influenza_export.py

run-nohup:
	nohup Rscript scripts/run.R &

run-nohup-restarting:
	scripts/hardRestarting.sh &

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

update_site:
	Rscript -e "suppressPackageStartupMessages(source(here::here('R', 'load_all.R'))); update_site()" > cache/update_site_log.txt

netlify:
	netlify deploy --dir=reports --prod
