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

get_nwss:
	mkdir -p aux_data/nwss_covid_data; \
	mkdir -p aux_data/nwss_flu_data; \
	cd scripts/nwss_export_tool/; \
	python nwss_covid_export.py; \
	python nwss_covid_export.py

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
	Rscript -e "source('R/utils.R'); update_site()"

netlify:
	netlify deploy --prod
