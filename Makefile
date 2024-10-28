install:
	Rscript -e "install.packages(c('renv', 'pak', 'rspm'))"
	Rscript -e "renv::restore()"
	Rscript -e 'renv::install(".")'

.PHONY: all test test-forecasters run run-nohup sync download upload dashboard

test:
	Rscript -e "testthat::test_dir('tests/testthat')"

run:
	Rscript scripts/run.R

get_nwss:
	mkdir -p aux_data/nwss_covid_data; \
	mkdir -p aux_data/nwss_flu_data; \
	cd scripts/nwss_export_tool/; \
	python nwss_covid_export.py; \
	python nwss_covid_export.py

run-nohup:
	nohup Rscript scripts/run.R &

sync:
	Rscript -e "source('R/utils.R'); sync_aws()"

pull:
	Rscript -e "source('R/utils.R'); sync_aws(direction = 'download')"

download:pull

push:
	Rscript -e "source('R/utils.R'); sync_aws(direction = 'upload')"

upload: push

dashboard:
	Rscript scripts/dashboard.R
