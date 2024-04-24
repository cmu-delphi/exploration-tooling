install:
	Rscript -e "install.packages(c('renv', 'pak', 'rspm'))"
	Rscript -e "renv::restore()"
	Rscript -e 'renv::install(".")'

.PHONY: all run run-nohup sync download upload dashboard

run:
	Rscript scripts/run.R

run-nohup:
	nohup Rscript scripts/run.R &

sync:
	Rscript -e "source('R/utils.R'); manage_s3_forecast_cache()"

pull:
	Rscript -e "source('R/utils.R'); manage_s3_forecast_cache(direction = 'download')"

download:pull

push:
	Rscript -e "source('R/utils.R'); manage_s3_forecast_cache(direction = 'upload')"

upload: push

dashboard:
	Rscript scripts/dashboard.R

test:
	Rscript -e "testthat::test_dir('tests/testthat')"

test-forecasters:
	Rscript scripts/test-forecasters-data.R
