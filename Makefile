install:
	Rscript -e "install.packages(c('renv', 'pak', 'rspm'))"
	Rscript -e "renv::restore()"
	Rscript -e 'renv::install(".")'

.PHONY: all test test-forecasters run run-nohup sync download upload dashboard

test:
	Rscript -e "testthat::test_dir('tests/testthat')"

run:
	Rscript scripts/run.R

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
