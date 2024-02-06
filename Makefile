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
	Rscript scripts/sync.R

download:
	Rscript scripts/sync.R download

upload:
	Rscript scripts/sync.R upload

dashboard:
	Rscript scripts/dashboard.R

test-forecasters:
	Rscript scripts/test-forecasters-data.R
