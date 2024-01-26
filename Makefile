install:
	Rscript -e "install.packages(c('renv', 'pak', 'rspm'))"
	Rscript -e "renv::restore()"
	Rscript -e 'renv::install(".")'

.PHONY: all run run-nohup sync download upload dashboard

run:
	Rscript run.R

run-nohup:
	nohup Rscript run.R &

sync:
	Rscript sync.R

download:
	Rscript sync.R download

upload:
	Rscript sync.R upload

dashboard:
	Rscript dashboard.R
