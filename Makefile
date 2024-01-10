install:
	Rscript -e "install.packages(c('renv', 'pak', 'rspm'))"
	Rscript -e "renv::restore()"
	Rscript -e 'renv::install(".")'

run:
	Rscript run.R

run-nohup:
	nohup Rscript run.R &

sync:
	Rscript sync.R

dashboard:
	Rscript dashboard.R
