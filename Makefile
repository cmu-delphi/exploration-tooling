install:
	Rscript --no-init -e "install.packages(c('renv', 'pak', 'rspm'))"
	Rscript --no-init -e "renv::restore()"

run:
	Rscript run.R

run-nohup:
	nohup Rscript run.R &
