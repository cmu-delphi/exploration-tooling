install:
	Rscript -e "install.packages(c('renv', 'pak'))"
	Rscript -e "renv::restore()"

run:
	Rscript run.R