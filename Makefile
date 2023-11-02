install:
	Rscript -e "install.packages(c('renv', 'pak', 'rspm'))"
	Rscript -e "renv::restore()"

run:
	Rscript run.R