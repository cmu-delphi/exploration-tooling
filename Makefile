install:
	Rscript --no-init-file -e "install.packages(c('renv', 'pak', 'rspm'))"
	Rscript --no-init-file -e "renv::restore()"

run:
	Rscript run.R

run-nohup:
	nohup Rscript run.R &
