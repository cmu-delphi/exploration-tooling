install:
	Rscript -e "install.packages(c('renv', 'pak', 'rspm'))"
	Rscript -e "renv::restore()"

run:
	Rscript run.R

run-nohup:
	nohup Rscript run.R &

sync:
	Rscript sync.R
