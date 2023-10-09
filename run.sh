#!/bin/bash

# Submit the pipeline as a background process with ./run.sh
# module load R # Uncomment if R is an environment module.
echo "Look at nohup.out and run.Rout for logs"
nohup nice -4 R CMD BATCH run.R &

export APP_PORT=`grep runApp run.R | grep -Eo '[0-9]{4}'`
echo "The shiny app should be running on http://127.0.0.1:${APP_PORT:=unkown port}"

# Change the nice level above as appropriate
# for your situation and system.

# Removing .RData is recommended.
# rm -f .RData
