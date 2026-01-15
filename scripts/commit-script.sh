#!/bin/bash

# This script automates the process of committing to the CMU-Delphi COVID-19 and
# FluSight forecast hubs. Because COVID-19 squash merges, we often have "dirty"
# working directories with uncommitted changes. This script selectively commits
# only the relevant files from a specific directory while discarding other
# changes.

# Take command line input for directory switch
[ $# -ne 1 ] && { echo "Usage: $0 <target-directory>"; exit 1; }

echo "Committing to hub in directory: $1"

cd $1 || { echo "Directory not found: $1"; exit 1; }

# 1. Update remotes
git fetch origin main;
git fetch delphi main;

# 2. Save current "dirty" state (including untracked files) to a temp branch
git checkout -b work-backup;
git add .;
git commit -m "Temp backup of today's work";

# 3. Switch back to main and hard-reset to the clean origin/main
git checkout main;
git reset --hard origin/main;

# 4. Rely on git checkout to selectively restore only the different files
# (If this doesn't work, just use the date pattern to select the right file.)
git checkout work-backup -- model-output/CMU-TimeSeries/;

# 5. Add, commit, and push
git add model-output/CMU-TimeSeries/*;
git commit -m "CMU-Delphi submission $(date +%Y-%m-%d)";
git push --force delphi main;

# 6. Cleanup the temp branch
git branch -D work-backup;
