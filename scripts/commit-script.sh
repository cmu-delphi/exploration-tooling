#!/bin/bash

# This script automates the process of committing to the CMU-Delphi COVID-19 and
# FluSight forecast hubs. Because COVID-19 squash merges, we often have "dirty"
# working directories with uncommitted changes. This script selectively commits
# only the relevant files from a specific directory while discarding other
# changes.

# Take command line input for directory switch
[ $# -ne 1 ] && {
	echo "Usage: $0 <target-directory>"
	exit 1
}

echo "Committing to hub in directory: $1"

cd $1 || {
	echo "Directory not found: $1"
	exit 1
}

# 1. Update remotes
git fetch origin main
git fetch delphi main

# 2. Save current "dirty" state (including untracked files) to a temp branch
git checkout -B work-backup
git add .
git commit -m "Temp backup of today's work"

# 3. Switch back to main and hard-reset to the clean origin/main
git checkout main
git reset --hard origin/main

# 4. Restore our model directories from the backup. Not every hub has every
# model (rsv has no CMU-TimeSeries_Cal), and git checkout restores nothing if
# one pathspec is missing, so restore each directory that exists separately.
for model_dir in model-output/CMU-TimeSeries model-output/CMU-TimeSeries_Cal; do
	if git cat-file -e "work-backup:$model_dir" 2>/dev/null; then
		git checkout work-backup -- "$model_dir"
		git add "$model_dir"
	fi
done

# 5. Commit and push. Stop, and keep work-backup, if there is nothing to submit.
if git diff --cached --quiet; then
	echo "No submission files differ from origin/main; not pushing. Today's work is on branch work-backup."
	exit 1
fi
git commit -m "CMU-Delphi submission $(date +%Y-%m-%d)"
git push --force delphi main

# 6. Cleanup the temp branch
git branch -D work-backup
