#!/bin/bash

# This script automates the process of committing to the CMU-Delphi COVID-19 and
# FluSight forecast hubs. Because COVID-19 squash merges, we often have "dirty"
# working directories with uncommitted changes. This script selectively commits
# only the relevant files from a specific directory while discarding other
# changes.

[ $# -ne 2 ] && {
	echo "Usage: $0 <target-directory> <reference-date>"
	exit 1
}
ref_date=$2

echo "Committing round $ref_date to hub in directory: $1"

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

# 4. Restore this round's files from the backup. Not every hub has every model
# (rsv has no CMU-TimeSeries_Cal), so restore each file that exists separately.
# The hub rejects changes to older rounds, so those stay out of the commit.
for model in CMU-TimeSeries CMU-TimeSeries_Cal; do
	file="model-output/$model/$ref_date-$model.csv"
	if git cat-file -e "work-backup:$file" 2>/dev/null; then
		git checkout work-backup -- "$file"
		git add "$file"
	fi
done
left_out=$(git diff --name-only --diff-filter=AM HEAD work-backup -- model-output/CMU-TimeSeries model-output/CMU-TimeSeries_Cal | grep -v "/$ref_date-" || true)
if [ -n "$left_out" ]; then
	echo "Left out of the commit (other rounds, rejected by the hub):"
	echo "$left_out"
fi

# 5. Commit and push. Stop, and keep work-backup, if there is nothing to submit.
if git diff --cached --quiet; then
	echo "No submission files differ from origin/main; not pushing. Today's work is on branch work-backup."
	exit 1
fi
git commit -m "CMU-Delphi submission $(date +%Y-%m-%d)"
git push --force delphi main

# 6. Cleanup the temp branch
git branch -D work-backup
