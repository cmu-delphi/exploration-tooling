#!/usr/bin/env bash
# Follow the runner log and crew worker logs of a targets project's latest run,
# one prefixed stream. Picks up logs created after it starts. See
# notes/run-logs.md. Usage: scripts/tail_logs.sh <project>
set -uo pipefail
project=${1:?usage: scripts/tail_logs.sh <targets project>}
cd "$(dirname "$0")/.."

trap 'kill 0' EXIT
trap 'exit 0' INT TERM

# follow <file> <tag> <tail -n arg>: crew's sessionInfo header is dropped.
follow() {
  tail -n "$3" -F "$1" 2>/dev/null | sed -u -e '/^• /d' -e '/^R version /d' -e "s/^/[$2] /" &
}

declare -A followed
first=1
while true; do
  run_dir=$(ls -d "local_logs/$project"/*/ 2>/dev/null | sort | tail -n 1)
  [ -n "$run_dir" ] || { first=0; sleep 2; continue; }
  for f in "$run_dir"*.log; do
    [ -e "$f" ] && [ -z "${followed[$f]:-}" ] || continue
    followed[$f]=1
    # Show the tail of logs that existed at start, all of logs that appear later.
    name=$(basename "$f" .log)
    if [ "$name" = runner ]; then
      follow "$f" runner "$([ $first = 1 ] && echo 20 || echo +1)"
    else
      follow "$f" "worker ${name:0:6}" "$([ $first = 1 ] && echo 5 || echo +1)"
    fi
  done
  first=0
  sleep 2
done
