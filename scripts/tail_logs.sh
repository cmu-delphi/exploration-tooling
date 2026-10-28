#!/usr/bin/env bash
# Follow a targets project's runner log and the crew worker logs of its latest
# run, one prefixed stream. Picks up worker logs created after it starts. See
# notes/run-logs.md. Usage: scripts/tail_logs.sh <project>
set -uo pipefail
project=${1:?usage: scripts/tail_logs.sh <targets project>}
cd "$(dirname "$0")/.."

# Runner log names follow the Makefile: <kind>_<disease>.
disease=${project%%_*}
case ${project##*_} in
  prod) runner=cache/logs/prod_$disease ;;
  evaluation) runner=cache/logs/eval_$disease ;;
  *) runner= ;;
esac

trap 'kill 0' EXIT
trap 'exit 0' INT TERM

# follow <file> <tag> <tail -n arg>: crew's sessionInfo header is dropped.
follow() {
  tail -n "$3" -F "$1" 2>/dev/null | sed -u -e '/^• /d' -e '/^R version /d' -e "s/^/[$2] /" &
}

if [ -n "$runner" ]; then
  follow "$runner" runner 20
else
  echo "no runner log for $project; following worker logs only" >&2
fi

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
    follow "$f" "worker ${name:0:6}" "$([ $first = 1 ] && echo 5 || echo +1)"
  done
  first=0
  sleep 2
done
