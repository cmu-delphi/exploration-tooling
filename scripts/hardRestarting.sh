#!/usr/bin/env sh
# a jank way of running the targets, restarting every hour. Useful if you have memory problems
nohup Rscript scripts/run.R &

PROCESS="R"
MAXTIME=`date -d '1 hour ago' +'%s'`

function killpids()
{
    PIDS=`pgrep -u "${USER}" -x "${PROCESS}"`

    # Loop over all matching PIDs
    for pid in ${PIDS}; do
        # Retrieve duration of the process
        TIME=`ps -o time:1= -p "${pid}" |
              egrep -o "[0-9]{0,2}:?[0-9]{0,2}:[0-9]{2}$"`

        # Convert TIME to timestamp
        TTIME=`date -d "${TIME}" +'%s'`

        # Check if the process should be killed
        if [ "${TTIME}" -gt "${MAXTIME}" ]; then
            kill ${1} "${pid}"
        fi
    done
}
# wait an hour and a minute
sleep 3660
# Leave a chance to kill processes properly (SIGTERM)
killpids "-15"
sleep 5

# Now kill remaining processes (SIGKILL)
killpids "-9"

nohup Rscript scripts/run.R &
# wait an hour and a minute
sleep 3660
# Leave a chance to kill processes properly (SIGTERM)
killpids "-15"
sleep 5

# Now kill remaining processes (SIGKILL)
killpids "-9"
