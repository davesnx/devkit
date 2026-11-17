#!/bin/sh
set -eu

old_ref=${OLD_REF:-master}
old_dir=${OLD_DIR:-${TMPDIR:-/tmp}/devkit-httpev-old}
runs=${RUNS:-3}
duration=${DURATION:-10}
connections=${CONNECTIONS:-64}
threads=${THREADS:-4}
server_cpu=${SERVER_CPU:-2}
wrk_cpus=${WRK_CPUS:-4-7}
port=${PORT:-18080}

case $old_dir in
  "" | / | "$HOME" | "$PWD" | .) echo "refusing OLD_DIR=$old_dir" >&2; exit 2 ;;
esac
rm -rf "$old_dir"
mkdir -p "$old_dir/httpev_bench"
git archive "$old_ref" | tar -x -C "$old_dir"
cp bench/httpev_server.ml "$old_dir/httpev_bench/"
printf '(executable (name httpev_server) (libraries devkit unix))\n' > "$old_dir/httpev_bench/dune"
(cd "$old_dir" && dune build --root . --profile release httpev_bench/httpev_server.exe)
dune build --profile release bench/httpev_server.exe

old_exe=$old_dir/_build/default/httpev_bench/httpev_server.exe
new_exe=_build/default/bench/httpev_server.exe

printf 'run\tserver\tpath\tconnection\treq/s\tp50\tp99\terrors\n'
for run in $(seq "$runs"); do
  for server in old new; do
    if [ "$server" = old ]; then exe=$old_exe; else exe=$new_exe; fi
    taskset -c "$server_cpu" "$exe" "$port" >/dev/null 2>&1 &
    pid=$!
    sleep 1
    for path in /hello /json; do
      for connection in keep-alive close; do
        header="Connection: $connection"
        taskset -c "$wrk_cpus" wrk -t "$threads" -c "$connections" -d 2 -H "$header" "http://127.0.0.1:$port$path" >/dev/null
        out=$(taskset -c "$wrk_cpus" wrk -t "$threads" -c "$connections" -d "$duration" --latency -H "$header" "http://127.0.0.1:$port$path")
        rps=$(echo "$out" | awk '/^Requests\/sec/ {print $2}')
        p50=$(echo "$out" | awk '$1 == "50%" {print $2}')
        p99=$(echo "$out" | awk '$1 == "99%" {print $2}')
        errors=$(echo "$out" | awk '/Socket errors|Non-2xx/ {sub(/^ +/, ""); printf "%s; ", $0}')
        printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "$run" "$server" "$path" "$connection" "$rps" "$p50" "$p99" "${errors:--}"
      done
    done
    kill "$pid"
    wait "$pid" 2>/dev/null || true
  done
done
