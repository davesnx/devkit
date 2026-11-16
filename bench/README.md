# Benchmarks

## httpev_server

`httpev_server.exe PORT` serves one handler with `Httpev.server_lwt`:
`GET /hello` (5 bytes of text) and `GET /json` (12 KiB of JSON).

```sh
make bench-httpev
```

`bench/httpev_server.sh` compares the Lwt server of this tree with the one of
`OLD_REF` (default `master`). It extracts `OLD_REF` with `git archive` into
`OLD_DIR`, builds the same `httpev_server.ml` there and here in the release
profile, then runs `wrk` against each server in turn, `RUNS` times, so both
servers share the same window of machine load. For each server it loads both
routes twice: with `Connection: keep-alive` (wrk's default) and with
`Connection: close`. The `master` server closes the connection after each
response in both cases. Each measured run follows a 2 s warm-up and prints one
line: req/s, p50 and p99 latency from `wrk --latency`, and wrk errors.

The server runs on CPU `SERVER_CPU` and wrk on `WRK_CPUS` (`taskset`). Choose
idle CPUs on a shared machine. Other settings: `DURATION` (s), `CONNECTIONS`,
`THREADS`, `PORT`.

```sh
RUNS=3 DURATION=10 SERVER_CPU=142 WRK_CPUS=164,189,462,473 sh bench/httpev_server.sh
```
