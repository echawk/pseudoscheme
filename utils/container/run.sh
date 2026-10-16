#!/bin/sh
# Run a command in the Linux container, on a fresh copy of the repository.
#
# Usage: sh utils/container/run.sh NAME COMMAND ...
#
# The repository is mounted read-only at /src and copied to /work (a
# volume, so that the copy, bin/pseudoscheme and the compiled files stay
# between runs); COMMAND runs in /work under sh -c.  Its output goes to
# the terminal and to build/container/logs/NAME.log, and whatever
# COMMAND writes under /logs lands in build/container/logs/NAME/.
# Only the files Git knows of (tracked, or untracked and not ignored)
# are copied.  LIMIT=SECONDS kills the run after that long.  Runs share
# /work and the compiled files, so only one runs at a time.
set -e
[ $# -ge 2 ] || { echo "usage: $0 NAME COMMAND ..." >&2; exit 2; }
name=$1; shift
cd "$(dirname "$0")"
. ./config.sh
# One run at a time: runs share /work and the compiled files
running=$(podman $CONNECTION ps --format '{{.Names}}' | grep '^pseudoscheme-' || true)
if [ -n "$running" ]; then
  echo "Another run is in progress ($running); wait for it, or: podman $CONNECTION kill $running" >&2
  exit 1
fi
mkdir -p "$LOGS/$name"
(cd "$ROOT" && git ls-files --cached --others --exclude-standard) > "$LOGS/$name/files.list"
log=$LOGS/$name.log
{
  echo "# $(date -u +%Y-%m-%dT%H:%M:%SZ) $*"
  podman $CONNECTION run --rm --privileged --name "pseudoscheme-$name" \
    -v "$ROOT:/src:ro" -v "$LOGS/$name:/logs" \
    -v pseudoscheme-work:/work -v pseudoscheme-cache:/root/.cache \
    -v pseudoscheme-gnu:/gnu -v pseudoscheme-var-guix:/var/guix \
    "$IMAGE" sh -c 'rsync -a --delete --files-from=/logs/files.list /src/ /work/ \
                      && rm /logs/files.list && cd /work && exec sh -c "$0" ' "$*" &
  pid=$!
  sleeper=
  if [ -n "$LIMIT" ]; then
    ( sleep "$LIMIT"; podman $CONNECTION kill "pseudoscheme-$name" ) > /dev/null 2>&1 &
    sleeper=$!
  fi
  status=0
  wait $pid || status=$?
  # the time limit's watcher must not outlive the run, or it kills a
  # later run of the same name
  if [ -n "$sleeper" ]; then kill "$sleeper" 2>/dev/null || true; fi
  echo "# exit $status"
} 2>&1 | tee "$log"
