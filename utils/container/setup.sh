#!/bin/sh
# Set up the Linux container (utils/container/README.md): on macOS a
# podman machine of its own, then the image and the volumes.
#
# Usage: sh utils/container/setup.sh
#   MACHINE_CPUS (6), MACHINE_MEMORY (12288 MiB), MACHINE_DISK (60 GiB)
#   size the machine when it is first made.
set -e
cd "$(dirname "$0")"
. ./config.sh

if [ "$(uname)" = Darwin ]; then
  if ! podman machine inspect "$MACHINE" > /dev/null 2>&1; then
    echo "Making the podman machine $MACHINE ..."
    podman machine init "$MACHINE" --rootful \
      --cpus "${MACHINE_CPUS:-6}" --memory "${MACHINE_MEMORY:-12288}" --disk-size "${MACHINE_DISK:-60}"
  fi
  state=$(podman machine inspect "$MACHINE" --format '{{.State}}')
  if [ "$state" != running ]; then
    other=$(podman machine list --format '{{.Name}} {{.Running}}' | awk '$2 == "true" { print $1 }' | tr -d '*')
    if [ -n "$other" ]; then
      echo "The podman machine $other is running; stop it first (podman machine stop $other)." >&2
      exit 1
    fi
    podman machine start "$MACHINE"
  fi
fi

echo "Building the image $IMAGE ..."
podman $CONNECTION build -t "$IMAGE" -f Containerfile .
for v in $VOLUMES; do
  podman $CONNECTION volume exists "$v" || podman $CONNECTION volume create "$v" > /dev/null
done
mkdir -p "$LOGS"
echo "Ready: sh utils/container/run.sh NAME COMMAND ..."
