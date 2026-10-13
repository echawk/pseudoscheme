#!/bin/sh
# Undo setup.sh: remove the volumes and the image, and on macOS stop and
# remove the podman machine.  KEEP_MACHINE=1 keeps the machine;
# the logs under build/container/ stay.
#
# Usage: sh utils/container/teardown.sh
cd "$(dirname "$0")"
. ./config.sh
if [ "$(uname)" = Darwin ] && ! podman machine inspect "$MACHINE" > /dev/null 2>&1; then
  echo "No podman machine $MACHINE; nothing to do."
  exit 0
fi
podman $CONNECTION ps -a --format '{{.Names}}' | grep '^pseudoscheme-' | xargs -r podman $CONNECTION rm -f
for v in $VOLUMES; do podman $CONNECTION volume rm -f "$v" > /dev/null 2>&1 || true; done
podman $CONNECTION rmi -f "$IMAGE" > /dev/null 2>&1 || true
if [ "$(uname)" = Darwin ] && [ -z "$KEEP_MACHINE" ]; then
  podman machine stop "$MACHINE" 2>/dev/null || true
  podman machine rm -f "$MACHINE"
fi
echo "Removed."
