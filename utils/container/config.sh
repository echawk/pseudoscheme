# Shared by setup.sh, run.sh and teardown.sh (sourced from utils/container/).
MACHINE=quasischeme
IMAGE=localhost/quasischeme-dev
# /work: the repository's copy; /root/.cache: ASDF's compiled files and
# the Guile module cache; /gnu and /var/guix: Guix's store and database
VOLUMES="quasischeme-work quasischeme-cache quasischeme-gnu quasischeme-var-guix"
ROOT=$(cd ../.. && pwd)
LOGS=$ROOT/build/container/logs
CONNECTION=
if [ "$(uname)" = Darwin ]; then CONNECTION="--connection $MACHINE-root"; fi
