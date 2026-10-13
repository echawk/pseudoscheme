# Shared by setup.sh, run.sh and teardown.sh (sourced from utils/container/).
MACHINE=pseudoscheme
IMAGE=localhost/pseudoscheme-dev
# /work: the repository's copy; /root/.cache: ASDF's compiled files and
# the Guile module cache; /gnu and /var/guix: Guix's store and database
VOLUMES="pseudoscheme-work pseudoscheme-cache pseudoscheme-gnu pseudoscheme-var-guix"
ROOT=$(cd ../.. && pwd)
LOGS=$ROOT/build/container/logs
CONNECTION=
if [ "$(uname)" = Darwin ]; then CONNECTION="--connection $MACHINE-root"; fi
