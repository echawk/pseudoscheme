#!/usr/bin/env bash
# Regenerate Pseudoscheme's .pso files (and spack.lisp) from their
# .scm sources, using some Scheme other than Pseudoscheme.  See
# boot/README.md.
#
# Usage: boot/bootstrap.sh [--all] [--no-install] [--check]
#
#   SCHEME=<name>  use that host (one of: $HOSTS below); by default,
#                  the first one installed.
#   --all          run every installed host, and check that they all
#                  produce the same files.
#   --no-install   leave the results in boot/build/<host>/ instead of
#                  copying them into src/.
#   --check        also check the results against what Pseudoscheme's
#                  own translator, loaded from src/, produces (needs
#                  $SBCL, default sbcl; see boot/check.lisp).
#
# Each host writes boot/build/<host>/, logging to boot/build/<host>.log.

set -eu

cd "$(dirname "$0")/.."

HOSTS="chibi guile gauche chicken chez racket scheme48 s7"

# The command for a host, and the executable it needs.
host_command() {
    case "$1" in
	chibi)    echo "chibi-scheme boot/hosts/chibi.scm" ;;
	guile)    echo "guile --no-auto-compile -s boot/hosts/guile.scm" ;;
	gauche)   echo "gosh boot/hosts/gauche.scm" ;;
	chicken)  echo "csi -s boot/hosts/chicken.scm" ;;
	chez)     echo "chez --script boot/hosts/chez.scm" ;;
	racket)   echo "plt-r5rs boot/hosts/racket.scm" ;;
	scheme48) echo "scheme48 < boot/hosts/scheme48.scm" ;;
	s7)       echo "s7 boot/hosts/s7.scm" ;;
	*)        return 1 ;;
    esac
}

host_installed() {
    command -v "$(host_command "$1" | cut -d' ' -f1)" >/dev/null 2>&1
}

# The files bootstrap.scm writes: closed, read and write, the
# translator, and spack.lisp.
expected_files() {
    echo closed.pso read.pso write.pso
    sed 's/;.*//' src/translator.files | grep -o '"[^"]*"' | tr -d '"' | sed 's/$/.pso/'
    echo spack.lisp
}

run_host() {
    host=$1
    out=boot/build/$host
    log=boot/build/$host.log
    printf 'Bootstrapping with %s ... ' "$host"
    rm -rf boot/build/out "$out"
    mkdir -p boot/build/out
    if ! sh -c "$(host_command "$host")" >"$log" 2>&1 || ! grep -q '^Done\.$' "$log"; then
	echo "failed; see $log"
	return 1
    fi
    for f in $(expected_files); do
	if [ ! -s "boot/build/out/$f" ]; then
	    echo "failed: no $f; see $log"
	    return 1
	fi
    done
    mv boot/build/out "$out"
    echo "ok"
}

# Whether two hosts wrote the same files, apart from header comments
# (which name the host).
same_output() {
    for f in $(expected_files); do
	if ! diff -q <(grep -v '^;' "boot/build/$1/$f") \
		     <(grep -v '^;' "boot/build/$2/$f") >/dev/null; then
	    echo "  $1 and $2 differ on $f"
	    return 1
	fi
    done
}

all=no
install=yes
check=no
for arg in "$@"; do
    case "$arg" in
	--all) all=yes ;;
	--no-install) install=no ;;
	--check) check=yes ;;
	*) echo "usage: $0 [--all] [--no-install] [--check]" >&2; exit 2 ;;
    esac
done

if [ -n "${SCHEME:-}" ]; then
    host_command "$SCHEME" >/dev/null || { echo "unknown SCHEME=$SCHEME (try one of: $HOSTS)" >&2; exit 2; }
    host_installed "$SCHEME" || { echo "SCHEME=$SCHEME: $(host_command "$SCHEME" | cut -d' ' -f1) not found" >&2; exit 1; }
    hosts=$SCHEME
else
    hosts=
    for h in $HOSTS; do
	if host_installed "$h"; then
	    hosts="$hosts $h"
	    [ "$all" = yes ] || break
	fi
    done
    [ -n "$hosts" ] || { echo "no Scheme found (looked for: $HOSTS); set SCHEME" >&2; exit 1; }
fi

mkdir -p boot/build
succeeded=
for h in $hosts; do
    if run_host "$h"; then
	succeeded="$succeeded $h"
    fi
done
[ -n "$succeeded" ] || { echo "no host succeeded" >&2; exit 1; }

first=$(echo $succeeded | cut -d' ' -f1)
status=0
for h in $succeeded; do
    [ "$h" = "$first" ] && continue
    if same_output "$first" "$h"; then
	echo "$h's output is identical to $first's"
    else
	status=1
    fi
done
[ "$status" = 0 ] || { echo "hosts disagree; not installing" >&2; exit 1; }
for h in $hosts; do
    case " $succeeded " in *" $h "*) ;; *) echo "$h failed; not installing" >&2; exit 1 ;; esac
done

if [ "$check" = yes ]; then
    "${SBCL:-sbcl}" --script boot/check.lisp "boot/build/$first" || exit 1
fi

if [ "$install" = yes ]; then
    for f in $(expected_files); do
	cp "boot/build/$first/$f" "src/$f"
    done
    echo "Installed $first's output in src/."
fi
