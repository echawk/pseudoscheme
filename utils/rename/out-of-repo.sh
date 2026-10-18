#!/bin/sh
# The steps outside the repository (docs/rename-quasischeme.md, section
# 5), printed for the user to run: nothing here touches the machine.
cat <<'STEPS'
# GitHub: rename the repository on github.com, then
git remote set-url echawk git@github.com:echawk/quasischeme.git
# the checkout (Claude Code keys its project memory on the path):
mv ~/schemes/pseudoscheme ~/schemes/quasischeme
cp -R ~/.claude/projects/-Users-ethan-schemes-pseudoscheme ~/.claude/projects/-Users-ethan-schemes-quasischeme
# caches (rebuilt on first use):
rm -rf ~/.cache/pseudoscheme    # and ASDF's fasls for the old path, under ~/.cache/common-lisp/
# the container (its machine, image and volumes are named for the project):
sh utils/container/teardown.sh   # with the OLD names, before applying the rename
sh utils/container/setup.sh      # after: new names
# Emacs: update load-path and (require 'quasischeme) in init.el
STEPS
