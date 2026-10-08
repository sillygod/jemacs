#!/bin/bash
# Tangle every *_lp.org of this config into its .el.
#
# emacspath picks the Emacs; by default, the one on PATH.  It used to
# default to /usr/local/bin/emacs, which Homebrew on Apple Silicon does
# not install, so every tangle failed and the .el files silently stayed
# old.  Now a missing Emacs, or a failed tangle, stops the script.

set -euo pipefail

root="$(cd "$(dirname "$0")" && pwd)"
emacspath="${emacspath:-$(command -v emacs || true)}"

if [ -z "$emacspath" ] || ! command -v "$emacspath" >/dev/null; then
    echo "lp.sh: no emacs; put it on PATH or set emacspath=/path/to/emacs" >&2
    exit 1
fi

# emacs-home/ is straight's tree: no config of ours, and slow to walk.
find "$root" -path "$root/emacs-home" -prune -o -name "*_lp.org" -print0 |
    while IFS= read -r -d '' f; do
        echo "tangle ${f#"$root"/}"
        "$emacspath" --batch --init-directory="$root/emacs-home/" \
            --eval "(require 'org)" --eval "(require 'ob-tangle)" \
            --eval "(find-file \"$f\")" --eval "(org-babel-tangle)"
    done
