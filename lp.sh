#!/bin/bash

# add emacspath env or change this to your emacs path
emacspath="${emacspath:=/usr/local/bin/emacs}"

for f in $(find $(pwd)  -name "*_lp.org");
do
    cmd="$emacspath --batch --init-directory=\"$(cd "$(dirname "$0")" && pwd)/emacs-home/\" --eval \"(require 'org)\" --eval \"(require 'ob-tangle)\" --eval '(find-file \"$f\")' --eval '(org-babel-tangle)'"
    echo $cmd
    eval $cmd
done;
