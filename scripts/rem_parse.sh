#!/usr/bin/env bash
SRC=~/Dropbox/org/ios.org

/usr/local/bin/emacs --batch --user mkastner --eval "(cli-org-export-json)" ${SRC} &> ~/.emacs.d/scripts/rem.log
~/.emacs.d/scripts/rem parseorg ${SRC}.json >> ~/.emacs.d/scripts/rem.log 2>&1
