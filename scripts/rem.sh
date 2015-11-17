#!/usr/bin/env bash

SRC=/tmp/rem.org
DEST=~/Dropbox/org/ios.org

echo "# -*- coding: utf-8; eval: (auto-revert-mode 1); -*-" > ${SRC}
~/.emacs.d/scripts/rem.py >> ${SRC}

# check if it changed
if ! cmp ${SRC} ${DEST} > /dev/null 2>&1
then
    cp ${SRC} ${DEST}
fi

