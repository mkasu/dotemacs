#!/usr/bin/env bash
USER=mkastner
CONFIG=/Users/$USER/.toggl.ini
FOLDER=/Users/$USER/Dropbox/org/*.org
LOG=/Users/$USER/.emacs.d/scripts/toggl.log
ORG_TOGGL_PY=/Users/$USER/.emacs.d/scripts/org-toggl-py

date > $LOG

for f in $FOLDER
do
    echo "Processing file: $f..." >> $LOG 2>&1
    (cd $ORG_TOGGL_PY && PATH=/usr/local/bin:/usr/sbin:/usr/bin ./org-toggl.sh $CONFIG $f ) >> $LOG 2>&1
    rm $f.json >> $LOG 2>&1
done
