#!/bin/sh
# Cron job script for exporting org-mode stuff in background
/usr/local/bin/emacs --batch --user mkastner --eval "(org-icalendar-combine-agenda-files)"

