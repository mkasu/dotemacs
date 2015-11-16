#!/bin/sh
# Cron job script for exporting org-mode stuff in background
emacs --batch --user mkastner --eval "(org-icalendar-combine-agenda-files)"

