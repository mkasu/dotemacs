#!/usr/bin/env python

import subprocess

def rem(args):
    '''Execute the 'rem' command with whatever arguments passed and capture 
output'''
    cmd = "~/.emacs.d/scripts/rem " + args + " |perl -p -e 's/[[:^ascii:]]//g'"
    value = subprocess.check_output(cmd, shell=True)
    return value

def get_lists():
    '''Return a list of the list names'''
    r = rem("ls")
    r = r.split("\n")
    r = r[1:-1]  # drop the "Reminders" title and final newline
    r = [rr.strip() for rr in r]
    l = [rr for rr in r if not rr[0].isdigit()]
    return l

def get_num_items(lists):
    '''Return an integer vector with the number of items in each list'''
    n = []
    for l in lists:
        i = rem("ls " + l)
        i = i.split("\n")
        i = i[2:-1]  # drop the "Reminders" title and final newline
        n.append(len(i))
    return n
        
def reminder_item_to_task(list, item, depth=2, TODO=False):
    task = rem("cat " + list + " " + str(item))
    task = task.split("\n")[:-1]

    TODO = "TODO " if TODO else ""

    title = task[0].split(": ")[1:]
    title = ''.join(title)
    title = '*'*depth + " " + TODO + title

    depth += 1

    meta = task[2:]
    meta = [m.replace('\t','') for m in meta]
    #meta = [' '*depth + m for m in meta]
    meta = '\n'.join(meta)
    
    # notes = task[4:]
    # if len(notes) == 0:
    #     notes = ''
    # else:
    #     notes[0] = notes[0].split(": ")[1]
    #     notes = [' '*depth + n for n in notes]
    #     notes = '\n'.join(notes)

    return title + "\n" + meta

if __name__ == "__main__":
    lists = get_lists()
    num = get_num_items(lists)
    for l,n in zip(lists,num):
        print "* " + l
        for i in range(n):
            t = reminder_item_to_task(l, i+1, TODO=True)
            print t
