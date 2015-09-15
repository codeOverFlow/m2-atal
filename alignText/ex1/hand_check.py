#!/usr/bin/env python
# -*- encoding: utf-8
# author: Adrien Bodineeau

import codecs
import sys
from random import randint

correct = 0
incorrect = 0
maybe = 0

list_r = []

with codecs.open('cognat_candidates.txt', 'r', 'utf-8') as file_r:
    lines = file_r.readlines()
    res = 0
    while res != '9':
        r = randint(0, len(lines)-1)
        while r in list_r:
            r = randint(0, len(lines)-1)
        list_r.append(r)
        l = lines[r]
        if correct != 0 or incorrect != 0:
            print correct, '|', incorrect, '|', maybe, '|',\
                    (correct*1./(correct+incorrect)*1.)*100.
        print l.strip()+' ?'
        res = sys.stdin.readline().strip()
        if res == '1':
            correct = correct + 1
        elif res == '0':
            incorrect = incorrect+1
        elif res == '9':
            break
        else:
            maybe = maybe+1

print correct, '|', incorrect, '|', maybe, '|',\
        (correct*1./(correct+incorrect)*1.)*100*1. 

