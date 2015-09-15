#!/usr/bin/env python
# -*- coding: utf-8 -*-

import codecs
from random import randint

words = {}
cognats = []

with codecs.open('dicfrenelda-utf8.txt', 'r', 'utf-8') as file_r:
    lines = file_r.readlines()
    for l in lines:
        l = l.split(';')
        words[l[0]] = l[-3]

correct = 0
incorrect = 0

with codecs.open('cognat_candidates.txt', 'r', 'utf-8') as file_r:
    lines = file_r.readlines()
    r = randint(0, len(lines)-1)
    l = lines[r].split(' <===> ')
    if l[0] in words.keys():
        if l[1].strip() == words[l[0]].strip():
            correct = correct+1
        else:
            incorrect = incorrect+1


print correct, ' | ',
print incorrect, ' | ',
print correct*1./((correct+incorrect)*1.)*100, '%'

