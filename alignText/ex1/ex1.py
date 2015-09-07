#!/usr/bin/env python
# coding: utf8
# author: Adrien Bodineau

import re

filter_tag = re.compile("DET|DTN|PREP|TO|DT|\W")

with open('termer_source/corpus.lem', 'r') as source:
    with open('termer_target/corpus.lem', 'r') as target:
        line_source = source.read().split(' ')
        line_target = target.read().split(' ')

        for l, ll in zip(line_source, line_target):
            if l != '' and not filter_tag.match(l):
                print not filter_tag.match(l)
                lem = l.split('/')[-1]
                if ':' in lem:
                    lem = lem.split(':')[0]
                print l
                print lem, '\n\n'
            #if ll != '' and not filter_tag.match(ll):
            #    lem = ll.split('/')[-2]
            #    print ll
            #    print lem, '\n\n'

