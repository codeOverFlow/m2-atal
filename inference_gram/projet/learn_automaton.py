#!/usr/bin/env python
# -*- encoding: utf-8 -*-

import codecs;


def read_file(filename):
    states = []
    text_to_check = ''
    automatons = []
    with codecs.open(filename, "r", "utf-8") as file_r:
        for l in file_r.readlines():
            l = l.strip()
            l = l.split(',')
            text_to_check = l[1]
            states = ([l[1][i:i+2] for i in range(0, len(l[1])) if i%2 == 0],\
                    [l[2][i:i+2] for i in range(0, len(l[2])) if i%2 == 0])
            print "text:" + text_to_check
            print "states: " + str(states)
            if len(states[1]) < 2: 
                continue
            automaton = {}
            learn_automaton(states, automaton, text_to_check)
            automatons.append(automaton)
    return automatons

def learn_automaton(t, d, txt):
    iter = 1
    trans, st = t
    current_st = ''
    current_tr = ''
    for c in zip(trans, st):
        if iter == 1:
            d[u'start'] = c[1]
            current_st = c[1]
            current_tr = c[0]
        else:
            if current_st in d.keys():
                d[current_st].append((current_tr, c[1]))
            else:
                d[current_st] = [(current_tr, c[1])]
            current_st = c[1]
            current_tr = c[0]
            d[u'end'] = c[1]
        iter = iter+1
    print "automaton: " + str(d)
    print "texte: " + txt
    check_automoton(d, txt)

def check_automoton(d, txt):
    current_st = d[u'start']
    text = [txt[i:i+2] for i in range(0, len(txt)) if i%2 == 0]
    for s in text:
        if not current_st in d.keys():
            break
        print d[current_st]
        for tu in d[current_st]:
            t,st = tu
            if s == t:
                current_st = st
    if current_st == d[u'end']:
        print 'OK'
        print_automaton(d, txt)
    else:
        print 'NOT'

def print_automaton(d, name):
    with codecs.open(name+".dot", "w+", "utf-8") as file_w:
        file_w.write("digraph G {\n")
        for k,v in d.items():
            for tu in v:
                if k == u'start':
                    print k, v
                    file_w.write(v+' [shape="diamond"];\n')
                elif k == u'end':
                    print k, v
                    file_w.write(v+' [shape="doublecircle"];\n')
                    continue
                else:
                    print k, tu[0], tu[1]
                    file_w.write(k + '->' + str(tu[1]) + '[label="' + str(tu[0]) + '"];\n')
        file_w.write('}')
            

learned_automatons = read_file("112_initial.fsa.res")

