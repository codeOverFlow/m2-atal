#!/usr/bin/env python
# -*- encoding: utf-8 -*-
# author: Adrien Bodineau

import re
import codecs
import unicodedata

# dictionnary containing all lemms for each files
# FOR FUTURE USE
files_source = {}
files_target = {}


def create_structure(file_name,d):
    count_words = {} 
    with codecs.open(file_name, 'r', 'utf-8') as file_r:
        name = ''
        res = set()
        for l in file_r.read().split(' '):
            if len(l) == 0 or l == '\n':
                continue
            if '__ENDFILE' in l:
                continue
            if '__FILE' in l:
                if res != '':
                    d[name] = res
                res = set()
                name = l.split('/')[0]
                name = name.split('=')[1]
                # use the filename as a key
                d[name] = []
                continue
            else:
                term_regex = re.compile('([^\s/()]+?)/([^\s/()]+?)/([^\s/()]+)')
                matches = []
                forbidden_tags = re.compile('''DTN|DTC|INJ|PUL|PFX|PREP|PRV|PRO|REL|
                        SUB|CC|DT|EX|IN|LS|MD|PDT|POS|PP|RP|TO|UH|WP''')
                total = 0
                if len(l) == 0 or l == '\n':
                    continue
                if not term_regex.match(l):
                    continue
                word = term_regex.match(l)
                wo,le,t = word.group(1), word.group(3), word.group(2).split(':')[0]
                if ':' in le:
                    le = le.split(':')[0]
                if le in count_words:
                    count_words[le] = count_words[le]+1
                else:
                    count_words[le] = 0   
                if not forbidden_tags.match(t):
                    res.add((wo, le, t))
    print len(d)
    for k,v in d.items():
        for w in [x for x in v]:
            if count_words[w[1]] < 5:
                v.remove(w)
        if len(v) == 0:
            d.pop(k)
    print len(d)


def is_transfuges(lem_source, lem_target):
    """
        find word which should be transfuges

        Params:
            lemm_set_source -> the set containing the lemms of the source file
            lemm_set_target -> the set containing the lemms of the target file
        Return:
            the set containing all the transfuges
    """
    return lem_source == lem_target


def prefix_feature(s, t):
    pos = 0
    while (pos < len(s) and pos < len(t)) and s[pos] == t[pos]:
        pos = pos + 1
    if len(s) >= len(t):
        return float(pos)/float(len(s))
    else:
        return float(pos)/float(len(t))


def kgrams(w, k):
    res = set()
    for i in range(0, len(w)-k):
        res.add(w[i:i+k])
    return res

def dice_feature(s, t, k):
    if float(len(kgrams(s, k) | kgrams(t, k))) == 0:
        return 0
    else:
        return (float(2*len(kgrams(s, k) & kgrams(t, k))))/\
                (float(len(kgrams(s, k) | kgrams(t, k))))


def xgrams(w):
    res = set()
    for i in range(0, len(w)-3):
        res.add(w[i]+w[i+2])
    return res


def xdice_feature(s, t):
    if float(len(xgrams(s) | xgrams(t))) == 0:
        return 0
    return (float(2*len(xgrams(s) & xgrams(t))))/\
            (float(len(xgrams(s) | xgrams(t))))


def is_cognat(ls, lt):
    return xdice_feature(ls, lt) > 0.7\
            and dice_feature(ls, lt, 3) > 0.75\
            and dice_feature(ls, lt, 2) > 0.75\
            and prefix_feature(ls, lt) > 0.6

def find_transfuges_and_cognat(d_source, d_target):
    transfuges = []
    candidates = []
    ss = [x for subl in d_source.values() for x in subl]
    set(ss)
    print ss
    for x in [k for k in ss]:
        if re.match('^[0-9](.[0-9]*)?$', x[0]):
            ss.remove(x)
    st = set([x for subl in d_target.values() for x in subl])
    for x in [k for k in st]:
        if re.match('^[0-9](.[0-9]*)?$', x[0]):
            ss.remove(x)
    print len(ss)
    print len(st)
    for ws,ls in set([(x[0], x[1]) for subl in d_source.values() for x in subl]):
        for wt,lt in set([(x[0], x[1]) for subl in d_target.values() for x in subl]):
            if len(ls) == 0 or len(lt) == 0:
                continue
            if abs(len(ls)-len(lt)) > 5:
                continue
            if is_transfuges(ws, wt):
                transfuges.append((ws, wt))
            elif is_cognat(ls, lt):
                candidates.append((ls,lt))
    return transfuges, candidates



def remove_digit_and_too_short(set_to_process):
    tmp = set()
    for x in set_to_process:
        if not filter_numbers.match(x):
            tmp.add(x)
    return tmp


print '###############################################################'
print '################## START PREPROCESSING      ###################'
print '################## START SOURCE             ###################'

create_structure('termer_source/corpus.lem', files_source)
#source_lemms = get_words('termer_source/corpus.lem', files_source, -1)


print '################## END   SOURCE             ###################'
print '################## START TARGET             ###################'


create_structure('termer_target/corpus.lem', files_target)
#target_lemms = get_words('termer_target/corpus.lem', files_target, -2)


print '################## END   TARGET             ###################'
print '################## END   PREPROCESSING      ###################'
print '###############################################################', '\n\n'


print '################## SEARCH TRANSFUGES        ###################', '\n'
transfuges, cognats = find_transfuges_and_cognat(files_source, files_target)
with codecs.open('transfuges.txt', 'w+', 'utf-8') as file_w:
    for ws,wt in transfuges:
        file_w.write(ws + ' <===> ' + wt + '\n')

with codecs.open('cognat_candidates.txt', 'w+', 'utf-8') as file_w:
    for ls,lt in cognats:
        file_w.write(ls + ' <===> ' + lt + '\n')
