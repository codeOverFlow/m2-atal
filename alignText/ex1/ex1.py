#!/usr/bin/env python
# coding: utf-8
# author: Adrien Bodineau

import re
import codecs
import unicodedata

# dictionnary containing all lemms for each files
# FOR FUTURE USE
files_source = {}
files_target = {}


def create_structure(file_name,d):
    with codecs.open(file_name, 'r', 'utf-8') as file_r:
        for l in file_r.readlines():
            if '__FILE' in l:
                name = l.split('/')[0]
                name = name.split('=')[1]
                # use the filename as a key
                d[name] = []
                continue
            elif '__ENDFILE' in l:
                continue
            else
                d[name].append(l)


def get_words(file_to_read, dictionary, lemm_index):
    """
        Get all the lemms and tags from the tagged files

        Params:
            - file_to_read -> the name of the tagged file to read
            - dictionary   -> the dictionary in which put lemms and tags
            - lemm_index   -> the index of the lemm(usually -1 or -2)
        Return:
            a set containing all the lemms
    """

    # occurences of each words
    count_words = {}
    # list of lemms
    only_words = []
    with codecs.open(file_to_read, 'r', 'utf-8') as source:
        # to check that the tag is a real one
        filter_tag = re.compile('[A-Z]')
        forbidden_tags = re.compile('''DTN|DTC|INJ|PUL|PFX|PREP|PRV|PRO|REL|
                SUB|CC|DT|EX|IN|LS|MD|PDT|POS|PP|RP|TO|UH|WP''')

        # read all the file and split each elements
        line_source = source.read().split(' ')

        file_name = ''

        for l in line_source:

            #print file_name

            # extract filename
            if '__FILE' in l:
                file_name = l.split('/')[0]
                file_name = file_name.split('=')[1]
                # use the filename as a key
                dictionary[file_name] = []
                continue
            elif '__ENDFILE' in l:
                continue

            # check that the element is not empty
            if len(l) > 0:
                # remove any \n
                l = l.strip()
                #print l
                l = l.split('/')
                #print l

                # prevent error for non tagged things
                if not l[lemm_index].isdigit() and len(l[lemm_index]) < 2:
                    continue 

                #print l
                if '/' in l and '|' in l[l.index('/'):]:
                    l = l.split('|')[0]

                # get the tag
                tag = l[1]
                # check if it is a valid tag
                if not filter_tag.match(tag):
                    tag = l[2]
                    if not filter_tag.match(tag):
                        #print '\n\n'
                        continue
                if forbidden_tags.match(tag):
                    continue
                if ':' in tag:
                    # remove gender and number
                    tag = tag.split(':')[0]

                #print tag
                # get the lemm and remove accents
                word = l[lemm_index]
                #word = unicodedata.normalize('NFD', word)\
                #        .encode('ascii', 'ignore')
                if ':' in word: 
                    word = word.split(':')[0]

                # initialize occurences with 0
                # or increment it if the key exists
                if word in count_words:
                    count_words[word] = count_words[word]+1
                else:
                    count_words[word] = 0

                # if all ok add the (word, tag) in the list of the filename
                dictionary[file_name].append((word, tag))
                # and add the lemm in the list
                only_words.append(word)

    # remove duplicate items
    only_words = set(only_words)
    for k,v in count_words.items():
        # remove if not occurs at least 3 times
        if v < 3:
            only_words.remove(k)

    # return the list of lemms
    return only_words



def find_transfuges(lemm_set_source, lemm_set_target):
    """
        find word which should be transfuges

        Params:
            lemm_set_source -> the set containing the lemms of the source file
            lemm_set_target -> the set containing the lemms of the target file
        Return:
            the set containing all the transfuges
    """
    return lemm_set_source & lemm_set_target


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
    els:
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


def find_cognat(lemm_set_source, lemm_set_target):
    candidates = []
    i=1
    for ls in lemm_set_source:
        for lt in lemm_set_target:
            if len(ls) == 0 or len(lt) == 0:
                continue
            if abs(len(ls)-len(lt)) > 5:
                continue
            res = i*100/(len(lemm_set_source)*len(lemm_set_target))
            print '\r'+str(res)+'%',
            prefix = prefix_feature(ls, lt)
            dice  = dice_feature(ls, lt, 2)
            trigram = dice_feature(ls, lt, 3)
            xdice = xdice_feature(ls, lt)
            if xdice > 0.7:
                if trigram > 0.75:
                    if dice > 0.75:
                        if prefix > 0.6:
                            candidates.append((ls,lt))
            i = i+1
    return candidates



def remove_digit_and_too_short(set_to_process):
    tmp = set()
    for x in set_to_process:
        if not filter_numbers.match(x):
            tmp.add(x)
    return tmp
            

print '###############################################################'
print '################## START PREPROCESSING      ###################'
print '################## START SOURCE             ###################'


source_lemms = get_words('termer_source/corpus.lem', files_source, -1)


print '################## END   SOURCE             ###################'
print '################## START TARGET             ###################'


target_lemms = get_words('termer_target/corpus.lem', files_target, -2)


print '################## END   TARGET             ###################'
print '################## END   PREPROCESSING      ###################'
print '###############################################################', '\n\n'


print '################## SEARCH TRANSFUGES        ###################', '\n'
transfuge_set = find_transfuges(source_lemms, target_lemms)

tmp = set()
for w in source_lemms:
    tmp.add(unicodedata.normalize('NFD', w).encode('ascii', 'ignore'))

source_lemms = tmp

tmp = set()
for w in target_lemms:
    tmp.add(unicodedata.normalize('NFD', w).encode('ascii', 'ignore'))

target_lemms = tmp

with codecs.open('transfuges.txt', 'w+', 'utf-8') as file_w:
    for x in transfuge_set:
        file_w.write(x+'\n')

# filter again before looking for cognat
print len(source_lemms)
source_lemms = source_lemms.difference(transfuge_set)
filter_numbers = re.compile('\d+(.\d+)*')
source_lemms = remove_digit_and_too_short(source_lemms)
print len(source_lemms)

print len(target_lemms)
target_lemms = target_lemms.difference(transfuge_set)
target_lemms = remove_digit_and_too_short(target_lemms)
print len(target_lemms)

print '\n', '###############################################################'
print '################## SEARCH COGNAT CANDIDATES ###################'
cognat_candidates_list = find_cognat(source_lemms, target_lemms)
with codecs.open('cognat_candidates.txt', 'w+', 'utf-8') as file_w:
    for ls,lt in cognat_candidates_list:
        file_w.write(ls + ' <===> ' + lt + '\n')

print '\n###############################################################'
#w, ww = 'facteur', 'factor'
#print prefix_feature(w, ww)
#print kgrams(w, 2)
#print kgrams(ww, 2)
#print kgrams(w, 2) & kgrams(ww, 2)
#print kgrams(w, 2) | kgrams(ww, 2)
#print dice_feature(w, ww, 2)
#print kgrams(w, 3)
#print kgrams(ww, 3)
#print kgrams(w, 3) & kgrams(ww, 3)
#print kgrams(w, 3) | kgrams(ww, 3)
#print dice_feature(w, ww, 3)
#print xgrams(w)
#print xgrams(ww)
#print xgrams(w) & xgrams(ww)
#print xgrams(w) | xgrams(ww)
#print xdice_feature(w, ww)
