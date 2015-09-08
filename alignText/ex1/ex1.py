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

KGRAMM = 5


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
        forbidden_tags = re.compile('DTN|DTC|INJ|PUL|PFX|PREP|PRV|PRO|REL|SUB|CC|DT|EX|IN|LS|MD|PDT|POS|PP|RP|TO|UH|WP')

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
                    #print '\n\n'
                    continue
                elif forbidden_tags.match(tag):
                    continue
                elif ':' in tag:
                    # remove gender and number
                    tag = tag.split(':')[0]

                #print tag
                # get the lemm and remove accents
                word = l[lemm_index]
                word = unicodedata.normalize('NFD', word)\
                        .encode('ascii', 'ignore')
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


def find_cognat(lemm_set_source, lemm_set_target, k_gramm):
    candidates = []
    for ls in lemm_set_source:
        for lt in lemm_set_target:
            if ls[:k_gramm] == lt[:k_gramm]:
                if abs(len(ls[k_gramm:]) - len(lt[k_gramm:])) < 5:
                    middle = len(ls)/2 +1
                    if ls[middle-(KGRAMM/2):middle+(KGRAMM/2)] in lt:
                        candidates.append((ls,lt))
    return candidates


def remove_digit_and_too_short(set_to_process):
    tmp = set()
    for x in set_to_process:
        if len(x) > KGRAMM and not filter_numbers.match(x):
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
cognat_candidates_list = find_cognat(source_lemms, target_lemms, KGRAMM)
with codecs.open('cognat_candidates.txt', 'w+', 'utf-8') as file_w:
    for ls,lt in cognat_candidates_list:
        file_w.write(ls + ' <===> ' + lt + '\n')

print '###############################################################'
