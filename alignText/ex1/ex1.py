#!/usr/bin/env python
# coding: utf8
# author: Adrien Bodineau

import re
import codecs
import unicodedata

files_source = {}
files_target = {}

def get_words(file_to_read, dictionary):
    with codecs.open(file_to_read, 'r', 'utf-8') as source:
        filter_tag = re.compile("[A-Z]")

        line_source = source.read().split(' ')
        
        file_name = ''
        for l in line_source:

            #print file_name

            # extract filename
            if '__FILE' in l:
                file_name = l.split('/')[0]
                file_name = file_name.split('=')[1]
                dictionary[file_name] = []
                continue
            elif '__ENDFILE' in l:
                continue

            if len(l) > 1:
                l = l.strip()
                #print l
                l = l.split('/')
                #print l
        
                # get the word and remove accents
                word = l[0]
                word = unicodedata.normalize('NFD', word).encode('ascii', 'ignore')
        
                # get the tag
                tag = l[1]
                if not filter_tag.match(tag):
                    #print '\n\n'
                    continue
                elif ':' in tag:
                    tag = tag.split(':')[0]

                dictionary[file_name].append((word, tag))
                #print files_source
                #print word, tag, '\n\n'


def find_transfuges(dict_source, dict_target):
    word_list_source = []
    word_list_target = []

    for key in dict_source.keys():
        word_list_source = [w for w,l in dict_source[str(key)]]
    for key in dict_target.keys():
        word_list_target = [w for w,l in dict_target[str(key)]]
    print set(word_list_source).intersection(word_list_target)

print '###############################################################'
print '################## START PREPROCESSING      ###################'
print '################## START SOURCE             ###################'
get_words('termer_source/corpus.lem', files_source)
print '################## END   SOURCE             ###################'
print '################## START TARGET             ###################'
get_words('termer_target/corpus.lem', files_target)
print '################## END   TARGET             ###################'
print '################## END   PREPROCESSING      ###################'
print '###############################################################', '\n\n'

print '################## SEARCH TRANSFUGES        ###################', '\n'
find_transfuges(files_source, files_target)
print '\n', '###############################################################'
print '################## SEARCH COGNAT CANDIDATES ###################'

