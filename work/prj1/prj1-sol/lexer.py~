#!/usr/bin/env python3

import re
import sys
from collections import namedtuple




def next_match(text):
    m = re.compile(r'\s+').match(text)
    if (m): return (m, None)  #ignore white space

    m = re.compile(r'\d+').match(text)
    if (m): return (m, 'INT')
    
    m = re.compile(r'def').match(text)
    if (m): return (m, 'DEF')
    
    m = re.compile(r'\w+').match(text) #alpanumeric or _
    if (m): return (m, 'ID')
    
    m = re.compile(r'==').match(text)
    if (m): return (m, '==')
    
    m = re.compile(r'!=').match(text)
    if (m): return (m, '!=')
    
    m = re.compile(r'<=').match(text)
    if (m): return (m, '<=')
    
    m = re.compile(r'>=').match(text)
    if (m): return (m, '>=')
    
    m = re.compile(r'.').match(text)  
    if (m): return (m, m.group()) 



def scan(text):
    tokens = []
    comment =0
    temp = False
    while (len(text) > 0):
        (match, kind) = next_match(text)
        lexeme = match.group()
        if (kind is r'#'):
                comment = comment + 1
                temp = True
        if (kind and temp is False and comment !=1):
                tokens.append(Token(kind,lexeme))
        text = text[len(lexeme):]
        temp = False
    return tokens

def main():
    contents = sys.stdin.read();
    tokens = scan(contents)
    print('\n');
    print('[');
    for t in tokens: 
       print('','','','{\n', '','','','','',
              '"kind" :', '"' + t.kind + '",', '\n','','','','','', 
              '"lexeme" :', '"' + t.lexeme + '"', '\n', '', '', '},')
    
    
    print('','','','{\n', '','','','','',
              '"kind" :', '"<EOF>",', '\n','','','','','', 
              '"lexeme" :', '"<EOF>"', '\n', '', '', '}')
    print(']');    
    print('\n');



Token = namedtuple('Token', ['kind', 'lexeme'])


if __name__ == "__main__":
    main()
