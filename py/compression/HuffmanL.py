#! /usr/bin/python
# Like Huffman.py, but it returns a list of lists            (c) David MacKay  Dec 2005
# - writes a huffman code                              This is Free Software. License: GPL
import sys, string
class node:
    def __init__(self, count, index , name="" ):
        self.count = float(count)
        self.index = index
        self.name  = name ## optional argument
        if self.name=="" : self.name = index
        self.word = "" ## codeword will go here
    def __cmp__(self, other):
        return cmp(self.count, other.count)
    def report(self):
        if (self.index == 0 ) :
            print '#Symbol\tCount\t\tCodeword'
        print '%s\t(%-8.4g)\t%s' % (self.name,self.count,self.word)

def find(f, seq):
    """Return first item in sequence where f(item) == True."""
    for item in seq:
        if f(item): 
            return item

    """
    Example usage of iterate: 
    >>> c = []; \
        c.append(node(0.5,1,'a')); \
        c.append(node(0.25,2,'b')); \
        c.append(node(0.125,3,'c')); \
        c.append(node(0.125,4,'d')); \
        iterate(c) ; reportcode(c)                          # doctest: +NORMALIZE_WHITESPACE
    #Symbol Count   Codeword
    a       (0.5)   1
    b       (0.25)  01
    c       (0.12)  000
    d       (0.12)  001
    """
def iterate (c, BigList) :   ## The list of nodes c is destroyed as we go, then recreated
    if ( len(c) > 1 ) :
        c.sort() ## sort the nodes by count, using the __cmp__ function defined in the node class
        deletednode = c[0] ## keep copy of smallest node so that we can put it back in later
        second = c[1].index ## index of second smallest node
        # MERGE THE BOTTOM TWO
        c[1].count += c[0].count  ##  this merged node retains the name of the bigger leaf.
        del c[0]
#        print len(BigList), deletednode.index, second, len(c)
        BigList[ second ] = [ BigList[ deletednode.index ],  BigList[ second ] ]
#        print BigList

        A = iterate ( c, BigList )
        if A=="Done!" :
            A = BigList[ second ]

        ## find the codeword that has been split/joined at this step
        co = find( lambda p: p.index == second , c )
        deletednode.word = co.word+'0'
        c.append( deletednode )  ## smaller one gets 0
        co.word += '1'
        co.count -= deletednode.count   ## restore correct count
    else :
#        print len(c), "Done"
        c[0].word = ""
        A = "Done!"
    return A

def encode(sourcelist,code):
    """
    Takes a list of source symbols. Returns a binary string.
    """
    answer = ""
    for s in sourcelist:
        co = find(lambda p: p.name == s, code)
        if ( not co  ):
            import sys
            print >> sys.stderr, "Warning: symbol",`s`,"has no encoding!"
            pass
        else:
            answer = answer + co.word
            pass
    return answer

def decode(string,root):
    """
    Decodes a binary string using the Huffman tree accessed via a list of lists
    How the decoder could work with this "A" structure:

    l = list(string)
    p = A
    while len(l)>0:
     c = int(l.pop(0))
     p = p[c]
     if p is a string :
         output.append( p ) ; p = A
    """
    ## split the string into a list
    ## then copy the elements of the list one by one.
    answer = []
    clist = list( string )
    ## start from root
    currentnode = root
    for c in clist:
        if ( c=='\n' ):  continue ## special case for newline characters
        assert ( c == '0' )or( c == '1')
        currentnode = currentnode[int(c)]
        if isinstance( currentnode , str ) :
            answer.append( currentnode )
            currentnode = root
        pass
    assert (currentnode == root) ## if this is not true then we have run out of characters and are half-way through a codeword
    return answer

def easytest():
    """
    This test routine demonstrates use of the huffman function, which takes
    a list of simple counts as its input.  It uses the encode function
    to encode a sequence of symbols, [1,2,3,4,3,2,1].  It then uses
    decode to recover the original sequence.
    >>> easytest()
    10010111011101100
    ['1', '2', '3', '4', '3', '2', '1']

    See also the function makenodes for a more sophisticated example.
    See also the file Example.py for a python program that uses this package.
    """
    (c,root) = huffman([1, 2, 3, 4])
    s = encode([1,2,3,0,3,2,1],c)
    print s
    ans = decode( s, root )
    print ans 

def test(): ## This is the main example. It must be run from a shell and it reads in counts from stdin
## begin read in the list of counts ####################################
    easytest()

def testfromstdin():    
    c=[]
    m=0
    for line in sys.stdin.readlines():
        if line[0] != '#' :
            words = string.split(line) 
            if len(words) >= 2:
                c.append( node( words[0], m, words[1] ) ) ; m += 1 
            elif len(words) >=1 :
                c.append( node( words[0], m ) ) ; m += 1   
    ## end  read in the list of counts ####################################

    ## make a list of all the symbols' indexes, which will be turned into a binary tree of lists
    BigList = []
    for co in c :
        BigList.append( co.index )
#    print BigList
    A = iterate ( c, BigList )  # make huffman code
    reportcode(c)
    reportLH(c)
    print A


def reportcode(c):    
    c.sort(lambda x, y: cmp(x.index, y.index)) # sort by index 
    for co in c :                              # and write the answer
        co.report()    

def reportLH(c,verbose=1):
    from  math import log
    total=0;     H=0 ; L=0
    for co in c :                             
        total += co.count
    for co in c :                             
        p = co.count * 1.0 / total
        logp = log(p)/log(2.0)
        H -= p * logp
        L += p * len(co.word)
    if verbose: print "#L = %10.6g    H = %10.6g      L/H = %10.7g" % ( L, H , L/H)
    return (L,H)
        
## end ##########################################################################

## alternate way of calling huffman with a list of counts ## for use as package by other programs ######
## two example ways of using it:
#>>> from Huffman import *
#>>> huffman([0.5, 0.25, 0.125, 0.125],1)
#>>> c = huffman([1, 2, 3, 4])

def huffman( counts , verbose=0 ) :
    """
    >>> (c,A) = huffman([0.5, 0.25, 0.125, 0.125],1)          # doctest: +NORMALIZE_WHITESPACE
    #Symbol Count   Codeword
    1       (0.5)   1
    2       (0.25)  01
    3       (0.12)  000
    4       (0.12)  001
    """
    c=[] ; BigList=[] ## array of nodes
    m=0
    for line in counts : 
        c.append( node( line, m ) )
        BigList.append( str(m) )
        m += 1 
    A = iterate ( c , BigList )  # make huffman code
    if (verbose) :
        reportcode(c)
        reportLH(c)
        print A
    return (c,A)
## end ##########################################################################
    
if __name__ == '__main__':
    import sys
    if sys.argv == [''] : ## probably we have been invoked by C-c C-c in emacs
        test()
        pass
    else : ## read data from stdin and write to stdout
        testfromstdin()
    pass
