#!/usr/bin/env python
##                   (c) David MacKay - Free software. License: GPL
## For license statement see  http://www.gnu.org/copyleft/gpl.html
"""
This is a BLOCK compression algorithm
that uses the Huffman algorithm.

This simple block compressor assumes that the source file
is an exact multiple of the block length.
The encoding does not itself delimit the size of the file, so
the decoder needs to knows where the end of the compressed
file is. Thus we must either ensure the decoder knows
the original file's length, or we have to use a special
end-of-file character. A superior compressor would first
encode the source file's length at the head of the compressed
file; then the decoder would be able to stop at the right
point and correct any truncation errors. I'll fix this
in block2.py.

The package contains the following functions:

 findprobs(f=0.01,N=6):
    Find probabilities of all the events
    000000
    000001
     ...
    111111
    <-N ->

 Bencode(string,symbols,N):
    Reads in a string of 0s and 1s, forms blocks, and encodes with Huffman.
    
 Bdecode(string,root,N):
    Decode a binary string into blocks, then return appropriate 0s and 1s
    
 compress_it( inputfile, outputfile ):
    Make Huffman code, and compress
    
 uncompress_it( inputfile, outputfile ):
    Make Huffman code, and uncompress

 There are also three test functions.
 If block.py is run from a terminal, it invokes compress_it (using stdin)
 or uncompress_it (using stdin), respectively if there are zero arguments
 or one argument.

"""
## /home/mackay/python/compression/huffman/Huffman3.py
## This supplies the huffman algorithm, complete with encoders and decoders:
from Huffman3  import  *
from IntegerCodes import *
verbose=0

def weight(string):
    """
    ## returns number of 0s and number of 1s in the string
    >>> print weight("00011")
    (3, 2)
    """
    w0=0;w1=0
    for c in list(string):
        if(c=='0'):
            w0+=1
            pass
        elif(c=='1'):
            w1+=1
            pass
        pass
    return (w0,w1)


def findprobs(f=0.01,N=6):
    """ Find probabilities of all the events
    000000
    000001
     ...
    111111
    <-N ->
    >>> print findprobs(0.1,3)              # doctest:+ELLIPSIS
    [('000', 0.7...),..., ('111', 0.001...)]
    """
    answer = []
    for n in range(2**N):
        s = dec_to_bin(n,N)
        (w0,w1) = weight(s)
        if verbose and 0 :
            print s,w0,w1
        answer.append( (s, f**w1 * (1-f)**w0 ) )
        pass
    assert ( len(answer) == 2**N )
    return answer

def Bencode(string,symbols,N):
    """
    Reads in a string of 0s and 1s.
    Creates a list of blocks of size N.
    Sends this list to the general-purpose Huffman encoder
    defined by the nodes in the list "symbols".
    """
    blocks = []
    chars = list(string)

    s=""
    for c in chars:
        s = s+c
        if (len(s)>=N):
            blocks.append( s )
            s = ""
            pass
        pass
    if (len(s)>0):
        import sys
        print >> sys.stderr, "warning, padding last block with 0s"
        while (len(s)<N):
            s = s+'0'
            pass
        blocks.append( s )
        pass
        
    if verbose:
        print "blocks to be encoded:"
        print blocks
        pass
    zipped = encode( blocks , symbols )
    return zipped

def Bdecode(string,root,N):
    """
    Decode a binary string into blocks.
    """
    answer = decode( string, root )
    if verbose:
        print "blocks from decoder:"
        print answer
        pass
    output = "".join( answer )
    ## this assumes that the source file was an exact multiple of the blocklength
    return output

def easytest():
    """
    Tests block code with N=3, f=0.01 on a tiny example.
    >>> easytest()                 # doctest:+NORMALIZE_WHITESPACE
    #Symbol	Count	        Codeword
    000	        (0.97)	        1
    001	        (0.0098)	001
    010	        (0.0098)	010
    011	        (9.9e-05)	00001
    100	        (0.0098)	011
    101	        (9.9e-05)	00010
    110	        (9.9e-05)	00011
    111	        (1e-06) 	00000
    zipped  = 1001010000010110111
    decoded = ['000', '001', '010', '011', '100', '100', '000']
    OK!
    """
    N=3
    f=0.01
    probs = findprobs(f,N)
    if len(probs) > 999 :
        import sys
        sys.setrecursionlimit( len(probs)+100 )
    symbols = makenodes(probs) # makenodes is defined at the bottom of Huffman3 package
    root = iterate(symbols) # make huffman code and put it into the symbols' nodes, and return the root of the decoding tree

    symbols.sort(lambda x, y: cmp(x.index, y.index)) # sort by index 
    for co in symbols :                              # and write the answer
        co.report()

    source = ['000','001','010','011','100','100','000']
    zipped = encode(source, symbols)
    print "zipped  =",zipped
    answer = decode( zipped, root )
    print "decoded =",answer
    if ( source != answer ):
        print "ERROR"
    else:
        print "OK!"
    pass

def test():
    easytest()
    import doctest
    verbose=1
    if(verbose):
        doctest.testmod(None,None,None,True)
    else:
        doctest.testmod()
    hardertest()
    pass

def hardertest():
    print "Reading the BentCoinFile"
    inputfile = open( "/home/mackay/compress/BentCoinFile" , "r" )
    outputfile = open( "tmp.zip" , "w" )
    print  "Compressing to tmp.zip"
    compress_it(inputfile, outputfile)
    outputfile.close();     inputfile.close()
#    print "DONE compressing"

    inputfile = open( "tmp.zip" , "r" )
    outputfile = open( "tmp2" , "w" )
    print  "Uncompressing to tmp2"
    uncompress_it(inputfile, outputfile)
    outputfile.close();     inputfile.close()
#    print "DONE uncompressing"

    print "Checking for differences..."
    import os
    os.system( "diff /home/mackay/compress/BentCoinFile tmp2" )
    os.system( "wc tmp.zip /home/mackay/compress/BentCoinFile tmp2" ) 
    pass

f=0.01; N=12   #  1244 bits if N==12
f=0.01; N=5   #  2266  bits if N==5
f=0.01; N=10   #  1379 bits if N==10

def compress_it( inputfile, outputfile ):
    """
    Make Huffman code for blocks, and 
    Compress from file (possibly stdin).
    """
    probs = findprobs(f,N)
    symbols = makenodes(probs)
    if len(probs) > 999 :
        import sys
        sys.setrecursionlimit( len(probs)+100 )
    root = iterate(symbols) # make huffman code and put it into the symbols' nodes, and return the root of the decoding tree

    string = inputfile.read()
    outputfile.write( Bencode(string, symbols, N) )
    pass

def uncompress_it( inputfile, outputfile ):
    """
    Make Huffman code for blocks, and 
    UNCompress from file (possibly stdin).
    """
    probs = findprobs(f,N)
    if len(probs) > 999 :
        import sys
        sys.setrecursionlimit( len(probs)+100 )
    symbols = makenodes(probs)
    root = iterate(symbols) # make huffman code and put it into the symbols' nodes, and return the root of the decoding tree

    string = inputfile.read()
    outputfile.write( Bdecode(string, root, N) )
    pass

if __name__ == '__main__':
    test()
    pass

