#!/usr/bin/env python
##                   (c) David MacKay - Free software. License: GPL
## For license statement see  http://www.gnu.org/copyleft/gpl.html
"""
This is a RunLength Encoding compression algorithm
that uses the Huffman algorithm to define a code
for runlengths.

The package contains the following functions:

 findprobs(f=0.01,N=69):
    Find probabilities of all the events
    - No 0s followed by a 1;
    - 1 0  followed by a 1;
    - 2 0s  followed by a 1;
    - - - - - - - - 
    - (N-1) 0s followed by a 1;
    - N 0s

 RLencode(string,symbols,N):
    Reads in a string of 0s and 1s.
    Creates a list of run lengths and special symbols 'Z',
    which denote 'a lot of Zeroes'. ('A lot' is N.)
    Sends this list to the general-purpose Huffman encoder
    defined by the nodes in the list "symbols".
    
 RLdecode(string,root,N):
    Decode a binary string into runs, then return appropriate 0s and 1s
    
 compress_it( inputfile, outputfile ):
    Make Huffman code using runlengths, and compress
    
 uncompress_it( inputfile, outputfile ):
    Make Huffman code using runlengths, and uncompress

 There are also three test functions.
 If RLE.py is run from a terminal, it invokes compress_it (using stdin)
 or uncompress_it (using stdin), respectively if there are zero arguments
 or one argument.

"""
## /home/mackay/python/compression/huffman/Huffman3.py
## This supplies the huffman algorithm, complete with encoders and decoders:
from Huffman3  import  *
verbose=0

def findprobs(f=0.01,N=69):
    """ Find probabilities of all the events
    - No 0s followed by a 1;
    - 1 0  followed by a 1;
    - 2 0s  followed by a 1;
    - 3 0s  followed by a 1;
    - - - 
    - (N-1) 0s followed by a 1;
    - N 0s
    >>> print findprobs(0.1,3)              # doctest:+ELLIPSIS
    [('0', 0.1...), ('1', 0.09...), ('2', 0.081...), ('Z', 0.729...)]
    """
    answer = []
    for n in range(N):
        answer.append( (`n`, f * (1-f)**n) )
        pass
    answer.append( ('Z', (1-f)**N) )
    assert ( len(answer) == N+1 )
    return answer

def RLencode(string,symbols,N):
    """
    Reads in a string of 0s and 1s.
    Creates a list of run lengths and special symbols 'Z',
    which denote 'a lot of Zeroes'. ('A lot' is N.)
    Sends this list to the general-purpose Huffman encoder
    defined by the nodes in the list "symbols".
    """
    chars = list(string)
    runs = [] ## list of run lengths
    r = 0     ## the current run length
    for c in chars:
        if ( c == '0' ):
            r += 1
            if (r>=N):
                runs.append('Z')
                r=0
                pass
            pass
        else:
            runs.append(`r`)
            r=0
            pass
        pass
    runs.append(`r`) ## pretend that there is a final 1. The decoder must strip this off.
    if verbose:
        print "runs to be encoded:"
        print runs
        pass
    zipped = encode( runs , symbols )
    return zipped

def RLdecode(string,root,N):
    """
    Decode a binary string into runs, then return appropriate 0s and 1s
    """
    answer = decode( string, root )
    if verbose:
        print "runs from decoder:"
        print answer
        pass
    output = ""
    for r in answer:
        if ( r == 'Z' ):
            output = output + '0'*N
        else:
            temporary = output + '0'*int(r)
            output = temporary + '1'
        pass
    ## strip off the final '1'

    assert ( r != 'Z' ) ## the output at this point should always end with a 1
    output = temporary  ## remove that final 1
    return output

def easytest():
    N=5
    f=0.01
    probs = findprobs(f,N)
    symbols = makenodes(probs) # makenodes is defined at the bottom of Huffman3 package
    root = iterate(symbols) # make huffman code and put it into the symbols' nodes, and return the root of the decoding tree

    symbols.sort(lambda x, y: cmp(x.index, y.index)) # sort by index 
    for co in symbols :                              # and write the answer
        co.report()

    zipped = encode(['0','1','2','Z','2','1','0'], symbols)
    print zipped
    answer = decode( zipped, root )
    print answer

    for s in [ "00000000000001",\
               "00000000000011",\
               "00000000000011",\
               "00000000000111",\
               "0000000000110",\
               "0000000000111100000000",\
               "0000000000001111000000000",\
               "01", "1000" ,  "0000000000001000001000000001100010" ,\
               "0000000000000000000000000000000000000000010000000000000000000000000000000000000001000000000000000000000001110" ]:
        print "=== encoding", s
        zipped = RLencode( s, symbols, N )
        print zipped
        output = RLdecode( zipped, root, N)
        print output
        if (s==output):
            print "OK!"
        else:
            print s
            print output
            print "ERROR"
            pass
        pass
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

    print "Checking for differences"
    import os
    os.system( "diff /home/mackay/compress/BentCoinFile tmp2" )
    pass

f=0.01; N=69

def compress_it( inputfile, outputfile ):
    """
    Make Huffman code using runlengths, and 
    Compress from file (possibly stdin).
    """
    probs = findprobs(f,N)
    symbols = makenodes(probs)
    root = iterate(symbols) # make huffman code and put it into the symbols' nodes, and return the root of the decoding tree

    string = inputfile.read()
    outputfile.write( RLencode(string, symbols, N) )
    pass

def uncompress_it( inputfile, outputfile ):
    """
    Make Huffman code using runlengths, and 
    UNCompress from file (possibly stdin).
    """
    probs = findprobs(f,N)
    symbols = makenodes(probs)
    root = iterate(symbols) # make huffman code and put it into the symbols' nodes, and return the root of the decoding tree

    string = inputfile.read()
    outputfile.write( RLdecode(string, root, N) )
    pass

if __name__ == '__main__':
    print "testing runlength encoder"
    
    test()
    pass

