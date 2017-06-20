#!/usr/bin/env python
"""
 Run-length compressor, "without explicit probabilistic model", for Neil's file.

 Compresses lengths of runs of 1s or 0s into a uniquely decodeable representation, C_alpha,
 as described in Information Theory, Inference, and Learning Algorithms. (Ch 7: Codes for Integers)
 Runs of 0s and 1s are treated identically.

 So this decoder is expected to perform 'quite well' for
 a wide range of redundant sources. It's not only quite good for
 files like '000000000000000100000000000000000001010000000'
 and '1111111111111111011111111111111111111111001111111'
 but also especially good for files like
 '11111111111111111111000000000000000000000',
 which none of the other 'Bent Coin' compressors handle well.

 usage I: This package can be directly executed as follows:

 COMPRESS:
 python IRL.py < BentCoinFile > tmp
 UNCOMPRESS: 
 python IRL.py --uncompress < tmp > tmp2

 If this package is executed by C-c C-c from within emacs,
 it runs the function "test"
 which performs a few small encodings and decodings.

 usage II: see IRLc.py and IRLd.py for examples of programs that use this
 package.

 This compressor can be used in two ways: the 'smart' way and the 'nonsmart'
 way. The 'smart' way generates slightly longer compressed files that are fully self-delimiting.
 So you can concatenate the compressed files together end to end, and the decoder can
 read along, uncompressing each file separately.
 This smartness is achieved by <b>prepending</b> the Number Of Runs
 to the compressed string.

 The package includes functions <b>encode</b> and <b>decode</b>
 which produce the compressed file, and optionally prepend 
 an encoded integer to the head of the file, thus making 
 it self-delimiting.
 decode calls either smartdecode (which carefully reads 
 just the right number of bits from the compressed list 
 in order to recover one file) or simpledecode (which 
 reads until the end of the compressed list is reached, 
 and which thus requires  either that the receiver 
 know the file length, or that the file system has a special
 end of file character.

 The function multiplefiledecode() illustrates how the smartdecoder
 can recover multiple compressed files that have been concatenated.
"""

# ENCODING:
def encode(string,selfdelimiting=0):
    """
    Encode one string's runs using the code C_alpha.
    If selfdelimiting, prepend the number of
    runs, so the decoder will know when to stop reading.
    The first run's first character is sent in the clear;
    all subsequent runs alternate between 0 and 1.

    >>> print encode("1")
    11
    >>> print encode("1111111") ## seven 1s
    100111
    >>> print encode("11111110000000") ## seven 1s and seven 0s
    10011100111
    >>> print encode("00000001111111") ## seven 0s and seven 1s
    00011100111
    
    """
    N=0 ## counts the number of characters read from the source string
    Nruns=0 ## counts the number of runs
    firsttime = 1
    assert(len(string)>0) ## Can't encode an empty string
    for c in string :
        if ( firsttime ) :
            # print the very first character
            if( c == '1' ) or  (c=='0') :
                ans = c ; N+=1
                oldc = c
                r = 1 ## start counting run length
                firsttime = 0 
                pass
            else: ## ignore bogus character
                pass
	else :
            if( (c == '1' ) or (c=='0') ) :
                N+=1
                if ( c != oldc ) :
                    ans = ans + encoded_alpha (r)
                    Nruns +=1
                    oldc = c 
                    r = 1
                    pass
                else:
                    r+=1
                    pass
                pass
            pass
        pass
    ans = ans + encoded_alpha (r) ## report the final run.
    Nruns +=1
    if(selfdelimiting):  ## prepend the number of runs, so the decoder will know when to stop
        ans = encoded_alpha(Nruns) + ans
        pass
    return ans
    pass

from IntegerCodes import dec_to_bin, encoded_alpha, get_alpha_integer, bin_to_dec

## :DECODING: 
def decode(string,selfdelimiting=0):
    """
    general runlength decoder:
    must receive a string of 0 and 1, no other characters

    >>> print decode("11")
    1
    >>> print decode("111111111")
    10101010
    """
    clist = list ( string )
    if(selfdelimiting==0):
        return simpledecode(clist)
    else:
        runs = get_alpha_integer( clist ) ## r is the number of runs
        return smartdecode(clist,runs)
    pass

def smartdecode(clist,runs):
    ans=""
    if(runs>0):
        c=clist.pop(0)
        while ( runs>0 ):
            runs -= 1
            assert ( len(clist) > 0 ) 
            r = get_alpha_integer( clist )
            ans = ans + c*r ## add r copies of the current character
            if ( c=='1' ): c='0' ; pass
            else: c='1' ; pass
            pass
        pass
    return ans

## :DECODING: must receive a string of 0 and 1,
## no other characters
def simpledecode(clist):
    """ SIMPLEDECODE uses 'length of list is zero' to know when to stop """
    ans=""
    if ( len(clist) > 0 ) :
        c=clist.pop(0)
        while 1 :
            r = get_alpha_integer( clist )
            if (r<=0): break
            ans = ans + c*r ## add r copies of the current character
            if ( c=='1' ): c='0' ; pass
            else: c='1' ; pass
            pass
        pass
    return ans

def multiplefiledecode(string):
    """
    assumes that a set of files have been compressed using smartencoding,
    then concatenated
    """
    clist = list ( string )
    files = [] 
    while ( len(clist) > 0 ):
        runs = get_alpha_integer( clist ) ## r is the number of runs
        files.append(  smartdecode(clist,runs) )
        pass
    return files


def test():
    sources = [\
        "010",\
 '0000000000000000100000000000000000001010000000',\
 '1111111111111111111011111111111111111111111001111111',\
 '111111111111111111111111111100000000000000000000000',\
                 "0",\
        "1",\
        "00000000000001111111111111111110000000000000000000000",\
        "000111000"\
        ]
    for smartness in [1,0] :
        print "=============================================="
        print "smartness", smartness
        for source in sources :
            compressed = encode(source,smartness)
            uncompressed = decode(compressed,smartness)
            print
            print "encoding",source,"->", compressed
            print "decoding",uncompressed
            if source!=uncompressed:
                print "ERROR"
                pass
            pass

    print "\n ##demonstrate that the smart decoder can recover multiple files from a single archive"
    smartness=1
    zipfile = ""
    print sources
    for source in sources :
        compressed = encode(source,smartness)
        zipfile = zipfile + compressed
        pass
    print " -> compressed into single file "
    print zipfile
    uncompressed = multiplefiledecode(zipfile)
    print " -> recovered files"
    print uncompressed
    assert sources==uncompressed
    pass

def compress_it( inputfile, outputfile ):
    string = inputfile.read()
    outputfile.write(  encode(string) )
    pass

def uncompress_it( inputfile, outputfile ):
    string = inputfile.read()
    outputfile.write(  decode(string) )
    pass

if __name__ == '__main__':
    import sys
    verbose = 0 
    #    find out the command
    if verbose :
        print "Test has been called with the following argv:" 
        print sys.argv
        pass
    if sys.argv == [''] : ## probably we have been invoked by C-c C-c
        test()
        import doctest
        verbose=1
        if(verbose):
            doctest.testmod(None,None,None,True)
        else:
            doctest.testmod()
            pass
    else : ## read data from stdin and write to stdout
        if (len(sys.argv)==1): ## IRL.py
            print >> sys.stderr, "Compressing"
            compress_it(sys.stdin,sys.stdout)
        else:
            print >> sys.stderr,  "UNCompressing"
            uncompress_it(sys.stdin,sys.stdout)
        pass
    pass

    
