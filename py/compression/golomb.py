#!/usr/bin/env python
## compressor (Golomb-Rice) for a sparse file, with many 0s and few 1s.
## Has one parameter m, which defines via M=2^m the typical number of consecutive 0s expected;
## The Golomb-Rice code can be viewed as an approximate arithmetic code
## (c) David J.C. MacKay
## License: GPL                                  http://www.gnu.org/copyleft/gpl.html
## Originates from:  http://www.inference.phy.cam.ac.uk/mackay/itprnn/code/c/compress/
"""
 Compressor (Golomb) for a sparse file, with many 0s and few 1s.
 Has one parameter m, which defines via M=2^m the typical number of consecutive 0s expected;
 The Golomb code can be viewed as an approximate arithmetic code.
 The Golomb code with parameter m is also identical to the
 Huffman run-length code for a particular value of the P(1).

 This package provides functions <b>encode(string,m)</b> and  <b>decode(string,m)</b>
"""
import sys, string , re
from regex_example import re_show, re_delete
from IntegerCodes import dec_to_bin

verbose=0;
mGolombDefault = 7

def encode(string, mGolomb=mGolombDefault):
    MGolomb = (1<<mGolomb)
    tot0=0;r=0;tot1=0
    counter=0 ; ans=""
    for s in string : ## run through the symbols
        if ( s  == '0' ) :
            tot0 += 1
            r+=1 
            if( r>= MGolomb ) :
                ans = ans+"1"  ## sending "1" encodes "0"xM
                if verbose: print ans
                r = 0
                counter +=1
                pass
            pass
        elif ( s=='1' ) :
            tot1 += 1
            ## sending "0" encodes the fact that, next, there are fewer than Mx"0" on the way, followed by a "1"
            ## print out current value of r using m bits
            counter +=1
            ans = ans + "0" + dec_to_bin( r , mGolomb ) ## sending "r" in binary encodes  "0"xr followed by 1.
            if verbose: print ans
            counter +=mGolomb
            r = 0
            pass
        else :
                ## we ignore carriage returns, etc
            pass
        pass
## To make a self-delimiting file, we need to send the final value of r, and have the receiver
## know to remove the final "1".  As encoder, we act as if an extra '1' were received.
    ans = ans + "0" + dec_to_bin( r , mGolomb ) ; 
    counter +=1 + mGolomb
##       sys.stderr.write("\nShannon information content of the file is %d log(f) + %d log(1-f) = %8.3f\n",tot0,tot1, (tot0*log(0.99)+tot1*log(0.01))/log(0.5) )  
  ##  fprintf(stderr,"Decompress with GolombDecode %d < filename\n",tot0+tot1 ) ;
    ## print ans
    N=tot0+tot1
    print >> sys.stderr,  N,"bits read (",tot0," 0s,",tot1," 1s); ",counter,"bits written"
    return ans
    pass

## prints a string of r zeroes, PRECEDED by "1" if the second flag is set. 

def  printzeroes( r , needtoprint1 , ans ) :
    ## here we wish to modify the value of needtoprint1, so we send it as an array pointer
    if ( needtoprint1[0] ) :
        if(verbose): print ans ;         print ans[0] ; pass
        ans[0] = ans[0] + "1"
        needtoprint1[0] = 0
        pass
    if r>0:
        ans[0] = ans[0] + "0"*r
    pass

def join(alist,sep='\n'): return sep.join(alist)

def decode(mystring, mGolomb=mGolombDefault):
    if(verbose): print "decoding ", mystring
    MGolomb = (1<<mGolomb)
    needtoprint1 = [0] ; r=0
    pointer = 0
    L = len(mystring)
    ans=[""]
    
    while pointer<L :
        s=mystring[pointer]
        pointer+=1
#        print pointer,s
        if ( s  == '1' ) :
            if(verbose): print "read 1"
            printzeroes(MGolomb, needtoprint1 , ans )
            r = 0
            pass
        elif ( s=='0' ) : ## read in the next m bits as an integer
            r=0
            if(verbose): print "read 0, pointer,L is ", pointer, L
            for u in range(mGolomb) :
                r = r * 2 
                assert pointer<L
                c=mystring[pointer]
                pointer+=1
                if( c == '1' ) : r+=1 ; pass
                pass
            printzeroes(r, needtoprint1 , ans ) ## Ideally we should print a "1" immediately, but
                        ## instead we postpone this printing event and make a note that we need to do it later;
                        # this is an ugly hack to cope with the "add a virtual trailing 1" behaviour of the encoder.
                        # The final "1" does not get printed.
            needtoprint1 = [1];
            pass
        else :
            ## we ignore carriage returns, etc
            pass
        pass
    return ans[0]
    pass

def test():
    for a in [ '000001000000000000000000' , '010' ]:
        b = encode(a)
        c = decode(b)
        print b, ", encoded from"
        print a, " decoded again as"
        print c
        print
##    decodefromstdin()
    pass

if __name__ == '__main__':
    test()
    pass
