#!/usr/bin/env python
"""
position code for compression

 Method: 

 Divide data into blocks of size B = 2**bits

 Encode the 1s positions into bytes
 If there are multiple blocks, 
   precede every 1 by a "1" and every end of block by a "0".

 File length N must be known somehow to receiver

 How well it works: 

 bits=14, multipleblocks=0: 1442

 bits=5: Achieves 931 bits on Neil's file.
 bits=6: Achieves 878 bits on Neil's file.
 bits=7: Achieves 903 bits on Neil's file.
 bits=8: Achieves 967 bits on Neil's file.
 bits=14: 1546  (ie all in one block, with a waste of 100 bits
        used to convey the fact that there are 100 1 bits in the file,
        since that number is effectively conveyed in unary)

 usage:
  position.py -bits 5 < /home/mackay/compress/BentCoinFile > encoded.pos5
  position.py -bits 5  -decode 1 < encoded.pos5  > recovered5
  position.py -bits 8 < /home/mackay/compress/BentCoinFile > encoded.pos8
  position.py -bits 8  -decode 1 < encoded.pos8  > recovered8
  position.py -bits 6 < /home/mackay/compress/BentCoinFile > encoded.pos6
  position.py -bits 6  -decode 1 < encoded.pos6  > recovered6
  position.py -bits 7 < /home/mackay/compress/BentCoinFile > encoded.pos7
  position.py -bits 7  -decode 1 < encoded.pos7  > recovered7
  position.py -multipleblocks 0 -bits 14 < /home/mackay/compress/BentCoinFile > encoded.pos
  position.py -multipleblocks 0 -bits 14 -decode 1 < encoded.pos  > recovered
"""
from IntegerCodes import *

def encode(string,bits=7,multipleblocks=1):
    """
    In the special case where we encode the whole file in one
    block, by sending the positions of the 1s, we can set
    multipleblocks=0 and save a few bits.

    >>> print encode("00100000",3,0) ## 2nd bit is a 1.  (numbering from 0)
    010
    >>> print encode("00100001",3,0) ## 2nd and 7th bits (numbering from 0)
    010111
    >>> print encode("00100001",3,1) ## 2nd and 7th bits (numbering from 0)
    1010111100
    >>> print encode("0010000100000000",3,1) ## (2nd and 7th bits)(no bits)
    10101111000
    """
    B=1<<bits
    i = 0
    ans = ""
    for c in list(string):
    ## read a single character
        if( c == "1" ) :
            if (multipleblocks) : ans = ans + "1" ; pass  ## to say 'there is another 1 in this block'
	    ans = ans + dec_to_bin(i,bits)  ## send the details of the bit
            i += 1
            pass
        elif( c == "0"):
            i += 1
	if (multipleblocks and (i>=B) ) :
	    ans = ans + "0"  ## to say 'a block has ended'
	    i=0
            pass
    ## finish the file.
    if (multipleblocks) :    
        ans = ans + "0"
        pass
    return ans
    pass

def pos_decode(string,N=10000,bits=7,multipleblocks=1):
    """
    Decoder must be told the uncompressed filelength, N

    >>> print pos_decode("010",8,3,0) ## 2nd bit is a 1.  (numbering from 0)
    00100000
    >>> print pos_decode("010111",8,3,0) ## 2nd and 7th bits (numbering from 0)
    00100001
    >>> print pos_decode("1010111100",8,3,1) ## 2nd and 7th bits (numbering from 0)
    00100001
    >>> print pos_decode("10101111000",16,3,1) ## (2nd and 7th bits)(no bits)
    0010000100000000
    """
    B=1<<bits
    offset=0
    output = [] ## create an array of N zeroes
    for n in range( N ):
        output.append("0")
    clist =  list(string)
    if (multipleblocks):
        while len(clist)>0:
            c = clist.pop(0) ## delete the first bit from the input
            if( c == "1" ) : ## read 'bits' more bits from the list.
		i = bin_to_dec( clist , bits ) 
		output[i + offset] = "1"  
		pass
            else :
		offset += B 
                pass
            pass
        assert offset>=N ## check that we received the number of blocks expected
	pass
    else :
        while len(clist)>0 :
            i =  bin_to_dec( clist , bits ) 
	    output[i] = "1"
            pass
        pass
    return "".join(output)

def test():
    import doctest
    verbose=1
    if(verbose):
        doctest.testmod(None,None,None,True)
    else:
        doctest.testmod()
    pass

def usage(name):
    print "Usage:"
    print name,"<options>"
    print "options:               (defaults)"
    print "   -N filelength        (10000)"
    print "   -multipleblocks 0/1  (1)"
    print "   -bits bits           (8)  number of bits defines block size (2^b)"
    print "   -verbose verbosity   (0)"
    print "   -decode 0/1          (0)"
    print "   -outputf filename    (stdout)"
    exit
    pass

# from string import *
# from types import *

def main():
    ## This part sets defaults then reads values from the command line ARGV
    ## DEFAULTS
    decode=0  ## are we decoding?
    verbose=0 ## verbose output?
    multipleblocks=1 ## should we chop the file into multiple blocks?
    bits=7    ## how big are the blocks? (2^bits)
    output = sys.stdout ; outputf = "stdout" ## Sorry, this program only writes to stdout
    N = 10000 ## What is the file length?

    ## End defaults
    ## Command-line reader:  Reads pairs of the form
    ##        -variableName value
    ## and sets  variableName=value
    while 1 :
        if( len(sys.argv) <= 1 ): break
        if ( sys.argv[1][0] == "-" ):
            name = sys.argv.pop(1)
            name = name[1:len(name)] 
            if verbose > 2:
                print >> sys.stderr, "Reading from commandline: ",name
            if( len(sys.argv) <= 1 ): 
                print >> sys.stderr, "could not get value for name ", name, len(sys.argv)
                usage(sys.argv[0])
                pass
            else:
                value = sys.argv.pop(1)
                # should assert that this variable exists!
                command = "ntype = type(%s)" % (name)  # find type of this variable
                exec command
                
                # need to make value of the right type
                command = "%s = ntype(%s) # %s" % ( name,`value`, str(ntype) )
                if verbose:
                    print >> sys.stderr , "setting value:", command
                exec command
                pass
            pass
        else:
            usage(sys.argv[0])
            pass
        pass

    print >> sys.stderr, "  N =",N,"  bits =",bits, \
          "    multiblocks = ",multipleblocks
    if decode:
        print >> sys.stderr, "DECODING"
        output.write( pos_decode( sys.stdin.read() , N, bits, multipleblocks  ) )
        pass
    else:
        print >> sys.stderr, "ENCODING"
        output.write( encode( sys.stdin.read() , bits, multipleblocks ) )
        pass
    pass

if __name__ == '__main__':
    if sys.argv == [''] : ## probably we have been invoked by C-c C-c
        test()
        pass
    else : ## read data from stdin and write to stdout
        main()
        pass
    pass

