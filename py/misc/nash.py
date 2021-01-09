# Implementation of Nash Cryptosystem
# Ronald L. Rivest
# 2/17/2012

# This is a self-synchronizing cipher feedback stream cipher
# proposed by John Nash in 1950, just recently declassified.

# notes:
#
#    -- "^"  in python is xor (mod-2 addition)
#
#    -- in python range(b) is the sequence 0, 1, 2, ..., b-1
#
#    -- Nash doesn't say anything about the initial state of the bits
#       in the system; here we allow an initial state as part of the key
#       It would be reasonable and interesting to consider other 
#       possibilities, such as having a fixed initial state (all zeros),
#       or running the system with "0"'s as input for a while to arrive
#       at an initial state, or ... ??
#
#    -- We implement the example given in his note.  There is one arrow
#       missing a label; we assume here the missing label is a "+".
#       We also choose an arbitrary starting state as part of the key.
#
#    -- There are many interesting open questions about this system; 
#       here are some as ``food for thought'':
#       (a) Are there ``weak keys''?  (Keys that shouldn't be used?)
#       (b) If the system receives periodic input, it will exhibit
#           periodic output.  (E.g. input 001001001001001...)
#           What can be said about the periodicities?
#       (c) How do different guesses about what Nash intended
#           for the starting state affect security?
#       (d) How long can a given bit circulate internally?
#       (e) Can you figure out the permutations and bit-flips if you are allowed
#           to specify inputs to the system, and to reset it to
#           the initial state whenever you like?  (Effectively, a
#           chosen ciphertext attack)
#       (f) Is the output of the system balanced (equal number of 0's and 1's)
#           or unbalanced (biased somehow)?

class NashMethod:
    def __init__(self,n,redp,redbits,bluep,bluebits,initialP):
        # check that inputs are all of correct length
        assert n+2 == len(redp)
        assert n+2 == len(redbits)
        assert n+2 == len(bluep)
        assert n+2 == len(bluebits)
        assert n+2 == len(initialP)
        # initialize the Nash cryptosystem private state with the given parameters
        self.n = n                # number of state bits (not counting D, P entry point, or output bit)
        self.redp = redp          # specifies the red permutation: redp[i] says where bit i comes from, in the red permutation
        self.redbits = redbits    # 1 = complement, 0 = no complement: redbits[i] == 1 if you complemenet when copying to P[i]
        self.bluep = bluep        # blue permutation
        self.bluebits = bluebits  # same as for redbits
        self.P = initialP         # initialP = initial state P[0...n] and P[n+1]=output bit.  P[0] is entry point
                                  # as noted in the comments, it isn't really clear what Nash intended for the 
                                  # initial state of the system.

    def tick(self,c):
        """
        advance state for one tick, with input ciphertext bit c.
        """
        if c == 0:
            # use blue permutation
            # copy P[bluep[[i]] to P[i], complementing if bluebits[i]==1   (a "-" label on the blue arrow)
            self.P = [ self.P[self.bluep[i]] ^ self.bluebits[i] for i in range(self.n+2) ]
            # entry point of P gets new bit
            self.P[0] = c
        else:
            # use red permutation
            # copy P[redp[[i]] to P[i], complementing if redbits[i]==1   (a "-" label on the red arrow)
            self.P = [ self.P[self.redp[i]] ^ self.redbits[i] for i in range(self.n+2) ]
            # entry point of P gets new bit
            self.P[0] = c
        print "state: ", c, self.P

    def Enc(self,bs):
        """
        Encrypt bitstring bs, return ciphertext string
        """
        print "Enc: encrypting string bs = ", bs
        cs = [ ] 
        for b in bs:
            c = b ^ self.P[-1]       # add b to output bit to get next ciphertext bit (save it)
            cs.append(c)             # save ciphertext output
            self.tick(c)             # feedback and advance state
        print "Enc: ciphertext string cs = ", cs
        return cs

    def Dec(self,cs):
        """
        Decrypt bitstring cs, return ciphertext string
        """
        print "Dec: decrypting string cs = ", cs
        bs = [ ]
        for c in cs:
            b = self.P[-1] ^ c       # decoded plaintext
            self.tick(c)
            bs.append(b)
        print "Dec: plaintext string bs =  ", bs
        return bs
            
        
# test example from his paper
# entry point of P is position 0 of state: P[0]
# positions 1, 2, 3 across top row:  P[1], P[2], P[3]
# positions 4, 5, 6 across bottom row: P[4], P[5], P[6]
# position 7 is output bit: P[7]

def testNash():

    # set up key for encryption
    # key consists of: redp, redbits, bluep, bluebits, initialP  (see his figure)

    N1 = NashMethod( 6,                           # n = 6
                     [ 0, 5, 0, 4, 1, 6, 2, 3 ],  # redp
                     [ 0, 0, 0, 1, 0, 0, 1, 1 ],  # redbits [note assuming arrow to 4 is a "+" ]
                     [ 0, 6, 4, 2, 0, 1, 3, 5 ],  # bluep
                     [ 0, 1, 0, 1, 1, 1, 0, 0 ],  # bluebits
                     [ 0, 1, 1, 0, 1, 1, 0, 1 ],  # initialP -- initial state P[0...n+1] (arbitrary choice)
                     )

    # define test string to encrypt
    bs = [ 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1]

    # now do the encryption, save results as cs
    cs = N1.Enc(bs)

    # set up (same) key for decryption
    # key consists of: redp, redbits, bluep, bluebits, initialP  (see his figure)
    N2 = NashMethod( 6,                           # n = 6
                     [ 0, 5, 0, 4, 1, 6, 2, 3 ],  # redp
                     [ 0, 0, 0, 1, 0, 0, 1, 1 ],  # redbits [note assuming arrow to 4 is a "+"
                     [ 0, 6, 4, 2, 0, 1, 3, 5 ],  # bluep
                     [ 0, 1, 0, 1, 1, 1, 0, 0 ],  # bluebits
                     [ 0, 1, 1, 0, 1, 1, 0, 1 ],  # initialP -- initial state P[0...n+1] (arbitrary choice)
                     )

    # now do the decryption, save result as bs2
    bs2 = N2.Dec(cs)

    # test -- did we get our original plaintext back?
    if bs == bs2:
        print "Encryption/decryption successful!"
    else:
        print "Encryption/decryption failed."

testNash()




