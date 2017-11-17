# Steganography

# Table of Contents
  * [WavSteg](#WavSteg)

## WavSteg
WavSteg uses least significant bit steganography to hide a file in the samples
of a .wav file.

For each sample in the audio file, we overwrite the least significant bits with
the data from our file.

### How to use
WavSteg requires Python 3

Run WavSteg with the following command line arguments:

    -h, --hide        To hide data in a sound file
    -r, --recover     To recover data from a sound file
    -s, --sound=      Path to a .wav file
    -f, --file=       Path to a file to hide in the sound file
    -o, --output=     Path to an output file
    -n, --LSBs=       How many LSBs to use
    -b, --bytes=      How many bytes to recover from the sound file
    --help            Display this message

Example:

    WavSteg.py -h -s sound.wav -f file.txt -o sound_steg.wav -n 1
	# OR
	WavSteg.py -r -s sound_steg.wav -o output.txt -n 1 -b 1000

### Hiding Data
Hiding data requires the arguments -h, -s, -f, -o, and -n.

The following command would hide the contents of file.txt into sound.wav and
save the result as sound_steg.wav. The command also outputs how many bytes have
been used out of a theoretical maximum. In practice, the maximum amount of data
we can hide may be slightly lower than what is displayed because we skip
samples that have the smallest possible value (for most files, this never
occurs).

Example:

    $ WavSteg.py -h -s sound.wav -f file.txt -o sound_steg.wav -n 1
	Using 1000 B out of 2000 B

If you attempt to hide too much data, WavSteg will raise a ValueError and
print the minimum number of LSBs required to hide your data.

### Recovering Data
Recovering data requires the arguments -r, -s, -o, -n, and -b

The following command would recover the hidden data from sound_steg.wav and
save it as output.txt. This requires the size in bytes of the hidden data to
be accurate or the result may be too short or contain extraneous data.

Example:

    $ WavSteg.py -r -s sound_steg.wav -o output.txt -n 1 -b 1000
