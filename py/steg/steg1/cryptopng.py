#!/usr/bin/env python
#-*- coding: utf-8 -*-
# 
# CryptoPNG 0.1
#
# Reading/writing secret text from/to image. Every 3 bits are encoded to
# 3x3 pixelgrid. Central pixel keeps the information, each channel of RGB
# represents one bit. If at least one of a neighbouring pixels color channel
# is same as color channel of central pixel, value is True, else False.
#
# author: Vojtech Smejkal (smejkalv@gmail.com), 2008
#

from PIL import Image
import sys


# 6-bite character set
chars = 'abcdefghijklmnopqrstuvwxyz0123456789 .,:;$?!+-*/<=>()#&@%°"^_\n'

# Help string
help = """Reading or writing secret text from/to PNG image with minimal change.
Examples:
    echo text | cryptopng image    Write secret text to image.
    cryptopng image                Read text from image.
    cryptopng --capacity image     Print count of chars image can keep.
    cryptopng --help               Print this help."""


def capacity(filename):
	'''
	Maximal count of characters that can be save to image.
	'''	
	img = Image.open(filename)
	width, height = img.getbbox()[2:]
	
	return int(width / 3) * int(height / 3) * 3 / 6 - 2


def read(filename):
	'''
	Decode hidden text from image.
	'''
	# Load image
	img = Image.open(filename)
	width, height = img.getbbox()[2:]
	binary = []
	
	# Read binary data from image
	for x in range(0, width - 2, 3):
		if len(binary) % 6 == 0 and binary[-6:len(binary)] == [False] * 6:
			break
		
		for y in range(0, height - 2, 3):
			if len(binary) % 6 == 0 and binary[-6:len(binary)] == [False] * 6:
				break
			
			# Neighbouring pixels
			neigh = [0, 0, 0, 0]
			neigh[0] = img.getpixel((x + 1, y))
			neigh[1] = img.getpixel((x + 2, y + 1))
			neigh[2] = img.getpixel((x + 1, y + 2))
			neigh[3] = img.getpixel((x, y + 1))
			
			pxl = img.getpixel((x + 1, y + 1))
			
			# Compare RGB channels
			for i in range(3):
				same = False
				
				# Compare pixel with neighbours
				for n in neigh:
					if n[i] == pxl[i]:
						same = True
						break
				
				# Same color -> False
				if same:
					binary += [False]
				# Different color -> True
				else:
					binary += [True]

	text = ''
	offset = 0
		
	# Decode binary data to text
	while offset < len(binary):
		char = -1
		
		# Read one character
		for n in range(5, -1, -1):
			if binary[offset] == True:
				char += 2 ** n
			offset += 1
		
		if -1 < char < len(chars):
			text += chars[char]
	
	return text


def write(filename, text):
	'''
	Encode text to image.
	'''
	# Load image
	img = Image.open(filename)
	width, height = img.getbbox()[2:]
	
	# Check allowed characters
	for ch in text:
		if ch not in chars:
			print('Error: text contains illegal character %s' % ch)
			exit()

	binary = []

	# Encode text to binary data
	for ch in text:
		bin_char = [False] * 6
		dec = chars.index(ch) + 1
	
		for n in range(5, -1, -1):
			if dec >= 2 ** n:
				dec -= 2 ** n
				bin_char[-n - 1] = True
	
		binary += bin_char

	# Add EOF char
	binary += [False] * 6

	# Check text size
	if len(binary) > int(width / 3) * int(height / 3) * 3:
		print('Error: text is too long')
		exit()


	offset = 0
	
	# Write binary data to image
	for x in range(0, width - 2, 3):
		if offset == len(binary):
			break
	
		for y in range(0, height - 2, 3):
			if offset == len(binary):
				break
		
			neigh = [0, 0, 0, 0] # Neighbouring pixels
			neigh[0] = list(img.getpixel((x + 1, y)))
			neigh[1] = list(img.getpixel((x + 2, y + 1)))
			neigh[2] = list(img.getpixel((x + 1, y + 2)))
			neigh[3] = list(img.getpixel((x, y + 1)))
		
			pxl = list(img.getpixel((x + 1, y + 1)))
			sim = neigh[0][:]
		
			# Get the most similar neighbour
			for i in range(1, 4):
				if abs(sum(sim) - sum(pxl)) > abs(sum(neigh[i]) - sum(pxl)):
					sim = neigh[i][:]
			pxl = sim

			# Generate new RGB colors
			for i in range(3):
				# True - different color
				if binary[offset] == True:
					# Make neighbours different
					for n in neigh:
						if n[i] == pxl[i]:
							if n[i] < 255:
								n[i] = n[i] + 1
							else:
								n[i] = n[i] - 1

				offset += 1
	
			img.putpixel((x + 1, y + 1), tuple(pxl))
			img.putpixel((x + 1, y), tuple(neigh[0]))
			img.putpixel((x + 2, y + 1), tuple(neigh[1]))
			img.putpixel((x + 1, y + 2), tuple(neigh[2]))
			img.putpixel((x, y + 1), tuple(neigh[3]))

	# Save image
	img.save(filename)


if __name__ == '__main__':
	# Print help
	if len(sys.argv) == 1:
		print(help)
	
	elif sys.argv[1] == '--help':
		print(help)
	
	# Print max. image capacity
	elif sys.argv[1] == '--capacity':
		c = capacity(sys.argv[2])
		print('Capacity of %s is %s chars' % (sys.argv[2], c))

	# Encode text to image
	elif not sys.stdin.isatty():
		text = sys.stdin.read().lower()
		write(sys.argv[1], text)
		print('Text successfully written to image')

	# Decode text from image
	else:
		print(read(sys.argv[1]))

