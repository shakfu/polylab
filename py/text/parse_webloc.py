from xml.etree.ElementTree import ElementTree
import os


TARGET = "./Links"

for p in os.listdir(TARGET):
	if p.endswith(".webloc"):
		path = os.path.join(TARGET, p)
		xml = ElementTree().parse(path)
		string = xml.find("dict/string")
        print p, "->", string.text



