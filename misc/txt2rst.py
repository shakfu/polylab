import re


tst = """

Hello “there” my friend’s world.

oPre-numbered Insurance cover notes / policy docs etc for ABC.
oDaily villa rental register, room occupancy register, man-day sheets, meals register, pre-numbered function order sheet, miscellaneous service order sheet for BBC and Zoo Divisions.
oPre-numbered job completion Report, Wk. order completion report, Equipment hire sheet, Technician hire report etc. for Acme, Crop.
oContract completion certificate, Running A/C billing doc, WIP statements, Variation order claims doc etc for CD.


1.Helo there my friend
2.bad body


A)hello there my friend
c)HELLO there
"""

substitutions = [
    # bullet to hyphen e.g oHello
    (r'^o(?=[a-z|A-Z])', '- '),

    # compressed ordered enumeration e.g 1.Hello
    (r'^\d\.(?=[a-z|A-Z])', '#. '),

    # compressed ordered alpha enumeration e.g a.Hello
    (r'^[a-z|A-Z]\.(?=[a-z|A-Z])', '#. '),


    # compressed ordered enumeration with bracket e.g a)Hello
    (r'^[a-z|A-Z]\)(?=[a-z|A-Z])', '#. '),

    # compressed ordered alpha enumeration with bracket e.g (a)Hello
    (r'^\([a-z|A-Z]\)(?=[a-z|A-Z])', '#. '),

    # compressed orderlist without separator
    (r'^\d+(?=[a-z|A-Z])', '#. '),

    # clean whitespace at beginning of line e.g '  Hello'
    #~ (r'^\s+(?=[a-z|A-Z])', ''),

    # hyphen to hyphen space e.g -Hello
    (r'^-(?=[a-z|A-Z])', '- '),


    # compressed ordered enumeration with bracket e.g a)Hello
    (r'^[0-9]\)(?=[a-z|A-Z])', '#. '),

    # ---------------------------------------------------
    # small subs
    # ---------------------------------------------------

    # curved apostrophe
    (r'’', "'"),

    # left curved quotation
    (r'“', '"'),

    # right curved quotation
    (r'”', '"'),

]


def clean(txt):
    for pattern, replacement in substitutions:
        txt = re.sub(pattern, replacement, txt, flags=re.MULTILINE)
    return txt
