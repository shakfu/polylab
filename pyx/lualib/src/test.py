#!/usr/bin/env python
#import sys, os
#parent = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
#sys.path.insert(0, parent)


import lualib

def test():
    lualib.lua(file='script.lua')
    lualib.lua(code="print(1+2)")


def test_execute():
    lualib.execute()

def test_app():
    app = lualib.Application('settings.lua', ['screenWidth',
                                              'appName',
                                              'isfun',
                                             ])
    app.describe()
    app.name = "hello"
    print app.name

if __name__ == '__main__':
    test()
    test_execute()
    test_app()


