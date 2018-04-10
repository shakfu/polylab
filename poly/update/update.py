#!/usr/bin/env python2

"""
A convenience script to update a pre-specified folder containing 
subversion, mercurial, bazaar, and/or git source folders

To use it: 
    - change the 'SRC' variable below to point to your source folder
    - name this script to something appropriate (I call it 'update')
    - put it into a directory on your PATH
"""

import os, sys

operations = {
    '.bzr': ['bzr pull', 'bzr update'],
    '.hg' : ['hg pull', 'hg update'],
    '.svn': ['svn update'],
    '.git': ['git pull']
} 

def update_sources(topdir):
    for folder in os.listdir(topdir):
        target = os.path.join(topdir, folder)
        if os.path.isdir(target):
            contents = os.listdir(target)
            for f in contents:
                if f in operations:
                    os.chdir(target)
                    cmds = operations[f]
                    print
                    print
                    print target, '-->',
                    for cmd in cmds:
                        print cmd
                        os.system(cmd)

if __name__ == '__main__':
    if len(sys.argv) >= 2:
        for dir in sys.argv[1:]:
            update_sources(dir)
    else:
        # define default source folder here
        SRC = '/home/user/src'
        update_sources(SRC)
