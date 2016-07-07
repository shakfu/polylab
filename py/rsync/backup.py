#!/usr/bin/env python

"""
rsync [options] source destination

rsync -r -z -a -v -e "ssh -l <user>" 192.168.1.100:/<src>/ /<dst>

rsync -r -z -a -v -e "ssh -l <user>" --delete /<src>/ 192.168.1.100:/<dst>/

"""

import getpass, os
import pxssh


class Syncer(object):
    def __init__(self, src_host, user, password=None):
        self.src_host = src_host
        self.user = user
        self.password = password

    def rsync(self, src, dst):
        _cmd = 'rsync -r -z -a -v -e "ssh -l {user}" {src} {dst}'
        cmd = _cmd.format(user=self.user,
                          src=self.src_host+":"+src,
                          dst=dst)

    def ssh_command (self, command):
        import pexpect
        ssh_newkey = 'Are you sure you want to continue connecting'
        child = pexpect.spawn('ssh -l %s %s %s'%(self.user,
                                                 self.src_host,
                                                 command))
        i = child.expect([pexpect.TIMEOUT, ssh_newkey, 'password: '])
        if i == 0: # Timeout
            print 'ERROR!'
            print 'SSH could not login. Here is what SSH said:'
            print child.before, child.after
            return None
        if i == 1: # SSH does not have the public key. Just accept it.
            child.sendline ('yes')
            child.expect ('password: ')
            i = child.expect([pexpect.TIMEOUT, 'password: '])
            if i == 0: # Timeout
                print 'ERROR!'
                print 'SSH could not login. Here is what SSH said:'
                print child.before, child.after
                return None
        child.sendline(self.password)
        return child

    def send(self, s, cmd):
        s.sendline(cmd)  # run a command
        s.prompt()             # match the prompt
        print s.before         # print everything before the prompt.

    def test(self):
        import pxssh
        import getpass
        try:
            s = pxssh.pxssh()
            if not self.password:
                self.password = getpass.getpass('password: ')
            s.login (self.src_host, self.user, self.password)
            self.send(s, 'uptime')
            self.send(s, 'ls -l')
            self.send(s, 'df')
            self.send(s, 'cd /share/')
            self.send(s, 'ls -l')
            s.logout()
        except pxssh.ExceptionPxssh, e:
            print "pxssh failed on login."
            print str(e)

if __name__ == '__main__':
    app = Syncer('192.168.1.100', 'username')
    app.test()
