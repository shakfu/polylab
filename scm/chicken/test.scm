#!/usr/bin/csi -s

(use simple-sha1)
(display (string->sha1sum "hello world"))
(newline)

