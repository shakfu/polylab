#!/usr/bin/csi -s

(use utils srfi-13)
(use simple-sha1)
(display (string->sha1sum (read-all)))
(define (f x) 
    (+ x x))
(newline)