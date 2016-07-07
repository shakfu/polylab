;(import foreign)

(print "hello world")

;; area : number -> number -> number
(define (area x y)
  (* x y))

(define absolute
  (lambda (n)
    (if (< n 0)
        (- 0 n)
        n)))

(define (fact n)
  (if (<= n 1) 1 (* n (fact (- n 1)))))

;; hash maps
(define dict '(
  (a 100)
  (b 10)
  (c 200)))

;; get it by key
(print (assoc 'a dict))

;; functional programming
(define (f x) (+ x 1))
(map f '(1 2 3 4))

(define (f x y) (+ x y))
(foldr f 0 '(1 2 3 4 5))

;; hello there
(define (f x y)
  (+ x y))


(define (greater? x y)
    (x > y))

;; list of funcs
(define fns (list area absolute fact f greater?))
(print ((car fns) 10 20))

; with eval
(define funcs '(area absolute fact f greater?))
(print ((eval (car funcs)) 10 20))


(call/cc
  (lambda (k)
    (* 5 4))) ; -> 20

;; Here we are going to create a new macro called "return-me"
;; that will simply return in a list whatever is passed to it
(define-syntax return-me
  (syntax-rules () ; Any symbols placed in this list will be ignored
    [(return-me body ...) ; This is what the arguments should look like
        (list body ...)])) ; And this is how they should be transformed

(define-syntax debug
  (syntax-rules ()
    ((_ var)
     (print 'var ": " var))))
    
(define myvar '(this is a list))
  
(debug myvar)


;; a macro that creates a new way of defining functions
(define-syntax function
    (syntax-rules ()
        [(function name (args ...) body ...)
            (define (name args ...) body ...)]))

;; eg
(define (maxsquared x y)
    (if (> x y)
        (* x x)
        (* y y)))

(define (maxq x y z)
  (cond 
    ((< x y) z)
    ((< y z) x)
    (else (+ x y z))))

(print "maxquared\n")
(print (maxsquared 10 32))

;; Examples
(print (return-me "Hello" "-" "World" "\n"))

(function hello (arg) (print arg))

(hello "WORLD")

;; PI
(define PI 3.14159)

(print (number->string PI))
(newline)
