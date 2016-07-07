(load "~~/postgresql/postgresql")
(include "~~/postgresql/postgresql#.scm")

(define (println s)
    (display s)
    (newline))

(println "hello world")

(define (func)
    (println "works"))

;(with-connection 
;    (list database: "sa"
;          username: "sa"
;          password: "sa")
;    (display(execute "select * from users")))

(with-connection
 (list database: "sa"
       username: "sa"
       password: "sa")
  (lambda () 
   (execute "SELECT key, val FROM key_value_table WHERE key like $1"
            arguments: (list "%a%")
            initial-value: '()
            reducer: (lambda (state key value) (cons (cons key value) state)))))