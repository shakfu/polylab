

(define (print-help)
    (println "Hello Command Line (native) command line arguments:")
    (println "   -help           -- to print help message")
    (println "   -i <file name>  -- to define the input file name")
    (println "   -o <file name>  -- to specify the output file name"))


(define (print-arg-list arg-list)
    (define (print-arg arg) 
        (display "Next command-line-argument: ")
        (display (car arg-list))
        (newline))
    (if (pair? arg-list)
        (let ()
            (print-arg (car arg-list))
            (print-arg-list (cdr arg-list)))))


(define (select option)
    (cond 
        ((= option 10) "hello")
        ((= option 20) "world")
        ((= option 30) "love")))

(define (process-file file)
    (display "file: ")
    (println file))

(define (process-out-file out-file)
    (process-file out-file))

(define (process-in-file in-file)
    (process-file in-file))

(let* ((args (command-line))
        (in-file (member "-i" args))
        (out-file (member "-o" args))
        (arg-hash-table (make-table)))
    ;; let's start by printing the command line arguments:
    (print-arg-list args)
    (if (member "-help" args) (print-help))
    ;; now print out input and output file names, if they are supplied:
    (if (and
            in-file
            (> (length in-file) 1))
            (process-in-file (cadr in-file)))
    (if (and
            out-file
            (> (length out-file) 1))
            (process-out-file (cadr out-file))))
