(define (print-help)
    (display "Hello Command Line (native) command line arguments:")
    (newline)
    (display "   -help           -- to print help message")
    (newline)
    (display "   -i <file name>  -- to define the input file name")
    (newline)
    (display "   -o <file name>  -- to specify the output file name")
    (newline))

(define (print-arg-list arg-list)
    (define (print-arg arg) 
        (display "Next command-line-argument: ")
        (display (car arg-list))
        (newline))
    (if (pair? arg-list)
            (let ()
                (print-arg (car arg-list))
                (print-arg-list (cdr arg-list)))))

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
            (let ()
                (display "Input file: ") (display (cadr in-file)) (newline)))
    (if (and
             out-file
             (> (length out-file) 1))
            (let ()
                (display "Output file: ") (display (cadr out-file)) (newline))))
