(use posix)

(define src-dir "/home/sa/src")

(define ops '(
    (".bzr" ["bzr pull" "bzr update"])
    (".hg"  ["hg pull" "hg update"])
    (".svn" ["svn update"])
    (".git" ["git pull"])))

(define (println label variable)
  (newline)
  (print label ": " variable))

(define (get-dirs path)
  (map (lambda (x) (string-append path "/" x)) 
       (directory path #t)))

(define (dispatch path sentinel)
  (println "project" path)
  (change-directory path)
  (for-each system (cadr (assoc sentinel ops))))

(define (update-project path)
  (let ((names (directory path #t))
        (sentinels (map car ops)))
    (for-each (lambda (i) 
                (if (member i sentinels)
                  (dispatch path i))) names)))

(define (update-root path)
  (let ((projects (get-dirs path)))
    (println "root" path)
    (for-each (lambda (p)
                (update-project p)) projects)))

(let ((args (command-line-arguments)))
  (if (< 1 (length args))
    (for-each (lambda (p)
                (update-root p)) (cdr args))
    (update-root src-dir)))


