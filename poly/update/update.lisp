;; a lisp version of update

(defconstant SRC-DIR "/home/sa/src")

;; portable directory functions

(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p (p)
  (and
    (not (component-present-p (pathname-name p)))
    (not (component-present-p (pathname-type p)))
    p))

(defun pathname-as-directory (name)
  (let ((pathname (pathname-name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames"))
    (if (not (directory-pathname-p name))
      (make-pathname
        :directory (append (or (pathname-directory pathname) (list :relative)) (list (file-namestring pathname)))
        :name nil
        :type nil
        :defaults pathname)
      pathname)))

(defun get-dirs (path)
  (let ((target (concatenate 'string path "/*")))
    (directory target)))

(defun update-root (path)
  (loop for p in (get-dirs path) do
	 (format t "proj: ~S ~%" p )))
     ;(loop for d in (directory p) do
           ;(if (not (prob-file d))
     ;        (format t "dir: ~S ~%" d))))
     ;(probe-file i)
	 ;(print i)))

;(defun update-project (path)
;  (run-program "ls" nil)
;  (format t "dir: "))

(update-root SRC-DIR)

