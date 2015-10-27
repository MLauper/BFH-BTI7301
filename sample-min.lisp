(require :cl-fuse-meta-fs)
#+sbcl (require :sb-posix)
(require :iterate)

;(eval-when (:compile-toplevel :load-toplevel :execute)
;  (unexport 'iterate:terminate :iterate))

(use-package :cl-fuse-meta-fs)
(use-package :iterate)

(defvar *description* 
    (list
         (mk-file "README" "Hello here!")
	    )
     )
 
(setf cl-fuse::*break-on-errors* t)

(cl-fuse:fuse-complain "FS is ~s~%" *description*)

(cl-fuse-meta-fs:run-lisp-meta-fs *description*)
