#+sbcl (require :sb-posix)
;;(require :iterate)
;;(require :cl-fuse-meta-fs)

(asdf:oos 'asdf:load-op 'cl-fuse-meta-fs)

;(eval-when (:compile-toplevel :load-toplevel :execute)
;  (unexport 'iterate:terminate :iterate))

(use-package :cl-fuse-meta-fs)
(use-package :iterate)

(defvar *description* (list (cl-fuse-meta-fs:mk-file "README" "Hello here!")))
 
(setf cl-fuse::*break-on-errors* t)

(cl-fuse:fuse-complain "FS is ~s~%" *description*)

(cl-fuse-meta-fs:run-lisp-meta-fs *description*)

