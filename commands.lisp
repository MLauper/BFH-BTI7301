sbcl
umount /tmp/mytest

(defclass Loooal ()
	(SomeString SomeInteger SomeList SomeObject))
(add-fuse-object (list (make-instance 'Loooal)))


(file-size nil)
(directory-content nil)

(defun run-fuse (basePath)
  (defparameter *fuse-base-path* (cdr (split-sequence:split-sequence #\/ basePath)))
  (defparameter *fuse-objects* nil)
  (ensure-directories-exist "/tmp/mytest")
  (directory-content nil)
  
  (*fuse-objects*))
  
  (run-fuse "/tmp/mytest")
  
  
(format t "hallo ~a" *fuse-objects*)


()

(do-urlencode:urldecode '"%23%3CSTANDARD-CLASS%20THEFUSEPROJECT%3A%3ASAMPLE-CLASS2%3E")

(cl-json:encode-json (directory-content nil))
(directory-content (split-sequence:SPLIT-SEQUENCE #\/ "asdasd/asdasdaasd/asdasd"))