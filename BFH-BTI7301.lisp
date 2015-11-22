;;;; BFH-BTI7301.lisp
(declaim (optimize (speed 0) (safety 3) (debug 3)))
(ql:quickload "cl-fuse")
(require 'cl-fuse)

;(in-package #:bfh-	bti7301)
(format t "Starting BTI7301 Project File")

(defclass sample-class ()
  (slotA
   slotB
   slotC))

(defclass sample-class2 ()
  (SomeString
   SomeInteger
   SomeList
   SomeObject)
  )

(defgeneric dumpObject (object)
  (:documentation "Dump a random object into a file)"))
(defmethod dumpObject(object)
;;  (with-open-file (out "/tmp/test1.txt" :direction :output :if-does-not-exist :create :if-exists :supersede)
;;    (format out (slot-value object 'slotA))
;;    (format t (slot-value object 'slotA))
;;    (defparameter slots (mapcar #'sb-pcl:slot-definition-name (sb-pcl:class-slots (class-of object))))
;;    (print slots)
  (defparameter slots (mapcar #'sb-pcl:slot-definition-name (sb-pcl:class-slots (class-of object))))
  ;;(print slots)
  (defparameter basePath (concatenate 'string "/tmp/newDir/" (write-to-string (class-of object)) "/" (write-to-string object) "/"))
  (dolist (slot slots)
    (defparameter filePath (concatenate 'string basePath (symbol-name slot) ".txt"))
    (print filePath)
    (with-open-file (out (ensure-directories-exist filePath) :direction :output :if-does-not-exist :create :if-exists :supersede)
      (print (slot-value object slot))
      (print out)
      (format out (write-to-string (slot-value object slot)))
      )
  )
)

(defparameter sample-instance (make-instance 'sample-class))
(setf (slot-value sample-instance 'slotA) "A")
(setf (slot-value sample-instance 'slotB) "B")
(setf (slot-value sample-instance 'slotC) "C")
(dumpObject sample-instance)

(defparameter sample-instance2 (make-instance 'sample-class))
(setf (slot-value sample-instance2 'slotA) 1)
(setf (slot-value sample-instance2 'slotB) 2)
(setf (slot-value sample-instance2 'slotC) 3)
(dumpObject sample-instance2)

(defparameter sample-instance3 (make-instance 'sample-class2))
(setf (slot-value sample-instance3 'SomeString) "Sample Content")
(setf (slot-value sample-instance3 'SomeInteger) 42)
(setf (slot-value sample-instance3 'SomeList) (list 1 2 3))
(setf (slot-value sample-instance3 'SomeObject) sample-instance2)
(dumpObject sample-instance3)

;;;;;;;;;;;;;;;;;;;;;;;; Get informed of new Instances
(defvar *objectInstances*)
(setf *objectInstances* (list))
(defmethod make-instance :after ((object sample-class) &rest initargs)
  (declare (ignore initargs))
  (print object)
  (append object *objectInstances*)
  ;;'(nil)
  )
(find-class 'sample-class)
(print *objectInstances*)

(sb-mop:class-direct-subclasses (find-class 'standard-object)) ;;; Retrieve all subclasses of the standard-object

(let ((lst ()))
  (do-all-symbols (s lst)
    (push s lst))
  (print lst))


(let ((lst ()) )
  (do-all-symbols (s lst)
    (push (class-of s) lst))
  ;(print lst)
  (with-open-file (stream "/tmp/symbolDump.txt" :direction :output)
    (princ lst stream))
  )

(class-of sample-instance)



;;;;;;;;;;;;;;;;;;;;;;;; Cl-Fuse Tinkering
(ql:quickload "cl-fuse")
(ensure-directories-exist "/tmp/testFuseEmpty/")
(cl-fuse:fuse-run '("none" "-d" "/tmp/testFuseEmpty"))

(require 'cl-fuse)
(require 'cffi)
(require 'cl-utilities)

(use-package 'cl-fuse)
(use-package 'cffi)
(use-package 'cl-utilities)

(defun is-directory (split-path)
  (print "-------------------------- IS-DIRECTORY:")
  (print (concatenate 'string "Split-Path Length: " (write-to-string (length split-path))))
  (print split-path)
  (or
   (null split-path) ;; Return true for root directory
   (equalp (car split-path) "") ;; Return true for empty directory
   (> 3 (length split-path)) ;; Reutrn true for first and second stage
   ) ;; Return false for any other element
)

(defun symlink-target (split-path)
	(print "-------------------------- SYMLINK-TARGET:")
	(cond 
		((equalp (car split-path) "symlinks") (cdr split-path))
		((equalp (car split-path) "many-files") (cdr split-path))
		(t nil)
	)
)

(defun directory-content (split-path)
  (print "-------------------------- DIRECTORY-CONTENT:")

	(defparameter *fuse-display-classes* (remove-duplicates (mapcar #'(lambda (object) (write-to-string (class-of object))) *fuse-objects*) :test #'equal))
	(defparameter *fuse-display-class-instances*  (remove-duplicates (mapcar #'(lambda (object) (write-to-string object)) (remove-if-not #'(lambda (object) (cond ((equal (first split-path) (write-to-string (class-of object))) t) (t nil))) *fuse-objects*)) :test #'equal))
	;;(defparameter *fuse-display-class-instance-slots* '("x" "y" "z"))
	;;(defparameter *fuse-display-class-instance-slots*  (remove-duplicates (mapcar #'(lambda (object) (write-to-string object)) (remove-if-not #'(lambda (object) (cond ((equal (second split-path) (write-to-string object)) t) (t nil))) *fuse-objects*)) :test #'equal))
	;;(defparameter *fuse-display-class-instance-slots* (mapcar #'(lambda (slot) (concatenate 'string (write-to-string slot) "_")) (mapcar #'sb-pcl:slot-definition-name (sb-pcl:class-slots (class-of (first (remove-if-not #'(lambda (object) (cond ((equal (second split-path) (write-to-string object)) t) (t nil))) *fuse-objects*)))))))
	(defparameter *fuse-dispaly-class-instance* (first (remove-if-not #'(lambda (object) (cond ((equal (second split-path) (write-to-string object)) t) (t nil))) *fuse-objects*)))
	(defparameter *fuse-display-class-instance-slots* (mapcar #'(lambda (slot) (concatenate 'string (write-to-string slot) "_")) (mapcar #'sb-pcl:slot-definition-name (sb-pcl:class-slots (class-of (first (remove-if-not #'(lambda (object) (cond ((equal (second split-path) (write-to-string object)) t) (t nil))) *fuse-objects*)))))))
	
	
	(cond
	  ((= (length split-path) 0) *fuse-display-classes*) ;; Dump Classes on the first layer
	  ((= (length split-path) 1) *fuse-display-class-instances*) ;; Dump Instances on the second layer
	  ((= (length split-path) 2) *fuse-display-class-instance-slots*) ;; Dump slots on the third layer
	  (t '("a" "b" "c"))
	  )
)

(defun file-size (split-path)
	(print "-------------------------- FILE-SIZE:")
	(cond
		(t 10)
		((and (equalp (car split-path) "same-name") (null (cddr split-path)))
			(length (cadr split-path)))
		(t nil))
)

(defun file-read (split-path size offset fh)
	(declare (ignore fh))
	(declare (ignore size))
	(declare (ignore offset))
	(print "-------------------------- FILE-READ:")
;;  (let* (
;;         (name (cadr split-path))
;;         )
  ;;        `(:offset 0 ,name)))
	'(:offset 0 "someString")
)

(defun is-executable (split-path)
  (declare (ignore split-path))
  (print "-------------------------- IS-EXECUTABLE:")
  (= 1 2)
)
  
(defun is-symlink (split-path)
	(print "-------------------------- IS-SYMLINK:")
	(= 1 2)
)

(defun file-write-whole (split-path data)
	(print "-------------------------- WRITE-FILE")
	nil
)

(defun is-writeable (split-path)
	(declare (ignore split-path))
	(print "-------------------------- IS-writeable:")
	(= 1 2)
)

(defun file-flush (path fh)
	(print "-------------------------- FILE-FLUSH")
	0
)

(defun file-release (path flags)
	(print "-------------------------- FILE-RELEASE")
	0
)
  
(defun file-truncate (path offset)
	(print "-------------------------- FILE-TRUNCATE")
	0
)
  
(defun file-create (path mode dev)
	(print "-------------------------- FILE-CREATE")
	nil
)
 
(defun dir-create (path mode)
	(print "-------------------------- DIR-CREATE")
	(- error-EACCES)
)

(defun file-remove (path)
	(print "-------------------------- FILE-REMOVE")
	(- error-EACCES)
)

(defun dir-remove (path)
	(print "-------------------------- DIR-REMOVE")
	(- error-EACCES)
)

(defun symlink (path content)
	(print "-------------------------- SYMLINK")
	(- error-EACCES)
)
 
(defun fuse-test ()
  (defparameter *fuse-objects* nil)
  (fuse-run '("none" "/tmp/mytest" "-d")
	    :directoryp 'is-directory
	    :directory-content 'directory-content
	    :symlink-target 'symlink-target
	    :file-size 'file-size
	    :file-read 'file-read
	    :file-executable-p 'is-executable
	    :symlinkp 'is-symlink
	    :file-write-whole 'file-write-whole
	    :file-writeable-p 'is-writeable
	    :file-flush 'file-flush
	    :file-release 'file-release
	    :truncate 'file-truncate
	    :file-create 'file-create
	    :mkdir 'dir-create
	    :unlink 'file-remove
	    :rmdir  'dir-remove
	    :symlink 'symlink
	    )
  )

(defun add-fuse-object (object)
  (setq *fuse-objects* (append *fuse-objects* object))
  )

(fuse-test)

(defparameter sample-instance3 (make-instance 'sample-class2))
(setf (slot-value sample-instance3 'SomeString) "Sample Content")
(setf (slot-value sample-instance3 'SomeInteger) 42)
(setf (slot-value sample-instance3 'SomeList) (list 1 2 3))
(setf (slot-value sample-instance3 'SomeObject) sample-instance2)
(add-fuse-object (list sample-instance3))

(add-fuse-object (list sample-instance sample-instance2))

(defclass sample-class3 ()
  (SomeString
   SomeInteger
   SomeList
   SomeObject)
  )
(add-fuse-object (list (make-instance 'sample-class3)))
