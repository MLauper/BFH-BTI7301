;;;; BFH-BTI7301.lisp
(declaim (optimize (speed 0) (safety 3) (debug 3)))
(require 'cl-fuse)

;(in-package #:bfh-bti7301)
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













;;;; Cl-Fuse Tinkering
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
    '("a" "b" "c")
    
    
;;  (cond 
;;   ((or (null split-path) (equalp "" (car split-path)))
;;    '("symlinks" "same-name" "many-files" "myDir")) ;; <- Root Dir Content
;;   ((and
;;     (equalp "many-files" (car split-path))
;;     (null (cdr split-path)))
;;    (loop for i from 0 upto 100 collect (format nil "~a" i)))
;;   ((and
;;     (equalp "myDir" (car split-path))
;;     (null (cdr split-path))
;;     )
;;    '("one" "two" "three")) ;; <- New TestDir Content
;;   (t nil)
;;   )
  )

(defun file-size (split-path)
    (print "-------------------------- FILE-SIZE:")
  (cond
    (t 10)
   ((and (equalp (car split-path) "same-name") (null (cddr split-path)))
    (length (cadr split-path)))
   (t nil)))

(defun file-read (split-path size offset fh)
  (declare (ignore fh))
  (declare (ignore size))
  (declare (ignore offset))
  (print "                                FILE READ")
;;  (let* (
;;         (name (cadr split-path))
;;         )
  ;;        `(:offset 0 ,name)))
  '(:offset 0 "someString"))

(defun is-executable (split-path)
  (declare (ignore split-path))
  (print "-------------------------- IS-EXECUTABLE:")
  (= 1 2)
  )

(defun fuse-test ()
  (fuse-run '("none" "/tmp/mytest" "-d")
            :directoryp 'is-directory 
            :directory-content 'directory-content
;;            :symlink-target 'symlink-target
            :file-size 'file-size
            :file-read 'file-read
	    :file-executable-p 'is-executable
            ))

(fuse-test)


