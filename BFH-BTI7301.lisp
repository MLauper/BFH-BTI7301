;;;; BFH-BTI7301.lisp
;;;; Test
(declaim (optimize (speed 0)
		   (safety 3)
		   (debug 3)))
(ql:quickload "cl-fuse")
(require 'cl-fuse)

					;(in-package #:bfh-	bti7301)
(format t "Starting BTI7301 Project File")

(defclass sample-class ()
  (slotA slotB slotC))

(defclass sample-class2 ()
  (SomeString SomeInteger SomeList SomeObject))

(defgeneric dumpObject
  (object)
  (:documentation "Dump a random object into a file)"))
(defmethod dumpObject
  (object)
  ;;  (with-open-file (out "/tmp/test1.txt" :direction :output :if-does-not-exist :create :if-exists :supersede)
  ;;    (format out (slot-value object 'slotA))
  ;;    (format t (slot-value object 'slotA))
  ;;    (defparameter slots (mapcar #'sb-pcl:slot-definition-name (sb-pcl:class-slots (class-of object))))
  ;;    (print slots)
  (defparameter slots (mapcar #'sb-pcl:slot-definition-name (sb-pcl:class-slots (class-of object))))
  ;;(print slots)
  (defparameter basePath (concatenate 'string
				      "/tmp/newDir/"
				      (write-to-string (class-of object))
				      "/"
				      (write-to-string object)
				      "/"))
  (dolist (slot slots)
    (defparameter filePath (concatenate 'string
					basePath
					(symbol-name slot)
					".txt"))
    (print filePath)
    (with-open-file (out (ensure-directories-exist filePath)
			 :direction :output
			 :if-does-not-exist :create
			 :if-exists :supersede)
		    (print (slot-value object slot))
		    (print out)
		    (format out
			    (write-to-string (slot-value object slot))))))

(defparameter sample-instance (make-instance 'sample-class))
(setf (slot-value sample-instance 'slotA) "A")
(setf (slot-value sample-instance 'slotB) "B")
(setf (slot-value sample-instance 'slotC) "C")
;;(dumpObject sample-instance)

(defparameter sample-instance2 (make-instance 'sample-class))
(setf (slot-value sample-instance2 'slotA) 1)
(setf (slot-value sample-instance2 'slotB) 2)
(setf (slot-value sample-instance2 'slotC) 3)
;;(dumpObject sample-instance2)

(defparameter sample-instance3 (make-instance 'sample-class2))
(setf (slot-value sample-instance3 'SomeString) "Sample Content")
(setf (slot-value sample-instance3 'SomeInteger) 42)
(setf (slot-value sample-instance3 'SomeList) (list 1 2 3))
(setf (slot-value sample-instance3 'SomeObject) sample-instance2)
;;(dumpObject sample-instance3)

;;;;;;;;;;;;;;;;;;;;;;;; Get informed of new Instances
(defvar *objectInstances*)
(setf *objectInstances* (list))
(defmethod make-instance
  :after ((object sample-class) &rest
	  initargs)(declare (ignore initargs))(print object)(append object *objectInstances*))
(find-class 'sample-class)
;;(print *objectInstances*)

;;(sb-mop:class-direct-subclasses (find-class 'standard-object)) ;;; Retrieve all subclasses of the standard-object

;;(let ((lst ()))
;;  (do-all-symbols (s lst)
;;    (push s lst))
;;  (print lst))
;;
;;
;;(let ((lst ()) )
;;  (do-all-symbols (s lst)
;;    (push (class-of s) lst))
;;  ;(print lst)
;;  (with-open-file (stream "/tmp/symbolDump.txt" :direction :output)
;;    (princ lst stream))
;;  )
;;
;;(class-of sample-instance)



;;;;;;;;;;;;;;;;;;;;;;;; Cl-Fuse Tinkering
(ql:quickload "cl-fuse")
(ql:quickload :split-sequence) 
;;(ensure-directories-exist "/tmp/testFuseEmpty/")
;;(cl-fuse:fuse-run '("none" "-d" "/tmp/testFuseEmpty"))

(require 'cl-fuse)
(require 'cffi)
(require 'cl-utilities)


(use-package 'cl-fuse)
(use-package 'cffi)
(use-package 'cl-utilities)

(defun is-directory (split-path)
  (print "-------------------------- IS-DIRECTORY:")
  (defparameter *dir-split-path* split-path)
  ;;(print (concatenate 'string "Split-Path Length: " (write-to-string (length *dir-split-path*))))
  (defparameter *dir-current-object* (first (remove-if-not #'(lambda (object)
							   (cond
							    ((equal (second split-path) (write-to-string object)) t)
							    (t nil)))
						       *fuse-objects*)))
	
  (or (null *dir-split-path*) ;; Return true for root directory
      (equalp (car *dir-split-path*) "") ;; Return true for empty directory
      (> 3 (length *dir-split-path*)) ;; Reutrn true for first and second stage
		(and 
			(= (length *dir-split-path*) 3)
			(slot-boundp *dir-current-object* (intern (car (last split-path)))) ;; Return false for unbound slot values
			(equalp (write-to-string (class-of (slot-value *dir-current-object* (intern (car (last split-path)))))) "#<BUILT-IN-CLASS COMMON-LISP:CONS>") ;; Return true for Lists
		)
		(and
				(> (length *dir-split-path*) 3)
				(slot-boundp *dir-current-object* (intern (car (cdr (cdr *dir-split-path*)))))
				(progn
					(defparameter *dir-list-path* (cdr (cdr *dir-split-path*)))
					(defparameter *dir-current-list* (slot-value *dir-current-object* (intern (car *dir-list-path*))))
					(defparameter *dir-list-path* (cdr *dir-list-path*))
					
					(loop while (not (= 1 (length *dir-list-path*))) do
						(progn
							(defparameter *dir-current-list* (nth (parse-integer (car *dir-list-path*)) *dir-current-list*))
							(defparameter *dir-list-path* (cdr *dir-list-path*))
						)
					)

					(equalp (write-to-string (class-of (nth (parse-integer (car *dir-list-path*)) *dir-current-list*))) "#<BUILT-IN-CLASS COMMON-LISP:CONS>")
				
				)	
		)
  ) ;; Return false for any other element
)

(defun symlink-target (split-path)
  (print "-------------------------- SYMLINK-TARGET:")
	
	(defparameter *split-path* split-path)
	(defparameter *list-path* (cdr (cdr *split-path*)))
	(defparameter *current-object* (first (remove-if-not #'(lambda (object) (cond ((equal (second *split-path*) (write-to-string object)) t) (t nil))) *fuse-objects*)))
	(defparameter *fuse-symlink* nil)
	(when (slot-boundp *current-object* (intern (car *list-path*)))
	(if (cdr *list-path*)
	(progn
		(defparameter *current-list* (slot-value *current-object* (intern (car *list-path*))))
		(defparameter *list-path* (cdr *list-path*))
		(loop while *list-path* do
			(progn
				(defparameter *current-list* (nth (parse-integer (car *list-path*)) *current-list*))
				(defparameter *list-path* (cdr *list-path*))
			)
		)
		(defparameter *fuse-symlink* *current-list*)
	)
	(defparameter *fuse-symlink* (slot-value *current-object* (intern (first (cdr (cdr *split-path*))))))
	))
	
	(defparameter *fuse-symlink-class* (remove-duplicates (mapcar #'(lambda (object) (write-to-string (class-of object))) (remove-if-not #'(lambda (object) (cond ((equal (write-to-string *fuse-symlink*) (write-to-string  object)) t) (t nil))) *fuse-objects*)) :test #'equal))
	(defparameter *fuse-symlink-instance* (remove-duplicates (mapcar #'(lambda (object) (write-to-string object)) (remove-if-not #'(lambda (object) (cond ((equal (write-to-string *fuse-symlink*) (write-to-string  object)) t) (t nil))) *fuse-objects*)) :test #'equal))
	
	(append *fuse-base-path* *fuse-symlink-class* *fuse-symlink-instance*)
	
)

(defun directory-content (split-path)
  (print "-------------------------- DIRECTORY-CONTENT:")

  (cond
   ((= (length split-path) 0)
    (remove-duplicates (mapcar #'(lambda (object)
				   (write-to-string (class-of object)))
			       *fuse-objects*)
		       :test #'equal)) ;; Dump Classes on the first layer
   ((= (length split-path) 1)
    (remove-duplicates (mapcar #'(lambda (object)
				   (write-to-string object))
			       (remove-if-not #'(lambda (object)
						  (cond
						   ((equal (first split-path) (write-to-string (class-of object))) t)
						   (t nil)))
					      *fuse-objects*))
		       :test #'equal)) ;; Dump Instances on the second layer
   ((= (length split-path) 2)
    (mapcar #'(lambda (slot)
		(write-to-string slot))
	    (mapcar #'sb-pcl:slot-definition-name (sb-pcl:class-slots (class-of (first (remove-if-not #'(lambda (object)
													  (cond
													   ((equal (second split-path) (write-to-string object)) t)
													   (t nil)))
												      *fuse-objects*))))))) ;; Dump slots on the third layer
   ((> (length split-path) 2) 
	   
	   (defparameter *split-path* split-path) ;; DEGUBGGING
	   (defparameter *list-path* (cdr (cdr *split-path*)))
	   (defparameter *current-object* (first (remove-if-not #'(lambda (object)
							   (cond
							    ((equal (second *split-path*) (write-to-string object)) t)
							    (t nil)))
						       *fuse-objects*)))
		(defparameter *current-list* (slot-value *current-object* (intern (car *list-path*))))
		(defparameter *list-path* (cdr *list-path*))
	  (loop while *list-path* do
			(progn
				(defparameter *current-list* (nth (parse-integer (car *list-path*)) *current-list*))
				(defparameter *list-path* (cdr *list-path*))
			)
	  )
   
	(defparameter *list-objects* (list))
   (loop for i from 0 to (- (length *current-list*) 1) do (defparameter *list-objects* (append *list-objects* (list (write-to-string i)))))
	*list-objects*
	   
	   ) ;; Dump Integer-List for size of list
   
   
   
   
   (t '("NOT_IMPLEMENTED"))))

(defun file-size (split-path)
  (print "-------------------------- FILE-SIZE:")
  (cond
   (t 10)
   ((and (equalp (car split-path)
		 "same-name")
	 (null (cddr split-path)))
    (length (cadr split-path)))
   (t nil)))

(defun file-read (split-path size offset fh)
  (declare (ignore fh))
  (declare (ignore size))
  (declare (ignore offset))
  (print "-------------------------- FILE-READ:")
  ;; Always asuming, that only "true" files are read -> NO directories and no symlinks
  
  	(defparameter *split-path* split-path)
	(defparameter *list-path* (cdr (cdr *split-path*)))
	(defparameter *current-object* (first (remove-if-not #'(lambda (object) (cond ((equal (second *split-path*) (write-to-string object)) t) (t nil))) *fuse-objects*)))
	(defparameter *fuse-symbol-content* nil)
	(when (slot-boundp *current-object* (intern (car *list-path*)))
	(if (cdr *list-path*)
	(progn
		(defparameter *current-list* (slot-value *current-object* (intern (car *list-path*))))
		(defparameter *list-path* (cdr *list-path*))
		(loop while *list-path* do
			(progn
				(defparameter *current-list* (nth (parse-integer (car *list-path*)) *current-list*))
				(defparameter *list-path* (cdr *list-path*))
			)
		)
		(defparameter *fuse-symbol-content* *current-list*)
	)
	(defparameter *fuse-symbol-content* (slot-value *current-object* (intern (first (cdr (cdr *split-path*))))))
	))
	
	(append '(:offset 0) (list (write-to-string *fuse-symbol-content*)))
  )

(defun is-executable (split-path)
  (declare (ignore split-path))
  (print "-------------------------- IS-EXECUTABLE:")
  (= 1 2))

(defun is-symlink (split-path)
  (print "-------------------------- IS-SYMLINK:")
  (and 
	(> (length split-path) 2)
	(progn
		(defparameter *split-path* split-path)
		(defparameter *list-path* (cdr (cdr *split-path*)))
		(defparameter *current-object* (first (remove-if-not #'(lambda (object) (cond ((equal (second *split-path*) (write-to-string object)) t) (t nil))) *fuse-objects*)))
		(defparameter *fuse-symlink* nil)
		(when (slot-boundp *current-object* (intern (car *list-path*)))
		(if (cdr *list-path*)
		(progn
			(defparameter *current-list* (slot-value *current-object* (intern (car *list-path*))))
			(defparameter *list-path* (cdr *list-path*))
			(loop while *list-path* do
				(progn
					(defparameter *current-list* (nth (parse-integer (car *list-path*)) *current-list*))
					(defparameter *list-path* (cdr *list-path*))
				)
			)
			(defparameter *fuse-symlink* *current-list*)
		)
		(defparameter *fuse-symlink* (slot-value *current-object* (intern (first (cdr (cdr *split-path*))))))
		))
		
		(if (remove-duplicates (mapcar #'(lambda (object) (write-to-string object)) (remove-if-not #'(lambda (object) (cond ((equal (write-to-string *fuse-symlink*) (write-to-string  object)) t) (t nil))) *fuse-objects*)) :test #'equal) t)
		
	)
  )
)

(defun file-write-whole (split-path data)
  (print "-------------------------- WRITE-FILE")
  nil)

(defun is-writeable (split-path)
  (declare (ignore split-path))
  (print "-------------------------- IS-writeable:")
  (= 1 2))

(defun file-flush (path fh)
  (print "-------------------------- FILE-FLUSH")
  0)

(defun file-release (path flags)
  (print "-------------------------- FILE-RELEASE")
  0)

(defun file-truncate (path offset)
  (print "-------------------------- FILE-TRUNCATE")
  0)

(defun file-create (path mode dev)
  (print "-------------------------- FILE-CREATE")
  nil)

(defun dir-create (path mode)
  (print "-------------------------- DIR-CREATE")
  (- error-EACCES))

(defun file-remove (path)
  (print "-------------------------- FILE-REMOVE")
  (- error-EACCES))

(defun dir-remove (path)
  (print "-------------------------- DIR-REMOVE")
  (- error-EACCES))

(defun symlink (path content)
  (print "-------------------------- SYMLINK")
  (- error-EACCES))

(defun fuse-test (basePath)
  (defparameter *fuse-base-path* (cdr (split-sequence:split-sequence #\/ basePath)))
  (defparameter *fuse-objects* nil)
  (run-program "/bin/umount"
	       '()
	       :input "/tmp/mytest"
	       :output *standard-output*)
  (ensure-directories-exist "/tmp/mytest")
  ;; Execute Fuse-Run in separate thread to run async
  (sb-thread::make-thread (lambda ()
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
				      :rmdir 'dir-remove
				      :symlink 'symlink))))

(defun add-fuse-object (object)
  (setq *fuse-objects* (append *fuse-objects* object)))

(fuse-test "/tmp/mytest")

(defparameter sample-instance3 (make-instance 'sample-class2))
(setf (slot-value sample-instance3 'SomeString) "Sample Content")
(setf (slot-value sample-instance3 'SomeInteger) 42)
(setf (slot-value sample-instance3 'SomeList) (list 1 2 (list (list 3 (list 8 (list (list (make-instance 'sample-class2) sample-instance2 12 13)))))))
(setf (slot-value sample-instance3 'SomeObject) sample-instance2)
(add-fuse-object (list sample-instance3))

(add-fuse-object (list sample-instance sample-instance2))

(defclass sample-class3 ()
  (SomeString SomeInteger SomeList SomeObject))
(add-fuse-object (list (make-instance 'sample-class3)))
(print "THIS IS THE END OF FILE")
