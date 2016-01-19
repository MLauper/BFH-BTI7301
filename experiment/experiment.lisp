(declaim #+sbcl(sb-ext:muffle-conditions style-warning))

(require 'cl-fuse)
(require 'cffi)
(require 'cl-utilities)
(require 'sb-introspect)

(use-package 'cl-fuse)
(use-package 'cffi)
(use-package 'cl-utilities)

(require 'split-sequence)

; http://cl-cookbook.sourceforge.net/hashes.html
(defparameter *func-outputs* (make-hash-table))

; test function 1: simply print some values
(defun prnt (a b c)
	(format t "a='~A'~%b='~A'~%c='~A'~%" a b c)
)

; test function 2: quadratic equation solver
(defun qgl (a b c)
	(format t "Solution for a=~A, b=~A and c=~A:~%x1=~f~%x2=~f~%"
		a b c
		(/ (+ (- b) (sqrt (- (* b b) (* 4 a c)))) (* 2 a))
		(/ (- (- b) (sqrt (- (* b b) (* 4 a c)))) (* 2 a))
	)
)

; other silly test functions
(defun foo (a b &optional c d) (write-to-string (list a b c d)))
(defun bar (&key a b c) (write-to-string (list a b c)))

; global variable holding the functions we want to use
(defvar funcs (list 'prnt 'qgl 'foo 'bar))

(defun trim-whitespace (string)
	(string-trim '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout) string)
)

(defun is-directory (split-path)
	(declare (ignore fh)) ; ?
	(or
		(null split-path)
		(equalp (car split-path) "")
		(and
			(equalp (list-length split-path) 1)
			(find (car split-path) (mapcar 'write-to-string funcs) :test #'equal)
		)
	)
)

(defun symlink-target (split-path)
	nil
)

(defun directory-content (split-path)
	(cond
		( (or (null split-path) (equalp "" (car split-path)))
			(mapcar 'write-to-string funcs)
		)
		( (find (car split-path) (mapcar 'write-to-string funcs) :test #'equal)
			(loop
				for func in funcs
				when (equalp (car split-path) (write-to-string func))
				return (list "RUN" "OUTPUT" "PARAMS")
			)
		)
		(t nil)
	)
)

(defun file-size (split-path)
	; Simply return the length in bytes of the function name. Not very useful, though.
	;(format t ">>> file-size: ~A~%" split-path)
	;(cond
	;	( (equalp (list-length split-path) 1)
	;		(loop
	;			for func
	;			in (mapcar 'write-to-string funcs)
	;			when (equalp (car split-path) func)
	;			return (length func)
	;		)
	;	)
	;	(t 0)
	;)
	0
)

(defun file-read (split-path size offset fh)
	(declare (ignore fh))
	(declare (ignore size))
	(declare (ignore offset))
	(format t ">>> file-read: split-path=~A size=~A offset=~A fh=~A~%" split-path size offset fh)

	; TODO test
	;(list :offset 0 "test")

	(cond
		( (equalp (cadr split-path) "OUTPUT")
			;(let* (name (cadr split-path)) `(:offset 0 ,name))
			;(print (gethash (car split-path) *func-outputs*))
			(append '(:offset 0) (list (write-to-string funcs)))
		)
		( (equalp (cadr split-path) "PARAMS")
			(append '(:offset 0) (write-to-string (mapcar 'write-to-string (sb-introspect:function-lambda-list func))))
		)
		(t nil)
	)
)

(defun file-write (split-path data offset fh)
	(format t ">>> file-write: split-path=~A data=\"~A\" offset=~A fh=~A~%"
		split-path
		(trim-whitespace (map 'string #'code-char data))
		offset fh
	)

	(cond
		; Only allow input in the file RUN
		( (equalp (cadr split-path) "RUN")
			(setf (gethash (car split-path) *func-outputs*)
				(with-output-to-string (*standard-output*)
					(eval (read-from-string (concatenate 'string
						"(" (car split-path) " " (map 'string #'code-char data)  ")"
					) ) )
				)
			)

			(format t "Stored:~%~A~%" (gethash (car split-path) *func-outputs*))
			t
		)
		(t nil)
	)
)

(defun fuse-init (mountpoint fsname subtype debug &optional (threaded nil))
	(fuse-run
		(append
			(list "none" mountpoint
				"-o" (concatenate 'string "fsname=" fsname)
				"-o" (concatenate 'string "subtype=" subtype)
			)
			(if debug '("-d") '())
		)
		:directoryp		'is-directory 
		:directory-content	'directory-content
		:symlink-target		'symlink-target
		:file-size		'file-size
		:file-read		'file-read
		:file-write		'file-write
		:call-manager		(lambda (f &rest x)
						; TODO pass funcs directly to all fuse functions via an argument
						(declare (ignore x))
						(if threaded
							(bordeaux-threads:make-thread f)
							(funcall f)
						)
					)
	)
)

(defvar *SIGINT* 2)

(defmacro set-signal-handler (signo &body body)
	(let (handler (gensym "HANDLER"))
		`(progn
			(cffi:defcallback
				,handler
				:void ((signo :int))
				(declare (ignore signo))
				,@body
			)
			(cffi:foreign-funcall
				"signal"
				:int
				,signo
				:pointer
				(cffi:callback ,handler)
			)
		)
	)
)
 
(defun umount (mountpoint)
	(run-program "/bin/sh" (list "-c" (concatenate 'string "fusermount -u " mountpoint))
		:output *error-output*
	)
)

(defun main (&key mountpoint (fsname "experimentfs") (subtype "foo") (debug 0))
	(set-signal-handler *SIGINT*
		(format t "Goodbye, my friends. Unmounting ~A~%" mountpoint)
		(umount mountpoint)
		(quit)
	)

	(if (probe-file (make-pathname :name mountpoint))
		(progn
			(format t "~A exists.~%" mountpoint)

			(if (directory (make-pathname :directory mountpoint))
				(progn
					(format t "~A is a directory.~%" mountpoint)
					(if (run-program "/bin/sh" '("-c" '(concatenate "mountpoint -q " mountpoint)))
						(progn 
							(format t "Something is mounted at ~A, unmounting it.~%" mountpoint)
							(umount mountpoint)
						)
						(format t "Nothing is mointed at ~A~%" mountpoint)
					)
				)
				(progn
					(format t "~A is not directory, exiting.~%" mountpoint)
					(quit)
				)
			)

			; Alternative version (probe /proc/mounts for the mountpoint)
			;(with-open-file (input "/proc/mounts")
			;	(loop for line = (read-line input nil) while line do
			;		(if (equalp (cadr (split-sequence #\Space line)) mountpoint)
			;			(progn
			;				(format t "Something is mounted at ~A, unmount it.~%" mountpoint)
			;				(umount mountpoint)
			;				(return)
			;			)
			;		)
			;	)
			;)
		)
		(progn
			(format t "~A does not exist, creating it.~%" mountpoint)

			; Even though probe-file reports that the mountpoint does not exist, it might be
			; in the undefined state "Transport endpoint is not connected". Therefore we run
			; an unmount on it, just in case.
			(umount mountpoint)

			(ensure-directories-exist mountpoint)
		)
	)

	(fuse-init mountpoint fsname subtype debug
		(if (boundp 'common-lisp-user::*parallel-fuse-test*) t nil)
	)
)

(main
	:mountpoint "/tmp/test/"
	:fsname "experiment"
	:subtype "foo"
	:debug t
)
