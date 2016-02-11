(declaim #+sbcl(sb-ext:muffle-conditions style-warning))

(require 'cl-fuse)
(require 'cffi)
(require 'cl-utilities)
(require 'sb-introspect)

(use-package 'cl-fuse)
(use-package 'cffi)
(use-package 'cl-utilities)

(require 'split-sequence)

(setq *func-outputs* (make-hash-table :test 'equal))

; test function 1: simply print some values
(defun prnt (a b c)
	(format t "a='~A'~%b='~A'~%c='~A'~%" a b c)
)

; test function 2: quadratic equation solver
(defun qgl (a b c)
	(let
		(
			(x1 (/ (+ (- b) (sqrt (- (* b b) (* 4 a c)))) (* 2 a)))
			(x2 (/ (- (- b) (sqrt (- (* b b) (* 4 a c)))) (* 2 a)))
		)
		(format *error-output* "(-~A + sqrt(~A^2 - 4*~A*~A))/(2*~A) = ~f~%(-~A - sqrt(~A^2 - 4*~A*~A))/(2*~A) = ~f~%"
			b b a c a x1 b b a c a x2
		)
		(format t "Solution for a=~A, b=~A and c=~A:~%x1=~f~%x2=~f~%" a b c x1 x2)
		(list x1 x2)
	)
)

; other silly test functions
(defun foo (a b &optional c d) (format t "~A~%" (write-to-string (list a b c d))))
(defun bar (&key a b c) (format t "~A~%" (write-to-string (list a b c))))

; global variable holding the functions we want to use
(defvar funcs (list 'prnt 'qgl 'foo 'bar))

(defun is-directory (split-path)
	(let
		( (func	(car split-path)) )

		(or
			(null split-path)
			(equalp func "")
			(and
				(equalp (list-length split-path) 1)
				(find func (mapcar 'write-to-string funcs) :test #'equal)
			)
		)
	)
)

(defun directory-content (split-path)
	(let
		( (func	(car split-path)) )

		(cond
			( (or (null split-path) (equalp func ""))
				(mapcar 'write-to-string funcs)
			)
			( (find func (mapcar 'write-to-string funcs) :test #'equal)
				(loop
					for f in funcs
					when (equalp func (write-to-string f))
					return (list "INVOKE" "PARAMS" "RETURN" "STDOUT" "STDERR")
				)
			)
			(t nil)
		)
	)
)

(defun file-size (split-path)
	(let
		(
			(func	(car split-path))
			(action	(cadr split-path))
		)

		(cond
			( (equalp action "INVOKE") 0 )
			( (equalp action "PARAMS")
				(loop for f in funcs
					when (equalp func (write-to-string f))
					return (+ 1 (length (write-to-string (sb-introspect:function-lambda-list f))))
				)
			)
			( (equalp action "RETURN")
				(+ 1 (length (write-to-string (car (gethash func *func-outputs*)))))
			)
			( (equalp action "STDOUT")
				(length (nth 1 (gethash func *func-outputs*)))
			)
			( (equalp action "STDERR")
				(length (nth 2 (gethash func *func-outputs*)))
			)
			(t nil)
		)
	)
)

(defun file-read (split-path size offset fh)
	(declare (ignore fh))
	(declare (ignore size))
	(declare (ignore offset))

	(let
		(
			(func	(car split-path))
			(action	(cadr split-path))
		)

		(format t "[file-read] split-path:~A size:~A offset:~A fh:~A~%" split-path size offset fh)

		(cond
			( (equalp action "PARAMS")
				(loop for f in funcs
					when (equalp func (write-to-string f))
					return (append '(:offset 0)
						(list (format nil "~A~%"
							(write-to-string (sb-introspect:function-lambda-list f))
						) )
					)
				)
			)
			( (equalp action "RETURN")
				(append '(:offset 0)
					(list (format nil "~A~%"
						(write-to-string (car (gethash func *func-outputs*)))
					) )
				)
			)
			( (equalp action "STDOUT")
				(append '(:offset 0) (list (nth 1 (gethash func *func-outputs*))))
			)
			( (equalp action "STDERR")
				(append '(:offset 0) (list (nth 2 (gethash func *func-outputs*))))
			)
			(t nil)
		)
	)
)

(defun file-write (split-path data offset fh)
	(let
		(
			(func	(car split-path))
			(action	(cadr split-path))
			(data	(string-trim
					'(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout)
					(map 'string #'code-char data)
				)
			)
		)

		(format t "[file-write] func:~A data:\"~A\" offset:~A fh:~A~%" func data offset fh)

		(cond
			( (equalp action "INVOKE")
				(format t "[INVOKE] (~A ~A)~%" func data)

				; capture stdout, stderr and the function's return value in a list
				; and add it to the *func-outputs* hash table
				(let
					(
						(*standard-output* (make-string-output-stream))
						(*error-output* (make-string-output-stream))
					)
					(setf (gethash func *func-outputs*)
						(list
							(eval (read-from-string (format nil "(~A ~A)" func data)))
							(get-output-stream-string *standard-output*)
							(get-output-stream-string *error-output*)
						)
					)
				)

				; debugging
				(format t "Stored output of function '~A':~%return: ~A~%stdout: ~A~%stderr: ~A~%"
					func
					(append '(:offset 0) (list (car (gethash func *func-outputs*))))
					(append '(:offset 0) (list (nth 1 (gethash func *func-outputs*))))
					(append '(:offset 0) (list (nth 2 (gethash func *func-outputs*))))
				)
				t
			)
			(t nil)
		)
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
		:directory-content	'directory-content
		:directoryp		'is-directory
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
	:mountpoint "/tmp/test/" ; must end with a slash
	:fsname "experiment"
	:subtype "foo"
	:debug t
)
