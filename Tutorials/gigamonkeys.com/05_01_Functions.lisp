;;;; Basic syntax
;;; defun is a makro
(defun name (parameter*)
  "Optional documentation string."
  body-form*)

;;; Function naming conventsions
string->widget		   ; It's common to use -> for conversions
frob-widget    ; Use dashes, not underscores or inner caps
NOT frob_widget
NOT frobWidget

;;; Example:
(defun verbose-sum (x y)
  "Sum any two numbers after printing a message."
  (format t "Summing ~d and ~d.~%" x y)
  (+ x y))


;;;; Optional Parameters
&optional ; -> Keyword that can be used before optional parameters

;;; Exmample:
(defun foo (a b &optional c d) (list a b c d))
(foo 1 2 3 4 5) ; ERROR
(foo 1 2 3 4) ; (1 2 3 4)
(foo 1 2 3) ; (1 2 3 nil)
(foo 1 2) ; (1 2 nil nil)
(foo 1) ; ERROR

;;;; Default Values
;; -> Default Values can be passed by providing a second element with the default value
;; Example:
(defun foo (a &optional (b 10) (c 20)) (list a b c))
(foo 1 2 3) ; (1 2 3)
(foo 1 2); (1 2 20)
(foo 1); (1 10 20)
(foo); ERRO

;; -> Default values can reference to other parameters
(defun foo (a &optional (b a)) (list a b))
(foo 1 2) ; (1 2)
(foo 1) ; (1 1)

;;;; Supplied boolean
;; -> You can request if value was set by function caller or default value is being used
(defun foo (a &optional (b 10 b-supplied-p)) (list a b b-supplied-p))
(foo 1) ; (1 10 NIL)
(foo 1 2) ; (1 2 T)
(foo 1 10) ; (1 10 T)

;;;; Variable number of additional parameters
;; -> You can have a variable number of additional parameters with &rest
(defun foo (a b &rest others) (list a b others))
(foo 1 2) ; (1 2 NIL)
(foo 1 2 3) ; (1 2 (3))
(foo 1 2 3 4 5 6) ; (1 2 (3 4 5 6))

;;;; Named Parameters
;; -> You can name the parameters with &key
;; -> Remember, keywords are names that start with a colon and are defined as self-evaluating constants (:a -> a)
(defun foo (a &key b c) (list a b c))
(foo 1) ; (1 NIL NIL NIL)
(foo 1 :b 2 :c 3) ; (1 2 3)
(foo 1 :c 2 :b 3 ) ; (1 3 2)
(foo 1 :c 3) ; (1 NIL 3)

;; -> You can differentiate between the parameter name nad the internal variable name
(defun foo (&key ((:external internal) 0 internal-supplied-p)) (list internal internal-supplied-p))
(foo :external 5) ; (5 T)
(foo) ; (0 NIL)

;;;; Mixing parameter Types
;; -> Order must always be 1) required parametesr 2) optional parameters 3) rest parameters 4) keyword parameters
;; -> Attention while combining multiple types:
;; Optional and keyword
(defun foo (&optional a b &key c) (list a b c))
(foo :c 5) ; (:c 5 NIL), NOT (NIL NIL 5)
;; Rest and Key
(defun foo (&rest rest &key x) (list rest x))
(foo :x 5 ) ; ((:x 5) 5)

;;;; Return Values
;; -> Default return last expression
;; -> Return in middle of the function with retunr-from special operator
;; -> You have to state the function name from which to return
(defun foo () (return-from foo (list 'MiddleOfFunction)) (list 'EndOfFunction)))
(foo) ; (MIDDLEOFFUNCTION)

;;;; Functions as data (Higher-Order Functions)
;; -> You can receive a function object with function special operator
(defun foo (x y) (list x (* 2 x) y))
(function foo) ; #<Function Foo>
;; -> Shorthand for Function #
;; -> Remember Shorthand for Quote '
#'foo ;; equivalent call

;; -> A function object can be called with funcall or apply
;; -> With funcall, first parameter is the function object and all other parameters will be passed to the function
(funcall #'foo 5 15) ; (5 10 15)
;; -> With apply, first parameter is the function object and the second parameter is a list of the parameters passed to the function
(apply #'foo '(5 15)) ; (5 10 15)
;; -> apply also allows to combine fixed parameters, followed by a list of parameters
(apply #'foo 5 '(15)) ; (5 10 15)
(apply #'foo 5 15 ()) ; (5 10 15)

;;;; Anonymous Functions
;; Anonymous functions can be created with the lambda special operator
(lambda (paramters) body)
;; Example:
(funcall #'(lambda (x y) (+ x y)) 5 5) ; 10




