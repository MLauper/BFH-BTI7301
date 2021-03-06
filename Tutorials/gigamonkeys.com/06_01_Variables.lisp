;;;; Variables

;; Two Types: lexical and dynamic
;; lexical := roughly local
;; dynamic := roughly global

;;; Basics
;; -> Untyped (no variable defined) / Dynamically typed (variable types defined at runtime / Strongly typed (all variable type errors are detected)

;;;; Variable assignments
(let ((x 10) (y 15) z)(list x y z)) ; (10 15 NIL)

;; The scope is defined by the binding form (lexixal or dynamic)
(defun foo (x)
  (list x) ; x = arg, 1
  (let ((x 2))
    (list x) ; x = 2
    (let ((x 3))
      (list x) ; x = 3
      )
    (list x) ; x = 2 
    )
  (list x) ; x = arg, 1
  )
(foo 1)
;; Each reference to x refers to the smalles enclosing scope ("shadowing")

(let ((x 10) (y (+ x 1))) (list x y)) ; Error, x cannot be used in assignemnt of y
(let* ((x 10) (y (+ x 1))) (list x y)) ; Works, let* allows variables defined in the let-variable assignment list to be used later

;;; Lexical Variables
;; -> By defauld all variables are lexically scoped, that means, variables can be reffered only by code that's textually within the binding form
;; -> More or less the same as local variables in Java, C, Perl, Python
;; -> BUT, once assigned variable pointers, can be accessed outside of the lexical scope
(defparameter *fn* (let ((count 0)) #'(lambda () (setf count (1+ count)))))
(funcall *fn*) ; count get's incremented with each function call

;;; Dynamic / Special Variables
;; -> More or less global variables
;; Defined by DEFVAR and DEFPARAMETER
;; Naming convention: dynamic variables have * at the start and at the end
;; DEFVAR only assignes a value if the variable is undefined
;; DEFPARAMETER always overwrites
(defvar *x* 1) (defvar *x* 2) (list x) ; (1)
(defparameter *y* 1)(defparameter *y* 2)(list *y*) ; (2)
;; DEFVAR can define variables without giving it a value -> "unbound* variable

;; Global variables can also be temporary (lexically) overwritten by code (for example let or setf) ("shadowing")
(defparameter *x* 10)(let ((*x* 20)) (list *x*))(list *x*) ; (20) (10)


;;; Constants
;; -> Are defined by defconstant
(defconstant a 10) (list a) ; (10)
(defconstant a 20) ; Error
;; Constants cannot be reassigned or used as function parameters
;; Naming convention: Constants start and end with +

;;; Assignment
;; setf is a macro to assign variables
;; Expands to setq
(setf x 5)
;; Multiple variable definitions are possible
(setf x 1 y 2) (list x y) ; (1 2)
;; Setf returns the assigned value of the last variable
(setf x 10) ; 10
(setf x 10 y 15) ; 15
;; Can be used to assign multiple variables the same value
(setf x (setf y 20)) (list x y) ; (20 20)

;; Generally setf is always be used to assign variables
;; -> Setf could be extended to allow user-defined palces

;; Increment and decrement can be done by INCF and DECF macros
(setf x 1) (incf x) (incf x) (decf x) (list x) ; (2)

;; Swap values can be done with rotatef, can also be used with more arguments
(setf x 1) (setf y 2) (rotatef x y) (list x y) ; (2 1)
(setf x 1 y 2 z 3) (rotatef x y z) (list x y z) ; (2 3 1)

;; Shift can be used to shift values to the left, the last value will be assigned to the first variable
;; The original value of the first element is returned
(setf x 'a y 'b z' c) (shiftf x y z 'd) (list x y z) ; (B C D)


