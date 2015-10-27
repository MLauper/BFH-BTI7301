;; Numbers
123       ; the integer one hundred twenty-three
3/7       ; the ratio three-sevenths
1.0       ; the floating-point number one in default precision
1.0e0     ; another way to write the same floating-point number
1.0d0     ; the floating-point number one in "double" precision
1.0e-4    ; the floating-point equivalent to one-ten-thousandth
+42       ; the integer forty-two
-42       ; the integer negative forty-two
-1/4      ; the ratio negative one-quarter
-2/8      ; another way to write negative one-quarter
246/2     ; another way to write the integer one hundred twenty-three


;; Strings
"foo"     ; the string containing the characters f, o, and o.
"fo\o"    ; the same string
"fo\\o"   ; the string containing the characters f, o, \, and o.
"fo\"o"   ; the string containing the characters f, o, ", and o.

;; Naming Conventions
*global*    ; Global variable
+static+    ; Constant
%low-level% ; Low-Level Function

;; Example of lists
x             ; the symbol X
()            ; the empty list
(1 2 3)       ; a list of three numbers
("foo" "bar") ; a list of two strings
(x y z)       ; a list of three symbols
(x 1 "foo")   ; a list of a symbol, a number, and a string
(+ (* 2 3) 4) ; a list of a symbol, a list, and a number.
(defun hello-world () (format t "hello, world"))  ; four-item list containing two symbols, the empty list, another list (containint two symbols and a string)

;; Booleans
t    ; True (Everything but NIL is true)
NIL  ; False (only false value, is also used to represent an empty list)
()   ; Equal to NIL, completely interchangeable
'nil, '() ; Equal to NIL

;; Symbols
symbol    ; A symbol named symbol
:symbol   ; A keyword named symbol

;; Function call form, makro form and special form
(function-anem argument*)                                ; A function call
(if x (format t "x is true") (format t "x is false"))    ; special operator (if)
					; if is a special form, because otherwise all three arguments would be evaluated (therefore printing x, "x is true" and "x is false", possibly followed by the output of the if-function)
					; There are 25 speical operators
(quote (+ 1 2))                                          ; Other example of a special function (returns argnument without evaluating it)
'(+ 1 2)                                                 ; Shorthand for quote, same expression

;; Equality
; Each function returns true if equal and false if not
EQ             ; Tests for object identity (attention -> depends on lisp implementation
EQL            ; Same as EQ, but garranteas to consider two objects of the same class representing the same num or char as equivalent
EQUAL          ; Looser comparision -> i.e. strings with the same caracters and lists with the same elements are concidered equal
EQUALP         ; Even looser comparision -> i.e. case doesn't matter

; Examples:
(eq 3 3) ; t -> Should not be used, because it depends on lisp implementation
(eq 'x 'x) ; t -> Should not be used, because it depends on lisp implementation
(eq x x)   ; compares symbol reference
(eql 3 3)  ; t
(eql 3 3.0)				; NIL
(eql 'x 'x)				; t
(eql x x)				; compares symbol reference

(eql "abc" "abc")			; NIL
(equal "abc" "abc")			; t
(equal "abc" "ABC")			; NIL
(equalp "abc" "ABC")			; t

(eql '("a" "b") '("a" "b"))		; NIL
(equal '("a" "b") '("a" "b"))		; t
(equal '("a" "b") '("A" "b"))		; NIL
(equalp '("a" "b") '("A" "b"))		; t

(eql 1 1.0)				; NIL
(equal 1 1.0)				; NIL
(equalp 1 1.0)				; t


;; Commenting
;;;; Four semicolons are used for a file header comment.

;;; A comment with three semicolons will usually be a paragraph
;;; comment that applies to a large section of code that follows,

(defun foo (x)
  (dotimes (i x)
    ;; Two semicolons indicate this comment applies to the code
    ;; that follows. Note that this comment is indented the same
    ;; as the code that follows.
    (some-function-call)
    (another i)              ; this comment applies to this line only
    (and-another)            ; and this is for this line
        (baz)))






