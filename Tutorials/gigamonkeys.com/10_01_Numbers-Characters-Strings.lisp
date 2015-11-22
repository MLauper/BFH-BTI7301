;;; Numbers
;; Normal representation
'123
'+123
'-123
'123.
'2/3
'-2/3
'4/6
'6/3
;; Binary
#b10101 ; 21
;; Octal
#o777 ; 511
;; Hex
#xDADA ; 56026
;; Any other base with #nR
#2R10101 ; 21
#3R10 ; 3
#36rABCDEFGHIJKLMNOPQRSTUVWXYZ ; 8337503854730415241050377135811259267835
;; Floating points
1.0      ==> 1.0
1e0      ==> 1.0
1d0      ==> 1.0d0
123.0    ==> 123.0
123e0    ==> 123.0
0.123    ==> 0.123
.123     ==> 0.123
123e-3   ==> 0.123
123E-3   ==> 0.123
0.123e20 ==> 1.23e+19
123d23   ==> 1.23d+25
;; Imaginary Numbers
#c(2      1)    ==> #c(2 1)
#c(2/3  3/4)    ==> #c(2/3 3/4)
#c(2    1.0)    ==> #c(2.0 1.0)
#c(2.0  1.0d0)  ==> #c(2.0d0 1.0d0)
#c(1/2  1.0)    ==> #c(0.5 1.0)
#c(3      0)    ==> 3
#c(3.0  0.0)    ==> #c(3.0 0.0)
#c(1/2    0)    ==> 1/2
#c(-6/3   0)    ==> -2
;; Operations
(+ 1 2)              ==> 3
(+ 1 2 3)            ==> 6
(+ 10.0 3.0)         ==> 13.0
(+ #c(1 2) #c(3 4))  ==> #c(4 6)
(- 5 4)              ==> 1
(- 2)                ==> -2
(- 10 3 5)           ==> 2
(* 2 3)              ==> 6
(* 2 3 4)            ==> 24
(/ 10 5)             ==> 2
(/ 10 5 2)           ==> 1
(/ 2 3)              ==> 2/3
(/ 4)                ==> 1/4
;; More Operations
(+ 1 2.0)             ==> 3.0
(/ 2 3.0)             ==> 0.6666667
(+ #c(1 2) 3)         ==> #c(4 2)
(+ #c(1 2) 3/2)       ==> #c(5/2 2)
(+ #c(1 1) #c(2 -1))  ==> 3
;; Floor and truncate
(+ (* (floor    (/ x y)) y) (mod x y)) === x
(+ (* (truncate (/ x y)) y) (rem x y)) === x
(floor 2.8) ; 2 (Truncate to negative infinity
(ceiling 2.2) ; 3 (Truncate to positive infinity
(round 3.5) ; 4 ATTENTION: if it is between two integers, it rounds to the next even integer
(round 2.5) ; 2, NOT 3!!
(truncate 2.3) ; 2


;;; Characters
;; Normal characters
#\a
;; Special characters
#\tab
#\space
;; Comparision
(char= #\a 'b) ; Error, b is not a char
(char= #\a #\A) ; NIL
(char-equal #\a #\A) ; T
(char> #\b #\a) ; t
(char> #\A #\b) ; NIL

;;; Strings
;; -> One-dimensional arary of chars
"foobar"
;; \ can escape chars
;; Attention, the correct content of a string, will only be visible with format
(print "foo\"bar") ; "foo\"bar"
(format t "foo\"bar") ; foo"bar
;; Comparision
(string= "someString" "someString") ; t
(string= "abc" "Abc") ; Nil
(string-equal "abc" "Abc") ; T
;; Additional Arguments :start1 : end1 :start2 : end2
(string= "someString" "someOtherString" :start1 0 :end1 4 :start2 0 :end2 4) ; t
(string= "someString" "AndsomeOtherString" :start1 0 :end1 4 :start2 3 :end2 7) ; t
;; can also return first non-matching char
(string/= "abcde" "abcZZ") ; 3

