
;;;; Macros

;;; if (no macro)
(if (> 2 3) "Yup" "Nope") ; Nope
(if (> 2 3) "Yup") ; NIL
(if (> 3 2) "Yup" "Nope") ; Nope

;;; when
;; Can contain multiple commands in the body
(when (> 3 2) (list 1) (list 2)) ; Both are evaluated

;;; unless
;; Same as when, but reversed
(unless (> 3 2) (list 1) (list 2)); NIL

;;; cond
;; Multiple elements are evaluated, and only the first, that is true, will be executed
(cond ((< 3 2) (list 'a))
       ((> 2 3) (list 'b))
       ((< 3 2) (list 'c))
       (NIL (list 'd))
       )

;;; AND, OR and NOT
(not nil) ; T
(not (= 1 1)) ; NIL
(and (= 1 2) (= 3 3)) ; NIL
(or (= 1 2) (= 3 3)) ; T

;;; DOLIST
;; Loops thorugh a list
(dolist (x (list 'a 'b 'c)) (print x)) ; A \n B \n C \n NIL

;;; DOTIMES
;; Execute a statement n-times
(dotimes (i 4) (print i)) ; 0 1 2 3

;;; DO
(do ((i 0 (1+ i)))
    ((>= i 4))
  (print i))

;;; LOOP
;; Simple:
(loop
     (when (> (get-universal-time) *some-future-date*)
       (return))
   (format t "Waiting~%")
   (sleep 60))
;; Extended:
(loop for i from 1 to 10 collecting i)


