;;;; Generic Functions and Methods

;;; -> You define Generics and then define methods based on Generics
(defgeneric withdraf (account amount)
  (:documentation "Reduces amount of money on account"))
;; -> With defmethod you have a two-lement list for parameters, the first ist the name of the parameter, and the second ist the specializer (Name of class or an EQL specializer)
(defmethod withdraf ((account bank-account) amount)
  (when (< (balance account) amount)
    (error "Account overdrawn."))
  (decf (balance account) amount))
;; -> If no specializer is made, it is implicitly specialized on T -> Any object instance

;; CALL-NEXT-METHOD is used to pass function call to next method

