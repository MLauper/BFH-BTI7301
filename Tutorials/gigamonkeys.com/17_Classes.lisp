;;;; CLASSES
;; -> All classes have a root at the class T
;; -> Built-in and user-defined classes
;; -> Defined with DEFCLASS
(defclass name (direct-superclass-name*)
    (slot-specifier*))

;; After defining a class, MAKE-INSTANCE is used to create a new instance of user-defined class
;; Variables are stored in slots
(defclass bank-account ()
  (customer-name ; slot
   balance)) ; slot
;; Return-Value of Make-Instance is the instance of the object
(make-instance 'bank-account)
;; Slots are unbound by default, access them will cause an error
;; -> They have to be set first
(defparameter *account* (make-instance 'bank-account))n
(setf (slot-value *account* 'customer-name) "Some Customer")
(setf (slot-value *account* 'balance) 23)
(slot-value *account* 'balance) ; Returns 23



