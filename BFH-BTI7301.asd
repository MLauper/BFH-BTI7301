;;;; BFH-BTI7301.asd

(asdf:defsystem #:bfh-bti7301
  :description "Project 1 at BFH"
  :author "MLauper, YDenzer, BMueller"
  :license ""
  :depends-on (#:cl-fuse)
  :serial t
  :components ((:file "package")
               (:file "bfh-bti7301")))

