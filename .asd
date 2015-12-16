(defsystem 
  :author "BFH-BTI7301 <M. Lauper>"
  :maintainer "BFH-BTI7301 <M. Lauper>"
  :license ""
  :version "0.1"

  :depends-on (:lucerne
               :)
  :defsystem-depends-on (:asdf-linguist)
  :components ((:module "assets"
                :components
                ((:module "css"
                  :components
                  ((:sass "style")))
                 (:module "js"
                  :components
                  ((:static-file "scripts.js")))))
               (:module "src"
                :serial t
                :components
                ((:file ""))))
  :description ""
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op -test))))
