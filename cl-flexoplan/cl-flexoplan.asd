;;;; cl-flexoplan.asd

(asdf:defsystem #:cl-flexoplan
  :serial t
  :description "Backend part of FLEXOPLAN planning engine."
  :author "Alexander Popolitov <popolit@gmail.com>"
  :license "GPL"
  :depends-on (#:cl-ppcre #:cl-interpol #:defmacro-enhance #:iterate #:yaclanapht  #:esrap)
  :components ((:file "package")
               (:file "cl-flexoplan")))

