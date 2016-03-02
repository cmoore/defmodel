;;;; defmodel.asd

(asdf:defsystem #:defmodel
  :description "Lightweight(?) models for Postmodern."
  :author "Clint Moore <clint@ivy.io>"
  :license "MIT"
  :serial t
  
  :depends-on (#:postmodern
               #:local-time
               #:cl-postgres+local-time
               #:split-sequence)
  
  :components ((:file "package")
               (:file "defmodel")))

