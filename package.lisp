;;;; package.lisp

(defpackage #:defmodel
  (:use #:cl
        #:split-sequence
        #:postmodern)
  (:export #:defmodel
           #:symb
           
           #:*db-database*
           #:*db-user*
           #:*db-pass*
           #:*db-host*

           #:with-pg))
