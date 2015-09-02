;;;; package.lisp

(defpackage #:defmodel
  (:use #:cl
        #:postmodern)
  (:export #:defmodel
           #:symb
           #:*db-database*
           #:*db-user*
           #:*db-pass*
           #:*db-host*))


