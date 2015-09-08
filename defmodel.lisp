;;;
;;; defmodel
;;; Defines utilities for Postmodern databases.
;;;
;;;
;;; (in-package :yourpackage)
;;;
;;; (eval-when (:compile-toplevel :load-toplevel)
;;;    (setf defmodel:*db-database* "mydb")
;;;    (setf defmodel:*db-user* "myuser")
;;;    (setf defmodel:*db-pass* "mypassword")
;;;    (setf defmodel:*db-host* "127.0.0.1"))
;;;
;;;
;;; (defmodel person ((email :col-type text
;;;                          :accessor person-email
;;;                          :initarg :email
;;;                          :export t)))
;;;
;;;
;;; And that's it!
;;;
;;; Now you have available to you:
;;; find-by-person-email, person-select, person-get, person-update,
;;; person-select, person-delete, person-get-all, and person-query-dao
;;;
;;;


(in-package #:defmodel)


(defvar *db-database* nil)
(defvar *db-user* nil)
(defvar *db-pass* nil)
(defvar *db-host* nil)

(eval-when (:compile-toplevel :load-toplevel)
  (local-time:set-local-time-cl-postgres-readers)
  
  (defun symb (a b)
    (intern (format nil "~a-~a" (symbol-name a) (symbol-name b))))
  
  (defmacro with-pg (&body body)
    `(postmodern:with-connection
         (list ,*db-database* ,*db-user* ,*db-pass* ,*db-host* :pooled-p t)
       ,@body)))


(defmacro defmodel (name slot-definitions)
  
  (let ((exports (mapcan (lambda (spec)
                           (when (getf (cdr spec) :export)
                             (let ((name (getf (cdr spec) :accessor)))
                               (list name))))
                         slot-definitions)))
    `(progn
       (defclass ,name () ((uid :col-type string
                                :initform (format nil "~a" (uuid:make-v4-uuid))
                                :accessor ,(symb name :uid)
                                :export t)
                           ,@slot-definitions)
         (:metaclass dao-class)
         (:keys uid))

       (export ',(symb name 'uid))

       ;; Export symbols for all accessors marked as 'export'
       ,@ (mapcar (lambda (name) `(export ',name))
                  exports)

       ;;; Create the table if it does not already exist.
       (with-pg
         (unless (table-exists-p ',name)
           (execute (dao-table-definition ',name))))

       ;;; (table-create :slot "value" :slot "value" ...)
       (defmacro ,(symb name 'create) (&rest args)
         `(with-pg
            (make-dao ',',name ,@args)))
       (export ',(symb name 'create))

       ;;; (table-get-all) -> (#<TABLE> #<TABLE> ...)
       (defun ,(symb name 'get-all) ()
         (with-pg
           (select-dao ',name)))
       (export ',(symb name 'get-all))

       ;;; (table-get "uid") -> #<TABLE>
       (defun ,(symb name 'get) (id)
         (with-pg
           (get-dao ',name id)))
       (export ',(symb name 'get))

       
       (defmacro ,(symb name 'query-dao) (expression)
         `(with-pg
            (query-dao ',',name ,expression)))
       (export ',(symb name 'query-dao))

       ;;; (table-select (:and (:= 'slot "value")
       ;;;                     (:not-null 'slot)))
       (defmacro ,(symb name 'select) (sql-test &optional sort)
         `(with-pg
            (select-dao ',',name ,sql-test ,sort)))
       (export ',(symb name 'select))

       ;;; (defvar x (table-get "uid"))
       ;;; (setf (table-slot x) "Honk")
       ;;; (table-update x)
       (defun ,(symb name 'update) (,name)
         (with-pg
           (update-dao ,name)))
       (export ',(symb name 'update))

       ;;; (defvar x (table-get "uid"))
       ;;; (table-delete x)
       (defun ,(symb name 'delete) (,name)
         (with-pg
           (delete-dao ,name)))
       (export ',(symb name 'delete))

       ;;; (find-by-table-slot "value") -> (#<TABLE> #<TABLE>)
       ,@ (mapcar (lambda (slot)
                    `(progn (defun ,(symb 'find-by slot) (value)
                              (,(symb name 'select) (:= ',(intern
                                                           (cadr
                                                            (split-sequence:split-sequence #\- (string slot)))) value)))
                            (export ',(symb 'find-by slot))))
                  exports))))
