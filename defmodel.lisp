
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

       ;; Export symbols for all accessors marked as 'export'
       (export ',(symb name 'uid))
       ,@ (mapcar (lambda (name) `(export ',name))
                  exports)

       (with-pg
           (unless (table-exists-p ',name)
             (execute (dao-table-definition ',name))))

       (defmacro ,(symb name 'create) (&rest args)
         `(with-pg
              (make-dao ',',name ,@args)))
       (export ',(symb name 'create))

       (defun ,(symb name 'get-all) ()
         (with-pg
             (select-dao ',name)))
       (export ',(symb name 'get-all))

       (defun ,(symb name 'get) (id)
         (with-pg
             (get-dao ',name id)))
       (export ',(symb name 'get))

       (defmacro ,(symb name 'query-dao) (expression)
         `(with-pg
              (query-dao ',',name ,expression)))
       (export ',(symb name 'query-dao))

       (defmacro ,(symb name 'select) (sql-test &optional sort)
         `(with-pg
              (select-dao ',',name ,sql-test ,sort)))
       (export ',(symb name 'select))

       (defun ,(symb name 'update) (,name)
         (with-pg
             (update-dao ,name)))
       (export ',(symb name 'update))

       (defun ,(symb name 'delete) (,name)
         (with-pg
             (delete-dao ,name)))
       (export ',(symb name 'delete)))))
