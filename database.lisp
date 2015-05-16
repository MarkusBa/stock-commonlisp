;;(load "~/quicklisp/setup.lisp")
;;(ql:quickload "postmodern")
;;(asdf:load-system "stock-commonlisp")

(in-package #:stock-commonlisp)

(defclass pg-datastore ()
  ((connection-spec :initarg :connection-spec
                    :accessor connection-spec)))

(defparameter *db*
  (make-instance 'pg-datastore
                 :connection-spec '("stock-commonlisp" "markus" "1234" "localhost")))

(defclass player ()
  ((id :col-type serial :reader player-id)
   (name :col-type string :reader player-name :initarg :name)
   (password :col-type string :reader player-password :initarg :password)
   (salt :col-type string :reader player-salt :initarg :salt)
   (email :col-type string :reader player-email :initarg :email))
  (:metaclass dao-class)
  (:keys id))

(defclass item ()
  ((id :col-type serial :reader item-id)
   (symbol :col-type string :reader item-symbol :initarg :symbol)
   (amount :col-type numeric :reader item-amount :initarg :amount)
   (price :col-type numeric :reader item-price :initarg :price)
   (id-player :col-type integer :reader item-player-id :initarg :id-player)
   (ts :col-type timestamp :reader item-ts :initarg :ts))
  (:metaclass postmodern:dao-class)
  (:keys id))

(deftable item
  (!dao-def)
  (!foreign 'player 'id-player 'id))

(with-connection (connection-spec *db*)
                   (unless (table-exists-p 'player)
                     (execute (dao-table-definition 'player))))

(with-connection (connection-spec *db*)
                   (unless (table-exists-p 'item)
                     (create-table 'item)))
                   
