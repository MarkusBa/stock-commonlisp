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
   (ts :col-type timestamp :reader item-ts :initarg :ts :default :now))
  (:metaclass postmodern:dao-class)
  (:keys id))

(deftable item
  (!dao-def)
  (!foreign 'player 'id-player 'id))

(with-connection (connection-spec *db*)
                   (unless (table-exists-p 'player)
                     (execute (dao-table-definition 'player))))

(with-connection (connection-spec *db*)
                   (unless (table-exitss-p 'item)
                     (create-table 'item)))
                   
(with-connection (connection-spec *db*)
                 (insert-dao (make-instance 'player
                                            :name "test"
                                            :password "test"
                                            :salt "test"
                                            :email "test@test.com")))

(with-connection (connection-spec *db*)
                 (insert-dao (make-instance 'item
                                            :symbol "CASH"
                                            :amount 10000
                                            :price 1
                                            :id-player 1
                                            :ts (simple-date:universal-time-to-timestamp (get-universal-time)))))



(defun find-items (id-player)
  (with-connection (connection-spec *db*)
                   (query (:select :* :from 'item
                                   :where (:= id-player 'id-player))
                          :plist)))

(defun existing-amount (id-player symbol)
  (with-connection (connection-spec *db*)
                   (query (:select :amount :from 'item
                                   :where (:and (:= id-player 'id-player)
                                                (:= symbol 'symbol)
                          )):single)))

(defun update-item! (amount symbol id-player)
  (query (:update 'item :set 'amount amount :where  (:and (:= id-player 'id-player)
                                                          (:= symbol 'symbol)))))

(defun insert-item! (symbol amount price id-player ts)
    (execute (:insert-into 'item :set 'symbol symbol 'amount amount 'price price 'id-player id-player 'ts ts)))


(defun gettime ()
  (simple-date:universal-time-to-timestamp (get-universal-time)))

(defun order (ordersymbol amount price idplayer)
  (with-connection (connection-spec *db*)
                   (with-transaction (transaction)
                    (let ((money (existing-amount idplayer "CASH"))
                          (costs (* amount price))
                          (existingamount (existing-amount idplayer ordersymbol)))
                      (when (and (not money) (>= money costs) )
                        (update-item! (- money costs) "CASH" idplayer)
                        (if existingamount
                            (update-item! (+ existingamount amount) ordersymbol idplayer)
                            (insert-item! ordersymbol amount price idplayer (gettime))))))))





(defun get-all-symbols (&optional package)
  (let ((lst ())
        (package (find-package package)))
    (do-all-symbols (s lst)
      (when (fboundp s)
        (if package
            (when (eql (symbol-package s) package)
              (push s lst))
            (push s lst))))
    lst))


