;;;; stock-commonlisp.asd

(asdf:defsystem #:stock-commonlisp
  :description "Describe stock-commonlisp here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:drakma
               #:postmodern
               #:simple-date
               #:restas)
  :serial t
  :components ((:file "package")
               (:file "stock-commonlisp")
               (:file "database")))

