;;;; wordgames.asd

(asdf:defsystem #:wordgames
  :serial t
  :description ""
  :author "John Wood"
  :license "MIT"
  :depends-on (#:cl-ppcre)
  :components ((:file "words")
               (:file "boggle")))

