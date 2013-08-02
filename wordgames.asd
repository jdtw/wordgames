;;;; wordgames.asd

(asdf:defsystem #:wordgames
  :serial t
  :description "Contains a boggle solver and an anagram solver"
  :author "John Wood"
  :license "MIT"
  :depends-on (#:cl-ppcre)
  :components ((:file "words")
               (:file "boggle")
               (:file "anagram")))

