(defpackage tailrec-asd
  (:use #:cl #:asdf))
(in-package #:tailrec-asd)

(defsystem #:tailrec
  :description "Guaranteed tail call optimization."
  :author "Charles Jackson <charles.b.jackson@protonmail.com>"
  :license "LLGPL"
  :version "0"
  :serial t
  :components ((:file "tailrec")))
