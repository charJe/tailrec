(defpackage tailrec-llgpl-asd
  (:use #:cl #:asdf))
(in-package #:tailrec-llgpl-asd)

(defsystem #:tailrec-llgpl
  :description "Guaranteed tail call optimization."
  :author "Charles Jackson <charles.b.jackson@protonmail.com>"
  :license "LLGPL"
  :version "0"
  :serial t
  :components ((:file "tailrec")))
