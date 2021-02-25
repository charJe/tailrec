(defpackage tailrec-asd
  (:use #:cl #:asdf))
(in-package #:tailrec-asd)

(defsystem #:tailrec
  :description "Guaranteed tail call optimization."
  :author "Charles Jackson <charles.b.jackson@protonmail.com>"
  :license "GPL"
  :version "0"
  :serial t
  :depends-on (#:trivial-with-current-source-form)
  :components ((:file "tailrec")))
