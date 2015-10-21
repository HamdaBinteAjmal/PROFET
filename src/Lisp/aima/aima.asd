;;;; 2014-12-17 23:13:50

(defpackage #:aima-asd
  (:use :cl :asdf))

(in-package :aima-asd)

(defsystem aima
  :name "aima"
  :version "0.1"
  :components ((:file "defpackage")
               (:file "main" :depends-on ("defpackage")))
  :depends-on (:cl-ppcre))
