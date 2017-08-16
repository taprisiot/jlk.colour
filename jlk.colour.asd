;;; -*- mode: Common-Lisp; coding: utf-8-unix -*-

(asdf:defsystem #:jlk.colour
  :description "A simple colour library for Common Lisp"
  :version "0.1"
  :author "JLK"
  :license "BSD 3-Clause"
  :serial t
  :depends-on (#:jlk.string-index)
  :components ((:file "package")
	       (:file "helpers")
	       (:file "colour")
	       (:file "converters")
	       (:file "tests")
	       (:file "examples")))
