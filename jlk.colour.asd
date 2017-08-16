;;; -*- mode: Common-Lisp; coding: utf-8-unix -*-

(asdf:defsystem #:jlk.colour
  :description "A simple colour library for Common Lisp"
  :version "0.1"
  :author "JLK"
  :license "2-Clause BSD"
  :serial t
  :depends-on (#:jlk.string-index)
  :components ((:file "colour")))
