;;; -*- mode: Common-Lisp; coding: utf-8-unix -*-

(in-package #:jlk.colour)

(defun example-1 ()
  "Define a custom backend"
  (defclass colour () ((r :initarg :r) (g :initarg :g) (b :initarg :b)))
  (defmethod -make-colour-rgb-8 ((r fixnum) (g fixnum) (b fixnum) (output-format (eql :colour-class)))
    (declare (type uint8 r g b))
    (make-instance 'colour :r r :g g :b b))
  (set-internal-colour-format :colour-class)
  (set-default-palette (make-web-palette))
  (set-default-index (make-index)))
