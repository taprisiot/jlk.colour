;;; -*- mode: Common-Lisp; coding: utf-8-unix -*-

(in-package #:jlk.colour)

(defun test-make-colour ()
  (loop
     for i from 1
     for internal-format in '(:list-rgb-8
			      :alist-rgb-8
			      :plist-rgb-8
			      :list-rgb-d
			      :alist-rgb-d
			      :plist-rgb-d
			      :rgb-24)
     do (let ((*internal-colour-format* internal-format))
	  (format t "~d.1: ~a~%" i (make-colour-rgb-8 0 128 255))
	  (format t "~d.2: ~a~%" i (make-colour-rgb-d 0.0d0 0.5d0 1.0d0))))
  (loop
     for i from 1
     for internal-format in '(:list-rgba-8
			      :alist-rgba-8
			      :plist-rgba-8
			      :list-rgba-d
			      :alist-rgba-d
			      :plist-rgba-d
			      :rgba-32)
     do (let ((*internal-colour-format* internal-format))
	  (format t "~d.1: ~a~%" i (make-colour-rgba-8 0 128 255 255))
	  (format t "~d.2: ~a~%" i (make-colour-rgba-d 0.0d0 0.5d0 1.0d0 1.0d0)))))
