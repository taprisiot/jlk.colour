;;; -*- mode: Common-Lisp; coding: utf-8-unix -*-

(in-package #:jlk.colour)

;;;
;;; Helper functions
;;;

(defun split-string (str &optional (split-char #\Space))
  (labels ((chars->string (list)
	     (coerce list 'string)))
    (loop
       with words = nil
       with current-word = nil
       for ch across (reverse str)
       when (eq ch split-char)
       do (progn (push (chars->string current-word) words)
       		 (setf current-word nil))
       else
       do (push ch current-word)
       finally (progn (push (chars->string current-word) words)
       		      (return words)))))

(defun name->camel (name)
  "Convert keyword name to camel case"
  (let ((words (split-string (symbol-name name))))
    (intern (apply #'concatenate 'string (mapcar #'string-capitalize words))
	    :keyword)))

(defun u8->d (x)
  "Convert an 8-bit colour value (0-255) to normalised (0.0d0-1.0d0) double-float.

This is supposed to be fast - do not validate arguments."
  (declare (type uint8 x))
  (coerce (/ x 255) 'normalised-double-float))

(defun ud->8 (x)
  "Convert a normalised (0.0d0-1.0d0) double-float colour value to 8-bit (0-255)"
  ;; Not sure if we should use `round' or `floor'
  ;; `round' means that 0.5d0 goes to 128 instead of 127, but values near 1 go to 255
  ;; `floor' means that values near 1.0d0 go to 254, except for 1.0d0 that goes to 255
  (declare (type normalised-double-float x))
  (values (round (* x 255))))

(defun u8->24 (r g b)
  "Convert a RGB value to 24-bit RGB"
  (declare (type uint8 r g b))
  (logior (ash r 16)
	  (ash g 8)
	  b))

(defun u24->8 (x)
  "Convert a 24-bit RGB (hex-encoded) value to 8-bit (0-255)"
  (declare (type uint24 x))
  (values (logand (ash x -16) 255)
	  (logand (ash x -8) 255)
	  (logand x 255)))

(defun ud->24 (r g b)
  (declare (type normalised-double-float r g b))
  (u8->24 (ud->8 r)
	  (ud->8 g)
	  (ud->8 b)))

(defun u24->d (x)
  "Convert a 24-bit RGB (hex-encoded) value to normalised (0.0d0-1.0d0) double-float"
  (declare (type uint24 x))
  (multiple-value-bind (r g b)
      (u24->8 x)
    (values (u8->d r)
	    (u8->d g)
	    (u8->d b))))

(defun u8->32 (r g b a)
  "Convert a RGBA value to 32-bit RGBA"
  (declare (type uint8 r g b a))
  (logior (ash r 24)
	  (ash g 16)
	  (ash b 8)
	  a))

(defun u32->8 (x)
  "Convert a 32-bit RGBA (ie. hex-encoded) value to 8-bit (0-255)"
  (declare (type uint32 x))
  (values (logand (ash x -24) 255)
	  (logand (ash x -16) 255)
	  (logand (ash x -8) 255)
	  (logand x 255)))

(defun ud->32 (r g b a)
  "Convert a normalised-double-float RGBA value to 32-bit RGBA"
  (declare (type normalised-double-float r g b a))
  (u8->32 (ud->8 r)
	  (ud->8 g)
	  (ud->8 b)
	  (ud->8 a)))

(defun u32->d (x)
  "Convert a 32-bit RGBA (ie. hex-encoded) value to normalised (0.0d0-1.0d0) double-float"
  (declare (type uint32 x))
  (multiple-value-bind (r g b a)
      (u32->8 x)
    (values (u8->d r)
	    (u8->d g)
	    (u8->d b)
	    (u8->d a))))
