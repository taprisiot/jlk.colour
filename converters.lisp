;;; -*- mode: Common-Lisp; coding: utf-8-unix -*-

(in-package #:jlk.colour)

(defmethod -make-colour-rgb-8 ((r fixnum) (g fixnum) (b fixnum) (output-format (eql :list-rgb-8)))
  (declare (type uint8 r g b))
  `(,r ,g ,b))

(defmethod -make-colour-rgb-8 ((r fixnum) (g fixnum) (b fixnum) (output-format (eql :list-rgba-8)))
  (declare (type uint8 r g b))
  `(,r ,g ,b 255))

(defmethod -make-colour-rgb-8 ((r fixnum) (g fixnum) (b fixnum) (output-format (eql :alist-rgb-8)))
  (declare (type uint8 r g b))
  `((:r . ,r) (:g . ,g) (:b . ,b)))

(defmethod -make-colour-rgb-8 ((r fixnum) (g fixnum) (b fixnum) (output-format (eql :alist-rgba-8)))
  (declare (type uint8 r g b))
  `((:r . ,r) (:g . ,g) (:b . ,b) (:a . 255)))

(defmethod -make-colour-rgb-8 ((r fixnum) (g fixnum) (b fixnum) (output-format (eql :plist-rgb-8)))
  (declare (type uint8 r g b))
  `(:r ,r :g ,g :b ,b))

(defmethod -make-colour-rgb-8 ((r fixnum) (g fixnum) (b fixnum) (output-format (eql :plist-rgba-8)))
  (declare (type uint8 r g b))
  `(:r ,r :g ,g :b ,b :a 255))

(defmethod -make-colour-rgb-8 ((r fixnum) (g fixnum) (b fixnum) (output-format (eql :list-rgb-d)))
  (declare (type uint8 r g b))
  `(,(u8->d r) ,(u8->d g) ,(u8->d b)))

(defmethod -make-colour-rgb-8 ((r fixnum) (g fixnum) (b fixnum) (output-format (eql :list-rgba-d)))
  (declare (type uint8 r g b))
  `(,(u8->d r) ,(u8->d g) ,(u8->d b) 1.0d0))

(defmethod -make-colour-rgb-8 ((r fixnum) (g fixnum) (b fixnum) (output-format (eql :alist-rgb-d)))
  (declare (type uint8 r g b))
  `((:r . ,(u8->d r)) (:g ,(u8->d g)) (:b ,(u8->d b))))

(defmethod -make-colour-rgb-8 ((r fixnum) (g fixnum) (b fixnum) (output-format (eql :alist-rgba-d)))
  (declare (type uint8 r g b))
  `((:r . ,(u8->d r)) (:g ,(u8->d g)) (:b ,(u8->d b)) (:a 1.0d0)))

(defmethod -make-colour-rgb-8 ((r fixnum) (g fixnum) (b fixnum) (output-format (eql :plist-rgb-d)))
  (declare (type uint8 r g b))
  `(:r ,(u8->d r) :g ,(u8->d g) :b ,(u8->d b)))

(defmethod -make-colour-rgb-8 ((r fixnum) (g fixnum) (b fixnum) (output-format (eql :plist-rgba-d)))
  (declare (type uint8 r g b))
  `(:r ,(u8->d r) :g ,(u8->d g) :b ,(u8->d b) :a 1.0d0))

(defmethod -make-colour-rgb-8 ((r fixnum) (g fixnum) (b fixnum) (output-format (eql :rgb-24)))
  (declare (type uint8 r g b))
  (u8->24 r g b))

(defmethod -make-colour-rgba-8 ((r fixnum) (g fixnum) (b fixnum) (a fixnum) (output-format (eql :list-rgba-8)))
  (declare (type uint8 r g b a))
  `(,r ,g ,b ,a))

(defmethod -make-colour-rgba-8 ((r fixnum) (g fixnum) (b fixnum) (a fixnum) (output-format (eql :alist-rgba-8)))
  (declare (type uint8 r g b a))
  `((:r . ,r) (:g . ,g) (:b . ,b) (:a . ,a)))

(defmethod -make-colour-rgba-8 ((r fixnum) (g fixnum) (b fixnum) (a fixnum) (output-format (eql :plist-rgba-8)))
  (declare (type uint8 r g b a))
  `(:r ,r :g ,g :b ,b :a ,a))

(defmethod -make-colour-rgba-8 ((r fixnum) (g fixnum) (b fixnum) (a fixnum) (output-format (eql :list-rgba-d)))
  (declare (type uint8 r g b a))
  `(,(u8->d r) ,(u8->d g) ,(u8->d b) ,(u8->d a)))

(defmethod -make-colour-rgba-8 ((r fixnum) (g fixnum) (b fixnum) (a fixnum) (output-format (eql :alist-rgba-d)))
  (declare (type uint8 r g b a))
  `((:r ,(u8->d r)) (:g ,(u8->d g)) (:b ,(u8->d b)) (:a ,(u8->d a))))

(defmethod -make-colour-rgba-8 ((r fixnum) (g fixnum) (b fixnum) (a fixnum) (output-format (eql :plist-rgba-d)))
  (declare (type uint8 r g b a))
  `(:r ,(u8->d r) :g ,(u8->d g) :b ,(u8->d b) :a ,(u8->d a)))

(defmethod -make-colour-rgba-8 ((r fixnum) (g fixnum) (b fixnum) (a fixnum) (output-format (eql :rgba-32)))
  (declare (type uint8 r g b a))
  (u8->32 r g b a))

(defmethod -make-colour-rgb-d ((r double-float) (g double-float) (b double-float) (output-format (eql :list-rgb-8)))
  (declare (type normalised-double-float r g b))
  `(,(ud->8 r) ,(ud->8 g) ,(ud->8 b)))

(defmethod -make-colour-rgb-d ((r double-float) (g double-float) (b double-float) (output-format (eql :alist-rgb-8)))
  (declare (type normalised-double-float r g b))
  `((:r . ,(ud->8 r)) (:g . ,(ud->8 g)) (:b . ,(ud->8 b))))

(defmethod -make-colour-rgb-d ((r double-float) (g double-float) (b double-float) (output-format (eql :plist-rgb-8)))
  (declare (type normalised-double-float r g b))
  `(:r ,(ud->8 r) :g ,(ud->8 g) :b ,(ud->8 b)))

(defmethod -make-colour-rgb-d ((r double-float) (g double-float) (b double-float) (output-format (eql :list-rgb-d)))
  (declare (type normalised-double-float r g b))
  `(,r ,g ,b))

(defmethod -make-colour-rgb-d ((r double-float) (g double-float) (b double-float) (output-format (eql :alist-rgb-d)))
  (declare (type normalised-double-float r g b))
  `((r . ,r) (g . ,g) (b . ,b)))

(defmethod -make-colour-rgb-d ((r double-float) (g double-float) (b double-float) (output-format (eql :plist-rgb-d)))
  (declare (type normalised-double-float r g b))
  `(:r ,r :g ,g :b ,b))

(defmethod -make-colour-rgb-d ((r double-float) (g double-float) (b double-float) (output-format (eql :rgb-24)))
  (declare (type normalised-double-float r g b))
  (ud->24 r g b))

(defmethod -make-colour-rgba-d ((r double-float) (g double-float) (b double-float) (a double-float) (output-format (eql :list-rgba-8)))
  (declare (type normalised-double-float r g b a))
  `(,(ud->8 r) ,(ud->8 g) ,(ud->8 b) ,(ud->8 a)))

(defmethod -make-colour-rgba-d ((r double-float) (g double-float) (b double-float) (a double-float) (output-format (eql :alist-rgba-8)))
  (declare (type normalised-double-float r g b a))
  `((:r . ,(ud->8 r)) (:g . ,(ud->8 g)) (:b . ,(ud->8 b)) (:a . ,(ud->8 a))))

(defmethod -make-colour-rgba-d ((r double-float) (g double-float) (b double-float) (a double-float) (output-format (eql :plist-rgba-8)))
  (declare (type normalised-double-float r g b a))
  `(:r ,(ud->8 r) :g ,(ud->8 g) :b ,(ud->8 b) :a ,(ud->8 a)))

(defmethod -make-colour-rgba-d ((r double-float) (g double-float) (b double-float) (a double-float) (output-format (eql :list-rgba-d)))
  (declare (type normalised-double-float r g b a))
  `(,r ,g ,b ,a))

(defmethod -make-colour-rgba-d ((r double-float) (g double-float) (b double-float) (a double-float) (output-format (eql :alist-rgba-d)))
  (declare (type normalised-double-float r g b a))
  `((r . ,r) (g . ,g) (b . ,b) (a . ,a)))

(defmethod -make-colour-rgba-d ((r double-float) (g double-float) (b double-float) (a double-float) (output-format (eql :plist-rgba-d)))
  (declare (type normalised-double-float r g b a))
  `(:r ,r :g ,g :b ,b :a ,a))

(defmethod -make-colour-rgba-d ((r double-float) (g double-float) (b double-float) (a double-float) (output-format (eql :rgba-32)))
  (declare (type normalised-double-float r g b a))
  (ud->32 r g b a))
