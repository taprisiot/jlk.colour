;;; -*- mode: Common-Lisp; coding: utf-8-unix -*-

(defpackage #:jlk.colour
  (:use #:common-lisp)
  (:export *internal-colour-format*
	   *default-palette*
	   *default-index*

	   set-internal-colour-format
	   set-default-palette
	   set-default-index
	   make-index

	   get-colour
	   find-colour

	   make-simple-palette
	   make-html4-palette
	   make-web-palette
	   make-x11-palette

	   defcolour

	   make-colour-rgb-8
	   make-colour-rgba-8
	   make-colour-rgb-d
	   make-colour-rgba-d
	   make-colour-rgb-24
	   make-colour-rgba-32

	   reset-defaults)
  (:documentation "A simple colour library for Common Lisp.

To use the library:

First call function `set-internal-colour-format' to match the usage
within your applicaiton or use *internal-colour-format* as a dynamic
variable.

Then call function `set-default-palette' or use `*default-palette*' as
a dynamic binding. Alternatively, `colour' and `defcolour' take an
optional palette argument.

Define an index by calling `set-default-index' and `make-index'.

Colour names are defined using case-sensitive keywords. Lower case
names are used, with an alias automatically created to the camel-case
synonym.

To get a colour from the library, use `get-colour'. Note this will
return the value specified in the palette as `:default-colour' when
not found, or `nil' if undefined.

To search for a colour in the library, use `find-colour'.

To set a new colour use `defcolour' and make-colour-*. It is assumed
you know the type of the input data when you make a colour. You will
also need to re-create the index after defining new colours.

To define a new type of colour, define the methods for conversion
`-make-colour-*-*' from the data type of interest to the custom
format. See example in colour.lisp."))

(in-package #:jlk.colour)

;;; 
;;; Define types
;;; 

(deftype normalised-double-float ()
  '(double-float 0.0d0 1.0d0))

(deftype uint8 ()
  ;; TODO - work out if there is a reason to prefer one over the other
  ;; '(integer 0 255)
  '(unsigned-byte 8))

(deftype uint24 ()
  ;; '(integer 0 16777215)
  '(unsigned-byte 24))

(deftype uint32 ()
  ;; '(integer 0 4294967295)
  '(unsigned-byte 32))

;;;
;;; Define globals
;;; 

(defparameter *internal-colour-format* nil
  "Control how colours are created and stored within the library.

Set the value using function `set-internal-colour-format'.
Valid values are defined by the -make-colour-*-* functions.")

(defparameter *default-palette* nil
  "The default colour palette used by function colour.")

(defparameter *default-index* nil
  "The default index used when searching for colours")

;;;
;;; Helper functions
;;;

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

;;;
;;; Conversion functions
;;;

(defgeneric -make-colour-rgb-8 (r g b output-format)
  (:documentation "Generic dispatch by output-format, see make-colour-rgb8"))

(defgeneric -make-colour-rgba-8 (r g b a output-format)
  (:documentation "Generic dispatch by output-format, see make-colour-rgba-8"))

(defgeneric -make-colour-rgb-d (r g b output-format)
  (:documentation "Generic dispatch by output-format, see make-colour-rgb-d"))

(defgeneric -make-colour-rgba-d (r g b a output-format)
  (:documentation "Generic dispatch by output-format, see make-colour-rgba-d"))

(defun make-colour-rgb-8 (r g b &optional (output-format *internal-colour-format*))
  "Create a colour from the 8-bit (0-255) RGB values provided and
coerce into the format specified by *internal-colour-format*"
  (declare (type uint8 r g b))
  (-make-colour-rgb-8 r g b output-format))

(defun make-colour-rgba-8 (r g b a &optional (output-format *internal-colour-format*))
  "Create a colour from the 8-bit (0-255) RGBA values provided and
coerce into the format specified by *internal-colour-format*"
  (declare (type uint8 r g b a))
  (-make-colour-rgba-8 r g b a output-format))

(defun make-colour-rgb-d (r g b &optional (output-format *internal-colour-format*))
  "Create a colour from the normalised (0.0d0-1.0d0) double-float RGB
values provided and coerce into the format specified by
*internal-colour-format*"
  (declare (type normalised-double-float r g b))
  (-make-colour-rgb-d r g b output-format))

(defun make-colour-rgba-d (r g b a &optional (output-format *internal-colour-format*))
  "Create a colour from the normalised (0.0d0-1.0d0) double-float RGBA
values provided and coerce into the format specified by
*internal-colour-format*"
  (declare (type normalised-double-float r g b a))
  (-make-colour-rgba-d r g b a output-format))

(defun make-colour-rgb-24 (x &optional (output-format *internal-colour-format*))
  "Create a colour from a 24-bit RGB value and coerce into the format
  specified by *internal-colour-format*"
  (declare (type uint24 x))
  (multiple-value-bind (r g b)
      (u24->8 x)
    (make-colour-rgb-8 r g b output-format)))

(defun make-colour-rgba-32 (x &optional (output-format *internal-colour-format*))
  "Create a colour from a 32-bit RGBA value and coerce into the format
  specified by *internal-colour-format*"
  (declare (type uint32))
  (multiple-value-bind (r g b a)
      (u32->8 x)
    (make-colour-rgba-8 r g b a output-format)))

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

(defun get-colour (name &key (palette *default-palette*) (default-colour :|default|))
  "Return a colour from the palette, the value specified by default-colour, or nil.

Second return value indicates if the result returned is the default."
  (declare (type keyword name))
  (let ((colour (gethash name palette nil)))
    (if colour
	(values colour
		nil)
	(if default-colour
	    (values (get-colour default-colour :palette palette :default-colour nil)
		    t)
	    (values nil nil)))))

(defun defcolour (name colour &key (palette *default-palette*))
  "Define a new colour (see make-colour-*) in the palette"
  (declare (type keyword name))
  (setf (gethash name palette) colour))

(defun make-index (&key (palette *default-palette*))
  (loop
     for k being the hash-keys of palette
     collect (cons (symbol-name k) k) into rv
     finally (return (jlk.string-index:make-index rv))))

(defun find-colour (name &optional (index *default-index*))
  "Search for a colour by name"
  (declare (type string name))
  (jlk.string-index:human-search name index))

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

(defmacro make-palette (make-colour-fn &body body)
  "Create a new palette and define colours specified in body.

Body form is (name fn-args) or ((name alias alias...)  fn-args). Alias
will point to the same instance of the colour in memory, which for
compound types can therefore be updated using setf without modifying
the palette.

Colour names are case-sensitive keywords. Specify colours in lower
case with a single space between words. An alias in camel-case will be
created automatically."
  (let ((palette (gensym))
	(colour (gensym)))
    `(let ((,palette (make-hash-table :test #'eq)))
       ,@(loop
	    for (name . args) in body
	    collect `(let ((,colour (funcall ,make-colour-fn ,@args)))
		       ,@(if (listp name)
			     (loop
				for n in name
				collect `(setf (gethash ,n ,palette)
					       ,colour)
				collect `(setf (gethash (name->camel ,n) ,palette)
					       ,colour))
			     `((setf (gethash ,name ,palette)
				     ,colour)
			       (setf (gethash (name->camel ,name) ,palette)
				     ,colour)))))
       (values ,palette))))

(defun make-simple-palette ()
  "Create a simple 8 colour palette using keywords as keys. The
internal format of the colours within the hash-table will depend on
the value in *internal-colour-format*"
  (make-palette #'make-colour-rgb-8
    (:|black| 0 0 0)
    (:|red| 255 0 0)
    (:|green| 0 255 0)
    (:|blue| 0 0 255)
    (:|yellow| 255 255 0)
    (:|magenta| 255 0 255)
    (:|cyan| 0 255 255)
    (:|white| 255 255 255)))

(defun make-html4-palette ()
  "Create a palette of web colours using keywords as
keys (case-sensitive). The internal format of the colours within the
hash-table will depend on the value in *internal-colour-format*

See HTML4: https://www.w3.org/TR/html4/types.html#h-6.5"
  (make-palette #'make-colour-rgb-24
    (:|black| #x000000)
    (:|silver| #xC0C0C0)
    ((:|gray| :|grey|) #x808080)
    (:|white| #xFFFFFF)
    (:|maroon| #x800000)
    (:|red| #xFF0000)
    (:|purple| #x800080)
    (:|fuchsia| #xFF00FF)
    (:|green| #x008000)
    (:|lime| #x00FF00)
    (:|olive| #x808000)
    (:|yellow| #xFFFF00)
    (:|navy| #x000080)
    (:|blue| #x0000FF)
    (:|teal| #x008080)
    (:|aqua| #x00FFFF)))

(defun make-web-palette ()
  "Create a palette of web colours using keywords as
keys (case-sensitive). The internal format of the colours within the
hash-table will depend on the value in *internal-colour-format*

See CSS3: https://www.w3.org/TR/css3-color/"
  (make-palette #'make-colour-rgb-24
    (:|black| #x000000)
    (:|navy| #x000080)
    (:|dark blue| #x00008B)
    (:|medium blue| #x0000CD)
    (:|blue| #x0000FF)
    (:|dark green| #x006400)
    (:|green| #x008000)
    (:|teal| #x008080)
    (:|dark cyan| #x008B8B)
    (:|deep sky blue| #x00BFFF)
    (:|dark turquoise| #x00CED1)
    (:|medium spring green| #x00FA9A)
    (:|lime| #x00FF00)
    (:|spring green| #x00FF7F)
    (:|aqua| #x00FFFF)
    (:|cyan| #x00FFFF)
    (:|midnight blue| #x191970)
    (:|dodger blue| #x1E90FF)
    (:|light sea green| #x20B2AA)
    (:|forest green| #x228B22)
    (:|sea green| #x2E8B57)
    ((:|dark slate gray| :|dark slate grey|) #x2F4F4F)
    (:|lime green| #x32CD32)
    (:|medium sea green| #x3CB371)
    (:|turquoise| #x40E0D0)
    (:|royal blue| #x4169E1)
    (:|steel blue| #x4682B4)
    (:|dark slate blue| #x483D8B)
    (:|medium turquoise| #x48D1CC)
    (:|indigo| #x4B0082)
    (:|dark olive green| #x556B2F)
    (:|cadet blue| #x5F9EA0)
    (:|cornflower blue| #x6495ED)
    (:|rebecca purple| #x663399)
    (:|medium aqua marine| #x66CDAA)
    ((:|dim gray| :|dim grey|) #x696969)
    (:|slate blue| #x6A5ACD)
    (:|olive drab| #x6B8E23)
    ((:|slate gray| :|slate grey|) #x708090)
    ((:|light slate gray| :|light slate grey|) #x778899)
    (:|medium slate blue| #x7B68EE)
    (:|lawn green| #x7CFC00)
    (:|chartreuse| #x7FFF00)
    (:|aquamarine| #x7FFFD4)
    (:|maroon| #x800000)
    (:|purple| #x800080)
    (:|olive| #x808000)
    ((:|gray| :|grey|) #x808080)
    (:|sky blue| #x87CEEB)
    (:|light sky blue| #x87CEFA)
    (:|blue violet| #x8A2BE2)
    (:|dark red| #x8B0000)
    (:|dark magenta| #x8B008B)
    (:|saddle brown| #x8B4513)
    (:|dark sea green| #x8FBC8F)
    (:|light green| #x90EE90)
    (:|medium purple| #x9370DB)
    (:|dark violet| #x9400D3)
    (:|pale green| #x98FB98)
    (:|dark orchid| #x9932CC)
    (:|yellow green| #x9ACD32)
    (:|sienna| #xA0522D)
    (:|brown| #xA52A2A)
    ((:|dark gray| :|dark grey|) #xA9A9A9)
    (:|light blue| #xADD8E6)
    (:|green yellow| #xADFF2F)
    (:|pale turquoise| #xAFEEEE)
    (:|light steel blue| #xB0C4DE)
    (:|powder blue| #xB0E0E6)
    (:|fire brick| #xB22222)
    (:|dark golden rod| #xB8860B)
    (:|medium orchid| #xBA55D3)
    (:|rosy brown| #xBC8F8F)
    (:|dark khaki| #xBDB76B)
    (:|silver| #xC0C0C0)
    (:|medium violet red| #xC71585)
    (:|indian red| #xCD5C5C)
    (:|peru| #xCD853F)
    (:|chocolate| #xD2691E)
    (:|tan| #xD2B48C)
    ((:|light gray| :|light grey|) #xD3D3D3)
    (:|thistle| #xD8BFD8)
    (:|orchid| #xDA70D6)
    (:|golden rod| #xDAA520)
    (:|pale violet red| #xDB7093)
    (:|crimson| #xDC143C)
    (:|gainsboro| #xDCDCDC)
    (:|plum| #xDDA0DD)
    (:|burly wood| #xDEB887)
    (:|light cyan| #xE0FFFF)
    (:|lavender| #xE6E6FA)
    (:|dark salmon| #xE9967A)
    (:|violet| #xEE82EE)
    (:|pale golden rod| #xEEE8AA)
    (:|light coral| #xF08080)
    (:|khaki| #xF0E68C)
    (:|alice blue| #xF0F8FF)
    (:|honey dew| #xF0FFF0)
    (:|azure| #xF0FFFF)
    (:|sandy brown| #xF4A460)
    (:|wheat| #xF5DEB3)
    (:|beige| #xF5F5DC)
    (:|white smoke| #xF5F5F5)
    (:|mint cream| #xF5FFFA)
    (:|ghost white| #xF8F8FF)
    (:|salmon| #xFA8072)
    (:|antique white| #xFAEBD7)
    (:|linen| #xFAF0E6)
    (:|light golden rod yellow| #xFAFAD2)
    (:|old lace| #xFDF5E6)
    (:|red| #xFF0000)
    (:|fuchsia| #xFF00FF)
    (:|magenta| #xFF00FF)
    (:|deep pink| #xFF1493)
    (:|orange red| #xFF4500)
    (:|tomato| #xFF6347)
    (:|hot pink| #xFF69B4)
    (:|coral| #xFF7F50)
    (:|dark orange| #xFF8C00)
    (:|light salmon| #xFFA07A)
    (:|orange| #xFFA500)
    (:|light pink| #xFFB6C1)
    (:|pink| #xFFC0CB)
    (:|gold| #xFFD700)
    (:|peach puff| #xFFDAB9)
    (:|navajo white| #xFFDEAD)
    (:|moccasin| #xFFE4B5)
    (:|bisque| #xFFE4C4)
    (:|misty rose| #xFFE4E1)
    (:|blanched almond| #xFFEBCD)
    (:|papaya whip| #xFFEFD5)
    (:|lavender blush| #xFFF0F5)
    (:|sea shell| #xFFF5EE)
    (:|cornsilk| #xFFF8DC)
    (:|lemon chiffon| #xFFFACD)
    (:|floral white| #xFFFAF0)
    (:|snow| #xFFFAFA)
    (:|yellow| #xFFFF00)
    (:|light yellow| #xFFFFE0)
    (:|ivory| #xFFFFF0)
    (:|white| #xFFFFFF)))

(defun make-x11-palette ()
  "Create a palette of X11 colours using keywords as
keys (case-sensitive). The internal format of the colours within the
hash-table will depend on the value in *internal-colour-format*

See: https://en.wikipedia.org/wiki/X11_color_names"
  ;; TODO
  ;; Note spaces in colour names
  ;; Note additional 4 tone colours
  )

(defun set-internal-colour-format (format)
  "Set how colours are created and storred within the library.

See `*internal-colour-format*'."
  (declare (type keyword format))
  (setf *internal-colour-format* format)
  (values))

(defun set-default-palette (palette)
  "Set the default palette, see make-*-palette"
  (setf *default-palette* palette)
  (values))

(defun set-default-index (index)
  "Set the default index, see make-index"
  (setf *default-index* index)
  (values))

(defun set-default-colour (colour-name &key (palette *default-palette*))
  (let ((colour (get-colour colour-name :palette palette)))
    (if colour
	(setf (gethash :|default| *default-palette*) colour)
	(error "Colour not found in palette.")))
  (values))

(defun reset-defaults ()
  "Define somewhat sensible defaults."
  (set-internal-colour-format :list-rgb-8)
  (set-default-palette (make-simple-palette))
  (set-default-index (make-index)))

(reset-defaults)

(defun example-1 ()
  "Define a custom backend"
  (defclass colour () ((r :initarg :r) (g :initarg :g) (b :initarg :b)))
  (defmethod -make-colour-rgb-8 ((r fixnum) (g fixnum) (b fixnum) (output-format (eql :colour-class)))
    (declare (type uint8 r g b))
    (make-instance 'colour :r r :g g :b b))
  (set-internal-colour-format :colour-class)
  (set-default-palette (make-web-palette))
  (set-default-index (make-index)))
