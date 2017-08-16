;;; -*- mode: Common-Lisp; coding: utf-8-unix -*-

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

;;;
;;; Usage functions
;;;

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
  (set-default-index (make-index))
  (set-default-colour :|black|))
