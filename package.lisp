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

`reset-defaults' can be used to set sane default values.

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
