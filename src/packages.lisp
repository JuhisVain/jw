;;;; packages.lisp

(defpackage #:wtool
  (:use #:cl))

(defpackage #:unit
  (:use #:cl)
  (:export :army
           :make-army
	   :army-x :army-y :army-counter))

(defpackage #:war
  (:use #:cl))

(defpackage #:counter-gen
  (:use :cl)
  (:export :create-nato-symbol
	   :nato-dimension-init
	   :friendly :neutral :unknown :hostile
	   :land :air
	   :infantry :anti-tank :air-defense
	   :mountain
	   :nato-color-init))

