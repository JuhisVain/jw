;;;; packages.lisp

(defpackage #:wtool
  (:use #:cl))

(defpackage #:war-army
  (:use #:cl)
  (:export :army
           :make-army))

(defpackage #:war
  (:use #:cl :war-army))

(defpackage #:counter-gen
  (:use :cl)
  (:export :create-nato-symbol
	   :nato-dimension-init
	   :friendly :neutral :unknown :hostile
	   :land :air
	   :infantry :anti-tank :air-defense
	   :mountain))

