;;;; packages.lisp

(defpackage #:wtool
  (:use #:cl))

(defpackage #:war-army
  (:use #:cl)
  (:export :army
           :make-army))

(defpackage #:war
  (:use #:cl :war-army))


