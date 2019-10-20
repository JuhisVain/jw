(in-package :war)

(defstruct general
  (name nil :type string)
  ;; experience, bonuses etc..
  )

(defstruct oob-element
  (army nil :type army))

(defstruct (hq (:include oob-element))
  (general nil :type (or general null))
  (subordinates nil)
  (supply-sources nil)
  )

(defstruct (sub-hq (:include hq))
  (superior :type hq))

(defstruct (oob-pos (:inlude oob-element))
  (superior :type hq))
