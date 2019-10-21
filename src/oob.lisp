(in-package :war)

(defstruct general
  (name nil :type string)
  ;; experience, bonuses etc..
  )

(defstruct oob-element
  (army nil :type (or null army)))

(defstruct (supreme-hq (:include oob-element))
  (general nil :type (or general null))
  (subordinates nil)
  (supply-sources nil))

(defstruct (sub-hq (:include oob-element))
  (general nil :type (or general null))
  (superior nil :type (or sub-hq supreme-hq))
  (subordinates nil)
  (supply-sources nil))

(defstruct (oob-pos (:include oob-element))
  (superior nil :type (or sub-hq supreme-hq)))
