(in-package :war)

(defstruct faction
  (name "xxx" :type string) ; Faction ID for both humans and program. Some kind of string?
  (controller 'none :type symbol) ; What controls this faction
  (counter-base) ; The sdl:surface used as nato counter card's background
  (relationships) ; list diplomatic statuses with other factions
   ;; ex. (("Usa" . HOSTILE) ("China" . FRIENDLY) ("Martians" . UNKNOWN))
  (armies) ; List of armies owned by this faction
  )

(defun faction-relationship-with (pov-faction other-faction)
  (if (eq pov-faction other-faction)
      (return-from faction-relationship-with 'friendly))
  (dolist (relation (faction-relationships pov-faction))
    (if (string= (faction-name other-faction) (car relation))
	(return-from faction-relationship-with (cdr relation))))
  (format t "~&ERROR : Faction ~a does not know faction ~a!~%"
	  (faction-name pov-faction) (faction-name other-faction))
  'unknown)

