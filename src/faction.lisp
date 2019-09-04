(in-package :war)

(defstruct faction
  (name (name-init *world*) :type string) ; Faction ID for both humans and program. Some kind of string?
  (controller 'none :type symbol) ; What controls this faction
  (counter-base) ; The sdl:surface used as nato counter card's background
  (relationships) ; list diplomatic statuses with other factions
   ;; ex. (("Usa" . HOSTILE) ("China" . FRIENDLY) ("Martians" . UNKNOWN)) ; use actual structs instead of strings
  (armies) ; List of armies owned by this faction
  (enemy-unit-info (make-hash-table :test 'eq)) ; List of enemies seen on previous / this turn
					;key is enemy army, value is ??
  )

(defstruct unit-info
  "Hashtable value record to use in factions' enemy-unit-info hashtables"
  (was-seen-this-turn t :type boolean))

(defun create-faction (name &key controller (world *world*))
  (when (member-if #'(lambda (faction)
		       (if (string= (faction-name faction) name) t))
		   (world-factions world))
    (setf name (concatenate 'string name "X")))
  (let ((this
	 (make-faction :name name
		       :controller (or controller 'none)
		       :relationships (mapcar #'(lambda (faction) ; Init all to hostile
						  (cons faction 'hostile))
					      (world-factions world))
		       )))
    (dolist (faction (world-factions world))
      (push (cons this 'hostile) (faction-relationships faction)))
    (push this (world-factions world))
    this))

(defun faction-named (name)
  "Returns faction named NAME. NAME may be a quoted symbol or stringand does not care about case."
  (let ((name-string (string-upcase (string name))))
    (dolist (f (world-factions *world*))
      (when (string= name-string
		     (string-upcase (string (faction-name f))))
	(return f)))))

(defun faction-relationship-with (pov-faction other-faction)
  (if (eq pov-faction other-faction)
      (return-from faction-relationship-with 'friendly))
  (dolist (relation (faction-relationships pov-faction))
    (if (eq other-faction (car relation))
	(return-from faction-relationship-with (cdr relation))))
  (format t "~&ERROR : Faction ~a does not know faction ~a!~%"
	  (faction-name pov-faction) (faction-name other-faction))
  'unknown)

(defun name-init (world)
  (format t "~&make-faction call without name keyword!~%")
  (next-variant-id
   (or
    (and (world-factions world)
	 (faction-name (car (world-factions world))))
    "A")))

(defun new-army (faction x y &optional counter-desc)
  (let ((new-army (make-army :x x :y y
			     :owner faction
			     :troops '((commando . 0)) ;; WIP
			     :movement 25 ;; should be generated base on ???something???
			     :counter
			     (make-graphics
			      :surface (description-to-counter faction 40 (or counter-desc '(land analysis)))
			      :x-at 24 :y-at 7))))
    (push new-army (faction-armies faction))
    (place-unit new-army x y)
    new-army))
