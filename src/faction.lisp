(in-package :war)

(defstruct (faction
	     (:print-object faction-printer))
  (name (name-init *world*) :type string) ; Faction ID for both humans and program. Some kind of string?
  (controller 'none :type symbol) ; What controls this faction
  (counter-base) ; The sdl:surface used as nato counter card's background
  (color sdl:*black* :type sdl:color)
  (relationships) ; list diplomatic statuses with other factions
   ;; ex. (("Usa" . HOSTILE) ("China" . FRIENDLY) ("Martians" . UNKNOWN)) ; use actual structs instead of strings
  (armies) ; List of armies owned by this faction
  (chain-of-command nil :type (or null supreme-hq))
  (enemy-unit-info (make-hash-table :test 'eq)) ; List of enemies seen on previous / this turn
					;key is enemy army, value is ??
  (unit-types)
  )

(defstruct unit-info
  "Hashtable value record to use in factions' enemy-unit-info hashtables"
  ;; lower visibility = more visible
  (visibility (1+ (random 100)) :type integer) ; Maybe needs to be a bell curve?
  (has-been-seen t :type boolean))

(defun faction-printer (this stream)
  (declare (faction this))
  (format stream "#S(FACTION :name ~s) " (faction-name this)))

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
    (unless (world-current-turn world)
      (setf (world-current-turn world) this))
    this))

(defun faction-named (name)
  "Returns faction named NAME. NAME may be a quoted symbol or string and does not care about case."
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

(defun new-army (faction x y &key troops counter-desc)
  (let* ((coc (new-coc-position faction))
	 (new-army (make-army :x x :y y
			      :owner faction
			      :coc coc
			      :troops (or troops
					  (let ((unit-type (unit-type-by-name "Commando" faction)))
					    (list
					     (if unit-type ; if unit-lists have been setup:
						 (make-unit-stack
						  :type unit-type
						  :count 0)))))
			      :counter
			      (make-graphics
			       :surface (description-to-counter faction 40 (or counter-desc '(land analysis)))
			       :x-at 24 :y-at 7))))
    (setf (oob-element-army coc) new-army)
    (push new-army (faction-armies faction))
    (place-unit new-army x y)
    new-army))

(defun new-coc-position (faction &optional x y)
  "Returns free position in FACTION's chain of command. If both X and Y are given the position
will be under the HQ closest to the coordinates."
  (let ((coc (faction-chain-of-command faction)))
    (cond ((null coc) ; You're the man now, dog!
	   (setf (faction-chain-of-command faction) (make-supreme-hq)))
	  ((and x y)
	   );; TODO
	  (t
	   (car (push (make-oob-pos :superior coc) (supreme-hq-subordinates coc)))))))
