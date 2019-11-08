(in-package :war)

(defmacro funit (movement carry size count)
  `(make-UNIT-STACK
    :TYPE (make-FACTION-UNIT
	   :MOVEMENT ',movement
	   :CARRY-SPACE ,carry
	   :SIZE ,size
	   :NAME "x")
    :COUNT ,count))

(defun t-slowest-movecosts ()
  (let ((*unit-type-movecosts* (make-hash-table :test 'eq)))
    (setf (gethash 'slow *unit-type-movecosts*) '((a 1000)(b 2000)(c 3000)))
    (setf (gethash 'medium *unit-type-movecosts*) '((a 100)(b 200)(c 300)))
    (setf (gethash 'fast *unit-type-movecosts*) '((a 10)(b 20)(c 30)))

    (slowest-movecosts
     (troop-movecarry-data
      (list
       (funit slow 0 1 20)
       (funit slow 0 1 10)
       (funit medium 2 2 5)
       (funit fast 20 10 1)
       )))
    ))

(defun t-set-ready ()
  (dolist (u *testunit*)
    (dolist (uat (army-troops u))
      (setf (unit-stack-readiness uat) 100)
      (when (zerop (unit-stack-count (car (army-troops tu))))
	(setf (unit-stack-count (car (army-troops tu))) 5)))))
