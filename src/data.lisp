(in-package #:war)

(defstruct world
  (width nil)     ;amount of columns
  (height nil)    ;hexes in a column
  ;;If wrapping required on x-axis, column count must be even 
  (map nil)       ;a 2d array
  (factions nil)) ;list of faction structs

(defstruct faction
  (units nil)
  (techs nil))

(defstruct tile
  (type 'sea)
  (variant nil)  ;to be used as variant graphics for coast lines etc.
  (location nil) ;city/resource/airfield etc.
  (river-borders nil)
  (road-links nil)
  (rail-links nil)
  (units nil))


(defstruct army
  (id)
  (x) (y)
  (troops)
  (movement)
  (counter))

(defun place-unit (unit x y)
  (setf (tile-units (aref (world-map *world*) (army-x unit) (army-y unit)))
	(delete unit
		(tile-units (aref (world-map *world*) (army-x unit) (army-y unit)))
		:test #'eq))
  (setf (army-x unit) x)
  (setf (army-y unit) y)
  (pushnew unit (tile-units (aref (world-map *world*) x y))))
