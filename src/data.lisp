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
