(in-package #:war)

;;; Need to define functions
;;    (load-tiles ())
;;    (set-tile-size (size))

;;; Need to setup variables
;;    *graphics-variants*


;;; (GUGS large-tile-width large-tile-height large-tile-horizontal
;;        small-tile-width small-tile-height small-tile-horizontal
;;        :full ((aaa :large (priority x-ofs y-ofs)
;;                    :small (priority x-ofs y-ofs))
;;               (bbb :large (priority x-ofs y-ofs)
;;                    :small (priority x-ofs y-ofs)))
;;        :border ((stream :large (:north (priority x-ofs y-ofs)
;;                                :north-west (priority x-ofs y-ofs)
;;                                :south-west (priority x-ofs y-ofs))
;;                         :small (priority x-ofs y-ofs))))

;;;; spec notes:
;;;  - All full and overflowing tiles should be named 'NAME_VARIANT_SIZE.png'
;;       all caps
;;       -name of graphics, as used logically within game as identifying symbol for type
;;       -variant's id, proceeding alphabetically from A, ISO basic latin alphabet, Z->AA->AB
;;       -size is either LARGE or SMALL
;;   - All border tiles should be named 'NAME_VARIANT_SIZE_BORDER.png'
;;       as above
;;       -border is one of   N NW SW


(macroexpand-1
 '(gugs 1 2 3 4 5 6
   :full
   ((sea :large (100 -4 -9) :small (0 0 0))
    (grass :large (0 0 0) :small (0 0 0))
    (field :large (25 -4 -9) :small (25 0 0))
    (forest :large (75 -4 -17) :small (75 0 0))
    (city :large (50 -6 -6) :small (50 0 0))
    (suburb :large (50 0 0) :small (50 0 0))
    (swamp :large (1 0 0) :small (1 0 0))
    
    )))

(ggg
 :full
 '((sea :large (100 -4 -9) :small (0 0 0))
   (grass :large (0 0 0) :small (0 0 0))
   (field :large (25 -4 -9) :small (25 0 0))
   (forest :large (75 -4 -17) :small (75 0 0))
   (city :large (50 -6 -6) :small (50 0 0))
   (suburb :large (50 0 0) :small (50 0 0))
   (swamp :large (1 0 0) :small (1 0 0)))

 :border '(rivers maybe roads?)

 :misc
 '((selector :large (200 11 0) :small (200 5 0))
   (missing :large (300 0 0) :small (300 0 0)))
 )

(defun ggg (&rest args)
  (let ((load-tiles-list nil)
	(set-large-list nil)
	(set-small-list nil))

    ;; Process full tiles:
    (do ((head (getf args :full) (cdr head)))
	((null head))
      (let ((head-results (process-gform (car head) 'full)))
	(setf load-tiles-list (nconc (car head-results) load-tiles-list))
	(setf set-large-list (nconc (cadr head-results) set-large-list))
	(setf set-small-list (nconc (caddr head-results) set-small-list))))

    ;; Process border tiles
    ;; todo

    ;; Process miscellaneous graphics:
    (do ((head (getf args :misc) (cdr head)))
	((null head))
      (let ((head-results (process-gform (car head) 'misc)))
	(setf load-tiles-list (nconc (car head-results) load-tiles-list))
      ))

    
    ;;(list load-tiles-list set-large-list set-small-list) ;; just testing TODO remove

    (setf (symbol-function 'load-tiles)
	  (compile nil (append '(lambda ()) load-tiles-list '((set-tile-size 'large)))))

    ;; If there is not set-tile-size func, make dummy func
    ;;   before it's called in load-tiles
    ;;(unless (fboundp 'set-tile-size) ; This will actually have to be forced until old set-tile-size is wiped
    (setf (symbol-function 'set-tile-size)
	  #'(lambda (x) x))
    ;;)

    ;; Generates overflown border graphics required for set-tile-size func:
    ;;(load-tiles) ; Living on the razor's edge


    (do* ((head (getf args :full) (cdr head)))
	 ((null head))
      (let ((defpars
	     (mapcar #'(lambda (direction)
			 (let* ((symbol-name (symbol-name (caar head)))
				(border-sym-name-large
				 (intern (concatenate 'string ; 'field-a-large-border-north-east etc..
						      symbol-name
						      "-A-LARGE-BORDER-"
						      direction)))
				(border-sym-name-small
				 (intern (concatenate 'string
						      symbol-name
						      "-A-SMALL-BORDER-"
						      direction))))
			   (list ;; ((defparameter field-outskirts-ne field-a-large-border-north-east)
			    ;;  (defparameter field-outskirts-ne missing-small)) etc..
			    `(defparameter
				 ,(intern (concatenate 'string
						       symbol-name
						       "-OUTSKIRTS-"
						       (long-dir-short-string direction)))
			       ,(if (boundp border-sym-name-large)
				    border-sym-name-large
				    'placeholder))
			    ;; Using the placeholder to initialize logic symbols should be made redundant
			    ;;   instead make some list for the finalizer functions to check what borders to use

			    `(defparameter
				 ,(intern (concatenate 'string
						       symbol-name
						       "-OUTSKIRTS-"
						       (long-dir-short-string direction)))
			       ,(if (boundp border-sym-name-small)
				    border-sym-name-small
				    'placeholder)))))
		     (list "NORTH" "NORTH-WEST" "NORTH-EAST" "SOUTH" "SOUTH-WEST" "SOUTH-EAST"))))
	(dolist (defpar defpars)
	  (push (car defpar) set-large-list)
	  (push (cadr defpar) set-small-list))))

    (setf (symbol-function 'set-tile-size)
	  (compile nil (append '(lambda (size))
			       '((sdl:clear-display sdl:*black*)
				 (defvar placeholder (make-graphics :surface (sdl:create-surface 0 0))))
			       `((cond ((eq size 'large)
					,@set-large-list)
				       ((eq size 'small)
					,@set-small-list))))))
    
    ))


(defun long-dir-short-string (direction-string)
  (let ((ass-dir '(("NORTH" . "N")
		   ("NORTH-WEST" . "NW")
		   ("NORTH-EAST" . "NE")
		   ("SOUTH" . "S")
		   ("SOUTH-WEST" . "SW")
		   ("SOUTH-EAST" . "SE"))))
    (cdr (assoc direction-string ass-dir :test #'string=))))
    
(defun process-gform (graphics-form graphics-type)
  "Returns '((load-tiles contents) (set-tile-size 'large contents) (setsmall contents))"
  (let* ((load-tiles) (set-large) (set-small) ; Lists to return
	 (symbol (car graphics-form)) ; Base name of graphic
	 (vars-large (getf (cdr graphics-form) :large))
	 (priority-large (car vars-large))
	 (x-ofs-large (cadr vars-large))
	 (y-ofs-large (caddr vars-large))
	 
	 (vars-small (getf (cdr graphics-form) :small))
	 (priority-small (car vars-small))
	 (x-ofs-small (cadr vars-small))
	 (y-ofs-small (caddr vars-small))

	 (variant-list-large
	  (find-variant-files symbol "LARGE")) ; '(field field-a field-b field-c)
	 (variant-list-small
	  (find-variant-files symbol "SMALL")))


    ;; Store variants
    (when (eq graphics-type 'full)
      (push `(push ',variant-list-large *graphics-variants*) load-tiles))

    (setf load-tiles ; Call tile graphics setup for everything found
	  (nconc (form-graphics-setups
		  variant-list-large priority-large x-ofs-large y-ofs-large 'large graphics-type)
		 (form-graphics-setups
		  variant-list-small priority-small x-ofs-small y-ofs-small 'small graphics-type)
		 load-tiles))

    (let ((variant-defpars (form-variant-defpars variant-list-large
						 variant-list-small)))
      (setf
       set-large
       (append (car variant-defpars) set-large)
       set-small
       (append (cadr variant-defpars) set-small)))

    (list load-tiles set-large set-small)
    ))


;; This can not be done at compile time -> redo in ggg
(defmacro gugs (&rest args)

  ;; These list will be used in forming the setup functions:
  (let ((load-tiles-list '((set-tile-size 'large)))
	(set-large-list nil)
	(set-small-list nil))

    (do* ((head (getf args :full) (cdr head)))
	 ((null head))

      (let* ((symbol (caar head)) ;             field
	     
	     (vars-large (getf (cdar head) :large)) ; (25 -4 -9)
	     (priority-large (car vars-large)) ;       25
	     (x-ofs-large (cadr vars-large)) ;            -4
	     (y-ofs-large (caddr vars-large)) ;              -9
	     
	     (vars-small (getf (cdar head) :small))
	     (priority-small (car vars-small))
	     (x-ofs-small (cadr vars-small))
	     (y-ofs-small (caddr vars-small))

	     (variant-list-large
	      (find-variant-files symbol "LARGE")) ; '(field field-a field-b field-c)
	     (variant-list-small
	      (find-variant-files symbol "SMALL")))

	;; Store variants
	(push `(push ',variant-list-large *graphics-variants*) load-tiles-list)

	(setf load-tiles-list ; Call tile graphics setup for everything found
	      (nconc (form-graphics-setups
		       variant-list-large priority-large x-ofs-large y-ofs-large 'large)
		      (form-graphics-setups
		       variant-list-small priority-small x-ofs-small y-ofs-small 'small)
		      load-tiles-list))

	(let ((variant-defpars (form-variant-defpars variant-list-large
						     variant-list-small)))
	  (setf
	   set-large-list
	   (append (car variant-defpars) set-large-list)
	   set-small-list
	   (append (cadr variant-defpars) set-small-list)))
	))
    
    (setf load-tiles-list ; Add some special graphics on top
	  (append '(defun load-tiles ()) ; outrageous
		  '((setf *graphics-variants* nil))
		  '((tile-graphics-setup selector-large 200 11 0))
		  '((tile-graphics-setup selector-small 200 5 0))
		  '((tile-graphics-setup missing-large 300 0 0))
		  '((tile-graphics-setup missing-small 300 0 0))
		  load-tiles-list))

    ;; TODO: form defun set-tile-size, don't return the set-x-lists
    
    
    `(progn ,load-tiles-list
	    (defun set-tile-size (size)
	      "Switches between tile sizes"
	      (sdl:clear-display sdl:*black*)
	      (cond ((equal size 'large)
		     ,@set-large-list)
		    ((equal size 'small)
		     ,@set-small-list))
	    )
    )))


;; (form-variant-defpars (find-variant-files 'field) (find-variant-files 'field "SMALL"))
(defun form-variant-defpars (variant-list-large variant-list-small)
  "Returns list containing two lists containing defparameter statements.
CAR holds large's list and CADR holds small's list."
  (let ((ll-sl (list ()())))
    (dolist (large-var (cdr variant-list-large))
      (push `(defparameter ,large-var ,(abs-til-sym large-var 'large))
	    (car ll-sl))

      (if (member large-var variant-list-small)
	  (push `(defparameter ,large-var ,(abs-til-sym large-var 'small))
		(cadr ll-sl))
	  (push `(defparameter ,large-var missing-small)
		(cadr ll-sl))))
    ll-sl))


(defun form-graphics-setups (variant-list priority x-offset y-offset size-symbol graphics-type)
  "If graphics-type is 'full returns a list containing tile-graphics-setup calls for
elements in (cdr variant-list) of size size-symbol. Graphics-type nil "
  (and
   (or (cdr variant-list) ; return nil if no variants in list
       (eq graphics-type 'misc))
   (let ((variant-setup-list nil))
     (push `(tile-graphics-setup
	     ,(abs-til-sym (car variant-list)
			   size-symbol
			   (cond ((eq graphics-type 'full) 'a)
				 ((eq graphics-type 'misc) nil)))
	     ,priority ,x-offset ,y-offset)
	   variant-setup-list)
     ;; Escape when no variants requested:
     (when (eq graphics-type 'misc) (return-from form-graphics-setups variant-setup-list))
     
     (dolist (variant (cddr variant-list)) ; remaining variants
       (push `(tile-graphics-setup
	       ,(abs-til-sym variant size-symbol)
	       ,priority 0 0)
	     variant-setup-list))
     variant-setup-list)))


(defun abs-til-sym (tile-symbol size-symbol &optional (force-variant nil))
  "Return absolute tile symbol such as FIELD-A-LARGE"
  (if force-variant
      (intern (concatenate 'string (symbol-name tile-symbol)
			   "-" (symbol-name force-variant)
			   "-" (symbol-name size-symbol)))
      (intern (concatenate 'string (symbol-name tile-symbol)
			   "-" (symbol-name size-symbol)))))


(defun find-variant-files (symbol &optional (size-string "LARGE"))
  "Return list containing symbol and variant-names (xxx-A) of found graphics"
  (do* ((return-list (list symbol))
	(var-name (string-upcase (string symbol)))
	(var-id "A" (next-variant-id var-id))
	(file-path (concatenate 'string "./graphics/" var-name "_"))
	;; All tiles should have same large and small variants:
	(file-path-large (concatenate 'string file-path var-id "_" size-string ".png")
			 (concatenate 'string file-path var-id "_" size-string ".png")))
       ((null (probe-file file-path-large)) (reverse return-list))
    (push (intern (concatenate 'string var-name "-" var-id))
	  return-list)))

(defun next-variant-id (old-char-string)
  "Returns next character to be used as variant symbol/file identifier
'A' -> 'B' ; 'Z' -> 'AA' ; 'AA' -> 'AB'"
  (do* ((rev-string (reverse old-char-string))
	(i 0 (incf i))
	(character (char rev-string i) (char rev-string i)))
       (nil)
    (if (char= character #\Z)
	(progn
	  (setf (elt rev-string i) #\A)
	  (if (= (+ i 1) (length rev-string))
	      (progn
		(setf rev-string (concatenate 'string rev-string "A"))
		(return-from next-variant-id (reverse rev-string))
	      )))
	(progn
	  (setf (elt rev-string i) (code-char (1+ (char-code character))))
	  (return-from next-variant-id (reverse rev-string))))))

;; (macroexpand-1 '(grand-unified-graphics-setup (field :large (1 2 3) :small (3 2 1))))
(defmacro OBSOLETEgrand-unified-graphics-setup (&rest args)
  ;; The xxx_A.png files specify primary graphics -> overflowing borders should only be found in those
  
  
  ;; To be used like:
  ;; (gugs (xxx :large (priority x-ofs y-ofs)
  ;;            :small (priority x-ofs y-ofs))
  ;;       etc..)
  ;; should create xxx-a and variants xxx-b xxx-c etc. stored in *graphics-variants*
  (do* ((current args (cdr args))
	(form (car current) (car current))
	(load-tiles-list)
	(set-tile-size-list))
       ((null current) (list load-tiles-list set-tile-size-list))
    (setf load-tiles-list
	  (push `(create-tile ,@form) ; Create the tile-type symbol and
		load-tiles-list))     ; if and only when needed: primary border symbols
				      ; for use in variant assignment in size change func

    (do* ((variant-char "A" (next-variant-char variant-char)) ; You'd think this could be simpler;TODO: make fun
	  (variant-path-large
	   (probe-file (concatenate 'string "./graphics/" (string-upcase (string (car form))) "_"
				    (string variant-char) "_LARGE.png"))
	   (probe-file (concatenate 'string "./graphics/" (string-upcase (string (car form))) "_"
				    (string variant-char) "_LARGE.png")))
	  (variant-path-small
	   (probe-file (concatenate 'string "./graphics/" (string-upcase (string (car form))) "_"
			 (string variant-char) "_SMALL.png"))
	   (probe-file (concatenate 'string "./graphics/" (string-upcase (string (car form))) "_"
				    (string variant-char) "_SMALL.png"))))
	 ((null variant-path-large))
      (setf set-tile-size-list ; will need to duplicate + append the prime large/small variants
	    (push `(,(intern (concatenate 'string
					  (string-upcase (string (car form)))
					  "-" (string variant-char))))
		  set-tile-size-list))
      )
    )
  )

