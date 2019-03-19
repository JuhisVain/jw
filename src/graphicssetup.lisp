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
    )))

(defmacro gugs (&rest args)

  ;; These list will be used in forming the setup functions:
  (let ((load-tiles-list nil)
	(set-large-list nil)
	(set-small-list nil))

    (do* ((head (getf args :full) (cdr head)))
	 ((null head))

      (let* ((symbol (caar head))
	     
	     (vars-large (getf (cdar head) :large))
	     (priority-large (car vars-large))
	     (x-ofs-large (cadr vars-large))
	     (y-ofs-large (caddr vars-large))
	     
	     (vars-small (getf (cdar head) :small))
	     (priority-small (car vars-small))
	     (x-ofs-small (cadr vars-small))
	     (y-ofs-small (caddr vars-small))

	     (variant-list-large (find-variant-files symbol "LARGE"))
	     (variant-list-small (find-variant-files symbol "SMALL")))

	(push `(push ,variant-list-large *graphics-variants*) load-tiles-list)

	(setf load-tiles-list
	      (append (form-graphics-setups
		       variant-list-large priority-large x-ofs-large y-ofs-large 'large)
		      (form-graphics-setups
		       variant-list-small priority-small x-ofs-small y-ofs-small 'small)
		      load-tiles-list))

	))
    load-tiles-list
    ))


(defun form-graphics-setups (variant-list priority x-offset y-offset size-symbol)
  "Makes a list containing tile-graphics-setup calls for
elements in (cdr variant-list) of size size-symbol."
  (and
   (cdr variant-list) ; return nil if no variants in list
   (let ((variant-setup-list nil))
     (push `(tile-graphics-setup
	     ,(intern (concatenate 'string (symbol-name (cadr variant-list))
				   "-" (string-upcase (symbol-name size-symbol))))
	     ,priority ,x-offset ,y-offset)
	   variant-setup-list)
     (dolist (variant (cddr variant-list)) ; remaining variants
       (push `(tile-graphics-setup
	       ,(intern (concatenate 'string (symbol-name variant)
				     "-" (string-upcase (symbol-name size-symbol))))
	       ,priority 0 0)
	     variant-setup-list))
     variant-setup-list)))


(defmacro BROKENgugs (large-tile-width large-tile-height large-tile-horizontal
		small-tile-width small-tile-height small-tile-horizontal
		&rest args)
  ;; These list will be used in forming the setup functions:
  (let ((tile-graphics-setup-list nil)
	(set-large-list nil)
	(set-small-list nil))
    
    ;; Iterate through setup forms for full tiles:
    (do* ((head (getf args :full) (cdr head))
	  (symbol (caar head) (caar head))
	  (vars-large (getf (cdar head) :large)
		      (getf (cdar head) :large))
	  (priority-large (car vars-large) (car vars-large))
	  (x-ofs-large (cadr vars-large) (cadr vars-large))
	  (y-ofs-large (caddr vars-large) (caddr vars-large))
	  (vars-small (getf (cdar head) :small)
		      (getf (cdar head) :small))
	  (priority-small (car vars-small) (car vars-small))
	  (x-ofs-small (cadr vars-small) (cadr vars-small))
	  (y-ofs-small (caddr vars-small) (caddr vars-small))
	  (variant-list (find-variant-files symbol) ; Find files releated to current symbol
			(find-variant-files symbol))
	  (variant-list-small (find-variant-files symbol "SMALL")
			      (find-variant-files symbol "SMALL"))
	  )
	 ((null head))

      (push `(push ',variant-list *graphics-variants*) tile-graphics-setup-list)


      (let ((intern-large (intern (concatenate 'string (string (cadr variant-list)) "-LARGE")))
	    (intern-small (intern (concatenate 'string (string (cadr variant-list)) "-SMALL"))))
	;; load-tiles func:
	(push `(tile-graphics-setup
		,intern-large
		,priority-large ,x-ofs-large ,y-ofs-large)
	      tile-graphics-setup-list)
	(push `(tile-graphics-setup
		,intern-small
		,priority-small ,x-ofs-small ,y-ofs-small)
	      tile-graphics-setup-list)

	;; set-tile-size func:
	(push `(defparameter ,(cadr variant-list)
		 ,intern-large)
	      set-large-list)

	(push `(defparameter ,(cadr variant-list)
		 ,intern-small)
	      set-small-list)

	;; THIS will not do anything if these things aren't bound
	;; which they aren't because this binds them
	(dolist (direction (list "-SOUTH" "-SOUTH-EAST" "-SOUTH-WEST"
				 "-NORTH" "-NORTH-EAST" "-NORTH-WEST"))
	  (if (boundp (intern (concatenate 'string (string (cadr variant-list)) "-LARGE-BORDER" direction)))
	      (push `(defparameter
			 ,(intern (concatenate 'string (string symbol) "-BORDER" direction))
		       ,(intern (concatenate 'string (string (cadr variant-list)) "-LARGE-BORDER" direction)))
		    set-large-list))
	  (if (boundp (intern (concatenate 'string (string (cadr variant-list)) "-SMALL-BORDER" direction)))
	      (push `(defparameter
			 ,(intern (concatenate 'string (string symbol) "-BORDER" direction))
		       ,(intern (concatenate 'string (string (cadr variant-list)) "-SMALL-BORDER" direction)))
		    set-small-list)))
	)

      (dolist (variant (cddr variant-list))

	(let ((intern-large (intern (concatenate 'string (string variant) "-LARGE")))
	      (intern-small (intern (concatenate 'string (string variant) "-SMALL"))))
	  ;; load-tiles func:
	  (push `(tile-graphics-setup
		  ,intern-large
		  ,priority-large 0 0)
		tile-graphics-setup-list)
	  (push `(tile-graphics-setup
		  ,intern-small
		  ,priority-small 0 0)
		tile-graphics-setup-list)

	  ;; set-tile-size func:
	  (push `(defparameter ,variant
		   ,intern-large)
		set-large-list)
	  (push `(defparameter ,variant
		   ,intern-small)
		set-small-list)
	  ))

      )

    `(progn
       (defun load-tiles ()
	 (format t "~&Selectors~%")
	 (tile-graphics-setup selector-large 200 11 0)
	 (tile-graphics-setup selector-small 200 5 0)

	 (format t "~&Missing~%")
	 (tile-graphics-setup missing-large 199 0 0)
	 (tile-graphics-setup missing-small 199 0 0)

	 (format t "~&list~%")
	 ,@tile-graphics-setup-list

	 (set-tile-size 'large)
	 )

       (defun set-tile-size (var)
	 (cond ((equal var 'large)
		(defparameter selector selector-large)
		(defparameter missing missing-large)
		,@set-large-list
		)
	       ((equal var 'small)
		(defparameter selector selector-small)
		(defparameter missing missing-small)
		,@set-small-list)
	       ))

       )
    ))

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
(defmacro grand-unified-graphics-setup (&rest args)
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

