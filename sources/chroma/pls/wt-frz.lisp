;*****************************************************************************
;-------| PULS SYSTEM
;-------| This file is: $LLpls/wt-frz.ll
;-------| Version V1.0: Aug 19, 1991 /  
;-------| By Marco Stroppa
;-------| Copyright 1991 IRCAM
;*****************************************************************************
(in-package cr)

; SPECIFICATION AND CONTROL OF A FREEZING ALGORITHM FOR WT OBJECTS

;                 MUST BE ASSOCIATED TO TYPE WT

; DESCRIPTION OF THE DATA STRUCTURE:
; Set of functions to set and vary control parameters of the freezing
;    algorithm embedded by the function "freeze" in WTsys.ll. The algorithm
;    was designed and implemented by Jan Vandenheede at IRCAM in 1991.
; The algorithm stretches the duration of a sound file so as to fit the
;    duration specified in the score.
; The algorithm is controlled by two parameters: FRZ-CTL, FRZ-MODE.

; FRZ-CTL (MAIN FREEZE CONTROL) [A-list with reserved fields]
; FRZ-CTL specifies all the parameters required by the freeze algorithm to
;   function properly. They consist of the following fields:
;	inc-pt		: incrustation point, i.e. where in the original
;			    sound new segments are to be inserted
;	dur		: duration of the new segment
;	skip		: skip around inc-pt for the new segment
;	end-		: overlap between the new and the current segments
;	ampfac		: amplitude scaler
;	xin/xout	: crossfade at beginning/end of segment
;	first-xout
;	last-xin
;	last-xout	: special cases of the above crossfades
;	min-xin
;	min-xout	: local minimum values - distinct from the ones
;			    specified by (fade_wt ...) which are referred
;			    to the physical sound file - used during the
;			    test of some extreme cases, to avoid too small
;			    values that might produce clicks

; When retrieving the value of any field, both the entire list and the current
;    value are evaluated. This allows for the definition of dynamic controls
;    with some jitter or other functional dependencies.

; Therefore an absolute definition of a control list, such as the one below,
;    is rare:
;	( (inc-pt <sec>)	INCRUSTATION POINT
;	  (dur <sec>)		DURATION OF INCRUSTED SEGMENTS
;	  (skip <sec>)		SKIP POINT FOR EACH NEW SEGMENT
;	  (end- <sec>)		FINAL PORTION OF THE SOUND
;	  (ampfac <0:1>)	AMPLITUDE SCALER FOR EACH NEW SEGMENT
;	  (xin <sec>)		IN/OUT CROSSFADES
;	  (xout <sec>)
;	  (first-xout <sec>)
;	  (last-xin <sec>)
;	  (last-xout <sec>)
;	)

; Most often the control values are defined relative to a global variable,
;   usually set proportional to the duration of the the wt object, like
;   the ovlp (shortcut for overlap) below:
;   (let ((ovlp (* (dur_wt (curr_wt) (my-si_wt)) 0.1)) )
;     `(				OVLP SET TO 10% OF THE DURATION
;	 (inc-pt (* ,ovlp 4.0))		4 TIMES OVLP
;	 (dur (ran (* ,ovlp 2.0)	RANDOM VARIATION AROUND 1/2 OVLP
;		   (* ,ovlp 0.5)) )
;	 (skip (ran (* ,ovlp 4.0)	
;		    ,ovlp) )
;	 (end- ,ovlp)			
;	 (ampfac 1.0)			
;	 (xin ,ovlp)			
;	 (xout ,ovlp)
;	 (first-xout ,ovlp)
;	 (last-xin ,ovlp)
;	 (last-xout ,ovlp)
;	 (min-xin (* (get-gbl 'DURMIN) 0.5) )
;	 (min-xout (* (get-gbl 'DURMIN) 0.5) )
;	)
;    )

; It is important to specify whether a control parameter is defined absolutely
;   (ie once for all with respect to the current wt object) or must vary as
;   a function of the current sample increment.
; In this latter case, the function (my-si_wt) is to be used. It means that
;   the value of the sample increment will be dynamically bound when the
;   wt object is instantiated by WTsys. This function, however, cannot be
;   used outside of that context (see "SPECIAL FEATURES" below).
; There may be any number of freeze control structures associated to a WT
;    object. They will be ordered according to their definition and can be
;    independently retrieved using nth-frz_wt.

; FRZ-MODE (FREEZE MODE) [-1 or 0 or 1]
; It determines the behavior of the freeze algorithm with respect to the
;     current WT object.
;	mode = 1 : call the freeze algorithm when needed
;	mode = 0 : do not freeze, START object at the right action time
;	mode = -1: do not freeze, END object at the right time

; LCO (LOCAL OVERWRITING OF GLOBAL CONTROLS)
; Before being used by the freeze algorithm a WT object must be initialized
;    with the freeze controls. This is usually done by calling
;    (set-frz_wt ...) and (set-frz-mode_wt ...) when defining the object.
; However, all the controls can be locally overwritten by adding a third
;    argument to the FQ field, whose first and second arguments mean the
;    resulting frequency and the reference frequency.
; Those local fields must belong to a structure of type CTL (see ctl.ll).
;	si	 : if t the value of the FQ field is a sample increment,
;		      otherwise it is a frequency
;	flt	 : object's filter
;	frz-mode : freeze mode
;	frz-ctl	 : freeze local control overwrite. This may be:
;			NUM : use the freeze control structure number NUM
;				using nth-frz_wt
;			CTL : control structure as specified above (FRZ-CTL);
;				if not complete, the missing values will be
;				retrieved from the current WT object
; For the usage and the interpretation of LCO see here below.
; Ex. (FQ '(list 1.0 1.0 (make_ctl '(si t) '(flt '(/ (get-gbl 'SR/2 4)))
;				'(frz-ctl 2))) ... )

; SPECIAL FEATURES
; Besides the usual selectors, modifiers, predicates and suppliers of
;    information - constructors do not exist, since this is not a new
;    structure, but some added features to a WT structure - there are some
;    LOCAL and AUTOMATIC SELECTORS.
; Both selectors can be used only from within the freeze algorithm, since they
;    dynamically bind to some local variables linked to the currently selected
;    (active) WT object. They implement the meaning of "my" in object-oriented
;    languages.
; LOCAL SELECTORS supply the value of the currently active WT object
;    (curr_wt), its sample increment (my-si_wt), computed after all the
;    modifications, its added control structure (see above) (my-lco_wt) and
;    the absolute and time with respect to the current beginning of the
;    object (after the computation of any local offsets, if existing)
;    (my-trel_wt, my-tnorm_wt).
; AUTOMATIC SELECTORS perform higher-order computations that are directly used
;    by the freeze algorithm. They all first look whether a LCO exist for the
;    current active object and, if so, whether a local field is defined. If
;    not, they will look at the object's field. If the field is still not
;    defined, they will return the global default.
;    (auto-is-si_wt)	: return t if the current value is a sample increment
;    (auto-frz-mode_wt)
;    (auto-flt_wt)	: return the current value
;    (auto-frz-field)   : if it is a number, look for the field in the
;			   control structure of that number in the current
;			   WT object, else it must be a control structure
;			   (not necessarily complete) in the correct format
; REMARK: si, mode and flt are evaulated once when retrieved
;	: the frz-field is evaluated twice, as usual (see above).

; CURRENT IMPLEMENTATION:
;  Added fields to an already existing structure of type WT.
;    Added fields are:
;	frz-ctl
;	frz-mode

; set global variables in package #:marco:pls
;(set-colon 'PLS)

; AVAILABLE FUNCTIONS:
;	SELECTORS/[MODIFIERS]:	[set-]frz_wt
;				[set-]frz-mode_wt

;	SELECTORS:		first-frz_wt
;				last-frz_wt
;				nth-frz_wt
;				frz-field_wt

;	MODIFIERS:		[fast-freeze_wt]

;	LOCAL SELECTORS:	curr_wt
;				curr-win_wt
;				my-si_wt
;				my-trel_wt
;				my-tnorm_wt
;				seg-dur_wt
;				seg-last-xin_wt

;	AUTOMATIC SELECTORS:	auto-frz-flt_wt
;				auto-frz-mode_wt
;				auto-frz-field_wt
;				auto-is-si_wt

;	PREDICATES:		is-frz_wt

;	INFO:			length-frz_wt
;				prnt-frz_wt
;				short-print-frz_wt

; DESCRIPTION OF THE PACKAGE:


(defun set-frz_wt (wt &rest frz-ctl-lst)
"
;	NAME:		set-frz_wt / fast-frz_wt (CONSTRUCTORS)
;	TYPE:		Expr with 2+ arguments
;	CALL:		(set-frz_wt wt freeze1 ... freezeN)
;			(fast-frz_wt wt freeze1 ... freezen)
;	FUNCTION:	set-frz_wt: define an arbitrary number of freeze
;			   control lists
;			fast-frz_wt: faster specification of controls
;	VALUE:		'ok
;	SOURCE:		$LLpls/freeze-wt.ll

; SET-FRZ_WT
; FOR THE TIME BEING THE CONTROL LIST MUST BE COMPLETE
; IN THE FUTURE A MORE INTELLIGENT ANALYSIS OF THE INPUT PARAMETER WILL BE
;   IMPLEMENTED
; ANY NUMBER OF CONTROL LISTS, SPECIFYING DIFFERENT CONTROL PARAMETERS OF THE
;    FREEZE ALGORITHM, IS POSSIBLE

"
   (pls-check-type 'WT wt 'set-frz_wt)
   (set-frz-mode_wt wt 1)
;   (unless (is-frz_wt wt)   ; IF NOT ALREADY DONE, SET MODE TO 1 BY DEFAULT
;	(set-frz-mode_wt wt 1) )
   (add-field_wt wt 'frz-ctl (apply 'vector frz-ctl-lst))
   'ok)


; FAST-FRZ_WT
; ARGUMENTS ARE A LIST CONTAINING THE INCRUSTATION POINT AND THE VALUE OF
;    THE OVERLAP
; VALUES WILL NOT BE EVALUATED (SO FUNCTIONS WILL BE KEPT AS SUCH AND
;    EVALUATED ONLY WHEN NEEDED)

;(defun fast-freeze_wt (wt &rest fast-frz-lst)
;	(print "NOT IMPLEMENTED YET")
;	'BYE)
;   (pls-check-type 'WT wt 'fast-frz_wt)
;   (let ((result ()) )
;     (while fast-frz-lst
;	(lets ( (curr-ctl (nextl fast-frz-lst))
;		(curr-inc-pt (car curr-ctl))
;		(curr-ovlp (cadr fast-frz-lst))) ))))


;	NAME:		frz-mode_wt (SELECTOR / MODIFIER)
;	TYPE:		Expr with 1/2 arguments
;	CALL:		(frz-mode_wt wt) / (set-frz-mode_wt wt mode)
;	FUNCTION:	get / set the freeze mode (see above)
;	VALUE:		the above value / the symbol 'ok
;	SOURCE:		$LLpls/wt-frz.ll

;[SET-]FRZ-MODE_WT
(defun frz-mode_wt (wt)
	(pls-check-type 'WT wt 'frz-mode_wt)
	(field_wt wt 'frz-mode))
;
(defun set-frz-mode_wt (wt mode)
  (pls-check-type 'WT wt 'set-frz-mode_wt)
  (ifn (member mode '(-1 0 1))
       (error "WRONG MODE, MUST CHOOSE ONLY BETWEEN -1, 0 OR 1 ~a" mode)
	(add-field_wt wt 'frz-mode mode)
	'ok) )


;	NAME:		first/last/nth-frz_wt (SELECTORS)
;	TYPE:		Expr with 1/1/2 arguments
;	CALL:		(first/last-frz_wt wt) / (nth-frz_wt wt pos)
;	FUNCTION:	retrieve a whole control list
;	VALUE:		control list
;	SOURCE:		$LLpls/wt-frz.ll

;FIRST-FRZ_WT
(defun first-frz_wt (wt)
    (pls-check-type 'WT wt 'first-frz_wt)
    (svref (field_wt wt 'frz-ctl) 0) )

;LAST-FRZ_WT
(defun last-frz_wt (wt)
    (pls-check-type 'WT wt 'last-frz_wt)
    (svref (field_wt wt 'frz-ctl)
	  (1- (length-frz_wt wt))) )

;NTH-FRZ_WT
;IF INDEX IS BEYOND THE LIMITS, RETRIEVE THE FIRST/LAST VALUES
(defun nth-frz_wt (wt nn)
    (pls-check-type 'WT wt 'nth-frz_wt)
    (let ((lgth (1- (length-frz_wt wt)))
	  (nn (1- nn)) )
	(cond
	    ((> nn lgth)
	     (svref (field_wt wt 'frz-ctl) lgth))
	    ((< nn 0)
	     (svref (field_wt wt 'frz-ctl) 0))
	    (t
	     (svref (field_wt wt 'frz-ctl) nn))
	)) )


;	NAME:		frz-field_wt (SELECTOR)
;	TYPE:		Expr with 2 arguments
;	CALL:		(frz-field frz-ctl field)
;	FUNCTION:	retrieve the value of field in frz-ctl
;	VALUE:		double evaluation: frz-ctl and field
;	SOURCE:		$LLpls/wt-frz.ll

(defun frz-field_wt (ctl field)
   (eval
     (car (cassq field (eval ctl)))) )


;	NAME:		auto-frz-field_wt / etc. (SELECTORS)
;	TYPE:		Expr with 0 / 1 arguments
;	CALL:		(auto-frz-field field) / (auto-is-si_wt) / etc.
;	FUNCTION:	retrieve the value after intelligent search
;	VALUE:		the above value
;	SOURCE:		$LLpls/wt-frz.ll

; AUTO-IS-SI_WT: RETURN T IF THE VALUE OF THE FQ FIELD IS EXPRESSED IN SI
(defun auto-is-si_wt ()
   (if (and (my-lco_wt)			; IF THERE IS AN OVERWRITE STRUCTURE
	    (is-key_ctl (my-lco_wt) 'si) )	;    AND SI IS DEFINED
       (eval (get_ctl (my-lco_wt) 'si))	; RETRIEVE AND EVAL FIELD SI
       (is-si_wt (curr_wt))	; ELSE LOOK AT THE VALUE IN THE CURR OBJECT
   ))

; AUTO-FLT_WT: RETURN THE VALUE OF THE FILTER FIELD
(defun auto-flt_wt ()
   (if (and (my-lco_wt)			; LOCALLY OVERWRITTEN
	    (get_ctl (my-lco_wt) 'flt))
	(eval (get_ctl (my-lco_wt) 'flt))
	(flt_wt (curr_wt))) )

; AUTO-FRZ-MODE_WT: RETURN THE FREEZE MODE
(defun auto-frz-mode_wt ()
   (cond
   	((and (my-lco_wt)			; LOCALLY OVERWRITTEN
	      (get_ctl (my-lco_wt) 'frz-mode))
	 (eval (get_ctl (my-lco_wt) 'frz-mode)) )
	((is-frz_wt (curr_wt))			; LOOK IN CURRENT OBJECT
	 (eval (frz-mode_wt (curr_wt))) )
	(t					; OTHERWISE GET GLOBAL
	 (get-gbl 'FRZ-MODE))
   ))

; AUTO-FRZ-FIELD_WT: RETURN THE CURRENT FIELD
; REMARK: WHEN THE OBJECT IS FROZEN (I.E. FRZ-CTL AND MODE ARE DEFINED),
;	    IF THE CURRENT FIELD DOES NOT EXIST, IT WILL BE FETCHED FROM
;	    THE DEFAULT CONTROL STRUCTURE
(defun auto-frz-field_wt (field)
"
; AUTO-FRZ-FIELD_WT: RETURN THE CURRENT FIELD
; REMARK: WHEN THE OBJECT IS FROZEN (I.E. FRZ-CTL AND MODE ARE DEFINED),
;	    IF THE CURRENT FIELD DOES NOT EXIST, IT WILL BE FETCHED FROM
;	    THE DEFAULT CONTROL STRUCTURE
"
   (let ((tmp-frz (cond ((null (my-lco_wt)) ())
			((numberp (my-lco_wt)) (my-lco_wt))
			(t (get_ctl (my-lco_wt) 'frz-ctl))) )
	)

				; IF THERE IS NO FRZ-CTL STRUCTURE
      (ifn tmp-frz
			; GET THE VALUE FROM THE OBJECT OR THE GLOBAL DEFAULT
	   (if (is-frz_wt (curr_wt))
	       (let ((fld (frz-field_wt (first-frz_wt (curr_wt)) field) )
		    )
		  (ifn fld		; OBJECT FROZEN, BUT FIELD NOT DEFINED
		       (frz-field_wt (get-gbl 'DEF-FRZ) field)
		       fld)
		)
	       (frz-field_wt (get-gbl 'DEF-FRZ) field) )

				; ELSE THE STRUCTURE IS A NUMBER
	   (if (numberp tmp-frz)

	       (let ((fld (frz-field_wt (nth-frz_wt (curr_wt) tmp-frz) field))
		     )
		   (ifn fld
		       (frz-field_wt (get-gbl 'DEF-FRZ) field)
			fld)
		)
	       
			;    OR A CTL STRUCTURE WHICH MAY CONTAIN OR NOT FIELD
	       (if (cassq field (eval tmp-frz))		; MUST ALWAYS EVALUATE
		   (frz-field_wt tmp-frz field)		; FIELD EXISTS

				; FIELD MUST BE FETCHED FROM OBJ OR DEFAULT
		   (if (is-frz_wt (curr_wt))
		       (let ((fld
				(frz-field_wt (first-frz_wt (curr_wt)) field))
			    )
		       (ifn fld		; OBJECT FROZEN, BUT FIELD UNDEFINED
		          (frz-field_wt (get-gbl 'DEF-FRZ) field)
			  fld)
		       )
		       (frz-field_wt (get-gbl 'DEF-FRZ) field))
		)
	    )
	)
   ))
		   

;				seg-dur_wt
;				seg-last-xin_wt
;	NAME:		curr_wt / my-lco/si/time_wt / etc. (LOCAL SELECTORS)
;	TYPE:		Expr with 0 arguments
;	CALL:		(curr_wt) / (my-si_wt) / (my-trel_wt) / etc.
;	FUNCTION:	return the currently bound WT object / window / LCO
;			       the current sample increment
;			       the starting time [sec] of the current element
;			          with respect to the the beginning of the
;			          object after local adjustements
;			       the starting time normalized with respect to
;				  the total duration
;			REMARK: LOCAL SELCTORS can only be used within
;			   the freezing algorithm, where the needed variables
;			   will be dynamically bound
;	VALUE:		the above value
;	SOURCE:		$LLpls/wt-frz.ll

; CURR_WT
(defun curr_wt ()
  (declare (special current-wt))
    (ifn (boundp 'current-wt)	; BOUND ONLY WITHIN THE ALGORITHM IN WTsys
	 (error "WRONG CONTEXT: THIS FUNCTION CAN ONLY BE USED WHILE FREEZING"
	 'SORRY)
	 current-wt) )

;
; CURR-WIN_WT: RETURN THE CURRENT WINDOW, WHEN SPECIFIED, () OTHERWISE
;	THE WINDOW IS WHAT FOLLOWS THE "wanted-si" "reference-si" IN THE
;	   CWT FIELD OF THE WT INSTRUCTION
;	IT CONTAINS: "local beg-off" "local end-off" ['(xin xout)]
(defun curr-win_wt ()
 (declare (special current-win))
    (ifn (boundp 'current-win)	; BOUND ONLY WITHIN THE ALGORITHM IN WTsys
	 (error  "WRONG CONTEXT: THIS FUNCTION CAN ONLY BE USED WHILE FREEZING"
	 'SORRY)
	 current-win) )

;
; MY-SI_WT : CURRENT SAMPLE INCREMENT
(defun my-si_wt ()
 (declare (special current-si))
    (ifn (boundp 'current-si)
	(error "WRONG CONTEXT: THIS FUNCTION CAN ONLY BE USED WHILE FREEZING"
	'SORRY)
	current-si) )

; MY-LCO_WT : CURRENT CONTROL STRUCTURE (TYPE CTL, OR () IF UNDEFINED)
(defun my-lco_wt ()
 (declare (special current-lco))
    (ifn (boundp 'current-lco)
	(error "WRONG CONTEXT: THIS FUNCTION CAN ONLY BE USED WHILE FREEZING"
	'SORRY)
	current-lco) )

;
; MY-TREL_WT
(defun my-trel_wt ()
 (declare (special current-trelative))
    (ifn (boundp 'current-trelative)
	(error "WRONG CONTEXT: THIS FUNCTION CAN ONLY BE USED WHILE FREEZING"
	'SORRY)
	current-trelative) )
;
; MY-TNORM_WT
(defun my-tnorm_wt ()
   (declare (special current-tnorm))
    (ifn (boundp 'current-tnorm)
	(error "WRONG CONTEXT: THIS FUNCTION CAN ONLY BE USED WHILE FREEZING"
	'SORRY)
	current-tnorm) )



;	NAME:		seg-dur_wt / seg-last-xin_wt (LOCAL SELECTORS)
;	TYPE:		Expr with 0 arguments
;	CALL:		(seg-dur_wt) / (seg-last-xin_wt)
;	FUNCTION:	return the duration and the value of the last xin
;			   of the current segment used by the freeze algorithm
;			REMARK: LOCAL SELCTORS can only be used within
;			   the freezing algorithm, where the needed variables
;			   will be dynamically bound
;	VALUE:		the above value
;	SOURCE:		$LLpls/wt-frz.ll

; SEG-DUR_WT
(defun seg-dur_wt ()
 (declare (special next-dur))
    (ifn (boundp 'next-dur)	; BOUND ONLY WITHIN THE ALGORITHM IN WTsys
	 (error "WRONG CONTEXT: THIS FUNCTION CAN ONLY BE USED WHILE FREEZING"
	 'SORRY)
	 next-dur) )

;
; SEG-LAST-XIN_WT
(defun seg-last-xin_wt ()
  (declare (special last-xin))
    (ifn (boundp 'last-xin)	; BOUND ONLY WITHIN THE ALGORITHM IN WTsys
	 (error "WRONG CONTEXT: THIS FUNCTION CAN ONLY BE USED WHILE FREEZING"
	 'SORRY)
	 last-xin) )



(defun is-frz_wt (wt)
"
;	NAME:		is-frz_wt (PREDICATE)
;	TYPE:		Expr with 1 argument
;	CALL:		(is-frz_wt wt)
;	FUNCTION:	test whether the wt object has a field for freeze
;			   control; if frz-mode = 0, do not check whether
;                          a freeze control structure is present and return t;
;                          otherwise, both structures must be present
;	VALUE:		t or nil according to the test
;	SOURCE:		$LLpls/wt-frz.ll
"
  (pls-check-type 'WT wt 'is-frz_wt)
  (cond ((null (is-field_wt wt 'frz-mode)) nil)
        ((= (frz-mode_wt wt) 1)(is-field_wt wt 'frz-ctl))
        (t t)))


;	NAME:		length-frz_wt/print_/short-print_wt  (INFO)
;	TYPE:		Expr with 1 argument
;	CALL:		(length-frz_wt wt)
;			(print_wt wt)
;			(short-print_wt wt)
;	FUNCTION:	compute how many methods for freeze control exist
;			nicely or shortely print a structure of type TBL
;	VALUE:		the amount of methods or the string 'done
;	SOURCE:		$LLpls/wt-frz.ll

(defun length-frz_wt (wt)
    (pls-check-type 'WT wt 'length-frz_wt)
    (if (is-field_wt wt 'frz-ctl)
	(length (field_wt wt 'frz-ctl))
	(error "I'M NOT SURE THAT A FREEZE CONTROL IS DEFINED FOR ~a"wt)) )

(defun print-frz_wt (wt)
    (pls-check-type 'WT wt 'print-frz_wt)
   ; (print (frz_wt wt)) 
    (print wt)
    )

(defun short-print-frz_wt (wt)
    (pls-check-type 'WT wt 'short-print-frz_wt)
  ;  (print (frz_wt wt))
    (print wt)
    )
