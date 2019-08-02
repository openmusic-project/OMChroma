;*****************************************************************************
;-------| PULS SYSTEM
;-------| This file is: $LLpls/ve.lisp
;-------| Version V1.0: Feb 6, 1990
;-------| Modified: Mar 2000 (added type DVE), revised 1003
;-------| Revised 1003, eliminated DVE, changed dyn allocation
;-------| By Marco Stroppa
;-------| Copyright 1990 IRCAM
;*****************************************************************************
(in-package :cr)

; PACKAGE TO DEAL WITH VIRTUAL ENVELOPES
;                 ASSOCIATED TYPE NAME: VE


; DESCRIPTION OF THE DATA STRUCTURE:
; A Virtual Envelope consists of a break-point function of type FUN
;   with an associated internal number that will be used when converting
;   virtual envelopes to structures recognized by real synthesizers.

;MODIF 2000
; In case the structure is not recognized (e.g. an object of type "fun" 
;    and not "ve" for csound), appropriate synthesizer-dependent actions
;    are taken (new 1003):
; csound:
;    transform "fun" into a gen-cs-table of type 7 or 5 and use OMChroma
;       dynamic table allocation

; NB: the old variables GEN-MIN, GEN-MAX, GEN-CURR, GEN-FILE and the type DVE
;        have been deactivated


; CURRENT IMPLEMENTATION:
; cons of FUN and number


; AVAILABLE FUNCTIONS:

;	CONSTRUCTORS: make_ve
;	SELECTORS:    fun_ve
;		      num_ve
;		      s_ve
;	PREDICATES:   is_ve
;                     is-empty_ve
;	INFO:	      print_ve
;		      short-print_ve



; DESCRIPTION OF THE PACKAGE:
(defun make_ve (fun &optional num)
  "
;	NAME:		make_ve  (CONSTRUCTOR)
;	TYPE:		Expr with 1 or 2 arguments
;	CALL:		(make_ve fun [num])
;	FUNCTION:	define and initialize a structure of type VE
;			<fun> must be of type FUN
;	VALUE:		the new structure
"
  (pls-check-type 'FUN fun 'make_ve)
  (attach-type 'VE (cons num fun)) )


(defun fun_ve (ve)
"
;	NAME:		fun_ / num_ / name_ve  (SELECTORS)
;	TYPE:		Expr with 1 argument
;	CALL:		(fun ... _ve ve)
;	FUNCTION:	return the function, the number and a string derived
;			   from the number of ve
;	VALUE:		the new data
"
    (pls-check-type 'VE ve 'fun_ve)
    (cdr (contents ve)) )

(defun num_ve (ve)
"
;	NAME:		fun_ / num_ / name_ve  (SELECTORS)
;	TYPE:		Expr with 1 argument
;	CALL:		(fun ... _ve ve)
;	FUNCTION:	return the function, the number and a string derived
;			   from the number of ve
;	VALUE:		the new data
"
    (pls-check-type 'VE ve 'num_ve)
    (car (contents ve)))


(defun name_ve (ve)
"
;	NAME:		fun_ / num_ / name_ve  (SELECTORS)
;	TYPE:		Expr with 1 argument
;	CALL:		(fun ... _ve ve)
;	FUNCTION:	return the function, the number and a string derived
;			   from the number of ve
;	VALUE:		the new data
"
    (pls-check-type 'VE ve 'name_ve)
    (catenate 've_ (num_ve ve)) )





;	NAME:		is_ve  (PREDICATE)
;	TYPE:		Expr with 1 argument
;	CALL:		(is_ve ve)
;	VECTION:	test whether the argument is a structure of type VE
;	VALUE:		t or nil according to the test
;	SOURCE:		$LLpls/ve.ll

(defun is_ve (ve)
  (when (is-tagged ve)
   (eq (pls-type ve) 'VE)))


;	NAME:		print_/short-print_ve  (INFO)
;	TYPE:		Expr with 1 argument
;	CALL:		(print_ve ve)
;			(short-print_ve ve)
;	VECTION:	nicely or shortely print a structure of type VE
;	VALUE:		the string 'done
;	SOURCE:		$LLpls/ve.ll

(defun print_ve (ve)
    (print (format () "VIRTUAL ENVELOPE - STRUCTURE OF TYPE : ~a" (pls-type ve)))
    (print (format () "        NUM  = ~a" (num_ve ve)))
    (print (format () "        NAME = ~a" (name_ve ve)))
    (print (format () "        FUN ---> "))
    (prnt (fun_ve ve)) )

(defun short-print_ve (ve)
    (print (format () "VE: NUM  = ~a" (num_ve ve)))
    (print (format () "    NAME = ~a" (name_ve ve)))
    (print (format () "    FUN ---> "))
    (short-prnt (fun_ve ve)) )
