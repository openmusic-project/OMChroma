;*****************************************************************************
;-------| PULS SYSTEM
;-------| This file is: $LLpls/type.ll
;-------| Version V1.1: Jan 17, 1990
;-------| By Marco Stroppa
;-------| Copyright 1986 MIT
;*****************************************************************************

; CONSTRUCTOR AND SELECTORS FOR MANIFEST TYPING ARCHITECTURE


; DESCRIPTION OF THE STRATEGY:
;    A detailed and complete description of this architecture is contained
;	in the file $LLpls/README.

; CURRENT IMPLEMENTATION:
;    A tagged datum is always a cons of a type and its contents.

(in-package :cr)

;-----------------------------------------------------------------------------
; DEFAULT STRUCTURES:
;	*TYPES*	    :  list of all the types known by the tagged architecture
;-----------------------------------------------------------------------------

; types DVE, RAW added on March 2000
(defvar *TYPES* '(WT CTL TBL FUN VE DVE RAW))
;-----------------------------------------------------------------------------


; AVAILABLE FUNCTIONS:
;	CONSTRUCTORS: attach-type
;	SELECTORS:    pls-type
;		      contents
;	PREDICATES:   is-tagged


; DESCRIPTION OF THE PACKAGE:

;	NAME:		attach-type  (CONSTRUCTOR)
;	TYPE:		Expr with 2 arguments
;	CALL:		(attach-type type contents)
;	FUNCTION:	define and initialize a data structure of type "type"
;			   and contents "contents"
;	VALUE:		the new data structure
;	SOURCE:		$LLpls/type.ll

(defun attach-type (type contents)
   (cons type contents))


;	NAME:		pls-type/contents  (SELECTORS)
;	TYPE:		Expr with 1 argument
;	CALL:		(pls-type datum) / (contents datum)
;	FUNCTION:	returns the type/contents of datum
;	VALUE:		the above values
;	SOURCE:		$LLpls/type.ll


(defun pls-type (datum)
   (if (is-tagged datum)
       (car datum)
       (error "Bad typed datum ~A " datum)))


(defun contents (datum)
   (if (is-tagged datum)
       (cdr datum)
       (error "Bad typed datum  ~A" datum)))


;	NAME:		is-tagged  (PREDICATE)
;	TYPE:		Expr with 1 argument
;	CALL:		(is-tagged datum)
;	FUNCTION:	test whether datum is a known type
;	VALUE:		the TYPE name or () according to the test
;	SOURCE:		$LLpls/type.ll

(defun is-tagged (datum)
   (when (and (not (atom datum))
	(member (car datum) *TYPES*))
     (car datum)))
;-----------------------------------------------------------------------------
