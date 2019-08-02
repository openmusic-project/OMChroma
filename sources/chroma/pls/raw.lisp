;*****************************************************************************
;-------| PULS SYSTEM
;-------| This file is: $LLsys/raw.ll
;-------| Version V1.0: Mar 29, 2000
;-------| By Marco Stroppa
;-------| Copyright 2000 IRCAM
;*****************************************************************************

; PACKAGE TO DEAL WITH RAW DATA
;                 ASSOCIATED TYPE NAME: raw

(in-package :cr)

; DESCRIPTION OF THE DATA STRUCTURE:

; Raw data are context-dependent and are stored and retrieved as such without
;    any special operation

; CURRENT IMPLEMENTATION:
; Type RAW is a simple cons of type and data


; AVAILABLE FUNCTIONS:

;	CONSTRUCTORS: make_raw
;     [ SELECTORS   : contents ]
;	PREDICATES  : is_raw
;		      is-empty_raw
;	INFO        : print_raw
;		      short-print_raw



; DESCRIPTION OF THE PACKAGE:

;	NAME:		make_raw  (CONSTRUCTOR)
;	TYPE:		Expr with N arguments
;	CALL:		(make_raw data)
;	FUNCTION:	define a of type RAW, data can be anything
;	VALUE:		the new structure with the data contained in a list
;	SOURCE:		$LLsys/raw.ll

(defun make_raw (&rest vals)
  (attach-type 'RAW vals))



;	NAME:		is_raw  (PREDICATE)
;	TYPE:		Expr with 1 argument
;	CALL:		(is_raw data)
;	FUNCTION:	test whether the argument is a structure of type RAW
;	VALUE:		t or nil according to the test
;	SOURCE:		$LLsys/raw.ll

(defun is_raw (data)
  (when (is-tagged data)
   (eq (pls-type data) 'RAW)))


;	NAME:		is-empty_raw  (PREDICATE)
;	TYPE:		Expr with 1 argument
;	CALL:		(is-empty_raw data)
;	FUNCTION:	test whether a structure of type RAW is empty
;	VALUE:		t or nil according to the test
;	SOURCE:		$LLsys/raw.ll

(defun is-empty_raw (data)
   (pls-check-type 'RAW data 'is-empty_raw)
   (let ((data (contents data)))
         (if (is-tagged data)
             (is-empty data)
           (null data))))

;	NAME:		sizeof_raw  (PREDICATE)
;	TYPE:		Expr with 1 argument
;	CALL:		(sizeof_raw data)
;	FUNCTION:	return the amount of items in data
;	VALUE:		a number
;	SOURCE:		$LLsys/raw.ll

(defun sizeof_raw (data)
   (pls-check-type 'RAW data 'sizeof_raw)
   (let ((data (contents data)))
     (if data
       (length data)
       0)))

;	NAME:		print_/short-print_raw  (INFO)
;	TYPE:		Expr with 1 argument
;	CALL:		(print_raw raw)
;			(short-print_raw raw)
;	FUNCTION:	nicely or shortely print a structure of type RAW
;	VALUE:		the string 'done
;	SOURCE:		$LLsys/raw.ll

(defun print_raw (data)
   (pls-check-type 'RAW data 'print_raw)
   (print (format ()  "   STRUCTURE OF TYPE : ~a DATA CONSISTING OF ~a~%"
           (pls-type data) (contents data))))

(defun short-print_raw (data)
   (pls-check-type 'RAW data 'short-print_raw)
   (print (format () "~a / " (pls-type data)))
   (short-prnt (contents data)) )
