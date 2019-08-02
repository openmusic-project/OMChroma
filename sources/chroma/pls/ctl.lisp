;*****************************************************************************
;-------| PULS SYSTEM
;-------| This file is: $LLsys/ctl.ll
;-------| Version V1.1: Aug 19, 1991
;-------| By Marco Stroppa
;-------| Copyright 1991 IRCAM
;*****************************************************************************

; PACKAGE TO DEAL WITH CONTROL DATA
;                 ASSOCIATED TYPE NAME: CTL

(in-package :cr)

; DESCRIPTION OF THE DATA STRUCTURE:
; Control data are characterized by a KEY and a VALUE associated to the key.
; They are quite similar to mono-dimensional tables, but the retrieval
;    mechanism is more souple and never produces error or warning messages
;    when the specified key does not exist.

; CURRENT IMPLEMENTATION:
; Type CTL is structure of type TBL with built-in controls to avoid error
;    messages.


; AVAILABLE FUNCTIONS:

;	CONSTRUCTORS: make_ctl
;	SELECTORS:    get_ctl
;		      list_ctl
;	MODIFIERS:    set_ctl
;		      rm_ctl
;	PREDICATES:   is_ctl
;		      is-key_ctl
;		      is-empty_ctl
;	INFO:	      print_ctl
;		      short-print_ctl



; DESCRIPTION OF THE PACKAGE:

;	NAME:		make_ctl  (CONSTRUCTOR)
;	TYPE:		Expr with N arguments
;	CALL:		(make_ctl '(key1 val1) ... '(keyN valN))
;	FUNCTION:	define a of type CTL
;	VALUE:		the new structure
;	SOURCE:		$LLsys/ctl.ll

(defun make_ctl (&rest vals)
   (let ((ctl (attach-type 'CTL (make_tbl))) )
      (ifn vals
	   ctl
	   (mapc (lambda (x) (set_ctl ctl (car x) (cadr x)))
		 vals)
	   ctl)
   ))


;	NAME:		get_ctl  (SELECTOR)
;	TYPE:		Expr with 2 arguments
;	CALL:		(get_ctl ctl key)
;	FUNCTION:	return the value associated to key1; it key does not
;			   exist return ()
;	VALUE:		the above value
;	SOURCE:		$LLsys/ctl.ll

(defun get_ctl (ctl key)
   (pls-check-type 'CTL ctl 'get_ctl)
   (if (is-key_ctl ctl key)
       (lookup_tbl (contents ctl) key)
       ()
   ))


;	NAME:		list_ctl  (PREDICATE)
;	TYPE:		Expr with 1 argument
;	CALL:		(list_ctl ctl)
;	FUNCTION:	return a list with all the control keys belonging
;			   to ctl
;	VALUE:		the list above
;	SOURCE:		$LLsys/ctl.ll

(defun list_ctl (ctl)
   (pls-check-type 'CTL ctl 'list_ctl)
   (lkeys_tbl (contents ctl)) )


;	NAME:		sizeof_ctl  (PREDICATE)
;	TYPE:		Expr with 1 argument
;	CALL:		(sizeof_ctl ctl)
;	FUNCTION:	return a the amount of elements
;	VALUE:		the list above
;	SOURCE:		$LLsys/ctl.ll

(defun sizeof_ctl (ctl)
   (pls-check-type 'CTL ctl 'sizeof_ctl)
   (sizeof (contents ctl)) )



;	NAME:		set_/rm_ctl  (MODIFIERS)
;	TYPE:		Expr with 3 / 2 arguments
;	CALL:		(set_ctl ctl key val)
;			(rm_ctl ctl key)
;	FUNCTION:	respectively create (if needed) and set a control key
;			   or delete an already existing key from a structure
;			   of type CTL
;	VALUE:		the symbol 'ok if the control existed, () otherwise
;	SOURCE:		$LLsys/ctl.ll

(defun set_ctl (ctl key val)
   (pls-check-type 'CTL ctl 'set_ctl)
   (insert_tbl (contents ctl) key val) )

(defun rm_ctl (ctl key)
   (pls-check-type 'CTL ctl 'rm_ctl)
   (when (is-key_ctl ctl key)
       (rm_tbl (contents ctl) key)) )
       

;	NAME:		is_ctl  (PREDICATE)
;	TYPE:		Expr with 1 argument
;	CALL:		(is_ctl ctl)
;	FUNCTION:	test whether the argument is a structure of type CTL
;	VALUE:		t or nil according to the test
;	SOURCE:		$LLsys/ctl.ll

(defun is_ctl (ctl)
  (when (is-tagged ctl)
   (eq (pls-type ctl) 'CTL)))


;	NAME:		is-key_ctl  (PREDICATE)
;	TYPE:		Expr with 2 arguments
;	CALL:		(is-key_ctl ctl key)
;	FUNCTION:	test whether the argument is a valid key for a
;			   structure of type TBL
;	VALUE:		key1 [or key2] or nil according to the test
;	SOURCE:		$LLsys/ctl.ll

(defun is-key_ctl (ctl key)
   (pls-check-type 'CTL ctl 'is-key_ctl)
   (is-key_tbl (contents ctl) key) )


;	NAME:		is-empty_ctl  (PREDICATE)
;	TYPE:		Expr with 1 argument
;	CALL:		(is-empty_ctl ctl)
;	FUNCTION:	test whether a structure of type CTL is empty
;	VALUE:		t or nil according to the test
;	SOURCE:		$LLsys/ctl.ll

(defun is-empty_ctl (ctl)
   (pls-check-type 'CTL ctl 'is-empty_ctl)
   (is-empty_tbl (contents ctl)) )


;	NAME:		print_/short-print_ctl  (INFO)
;	TYPE:		Expr with 1 argument
;	CALL:		(print_ctl ctl)
;			(short-print_ctl ctl)
;	FUNCTION:	nicely or shortely print a structure of type CTL
;	VALUE:		the string 'done
;	SOURCE:		$LLsys/ctl.ll

(defun print_ctl (ctl)
   (pls-check-type 'CTL ctl 'print_ctl)
   (print (format ()  "   STRUCTURE OF TYPE ~a MADE OF A " (pls-type ctl)))
   (prnt (contents ctl)) )

(defun short-print_ctl (ctl)
   (pls-check-type 'CTL ctl 'short-print_ctl)
   (prin (pls-type ctl) " / ")
   (short-prnt (contents ctl)) )
