;*********************************************************************
;-------| PULS SYSTEM
;-------| This file is: $LLpls/allpurp.ll
;-------| Version V2.0: Apr 18, 2002 / NOW compatible with omchroma
;-------| By Marco Stroppa
;-------| Copyright 1986 MIT
;*********************************************************************

(in-package :cr)

; GENERAL-PURPOSE FUNCTIONS INDEPENDENT OF THE PARTICULAR TYPE


; Each of the functions automatically dispatches on the type of the data
;	structure to a particular function called:
;		       		   "<ALL-PURPOSE-NAME>_<DATA-TYPE>"

; Recognized types are contained in #:marco:pls:TYPES& (see default.ll)


; AVAILABLE FUNCTIONS:
;	PREDICATES:	is-empty
;	INFO:		sizeof
;			prnt
;			short-prnt


(defun is-empty (datum)
"Build a function called is-empty_<type-of datum> and call it.

;	NAME:		is-empty  (PREDICATE)
;	TYPE:		Expr with 1 argument (list with 1 or more elements)
;	CALL:		(is-empty datum)
;	FUNCTION:	test whether datum is empty
;	VALUE:		t or () depending on the test
;	SOURCE:		$PLSsys/allpurp.ll

"
; NEW VERSION WORKING WITH CHROMA AND OM PACKAGES / Carlos + Marco, 020418
  (let ((name (string-upcase (concatenate 'string "is-empty_" (string (pls-type datum))))))
    (setf name (intern name :cr))
    (funcall (fdefinition name) datum)))


(defun sizeof (datum)
"
;	NAME:		sizeof  (INFO)
;	TYPE:		Expr with 1 or more arguments
;	CALL:		(sizeof datum)
;	FUNCTION:	return the size (or number of elements) of datum
;	VALUE:		the number above
;	SOURCE:		$PLSsys/allpurp.ll

"

   (let* ((datca (car datum))
;         (datcd (cdr datum))
          (name (string-upcase (concatenate 'string "sizeof_" (string (pls-type datum))))))
     (setf name (intern name :cr))
     (funcall (fdefinition name) datum)))
;     (ifn datcd
;          (funcall (fdefinition name) datum)
;       (funcall (fdefinition name) datca datcd))))


(defun prnt (&rest data)
"
;	NAME:		prnt/short-prnt  (INFO)
;	TYPE:		Expr with n arguments
;	CALL:		(prnt datum1 ... datumN)
;			(short-prnt datum1 ... datumN)
;	FUNCTION:	nicely or shortely print data structures
;	VALUE:		the string 'done
;	SOURCE:		$PLSsys/allpurp.ll
"
    (mapc (lambda (datum)
		  (if (is-tagged datum)
                    (let ((name (string-upcase (concatenate 'string "print_" (string (pls-type datum))))))
                      (setf name (intern name :cr))
                      (funcall (fdefinition name) datum))

		      (print datum)))
	  data)
    'done)

(defun short-prnt (&rest data)
"
;	NAME:		prnt/short-prnt  (INFO)
;	TYPE:		Expr with n arguments
;	CALL:		(prnt datum1 ... datumN)
;			(short-prnt datum1 ... datumN)
;	FUNCTION:	nicely or shortely print data structures
;	VALUE:		the string 'done
;	SOURCE:		$PLSsys/allpurp.ll
"
    (mapc (lambda (datum)
		  (if (is-tagged datum)
                    (let ((name (string-upcase (concatenate 'string "short-print_" (string (pls-type datum))))))
                      (setf name (intern name :cr))
                      (funcall (fdefinition name) datum))
		      (print datum)))
	  data)
    'done)