;*****************************************************************************
;-------| PULS SYSTEM
;-------| This file is: $LLsys/tbl.ll
;-------| Version V1.1: Aug 13, 1991
;-------| By Marco Stroppa
;-------| Copyright 1986 MIT
;*****************************************************************************

(in-package :cr)

; PACKAGE TO DEAL WITH ONE- OR TWO-DIMENSIONAL TABLES
;                 ASSOCIATED TYPE NAME: TBL


; DESCRIPTION OF THE DATA STRUCTURE:
;    The structure consists of a one- or two-dimensional table, the elements
;       of which are reached through one or two keys.  Their contents can
;	be anything at all and is retrieved in the same conditions it has been
;	put in.
;    Note: insertion of new els is done at the beginning of the structure
;          there is no consideration for any kind of orders (see lstep.ll).

; CURRENT IMPLEMENTATION:
;    The structure of type TBL is an A-list of sub-tables with type prefix.
;       Each sub-table is an A-list of keys/values and is preceded by its key.


; AVAILABLE FUNCTIONS:

;	CONSTRUCTORS: make_tbl
;	SELECTORS:    lookup_tbl
;		      soft_lkp_tbl
;		      lkeys_tbl
;	MODIFIERS:    insert_tbl
;		      rm_tbl
;		      merge_tbl
;	PREDICATES:   is_tbl
;		      is-key_tbl
;		      is-empty_tbl
;	INFO:	      print_tbl
;		      short-print_tbl


; DESCRIPTION OF THE PACKAGE:

;	NAME:		make_tbl  (CONSTRUCTOR)
;	TYPE:		Expr with 0, 1, 2 or 3 arguments
;	CALLS:		(make_tbl)
;			(make_tbl key1)
;			(make_tbl key1 val1)
;			(make_tbl key1 key2)
;			(make_tbl key1 key2 val2)
;	FUNCTION:	define a structure of type TBL and fills it if args are given
;	VALUE:		the new structure
;	SOURCE:		$LLsys/tbl.ll

(defun make_tbl (&optional key1 &rest key2)
"
;	NAME:		make_tbl  (CONSTRUCTOR)
;	TYPE:		Expr with 0, 1, 2 or 3 arguments
;	CALLS:		(make_tbl)
;			(make_tbl key1)
;			(make_tbl key1 val1)
;			(make_tbl key1 key2)
;			(make_tbl key1 key2 val2)
;	FUNCTION:	define a structure of type TBL and fills it if args are given
;	VALUE:		the new structure
"
  (if (null key1) (attach-type 'TBL ())
    (Let ((tbl (attach-type 'TBL ())))
      (cond
       ((null key2)
          (insert_tbl tbl key1 ()))
       ((null (cdr key2))
          (insert_tbl tbl key1 (car key2)))
       (t
        (insert_tbl tbl key1 (car key2) (cadr key2)))
       )
      tbl)))


;	NAME:		lookup_tbl  (SELECTOR)
;	TYPE:		Expr with 2 or 3 arguments
;	CALL:		(lookup_tbl tbl key1 [key2])
;	FUNCTION:	return the value associated to key1 (and key2 if the
;			   structure is two-dimensional)
;	VALUE:		the above value
;	NOTE:		if there is no key1 (and key2) an error is produced
;	SOURCE:		$LLsys/tbl.ll

(defun lookup_tbl (tbl key1 &rest key2)
"
;	NAME:		lookup_tbl  (SELECTOR)
;	TYPE:		Expr with 2 or 3 arguments
;	CALL:		(lookup_tbl tbl key1 [key2])
;	FUNCTION:	return the value associated to key1 (and key2 if the
;			   structure is two-dimensional)
;	VALUE:		the above value
;	NOTE:		if there is no key1 (and key2) an error is produced
"
  (unless (is_tbl tbl)
    (error-type 'lookup_tbl tbl))
  (when (is-empty tbl)
    (error "Can't find something in an empty table, sir, come on! ~a" 'PRRRR))
  (let ((key2 (ifn key2 key1 (car key2)))
        (subtable (assoc key1 (contents tbl))))
    (ifn subtable
         (error "We're sorry, the key ~A you're interested in doesn't exist in ~a" key1 tbl)
      (let ((record (assoc key2 (cdr subtable))))
        (ifn record
             (error "We're sorry, the key ~A you're interested in doesn't exist in ~a" key2 tbl)
          (cdr record))))))


;	NAME:		soft-lkp_tbl  (SELECTOR)
;	TYPE:		Expr with 2 or 3 arguments
;	CALL:		(soft-lkp_tbl tbl key1 [key2])
;	FUNCTION:	return the value associated to key1 (and key2 if the
;			   structure is two-dimensional)
;	VALUE:		the above value
;	NOTE:		if there is no key1 (and key2) nil is returned
;	SOURCE:		$LLsys/tbl.ll

(defun soft-lkp_tbl (tbl key1 &rest key2)
"
;	NAME:		soft-lkp_tbl  (SELECTOR)
;	TYPE:		Expr with 2 or 3 arguments
;	CALL:		(soft-lkp_tbl tbl key1 [key2])
;	FUNCTION:	return the value associated to key1 (and key2 if the
;			   structure is two-dimensional)
;	VALUE:		the above value
;	NOTE:		if there is no key1 (and key2) nil is returned
"
  (unless (is_tbl tbl)
    (error-type 'soft-lkp_tbl tbl))
  (if (is-empty tbl)
      nil
    (let ((key2 (ifn key2 key1 (car key2)))
          (subtable (assoc key1 (contents tbl))))
      (ifn subtable
          ()
        (cdr (assoc key2 (cdr subtable)))))))


;	NAME:		lkeys/lels_tbl  (PREDICATE)
;	TYPE:		Expr with 1 argument
;	CALL:		(lkeys/lels_tbl tbl)
;	FUNCTION:	return a list with all the keys/elements belonging to table tbl
;	VALUE:		the list above (ex. (k1 k2 (k3 k31 k32) k4) )
;	SOURCE:		$LLsys/tbl.ll

(defun lkeys_tbl (tbl)
"
;	NAME:		lkeys/lels_tbl  (PREDICATE)
;	TYPE:		Expr with 1 argument
;	CALL:		(lkeys/lels_tbl tbl)
;	FUNCTION:	return a list with all the keys/elements belonging to table tbl
;	VALUE:		the list above (ex. (k1 k2 (k3 k31 k32) k4) )
"
    (unless (is_tbl tbl)
      (error-type 'lkey_tbl tbl))
    (om::flat (kltbl (contents tbl)) 1))

(defun kltbl (ctbl)
  (ifn (car ctbl)
      ()
    (cons
     (let ((key1 (caar ctbl))
           (rest (cdar ctbl)) )
       (if (eq key1 (caar rest))
           (list key1)
         (append
          (let ((result ()) )
            (loop while rest
                  do (newl result (list key1 (car (nextl rest)))) )
            result))
         ))
     (kltbl (cdr ctbl)))))


(defun lels_tbl (tbl)
"
;	NAME:		lkeys/lels_tbl  (PREDICATE)
;	TYPE:		Expr with 1 argument
;	CALL:		(lkeys/lels_tbl tbl)
;	FUNCTION:	return a list with all the keys/elements belonging to table tbl
;	VALUE:		the list above (ex. (k1 k2 (k3 k31 k32) k4) )
"
    (unless (is_tbl tbl)
	    (error-type 'lels_tbl tbl))
    (let ((keys (lkeys_tbl tbl)))
      (loop for key in keys
           collect (if (listp key) (lookup_tbl tbl (car key) (cadr key))
                     (lookup_tbl tbl key)))))


;	NAME:		insert_/rm_tbl  (MODIFIERS)
;	TYPE:		Expr with 3 or 4/2 or 3 arguments
;	CALL:		(insert_tbl tbl key1 [key2] val)
;			(rm_tbl tbl key1 [key2])
;	FUNCTION:	respectively insert a new element or delete an already
;			   existing one from a structure of type TBL
;			if the element to insert already exists, only its
;			   value will be changed
;	VALUE:		the symbol 'ok if element exists, () otherwise
;	SOURCE:		$LLsys/tbl.ll

(defun insert_tbl (tbl key1 &rest rest)
"
;	NAME:		insert_/rm_tbl  (MODIFIERS)
;	TYPE:		Expr with 3 or 4/2 or 3 arguments
;	CALL:		(insert_tbl tbl key1 [key2] val)
;			(rm_tbl tbl key1 [key2])
;	FUNCTION:	respectively insert a new element or delete an already
;			   existing one from a structure of type TBL
;			if the element to insert already exists, only its
;			   value will be changed
;	VALUE:		the symbol 'ok if element exists, () otherwise
"
  (unless (is_tbl tbl)
    (error-type 'insert_tbl tbl))
  (unless (<= (length rest) 2)
    (error "No insertion, sir!  Can't have so many elements ~a" (cons key1 rest)))
  (let ((key2 (ifn (cdr rest) key1 (nextl rest)))
        (val (car rest))
        (subtable (assoc key1 (contents tbl))))

    (ifn subtable
         (progn
           (rplacd tbl
                   (cons (list key1
;                   (list (list key1
                               (cons key2 val))
                         (contents tbl)))
           ())
      (let ((record (print (assoc key2 (cdr subtable)))))
        (ifn record	
             (progn (rplacd subtable
;                            (list (cons key2 val)
                            (print (cons (cons key2 val)
                                  (cdr subtable))))
                    ())
           (rplacd record val)
          )) )
    ) tbl)


(list 1 (cons 'a 3))

(defun rm_tbl (tbl key1 &rest key2)
"
;	NAME:		insert_/rm_tbl  (MODIFIERS)
;	TYPE:		Expr with 3 or 4/2 or 3 arguments
;	CALL:		(insert_tbl tbl key1 [key2] val)
;			(rm_tbl tbl key1 [key2])
;	FUNCTION:	respectively insert a new element or delete an already
;			   existing one from a structure of type TBL
;			if the element to insert already exists, only its
;			   value will be changed
;	VALUE:		the symbol 'ok if element exists, () otherwise
"
  (unless (is_tbl tbl)
    (error-type 'rm_tbl tbl))
  (let* ((key2 (ifn key2 key1 (car key2)))
         (subtable (assoc key1 (contents tbl))))
    (if (null subtable)
      (error "No deletion, sir!  The key ~a you asked for does not exist" key1)
      (rm_tbl-sub1 subtable key1 key2)) 
    (unless (cdr subtable) 		 	; IF SUBTABLE IS NOW EMPTY,
      (rm_tbl-sub2 tbl key1))
    ) tbl)


(defun rm_tbl-sub1 (sbtbl key1 key2)
  (cond
   ((null sbtbl)
    (error "No deletion, sir! The keys ~a you asked for do not exist" (cons key1 key2)))
   ((eq (caadr sbtbl) key2)
    (rplacd sbtbl (cddr sbtbl)))   ; REAL DELETION;
   (t
    (rm_tbl-sub1(cdr sbtbl) key1 key2))))

(defun rm_tbl-sub2 (tbl key1)		;    DELETE IT FROM TABLE
  (if (eq (caadr tbl) key1)
    (rplacd tbl (cddr tbl))
    (rm_tbl-sub2 (cdr tbl) key1)))


;	NAME:		merge_tbl  (MODIFIERS)
;	TYPE:		Expr with at least arguments
;	CALL:		(merge_tbl tbl1 tbl2l [tblN])
;	FUNCTION:	merge the keys and contents of two or more tables
;			if the same key is found, it will overwrite the
;			   previously existing value
;	VALUE:		the new tbl structure
;	SOURCE:		$LLsys/tbl.ll

(defun merge_tbl (&rest tbls)
"
;	NAME:		merge_tbl  (MODIFIERS)
;	TYPE:		Expr with at least arguments
;	CALL:		(merge_tbl tbl1 tbl2l [tblN])
;	FUNCTION:	merge the keys and contents of two or more tables
;			if the same key is found, it will overwrite the
;			   previously existing value
;	VALUE:		the new tbl structure
"
  (mapc (lambda (tbl) (unless (is_tbl tbl)
                        (error-type 'insert_tbl tbl))) tbls)
  (if (< (length tbls) 2)
;    (error "No insertion, sir ~a!  Must have at least two tables: ~a" (get-gbl 'USER) tbls))
      tbls
    (let ((newtbl (make_tbl)))
      (loop for tbl in tbls do
            (mapc (lambda (key val) (if (listp key)
                                        (insert_tbl newtbl (car key) (cadr key) val)
                                      (insert_tbl newtbl key val)))
                  (lkeys_tbl tbl) (lels_tbl tbl)))
      newtbl)))

;	NAME:		is_tbl  (PREDICATE)
;	TYPE:		Expr with 1 argument
;	CALL:		(is_tbl tbl)
;	FUNCTION:	test whether the argument is a structure of type TBL
;	VALUE:		t or nil according to the test
;	SOURCE:		$LLsys/tbl.ll

(defun is_tbl (tbl)
"
;	NAME:		is_tbl  (PREDICATE)
;	TYPE:		Expr with 1 argument
;	CALL:		(is_tbl tbl)
;	FUNCTION:	test whether the argument is a structure of type TBL
;	VALUE:		t or nil according to the test
"
  (when (is-tagged tbl)
   (eq (pls-type tbl) 'TBL)))


;	NAME:		is-key_tbl  (PREDICATE)
;	TYPE:		Expr with 2/3 arguments
;	CALL:		(is-key_tbl tbl key1 [key2])
;	FUNCTION:	test whether the argument is a valid key for a
;			   structure of type TBL
;	VALUE:		key1 [or key2] or nil according to the test
;	SOURCE:		$LLsys/tbl.ll

(defun is-key_tbl (tbl key1 &rest key2)
"
;	NAME:		is-key_tbl  (PREDICATE)
;	TYPE:		Expr with 2/3 arguments
;	CALL:		(is-key_tbl tbl key1 [key2])
;	FUNCTION:	test whether the argument is a valid key for a
;			   structure of type TBL
;	VALUE:		key1 [or key2] or nil according to the test
"
    (unless (is_tbl tbl)
	    (error-type 'rm_tbl tbl))
    (let ((key2 (ifn key2 key1 (car key2)))
	  (subtable (assoc key1 (contents tbl))))
       (when (and subtable (assoc key2 (cdr subtable)) )
	     key2)))


;	NAME:		is-empty_tbl  (PREDICATE)
;	TYPE:		Expr with 1 argument
;	CALL:		(is-empty_tbl tbl)
;	FUNCTION:	test whether a structure of type TBL is empty
;	VALUE:		t or nil according to the test
;	SOURCE:		$LLsys/tbl.ll

(defun is-empty_tbl (tbl)
"
;	NAME:		is-empty_tbl  (PREDICATE)
;	TYPE:		Expr with 1 argument
;	CALL:		(is-empty_tbl tbl)
;	FUNCTION:	test whether a structure of type TBL is empty
;	VALUE:		t or nil according to the test
"
    (null (contents tbl)))


;	NAME:		sizeof_tbl  (PREDICATE)
;	TYPE:		Expr with 1 argument
;	CALL:		(sizeof_tbl tbl)
;	FUNCTION:	returns the number of primary elements in the table
;	VALUE:		one element = 1
;	SOURCE:		$LLsys/tbl.ll

(defun sizeof_tbl (tbl)
"
;	NAME:		sizeof_tbl  (PREDICATE)
;	TYPE:		Expr with 1 argument
;	CALL:		(sizeof_tbl tbl)
;	FUNCTION:	returns the number of primary elements in the table
;	VALUE:		one element = 1
;	SOURCE:		$LLsys/tbl.ll
"
     (length (contents tbl)))

(defun sizeof-all_tbl (tbl)
"
;	NAME:		sizeof-all_tbl  (PREDICATE)
;	TYPE:		Expr with 1 argument
;	CALL:		(sizeof-all_tbl tbl)
;	FUNCTION:	returns the number of all the elements in the table and subtables
;	VALUE:		one element = 1
;	SOURCE:		$LLsys/tbl.ll
"
(loop for i in (contents tbl)
      sum (length (cdr i))))

;	NAME:		print_/short-print_tbl  (INFO)
;	TYPE:		Expr with 1 argument
;	CALL:		(print_tbl tbl)
;			(short-print_tbl tbl)
;	FUNCTION:	nicely or shortely print a structure of type TBL
;	VALUE:		the string 'done
;	SOURCE:		$LLsys/tbl.ll

(defun print_tbl (tbl)
"
;	NAME:		print_/short-print_tbl  (INFO)
;	TYPE:		Expr with 1 argument
;	CALL:		(print_tbl tbl)
;			(short-print_tbl tbl)
;	FUNCTION:	nicely or shortely print a structure of type TBL
;	VALUE:		the string 'done
"
   (print
    (format () "~%Structure of type : ~a~%" (pls-type tbl)))
   (mapc 'prntsbtbl (contents tbl))
   'done)

(defun prntsbtbl (sbtbl)
  (let ((key1 (car sbtbl)))
    (mapc
     (lambda (ssbtbl)
       (if (eq key1 (car ssbtbl))
           (print (format () "  Key: ~a /  Value: ~a~%" key1 (cdr ssbtbl)))
         (print (format () "  Keys: ~a, ~a / Value: ~a~%" key1 (car ssbtbl) (cdr ssbtbl)))))
     (cdr sbtbl))))


(defun short-print_tbl (tbl)
"
;	NAME:		print_/short-print_tbl  (INFO)
;	TYPE:		Expr with 1 argument
;	CALL:		(print_tbl tbl)
;			(short-print_tbl tbl)
;	FUNCTION:	nicely or shortely print a structure of type TBL
;	VALUE:		the string 'done
"
  (print (format () "~a" (pls-type tbl)))
  (mapc 'shrtprntsbtbl (contents tbl))
  'done)

(defun shrtprntsbtbl (sbtbl)
  (let ((key1 (car sbtbl)))
    (mapc
     (lambda (ssbtbl)
       (if (eq key1 (car ssbtbl))
           (print (format () "  ~a / ~a" key1 (cdr ssbtbl)))
           (print (format () "  ~a, ~a / ~a" key1 (car ssbtbl) (cdr ssbtbl)))))
     (cdr sbtbl))))
