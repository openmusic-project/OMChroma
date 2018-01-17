;******************************************************************
;-------| CHROMA SYSTEM
;-------| This file is: $LLsys/utils-cr.lisp
;-------| Implemented by Marco Stroppa
;-------| Version: 000817, updated 050221, 060814
;******************************************************************

; THIS FILE CONTAINS THE DEFINITION OF MIXED FUNCTIONS GENERALLY USEFUL EITHER
;   FOR THE CHROMA SYSTEM INTERNALLY ONLY OR BETWEEN CHROMA AND OM

(in-package cr)

;------------------------------------------------------------------
; FUNCTIONS (in alphabetical order):
;	bpf->fun
;	fun->bpf
;	fun->gen-cs-table
;	ran / ran-from / ran%
;	select
;       within-p
;------------------------------------------------------------------
; MACROS (in alphabetical order):
;	cassq
;	ifn
;	nextl
;	newl
;------------------------------------------------------------------
; METHODS! (in alphabetical order):
;	find-chroma-file
;------------------------------------------------------------------


;;;================================================================
;;;; MACROS
; (cassq s l)
(defun cassq (sym l)
  (cdr (assoc sym l)))

; (ifn test body)
(defmacro ifn (testform elseform &body body)
  `(if (not ,testform) ,elseform (progn ,.body)))

; (nextl l [symb])
; (newl l [el])

(defmacro nextl (lst &optional symb)
  (if symb
    `(setq ,symb (pop ,lst))
    `(pop ,lst) ))

#|
(defun nextl (lst &optional symb)
  (if symb
    (setq symb (pop lst))
    (pop lst) ))
|#

(defmacro newl (lst elem) `(push ,elem ,lst))

;------------------------------------------------------------------c


;------------------------------------------------------------------
; fun->bpf

;	NAME:		fun->bpf  (CONSTRUCTOR)
;	TYPE:		Expr with 1 or 2 arguments
;	CALL:		(fun->bpf fun [precision])
;	FUNCTION:	return a bpf object of a given precision (default 0)
;	VALUE:		the bpf
;	SOURCE:		$LLsys/utils.lisp

(defun fun->bpf (fun &optional (precision 5))
"POLYMORPHIC FUNCTION:
   Convert a CHROMA FUN object into a BPF with optional precision.
   If a list of FUNs is given, return a list of BPFs with the same optional precision."
  (if (is_fun fun)
    (om::simple-bpf-from-list (x-list_fun fun) (y-list_fun fun) 'om::bpf precision)
    (fun-list->bpf fun precision)))

(defun fun-list->bpf (fun-list prec)
  (loop for f in fun-list
        collect (fun->bpf f prec)))

; (fun->bpf (make_fun '(0 0 1 1 .5 2)) 3)
; (fun->bpf (list (make_fun '(0 0 1 1 .5 2)) (make_fun '(0 0 1 1 .5 2 1 3))))

;------------------------------------------------------------------
; bpf->fun
;new, ms_1109
;	NAME:		bpf->fun  (CONSTRUCTOR)
;	TYPE:		Expr with 1 argument
;	CALL:		(bpf->fun bpf)
;	FUNCTION:	return a CHROMA FUN object
;	VALUE:		the FUN
;	SOURCE:		$LLsys/utils-cr.lisp

(defun bpf->fun (bpf &optional (precision 5))
"POLYMORPHIC FUNCTION:
   Convert a BPF into a CHROMA FUN object.
   If a list of BPFs is given, return a list of FUNs."
  (if (listp bpf)
      (bpf-list->fun bpf)
    (make_fun
     (loop for x in (om::x-points bpf) for y in (om::y-points bpf)
           append (list y x))))
    )

(defun bpf-list->fun (bpf-list)
  (loop for f in bpf-list
        collect (bpf->fun f)))

;(bpf->fun (om::simple-bpf-from-list '(0 10.002342 20 30 40) '(0 100 50 200 0) 'om::bpf 3))

;(bpf->fun (list (om::simple-bpf-from-list '(0 10.002342 20 30 40) '(0 100 50 200 0) 'om::bpf 3)
;                (om::simple-bpf-from-list '(0 10 20) '(0 100 0) 'om::bpf 3)))

;------------------------------------------------------------------
; fun->cs-gen-table

;	NAME:		fun->gen-cs-table  (CONSTRUCTOR)
;	TYPE:		Expr with 1 or 2 arguments
;	CALL:		(fun->gen-cs-table fun gentype)
;	FUNCTION:	return a gen-cs-table object of a given type
;	VALUE:		the object
;	SOURCE:		$LLsys/utils.lisp

(defun fun->gen-cs-table (fun &optional (gentyp 7) &key (gensize (get-gbl 'DEF-GEN-SIZE)) (expzero (get-gbl 'EXPZERO)))
"POLYMORPHIC FUNCTION:
   Convert a CHROMA FUN object into a GEN-CS-TABLE of type <gentyp>.
   In case of GEN 5 (exponential bpf), replace 0 by expzero.
   If a list of FUNs is given, return a list of objects."
  (if (is_fun fun)
    (make-instance 'om::gen-cs-table
                  :size gensize
                  :gen-num gentyp
                  :param-list (fun-points fun gensize))
    (fun-list->gen-cs-table fun)))

(defun fun-list->gen-cs-table (fun-list gentyp)
  (loop for f in fun-list
        collect (fun->gen-cs-table f gentyp)))

(defmethod fun-points ((fun list) (size integer))
   (let* ((pointx (x-list_fun fun))
          (pointy (y-list_fun fun)))
     (setf pointx (cdr (mapcar 'round (om::om-scale pointx 0 (- size 1)))))
     
     (append (loop for y in pointy
                   for last = 0 then x
                   for x in pointx
                   append (list y (- x last))) (last pointy)) ))

(defun sum-xpts (funpoints)
  "(sum-xpts (fun-points fun size)) should give size-1 if the conversion from
bpf to incremental x-points is correct.
"
  (apply '+ (car (om::list-modulo (cdr funpoints) 2))))


;(setf aaa (fun->gen-cs-table (make_fun '(0 0 100 1000)) 5))
;(sum-xpts (fun-points (make_fun '(0 0 100 1000000 11 29999999)) 513))
;(om::cs-table-string (fun->gen-cs-table (make_fun '(0 0 1 1)) 5) )

; (fun->gen-cs-table (make_fun '(0 0 1 1 .5 2)))
; (fun->gen-cs-table (list (make_fun '(0 0 1 1 .5 2)) (make_fun '(0 0 1 1 .5 2 1 3))))
;------------------------------------------------------------------


;------------------------------------------------------------------

;------------------------------------------------------------------
; select (rewrites a function that disappeared in OM 5)

;	NAME:		select  (SELCTOR)
;	TYPE:		Expr with 2 arguments
;	CALL:		(select <list> <pos>)
;	FUNCTION:	extracts the elements at position <pos> in <list> (1 = first element)
;	VALUE:		the list with the selected elements
;	SOURCE:		$LLsys/utils.lisp

(defun select (list pos)
"Extracts the elements at position <pos> from <list>.
 Contrary to OM's posn-match the first element is number 1."
   (om::posn-match list (om::om- pos 1)))
;(select '(1 2 3 4 5) '(2 3))

;------------------------------------------------------------------
; within-p (el range)
;ms_1109
(defun within-p (el range &optional max)
" If range is a list of two values: return t, if el is contained within range.
  If it is a number, it indicated the min value, then optional is needed,
    and the function returns t if el is between min and max.
If el is a list, test each el of the list.
Ex: (within-p 6000 '(5000 7000))
    (within-p 6000 5000 7000)
    (within-p '(6000 8000) '(5000 7000))
"
   (let ((min (if (numberp range) range (car range)))
         (max (if (numberp range) max (cadr range))))
     (when (> min max) (let ((minn max) (maxx min)) (setf min minn) (setf max maxx)))
     (cond
      ((listp el) (mapcar (lambda (x) (within-p x min max)) el))
      ((numberp el)
       (and (>= el min) (<= el max)))
      (t el))))

;------------------------------------------------------------------

; (ran [itvl] / [itvl var] )
; (ran% [itvl] / [itvl var] )
; (ran-from from to)
;	ran returns a random number comprised between -itvl and +itvl
;	   (-1/+1 if itvl is not present), or between itvl-var and itvl+var
;	ran% is the same as ran, but var is expressed in % [0-1] of itvl

(defun ran (&rest itvl)
  (let* ((a (car itvl))
         (b (cadr itvl))
         (from (ifn a
                    -1.0
                 (ifn b (* a -1) (- a b))))
         (to (ifn a
                  1.0
               (ifn b a (+ a b))))
         (size (abs (- to from)))
         (tmp to)
         )
    (when (> from to)		; SWAP ITVL IF NEEDED
      (setf to from)
      (setf from tmp) )
    (+ from
       (* size
          (/ (random 10000.0) 10000.0)))))

(defun ran-from (from to)
  (let ((itvl (- to from)))
    (+ from (random itvl))))


(defun ran% (&rest itvl)
    (let* ((a (car itvl))
	  (b (cadr itvl))
	  (from (ifn a
		     -1.0
		     (ifn b (* a -1) (- a (* a b)))) )
	  (to (ifn a
		   1.0
		   (ifn b a (+ a (* a b)))) )
	  (size (abs (- to from)) )
	  (tmp to) )

	(when (> from to)		; SWAP ITVL IF NEEDED
	    (setq to from)
	    (setq from tmp) )

	(+ from
	   (* size
	      (/ (random 10000.0) 10000.0))) ))

;------------------------------------------------------------------


(export '(fun->bpf bpf->fun fun->gen-cs-table within-p)
        :chroma)

