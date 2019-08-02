;******************************************************************
;-------| CHROMA SYSTEM
;-------| This file is: $LLsys/utils.lisp
;-------| Implemented by Marco Stroppa
;-------| Version: 000817, updated 050221, 060814
;******************************************************************

; THIS FILE CONTAINS THE DEFINITION OF MIXED FUNCTIONS GENERALLY USEFUL EITHER
; FOR THE CHROMA SYSTEM INTERNALLY

(in-package :cr)

(export '(within-p) :chroma)


;;; The same function exist in OM
;;; here we have them in the CR package
(defun lintodb (x)
  (let ((y (if (= 0.0 x) 0.00000001 x)))
    (* (log y 10) 20)))

(defun dbtolin (x)
  (expt 10.0 (/ x 20.0)))


;------------------------------------------------------------------
; FUNCTIONS (in alphabetical order):
;	ran / ran-from / ran%
;	select
; within-p
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

(defmacro newl (lst elem) 
  `(push ,elem ,lst))

;------------------------------------------------------------------c

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
"Extracts the elements at position <pos> (STARTING AT 1) from <list>.
 Contrary to OM's posn-match the first element is number 1."
   (if (listp pos)
       (loop for p in pos collect (select list p))
     (nth (1- pos) list)))

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
; printdate / stringdate
;	nicely print the current date (calling the lisp function date)

(defun printdate (&optional (outchan t))
  "Nicely print the current date (calling the lisp function get-decoded-time).
It can send its output to another output channel if an argument is present."
  (multiple-value-bind
    (second minute hour date month year)
    (get-decoded-time)
    (format outchan "~a ~a, ~a - AT ~a:~a (~a sec)" month date year hour minute second)))

(defun stringdate ()
  "Nicely return a stream with the current date (calling the lisp function get-decoded-time)"
  (multiple-value-bind
    (second minute hour date month year)
    (get-decoded-time)
    (format () "~a ~a, ~a - AT ~a:~a (~a sec)~%"month date year hour minute second  )))




