;=====================================================
; CHROMA 
;=====================================================
; part of the OMChroma library
; -> High-level control of sound synthesis in OM
;=====================================================
;
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
; File author: M. Stroppa
;=====================================================


;*****************************************************************************
;-------| DG SYSTEM
;-------| This file is: $LLdg/DF.ll
;-------| Version V1.0: Jan 22, 1990, V2.0, Aug 2000
;-------| Rev. 3.0, Marco, Feb 2010
;-------| By Marco Stroppa
;-------| Copyright 1990 IRCAM
;*****************************************************************************

; THIS FILES CONTAINS ALL THE FUNCTIONS THAT GENERATE DATA PASSED AS TABLE-
;    -LOOKUP FUNCTIONS THAT WILL BE SAMPLED A CERTAIN AMOUNT OF TIMES

; ADDED AUGUST 2000: VARIOUS KINDS OF INTERPOLATION (SEE FUN.LISP)

(in-package :cr)

;-----------------------------------------------------------------------------
; GLOBALS
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
; AVAILABLE FUNCTIONS (in alphabetical order):
;	lkp / fix-lkp	: linear table-lookup (fix-point output)
;	lkpr / fix-lkp	: linear table-lookup with rescaling of y values
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (lkp NEV FUN) / (fix-lkp NEV FUN)
; 	sample FUN NEV equally spaced times
;	FUN must be of type FUN
;	the input format is NO LONGER compatible with lkpr
; EX: (setf fun (make_fun '(0 0 1 1))) (setf a '(0 0 1 1)),
; (lkp 11 fun) ===> (0 .1 .2 .3 .4 .5 .6 .7 .8 .9 1)
;ADDED 0008, Marco: generalize other forms of interpolation

;corrected null exp, ms, 1002
(defun lkp (nev fun &optional (exp 0.0))
"
; (lkp NEV FUN) / (fix-lkp NEV FUN)
; 	sample FUN NEV equally spaced times
;	FUN must be of type FUN
;	the input format is NO LONGER compatible with lkpr
; EX: (setf fun (make_fun '(0 0 1 1))) (setf a '(0 0 1 1)),
; (lkp 11 fun) ===> (0 .1 .2 .3 .4 .5 .6 .7 .8 .9 1)
"
  (let ((fun (copy-list fun))
        (nev (if (< nev 2) 2 nev))
        (exp (ifn exp 1.0 exp))
        )
    (x-resc_fun fun 0 (1- nev))
    (let ((result ())
          (step 1)
          (acc 0) )
      (loop while (>= (decf nev) 0)
             do (newl result (y-val_fun fun acc exp))
            do (incf acc step))
      (nreverse result)) ) )

;using om-round, corrected null exp, ms, 1002
(defun fix-lkp (nev fun &optional (exp 0.0))
"
; (lkp NEV FUN) / (fix-lkp NEV FUN)
; 	sample FUN NEV equally spaced times, fixed-point result
;	FUN must be of type FUN
;	the input format is NO LONGER compatible with lkpr
; EX: (setf fun (make_fun '(0 0 1 1))) (setf a '(0 0 1 1)),
; (lkp 11 fun) ===> (0 .1 .2 .3 .4 .5 .6 .7 .8 .9 1)
"
  (let ((fun (copy-list fun))
         (nev (if (< nev 2) 2 nev))
         (exp (ifn exp 1.0 exp))
        )
    (x-resc_fun fun 0 (1- nev))
    (let ((result ())
          (step 1)
          (acc 0) )
      (loop while (>= (decf nev) 0)
            do
            (let* ((val (y-val_fun fun acc exp)))
              (newl result (round val)))
            do (incf acc step))
      (nreverse result)) ) )

;-----------------------------------------------------------------------------
; (lkpr NEV FUN YMIN YMAX [OFFS]) / (fix-lkpr NEV FUN YMIN YMAX [OFFS])
; 	sample FUN NEV equally spaced times
;	Y values will be rescaled to from YMIN and YMAX and offset by OFFS
;	FUN must be of type FUN
;	to make the format compatible with lkp, if YMIN and YMAX are not
;	   specified, lkpr behaves like lkp
; EX: (setf fun (make_fun '(0 0 1 1))), (lkpr 5 fun -10 10) ===> (-10 -5 0 5 10)
(defun lkpr (nev fun ymin ymax &optional (exp 1.0) &key (offset 0.0))
"
; (lkpr NEV FUN YMIN YMAX [OFFS]) / (fix-lkpr NEV FUN YMIN YMAX [OFFS])
; 	sample FUN NEV equally spaced times
;	Y values will be rescaled to from YMIN and YMAX and offset by OFFS
;	FUN must be of type FUN
;	to make the format compatible with lkp, if YMIN and YMAX are not
;	   specified, lkpr behaves like lkp
; EX: (setf fun (make_fun '(0 0 1 1))), (lkpr 5 fun -10 10) ===> (-10 -5 0 5 10)
"
  (let ((fun (copy-list fun)) )
    (y-resc_fun fun (+ ymin offset) (+ ymax offset) exp)
    (lkp nev fun exp)) )

(defun fix-lkpr (nev fun ymin ymax &optional (exp 1.0) &key (offset 0.0))
"
; (lkpr NEV FUN YMIN YMAX [OFFS]) / (fix-lkpr NEV FUN YMIN YMAX [OFFS])
; 	sample FUN NEV equally spaced times
;	Y values will be rescaled to from YMIN and YMAX and offset by OFFS
;	FUN must be of type FUN
;	to make the format compatible with lkp, if YMIN and YMAX are not
;	   specified, lkpr behaves like lkp
; EX: (setf fun (make_fun '(0 0 1 1))), (lkpr 5 fun -10 10) ===> (-10 -5 0 5 10)
"
  (let ((fun (copy-list fun)) )
    (y-resc_fun fun (+ ymin offset) (+ ymax offset))
    (fix-lkp nev fun exp)) )
;-----------------------------------------------------------------------------
