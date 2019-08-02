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
;-------| This file is: $LLdg/DV.ll
;-------| Version V1.0: Jan 22, 1990
;-------| By Marco Stroppa
;-------| Copyright 1990 IRCAM
;*****************************************************************************

; THIS FILES CONTAINS ALL THE FUNCTIONS THAT GENERATE DATA PASSED AS DIRECT
;     VALUES TO BE GENERATED A CERTAIN AMOUNT OF TIMES

(in-package :cr)

;-----------------------------------------------------------------------------
; GLOBALS
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
; AVAILABLE FUNCTIONS
;	l-val		: list of values
;	lp		: loop
;	bkwd-lp		: backward loop
;	rept		: repetition
;	rept-lp		: repetition with loop at the end
;	bkwd-rept-lp	: repetition with backward loop et the end
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (l-val NEV LIST)
; 	if (length LIST) < NEV, then repeat the last value until NEV
;	   else give back the first NEV els of LIST
; EX: (setf a '(1 2 3 4 5)), (l-val 10 a) ===> '(1 2 3 4 5 5 5 5 5 5)
(defun l-val (nev l)
  (if (<= nev (length l))
    (firstn nev l)
    (let ((last (copy-list (last l))) )
      (firstn nev
              (append l (nconc last last))))) )
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (lp NEV LIST)
; 	 loop along the values contained in LIST
; EX: (setf a '(1 2 3)), (lp 10 a) ===> '(1 2 3 1 2 3 1 2 3 1)
(defun lp (nev l)
    (let ((ll (copy-list l)) )
	(nconc ll ll)
	(firstn nev ll)))
;-----------------------------------------------------------------------------
; (bkwd-lp NEV LIST)
; (bkwd1-lp NEV LIST)
; 	loop along the values contained in LIST going backward and forward
; 	bkwd1: do not repeat the border values if the list has > 2 els
; EX: (setf a '(1 2 3)), (bkwd-lp 10 a) ===> '(1 2 3 3 2 1 1 2 3 3)
; EX: (setf a '(1 2 3 4)), (bkwd1-lp 10 a) ===> '(1 2 3 4 3 2 1 2 3 4)
(defun bkwd1-lp (nev l)
  (if (< (length l) 3)
      (bkwd-lp nev l)
    (let ((ll (reverse (butlast (cdr l)))))
      (lp nev (append l ll)))))
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (rept NEV VAL)
; 	repeat VAL NEV times
; EX: (rept 10 'a) ===> (a a a a a a a a a a)
(defun rept (nev val)
    (let ((result ()) )
	(loop while (>= (decf nev) 0)
	    do(newl result val))
	(nreverse result)) )
;-----------------------------------------------------------------------------
; (rept-lp NEV LIST)
; 	loop along the last values contained in LIST
;	LIST has the structure: (val1 ... valM (l1 ... lN)), loop is between
;	   l1 and lN
; EX: (setf a '(1 2 (3 4 5))), (rept-lp 10 a) ===> '(1 2 3 4 5 3 4 5 3 4)
(defun rept-lp (nev l)
  (if (atom (car (last l))) ; it is not a list of lists -> consider like rept
      (rept nev l)
    (let ((last (copy-list (car (last l))))
	  (result (firstn (1- (length l)) l)) )
      (firstn nev
              (append result (nconc last last)))) ) )
;-----------------------------------------------------------------------------
; (bkwd-rept-lp NEV LIST)
; (bkwd1-rept-lp NEV LIST)
; 	loop along the last values contained in LIST backward and forward
;	LIST has the structure: (val1 ... valM (l1 ... lN)), loop is between
;	   l1 and lN
;	bkwd1: do not repeat the border values if the list has > 2 els
; EX: (setf a '(1 2 (3 4 5))), (bkwd-rept-lp 10 a)  ===> '(1 2 3 4 5 5 4 3 3 4)
; EX: (setf a '(1 2 (3 4 5))), (bkwd1-rept-lp 10 a) ===> '(1 2 3 4 5 4 3 4 5 4)
(defun bkwd-rept-lp (nev l)
  (if (atom (car (last l))) ; it is not a list of lists -> consider like bkwd-lp
      (bkwd-lp nev l)
    (rept-lp nev (append (butlast l) (list (append (car (last l)) (reverse (car (last l)))))))))

(defun bkwd1-rept-lp (nev l)
  (let ((ll (car (last l))))
    (if (< (length ll) 3)
        (bkwd-rept-lp nev l)
      (let ((lll (append ll (reverse (butlast (cdr ll))))))
        (rept-lp nev (append (butlast l) (list lll)))))))


;;; JB COMMENTED THIS TEMPORARILY....
#|
  (if (atom (car (last l))) ; it is not a list of lists -> consider like bkwd-lp
      (bkwd-lp nev l)
    (rept-lp nev (append (butlast l) (list (append (car (last l)) (reverse (car (last l)))))))))

|#
;-----------------------------------------------------------------------------




