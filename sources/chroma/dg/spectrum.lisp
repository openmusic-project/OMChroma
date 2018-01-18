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
;-------| This file is: $LLdg/spectrum.ll
;-------| Version V1.0: Jan 22, 1990, Rev. 2.0, Feb 2010
;-------| By Marco Stroppa
;-------| Copyright 1990 IRCAM
;*****************************************************************************

; THIS FILES CONTAINS VARYING FUNCTIONS TO GENERATE HARMONIC, SHIFTED,
;	AND STRETCHED SPECTRA ACCORDING TO MC ADAMS' FORMULAS

(in-package :cr)
;-----------------------------------------------------------------------------
; AVAILABLE FUNCTIONS
;	sp
;	spsh
;	spst
;	spsht
;	xpose / xpose-n
;	xp-int / xp-int-1
;	stretch-factor
;	correct-shift
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (sp F0 NP [STON])
;	generate a harmonic spectrum
;	F0 = fundamental frequency
;	NP = number of components to generate
;	   it may be:
;		a NUMBER : all the components from 1 to NP will be generated
;		a LIST containing a list of the scalers of f0
;	STON = % of detuning applied when computing each component [0-1]
; EX: (sp 440 10), (sp 440 10 0.06), (sp 440 '(2 3.3 5.1 7) 0.01)
(defun sp (f0 np &optional ston)
"
; (sp F0 NP [STON])
;	generate a harmonic spectrum
;	F0 = fundamental frequency
;	NP = number of components to generate
;	   it may be:
;		a NUMBER : all the components from 1 to NP will be generated
;		a LIST containing a list of the scalers of f0
;	STON = % of detuning applied when computing each component [0-1]
; EX: (sp 440 10), (sp 440 10 0.06), (sp 440 '(2 3.3 5.1 7) 0.01)
"
  (let ((ston (ifn ston 0 ston))
        (result ()) )
    (if (numberp np)
      (let ((cnt 0))
        (loop while (<= (incf cnt) np)
          do (newl result (* (+ 1 (ran ston))
                          cnt
                          f0)) ))
      (loop while np
        do (let ((curr (nextl np)) )
          (newl result (* (+ 1 (ran ston))
                          curr
                          f0)) )) )
    (nreverse result)) )
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (spsh F0 NP SH [STON])
;	generate a shifted spectrum
;	F0 = fundamental frequency
;	NP = number of components to generate
;	   it may be:
;		a NUMBER : all the components from 1 to NP will be generated
;		a LIST containing a list of the scalers of f0
;	SH = shifting factor (% of f0)
;	STON = % of detuning applied when computing each component [0-1]
; EX: (spsh 440 10 0.06), (spsh 440 10 0.1 0.06)
;	(spsh 440 '(2 3.3 5.1 7) 0.5 0.01)
(defun spsh (f0 np sh &optional ston)
"
; (spsh F0 NP SH [STON])
;	generate a shifted spectrum
;	F0 = fundamental frequency
;	NP = number of components to generate
;	   it may be:
;		a NUMBER : all the components from 1 to NP will be generated
;		a LIST containing a list of the scalers of f0
;	SH = shifting factor (% of f0)
;	STON = % of detuning applied when computing each component [0-1]
; EX: (spsh 440 10 0.06), (spsh 440 10 0.1 0.06)
;	(spsh 440 '(2 3.3 5.1 7) 0.5 0.01)
"
  (let ((ston (ifn ston 0 ston))
        (result ())
        (shift (* f0 sh)) )
    (if (numberp np)
      (let ((cnt 0))
        (loop while (<= (incf cnt) np)
          do (newl result (+  (* (+ 1 (ran ston))
                              cnt
                              f0)
                           shift))) )
      (loop while np
        do (let ((curr (nextl np)) )
          (newl result (+  (* (+ 1 (ran ston))
                              curr
                              f0)
                           shift))) ))
    (nreverse result)) )
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (spst F0 NP ST [STON])
;	generate a stretched/compressed spectrum
;	F0 = fundamental frequency
;	NP = number of components to generate
;	   it may be:
;		a NUMBER : all the components from 1 to NP will be generated
;		a LIST containing a list of the scalers of f0
;	ST = stretching/compressing factor (2.0 = octave)
;	STON = % of detuning applied when computing each component [0-1]
; EX: (spst 440 10 2.1), (spst 440 10 1.76 0.06)
;	(spst 440 '(2 3.3 5.1 7) 2.5 0.01)
(defun spst (f0 np st &optional ston)
"
; (spst F0 NP ST [STON])
;	generate a stretched/compressed spectrum
;	F0 = fundamental frequency
;	NP = number of components to generate
;	   it may be:
;		a NUMBER : all the components from 1 to NP will be generated
;		a LIST containing a list of the scalers of f0
;	ST = stretching/compressing factor (2.0 = octave)
;	STON = % of detuning applied when computing each component [0-1]
; EX: (spst 440 10 2.1), (spst 440 10 1.76 0.06)
;	(spst 440 '(2 3.3 5.1 7) 2.5 0.01)
"
  (let ((ston (ifn ston 0 ston))
        (result ())
        (stretch (/ (log st) (log 2.0))) )
    (if (numberp np)
      (let ((cnt 0))
        (loop while (<= (incf cnt) np)
          do (newl result (* (+ 1 (ran ston))
                          (expt cnt stretch)
                          f0)) ))
      (loop while np
        do (let ((curr (nextl np)) )
          (newl result (* (+ 1 (ran ston))
                          (expt curr stretch)
                          f0)) )) )
    (nreverse result)) )
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (spsht F0 NP SH ST [STON])
;	generate a shifted spectrum
;	F0 = fundamental frequency
;	NP = number of components to generate
;	   it may be:
;		a NUMBER : all the components from 1 to NP will be generated
;		a LIST containing a list of the scalers of f0
;	SH = shifting factor (% of f0)
;	ST = stretching/compressing factor (2.0 = octave)
;	STON = % of detuning applied when computing each component [0-1]
; EX: (spsht 440 10 0.06 2.0) , (spsht 440 10 1.76 0.06)
;	(spsht 440 '(2 3.3 5.1 7) 2.5 0.01)
(defun spsht (f0 np sh st &optional ston)
"
; (spsht F0 NP SH ST [STON])
;	generate a shifted spectrum
;	F0 = fundamental frequency
;	NP = number of components to generate
;	   it may be:
;		a NUMBER : all the components from 1 to NP will be generated
;		a LIST containing a list of the scalers of f0
;	SH = shifting factor (% of f0)
;	ST = stretching/compressing factor (2.0 = octave)
;	STON = % of detuning applied when computing each component [0-1]
; EX: (spsht 440 10 0.06 2.0) , (spsht 440 10 1.76 0.06)
;	(spsht 440 '(2 3.3 5.1 7) 2.5 0.01)
"
  (let ((ston (ifn ston 0 ston))
        (result ())
        (shift (* f0 sh))
        (stretch (/ (log st) (log 2.0))) )
    (if (numberp np)
      (let ((cnt 0))
        (loop while (<= (incf cnt) np)
          do (newl result (+  (* (+ 1 (ran ston))
                              (expt cnt stretch)
                              f0)
                           shift)) ))
      (loop while np
        do (let ((curr (nextl np)) )
          (newl result (+  (* (+ 1 (ran ston))
                              (expt curr stretch)
                              f0)
                           shift)) )) )
    (nreverse result)) )
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (xpose SP INT)
;	transpose a spectrum (list of freq) by an interval INT (scaler of f0)

; (xpose-n N fq SP)
;	transpose a spectrum (list of freq) so that it's N'th component matches fq

; EX: (xpose (spsht 440 10 0.0 2.0) 0.5) --> octave below
;     (xpose (spsht 440 10 0.0 2.0) 2) --> fifth above
;     (xpose-n 3 (pch->fq 'do4) (spsht 100 10 0.0 2.0)) --> 3rd ovtn to C4
;     (xpose-n 3 400.0 (spsht 100 10 0.0 2.0)) --> 3rd ovtn to 400.0
;-----------------------------------------------------------------------------
(defun xpose (sp int)
"
; (xpose SP INT)
;	transpose a spectrum (list of freq) by an interval INT (scaler of f0)

; EX: (xpose (spsht 440 10 0.0 2.0) 0.5) --> octave below
;     (xpose (spsht 440 10 0.0 2.0) 2) --> octave above
"
    (let ((result ()) )
	(loop while sp
	    do (newl result (* int (nextl sp))) )
	(nreverse result)) )

;added 1002, ms
;made compatible with xpose, ms1801
(defun xpose-n (sp n fq)
"
; (xpose-n N FQ SP)
;	transpose a spectrum (list of freq) so that it's N'th component matches fq

; EX: (xpose-n 3 (pch->fq 'do4) (spsht 100 10 0.0 2.0)) --> 3rd ovtn to C4
;     (xpose-n 3 400.0 (spsht 100 10 0.0 2.0)) --> 3rd ovtn to 400.0
"
    (let ((sp (if (listp sp) sp fq))
          (n (if (listp sp) n sp))
          (fq (if (listp sp) fq n)))
      (let ((result ())
            (fqn (nth (1- n) sp)))
        (let ((ratio (/ fq fqn)))
          (loop while sp
                do (newl result (* ratio (nextl sp))) ))
	(nreverse result)) ))
    
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (xp-int INT) / (xp-int-1 INT)
;	generate a transposing interval of INT (in semitones, either positive
;	   or negative) to use with xpose and other functions
;	xp-int will work with functions needing with absolute values (e.g. xpose),
;	   whereas xp-int-1 is suited to functions in which the transposing
;	   factor will then be multiplied by the reference values (e.g. the
;	   sh factor of spsh[t])
;	no transposition : xp-int = 1, xp-int-1 = 0
;	one octave : xp-int = 2, xp-int-1 = 1

; EX:     (xpose (spsht 440 10 1.76 0.06) (xp-int 7)) --> fifth above
;-----------------------------------------------------------------------------
(defun xp-int (int)
"
; (xp-int INT) / (xp-int-1 INT)
;	generate a transposing interval of INT (in semitones, either positive
;	   or negative) to use with xpose and other functions
;	xp-int will work with functions needing with absolute values (e.g. xpose),
;	   whereas xp-int-1 is suited to functions in which the transposing
;	   factor will then be multiplied by the reference values (e.g. the
;	   sh factor of spsh[t])
;	no transposition : xp-int = 1, xp-int-1 = 0
;	one octave : xp-int = 2, xp-int-1 = 1
;	octave down: xp-int = 0.5, xp-int-1 = -0.5

; EX:     (xpose (spsht 440 10 1.76 0.06) (xp-int 7)) --> fifth above
"
    (let ((semitone (expt 2 (/ 1 12)))) ;1.059463095))
      (ifn (listp int)
          (expt semitone int)
        (mapcar 'xp-int int)))
    )


(defun xp-int-1 (int)
"
; (xp-int INT) / (xp-int-1 INT)
;	generate a transposing interval of INT (in semitones, either positive
;	   or negative) to use with xpose and other functions
;	xp-int will work functions needing with absolute values (e.g. xpose),
;	   whereas xp-int-1 is suited to functions in which the transposing
;	   factor will then be multiplied by the reference values (e.g. the
;	   sh factor of spsh[t])
;	no transposition : xp-int = 1, xp-int-1 = 0
;	one octave : xp-int = 2, xp-int-1 = 1
;	octave down: xp-int = 0.5, xp-int-1 = -0.5

; EX:     (xpose (spsht 440 10 1.76 0.06) (xp-int 7)) --> fifth above
"
    (let ((semitone (expt 2 (/ 1 12))))
      (ifn (listp int)
          (1- (expt semitone int))
        (mapcar 'xp-int-1 int)))
    )
;-----------------------------------------------------------------------------
; (stretch-factor stretched_up stretched_down ref_up ref_down)
;	generate the stretching factor needed by spsht in order to have
;	   partials j, i correspond to the ratio fj / fi in the stretched
;	   spectrum.
;-----------------------------------------------------------------------------
;changed order of args, 1002, ms
(defun stretch-factor (i j fi fj)
"Computes the streching factor needed by spsht in order to have the usually harmonic
partials i, j match fi, fj in the stretched or compressed spectrum.
2.0 = harmonic spectrum
1.0 < x < 2.0 = compressed spectrum
x > 2.0 = stretched spectrum
Beware: values <= 1.0 are meaningless.
"
  (let ((stretched_ratio (/ fj fi))
        (orig_ratio (/ j i))
        (ln2 (log 2.0)))
    (exp (/ (* ln2 (log stretched_ratio)) (log orig_ratio)))))

; (stretch-factor 5 3 5 3) -> 2.0
; (stretch-factor 3 5 (car (spsht 1 '(3) 0 2.2)) (car (spsht 1 '(5) 0 2.2)) ) -> 2.2
; (stretch-factor 5 3 (car (spsht 1 '(5) 0 1.2)) (car (spsht 1 '(3) 0 1.2)) ) -> 1.2

;-----------------------------------------------------------------------------
;added 1002, ms
(defun correct-shift (f0 sh)
"When computing a shifted spectrum, correct its f0 accordingly, so that
the spectrum starts with the original f0.
Usually inserted between the original f0 and the f0 input of the spectrum,
with the same sh factor.
(correct-shift 100 0.1) -> 90.90909
(correct-shift 100 -0.1) -> 111.111
"
  (/ f0 (1+ sh)))

;-----------------------------------------------------------------------------
