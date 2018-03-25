;*******************************************************************
;-------| CHROMA SYSTEM
;-------| This file is: $LLsys/globals.lisp
;-------| Designed and implemented in LeLisp by Marco Stroppa 
;-------| Ported to Common Lisp by Serge Lemouton
;-------| Version: Nov 28, 1996
;-------| Modified March 2000 (added dynamic functions in csound)
;-------| ADAPTED to omChroma 050221, Marco Stroppa
;*******************************************************************

(in-package :cr)

;-----------------------------------------------------------------------------
(defun get-gbl (var)
    (eval (eval var)))

(defun set-gbl (var val)
   (set var val))


;-----------------------------------------------------------------------------
; DEBUGGING FLAGS
; default value

(set-gbl 'DEBFLG 1) 

(defun deb-mode ()
   (set-gbl 'DEBFLG 0))

(defun normal-mode ()
   (set-gbl 'DEBFLG 1))

(defun alternative-mode ()
   (set-gbl 'DEBFLG 2))

;-----------------------------------------------------------------------------
; PRINTOUT
; default values

(set-gbl 'PRNFLG t) 
(set-gbl 'CTL1-PRINT t) 
(set-gbl 'CTL2-PRINT t) 

(defun enable-print ()
   (set-gbl 'PRNFLG t))

(defun disable-print ()
   (set-gbl 'PRNFLG ()))

(defun enable-print-ctl1 ()
   (set-gbl 'CTL1-PRINT t))

(defun disable-print-ctl1 ()
   (set-gbl 'CTL1-PRINT ()))

(defun enable-print-ctl2 ()
   (set-gbl 'CTL2-PRINT t))

(defun disable-print-ctl2 ()
   (set-gbl 'CTL2-PRINT ()))

;-----------------------------------------------------------------------------
; SYNTHESIS ENABLE FLAG
; kept for historical compatibility
(set-gbl 'PROBLM ()) 

(defun unable-synthesis ()
   (set-gbl 'PROBLM ()))

(defun enable-synthesis ()
   (set-gbl 'PROBLM ()))

(defun disable-synthesis ()
   (set-gbl 'PROBLM t))




;******************************************************************
; GLOBAL VARIABLES FOR WHOLE SYSTEM
;------------------------------------------------------------------
(defvar *MONTHS*
   (vector () 'Jan 'Feb 'Mar 'Apr 'May 'Jun 'Jul 'Aug 'Sep 'Oct 'Nov 'Dec))
;-----------------------------------------------------------------------------

; NEW SETUP, UP TO 1000.0 ABSOLUTE, FOR REASONS OF DECIMAL QUALITY
(set-gbl 'MAXAMP 1000.0)		; MAXIMUM AMPLITUDE FOR SYNTHESIZER
(set-gbl 'GBLAMP 1000.0)		; GLOBAL AMPLITUDE FOR LOCAL EVENT

(set-gbl 'SR om::*audio-sr*)		; SAMPLING RATE
(set-gbl 'SR/2 '(/ (get-gbl SR) 2))		; NYQUIST FREQUENCY
(set-gbl 'KR '(get-gbl SR))		; CONTROL RATE FOR CSOUND
(set-gbl 'NCH 1)		; NUMBER OF CHANNELS
(set-gbl 'DURMIN 0.01)		; MINIMUM DURATION FOR A NOTE
(set-gbl 'MINFQ 13.0)		; MINIMUM FREQUENCY FOR A NOTE
(set-gbl 'MAXSI 12.0)		; MAXIMUM SAMPLE INCREMENT WHEN READING A WT

(set-gbl 'DEF-GEN-SIZE 16385)	; DEFAULT GEN SIZE
(set-gbl 'EXPZERO 0.00001)	; REPLACE 0 IN EXPONENTIAL GENS

(set-gbl 'PMAX 1024) ; MAXIMUM NUMBER OF CSOUND P FIELDS
;(set-gbl 'PMAX om::*cs-max-points*) ; MAXIMUM NUMBER OF CSOUND P FIELDS
                            ; USED WHEN COMPILING FUNCTIONS (= MAX - 2,
                            ;    USED AT THE BEGINNING)
                            ; IF () NO CONTROL
(set-gbl 'DIAPASON 'om::*diapason-freq*)	; CURRENT DIAPASON


;-----------------------------------------------------------------------------
(set-gbl 'PRT-FM 0)		; PORTAMENTO FLAG FOR FM (USED IN CIF D. BASE)
;				   0 = PORTAMENTO AFFECTS CARRIER FREQ
;				   1 = PORTAMENTO AFFECTS MODULATING FREQ
(set-gbl 'PRNFLG t)		; PRINTOUT FLAG
;				   t = ENABLE ALL PRINTOUTS
;				  () = PRINT ONLY ERROR AND DISCARDED PARTIALS
;-----------------------------------------------------------------------------

(set-gbl '*chroma-output* *standard-output*)	


(set-gbl 'USER 'om::*composer-name*)	; USER'S NAME FOR PERSONALIZED MESSAGES

(eval 'om::*diapason-freq*)
(eval DIAPASON)
;-----------------------------------------------------------------------------;-----------------------------------------------------------------------------
; Temporary place for the dynamically computed GENS of a WT object
(set-gbl 'WTL ())		; A-LIST OF WT OBJECTS
(set-gbl 'WTIND 0)		; INDEX OF CURRENT WT OBJECT

;		CSOUND STRUCTURE : '(	(sf1 dir1 f1 . n-smpls1)
;					(sf2 dir2 f2 . n-smpls2) ...)
;		WHERE:	sfX = name of sound file 1, 2, etc.
;			f1 = function number associated to that sound file
;			for csound a f{fX} function will be associated to
;			   input sf sndin.{position-in-A-list} [symbolic link]
				; DIR OF SOUND FILES ON MARC TO LOAD ON MOON

(set-gbl 'WTFO 5000)		; OFFSET WHEN WRITING CSOUND GEN 1 TABLES
(set-gbl 'WTFOMAX 9999)		; MAXIMUM OFFSET WHEN WRITING CSOUND GEN 1
                                ; PAY ATTENTION THAT IT IS NOT OVERLAPPING WITH
                                ;  THE AUTOMATIC TABLE NUMBER IN OM PREFERENCES
;(set-gbl 'DEFXFC ())		; DEFAULT CONTROL FOR INTERFACE OF WT SYSTEM

;-----------------------------------------------------------------------------
; CONTROL OF THE FREEZE ALGORITHM FOR WT OBJECTS
;  PARAMETERS ARE RELATIVE TO A BASIC OVERLAP, SET TO 10% OF THE DURATION
;    OF THE CURRENT WT OBJECT
; SEE FILE wt-frz.ll IN $LLctl1 FOR DETAILS
(set-gbl 'DEF-FRZ		;  DEFAULT FREEZE PARAMETERS
   ''(let ((ovlp (* (dur_wt (curr_wt) (my-si_wt)) 0.1)) )
       `(
	 (inc-pt (* ,ovlp 4.0))		; INCRUSTATION POINT
	 (dur (ran (* ,ovlp 2.0)	; DURATION OF INCRUSTED SEGMENTS
		   (* ,ovlp 0.5)) )
	 (skip (ran (* ,ovlp 4.0)	; SKIP POINT FOR EACH NEW SEGMENT
		    ,ovlp) )
	 (end- ,ovlp)			; FINAL PORTION OF THE SOUND
	 (ampfac 1.0)			; AMPLITUDE SCALER FOR EACH NEW SEGMENT
	 (xin ,ovlp)			; IN/OUT CROSSFADES
	 (xout ,ovlp)
	 (first-xout ,ovlp)
	 (last-xin ,ovlp)
					; LAST-XOUT SET TO THE WHOLE DURATION
					;    OF THE LAST SEGMENT
	 (last-xout (- (seg-dur_wt) (seg-last-xin_wt)) )
	 (min-xin (* (get-gbl 'DURMIN) 0.5) )	; MIN-XIN / XOUT FOR EXTREME
	 (min-xout (* (get-gbl 'DURMIN) 0.5) )	;  TESTS
;	 (min-xin 0.05)
;	 (min-xout 0.05)
	)
     ))

;TEST
;(set-gbl 'DEF-FRZ		;  DEFAULT FREEZE PARAMETERS
;     ''(
;	 (inc-pt 0.1)
;	 (dur 0.5)
;	 (skip 0.2)
;	 (end- 0.25)
;	 (ampfac 0.5)
;	 (xin 0.1)
;	 (xout 0.15)
;	 (first-xout 0.25)
;	 (last-xin 0.2)
;    ))
(set-gbl 'FRZ-MODE 1)		; DEFAULT FREEZE MODE = RUN FREEZE WHEN NEEDED
;*****************************************************************************
