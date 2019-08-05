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

(defun cr-print? () (get-gbl 'PRNFLG))

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

(set-gbl 'SR/2 '(/ (get-gbl SR) 2))		; NYQUIST FREQUENCY
(set-gbl 'KR '(get-gbl SR))		; CONTROL RATE FOR CSOUND
(set-gbl 'NCH 1)		; NUMBER OF CHANNELS
(set-gbl 'DURMIN 0.01)		; MINIMUM DURATION FOR A NOTE
(set-gbl 'WINMIN 0.005)		; MINIMUM DURATION FOR A WINDOW OF A WT OBJECT
(set-gbl 'MINFQ 13.0)		; MINIMUM FREQUENCY FOR A NOTE
(set-gbl 'MAXSI 12.0)		; MAXIMUM SAMPLE INCREMENT WHEN READING A WT

(set-gbl 'DEF-GEN-SIZE 16385)	; DEFAULT GEN SIZE
(set-gbl 'EXPZERO 0.00001)	; REPLACE 0 IN EXPONENTIAL GENS

(set-gbl 'PMAX 1024) ; MAXIMUM NUMBER OF CSOUND P FIELDS

                            ; USED WHEN COMPILING FUNCTIONS (= MAX - 2,
                            ;    USED AT THE BEGINNING)
                            ; IF () NO CONTROL


;;; THESE TWO ARE REDEFINED WHEN LOADED IN OM
(set-gbl 'SR 44100)       ; SAMPLING RATE
(set-gbl 'DIAPASON 440.0) ; CURRENT DIAPASON
(set-gbl 'USER "Chroma User")	; USER'S NAME FOR PERSONALIZED MESSAGES
; (get-gbl 'USER)


;-----------------------------------------------------------------------------
(set-gbl 'PRT-FM 0)		; PORTAMENTO FLAG FOR FM (USED IN CIF D. BASE)
;				   0 = PORTAMENTO AFFECTS CARRIER FREQ
;				   1 = PORTAMENTO AFFECTS MODULATING FREQ
(set-gbl 'PRNFLG t)		; PRINTOUT FLAG
;				   t = ENABLE ALL PRINTOUTS
;				  () = PRINT ONLY ERROR AND DISCARDED PARTIALS
;-----------------------------------------------------------------------------

;;; redefined in OM
(set-gbl '*chroma-output* *standard-output*)

;*****************************************************************************
; TRANSPOSITION FACTORS FOR FUNCTION "xpose" (in spectrum.lisp)
;		UPWARD TRANSPOSITION
(set-gbl 'xp2- (interval 100))
(set-gbl 'xp2+ (interval 200))
(set-gbl 'xp3- (interval 300))
(set-gbl 'xp3+ (interval 400))
(set-gbl 'xp4 (interval 500))
(set-gbl 'xp4+ (interval 600))
(set-gbl 'xp5- (interval 600))
(set-gbl 'xp5 (interval 700))
(set-gbl 'xp6- (interval 800)) 
(set-gbl 'xp6+ (interval 900))
(set-gbl 'xp7- (interval 1000))
(set-gbl 'xp7+ (interval 1100))
(set-gbl 'xp8 (interval 1200))
(set-gbl 'xp9- (interval 1300))
(set-gbl 'xp9+ (interval 1400))
(set-gbl 'xp10- (interval 1500))
(set-gbl 'xp10+ (interval 1600))
(set-gbl 'xp11 (interval 1700))
(set-gbl 'xp11+ (interval 1800))
(set-gbl 'xp12- (interval 1800))
(set-gbl 'xp12 (interval 1900))
(set-gbl 'xp13- (interval 2000)) 
(set-gbl 'xp13+ (interval 2100))
(set-gbl 'xp14- (interval 2200))
(set-gbl 'xp14+ (interval 2300))

;		DOWNWARD TRANSPOSITION
(set-gbl '-xp2- (interval -100))
(set-gbl '-xp2+ (interval -200))
(set-gbl '-xp3- (interval -300))
(set-gbl '-xp3+ (interval -400))
(set-gbl '-xp4 (interval -500))
(set-gbl '-xp4+ (interval -600))
(set-gbl '-xp5- (interval -600))
(set-gbl '-xp5 (interval -700))
(set-gbl '-xp6- (interval -800)) 
(set-gbl '-xp6+ (interval -900))
(set-gbl '-xp7- (interval -1000))
(set-gbl '-xp7+ (interval -1100))
(set-gbl '-xp8 (interval -1200))
(set-gbl '-xp9- (interval -1300))
(set-gbl '-xp9+ (interval -1400))
(set-gbl '-xp10- (interval -1500))
(set-gbl '-xp10+ (interval -1600))
(set-gbl '-xp11 (interval -1700))
(set-gbl '-xp11+ (interval -1800))
(set-gbl '-xp12- (interval -1800))
(set-gbl '-xp12 (interval -1900))
(set-gbl '-xp13- (interval -2000)) 
(set-gbl '-xp13+ (interval -2100))
(set-gbl '-xp14- (interval -2200))
(set-gbl '-xp14+ (interval -2300))
;*****************************************************************************
