;******************************************************************
;		     CLASS BUZZ-1
;******************************************************************

(in-package :om)

; LISP-DEFINED CLASSES SHOULD RESIDE IN THE LIBRARY'S PACKAGE AND 
;   NOT IN THE USER PACKAGE, WHICH CONTAINS ALL THE CLASSES
;   GRAPHICALLY DEFINED


(defclass! buzz-1
  (cs-evt) 			; INHERIT FROM CS-EVT
  (

; GLOBAL SLOTS (LIGHT BLUE, ON THE LEFT OF THE CLASS):
;    THE METHOD BELOW TRANSFORMS THEM INTO GLOBAL SLOTS ("SHOW" UNCHEKED)
;    ATTENTION: A GLOBAL SLOT SHOULD NOT HAVE AN INITARG

   ( source-code :initform
                 (load-buffer-textfile
                  (get-orc-source (get-orc "buzz-1"))
                  'textfile "append")
                 :allocation :class
                 :type textfile
                 :accessor source-code)
   ( numchan :initform (or (get-orc-channels (get-orc "buzz-1")) 1)
             :allocation :class  :accessor numchan)

   (cs-inits :initform (get-cs-inits (get-orc "buzz-1")) 
             :allocation :class :type list :accessor cs-inits)

   (orc-header :initform (list
                          
                          "; GEN functions **********************************************************"
                          "; single cosine(!) wave for the buzz"
                          "f5 0 16777216 11 1 1"                        
                          ) 
               :allocation :class :type list :accessor orc-header)

   (InstID :initform 1  :allocation :class  :accessor InstID)

; LOCAL SLOTS (RED, CORRESPONDING TO THE P-FIELDS)
;    ATTENTION: A GLOBAL SLOT SHOULD HAVE AN INITARG
  ( amp		:type number
		:initarg :amp 
  		:initform -6.0
		:accessor amp)
  ( f0		:type number
        	:initarg :f0 
        	:initform 220.0
        	:accessor f0)
   ( aenv	:type cs-table
		:initarg :aenv 
                       ; x-points y-points decimals number size
  		:initform (make-cs-table 'Gen07  '(0 2048 4096)
                                         '(0 1 0) 1 "?" 4097)
		:accessor aenv)
   ( bzl	:type number
        	:initarg :bzl 
        	:initform 1
        	:accessor bzl
                :documentation "This value here is int")
   ( bzh	:type number
        	:initarg :bzh 
        	:initform 1.0
        	:accessor bzh
                :documentation "This value here is int")
   ( bzm	:type number
        	:initarg :bzm 
        	:initform 0.95
        	:accessor bzm)
   ( bzmenv	:type cs-table
		:initarg :bzmenv 
                       ; x-points y-points decimals number size
  		:initform (make-cs-table 'Gen07  '(0 2048 4096)
                                         '(0 1 0) 1 "?" 4097)
		:accessor bzmenv)
   )
  (:documentation
   "
;=============================================================================
;			BUZZ-1.ORC
; DYNAMIC SPECTRUM OSCILLATOR (FROM ACCCI, 43_21_1.ORC) / MONO
; AMPLITUDE ENVELOPE WITH POSCIL
;=============================================================================

; Timbre:    Various controlled noise spectra
; Synthesis: (g)buzz
;            POSCILI envelopes
; Coded:     jpg 8/92, modified ms 9/02, 8/08

; NB: NEW STRUCTURE FOR THE AMPLITUDES FROM AUGUST 2008!
;    Positive value > 0.0  : linear amplitude (>0.0-1000.0)
;    0.0 or negative value : amplitude in dB (0 = maximum value)

; The apparently arbitrary amplitude range (0-1000, rather than 0-1)
;         avoids printing small values with exponential notation

; Replaced oscili with poscil (precise oscillator), ms 8/08
; Default SR = 96000, recommended precision: 24 bits
;-----------------------------------------------------------------------------
;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4	= max amp [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= fundamental freq [Hz]
;	p6	= amp envelope [GEN]
;	p7	= lowest harmonic in the buzz [int]
;	p8	= % of maximum possible harmonic [0-1]
;	p9	= multiplier in the series of amp coeff [0-1]
;	p10	= envelope for the multiplier [GEN]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f5	large cosine
;_____________________________________________________________________________
"
   )
  (:icon 1002)
  )

