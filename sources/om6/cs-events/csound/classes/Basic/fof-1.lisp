;******************************************************************
;		     CLASS FOF-1
;******************************************************************

(in-package :om)

; LISP-DEFINED CLASSES SHOULD RESIDE IN THE LIBRARY'S PACKAGE AND 
;   NOT IN THE USER PACKAGE, WHICH CONTAINS ALL THE CLASSES
;   GRAPHICALLY DEFINED


(defclass! fof-1
  (cs-evt) 			; INHERIT FROM CS-EVT
  (

; GLOBAL SLOTS (LIGHT BLUE, ON THE LEFT OF THE CLASS):
;    THE METHOD BELOW TRANSFORMS THEM INTO GLOBAL SLOTS ("SHOW" UNCHEKED)
;    ATTENTION: A GLOBAL SLOT SHOULD NOT HAVE AN INITARG

   ( source-code :initform
                 (load-buffer-textfile
                  (get-orc-source (get-orc "fof-1"))
                  'textfile "append")
                 :allocation :class
                 :type textfile
                 :accessor source-code)
   ( numchan :initform (or (get-orc-channels (get-orc "fof-1")) 1)
             :allocation :class  :accessor numchan)

   (cs-inits :initform (get-cs-inits (get-orc "fof-1")) 
             :allocation :class :type list :accessor cs-inits)

  (orc-header :initform (list      
                          "; large non interpolating sine tone"
                          "f4  0 16777216  10  1"
                          "; sigmoid rise/decay"
                          "f19 0  65536  19 .5 .5 270 .5"
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
   ( freq	:type number
        	:initarg :freq 
        	:initform 609
        	:accessor freq)
   ( bw		:type number
        	:initarg :bw 
        	:initform 77
        	:accessor bw)
   ( aenv	:type cs-table
		:initarg :aenv 
                       ; x-points y-points decimals number size
  		:initform (make-cs-table 'Gen07  '(0 80 3600 4096)
                                         '(0 1 1 0) 1 "?" 4097)
                :accessor aenv)
   
   )

  (:documentation
   "
;=============================================================================
;			FOF-1.ORC
; FORMANTIC WAVEFORM (FROM ACCCI, 45_01_2.ORC) / MONO
; AMPLITUDE ENVELOPE WITH POSCIL
; NO CONTROL OF THE DETAILS OF THE FOF
;=============================================================================

; Timbre:    Granular synthesis with fof module, voice-like tones
; Synthesis: FOF (Forme d'Onde Formatique)
;            POSCIL envelopes
; Coded:     jpg 8/92, modified ms 9/02, 8/08

; NB: NEW STRUCTURE FOR THE AMPLITUDES FROM AUGUST 2008!
;    Positive value > 0.0  : linear amplitude (>0.0-1000.0)
;    0.0 or negative value : amplitude in dB (0 = maximum value)

; The apparently arbitrary amplitude range (0-1000, rather than 0-1)
;         avoids printing small values with exponential notation
; Replaced oscili with poscil (precise oscillator), ms 8/08
; Default SR = 96000, recommended precision: 24 bits
; NB1: this implementation works better with audio fundamental frequencies
;-----------------------------------------------------------------------------
;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4	= max amp [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= fundamental freq [Hz]
;	p6	= formant freq [Hz]
;	p7	= bandwidth [Hz]
;	p8	= amp envelope [GEN number]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f4	large non interpolating sine tone
;	f19	sigmoid rise/decay shape
;_____________________________________________________________________________
"
   )
  (:icon 1004)
  )