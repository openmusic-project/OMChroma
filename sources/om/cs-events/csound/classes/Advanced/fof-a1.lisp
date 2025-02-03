;******************************************************************
;		     CLASS FOF-A1
;******************************************************************

(in-package :om)

; LISP-DEFINED CLASSES SHOULD RESIDE IN THE LIBRARY'S PACKAGE AND 
;   NOT IN THE USER PACKAGE, WHICH CONTAINS ALL THE CLASSES
;   GRAPHICALLY DEFINED


(defclass! fof-a1
  (cs-evt) 			; INHERIT FROM CS-EVT
  (

; GLOBAL SLOTS (LIGHT BLUE, ON THE LEFT OF THE CLASS):
;    THE METHOD BELOW TRANSFORMS THEM INTO GLOBAL SLOTS ("SHOW" UNCHEKED)
;    ATTENTION: A GLOBAL SLOT SHOULD NOT HAVE AN INITARG

   ( source-code :initform
                 (load-buffer-textfile
                  (get-orc-source (get-orc "fof-a1"))
                  'textfile "append")
                 :allocation :class
                 :type textfile
                 :accessor source-code)
   ( numchan :initform (or (get-orc-channels (get-orc "fof-a1")) 1)
             :allocation :class  :accessor numchan)
   (cs-inits :initform (get-cs-inits (get-orc "fof-a1")) 
             :allocation :class :type list :accessor cs-inits)
   (orc-header :initform (list
                          "; large sine tone"
                          "f1  0 134217729  10  1"
                         "; sigmoid rise/decay"
                          "f19 0  2097152  19 .5 .5 270 .5"
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
        	:initform 609.0
        	:accessor freq)
   ( bw		:type number
        	:initarg :bw 
        	:initform 77.0
        	:accessor bw)
   ( aenv	:type cs-table
		:initarg :aenv 
                       ; x-points y-points decimals number size
   		:initform (make-cs-table 'Gen07  '(0 1600 7200 65536)
                                         '(0 1 1 0) 1 "?" 65537)
                :accessor aenv)
   ( win	:type number
        	:initarg :win 
        	:initform 0.003
        	:accessor win)
   ( wdur	:type number
        	:initarg :wdur 
        	:initform 0.02
        	:accessor wdur)
    ( wout	:type number
        	:initarg :wout 
        	:initform 0.007
        	:accessor wout)
   ( oct	:type number
        	:initarg :oct 
        	:initform 0.0
        	:accessor oct)
   ( phs	:type number
        	:initarg :phs 
        	:initform 0
        	:accessor phs)
   ( par1	:type number
        	:initarg :par1 
        	:initform 0
        	:accessor par1)
)

  (:documentation
   "
;=============================================================================
;			FOF-A1.ORC
; FORMANTIC WAVEFORM (FROM ACCCI, 45_01_2.ORC) / MONO
; AMPLITUDE ENVELOPE WITH POSCILI
; COMPLETE CONTROL OF THE DETAILS OF THE FOF FROM THE SCORE
;=============================================================================

; Timbre:    Granular synthesis with fof module, voice-like tones
; Synthesis: FOF (Forme d'Onde Formatique)
;            OSCILI envelopes
; Coded:     jpg 8/92, modified ms 9/04, 8/08

; NB: NEW STRUCTURE FOR THE AMPLITUDES FROM AUGUST 2008!
;    Positive value > 0.0  : linear amplitude (>0.0-1000.0)
;    0.0 or negative value : amplitude in dB (0 = maximum value)

; The apparently arbitrary amplitude range (0-1000, rather than 0-1)
;         avoids printing small values with exponential notation
; Replaced oscili with poscil (precise oscillator), ms 8/08
; Default SR = 96000, recommended precision: 24 bits
; NB1: this implementation works with both audio and sub-audio f0's and
;       allows for an independent control of rise, decay and grain duration
;-----------------------------------------------------------------------------
;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4	= max amplitude [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= fundamental frequency [Hz]
;	p6	= formant frequency [Hz]
;	p7	= bandwidth [Hz]
;	p8	= amplitude envelope [GEN number]
;	p9	= tex or rise time [sec]
;	p10	= total duration of the burst [sec]
;	p11	= decay time of the grain envelope [sec]
;	p12	= octaviation [=>0.0]
;	p13	= phase [0-1]
;	p14	= skip, if not 0, skip initalization (for legato)
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f1	large sine tone
;	f19	sigmoid rise/decay shape
;_____________________________________________________________________________
"
   )
  (:icon 1004)
  )
