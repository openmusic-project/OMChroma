;******************************************************************
;		     CLASS SNARE-1
;******************************************************************

(in-package :om)

; LISP-DEFINED CLASSES SHOULD RESIDE IN THE LIBRARY'S PACKAGE AND 
;   NOT IN THE USER PACKAGE, WHICH CONTAINS ALL THE CLASSES
;   GRAPHICALLY DEFINED


(defclass! snare-1
  (cs-evt) 			; INHERIT FROM CS-EVT
  (

; GLOBAL SLOTS (LIGHT BLUE, ON THE LEFT OF THE CLASS):
;    THE METHOD BELOW TRANSFORMS THEM INTO GLOBAL SLOTS ("SHOW" UNCHEKED)
;    ATTENTION: A GLOBAL SLOT SHOULD NOT HAVE AN INITARG

   ( source-code :initform
                 (load-buffer-textfile
                  (get-orc-source (get-orc "snare-1"))
                  'textfile "append")
                 :allocation :class
                 :type textfile
                 :accessor source-code)
   ( numchan :initform (or (get-orc-channels (get-orc "snare-1")) 1)
             :allocation :class  :accessor numchan)
   (cs-inits :initform (get-cs-inits (get-orc "snare-1")) 
             :allocation :class :type list :accessor cs-inits)
   (orc-header :initform (list
                          "; GEN functions **********************************************************"
                          "; waveforms"
                          "f11  0  65537 10 1"
                          "f12  0  262145 9  10  1  0"
                          "f13  0  262145 9  10  1  0  16  1  0  22  1  0  23  1  0"
                          ""
                          "; envelopes"
                          "f21  0  4097  5   256 4097  1"
                          "f22  0  4097  5  4096  4097 1	; steeper slope"
                          ) 
               :allocation :class :type list :accessor orc-header)
   (InstID :initform 1  :allocation :class  :accessor InstID)

; LOCAL SLOTS (RED, CORRESPONDING TO THE P-FIELDS)
;    ATTENTION: A GLOBAL SLOT SHOULD HAVE AN INITARG
  ( amp		:type number
		:initarg :amp 
  		:initform 200.0
		:accessor amp)
   ( freq	:type number
        	:initarg :freq 
        	:initform 20.0
        	:accessor freq)
   ( inha	:type number
        	:initarg :inha 
        	:initform 75.0
        	:accessor inha)
   ( jta	:type number
        	:initarg :jta 
        	:initform 250.0
        	:accessor jta)
   ( jtf	:type number
        	:initarg :jtf 
        	:initform 4000.0
        	:accessor jtf)
   ( bw 	:type number
        	:initarg :bw 
        	:initform 1500.0
        	:accessor bw)
   )

  (:documentation
   "
;=============================================================================
;			SNARE-1.ORC
; SIMPLE SIMULATION OF A SNARE DRUM (FROM ACCCI, 03_01_1.ORC) / MONO
;=============================================================================

; Timbre:    Drum and snare drum
; Synthesis: Additive different units
;            Units: noise / inharm / fundamental
; Source:    #400, Drum and Snare-drum like Sounds, Risset (1969)
; Coded:     jpg 8/93, modified ms 9/02, 8/08

; NB: NEW STRUCTURE FOR THE AMPLITUDES FROM AUGUST 2008!
;    Positive value > 0.0  : linear amplitude (>0.0-1000.0)
;    0.0 or negative value : amplitude in dB (0 = maximum value)

; The apparently arbitrary amplitude range (0-1000, rather than 0-1)
;         avoids printing small values with exponential notation
; Replaced oscili with poscil (precise oscillator), ms 8/08
; Default SR = 96000, recommended precision: 24 bits

; NB1: this instrument works better with short durations
;-----------------------------------------------------------------------------
;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4	= maximum amp [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= fundamental freq [Hz]
;	p6	= amp of pseudo inharmonic tone [linear, 0.0 -> 1000.0]
;	p7	= amp of the noise [linear, >0.0-1000.0 or dB, <= 0.0]
;	p8	= centre freq for the noise band [Hz]
;	p9	= noise's 1/2 bandwidth [Hz]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;	f11	noise-modulated sine wave
;	f12	sine wave with only one high partial (10th)
;	f13	pseudo-inharmonic spectrum made of high partials
;	f21	slowly descending exponential envelope
;	f22	rapidly descending exponential envelope
;_____________________________________________________________________________
"
   )
  (:icon 1013)
  )
