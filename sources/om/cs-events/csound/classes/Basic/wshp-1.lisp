;******************************************************************
;		     CLASS WSHP-1
;******************************************************************

(in-package :om)

; LISP-DEFINED CLASSES SHOULD RESIDE IN THE LIBRARY'S PACKAGE AND 
;   NOT IN THE USER PACKAGE, WHICH CONTAINS ALL THE CLASSES
;   GRAPHICALLY DEFINED


(defclass! wshp-1
  (cs-evt) 			; INHERIT FROM CS-EVT
  (

; GLOBAL SLOTS (LIGHT BLUE, ON THE LEFT OF THE CLASS):
;    THE METHOD BELOW TRANSFORMS THEM INTO GLOBAL SLOTS ("SHOW" UNCHEKED)
;    ATTENTION: A GLOBAL SLOT SHOULD NOT HAVE AN INITARG

   ( source-code :initform
                 (load-buffer-textfile
                  (get-orc-source (get-orc "wshp-1"))
                  'textfile "append")
                 :allocation :class
                 :type textfile
                 :accessor source-code)
   ( numchan :initform (or (get-orc-channels (get-orc "wshp-1")) 1)
             :allocation :class  :accessor numchan)
(cs-inits :initform (get-cs-inits (get-orc "wshp-1")) 
          :allocation :class :type list :accessor cs-inits)
   (orc-header :initform (list
                          
                          "; GEN functions **********************************************************"
                          "f1   0 65537 10 1                           ; sinus"
                          ""
                          "; transfer function waveshaper"
                          "f10  0  524289  7 -1.0 204800 -.5 114688 .5 204800 1.0"
                          
                          ) 
               :allocation :class :type list :accessor orc-header)

   (InstID :initform 1  :allocation :class  :accessor InstID)

; LOCAL SLOTS (RED, CORRESPONDING TO THE P-FIELDS)
;    ATTENTION: A GLOBAL SLOT SHOULD HAVE AN INITARG
   ( amp	:type number
		:initarg :amp 
  		:initform 500.0
		:accessor amp)
   ( freq	:type number
        	:initarg :freq 
        	:initform 220.0
        	:accessor freq)
   ( atk	:type number
        	:initarg :freq 
        	:initform 0.085
        	:accessor atk)
   ( dec	:type number
        	:initarg :freq 
        	:initform 0.64
        	:accessor dec)
   )

  (:documentation
   "
;=============================================================================
;				WSHP-1.ORC
; 		WAVESHAPING (FROM ACCCI, 40_02_1.ORC) / MONO
; 		AMPLITUDE ENVELOPE WITH LINEN
;=============================================================================

; Timbre:    Clarinet-like
; Synthesis: Waveshaping
;            Basic instrument with duration dependent envelope
; Source:    Risset(1969)
;            #150, Serial Excerpt with Clarinet-like Sounds by Nonlinearity
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
;	p5	= freq [Hz]
;	p6	= attack time of the amp envlp [sec]
;	p7	= decay time of the amp envlp [sec]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f1	sine tone
;	f10	transfer function for the waveshaper
;_____________________________________________________________________________
"
   )
  (:icon 1012)
  )
