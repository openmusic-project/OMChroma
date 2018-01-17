;******************************************************************
;		     CLASS RAN-1
;******************************************************************

(in-package :om)

; LISP-DEFINED CLASSES SHOULD RESIDE IN THE LIBRARY'S PACKAGE AND 
;   NOT IN THE USER PACKAGE, WHICH CONTAINS ALL THE CLASSES
;   GRAPHICALLY DEFINED


(defclass! ran-1
  (cs-evt) 			; INHERIT FROM CS-EVT
  (

; GLOBAL SLOTS (LIGHT BLUE, ON THE LEFT OF THE CLASS):
;    THE METHOD BELOW TRANSFORMS THEM INTO GLOBAL SLOTS ("SHOW" UNCHEKED)
;    ATTENTION: A GLOBAL SLOT SHOULD NOT HAVE AN INITARG

   (source-code :initform
                 (load-buffer-textfile
                  (get-orc-source (get-orc "ran-1"))
                  'textfile "append")
                 :allocation :class
                 :type textfile
                 :accessor source-code)
   (numchan :initform (or (get-orc-channels (get-orc "ran-1")) 1)
             :allocation :class  :accessor numchan)
   
   (cs-inits :initform (get-cs-inits (get-orc "ran-1")) 
             :allocation :class :type list :accessor cs-inits)
   
   (orc-header :initform (list
                          "; GEN functions **********************************************************"
                          "; AUDIO FUNCTIONS"
                          "f1  0  65537  10  1			; sinus"
                          ) 
               :allocation :class :type list :accessor orc-header)
   (InstID :initform 1  :allocation :class  :accessor InstID)

; LOCAL SLOTS (RED, CORRESPONDING TO THE P-FIELDS)
;    ATTENTION: A GLOBAL SLOT SHOULD HAVE AN INITARG
   ( amp	:type number
		:initarg :amp 
  		:initform -6.0
		:accessor amp)
   ( freq	:type number
        	:initarg :freq 
        	:initform 440.0
        	:accessor freq)
   ( jtf	:type number
        	:initarg :jtf 
        	:initform 50.0
        	:accessor jtf)
   ( atk	:type number
        	:initarg :atk 
        	:initform 0.2
        	:accessor atk)
   ( dec	:type number
        	:initarg :dec 
        	:initform 0.3
        	:accessor dec)
   )

  (:documentation
   "
;=============================================================================
;			RAN-1.ORC
; RANDOM NUMBER GENERATION MODULATING THE AMPLITUDE OF AN OSCILLATOR
;    (FROM ACCCI, 10_02_2.ORC) / MONO
; AMPLITUDE ENVELOPE WITH LINEN, CONTROL OF AUDIO FUN FROM THE SCORE
;=============================================================================

; Timbre:       Noise spectra, with control of bandwidth and center freq
; Synthesis:    Random Number Generation
;               RANDI(02)
;               LINEN envelope on RANDI ring modulates an oscillator (2)
; Source:       Dodge(1985), p.92
; Coded:        jpg 8/92. modified ms 9/02, 8/08

; NB: NEW STRUCTURE FOR THE AMPLITUDES FROM AUGUST 2008!
;    Positive value > 0.0  : linear amplitude (>0.0-1000.0)
;    0.0 or negative value : amplitude in dB (0 = maximum value)

; The apparently arbitrary amplitude range (0-1000, rather than 0-1)
;         avoids printing small values with exponential notation
; Replaced oscili with poscil (precise oscillator), ms 8/08
; Default SR = 96000, recommended precision: 24 bits

; NB 1: this instrument works better with short notes.
;-----------------------------------------------------------------------------
;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4	= maximum amp [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= freq [Hz]
;	p6	= freq of the noise module [Hz]
;	p7	= attack time of the amp envlp [sec]
;	p8	= decay time of the amp envlp [sec]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f1	sine tone
;_____________________________________________________________________________
"
   )
  (:icon 1007)
  )
