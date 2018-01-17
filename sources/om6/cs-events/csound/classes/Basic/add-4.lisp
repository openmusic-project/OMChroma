;******************************************************************
;		     CLASS ADD-4
;******************************************************************

(in-package :om)

; LISP-DEFINED CLASSES SHOULD RESIDE IN THE LIBRARY'S PACKAGE AND 
;   NOT IN THE USER PACKAGE, WHICH CONTAINS ALL THE CLASSES
;   GRAPHICALLY DEFINED


(defclass! add-4
  (cs-evt) 			; INHERIT FROM CS-EVT
  (

; GLOBAL SLOTS (LIGHT BLUE, ON THE LEFT OF THE CLASS):
;    THE METHOD BELOW TRANSFORMS THEM INTO GLOBAL SLOTS ("SHOW" UNCHEKED)
;    ATTENTION: A GLOBAL SLOT SHOULD NOT HAVE AN INITARG

   ( source-code :initform
                 (load-buffer-textfile
                  (get-orc-source (get-orc "add-4"))
                  'textfile "append")
                 :allocation :class
                 :type textfile
                 :accessor source-code)
   (numchan :initform (or (get-orc-channels (get-orc "add-4")) 1) :allocation :class  :accessor numchan)
   
   (cs-inits :initform (get-cs-inits (get-orc "add-4")) 
             :allocation :class :type list :accessor cs-inits)
   
   (orc-header :initform (list
                          "; GEN functions **********************************************************"
                          "; audio wave"
                          "f1  0   65537   10 1"
                          "; vibrato wave"
                          "f2  0   4097  -7 440.0 2048 220.0 2048 440.0"
                          
                          ) 
               :allocation :class :type list :accessor orc-header)

   (InstID :initform 1  :allocation :class  :accessor InstID)

; LOCAL SLOTS (RED, CORRESPONDING TO THE P-FIELDS)
;    ATTENTION: A GLOBAL SLOT SHOULD HAVE AN INITARG
  ( amp		:type number
		:initarg :amp 
  		:initform 1000.0
		:accessor amp)
   ( freq	:type number
        	:initarg :freq
        	:initform 1.0
        	:accessor freq)
   ( aenv	:type gen-07
		:initarg :aenv 
                                               ; x-points y-points decimals
  		:initform (make-cs-table  'Gen-07  '(0 1000 3000 4096) '(0.0 1.0 1.0 0.0) 5 "?" 4097)
		:accessor aenv)
   ( fenv	:type gen-07
		:initarg :fenv 
                                               ; x-points y-points decimals
  		:initform (make-cs-table  'Gen-07  '(0 4096) '(440.0 880.0) 5 "?" 4097)
		:accessor fenv)
   ( atk	:type number
        	:initarg :atk 
        	:initform 0.01
        	:accessor atk)
   ( dec  :type number
          :initarg :dec 
          :initform 0.01
          :accessor dec)
   ( phs  :type number
          :initarg :phs 
          :initform 0.0
          :accessor phs)
    )


  (:documentation
   "
;=============================================================================
;   ADD4.ORC
; SIMPLE ADDITIVE SYNTHESIS ADAPTED TO READ PARTIAL ANALYSIS DATA / MONO
; AMPLITUDE ENVELOPE WITH POSCIL AND ATTACK/RELEASE VALUES TO AVOID CLICKS
; CONTROL OF THE INITIAL PHASE OF THE AUDIO OSCILLATOR
;=============================================================================

; Timbre:    simple additive synthesis with variable amplitude and frequency
; Synthesis: additive same units
; Coded:     ms 17/07

; This class reads absolute values for amplitudes and frequencies
;   coming from analysis data. In this case, set freq to 1.0 (scaler) and amplitude
;   to 1000.0 or 0.0 (max dB). Note that GEN functions should have a negative number
;   in order not to be rescaled (GEN 0 -7)
; It can also be used with normalized amp and freq functions, and, in this case,
;   amp and freq should have a reasonable value.

; NB: NEW STRUCTURE FOR THE AMPLITUDES FROM AUGUST 2008!
;    Positive value > 0.0  : linear amplitude (>0.0-1000.0)
;    0.0 or negative value : amplitude in dB (0 = maximum value)

; The apparently arbitrary amplitude range (0-1000, rather than 0-1)
;         avoids printing small values with exponential notation

; Replaced oscili with poscil (precise oscillator), ms 8/08
; Default SR = 96000, recommended precision: 24 bits
;-----------------------------------------------------------------------------
; p1  = instrument number
; p2  = action time [sec]
; p3  = duration [sec]
; p4  = max amp [linear, >0.0-1000.0 or dB, <= 0.0]
; p5  = frequency [Hz or scaler]
; p6  = amplitude envelope [GEN number]
; p7  = frequency envelope [GEN number]
; p8  = attack time of the amp envlp [sec]
; p9  = decay time of the amp envlp [sec]
; p10 = initial phase of the audio oscillator [rad]

;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
; f1  audio wave
;_____________________________________________________________________________

; CLASS: ADD-4

;  GLOBAL KEYWORDS (default values within parentheses):
; NUMROWS   : amount of rows (components) in the event (1)
; ACTION-TIME : start time of the whole event [sec] (0.0)
; USER-FUN  : user-defined parsing function (nil)

;  LOCAL KEYWORDS:
; E-DELS  : entry delays [sec] (0.0)
; DURS  : duration [sec] (1.0)
; AMP   : amplitude [lin, >0.0-1000.0 or dB <- 0.0] (1000.0)
; FREQ  : frequency scalre [0-1 or Hz] (1.0)
; AENV  : fun number for the amp envlp [absolute GEN = negative GEN07] (trapezoid)
; FENV  : fun number for frequency env [absolute GEN = negative GEN07] (upward gliss)
; ATK   : attack time [sec] (0.01)
; DEC   : decay time [sec] (0.01)
; PHS   : initial phase [rad] (0.0)
;*****************************************************************************
"
   )
  (:icon 1001)
  )
