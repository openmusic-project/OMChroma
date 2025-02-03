;******************************************************************
;		     CLASS RANFL-1
;******************************************************************

(in-package :om)

; LISP-DEFINED CLASSES SHOULD RESIDE IN THE LIBRARY'S PACKAGE AND 
;   NOT IN THE USER PACKAGE, WHICH CONTAINS ALL THE CLASSES
;   GRAPHICALLY DEFINED


(defclass! ranfl-1
  (cs-evt) 			; INHERIT FROM CS-EVT
  (

; GLOBAL SLOTS (LIGHT BLUE, ON THE LEFT OF THE CLASS):
;    THE METHOD BELOW TRANSFORMS THEM INTO GLOBAL SLOTS ("SHOW" UNCHEKED)
;    ATTENTION: A GLOBAL SLOT SHOULD NOT HAVE AN INITARG

   ( source-code :initform
                 (load-buffer-textfile
                  (get-orc-source (get-orc "ranfl-1"))
                  'textfile "append")
                 :allocation :class
                 :type textfile
                 :accessor source-code)
   ( numchan :initform (or (get-orc-channels (get-orc "ranfl-1")) 1)
             :allocation :class  :accessor numchan)

   (cs-inits :initform (get-cs-inits (get-orc "ranfl-1")) 
          :allocation :class :type list :accessor cs-inits)

   (orc-header :initform (list
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
        	:initform 263.8
        	:accessor f0)
   ( freq	:type number
        	:initarg :freq 
        	:initform 1600.0
        	:accessor freq)
   ( bw 	:type number
        	:initarg :bw 
        	:initform 500.0
        	:accessor bw)
   ( aenv	:type cs-table
		:initarg :aenv 
                       ; x-points y-points decimals number size
  		:initform (make-cs-table 'Gen07  '(0 2048 4096)
                                         '(0 1 0) 1 "?" 4097)
 		:accessor aenv)
 ( win 	:type number
        	:initarg :win 
        	:initform 0.03
        	:accessor win)
  ( wout	:type number
        	:initarg :wout 
        	:initform 0.5
        	:accessor wout)
   )
  (:documentation
   "
;=============================================================================
;			RANFL-1.ORC
; FREQ LIMITED RANDOM NOISE FILTERED BY A CASCADED RESON AND FOF FILTERS / MONO
; AMPLITUDE ENVELOPE WITH POSCIL
;=============================================================================

; Timbre:    Formantic subtractive synthesis with noise
; Synthesis: randi with cascaded reson and fofilter
;            POSCIL envelopes
; Coded:     ms 2/09

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
;	p5	= freq of the noise [Hz]
;	p6	= centre freq of both filters [Hz]
;	p7	= bandwidth of the reson [Hz]
;	p8	= amp envelope [GEN]
;	p9	= impulse response attack time of the fofilter [sec]
;	p10	= impulse response decay time of the fofilter [sec]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;_____________________________________________________________________________
"
   )
  (:icon 1007)
  )

