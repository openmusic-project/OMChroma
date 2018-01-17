;******************************************************************
;		     CLASS ADD-1
;******************************************************************

(in-package :om)

; LISP-DEFINED CLASSES SHOULD RESIDE IN THE LIBRARY'S PACKAGE AND 
;   NOT IN THE USER PACKAGE, WHICH CONTAINS ALL THE CLASSES
;   GRAPHICALLY DEFINED

(defclass! add-1
  (cs-evt) 			; INHERIT FROM CS-EVT
  (

; GLOBAL SLOTS (LIGHT BLUE, ON THE LEFT OF THE CLASS):
;    THE METHOD BELOW TRANSFORMS THEM INTO GLOBAL SLOTS ("SHOW" UNCHEKED)
;    ATTENTION: A GLOBAL SLOT SHOULD NOT HAVE AN INITARG

   (source-code :initform
                 (load-buffer-textfile
                  (get-orc-source (get-orc "add-1"))
                  'textfile "supersede")
                 :allocation :class
                 :type textfile
                 :accessor source-code)
   (numchan :initform (or (get-orc-channels (get-orc "add-1")) 1) :allocation :class  :accessor numchan)

   (cs-inits :initform (get-cs-inits (get-orc "add-1")) :allocation :class :type list :accessor cs-inits)

   (orc-header :initform (list
                          "; GEN functions *****"
                          "; audio wave"
                          "f1  0  65537  10  10") 
               :allocation :class :type list :accessor orc-header)
   (InstID :initform 1  :allocation :class  :accessor InstID)

; LOCAL SLOTS (RED, CORRESPONDING TO THE P-FIELDS)
;    ATTENTION: A GLOBAL SLOT SHOULD HAVE AN INITARG
  ( amp		:type number
		:initarg :amp 
  		:initform -6.0
		:accessor amp)
   ( freq	:type number
        	:initarg :freq 
        	:initform 440.0
        	:accessor freq
                )
   ( aenv	:type cs-table
		:initarg :aenv 
                :initform (make-cs-table  'Gen07  '(0 2048 4096) '(0 100 0) 1 "?" 4097)
		:accessor aenv)
   )
  (:documentation
   "
;=============================================================================
;			ADD-1.ORC
; SIMPLE ADDITIVE SYNTHESIS (FROM ACCCI, 02_01_3.ORC) / MONO
; AMPLITUDE ENVELOPE WITH OSCILI
;=============================================================================

; Timbre:    gong
; Synthesis: additive same units (02)
;            basic instrument( 01)
; Source:    #420, Gong-like Sounds, Risset(1969)
; Coded:     jpg 8/93, modified ms 9/02, 8/08

; NB: NEW STRUCTURE FOR THE AMPLITUDES FROM AUGUST 2008!
;    Positive value > 0.0  : linear amplitude (>0.0-1000.0)
;    0.0 or negative value : amplitude in dB (0 = maximum value)

; The apparently arbitrary amplitude range (0-1000, rather than 0-1)
;         avoids Lisp printing small values with exponential notation

; Replaced oscili with poscil (precise oscillator), ms 8/08
; Default SR = 96000, recommended precision: 24 bits
;-----------------------------------------------------------------------------

;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4	= maximum amplitude [linear, 0.0 -> 1000.0]
;	p5	= frequency [Hz]
;	p6	= amplitude envelope [GEN number]

;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;	f1	audio wave
;_____________________________________________________________________________

"
   )
  (:icon 1001)
  )
