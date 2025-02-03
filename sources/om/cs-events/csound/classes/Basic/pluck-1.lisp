;******************************************************************
;		     CLASS PLUCK-1
;******************************************************************

(in-package :om)

; LISP-DEFINED CLASSES SHOULD RESIDE IN THE LIBRARY'S PACKAGE AND 
;   NOT IN THE USER PACKAGE, WHICH CONTAINS ALL THE CLASSES
;   GRAPHICALLY DEFINED


(defclass! pluck-1
  (cs-evt) 			; INHERIT FROM CS-EVT
  (
   
   ; GLOBAL SLOTS (LIGHT BLUE, ON THE LEFT OF THE CLASS):
   ;    THE METHOD BELOW TRANSFORMS THEM INTO GLOBAL SLOTS ("SHOW" UNCHEKED)
   ;    ATTENTION: A GLOBAL SLOT SHOULD NOT HAVE AN INITARG
   
   ( source-code :initform
                 (load-buffer-textfile
                  (get-orc-source (get-orc "pluck-1"))
                  'textfile "append")
                 :allocation :class
                 :type textfile
                 :accessor source-code)
   ( numchan :initform (or (get-orc-channels (get-orc "pluck-1")) 1)
             :allocation :class  :accessor numchan)


   (cs-inits :initform (get-cs-inits (get-orc "pluck-1")) 
             :allocation :class :type list :accessor cs-inits)

   
   (orc-header :initform (list
   
   "; GEN functions **********************************************************"
   ""
   "; none"
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
   ( dec	:type number
        	:initarg :dec
        	:initform 0.8
        	:accessor dec)
   )
  
  
  (:documentation
   "
;=============================================================================
;		PLUCK-1.ORC
; PLUCKED STRING USING THE KARPLUS-STRONG MODEL
;    (FROM ACCCI, 15_01_3.ORC) / MONO
; AMPLITUDE ENVELOPE WITH LINSEG
;=============================================================================

; Timbre:       Plucked string
; Synthesis:    Karplus-Strong algorithm
;               PLUCK
;               LINSEG envelope, cembalo sounds
; Coded:     	jpg 8/93, modified ms 9/02, 8/08

; NB: NEW STRUCTURE FOR THE AMPLITUDES FROM AUGUST 2008!
;    Positive value > 0.0  : linear amplitude (>0.0-1000.0)
;    0.0 or negative value : amplitude in dB (0 = maximum value)

; The apparently arbitrary amplitude range (0-1000, rather than 0-1)
;         avoids printing small values with exponential notation
; Default SR = 96000, recommended precision: 24 bits
;-----------------------------------------------------------------------------
;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4	= maximum amp [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= frequency [Hz]
;	p6	= intended freq value [Hz]
;	p7	= % of the total duration used for the last decay [0-1]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
; none
;_____________________________________________________________________________
"
   )
  (:icon 1006)
  )

