;******************************************************************
;		     CLASS ADD-3
;******************************************************************

(in-package :om)

; LISP-DEFINED CLASSES SHOULD RESIDE IN THE LIBRARY'S PACKAGE AND 
;   NOT IN THE USER PACKAGE, WHICH CONTAINS ALL THE CLASSES
;   GRAPHICALLY DEFINED


(defclass! add-3
  (cs-evt) 			; INHERIT FROM CS-EVT
  (

; GLOBAL SLOTS (LIGHT BLUE, ON THE LEFT OF THE CLASS):
;    THE METHOD BELOW TRANSFORMS THEM INTO GLOBAL SLOTS ("SHOW" UNCHEKED)
;    ATTENTION: A GLOBAL SLOT SHOULD NOT HAVE AN INITARG

   ( source-code :initform
                 (load-buffer-textfile
                  (get-orc-source (get-orc "add-3"))
                  'textfile "append")
                 :allocation :class
                 :type textfile
                 :accessor source-code)
   (numchan :initform (or (get-orc-channels (get-orc "add-3")) 1) :allocation :class  :accessor numchan)
   
   (cs-inits :initform (get-cs-inits (get-orc "add-3")) 
             :allocation :class :type list :accessor cs-inits)
   
   (orc-header :initform (list
                          "; GEN functions **********************************************************"
                          "; audio wave"
                          "f1  0   65537   10 1"
                          "; vibrato wave"
                          "f2  0   65537  10 1"
                          "; glissando wave"
                          "f6  0   4097  7  0  4096  1"
                          
                          ) 
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
        	:accessor freq)
   ( aenv	:type cs-table
		:initarg :aenv 
                                               ; x-points y-points decimals
  		:initform (make-cs-table  'Gen07  '(0 2048 4096) '(0 100 0) 1 "?" 4097)
		:accessor aenv)
   ( fdev	:type number
        	:initarg :fdev 
        	:initform 0.06
        	:accessor fdev)
   ( vfq	:type number
        	:initarg :vfq 
        	:initform 1.0
        	:accessor vfq)
   ( fenv	:type cs-table
		:initarg :fenv 
                                               ; x-points y-points decimals
  		:initform (make-cs-table  'Gen07  '(0 4096) '(0 1) 1 "?" 4097)
		:accessor fenv)
   )

  (:documentation
   "
;=============================================================================
;		ADD3.ORC
; SIMPLE ADDITIVE SYNTHESIS WITH FREQUENCY MODULATION (GLISS OR VIBR) / MONO
; AMPLITUDE ENVELOPE WITH OSCILI
; NO ENVELOPE FOR THE AMPLITUDE OF THE VIBRATO
;=============================================================================

; Timbre:    simple additive glissando
; Synthesis: additive same units
; Coded:     ms 12/07, 8/08

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
;	p5	= frequency [Hz]
;	p6	= amplitude envelope [GEN number]
;	p7	= amplitude of the vibrato [0-1]
;	p8	= frequency of the vibrato [Hz]
;	p9	= vibrato envelope [GEN number]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;	f1	audio wave
;	f2	vibrato function
;	f5	raising segment for glissandi
;_____________________________________________________________________________
"
   )
  (:icon 1001)
  )
