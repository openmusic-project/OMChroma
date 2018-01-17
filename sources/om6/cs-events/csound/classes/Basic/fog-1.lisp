;******************************************************************
;		     CLASS FOG-1
;******************************************************************

(in-package :om)

; LISP-DEFINED CLASSES SHOULD RESIDE IN THE LIBRARY'S PACKAGE AND 
;   NOT IN THE USER PACKAGE, WHICH CONTAINS ALL THE CLASSES
;   GRAPHICALLY DEFINED


(defclass! fog-1
  (cs-evt) 			; INHERIT FROM CS-EVT
  (

; GLOBAL SLOTS (LIGHT BLUE, ON THE LEFT OF THE CLASS):
;    THE METHOD BELOW TRANSFORMS THEM INTO GLOBAL SLOTS ("SHOW" UNCHEKED)
;    ATTENTION: A GLOBAL SLOT SHOULD NOT HAVE AN INITARG

   ( source-code :initform
                 (load-buffer-textfile
                  (get-orc-source (get-orc "fog-1"))
                  'textfile "append")
                 :allocation :class
                 :type textfile
                 :accessor source-code)
   ( numchan :initform (or (get-orc-channels (get-orc "fog-1")) 1)
             :allocation :class  :accessor numchan)

   (cs-inits :initform (get-cs-inits (get-orc "fog-1")) 
             :allocation :class :type list :accessor cs-inits)

   
   (orc-header :initform (list
; to write a full path in the score file
                          "; audio file"
                          (format nil "f31  0  524288  1 ~s  0 0 0"
                                  (om-path2cmdpath (infile "basmba.aiff")))
                          "; sigmoid rise/decay"
                          "f19 0  8192  19 .5 .5 270 .5"
                          
   ) 
               :allocation :class :type list :accessor orc-header)
   (InstID :initform 1  :allocation :class  :accessor InstID)

; LOCAL SLOTS (RED, CORRESPONDING TO THE P-FIELDS)
;    ATTENTION: A GLOBAL SLOT SHOULD HAVE AN INITARG
   ( amp	:type number
		:initarg :amp 
  		:initform -6.0
		:accessor amp)
   ( f0 	:type number
        	:initarg :f0 
        	:initform 100.0
        	:accessor f0
                :documentation "Density of the grain")
   ( freq	:type number
        	:initarg :freq 
        	:initform 1.0
        	:accessor freq
                :documentation "Transposition factor (1=same as original)")
   ( bw 	:type number
        	:initarg :bw 
        	:initform 0.0
        	:accessor bw)
   ( afil	:type number
        	:initarg :afil 
        	:initform 31.0
        	:accessor afil)
   ( spd	:type number
        	:initarg :spd
        	:initform 1.0
        	:accessor spd)
   ( aenv	:type cs-table
		:initarg :aenv 
                       ; x-points y-points decimals number size
  		:initform (make-cs-table 'Gen07  '(0 160 880 4096)
                                         '(0 1 1 0) 1 "?" 4097)
                :accessor aenv)
   ( win	:type number
        	:initarg :win 
        	:initform 0.01
        	:accessor win)
   ( wdur	:type number
        	:initarg :wdur 
        	:initform 0.1
        	:accessor wdur)
   ( wout	:type number
        	:initarg :wout 
        	:initform 0.05
        	:accessor wout)
   ( oct	:type number
        	:initarg :oct 
        	:initform 0.0
        	:accessor oct)
   )


  (:documentation
   "
;=============================================================================
;			FOG-1.ORC
;	GRANULAR SYNTHESIS (NEW INSTRUMENT) / MONO
;	AMPLITUDE ENVELOPE WITH POSCIL
;=============================================================================

; Timbre:    Granular synthesis with fog module, voice-like tones
; Synthesis: FOG (Forme d'Onde Granulaire)
;            POSCIL envelopes
; Coded:     ms 9/02, 2/09

; NB:
;	this instrument works AT BEST with samples whose length is a power of 2
;	  those which are shorter than the immediately superior power of 2 accepted
;	  by the GEN01, will produce silence when looking up the unsued portion of the table

; NB1: NEW STRUCTURE FOR THE AMPLITUDES FROM AUGUST 2008!
;    Positive value > 0.0  : linear amplitude (>0.0-1000.0)
;    0.0 or negative value : amplitude in dB (0 = maximum value)

; The apparently arbitrary amplitude range (0-1000, rather than 0-1)
;    avoids printing small values with exponential notation

; Replaced oscili with poscil (precise oscillator), ms 8/08
; Default SR = 96000, recommended precision: 24 bits
;-----------------------------------------------------------------------------
;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4	= maximum amp [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= density of the grains [Hz]
;	p6	= transposition factor [1=original]
;	p7	= bandwidth -> exponential decay [Hz]
;	p8	= audio file [GEN01]
;	p9	= speed of the starting pointer in the file [1=same as original]
;	p10	= amplitude envelope [GEN]
;	p11	= rise time of the grain envelope [sec]
;	p12	= overall duration of the grain [sec]
;	p13	= decay time of the grain envelope [sec]
;	p14	= octaviation index [>= 0.0]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f31	audio file
;	f19	sigmoid rise/decay shape
;_____________________________________________________________________________
"
   )
  (:icon 1005)
  )
