;******************************************************************
;		     CLASS SMPL-a4
;******************************************************************

(in-package :om)

; LISP-DEFINED CLASSES SHOULD RESIDE IN THE LIBRARY'S PACKAGE AND 
;   NOT IN THE USER PACKAGE, WHICH CONTAINS ALL THE CLASSES
;   GRAPHICALLY DEFINED


(defclass! smpl-a4
  (cs-evt) 			; INHERIT FROM CS-EVT
  (

; GLOBAL SLOTS (LIGHT BLUE, ON THE LEFT OF THE CLASS):
;    THE METHOD BELOW TRANSFORMS THEM INTO GLOBAL SLOTS ("SHOW" UNCHEKED)
;    ATTENTION: A GLOBAL SLOT SHOULD NOT HAVE AN INITARG

   ( source-code :initform
                 (load-buffer-textfile
                  (get-orc-source (get-orc "smpl-a4"))
                  'textfile "append")
                 :allocation :class
                 :type textfile
                 :accessor source-code)
   ( numchan :initform (or (get-orc-channels (get-orc "smpl-a4")) 1)
             :allocation :class  :accessor numchan)
   (cs-inits :initform (get-cs-inits (get-orc "smpl-a4")) 
             :allocation :class :type list :accessor cs-inits)
   (orc-header :initform (list
                          "; GEN functions **********************************************************"
                          "; sigmoid rise/decay"
                          "f19 0  65537  19 .5 .5 270 .5"
                          "; audio file"
                          (format nil "f31  0  0  -1 ~s  0 0 0"
                                  (om-path2cmdpath (infile "santuri.aiff")))
                          ) 
               :allocation :class :type list :accessor orc-header)

   (InstID :initform 1  :allocation :class  :accessor InstID)

; LOCAL SLOTS (RED, CORRESPONDING TO THE P-FIELDS)
;    ATTENTION: A GLOBAL SLOT SHOULD HAVE AN INITARG
  ( amp		:type number
		:initarg :amp 
  		:initform 0.0
		:accessor amp)
   ( f0 	:type number
        	:initarg :f0
        	:initform 1.0
        	:accessor f0)
   ( f0max 	:type number
        	:initarg :f0max
        	:initform 2.0
        	:accessor f0max)
   ( f0env 	:type cs-table
        	:initarg :f0env
                                               ; x-points y-points decimals
  		:initform (make-cs-table  'Gen07  '(0 4096) '(0 100) 1 "?" 4097)
        	:accessor f0env)
   ( f0jta 	:type number
        	:initarg :f0jta
        	:initform 0.1
        	:accessor f0jta)
   ( f0dur 	:type number
        	:initarg :f0dur
        	:initform 1.0
        	:accessor f0dur)
   ( jta 	:type number
        	:initarg :jta
        	:initform 0.5
        	:accessor jta)
   ( afil	:type number
        	:initarg :afil 
        	:initform 31
        	:accessor afil)
   ( skip	:type number
        	:initarg :skip 
        	:initform 0.0
        	:accessor skip)
   ( aenv	:type cs-table
		:initarg :aenv 
                                               ; x-points y-points decimals
  		:initform (make-cs-table  'Gen07  '(0 2048 4096) '(0 100 0) 1 "?" 4097)
		:accessor aenv)
   ( win	:type number
        	:initarg :win 
        	:initform 0.01
        	:accessor win)
   )

  (:documentation
   "
;=============================================================================
;		SMPL-a4.ORC
; SAMPLER STORING A SOUND FILE INTO A DEFERRED TABLE [GEN01] / MONO
; READING SAMPLES FROM THE TABLE / NOT NORMALIZED
; AMPLITUDE ENVELOPE WITH POSCIL + COSINE IN-OUT
; THIS SAMPLER WILL START THE FILE AGAIN, IF IT REACHES THE END OF THE TABLE
;   SO BEWARE OF LONG NOTES OR HIGH TRANSPOSITION FACTORS
; DYNAMIC TRANSPOSITION AND AMPLITUDE CONTROL + JITTER
; SAME AS SMPL-a3.ORC, BUT WITH RELATIVE STARTING AND LOOP POINTS
;=============================================================================

; Timbre:       Reading from a sound file, with transposition
; Synthesis:    Sampler, poscil3
; Coded:     	ms 3/09

; NB: NEW STRUCTURE FOR THE AMPLITUDES FROM AUGUST 2008!
;    Positive value > 0.0  : linear amplitude (>0.0-1000.0)
;    0.0 or negative value : amplitude in dB (0 = maximum value)

; The apparently arbitrary amplitude range (0-1000, rather than 0-1)
;     avoids printing small values with exponential notation

; Default SR = 96000, recommended precision: 24 bits
;-----------------------------------------------------------------------------
;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4	= max amplitude [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= min transposition factor [1=same as the file]
;	p6	= max transposition factor
;	p7	= envelope for the transposition factor [GEN]
;	p8	= jitter amplitude for the transposition factor [0-1]
;	p9	= duration of the transposition envelope [sec]
;	p10	= jitter amplitude for the max amplitude [0-1]
;	p11	= sound file [GEN01] (santuri.aiff)
;	p12	= starting point in file [%]
;	p13	= amp envelope [GEN]
;	p14	= duration of the local attack/decay time [sec]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;	f19	sigmoid rise/decay
;_____________________________________________________________________________

; CLASS: SMPL-A4

;  GLOBAL KEYWORDS (default values within parentheses):
;	NUMROWS		: amount of rows (components) in the event (1)
;	ACTION-TIME	: start time of the whole event [sec] (0.0)
;	USER-FUN	: user-defined parsing function (nil)

;  LOCAL KEYWORDS:
;	E-DELS	: entry delays [sec] (0.0)
;	DURS	: duration [sec] (1.0)
;	AMP 	: max amplitude [lin, >0.0-1000.0 or dB <- 0.0] (-6.0)
;	F0  	: min transposition factor [1=same as file] (1.0)
;	F0MAX  	: max transposition factor (2.0)
;	F0ENV  	: envelope for the transposition factor [GEN] (asc line)
;	F0JTA  	: jitter amp for the transposition factor [0-1] (0.1)
;	F0DUR  	: envelope's duration for the transposition factor [sec] (1.0)
;	JTA 	: jitter amp for maxamp [0-1] (0.5)
;	AFIL	: file name [GEN01] (santuri.aiff)
;	SKIP	: starting point in the file [%] (0.0)
;	AENV	: amp envelope [GEN] (straight line=1)
;	WIN 	: duration of the local attack/decay [sec] (0.01)
;*****************************************************************************
"
   )
  (:icon 1009)
  )
