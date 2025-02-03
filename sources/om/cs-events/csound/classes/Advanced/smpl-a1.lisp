;******************************************************************
;		     CLASS SMPL-a1
;******************************************************************

(in-package :om)

; LISP-DEFINED CLASSES SHOULD RESIDE IN THE LIBRARY'S PACKAGE AND 
;   NOT IN THE USER PACKAGE, WHICH CONTAINS ALL THE CLASSES
;   GRAPHICALLY DEFINED


(defclass! smpl-a1
  (cs-evt) 			; INHERIT FROM CS-EVT
  (

; GLOBAL SLOTS (LIGHT BLUE, ON THE LEFT OF THE CLASS):
;    THE METHOD BELOW TRANSFORMS THEM INTO GLOBAL SLOTS ("SHOW" UNCHEKED)
;    ATTENTION: A GLOBAL SLOT SHOULD NOT HAVE AN INITARG

   ( source-code :initform
                 (load-buffer-textfile
                  (get-orc-source (get-orc "smpl-a1"))
                  'textfile "append")
                 :allocation :class
                 :type textfile
                 :accessor source-code)
   ( numchan :initform (or (get-orc-channels (get-orc "smpl-a1")) 1)
             :allocation :class  :accessor numchan)
   (cs-inits :initform (get-cs-inits (get-orc "smpl-a1")) 
             :allocation :class :type list :accessor cs-inits)
   (orc-header :initform (list
                          "; GEN functions **********************************************************"
                          "; sigmoid rise/decay"
                          "f19 0  2097152  19 .5 .5 270 .5"
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
  		:initform (make-cs-table  'Gen07  '(0 65536) '(0 100) 1 "?" 65537)
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
   ( afil	:type t
        	:initarg :afil 
        	:initform (infile "santuri.aiff")
        	:accessor afil)
   ( skip	:type number
        	:initarg :skip 
        	:initform 0.0
        	:accessor skip)
   ( aenv	:type cs-table
		:initarg :aenv 
                                               ; x-points y-points decimals
  		:initform (make-cs-table  'Gen07  '(0 32768 65536) '(0 100 0) 1 "?" 65537)
		:accessor aenv)
   ( win	:type number
        	:initarg :win 
        	:initform 0.01
        	:accessor win)
   ( wrap	:type number
        	:initarg :wrap 
        	:initform 1
        	:accessor wrap)
   )

  (:documentation
   "
;=============================================================================
;		SMPL-a1.ORC
; SAMPLER READING FROM A SOUND FILE WITH AUTOMATIC SR CONVERSION / MONO
; READING SAMPLES THROUGH DISKIN2, NO LOOP
; CONTROLLABLE WRAP
; AMPLITUDE ENVELOPE WITH POSCILI + COSINE IN-OUT
; DYNAMIC TRANSPOSITION AND AMPLITUDE CONTROL + JITTER
;=============================================================================

; Timbre:       Reading from a sound file, with transposition
; Synthesis:    Sampler
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
;	p4	= max amp [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= min transposition factor [1=same as file]
;	p6	= max transposition factor [1=same as file]
;	p7	= envelope for the transposition factor [GEN]
;	p8	= jitter amplitude for the transposition factor [0-1]
;	p9	= duration of the transposition envelope [sec]
;	p10	= jitter amplitude for the max amplitude [0-1]
;	p11	= sound file [name]
;	p12	= starting point in file [sec]
;	p13	= amp envelope [GEN]
;	p14	= duration of the local attack/decay [sec]
;	p15	= wrap [0 or non 0]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;	f19	sigmoid rise/decay
;_____________________________________________________________________________
"
   )
  (:icon 1008)
  )
