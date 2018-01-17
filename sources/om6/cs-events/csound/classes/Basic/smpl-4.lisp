;******************************************************************
;		     CLASS SMPL-4
;******************************************************************

(in-package :om)

; LISP-DEFINED CLASSES SHOULD RESIDE IN THE LIBRARY'S PACKAGE AND 
;   NOT IN THE USER PACKAGE, WHICH CONTAINS ALL THE CLASSES
;   GRAPHICALLY DEFINED


(defclass! smpl-4
  (cs-evt) 			; INHERIT FROM CS-EVT
  (

; GLOBAL SLOTS (LIGHT BLUE, ON THE LEFT OF THE CLASS):
;    THE METHOD BELOW TRANSFORMS THEM INTO GLOBAL SLOTS ("SHOW" UNCHEKED)
;    ATTENTION: A GLOBAL SLOT SHOULD NOT HAVE AN INITARG

   ( source-code :initform
                 (load-buffer-textfile
                  (get-orc-source (get-orc "smpl-4"))
                  'textfile "append")
                 :allocation :class
                 :type textfile
                 :accessor source-code)
   ( numchan :initform (or (get-orc-channels (get-orc "smpl-4")) 1)
             :allocation :class  :accessor numchan)
   (cs-inits :initform (get-cs-inits (get-orc "smpl-4")) 
             :allocation :class :type list :accessor cs-inits)
   (orc-header :initform (list
                          "; GEN functions **********************************************************"
                          "; audio file"
                          (format nil "f31  0  524288  1 ~s  0 0 0"
                                  (om-path2cmdpath (infile "santuri.aiff")))
                          "; sigmoid rise/decay"
                          "f19 0  65537  19 .5 .5 270 .5"
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
        	:initform -1.0
        	:accessor f0)
   ( afil	:type number
        	:initarg :afil 
        	:initform 31.0
        	:accessor afil)
   ( skip	:type number
        	:initarg :skip 
        	:initform 0.5
        	:accessor skip)
   ( aenv	:type cs-table
		:initarg :aenv 
                                               ; x-points y-points decimals
  		:initform (make-cs-table  'Gen07  '(0 4097) '(1 1) 1 "?" 4097)
		:accessor aenv)
   ( win	:type number
        	:initarg :win 
        	:initform 0.01
        	:accessor win)
   )

  (:documentation
   "
;=============================================================================
;		SMPL-4.ORC
; SAMPLER STORING A SOUND FILE INTO A DEFERRED TABLE [GEN01] / MONO
; READING SAMPLES FROM THE TABLE / NOT NORMALIZED
; AMPLITUDE ENVELOPE WITH POSCIL3 + COSINE IN-OUT
; THIS SAMPLER WILL START THE FILE AGAIN, IF IT REACHES THE END OF THE TABLE
;   SO BEWARE OF LONG NOTES OR HIGH TRANSPOSITION FACTORS
; SAME AS SMPL-3.ORC, BUT WITH RELATIVE STARTING POINT
;=============================================================================

; Timbre:       Reading from a sound file, with transposition
; Synthesis:    Sampler
; Coded:     	ms 3/09

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
;	p4	= max amplitude [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= transposition factor [1=same freq as original]
;	p6	= sound file [GEN01]
;	p7	= starting point in file [sec]
;	p8	= amp envelope [GEN]
;	p9	= duration of the local attack/decay time [sec]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;	f19	sigmoid rise/decay
;_____________________________________________________________________________
"
   )
  (:icon 1009)
  )
