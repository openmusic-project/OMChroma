;******************************************************************
;		     CLASS SMPL-5
;******************************************************************

(in-package :om)

; LISP-DEFINED CLASSES SHOULD RESIDE IN THE LIBRARY'S PACKAGE AND 
;   NOT IN THE USER PACKAGE, WHICH CONTAINS ALL THE CLASSES
;   GRAPHICALLY DEFINED


(defclass! smpl-5
  (cs-evt) 			; INHERIT FROM CS-EVT
  (

; GLOBAL SLOTS (LIGHT BLUE, ON THE LEFT OF THE CLASS):
;    THE METHOD BELOW TRANSFORMS THEM INTO GLOBAL SLOTS ("SHOW" UNCHEKED)
;    ATTENTION: A GLOBAL SLOT SHOULD NOT HAVE AN INITARG

   ( source-code :initform
                 (load-buffer-textfile
                  (get-orc-source (get-orc "smpl-5"))
                  'textfile "append")
                 :allocation :class
                 :type textfile
                 :accessor source-code)
   ( numchan :initform (or (get-orc-channels (get-orc "smpl-5")) 1)
             :allocation :class  :accessor numchan)
   (cs-inits :initform (get-cs-inits (get-orc "smpl-5")) 
             :allocation :class :type list :accessor cs-inits)
   (orc-header :initform (list
                          "; GEN functions **********************************************************"
                          "; audio file"
                          (format nil "f31  0  524288  -1 ~s  0 0 0"
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
        	:initform 2.0
        	:accessor f0)
   ( afil	:type number
        	:initarg :afil 
        	:initform 31.0
        	:accessor afil)
   ( skip	:type number
        	:initarg :skip 
        	:initform 0.0
        	:accessor skip)
   ( aenv	:type cs-table
		:initarg :aenv 
                                               ; x-points y-points decimals
  		:initform (make-cs-table  'Gen07  '(0 4096) '(1 1) 1 "?" 4097)
		:accessor aenv)
  ( lpbeg	:type number
        	:initarg :lpbeg 
        	:initform 0.2
        	:accessor lpbeg)
   ( lpend	:type number
        	:initarg :lpend 
        	:initform 0.7
        	:accessor lpend)
   ( win	:type number
        	:initarg :win 
        	:initform 0.5
        	:accessor win)
   ( mode	:type number
        	:initarg :mode 
        	:initform 2
        	:accessor mode)
    )

  (:documentation
   "
;=============================================================================
;		SMPL-5.ORC
; SAMPLER READING FROM A DEFERRED TABLE WITH LOOP / MONO
; AMPLITUDE ENVELOPE WITH POSCIL
; FIXED TRANSPOSITION
;=============================================================================

; Timbre:       Reading a sound file into a deferred table, with transposition
; Synthesis:    Sampler, flooper2
; Coded:     	ms 3/09

; This instrument will loop through a deferred GEN01 table (deferred tables
;   exactly match the duration of the sound file).
; If the duration in the score is longer than the file, it will read the file
;   until loop-end, then loop between loop-beg and loop-end until the end
;   of the note's duration.

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
;	p5	= frequency [>=0, 1=same as original, 0.5=octave below, 2=octave above]
;	p6	= sound file [GEN01]
;	p7	= starting point in the table [sec]
;	p8	= amplitude envelope [GEN]
;	p9	= beginning of loop [sec]
;	p10	= end of loop [sec]
;	p11	= crossfade length [sec]
;	p12	= loop mode: 0=fwd, 1=bkwd, 2=fwd+bkwd
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;	f19	bell-shaped curve
;_____________________________________________________________________________
"
   )
  (:icon 1010)
  )
