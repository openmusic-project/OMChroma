;******************************************************************
;		     CLASS FOF-4
;******************************************************************

(in-package :om)

; LISP-DEFINED CLASSES SHOULD RESIDE IN THE LIBRARY'S PACKAGE AND 
;   NOT IN THE USER PACKAGE, WHICH CONTAINS ALL THE CLASSES
;   GRAPHICALLY DEFINED


(defclass! fof-4
  (cs-evt) 			; INHERIT FROM CS-EVT
  (

; GLOBAL SLOTS (LIGHT BLUE, ON THE LEFT OF THE CLASS):
;    THE METHOD BELOW TRANSFORMS THEM INTO GLOBAL SLOTS ("SHOW" UNCHEKED)
;    ATTENTION: A GLOBAL SLOT SHOULD NOT HAVE AN INITARG

   ( source-code :initform
                 (load-buffer-textfile
                  (get-orc-source (get-orc "fof-4"))
                  'textfile "append")
                 :allocation :class
                 :type textfile
                 :accessor source-code)
   ( numchan :initform (or (get-orc-channels (get-orc "fof-4")) 1)
             :allocation :class  :accessor numchan)

   (cs-inits :initform (get-cs-inits (get-orc "fof-4")) 
             :allocation :class :type list :accessor cs-inits)

   (orc-header :initform (list
                          "; sigmoid rise/decay"
                          "f19 0  65536  19 .5 .5 270 .5"
                          "; short grain"
                          (format ()
                                  "f32 0  32768  1 ~s 0 0 0"
                                  (namestring
                                   (infile "basmba.aiff")))
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
        	:initform 220.0
        	:accessor f0)
   ( freq	:type number
        	:initarg :freq 
        	:initform 5.0
        	:accessor freq)
   ( bw		:type number
        	:initarg :bw 
        	:initform 0.0
        	:accessor bw)
   ( aenv	:type cs-table
		:initarg :aenv 
                       ; x-points y-points decimals number size
  		:initform (make-cs-table 'Gen07  '(0 4096)
                                         '(1 1) 1 "?" 4097)
                :accessor aenv)
   ( win	:type number
        	:initarg :win
        	:initform 0.003
        	:accessor win)
   ( wdur	:type number
        	:initarg :wdur 
        	:initform 0.02
        	:accessor wdur)
   ( wout	:type number
        	:initarg :wout 
        	:initform 0.05
        	:accessor wout)
   ( oct	:type number
        	:initarg :oct 
        	:initform 0.0
        	:accessor oct)
   ( afil	:type cs-table
        	:initarg :afil 
        	:initform 32
        	:accessor afil)
   )

  (:documentation
   "
;=============================================================================
;			FOF-4.ORC
; FOF-BASED GRANULAR SYNTHESIS / MONO
; SAMPLED GRAIN RATHER THAN A SINE TONE IN THE FOF
; AMPLITUDE ENVELOPE WITH POSCIL
;=============================================================================

; Timbre:    Granular synthesis with a FOF (based on Byrne Villez's work)
; Synthesis: FOF (Forme d'Onde Formatique)
;            POSCIL envelopes
; Coded:     ms 9/04, 2/09

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
;	p5	= fundamental freq [Hz]
;	p6	= formant freq [Hz]
;	p7	= bandwidth [Hz]
;	p8	= amp envelope [GEN number]
;	p9	= tex or krise [sec]
;	p10	= total duration of the burst, see debatt [sec]
;	p11	= decay of the FOF [sec]
;	p12	= octaviation [>=0.0]
;	p13	= sound file [name, sound, string, pathname or GEN01] (2.0)
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f19	sigmoid rise/decay shape
;	f32	short sample (grain), here 0.64 sec appx.
;_____________________________________________________________________________
"
   )
  (:icon 1004)
  )
