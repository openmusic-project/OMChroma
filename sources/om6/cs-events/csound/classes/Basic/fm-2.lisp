;******************************************************************
;		     CLASS FM-2
;******************************************************************

(in-package :om)

; LISP-DEFINED CLASSES SHOULD RESIDE IN THE LIBRARY'S PACKAGE AND 
;   NOT IN THE USER PACKAGE, WHICH CONTAINS ALL THE CLASSES
;   GRAPHICALLY DEFINED


(defclass! fm-2
  (cs-evt) 			; INHERIT FROM CS-EVT
  (

; GLOBAL SLOTS (LIGHT BLUE, ON THE LEFT OF THE CLASS):
;    THE METHOD BELOW TRANSFORMS THEM INTO GLOBAL SLOTS ("SHOW" UNCHEKED)
;    ATTENTION: A GLOBAL SLOT SHOULD NOT HAVE AN INITARG

   ( source-code :initform
                 (load-buffer-textfile
                  (get-orc-source (get-orc "fm-2"))
                  'textfile "append")
                 :allocation :class
                 :type textfile
                 :accessor source-code)
   ( numchan :initform (or (get-orc-channels (get-orc "fm-2")) 1)
             :allocation :class  :accessor numchan)
   
   (cs-inits :initform (get-cs-inits (get-orc "fm-2")) 
             :allocation :class :type list :accessor cs-inits)

   (orc-header :initform (list
                          
                          "; GEN functions **********************************************************"
                          "; carrier"
                          "f1  0  65537  10  1"
                          ""
                          "; modulating"
                          "f2  0  65537  10  1"                          
                          ) 
               :allocation :class :type list :accessor orc-header)

   (InstID :initform 1  :allocation :class  :accessor InstID)

; LOCAL SLOTS (RED, CORRESPONDING TO THE P-FIELDS)
;    ATTENTION: A GLOBAL SLOT SHOULD HAVE AN INITARG
  ( amp		:type number
		:initarg :amp 
  		:initform -6.0
		:accessor amp)
   ( f0		:type number ; UNUSED HERE, ONLY FOR COMPATIBILITY WITH FM-1
        	:initarg :f0 
        	:initform 100.0
        	:accessor f0)
   ( freq	:type number
        	:initarg :freq 
        	:initform 500.0
        	:accessor freq)
   ( fmod	:type number
        	:initarg :fmod 
        	:initform 100.0
        	:accessor fmod)
   ( imax	:type number
        	:initarg :imax 
        	:initform 5
        	:accessor imax)
   ( imin	:type number
        	:initarg :imin 
        	:initform 0
        	:accessor imin)
   ( aenv	:type cs-table
		:initarg :aenv 
  		:initform (make-cs-table  'Gen07  '(0 2048 4096) '(0 100 0) 1 "?" 4097)
		:accessor aenv)
   ( ienv	:type cs-table
		:initarg :ienv 
  		:initform (make-cs-table  'Gen07  '(0 2048 4096) '(0 100 0) 1 "?" 4097)
		:accessor ienv)
   )

  (:documentation
   "
;=============================================================================
;		FM-2.ORC
; FREQUENCY MODULATION (FROM ACCCI, 20_10_1.ORC) / MONO
; AMPLITUDE AND INDEX ENVELOPES WITH OSCILI
; SAME AS FM-1.ORC, BUT WITH FREQ INSTEAD OF N1
;=============================================================================

; Synthesis: FM with dynamic spectral evolution
;            Bell settings
; Source:    Chowning (1973)
; Coded:     jpg 8/92, modified ms 1/08, 8/08

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
;	p4	= maximum amp [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= fundamental freq [Hz]
;	p6	= freq of the carrier [Hz]
;	p7	= N2
;	p8	= max index
;	p9	= min index
;	p10	= amp envelope [GEN number]
;	p11	= index envelope [GEN number]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f1	carrier audio wave (sine tone)
;	f2	modulating audio wave (sine tone)
;_____________________________________________________________________________
"
   )
  (:icon 1003)
  )
