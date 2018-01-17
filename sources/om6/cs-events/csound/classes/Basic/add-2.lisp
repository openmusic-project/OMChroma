;******************************************************************
;		     CLASS ADD-2
;******************************************************************

(in-package :om)

; LISP-DEFINED CLASSES SHOULD RESIDE IN THE LIBRARY'S PACKAGE AND 
;   NOT IN THE USER PACKAGE, WHICH CONTAINS ALL THE CLASSES
;   GRAPHICALLY DEFINED


(defclass! add-2
  (cs-evt) 			; INHERIT FROM CS-EVT
  (

; GLOBAL SLOTS (LIGHT BLUE, ON THE LEFT OF THE CLASS):
;    THE METHOD BELOW TRANSFORMS THEM INTO GLOBAL SLOTS ("SHOW" UNCHEKED)
;    ATTENTION: A GLOBAL SLOT SHOULD NOT HAVE AN INITARG

   ( source-code :initform
                 (load-buffer-textfile
                  (get-orc-source (get-orc "add-2"))
                  'textfile "append")
                 :allocation :class
                 :type textfile
                 :accessor source-code)
   ( numchan :initform (or (get-orc-channels (get-orc "add-2")) 1)
             :allocation :class  :accessor numchan)

   (cs-inits :initform (get-cs-inits (get-orc "add-2")) 
             :allocation :class :type list :accessor cs-inits)

   (orc-header :initform (list
                          
                          "; GEN functions **********************************************************"
                          ""
                          "; waveform"
                          "f1 0 65537 10 1"
                          
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
        	:initform 440.0
        	:accessor f0)
   ( atk	:type number
        	:initarg :atk 
        	:initform 0.003
        	:accessor atk)
   ( dec	:type number
        	:initarg :dec 
        	:initform 0.01
        	:accessor dec)
   ( scal	:type number
        	:initarg :scal 
        	:initform 1.0
        	:accessor scal)
   ( jta	:type number
        	:initarg :jta
        	:initform 0.02
        	:accessor jta)
   ( jtf	:type number
        	:initarg :jtf 
        	:initform 10.0
        	:accessor jtf)
   )

  (:documentation
   "
;=============================================================================
;		ADD-2.ORC
; SIMPLE ADDITIVE SYNTHESIS (FROM ACCCI, 02_41_1.ORC) / MONO
; AMPLITUDE ENVELOPE WITH LINEN
;=============================================================================
; Timbre:    Brass
; Synthesis: Additive, same building blocks units
;            Basic instrument with added random frequency variation
; Source:    #200, Brass-like Sounds through Independent Control of
;            Harmonics, Risset (1969)
; Coded:     jpg 8/93, modified ms 9/02, 8/08, 11/15

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
;	p6	= attack time of the amp envlp [sec]
;	p7	= decay time of the amp envlp [sec]
;	p8	= spectral scaler for the fund freq
;	p9	= % of jitter (low-freq random freq variation) [0-1]
;	p10	= frequency of the jitter [Hz]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;	f1	audio wave
;_____________________________________________________________________________
"
   )
  (:icon 1001)
  )

