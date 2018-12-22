;******************************************************************
;		     CLASS FOF-A4
;******************************************************************

(in-package om)

; LISP-DEFINED CLASSES SHOULD RESIDE IN THE LIBRARY'S PACKAGE AND 
;   NOT IN THE USER PACKAGE, WHICH CONTAINS ALL THE CLASSES
;   GRAPHICALLY DEFINED


(defclass! fof-a4
  (cs-evt) 			; INHERIT FROM CS-EVT
  (

; GLOBAL SLOTS (LIGHT BLUE, ON THE LEFT OF THE CLASS):
;    THE METHOD BELOW TRANSFORMS THEM INTO GLOBAL SLOTS ("SHOW" UNCHEKED)
;    ATTENTION: A GLOBAL SLOT SHOULD NOT HAVE AN INITARG

   ( source-code :initform
                 (load-buffer-textfile
                  (get-orc-source (get-orc "fof-a4"))
                  'textfile "append")
                 :allocation :class
                 :type textfile
                 :accessor source-code)
   ( numchan :initform (or (get-orc-channels (get-orc "fof-a4")) 1)
             :allocation :class  :accessor numchan)
   (cs-inits :initform (get-cs-inits (get-orc "fof-a4")) 
             :allocation :class :type list :accessor cs-inits)
   (orc-header :initform (list
                          "; sigmoid rise/decay"
                          "f19 0  2097152  19 .5 .5 270 .5"
                          "; short grain"
                          (format ()
                                  "f32 0  65536  1 ~s 0 0 0"
                                  (namestring
                                   (infile "short.aiff")))
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
        	:initform 131.0
        	:accessor f0)
   ( f0max	:type number
        	:initarg :f0max
        	:initform 165.0
        	:accessor f0max)
   ( f0env	:type cs-table
        	:initarg :f0env
                       ; x-points y-points decimals number size
  		:initform (make-cs-table 'Gen07  '(0 65536)
                                         '(0 1) 1 "?" 65537)
        	:accessor f0env)
   ( f0jta	:type number
        	:initarg :f0jta 
        	:initform 0.06
        	:accessor f0jta)
   ( freq	:type number
        	:initarg :freq 
        	:initform 2.0
        	:accessor freq)
   ( fqmax	:type number
        	:initarg :fqmax 
        	:initform 1.0
        	:accessor fqmax)
   ( fqenv	:type cs-table
        	:initarg :fqenv 
                       ; x-points y-points decimals number size
  		:initform (make-cs-table 'Gen07  '(0 65536)
                                         '(0 1) 1 "?" 65537)
        	:accessor fqenv)
   ( fqjta	:type number
        	:initarg :fqjta 
        	:initform 0.1
        	:accessor fqjta)
   ( bw		:type number
        	:initarg :bw 
        	:initform 0.0
        	:accessor bw)
   ( bwmax	:type number
        	:initarg :bwmax 
        	:initform 0.0
        	:accessor bwmax)
   ( bwenv	:type cs-table
        	:initarg :bwenv
                       ; x-points y-points decimals number size
  		:initform (make-cs-table 'Gen07  '(0 65536)
                                         '(0 1) 1 "?" 65537)
        	:accessor bwenv)
   ( aenv	:type cs-table
		:initarg :aenv 
                       ; x-points y-points decimals number size
   		:initform (make-cs-table 'Gen07  '(0 65536)
                                         '(1 1) 1 "?" 65537)
                :accessor aenv)
   ( win	:type number
        	:initarg :win 
        	:initform 0.003
        	:accessor win)
   ( winmax	:type number
        	:initarg :winmax
        	:initform 0.1
        	:accessor winmax)
   ( winenv	:type cs-table
        	:initarg :winenv
                       ; x-points y-points decimals number size
  		:initform (make-cs-table 'Gen07  '(0 65536)
                                         '(0 1) 1 "?" 65537)
        	:accessor winenv)
   ( wdur	:type number
        	:initarg :wdur 
        	:initform 0.1
        	:accessor wdur)
   ( wdurmax	:type number
        	:initarg :wdurmax
        	:initform 1.0
        	:accessor wdurmax)
   ( wdurenv	:type cs-table
        	:initarg :wdurenv
                       ; x-points y-points decimals number size
  		:initform (make-cs-table 'Gen07  '(0 65536)
                                         '(0 1) 1 "?" 65537)
        	:accessor wdurenv)
    ( wout	:type number
        	:initarg :wout 
        	:initform 0.02
        	:accessor wout)
   ( woutmax	:type number
        	:initarg :woutmax 
        	:initform 0.2
        	:accessor woutmax)
   ( woutenv	:type cs-table
        	:initarg :woutenv
                       ; x-points y-points decimals number size
  		:initform (make-cs-table 'Gen07  '(0 65536)
                                         '(0 1) 1 "?" 65537)
        	:accessor woutenv)
   ( oct	:type number
        	:initarg :oct 
        	:initform 0.0
        	:accessor oct)
   ( octmax	:type number
        	:initarg :octmax 
        	:initform 1.0
        	:accessor octmax)
   ( octenv	:type cs-table
        	:initarg :octenv
                       ; x-points y-points decimals number size
  		:initform (make-cs-table 'Gen07  '(0 65536)
                                         '(0 1) 1 "?" 65537)
        	:accessor octenv)
      ( afil	:type cs-table
        	:initarg :afil 
        	:initform 32
        	:accessor afil)
( mode	:type number
        	:initarg :mode 
        	:initform 1
        	:accessor mode)
)

  (:documentation
   "
;=============================================================================
;			FOF-A4.ORC
; FOF-BASED GRANULAR SYNTHESIS / MONO
; SAMPLED GRAIN RATHER THAN A SINE TONE IN THE FOF
; SEVERAL DYNAMIC CONTROLS OVER THE P-FIELDS + JITTER
; AMPLITUDE ENVELOPE WITH POSCIL
;=============================================================================

; Timbre:    Granular synthesis with a FOF (based on Byrne Villez's work)
; Synthesis: FOF (Forme d'Onde Formatique)
;            POSCIL envelopes
; Coded:     ms 2/09

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
;	p5	= min fundamental freq [Hz]
;	p6	= max fundamental freq [Hz]
;	p7	= fundamental freq envelope [GEN]
;	p8	= jitter amp of fundamental freq [0-1]
;	p9	= min formant freq [Hz]
;	p10	= max formant freq [Hz]
;	p11	= formant freq envelope [GEN]
;	p12	= jitter amp of formant freq [0-1]
;	p13	= min bandwidth [Hz]
;	p14	= max bandwidth [Hz]
;	p15	= bandwidth envelope [GEN]
;	p16	= amp envelope [GEN]
;	p17	= min tex or krise [sec]
;	p18	= max tex or krise [sec]
;	p19	= tex or krise envelope [GEN]
;	p20	= min total dur of the burst [sec]
;	p21	= max total duration of the burst [sec]
;	p22	= envelope of the total duration of the burst [GEN]
;	p23	= min decay of the FOF [sec]
;	p24	= max decay of the FOF [sec]
;	p25	= envelope of decay of the FOF [GEN]
;	p26	= min octaviation [>=0.0]
;	p27	= max octaviation [>=0.0]
;	p28	= octaviation envelope [GEN]
;	p29	= formant frequency mode [0=no gliss]
;	p30	= sound file [name, sound, string, pathname or GEN01] (32)
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f19	sigmoid rise/decay shape
;	f32	short sample (grain)
;_____________________________________________________________________________
"
   )
  (:icon 1004)
  )
