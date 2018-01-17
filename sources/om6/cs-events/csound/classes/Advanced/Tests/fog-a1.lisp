;******************************************************************
;		     CLASS FOG-A1
;******************************************************************

(in-package om)

; LISP-DEFINED CLASSES SHOULD RESIDE IN THE LIBRARY'S PACKAGE AND 
;   NOT IN THE USER PACKAGE, WHICH CONTAINS ALL THE CLASSES
;   GRAPHICALLY DEFINED


(defclass! fog-a1
  (cs-evt) 			; INHERIT FROM CS-EVT
  (

; GLOBAL SLOTS (LIGHT BLUE, ON THE LEFT OF THE CLASS):
;    THE METHOD BELOW TRANSFORMS THEM INTO GLOBAL SLOTS ("SHOW" UNCHEKED)
;    ATTENTION: A GLOBAL SLOT SHOULD NOT HAVE AN INITARG

   ( source-code :initform
                 (load-buffer-textfile
                  (get-orc-source (get-orc "fog-a1"))
                  'textfile "append")
                 :allocation :class
                 :type textfile
                 :accessor source-code)
   ( numchan :initform (or (get-orc-channels (get-orc "fog-a1")) 1)
             :allocation :class  :accessor numchan)
   ( globals-list :initform 
                  (get-orc-globals
                   (get-orc "fog-a1"))
                  :allocation :class
                  :type list
                  :accessor globals-list)
   ( macros-lis :initform 
                  (get-orc-macros
                   (get-orc "fog-a1"))
                  :allocation :class
                  :type list
                  :accessor macros-list)
   (orc-header :initform (list
                          "; sigmoid rise/decay"
                          "f19 0  65536  19 .5 .5 270 .5"
                          "; short grain"
                          (format ()
                                  "f31 0  524288  1 ~s 0 0 0"
                                  (om-path2cmdpath
                                   (infile "basmba_96.aif" :subdirs '("Snd"))))
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
        	:initform 10.0
        	:accessor f0)
   ( f0max	:type number
        	:initarg :f0max
        	:initform 20.0
        	:accessor f0max)
   ( f0env	:type cs-table
        	:initarg :f0env
                       ; x-points y-points decimals number size
  		:initform (make-cs-table 'Gen07  '(0 4096)
                                         '(0 1) 1 "?" 4097)
        	:accessor f0env)
   ( f0jta	:type number
        	:initarg :f0jta 
        	:initform 0.5
        	:accessor f0jta)
   ( freq	:type number
        	:initarg :freq 
        	:initform 1.0
        	:accessor freq)
   ( fqmax	:type number
        	:initarg :fqmax 
        	:initform 2.0
        	:accessor fqmax)
   ( fqenv	:type cs-table
        	:initarg :fqenv 
                       ; x-points y-points decimals number size
  		:initform (make-cs-table 'Gen07  '(0 4096)
                                         '(0 1) 1 "?" 4097)
        	:accessor fqenv)
   ( fqjta	:type number
        	:initarg :fqjt 
        	:initform 0.5
        	:accessor fqjta)
   ( afil	:type cs-table
        	:initarg :afil 
        	:initform 31
        	:accessor afil)
   ( spd	:type number
        	:initarg :spd
        	:initform 1.0
        	:accessor spd)
   ( spdmax	:type number
        	:initarg :spdmax
        	:initform 2.0
        	:accessor spdmax)
   ( spdenv	:type cs-table
        	:initarg :spdenv 
                       ; x-points y-points decimals number size
  		:initform (make-cs-table 'Gen07  '(0 4096)
                                         '(0 1) 1 "?" 4097)
        	:accessor spdenv)
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
  		:initform (make-cs-table 'Gen07  '(0 4096)
                                         '(0 1) 1 "?" 4097)
        	:accessor bwenv)
   ( aenv	:type cs-table
		:initarg :aenv 
                       ; x-points y-points decimals number size
   		:initform (make-cs-table 'Gen07  '(0 4096)
                                         '(1 1) 1 "?" 4097)
                :accessor aenv)
   ( win	:type number
        	:initarg :win 
        	:initform 0.05
        	:accessor win)
   ( winmax	:type number
        	:initarg :winmax
        	:initform 0.01
        	:accessor winmax)
   ( winenv	:type cs-table
        	:initarg :winenv
                       ; x-points y-points decimals number size
  		:initform (make-cs-table 'Gen07  '(0 4096)
                                         '(0 1) 1 "?" 4097)
        	:accessor winenv)
   ( wdur	:type number
        	:initarg :wdur 
        	:initform 0.2
        	:accessor wdur)
   ( wdurmax	:type number
        	:initarg :wdurmax
        	:initform 0.05
        	:accessor wdurmax)
   ( wdurenv	:type cs-table
        	:initarg :wdurenv
                       ; x-points y-points decimals number size
  		:initform (make-cs-table 'Gen07  '(0 4096)
                                         '(0 1) 1 "?" 4097)
        	:accessor wdurenv)
    ( wout	:type number
        	:initarg :wout 
        	:initform 0.1
        	:accessor wout)
   ( woutmax	:type number
        	:initarg :woutmax 
        	:initform 0.05
        	:accessor woutmax)
   ( woutenv	:type cs-table
        	:initarg :woutenv
                       ; x-points y-points decimals number size
  		:initform (make-cs-table 'Gen07  '(0 4096)
                                         '(0 1) 1 "?" 4097)
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
  		:initform (make-cs-table 'Gen07  '(0 4096)
                                         '(0 1) 1 "?" 4097)
        	:accessor octenv)
   ( mode	:type number
        	:initarg :mode 
        	:initform 1
        	:accessor mode)
)

  (:documentation
   "
;=============================================================================
;			FOG-A1.ORC
;	GRANULAR SYNTHESIS (NEW INSTRUMENT) / MONO
;	AMPLITUDE ENVELOPE WITH POSCIL, COMPLETE CONTROL OF THE PARAMETRES
;=============================================================================

; Timbre:    Granular synthesis with fog module, voice-like tones
; Synthesis: FOG (Forme d'Onde Granulaire)
;            POSCIL envelopes
; Coded:     ms 2/09

; NB:
; this instrument works AT BEST with samples whose length is a power of 2
;	those which are shorter than the immediately superior power of 2 accepted
;	by the GEN01, will produce silence when looking up the unsued portion of the table

; NB1: NEW STRUCTURE FOR THE AMPLITUDES FROM AUGUST 2008!
;	Positive value > 0.0  : linear amplitude (>0.0-1000.0)
;	0.0 or negative value : amplitude in dB (0 = maximum value)

; The apparently arbitrary amplitude range (0-1000, rather than 0-1)
;	avoids printing small values with exponential notation

; Replaced oscili with poscil (precise oscillator), ms 8/08
; Default SR = 96000, recommended precision: 24 bits
;-----------------------------------------------------------------------------
;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4	= max amp [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= min density of the grains [Hz]
;	p6	= max density of the grains [Hz]
;	p7	= envelope for the density of the grains [GEN]
;	p8	= jitter amp of density [0-1]
;	p9	= min transposition factor [1=original]
;	p10	= max transposition factor [1=original]
;	p11	= envelope for the transposition factor [GEN]
;	p12	= jitter amp of transposition factor [0-1]
;	p13	= audio file [GEN01]
;	p14	= min speed of the starting pointer in the file [1=same as original]
;	p15	= max speed of the starting pointer in the file [1=same as original]
;	p16	= envelope for the speed of the starting pointer in the file [GEN]
;	p17	= amplitude envelope [GEN]
;	p18	= min bandwidth (-> exponential decay) [Hz]
;	p19	= max bandwidth (-> exponential decay) [Hz]
;	p20	= envelope for the bandwidth -> exponential decay [GEN]
;	p21	= min rise time of the grain envelope [sec]
;	p22	= max rise time of the grain envelope [sec]
;	p23	= envelope for the rise time of the grain envelope [GEN]
;	p24	= min overall duration of the grain [sec]
;	p25	= max overall duration of the grain [sec]
;	p26	= envelope for the overall duration of the grain [GEN]
;	p27	= min decay time of the grain envelope [sec]
;	p28	= max decay time of the grain envelope [sec]
;	p29	= envelope for the decay time of the grain envelope [GEN]
;	p30 = min octaviation index [>= 0.0]
;	p31 = max octaviation index [>= 0.0]
;	p32 = envelope for the octaviation index [GEN]
;	p33 = transposition mode [if=0.0, no glissando within each grain]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f31	audio file
;	f19	sigmoid rise/decay shape
;_____________________________________________________________________________

; CLASS: FOG-1

;  GLOBAL KEYWORDS (default values within parentheses):
;	NUMROWS		: amount of rows (components) in the event (1)
;	ACTION-TIME	: start time of the whole event [sec] (0.0)
;	USER-FUN	: user-defined parsing function (nil)

;  LOCAL KEYWORDS:
;	E-DELS	: entry delays [sec] (0.0)
;	DURS	: duration [sec] (1.0)
;	AMP 	: amplitude [lin, >0.0-1000.0 or dB <- 0.0] (-6.0)
;	F0  	: min density of the grains [Hz] (10.0)
;	F0MAX  	: max density of the grains [Hz] (20.0)
;	F0ENV  	: envelope for the density of the grains [GEN] (asc line)
;	F0JTA  	: jitter's amp of the grains' density [0-1] (0.06)
;	FREQ 	: min transposition factor [1=original) (1.0)
;	FQMAX 	: max transposition factor [1=original) (2.0)
;	FQENV 	: envelope for the transposition factor [GEN] (asc line)
;	FQJTA 	: jitter's amp for the transposition factor [0-1] (0.1)
;	AFIL	: audio file [GEN01] (basmba_96.aiff)
;	SPD 	: min speed of the starting point in the sound [1=same as original] (1.0)
;	SPDMAX 	: max speed of the starting point in the sound [1=same as original] (2.0)
;	SPDENV 	: envelope for the speed of the starting point in the sound [GEN] (asc line)
;	BW  	: min bandwidth (-> exponential decay) [Hz] (0)
;	BWMAX  	: max bandwidth (-> exponential decay) [Hz] (0)
;	BWENV  	: bandwidth's envelope [GEN]  (asc line)
;	AENV	: amplitude envelope [GEN] (all 1)
;	WIN 	: min rise time of the grain envelope [sec] (0.05)
;	WINMAX 	: max rise time of the grain envelope [sec] (0.01)
;	WINENV 	: envelope for the rise time of the grain [GEN] (asc line)
;	WDUR	: min duration of the grain [sec] (0.2)
;	WDURMAX	: max duration of the grain [sec] (0.05)
;	WDURENV	: envelope for the duration of the grain [GEN] (asc line)
;	WOUT	: min decay time of the grain envelope [sec] (0.1)
;	WOUTMAX	: max decay time of the grain envelope [sec] (0.05)
;	WOUTENV	: envelope for the decay time of the grain envelope [GEN] (asc line)
;	OCT 	: min octaviation factor [=>0] (0.0)
;	OCTMAX 	: max octaviation factor [=>0] (0.0)
;	OCTENV 	: envelope for the octaviation factor [GEN] (asc line)
;	MODE	: transposition mode [if=0, no gliss. within each grain] (0)
;*****************************************************************************
"
   )
  (:icon 618)
  )
