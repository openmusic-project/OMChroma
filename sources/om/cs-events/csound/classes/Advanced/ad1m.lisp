;******************************************************************
;		     CLASS AD1M-RT
; 	      Root class for the class AD1M
;******************************************************************

(in-package :om)

; LISP-DEFINED CLASSES SHOULD RESIDE IN THE LIBRARY'S PACKAGE AND 
;   NOT IN THE USER PACKAGE, WHICH CONTAINS ALL THE CLASSES
;   GRAPHICALLY DEFINED

(defclass! ad1m-rt
  (cs-evt)
  (
   ( source-code :initform
                 (load-buffer-textfile
                  (get-orc-source (get-orc "ad1m"))
                  'textfile "append")
                 :allocation :class
                 :type textfile
                 :accessor source-code)
   ( numchan :initform (or (get-orc-channels (get-orc "ad1m")) 1)
             :allocation :class  :accessor numchan)
   ( globals-list :initform 
                  (get-orc-globals
                   (get-orc "ad1m"))
                  :allocation :class
                  :type list
                  :accessor globals-list)
   ( macros-list :initform 
                 (get-orc-macros (get-orc "ad1m"))
                 :allocation :class
                 :type list
                 :accessor macros-list)
   (orc-header :initform (list
                          "; DEFAULT GENs **********************************************************"
                          "; audio wave, sine"
                          "f1  0   524289   10 1"
                          "; vibrato wave, sine"
                          "f2  0   65537  10 1"
                          "; tremolo wave, sine"
                          "f3  0   65537  10 1"
                          ) 
               :allocation :class :type list :accessor orc-header)
   (InstID :initform 1  :allocation :class  :accessor InstID)

   ( amp	:type number
		:initarg :amp 
  		:initform -6.0
		:accessor amp)
   ( freq	:type number
        	:initarg :freq 
        	:initform 440.0
        	:accessor freq)
   ( atk	:type number
		:initarg :atk 
  		:initform 0.01
		:accessor atk)
   ( dec	:type number
		:initarg :dec 
  		:initform 0.05
		:accessor dec)
   ( aenv	:type cs-table
		:initarg :aenv 
                                                ; x-pts y-pts decimals number size
  		:initform (make-cs-table  'Gen07  '(0 32768 65536) '(0 1 0) 1 "?" 65537)
		:accessor aenv)
   ( jta	:type number
		:initarg :jta 
  		:initform 0.1
		:accessor jta)
   ( tra	:type number
		:initarg :tra 
  		:initform 0.1
		:accessor tra)
   ( trf	:type number
		:initarg :trf 
  		:initform 5.0
		:accessor trf)
  ( jtv		:type number
		:initarg :jtv 
  		:initform 0.5
		:accessor jtv)
   ( vfq	:type number
		:initarg :vfq 
  		:initform 6.0
		:accessor vfq)
   ( fdev	:type number
		:initarg :fdev 
  		:initform 1.0
		:accessor fdev)
   ( fenv	:type cs-table
		:initarg :fenv 
                                                 ; x-pts y-pts decimals number size
  		:initform (make-cs-table  'Gen07  '(0 32768 65536) '(0 1 0) 1 "?" 65537)
		:accessor fenv)
   ( plow	:type number
		:initarg :plow 
  		:initform -1.0
		:accessor plow)
   ( pup 	:type number
		:initarg :pup 
  		:initform 0.0
		:accessor pup)
   ( penv	:type cs-table
		:initarg :penv 
                                               ; x-points y-points decimals
  		:initform (make-cs-table  'Gen07  '(0 65536) '(0 1) 1 "?" 65537)
		:accessor penv)
   ( pdur	:type number
		:initarg :pdur 
  		:initform 0.1
		:accessor pdur)
   ( afil 	:type number
		:initarg :afil 
  		:initform 1
		:accessor afil)
   )
  (:documentation
   "
;=============================================================================
;		AD1m.ORC
; ADDITIVE SYNTHESIS INSTRUMENT N. 12 / COMPLETE / MONO
;=============================================================================
; NB: NEW STRUCTURE FOR THE AMPLITUDES FROM AUGUST 2008!
;    Positive value > 0.0  : linear amplitude (>0.0-1000.0)
;    0.0 or negative value : amplitude in dB (0 = maximum value)

; The apparently arbitrary amplitude range (0-1000, rather than 0-1)
;         avoids printing small values with exponential notation

; Replaced oscili with poscil (precise oscillator), ms 8/08
; Default SR = 96000, recommended precision: 24 bits
;-----------------------------------------------------------------------------
; SIMPLIFIED PERCEPTUAL COMPENSATION: SINCE THE COMPUTATION IS FLOATING POINT
;    NO NEED TO REDUCE THE AMPLITUDES IF MAXIMUM IS > 1 AFTER THE COMPENSATION
; ALSO NO MODIFICATION OF THE GLOBAL AMPLITUDE TO A RELATIVE AMPLITUDE
;-----------------------------------------------------------------------------
; p1	= instrument number [12]
; p2	= action time [sec]
; p3	= duration [sec]
; p4	= max amp [linear, >0.0-1000.0 or dB, <= 0.0]
; p5	= frequency [Hz]
; p6	= attack duration [sec]
; p7	= decay duration [sec], with internal readjustment if needed
; p8	= amplitude envelope [GEN]
; p9	= jitter amplitude, with internal perceptual compensation [0-1]
; p10	= tremolo amplitude [0-1]
; p11	= tremolo frequency [Hz]
; p12	= balance freq.jitter/vibrato [1 = jitter / 0 = vibrato]
; p13	= vibrato frequency [Hz]
; p14	= frequency deviation [semitones]
; p15	= frequency envelope [GEN]
; p16	= lower interval for portamento [semitones]
; p17	= upper interval for portamento [semitones]
; p18	= portamento envelope [GEN]
; p19	= portamento duration [sec]
; p20	= audio fun [GEN]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;	f1	audio wave (sine)
;	f2	vibrato function (sine)
;	f3	tremolo function (sine)
;_____________________________________________________________________________

; CONTROL KEYWORDS FOR CHROMA2001 (CTL1)

;  GLOBAL KEYWORDS:
; ACTION-TIME	: start time of the whole event (0.0)
; DURTOT		: total duration of the event (longer components are clipped) (1.0)
; AMPTOT		: amplitude scaler (linear, >0.0-1000.0 or dB, <= 0.0)
; NUMROWS		: amount of rows (components) in the event (1)
;=============================================================================

;  LOCAL KEYWORDS:
;  GENERAL
; E-DELS: entry delays [sec] (0.0)
; DURS	: duration [0-1, scaler of DURTOT] (1.0)
; AMP 	: max amp [0-1000 or dB, scaler of AMPTOT] (-6.0)
; FREQ	: frequency [Hz] (440.0)

;  AMPLITUDE ENVELOPE
; ATK 	: attack time [sec] (0.01)
; DEC 	: decay time [sec] (0.05)
; AENV	: amplitude envelope [GEN] (triangle)

;  AMPLITUDE MODULATION
; JTA 	: jitter amp [0-1, % of max amp+compensation] (0.1)
; TRA 	: tremolo amp [0-1, % of max amp] (0.1)
; TRF 	: tremolo freq [Hz] (5.0)

;  FREQUENCY MODULATION
; JTV 	: vibrato/jitter panpot [0 = all vibrato, 1 = all jitter] (0.5)
; VFQ 	: vibrato freq [Hz] (6.0)
; FDEV	: freq deviation [semitones] (1.0)
; FENV	: frequency envelope [GEN] (triangle)

;  PORTAMENTO
; PLOW	: lowest (minimum) portamento [semitones] (-1.0)
; PUP 	: upper (maximum) portamento amplitude [semitones] (0.0)
; PENV	: portamento envelope [GEN] (asc line)
; PDUR	: portamento duration [sec] (0.1)

; AFIL	: audio function [GEN] (1=sine)
;=============================================================================
;  SUB-COMPONENTS
; NPART	: number of sub-components (3)
; STON	: aleatoric frequency distribution of each sub-component (0.06)
;     	POSITIVE = distribution is linear
;     	NEGATIVE = distribution is logarithmic
; ED2 	: entry delay of each sub-compoment [sec, cumulative] (0.01)
; DUR2	: duration of each sub-component [0-1, nil = use main dur] (nil)
; AMP2	: amplitude of each sub-component [0-1, nil = use main amp] (nil)
;=============================================================================
; COMPULSORY FUNCTIONS :
; f1	= audio sine wave
; f2	= vibrato (sine wave)
; f3	= tremolo (sine wave)
;************************************************************************
   "
   )
  (:icon 1001)
  )


;******************************************************************
;			SUB-CLASSES
;******************************************************************
; AD1M
;------------------------------------------------------------------

(defclass! ad1m
  (ad1m-rt)
  (
   ; GLOBAL SLOTS: THE METHOD BELOW TRANSFORMS THEM INTO GLOBAL SLOTS
   ;    ATTENTION: A GLOBAL SLOT SHOULD NOT HAVE AN INITARG
   ( durtot :type number
            :initform 1.0
            :accessor durtot)
   ( amptot :type number
            :initform -6.0
            :accessor amptot)
   ( npart :type number
           :initarg :npart 
           :initform 3
           :accessor npart)
   ( ston :type number
          :initarg :ston
          :initform 0.06
          :accessor ston)
   ( ed2 :type number
         :initarg :ed2
         :initform 0.01
         :accessor ed2)
   ( dur2 :type number
          :initarg :dur2
          :initform -1
          :accessor dur2)
   ( amp2 :type number
          :initarg :amp2
          :initform -1
          :accessor amp2))
  (:documentation
   "
Extension of the instrument ADD1m, class ad1m-rt.

Added global slots:
  durtot [sec] = total duration of the whole event
  amptot [0-1000 or dB] = global amplitude scaler

Added local slots:
  npart [int] = number of sub-components
  ston [0-1]  = maximum frequency deviation for the sub-components
		positive = linear distribution (value in %)
		negative = logarithmic distribution (value in semitones)
  ed2 [sec]   = entry delay of each sub-component (cumulative effect) 
  dur2 [0-1]  = duration of each sub-component (scaler of DURTOT) (-1)
                   (if <0, use the value of the main component)
  amp2 [0-1]  = amplitude of each sub-component (scaler of AMPTOT) (-1)
                   (if <0, use the value of the main component)
"
)
  (:icon 1001)
  )


;------------------------------------------------------------------
;	METHODS FOR THIS CLASS
;------------------------------------------------------------------
; ADD SOME GLOBAL SLOTS (THE FIRST ONES ON THE LEFT OF A CLASS BOX)
;     THIS METHOD MUST BE DEFINED FOR THIS CLASS

(defmethod get-slot-in-out-names ((self ad1m))
; MEANING OF THE FIELDS:
;   SLOT NAMES (THE FIRST 4 ARE THE DEFAULT ONES)
;   DEFAULT VALUES
;   DOCUMENTATION
;   T IF IT IS A MENU
   (values '("self" "numrows" "action-time" "user-fun" "durtot" "amptot")
           '(nil 1 0 nil 1.0 1.0)
           '("Object or object list"
             "How many vertical components for init ?"
             "Local event offset (in seconds)"
             "User-defined function"
             "Total duration for the whole event [sec]"
             "Global amplitude scaler [0-1]")
           '(nil nil nil nil nil nil)))


; SPECIFY WHICH ARE THE GLOBAL SLOTS
(defmethod fixed-slots-list ((x om::ad1m))
  "Specifies the fixed (global) slots for this class"
  '(numrows action-time user-fun durtot amptot))


; ELIMINATE THE LAST 5 SLOTS IN THE INSTRUMENT (NOT NEEDED IN THE SCORE)
;(defmethod nullslots ((x om::ad1m))
;  "Eliminate the last 5 slots from the score"
;  5)

(defmethod add-n-slots ((self om::ad1m)) -5)

;******************************************************************
