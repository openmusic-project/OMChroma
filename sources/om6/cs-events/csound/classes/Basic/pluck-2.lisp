;******************************************************************
;		     CLASS PLUCK-2
;******************************************************************

(in-package :om)

; LISP-DEFINED CLASSES SHOULD RESIDE IN THE LIBRARY'S PACKAGE AND 
;   NOT IN THE USER PACKAGE, WHICH CONTAINS ALL THE CLASSES
;   GRAPHICALLY DEFINED


(defclass! pluck-2
  (cs-evt) 			; INHERIT FROM CS-EVT
  (
   
   ; GLOBAL SLOTS (LIGHT BLUE, ON THE LEFT OF THE CLASS):
   ;    THE METHOD BELOW TRANSFORMS THEM INTO GLOBAL SLOTS ("SHOW" UNCHEKED)
   ;    ATTENTION: A GLOBAL SLOT SHOULD NOT HAVE AN INITARG
   
   ( source-code :initform
                 (load-buffer-textfile
                  (get-orc-source (get-orc "pluck-2"))
                  'textfile "append")
                 :allocation :class
                 :type textfile
                 :accessor source-code)
   ( numchan :initform (or (get-orc-channels (get-orc "pluck-2")) 1)
             :allocation :class  :accessor numchan)

   (cs-inits :initform (get-cs-inits (get-orc "pluck-2")) 
             :allocation :class :type list :accessor cs-inits)

   (orc-header :initform (list
   
   "; GEN functions **********************************************************"
   ""
   "; none"
   )
               :allocation :class :type list :accessor orc-header)
   (InstID :initform 1  :allocation :class  :accessor InstID)
   ; LOCAL SLOTS (RED, CORRESPONDING TO THE P-FIELDS)
   ;    ATTENTION: A GLOBAL SLOT SHOULD HAVE AN INITARG
   ( amp	:type number
		:initarg :amp 
  		:initform -6.0
		:accessor amp)
   ( freq	:type number
        	:initarg :freq 
        	:initform 440.0
        	:accessor freq)
   ( buf	:type number
        	:initarg :buf 
        	:initform 220.0
        	:accessor buf)
   ( dec	:type number
        	:initarg :dec 
        	:initform 0.8
        	:accessor dec)
   ( ranfun	:type number
        	:initarg :ranfun 
        	:initform 0
        	:accessor ranfun)
   ( meth	:type number
        	:initarg :meth 
        	:initform 4
        	:accessor meth)
   ( par1	:type number
        	:initarg :par1 
        	:initform 0.5
        	:accessor par1
                :documentation "parameter required by some values of METH")
   ( par2	:type number
        	:initarg :par2 
        	:initform 10.0
        	:accessor par2
                :documentation "parameter required by some values of METH")
   )
  
  
  (:documentation
   "
;=============================================================================
;		PLUCK-2.ORC
; PLUCKED STRING USING THE KARPLUS-STRONG MODEL
;    (FROM ACCCI, 15_01_3.ORC) / MONO
; AMPLITUDE ENVELOPE WITH LINSEG, CONTROL OVER INTIALIZATION AND DECAY MODES
;=============================================================================

; Timbre:       Plucked string
; Synthesis:    Karplus-Strong algorithm
;               PLUCK
;               LINSEG envelope, cembalo sounds
; Coded:     	jpg 8/93, modified ms 9/02, 8/08

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
;	p4	= maximum amp [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= freq [Hz]
;	p6	= intended freq (buffer) [Hz]
;	p7	= % of the total duration used for the last decay [0-1]
;	p8	= table number of a stored function [GEN or 0=random]
;	p9	= method of natural decay
;		  1: simple averaging
;		  2: stretched averaging. ipar1=stretch factor of smoothing time
;		  3: simple drum. ipar1=roughness factor [0-1]
;				0=plucked string
;				1=reverse the polarity of each sample (oct down, odd harms)
;				0.5=optimum drum
;		  4: stretched drum. ipar1=roughness, ipar2=stretch factor
;		  5: weighted averaging. ipar1=weight for the current sample
;				ipar2: weight for the previous adjacent one.
;				NB: ipar1+ipar2<=1.0
;		  6: 1st order recursive filter with coef 0.5
;	p10	= ipar1 (used by p9, see above)
;	p11	= ipar2 (used by p9, see above)
;NB: plucked strings (1,2,5,6) are best realised by starting with random noise
;      (p8=0). Drum sounds (3,4) work best with a flat source (wide pulse).
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
; none
;_____________________________________________________________________________
"
   )
  (:icon 1006)
  )

