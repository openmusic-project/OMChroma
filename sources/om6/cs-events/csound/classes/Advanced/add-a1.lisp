;******************************************************************
;		     CLASS ADD-A1
;******************************************************************

(in-package :om)

; LISP-DEFINED CLASSES SHOULD RESIDE IN THE LIBRARY'S PACKAGE AND 
;   NOT IN THE USER PACKAGE, WHICH CONTAINS ALL THE CLASSES
;   GRAPHICALLY DEFINED




(defclass! add-a1
  (cs-evt) 			; INHERIT FROM CS-EVT
  (

; GLOBAL SLOTS (LIGHT BLUE, ON THE LEFT OF THE CLASS):
;    THE METHOD BELOW TRANSFORMS THEM INTO GLOBAL SLOTS ("SHOW" UNCHEKED)
;    ATTENTION: A GLOBAL SLOT SHOULD NOT HAVE AN INITARG

   ( source-code :initform
                 (load-buffer-textfile
                  (get-orc-source (get-orc "add-a1"))
                  'textfile "append")
                 :allocation :class
                 :type textfile
                 :accessor source-code)
   ( numchan :initform (or (get-orc-channels (get-orc "add-a1")) 1)
             :allocation :class  :accessor numchan)
   (cs-inits :initform (get-cs-inits (get-orc "add-a1")) 
          :allocation :class :type list :accessor cs-inits)
   (orc-header :initform (list

                          "; GEN functions **********************************************************"
                          "; audio wave"
                          "f1  0   134217729   9  1  1  0"
                          
                          ) 
               :allocation :class :type list :accessor orc-header)
   (InstID :initform 1  :allocation :class  :accessor InstID)

; LOCAL SLOTS (RED, CORRESPONDING TO THE P-FIELDS)
;    ATTENTION: A GLOBAL SLOT SHOULD HAVE AN INITARG
  ( amp		:type number
		:initarg :amp 
  		:initform 500.0
		:accessor amp)
   ( freq	:type number
        	:initarg :freq 
        	:initform 440.0
        	:accessor freq)
   ( aenv	:type cs-table
		:initarg :aenv 
                                               ; x-points y-points decimals
  		:initform (make-cs-table  'Gen07  '(0 32768 65537) '(0 100 0) 1 "?" 65537)
		:accessor aenv)
   ( fdev	:type number
        	:initarg :fdev 
        	:initform 1.0
        	:accessor fdev)
   ( fenv	:type cs-table
		:initarg :fenv 
                                               ; x-points y-points decimals
  		:initform (make-cs-table  'Gen07  '(0 32768 65537) '(0 100 0) 1 "?" 65537)
		:accessor fenv)
   )

  (:documentation
   "
;=============================================================================
;			ADD-A1.ORC
; ADDITIVE SYNTHESIS WITH FREQUENCY AND AMPLITUDE ENVELOPES / MONO
; ENVELOPES WITH OSCILI
;=============================================================================

; Timbre:    nothing speciaL
; Synthesis: additive same units
; Source:    new instrument
; Coded:     ms 0503

;-----------------------------------------------------------------------------
; NB: the apparently arbitrary amplitude range (0-1000, rather than 0-1)
;         avoids Lisp printing small values with exponential notation
;         when using linear amplitudes
;-----------------------------------------------------------------------------

;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4	= maximum amplitude [linear, 0.0 -> 1000.0]
;	p5	= reference frequency [Hz]
;	p6	= amplitude envelope [GEN number]
;	p7	= maximum frequency deviation [Hz]
;	p8	= frequency envelope [GEN number]

;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;	f1	audio wave
;_____________________________________________________________________________
"
   )
  (:icon 1001)
  )

