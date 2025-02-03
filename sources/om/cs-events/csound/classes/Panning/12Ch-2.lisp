(in-package :om)

;;;===================================
;;; SPAT CLASSES
;;;===================================

(defclass! 12Ch-2
  (cs-spat-evt) 			
  (
   (source-code :initform
                 (load-buffer-textfile
                  (get-orc-source (get-orc "12Ch-2"))
                  'textfile "append")
                 :allocation :class :type textfile :accessor source-code)
   (numchan :initform (or (get-orc-channels (get-orc "12Ch-2")) 2) :allocation :class  :accessor numchan)
   (globals-list :initform (get-orc-globals (get-orc "12Ch-2")) :allocation :class :type list :accessor globals-list)
   (macros-list :initform nil :allocation :class :type list :accessor macros-list)
   (orc-header :initform nil :allocation :class :type list :accessor orc-header)
   (InstID :initform 1  :allocation :class  :accessor InstID)
   
   ( afil	:type t
        	:initarg :afil 
        	:initform nil
        	:accessor afil)
   
   ( Channel1	:type number
        	:initarg :Channel1 
        	:initform 1.0
        	:accessor Channel1)
   ( Channel2	:type number
        	:initarg :Channel2 
        	:initform 0.0
        	:accessor Channel2)
   ( Channel3	:type number
        	:initarg :Channel3 
        	:initform 0.0
        	:accessor Channel3)
   ( Channel4	:type number
        	:initarg :Channel4 
        	:initform 0.0
        	:accessor Channel4)
   ( Channel5	:type number
        	:initarg :Channel5 
        	:initform 0.0
        	:accessor Channel5)
   ( Channel6	:type number
        	:initarg :Channel6 
        	:initform 0.0
        	:accessor Channel6)
   ( Channel7	:type number
        	:initarg :Channel7 
        	:initform 0.0
        	:accessor Channel7)
   ( Channel8	:type number
        	:initarg :Channel8 
        	:initform 0.0
        	:accessor Channel8)
   ( Channel9	:type number
        	:initarg :Channel9 
        	:initform 0.0
        	:accessor Channel9)
   ( Channel10	:type number
        	:initarg :Channel10 
        	:initform 0.0
        	:accessor Channel10)
   ( Channel11	:type number
        	:initarg :Channel11 
        	:initform 0.0
        	:accessor Channel11)
   ( Channel12	:type number
        	:initarg :Channel12
        	:initform 0.0
        	:accessor Channel12)
   )
  (:documentation "
;=============================================================================
;		12CH-2.ORC
; INDEPENDENT PANNING, 1 SLOT/CHANNEL 
;-----------------------------------------------------------------------------

;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4 	= input (file)
;	p5	= Ch 1 level [0-1]
;	p6	= Ch 2 level [0-1]
;	p7	= Ch 3 level [0-1]
;	p8	= Ch 4 level [0-1]
;	p9	= Ch 5 level [0-1]
;	p10	= Ch 6 level [0-1]
;	p11	= Ch 7 level [0-1]
;	p12	= Ch 8 level [0-1]
;	p13	= Ch 9 level [0-1]
;	p14	= Ch 10 level [0-1]
;	p15	= Ch 11 level [0-1]
;	p16	= Ch 12 level [0-1]

;	AFIL		= filename
;	CHANNEL1	= amplitude [0-1, 1]
;	CHANNEL2	= amplitude [0-1, 0]
;	CHANNEL3	= amplitude [0-1, 0]
;	CHANNEL4	= amplitude [0-1, 0]
;	CHANNEL5	= amplitude [0-1, 0]
;	CHANNEL6	= amplitude [0-1, 0]
;	CHANNEL7	= amplitude [0-1, 0]
;	CHANNEL8	= amplitude [0-1, 0]
;	CHANNEL9	= amplitude [0-1, 0]
;	CHANNEL10	= amplitude [0-1, 0]
;	CHANNEL11	= amplitude [0-1, 0]
;	CHANNEL12	= amplitude [0-1, 0]

")
  (:icon 3001)
  )




