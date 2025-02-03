(in-package :om)

;;;===================================
;;; SPAT CLASSES
;;;===================================

(defclass! 6Ch-1
  (cs-spat-evt) 			
  (
   (source-code :initform
                 (load-buffer-textfile
                  (get-orc-source (get-orc "6Ch-1"))
                  'textfile "append")
                 :allocation :class :type textfile :accessor source-code)
   (numchan :initform (or (get-orc-channels (get-orc "6Ch-1")) 2) :allocation :class  :accessor numchan)
   (globals-list :initform (get-orc-globals (get-orc "6Ch-1")) :allocation :class :type list :accessor globals-list)
   (macros-list :initform nil :allocation :class :type list :accessor macros-list)
   (orc-header :initform nil :allocation :class :type list :accessor orc-header)
   (InstID :initform 1  :allocation :class  :accessor InstID)
   
   ( afil	:type t
        	:initarg :afil 
        	:initform nil
        	:accessor afil)
   
   ( bal	:type number
        	:initarg :bal 
        	:initform 0.
        	:accessor bal)
   )
  (:documentation "
;=============================================================================
;		6CH-1.ORC : panning with 6 channels
;-----------------------------------------------------------------------------

;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4 	= input (file)
;	p5	= panning, equal power [Az, degrees]
;		  [0=1, 60/-300=2, 120/-240=3, 180/-180=4, 240/-120=5, 300/-60=6]

;	AFIL	= filename
;	BAL	= panning [Az, degrees]

")
  (:icon 3001)
  )




