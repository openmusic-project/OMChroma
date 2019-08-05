;=====================================================
; CHROMA 
;=====================================================
; part of the OMChroma library
; -> High-level control of sound synthesis in OM
;=====================================================
;
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
; File author: M. Stroppa, S. Lemouton
;=====================================================

;========================================
; by serge 07/08/2001
;========================================

(in-package :om)

;;; Utils to access and create arrays
(defun get-instance-keywords (o)
  "get-keywords in control synthesis object instance (class slots + instance specific controls)"
  (append (mapcar 
           #'(lambda (x) (read-from-string (om::name x )))
           (om::get-all-initargs-of-class (type-of o)))
          (mapcar #'car (om::lcontrols o))
          (list 'lprecision)))

(defun get-global-keywords (o)
  "get global controls in control synthesis object instance,
except numrows and action-time (amptot, durtot, etc)"
  (cddr (om::fixed-slots-list o)))


(defun mk-array (class numcols fixedargs &rest argkeys) 
  (let* ((rep (make-instance class :numcols numcols))
         (initargs (get-all-initargs-of-class class))
         (precision 3) slot?)
    (loop for slot in (cdr (fixed-slots-list rep))
          for i = 0 then (+ i 1) do  
          (setf (slot-value rep slot) (nth i fixedargs)))
    (loop while argkeys do
          (cond
           ((setf slot? (car (member (string (car argkeys)) initargs :test 'string-equal :key 'name)))
            (pop argkeys)
            (setf (slot-value rep (internp (name slot?) (slot-package slot?))) (pop argkeys)))
           ;((string-equal (string (car argkeys)) "precision")
           ; (pop argkeys)
           ; (setf precision (pop argkeys)))
           ((array-special-keyword rep (car argkeys))
            (array-special-keyword-action rep (pop argkeys) (pop argkeys)))
           (t
            (pushr (list (pop argkeys) (pop argkeys)) (Lcontrols rep)))))
    ;(put-precision rep precision)
    (set-data rep)
    rep))


;; used by some functiosn in CTL processing
(defmethod cr::offset ((self om::cr-model)) 0)

;;; will be redefined by some functions
(defun ctl2-result ())


;========================================
; SIMPLE VERSION VERSION (NO RULE)
;========================================

(defmethod make-simple-synthesis-event ((target-class symbol) onset data)
  (when data
    (let ((array (make-instance target-class
                                :numcols (length (get-vps-freqs data))
                                :freq (get-vps-freqs data)
                                :amp (get-vps-amps data))))
      (setf (action-time array) onset)
      array)))
          


;========================================
; OM/PATCH VERSION
;========================================

(defmethod make-ctl-synthesis-event ((cr-ctrl cr-control) (target-class symbol) i model)
  (let* ((t1 (car (nth i (time-struct model))))
         (t2 (cadr (nth i (time-struct model))))
         (dur (- t2 t1))
         (ncols nil)
         (instance (make-instance target-class))
         (fixedargs (make-list (- (length (fixed-slots-list instance)) 1)))
         (data (nth i (elements (data model)))))
    (when data 
      (let ((modeldata (make-instance 'virtual-modeldata :local-time t1 
                                      :local-dur dur
                                      :local-data data
                                      :local-rank i))
            (realslots (mapcar 'name (get-all-initargs-of-class target-class)))
            slotslist
            argslist 
            (i -1))
        (setf (global-model modeldata) model)
        (setf slotslist (multiple-value-list (apply (intern (string (code (ctl cr-ctrl))) :om) (cons modeldata (args cr-ctrl)))))
        
        ;;; test
        (setf (tmpmodeldata (ctl cr-ctrl)) modeldata)
        
        (let ((pos (position "numcols" slotslist :key 'car :test 'string-equal)))
          (when pos 
            (setf ncols (cadr (nth pos slotslist)))
            (setf slotslist (append (first-n slotslist pos) (nthcdr (+ pos 1) slotslist)))))
        
        
        (loop for fsl in (fixed-slots-list instance) do
              (unless (equal 'numcols fsl)
                (incf i)
                (let ((pos (position (string fsl) slotslist :key 'car :test 'string-equal)))
                  (if pos
                    (setf (nth i fixedargs) (cadr (nth pos slotslist))
                          slotslist (append (first-n slotslist pos) (nthcdr (+ pos 1) slotslist)))
                    ;; just for action time : set t1
                    (cond ((equal 'action-time fsl) (setf (nth i fixedargs) t1))
                          ((equal 'dur fsl) (setf (nth i fixedargs) 0))
                          (t nil))
                    ))))
              
        (loop for oneslot in slotslist do
              (when (member (car oneslot) realslots :test 'string-equal)
                (setf argslist (append argslist (list (string2initarg (car oneslot)) (cadr oneslot))))
            ;(setf (slot-value rep (intern (car oneslot) :om)) (cadr oneslot))
                ))
        
        (apply 'mk-array (append 
                          (list target-class 
                                (or ncols (max 1 (length (get-vps-freqs data))))
                                fixedargs)
                          argslist))
        ))))




;========================================
; TEXT VERSION
;========================================

(defun cr-interne-list (list package)
  (loop for elt in list 
        collect
        (cond ((listp elt) (cr-interne-list  elt package))
              ((symbolp elt) (intern (symbol-name elt) package))
              (t elt))))


;;; Expand model version Chroma
(defmethod! expand-model ((self cr-model) (ctl-model list) (target-class symbol))
  :icon 655
  (if (data self)
      
      (chroma-process-model self (cr-interne-list ctl-model :chroma) target-class)

    (om-beep-msg "Warning : CR-MODEL is not initialized!")))


(defmethod chroma-process-model ((self cr-model) (ctl-model list) target-class
                                 &key (outfile "yaka-out") (args-yaka-user nil)
                                 (by-time nil))
     
  (declare (special cr::outstream))
                    
  (let ((outf (cr::get-cr-path :tmp :name outfile :type "ctl2")))
    
    (with-open-file  (cr::outstream outf :direction :output :if-exists :supersede)
      
      (format cr::outstream "(in-package :chroma) (defun cr::ctl2-result() (list ~%")
      
      (let ((n (model-nb-evts self)))
        
        (loop for i from 0 to (1- n)
              do (make-chroma-synthesis-event target-class self ctl-model i cr::outstream args-yaka-user)))
      
      (format cr::outstream "))~%"))
    
    (load outf)
    (cr::ctl2-result)
    ))


(defmethod make-chroma-synthesis-event ((target-class symbol) 
                                        (model cr-model) 
                                        ctl-model i stream 
                                        args-yaka-user)
  
  (declare (special cr::my-model cr::my-rank cr::n-fql cr::my-time cr::my-dur cr::my-nev cr::my-fql cr::my-ptl))
  
  ;;; a whole bunch of side-effects for choroma "CTL" to work with...
  (setf cr::my-model model)
  (setf cr::my-rank i)
  (setf cr::my-time (car (nth cr::my-rank (time-struct cr::my-model))))
  (setf cr::my-dur (- (cadr (nth cr::my-rank (time-struct cr::my-model)))
                      (car (nth cr::my-rank (time-struct cr::my-model)))))
  (setf cr::n-fql (length (time-struct cr::my-model)))      
  (setf cr::my-fql (nth cr::my-rank (elements (data model))))
  (setf cr::my-nev (length (get-vps-freqs cr::my-fql)))
  (setf cr::my-ptl cr::my-fql)

  (when cr::my-fql
      
    (when args-yaka-user (apply #'cr::yaka-user args-yaka-user))
      
    (format stream "(om::mk-array 'om::~a ~a ~%~%" target-class cr::my-nev) 
      
    (if (cr::get-gbl 'cr::ctl2-print) (format t "~a~%" cr::my-time))
      
      ; A FAIRE : GLOBALSLOTS loop sur (first (multiple-value-list (om::get-slot-in-out-names self)))
    (let* ((instance (make-instance target-class))
           (global-args (get-global-keywords instance))
           (array-slots (get-instance-keywords instance)))
        
      (cr::CTL2_global ctl-model 
                       global-args
                       (mapcar #'(lambda (key) (funcall key instance)) global-args)
                       )
      
      (cr::CTL2_keywords_loop ctl-model array-slots)
      (format stream ") ~%~%")
      ))
  )








;========================================
; CHROMA ONLY...
;========================================





(defmethod CTL2 ((self om::class-array) (my-model cr::chroma-model) ctl-model
                 &key (outfile "yaka-out") (args-yaka-user nil)
                 (by-time nil))     
  "Generate a synthesis object by combining a control and an analysis model.
Write in the Intermediate-files folder by default."
  (setf outfile (get-cr-path :out :name outfile :type "ctl2"))
                                
  (let ((n-fql (1- (nev my-model)))      ; n-fql - 1 , nmarkers - 2
        resc-min resc-max 
        gblamp-val)
    
    (with-open-file (outstream outfile :direction :output :if-exists :supersede)
      (format  outstream "(defun cr::ctl2-result() (list ~%")
      
      ;;; not used anywhere... ?
      (if by-time
        (setf resc-min (begin-time my-model) resc-max (get-nth-time my-model n-fql))
        (setf resc-min 0 resc-max n-fql))
      
      (loop for i from 0 to n-fql
            do (CTL2_compute_one_array self my-model ctl-model i outstream args-yaka-user))
      
      (format outstream "))~%"))
    
    (load outfile))
  
  (cr::ctl2-result))


(defmethod CTL2_compute_one_array ((self om::class-array) (my-model cr::model-partials) 
                         ctl-model my-rank outstream 
                         args-yaka-user)
  "ctl2 subroutine, process 1 ptl/fql"
  (declare (special outstream my-model my-rank))
  (let*((my-time (get-nth-time my-model my-rank))
        (my-dur (get-nth-dur my-model my-rank))
        (n-fql (1- (nev my-model)))      ; n-fql - 1 , nmarkers - 2
        (my-ptl (nth my-rank (ptl-list my-model)))
        (my-fql my-ptl)
        (my-nev (length (the-list my-ptl))))
    (declare (special n-fql my-time my-dur my-nev my-fql my-ptl))
    (when my-ptl         ;to allow empty fql in models
 ;     (apply 'yaka-user args-yaka-user)
      (format outstream " (om::mk-array 'om::~a ~a ~% ~%"
              (type-of self) my-nev)
      (if (get-gbl 'ctl2-print) (format t "~a~%" my-time))

      (let ((global-args (om::get-global-keywords self)))
        (format outstream" (list ")
        (CTL2_global ctl-model 
                     global-args
                     (mapcar #'(lambda (key) (funcall key self)) global-args)
                     )
        (format outstream" )~%"))

      ; KEYWORDS LOOP
      (CTL2_keywords_loop ctl-model (om::get-instance-keywords self))
      
      ))
  (format  outstream ") ~%~%")
)

