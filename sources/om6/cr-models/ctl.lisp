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




(defun mk-array (class numcols fixedargs &rest argkeys) 
  (let* ((rep (make-instance class :numcols numcols))
         (initargs (get-all-initargs-of-class class))
         ; (precision 3) 
         slot?)
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
         (fixed-slots-list (fixed-slots-list instance))
         (fixedargs-size (if (find 'numcols fixed-slots-list)
                             (- (length fixed-slots-list) 1)
                           (length fixed-slots-list)))
         (fixedargs (make-list fixedargs-size))
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
            (setf slotslist (append (first-n slotslist pos) (nthcdr (+ pos 1) slotslist)))
          ))
        
        
        (loop for fsl in fixed-slots-list do
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
                ))
        
        (if (subtypep target-class 'class-array)
            (apply 'mk-array (append 
                              (list target-class 
                                    (or ncols (max 1 (length (get-vps-freqs data))))
                                    fixedargs)
                              argslist))
          (let ((rep (make-instance target-class)))
            (loop for fsl in fixed-slots-list 
                  for i from 0 do
                  (setf (slot-value rep fsl) (nth i fixedargs)))
            rep)
          )
          

        
        ))))




;========================================
; TEXT VERSION
;========================================

;;; Utils to access and create arrays
(defun get-ctl-instance-keywords (o)
  "get-keywords in control synthesis object instance (class slots + instance specific controls)"
  (append (mapcar 
           #'(lambda (x) (read-from-string (om::name x)))
           (om::get-all-initargs-of-class (type-of o)))
          (mapcar #'car (om::lcontrols o))
          (list 'lprecision)))

(defun get-ctl-global-keywords (o)
  "get global controls in control synthesis object instance,
except numrows and action-time (amptot, durtot, etc)"
  (cddr (om::fixed-slots-list o)))



(defun cr-interne-list (list package)
  (loop for elt in list 
        collect
        (cond ((listp elt) (cr-interne-list  elt package))
              ((symbolp elt) (intern (symbol-name elt) package))
              (t elt))))


;;; Expand model version Chroma
(defmethod! expand-model ((self cr-model) (rules list) (target-class symbol))
  :icon 655
  (if (data self)
      
      (cr::ctl-process-model self (cr-interne-list rules :chroma) target-class)

    (om-beep-msg "Warning : CR-MODEL is not initialized!")))



;;;================================================================
;;; switch to the chroma world

(in-package :cr)

;;;================================================================

;; used by some functiosn in CTL processing
(defmethod offset ((self om::cr-model)) 0)

;;; will be redefined by some functions
(defun ctl2-result ())


(defmethod ctl-process-model ((self om::cr-model) (rules list) target-class
                              &key (outfile "yaka-out") (args-yaka-user nil))
     
                    
  (let ((out-path (get-cr-path :tmp :name outfile :type "ctl2")))
    
    (with-open-file  (outstream out-path :direction :output :if-exists :supersede)
 
      ;;; outstream will be used by chroma functions
      (declare (special outstream))
 
      (format outstream "(in-package :cr)~%~%") 
      (format outstream "(defun cr::ctl2-result() ~%(list ~%")
      
      (let ((n (om::model-nb-evts self)))
        
        (loop for i from 0 to (1- n)
              do (make-ctl target-class self rules i outstream args-yaka-user)))
      
      (format outstream "))~%"))
    
    (load out-path)
    (cr::ctl2-result)
    ))


(defmethod make-ctl ((target-class symbol) 
                     (model om::cr-model) 
                     rules i stream 
                     args-yaka-user)
  
  (declare (special my-model my-rank n-fql my-time my-dur my-nev my-fql my-ptl))
  
  ;;; a whole bunch of side-effects for choroma "CTL" to work with these special variables
  (setf my-model model)
  (setf my-rank i)
  (setf my-time (car (nth my-rank (om::time-struct my-model))))
  (setf my-dur (- (cadr (nth my-rank (om::time-struct my-model)))
                      (car (nth my-rank (om::time-struct my-model)))))
  (setf n-fql (length (om::time-struct my-model)))      
  (setf my-fql (nth my-rank (om::elements (om::data model))))
  (setf my-nev (length (om::get-vps-freqs my-fql)))
  (setf my-ptl my-fql)

  (when my-fql
      
    (when args-yaka-user (apply #'yaka-user args-yaka-user))
      
    (format stream "(om::mk-array 'om::~a ~a ~%" target-class my-nev) 
      
    (if (cr::get-gbl 'cr::ctl2-print) (format t "~a~%" cr::my-time))
      
      ; A FAIRE : GLOBALSLOTS loop sur (first (multiple-value-list (om::get-slot-in-out-names self)))
    (let* ((instance (make-instance target-class))
           (global-args-names (om::get-ctl-global-keywords instance))
           (global-args-defvals (mapcar #'(lambda (key) (funcall key instance)) global-args-names))
           (array-slots (om::get-ctl-instance-keywords instance)))
      
      (format stream "(list ")
      (CTL2_global rules global-args-names global-args-defvals)
      (format stream " ) ~%")
            
      (CTL2_keywords_loop rules array-slots)

      (format stream ") ~%~%")
      ))
  )





