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

;; ...
(defmethod cr::offset ((self om::cr-model)) 0)

;;; Expand model version Chroma
(defmethod! expand-model ((my-model cr-model) (ctl-model list) (target-class SynthesisEvt))
  :icon 655
  (if (data my-model)
      (let ((n (length (elements (data my-model)))))
        (remove nil (make-cr-ctl2-list my-model ctl-model target-class)))
    (om-beep-msg "Warning : CR-MODEL is not initialized!")))


(defmethod make-cr-ctl2-list ((my-model cr-model) ctl-model target-class
                           &key (outfile "yaka-out") (args-yaka-user nil)
                           (by-time nil))     
  (setf outfile  (cr::get-cr-path :tmp :name outfile :type "ctl2"))
  (let ((n-fql (1- (model-nb-evts my-model)))      ; n-fql - 1 , nmarkers - 2
        resc-min resc-max gblamp-val )
    ; ? si outfile nil : stream pas vers fichier mais vers "load" ?
    (with-open-file  (outstream outfile :direction :output :if-exists :supersede)
      (format outstream "(in-package :chroma) (defun cr::ctl2-result() (list ~%")
      (if by-time
        (setf resc-min (car (car (time-struct my-model))) resc-max (cadr (last-elem (time-struct my-model))))
        (setf resc-min 0 resc-max n-fql))
      ; FQL LOOP     
      (loop for i from 0 to n-fql
            do (om-ctl2 target-class my-model (cr-interne-list ctl-model :chroma) i outstream args-yaka-user))
      (format outstream "))~%"))
    (load outfile))
  (cr::ctl2-result))

(defun cr-interne-list (list package)
  (loop for elt in list 
        collect
        (cond ((listp elt) (cr-interne-list  elt package))
              ((symbolp elt) (intern (symbol-name elt) package))
              (t elt))))

(defun mk-array (class numcols fixedargs  &rest argkeys) 
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



(defmethod om-ctl2 ((self SynthesisEvt) (my-model cr-model) 
                    ctl-model my-rank outstream 
                    args-yaka-user)
  (declare (special outstream my-model my-rank))
  (let*((my-time (car (nth my-rank (time-struct my-model))))
        (my-dur (- (cadr (nth (+ 1 my-rank) (time-struct my-model))) (car (nth (+ 1 my-rank) (time-struct my-model)))))
        (n-fql (1- (model-nb-evts my-model)))      ; n-fql - 1 , nmarkers - 2
        (my-fql (nth my-rank (elements (data my-model))))
        (my-ptl my-fql)
        (my-nev (length (get-vps-freqs my-fql)))
        string1)
    (declare (special n-fql my-time my-dur my-nev my-fql my-ptl))
    (when my-fql
      (when args-yaka-user (apply #'cr::yaka-user args-yaka-user))
      (format outstream "(om::mk-array 'om::~a ~a ~%~%" (type-of self) my-nev) 
      (if (cr::get-gbl 'cr::ctl2-print) (format t "~a~%" my-time))
      ; A FAIRE : GLOBALSLOTS loop sur (first (multiple-value-list (om::get-slot-in-out-names self)))
      (cr::CTL2_global self ctl-model)
      ; KEYWORDS LOOP
      (cr::CTL2_keywords_loop self ctl-model)
      (format  outstream ") ~%~%")
      ))
  )

;; doesn't work: use cons-array ??
(defmethod make-simple-synthesis-event (target-class onset data)
  (when data 
    (let ((rep (make-instance (class-name (class-of target-class))
                              :numcols (length (get-vps-freqs data)))))
      (setf (action-time rep) onset)
      (setf (freq rep) (get-vps-freqs data))
      (setf (amp rep) (get-vps-amps data))
      rep)))



;;; text ctl: do nothing ??
(defmethod make-ctl-synthesis-event ((ctl list) target-class i model)
  (let* ((t1 (car (nth i (time-struct model))))
         (t2 (cadr (nth i (time-struct model))))
         (data (nth i (elements (data model))))
         rep)
    (when data 
      (setf rep (make-instance (class-name (class-of target-class))
                               :numcols (length (get-vps-freqs data))))
      (setf (action-time rep) t1))
    rep))

    
;;; visual ctl
(defmethod make-ctl-synthesis-event ((cr-ctrl cr-control) (target-class SynthesisEvt) i model)
  (let* ((rep nil)
         (t1 (car (nth i (time-struct model))))
         (t2 (cadr (nth i (time-struct model))))
         (dur (- t2 t1))
         (ncols nil)
         (fixedargs (make-list (- (length (fixed-slots-list target-class)) 1)))
         (data (nth i (elements (data model)))))
    (when data 
      (let ((modeldata (make-instance 'virtual-modeldata :local-time t1 
                                      :local-dur dur
                                      :local-data data
                                      :local-rank i))
            (realslots (mapcar 'name (get-all-initargs-of-class (type-of target-class))))
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
        
        
        (loop for fsl in (fixed-slots-list target-class) do
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
        (setf rep (apply 'mk-array (append 
                                      (list (class-name (class-of target-class)) 
                                          (or ncols (max 1 (length (get-vps-freqs data))))
                                          fixedargs)
                                      argslist)))
        rep
        ))))



(defmethod make-ctl-synthesis-event ((cr-ctrl cr-control) (target-class t) i model)
  (let* ((rep nil)
         (t1 (car (nth i (time-struct model))))
         (t2 (cadr (nth i (time-struct model))))
         (dur (- t2 t1))
         (data (nth i (elements (data model)))))
    (when data 
      (let ((modeldata (make-instance 'virtual-modeldata :local-time t1 
                                      :local-dur dur
                                      :local-data data
                                      :local-rank i))
            slotslist)
        (setf (global-model modeldata) model)
        (setf slotslist (multiple-value-list (apply (intern (string (code (ctl cr-ctrl))) :om) (cons modeldata (args cr-ctrl)))))
        
        ;;; test
        (setf (tmpmodeldata (ctl cr-ctrl)) modeldata)

        (when (and (find 'action-time (get-init-instance-slots-of-class (type-of target-class)) :key 'car :test 'equal)
                   (not (find "action-time" slotslist :test 'string-equal :key 'car)))
          (setf slotslist (cons (list "action-time" t1) slotslist)))
        (when (and (find 'dur (get-init-instance-slots-of-class (type-of target-class)) :key 'car :test 'equal)
                   (not (find "dur" slotslist :test 'string-equal :key 'car)))
          (setf slotslist (cons (list "dur" dur) slotslist)))

        (setf rep (make-instance (class-name (class-of target-class))))
        (loop for s in slotslist do
              (setf (slot-value rep (interne (car s))) (cadr s)))
        rep
        ))))


