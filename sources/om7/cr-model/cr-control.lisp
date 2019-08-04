; OMChroma
; High-level control of sound synthesis in OM
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
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307,10 USA.
;
; (c) Ircam 2000 - 2019
;Authors: C. Agon, M. Stroppa, J. Bresson, S. Lemouton



;;;=======================
;;; CR-CONTROL VERSION OM7
;;;=======================
; J. Bresson, 2019


(in-package :cr)


(defmethod! expand-model ((my-model cr-model) ctl-model (target-class cs-evt))
  :initvals '(nil nil add-1)
  :indoc '("a CR-MODEL" "conversion rules as text (list) or as a lambda patch" "target synthesis-class")
  :icon 655
  :doc "Converts the data + time-structure in <my-model> to a list of synthesis classes of type of <target-class>,
following conversion rules as defined in <ctl-model>."
  :outdoc '("a list od synthesis events => synthesize")
  (expand-model my-model ctl-model (type-of target-class)))


;;; 3 CASES:
;;; 1) control model is a patch (reference mode)
(defmethod! expand-model ((my-model cr-model) (ctl-model function) (target-class symbol))
  (remove 
   nil 
   (loop for i from 0 to (1- (length (model-data my-model)))
         collect (make-ctl-synthesis-event ctl-model target-class i my-model)
         ))
  )

;;; 2) no conversion rule specified
(defmethod! expand-model ((my-model cr-model) (ctl-model null) (target-class symbol))
  (remove 
   nil 
   (loop for elt in (model-data my-model)
         for i from 0
         collect (make-simple-synthesis-event 
                  target-class 
                  (car (nth i (time-struct my-model)))
                  elt))
   ))

;;; 3) Chroma-style : a list of text-rules
(defmethod! expand-model ((my-model cr-model) (ctl-model list) (target-class symbol))
  (remove 
   nil 
   (loop for elt in (model-data my-model)
         collect (remove nil (cr::make-ctl2-list my-model ctl-model target-class)))
   ))



;;;==================================================================
;;; NO RULE
;;;==================================================================

(defmethod make-simple-synthesis-event (target-class onset datachunk)
  (when datachunk 
    (om::om-init-instance 
     (make-instance target-class
                    :elts (length (cr-partials-freqs datachunk))
                    :action-time onset)
       `((:freq ,(cr-partials-freqs datachunk))
         (:amp ,(cr-partials-amps datachunk))))
    ))


;;;==================================================================
;;; VISUAL RULE
;;;==================================================================

;;; MODEL-DATA
;;; Virtual access to the model in CR-CONTROL patch

(defclass! model-ctl () 
  ((global-model :accessor global-model :initarg :global-model :initform nil)
   (local-time :accessor local-time :initarg :local-time :initform nil)
   (local-dur :accessor local-dur :initarg :local-dur :initform nil)
   (local-rank :accessor local-rank :initarg :local-rank :initform nil)
   (local-data :accessor local-data :initarg :local-data :initform nil)))

(defmethod! out-slot (slot-name slot-val)
  :icon 665
  :doc "Declares a rule for the value of a given matrix slot in the control-patch of EXPAN-MODEL"
  :indoc'("slot name" "slot val")
  :outdoc '("connect me to an OUT box!")
  
  (list (om::intern-k slot-name) slot-val))

(defmethod make-ctl-synthesis-event ((cr-ctrl function) target-class i model)

  (let* ((segment (nth i (time-struct model)))
         (onset (car segment))
         (dur (- (cadr segment) (car segment)))
         (datachunk (get-nth-data-frame model i))
         
         (model-data (make-instance 'model-ctl
                                    :global-model model
                                    :local-rank i 
                                    :local-time onset
                                    :local-dur dur
                                    :local-data datachunk))
    
         (out-vals 
          (remove-if 
           #'(lambda (out) (not (and (consp out) (symbolp (car out)))))
           (multiple-value-list 
            (funcall cr-ctrl model-data)))))
         
    (unless (find :freq out-vals :key #'car)
      (when (cr-partials-freqs datachunk)
        (setf out-vals (cons (list :freq (cr-partials-freqs datachunk))
                             out-vals))))
    
    (unless (find :amp out-vals :key #'car)
      (when (cr-partials-amps datachunk)
        (setf out-vals (cons (list :amp (cr-partials-amps datachunk))
                             out-vals))))
    
    (unless (find :durs out-vals :key #'car)
      (setf out-vals (cons (list :durs dur) out-vals)))
    
    (print out-vals)

    (let* ((elts (or (cadr (find :elts out-vals :key #'car))
                     (length (cadr (find :freq out-vals :key #'car)))))
           (action-time (or (cadr (find :action-time out-vals :key #'car))
                            onset)))
      
      (om::om-init-instance 
       (make-instance target-class
                      :elts elts
                      :action-time action-time)
       
       out-vals)
      )
    ))



;;;==================================================================
;;; TEXT RULES : CONNECTION WITH THE CHROMA LIBRARY...
;;;==================================================================

;by serge 07082001
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

;;;;;========================================

(defmethod make-ctl2-list ((my-model cr-model) ctl-model target-class
                           &key (outfile "yaka-out") (args-yaka-user nil)
                           (by-time nil))     
  (setf outfile  (get-cr-path :tmp :name outfile :type "ctl2"))
  (let ((n-fql (1- (om::model-nb-evts my-model)))      ; n-fql - 1 , nmarkers - 2
        resc-min resc-max gblamp-val )
    ; ? si outfile nil : stream pas vers fichier mais vers "load" ?
    (with-open-file  (outstream outfile :direction :output :if-exists :supersede)
      (format  outstream "(in-package :chroma)(defun cr::ctl2-result() (list ~%")
      (if by-time
        (setf resc-min (car (car (om::time-struct my-model))) resc-max (cadr (last-elem (om::time-struct my-model))))
        (setf resc-min 0 resc-max n-fql))
      ; FQL LOOP     
      (loop for i from 0 to n-fql
            do (cr::om-CTL2 target-class my-model (cr-interne-list ctl-model :chroma) i outstream args-yaka-user))
      (format outstream "))~%"))
    (load outfile))
  (cr::ctl2-result))


(defun cr-interne-list (list package)
  (loop for elt in list 
        ;do (print elt)
        collect
        (cond ((listp elt) (cr-interne-list elt package))
              ((symbolp elt) (intern (symbol-name elt) package))
              (t elt))))

(defmethod om-ctl2 (type (my-model cr-model) ctl-model my-rank outstream 
                    args-yaka-user)
  (declare (special outstream my-model my-rank))
  ;(print ctl-model)
  (let*((my-time (car (nth my-rank (om::time-struct my-model))))
        (my-dur (- (cadr (nth (+ 1 my-rank) (om::time-struct my-model))) (car (nth (+ 1 my-rank) (om::time-struct my-model)))))
        (n-fql (1- (om::model-nb-evts my-model)))      ; n-fql - 1 , nmarkers - 2
        (my-fql (nth my-rank (om::elements (om::data my-model))))
        (my-ptl my-fql)
        (my-nev (length (om::get-vps-freqs my-fql)))
        string1)
    (declare (special n-fql my-time my-dur my-nev my-fql my-ptl))
    (when my-fql
      (when args-yaka-user (apply #'yaka-user args-yaka-user))
      (format outstream "(om::mk-array 'om::~a ~a ~%~%"
              type my-nev) 
      (if (cr::get-gbl 'cr::ctl2-print) (format t "~a~%" my-time))
      ; A FAIRE : GLOBALSLOTS loop sur (first (multiple-value-list (om::get-slot-in-out-names self)))
      (cr::CTL2_global self ctl-model)
      ; KEYWORDS LOOP
      (cr::CTL2_keywords_loop self ctl-model)
      (format  outstream ") ~%~%")
      ))
  )










