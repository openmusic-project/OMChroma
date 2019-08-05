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

;;; 1) no conversion rule specified
(defmethod! expand-model ((self cr-model) (ctl-model null) (target-class symbol))
  (remove 
   nil 
   (loop for elt in (model-data self)
         for i from 0
         collect (ctl-make-synthesis-event-simple 
                  target-class 
                  (car (nth i (time-struct self)))
                  elt))
   ))

;;; 2) control model is a patch (reference mode)
(defmethod! expand-model ((self cr-model) (ctl-model function) (target-class symbol))
  (remove 
   nil 
   (loop for i from 0 to (1- (length (model-data self)))
         collect (ctl-make-synthesis-event ctl-model target-class i self)
         ))
  )

;;; 3) Chroma-style : a list of text-rules
(defmethod! expand-model ((self cr-model) (ctl-model list) (target-class symbol))
  (remove 
   nil 
   (loop for i from 0 to (1- (length (model-data self)))
         collect (ctl-make-synthesis-event-chroma ctl-model target-class i self))
   ))



;;;==================================================================
;;; NO RULE
;;;==================================================================

(defmethod ctl-make-synthesis-event-simple (target-class onset datachunk)
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

(defmethod ctl-make-synthesis-event ((cr-ctrl function) target-class i model)

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
    
    ;; (print out-vals)

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

(defun cr-interne-list (list package)
  (loop for elt in list 
        collect
        (cond ((listp elt) (cr-interne-list elt package))
              ((symbolp elt) (intern (symbol-name elt) package))
              (t elt))))


(defmethod ctl-make-synthesis-event-chroma ((ctl-model list) target-class i (model cr-model))  
  
  (declare (special my-model my-rank my-time my-dur n-fql my-fql my-nev my-ptl outstream))
  
  ;;; a whole bunch of side-effects for choroma "CTL" to work with...
  (setf my-model model)
  (setf my-rank i)
  (setf my-time (car (nth my-rank (time-struct my-model))))
  (setf my-dur (- (cadr (nth my-rank (time-struct my-model)))
                  (car (nth my-rank (time-struct my-model)))))
  (setf n-fql (length (time-struct my-model)))      
  (setf my-fql (nth my-rank (model-data my-model)))
  (setf my-nev (length (get-vps-freqs my-fql)))
  (setf my-ptl my-fql)
  
  ; (print (list my-rank my-time my-dur n-fql my-fql my-nev))
  
  (let* ((dur-scaler-arg (find :dur-scale ctl-model :key #'car))
         (min-dur-arg (find :min-dur ctl-model :key #'car))
         (args-yaka-user (append dur-scaler-arg min-dur-arg)))
    (when args-yaka-user (apply #'yaka-user args-yaka-user)))
 
  
  (when my-fql
    
    (if (cr::get-gbl 'cr::ctl2-print) (print my-time))
     
    ;;; another side-effect for Chroma
    (setf outstream (make-string-output-stream))
    
    (let ((array (om::om-init-instance
                  (make-instance target-class
                                 :elts my-nev
                                 :action-time my-time))))
    
      ;;; we don't need to do this one as the action-time and nelts are already set
      ;;; (cr::CTL2_global array ctl-model)
      
      (cr::CTL2_keywords_loop ctl-model (mapcar #'(lambda (field) (intern (string-upcase (om::array-field-name field))))
                                                (om::data array)))
      
      (om::om-init-instance 
       array
       (om::group-list 
       (om::om-read-list-from-string (get-output-stream-string outstream)) 
       2 'om::linear))

      ))
  )



