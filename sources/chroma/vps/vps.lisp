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
; File author: M. Stroppa
;=====================================================

; VPS SUPERCLASSES

(in-package :cr)
        
(defclass VS ()
  ((the-list :type list 
             :initform nil
             :initarg :the-list 
             :accessor the-list
             :reader get_vps
             ))
  (:documentation "Vertical Structure" ))

(defmethod initialize-instance :after ((x vs) &rest initargs)
  (declare (ignore initargs))
  (if (null (the-list x)) 
    ;(error "MISSING LIST FOR ~a" (type-of x))
    (print (format nil "Warning: no list in ~a" (type-of x)))
    ))

(defmethod print_vs ((x vs))
  (format t "Vertical Structure :~%~a    (~a)
    List's items: ~a~%"
          (class-of x)
          (documentation (class-name (class-of x))'type)
          (slot-value x 'the-list)))

(defmethod print_vs ((x t))
  (print_vs (make_vps x)))

(defmethod number-of-notes ((x vs))
  "Number of Notes in a VPS"
  (length (the-list x)))

        
;;;;;;;;;

(om::defclass! VPS (VS)
  ((fql :initform nil
        ;:initarg :fql
        :accessor fql
        :documentation "List of Frequencies")
   ;(reference :initform nil
   ;           :initarg :reference
   ;           :accessor reference)
   (diapason :initform (get-gbl 'DIAPASON) :accessor diapason))
  (:documentation "Vertical Pitch Structure" ))

;; mixin class for AIL and ARL
(om::defclass! anchored-vps ()
  ((reference :initform nil :initarg :reference :accessor reference)))

(defmethod reorder-vps ((x vps)) 
  (error (concatenate 'string 
                      "No method is defined for automatic reordering of class " 
                      (string (type-of x))
                      ".")))




(defmethod check-order ((x vps))
  (let ((fql (copy-tree (get-fql x)))
        (reorder nil))
    (when (not (equal (sort fql '<) (get-fql x)))
        (print (format nil "Warning: FREQUENCIES ARE NOT ORDERED : ~a" (the-list x)))
        (setf reorder t))
    (when (unisons-p fql)
        (print (format nil "Warning: UNISON IS FORBIDDEN : ~a" (the-list x)))
        (setf reorder t))
    (when reorder
      (reorder-vps x))))



(defmethod  check-reference ((x vps))
  (pch->fq (reference x))
  (if(not(or(numberp (reference x))(pitch-with-octave-p (reference x))))
      (error "WRONG REFERENCE : ~a" (reference x)))
  )

(defmethod get-fql ((x t) &key &allow-other-keys)
  ())
;to allow empty fql in models (get-fql nil) -> nil

(defmethod get-fql ((x vps) &key &allow-other-keys)
 (fql x))

(defmethod get-rpl ((x vps) &key reference )
  (get-rpl (make-instance 'spl :the-list (get-spl x :reference reference ))))

(defmethod get-cil ((x vps) &key )
  (get-cil (make-instance 'spl :the-list (get-spl x :reference 'la4 :octave 2))))

(defmethod get-crl ((x vps))
  (fq->ratio (fql x)))

(defmethod get-arl ((x vps) &key reference)
  (freqs-to-arl (fql x) (pch->fq reference)))

(defmethod note-list ((x vps) &key reference)                
  " note-list "
  (note-list(make-instance 'spl :the-list
                           (get-spl x :octave 2 :reference reference))))

(defmethod get-gil ((x vps) &key (midi ()))
  (get-gil (make-instance 'spl :the-list (get-spl x :octave 2 :reference 'la4))
          :midi midi))

(defmethod get-surface ((x vps) &key (midi ()))
  (get-surface
   (make-instance 'spl :the-list
                  (get-spl x :octave 2 :reference 'la4)) :midi midi))

(defmethod get-density ((x vps))
  (get-density
   (make-instance 'spl :the-list (get-spl x :octave 2 :reference 'la4))))

(defmethod get-homogeneity ((x vps) &key (expanded ()) (midi ()))
  (get-homogeneity
   (make-instance 'spl :the-list (get-spl x :octave 2 :reference 'la4))
   :midi midi :expanded expanded))

(defmethod get-sd ((x vps))
  (get-sd (make-instance 'spl :the-list (get-spl x :octave 2 :reference 'la4))))

(defmethod get-cs ((x vps) &key (space ()))
  (get-cs
   (make-instance 'spl :the-list (get-spl x :octave 2 :reference 'la4))
   :space space))

(defmethod get-harmonicity ((x vps) &key octave reference  (f0 ())
                                    (expanded ()))
  (get-harmonicity
   (make-instance 'fql :the-list (get-fql x :octave octave
                                          :reference reference))
   :f0 f0 :expanded expanded))

(defmethod get-virt-fund ((x vps) &key octave reference  (grid-ratio 0.001))
  (get-virt-fund
   (make-instance 'fql :the-list (get-fql x :octave octave
                                          :reference reference))
   :grid-ratio grid-ratio ))

(defmethod get-max-fq ((x vps) &key octave reference)
  (let ((l (get-fql x :octave octave :reference reference)))
    (if (not (null l))(apply #'max l))))

(defmethod get-min-fq ((x vps)  &key octave reference)
  (let ((l (get-fql x :octave octave :reference reference)))
    (if (not (null l))(apply #'min l))))


;;; like copy-instance but without deep-copy (not needed)
(defmethod copy_vs ((source vps) &key to)
  "Shallow copies the slots of 'source' to the slots of 'target'. Source must be subclass of target"
  (let ((copy (or to (make-instance (type-of source)))))
    (loop for slot in (mapcar #'slot-definition-name (class-slots (class-of source)))
          when (slot-boundp source slot)
          do (setf (slot-value copy slot) (slot-value source slot)))
    copy))


(defmethod print_vs ((x vps))
  (format t "Vertical Structure :~%~a    (~a)
    List's items: ~a~%"
          (class-of x)
          (documentation (class-name (class-of x))'type)
          (slot-value x 'the-list)))

(defmethod number-of-notes ((x vps))
  "Number of Notes in a VPS"
  (length (the-list x)))

;to allow empty vps in models:
(defmethod get-max-fq ((x t)  &key  &allow-other-keys) nil)
(defmethod get-min-fq ((x t)  &key  &allow-other-keys) nil)
(defmethod get-max-amp ((x t)) nil)
(defmethod get-min-amp ((x t)) nil)
(defmethod get-max-bw ((x t)) nil)
(defmethod get-min-bw ((x t)) nil)
(defmethod the-list ((x t)) nil)
(defmethod amplitudes ((x t)) nil)

