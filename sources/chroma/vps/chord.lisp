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

(in-package :cr)


(export '(spl ail cil rpl) :cr)


(defclass CHORD 
  (VPS)
  ((sp-list :initform nil
            :documentation "list of symbolic-pitch objects"
            :accessor sp-list))
  (:documentation "Un accord")
  )

(defmethod check-syntax ((x chord))
  (setf (sp-list x) (make-sp-list (the-list x)))
  (setf (fql x) (mapcar #'freq (sp-list x))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass SPL
  (CHORD)
  ((the-list :type list 
             :initform '(DO2  DO3 DO5)
             :initarg :the-list 
             :accessor the-list
             :reader get_vps))
  (:documentation "Symbolic Pitch List" )
)

(defmethod initialize-instance :after ((x spl) &rest initargs)
  (declare (ignore initargs))
  (check-syntax x)
  (check-order x))

(defmethod check-syntax :before ((x spl))
  (if (not (pitch-with-octave-p (car (the-list x))))
      (print (format nil "pitch without octave in ~a" (the-list x)))))

(defmethod note-list ((x spl) &key reference)                
  " note-list "
  (declare (ignore reference))
  (relative-pitch (the-list x)))

(defmethod get-spl ((x spl) &key octave reference &allow-other-keys ) 
  (declare (ignore octave reference))
  (the-list x))

(defmethod print-spl ((x spl))
  (loop for i in (get-spl x)
        do(if(listp i)
            (format t "~%NOTE = ~a / deviation = ~a"
                    (car i)(if(numberp (cdr i))(round(cdr i))(cdr i)))
            (format t "~%NOTE = ~a " i))))
  
(defmethod get-cil ((x spl) &key (midi ()))
  (let ((result (midi->semitones (pch->midi (the-list x)))))
  (if midi
    result
    (mapcar #'semitones->itvl result)
    )))

(defmethod get-rpl ((x spl) &key reference)
  (declare (ignore reference))
  (let ((result)
        (origine-octave (octave(car(sp-list x)))))
    (loop for i in (sp-list x)
          do (if (equal (octave i) origine-octave)
               (if(equal(deviation i) 0)
                 (push (note+ i) result)
                 (push (cons(note+ i)(deviation i)) result))
               (if(equal(deviation i)0)
                 (push (format nil "~a~a" (note+ i) (- (octave i) origine-octave)) 
                       result)
                 (push (cons(format nil "~a~a" (note+ i)
                                    (- (octave i) origine-octave))(deviation i))
                       result))))
    (nreverse result)))

(defmethod get-ppl ((x spl) &key)
  (let ((result))
    (loop for i in (sp-list x)
          do (if(equal(deviation i) 0)
               (push (note+ i) result)
               (push (cons(note+ i)(deviation i)) result)))
    (nreverse result)))

(defmethod get-gil ((x spl) &key (midi ()))
  (let ((result (get-gil-vps (mapcar #'(lambda (y)  (midi-note y))(sp-list x)))))
    (if midi
      result
      (mapcar #'semitones->itvl result)
      )))

(defmethod get-ail ((x spl) &key (reference ())(midi ()) &allow-other-keys)
  (if(null reference)(error "MISSING REFERENCE IN ~a"(type-of x)))
  (let ((result (get-ail-vps (mapcar #'(lambda (y) (midi-note y))(sp-list x))
                             (pch->midi (fq->pch reference)))))
    (if midi
      result
      (mapcar #'semitones->itvl result)
      )))

(defmethod get-surface ((x spl) &key (midi ()))
  (let ((result (-(midi-note (car(last(sp-list x))))
                  (midi-note (first(sp-list x))))))
    (if midi
      result
      (semitones->itvl result)
      )))

(defmethod get-density ((x spl))
  (/ (number-of-notes x)(1+ (get-surface x :midi t))))

(defmethod get-homogeneity ((x spl) &key (expanded ())(midi ()))
  (let* ((cil (get-cil x :midi t))
        (result (list (apply #'max cil)(apply #'min cil))))
    (if expanded
      (if midi
        result
        (mapcar #'semitones->itvl result))
      (if midi
        (apply #'- result)
        (semitones->itvl (apply #'- result))
        ))))
      
(defmethod get-sd ((x spl))
  (get-sd-vps (get-cil x :midi t)))

(defmethod get-cs ((x spl) &key (space ()))
  (if space
    (get-cs-vps (get-gil x :midi t) space)
    (get-cs-vps (get-gil x :midi t))))

;;;;;;;;;2
(defclass RPL 
  (CHORD)
  ((the-list :type list 
             :initform '(DO2  DO3 do5)
             :initarg :the-list 
             :accessor the-list
             :reader get_vps))
  (:documentation "Relative Pitch List" )
  )

(defmethod get-rpl ((x rpl) &key reference)
  (declare (ignore reference))
  (the-list x))

(defmethod initialize-instance :after ((x rpl) &rest initargs)
  (declare (ignore initargs))
  (check-syntax x)
  (check-order x))

(defmethod get-spl ((x rpl) &key octave &allow-other-keys)
  (if (null octave)(error "MISSING REFERENCE OCTAVE"))
  (mapcar #'(lambda (y)(midi->pch(+ (* 12 octave) (midi-note y))))
             (sp-list x)))

(defmethod get-ail ((x rpl) &key octave reference &allow-other-keys)
  (get-ail (make-instance 'spl :the-list (get-spl x :octave octave ))
           :reference reference))

(defmethod get-fql ((x rpl)&key (octave 0))
  (mapcar #'(lambda (y) (* (expt 2 octave) y))(fql x)))

(defmethod get-arl ((x rpl) &key reference octave)
  (freqs-to-arl (get-fql x :octave octave) (pch->fq reference)))

;;;;;;;;;2b
(defclass PPL 
  (CHORD)
  ((the-list :type list 
             :initform '(DO LA SOL)
             :initarg :the-list 
             :accessor the-list
             :reader get_vps))
  (:documentation "Pure Pitch List" )
  )

(defmethod get-ppl ((x ppl) &key)
  (the-list x))

(defmethod initialize-instance :after ((x ppl) &rest initargs)
  (declare (ignore initargs))
  (check-syntax x)
  (check-order x)
  (setf (fql x) (mapcar #'freq (sp-list x))))

(defmethod check-syntax ((x ppl))
  (loop for i in (the-list x)
        do (if(not (pitch-without-octave-p i))
             (error "PITCH WITH OCTAVE ~a" i)))
  (setf (sp-list x) (make-sp-list (the-list x))))
 
(defmethod check-order ((x ppl))  ;les frequences sont ascendantes par definition
  (let ((l (sp-list x))(curr-freq 0))
    (loop for p in l
          do (loop while (< (freq p) curr-freq)
                   do (incf (octave p)))
          do (setf curr-freq (freq p)))))

(defmethod get-spl ((x ppl) &key octave &allow-other-keys)
  (if (null octave)(error "MISSING REFERENCE OCTAVE"))
  (mapcar #'(lambda (y)(midi->pch(+ (* 12 octave) (midi-note y))))
             (sp-list x)))

(defmethod get-ail ((x ppl) &key octave reference &allow-other-keys)
  (get-ail (make-instance 'spl :the-list (get-spl x :octave octave ))
           :reference reference))

(defmethod get-fql ((x ppl)&key (octave 0))
  (mapcar #'(lambda (y) (* (expt 2 octave) y))(fql x)))

(defmethod get-arl ((x ppl) &key reference octave)
  (freqs-to-arl (get-fql x :octave octave) (pch->fq reference)))


;;;;;;;;;3
(defclass CIL 
  (CHORD)
  ((the-list :type list 
             :initform '(6- 7+ 6- 6-)
             :initarg :the-list 
             :accessor the-list
             :reader get_vps))
  (:documentation "Contiguous Interval List" )
  ) 

(defmethod initialize-instance :after ((x cil) &rest initargs)
  (declare (ignore initargs))
  (check-syntax x)
  (check-order x))

(defmethod get-cil ((x cil) &key )
  (the-list x))

(defmethod check-order ((x cil))
  (let ((fql (copy-tree (get-fql x :reference 100))))
    (if (not(equal (sort fql '<) (get-fql x :reference 100)))
      (error "l'ordre n'est pas ascendant : ~a" (the-list x)))
    (if (unisons-p fql)
      (error "UNISSONS INTERDIT : ~a" (the-list x)))))

(defmethod check-syntax ((x cil))
  (if(null (the-list x))(error "EMPTY INTERVAL LIST"))
  (loop for i in (the-list x)
        when (not(or(memberp i *INTERVALLES*)
                    (and (memberp (car i) *INTERVALLES*)
                         (or(null (third i))
                            (numberp (third i))
                            (memberp (third i) *DEVIATIONS*)))))
        do (error "INTERVALLE NON AUTORISE : ~a" i)))

(defmethod number-of-notes ((x cil))
  "Number of Notes"
  (1+ (length (the-list x))))

(defmethod get-spl ((x cil) &key reference &allow-other-keys)
  (if (null reference)(error "MISSING REFERENCE PITCH"))
(mapcar #'midi->pch (itvl->midi (the-list x) (pch->midi(fq->pch reference)))))

(defmethod get-ail ((x cil) &key reference midi &allow-other-keys)
  (declare (ignore midi))
  (get-ail (make-instance 'spl :the-list (get-spl x :reference reference ))
           :reference reference))

(defmethod get-fql ((x cil) &key reference)
  (get-fql (make-instance 'spl :the-list (get-spl x :reference reference ))))

(defmethod  get-crl ((x cil))
  (fq->ratio (get-fql x :reference 100)))

(defmethod get-arl ((x cil) &key reference )
  (freqs-to-arl (get-fql x :reference reference) (pch->fq reference)))


;;;;;;;;4
(defclass AIL 
  (anchored-vps CHORD)
  ((the-list :type list 
             :initform '((1 -1 12) (2+ 0 -5) 3+ 7+ (3- 1 -50))
             :initarg :the-list 
             :accessor the-list
             :reader get_vps)
   (reference :initform "LA4"
              :initarg :reference
              :accessor reference))
  (:documentation "Anchored Interval List" )
  )

(defmethod get-ail ((x ail) &key reference midi &allow-other-keys)
  (declare (ignore midi))
  (get-ail (make-instance 'spl :the-list (get-spl x)) :reference reference))

(defmethod initialize-instance :after ((x ail) &rest initargs)
  (declare (ignore initargs))
  (check-reference x)
  (check-syntax x)
  (let ((reference-freq
         (if (numberp(reference x))
           (reference x)
           (pch->fq (reference x)))))
    (setf (fql x) (ail-to-freq (the-list x) reference-freq)))
  (check-order x))

(defmethod check-syntax ((x ail))
  (if (null (reference x))(error "MISSING REFERENCE"))
  (loop for i in (the-list x)
        when (not(or(memberp i *INTERVALLES*)
                    (and (memberp (car i) *INTERVALLES*)
                         (or(null (third i))
                            (numberp (third i))
                            (memberp (third i) *DEVIATIONS* )))))
        do (error "INTERVALLE NON AUTORISE : ~a" i)))

(defmethod get-spl ((x ail) &key &allow-other-keys)
  (mapcar #'midi->pch (ail-to-midi (the-list x) (pch->midi(fq->pch(reference x))))))

(defmethod get-cil ((x ail) &key )
 (get-cil (make-instance 'spl :the-list (get-spl x))))

(defmethod print_vs ((x ail))
  (format t "Vertical Structure :~%~a    (~a)
    Anchored intervals: ~a
    Reference: ~a~%"
          (class-of x)
          (documentation (class-name (class-of x))'type)
          (slot-value x 'the-list)
          (reference x)))
