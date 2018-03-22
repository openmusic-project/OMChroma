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

; MODIFICATIONS TO VPS'S.
; IMPLEMENTED BY SERGE LEMOUTON, IRCAM, APRIL 1997.
; MODIFIED AND EXTENDEND BY MARCO STROPPA, MAY 1998.

(in-package :cr)

;---------------------------------------------------------------------------------
; (revert spl [starting-pitch])
; Return a SPL with the same pitch classes read backwards (produce the
;    complementary intervals) and starting at the same octave or on a given
;    pitch.
; Ex: (DO3 MI3 SOL4 SIb3 RE4)  ->  (RE3 SIb3 SOL4 MI5 DO6)

(defmethod revert ((x spl) &optional start &key (end nil))
  (let* ((oct (octave (car (sp-list x))))
         (vps (make-instance
                'spl :the-list
                (get-spl (make-instance 'ppl :the-list (reverse (get-ppl x)))
                         :octave oct))))
    (if start
      (if end
        (transpose vps (car (pch->itvl (list (car (last (the-list vps))) start))))
        (transpose vps (car (pch->itvl (list (car (the-list vps)) start)))))
      vps)))

(defmethod revert ((x vps) &optional start &key (end nil))
  (declare (ignore start end))
  (error "I'M ONLY ACTIVE WITH SPL'S. SORRY: ~a" x))

;---------------------------------------------------------------------------------

;---------------------------------------------------------------------------------
; (transpose vps val)

(defmethod transpose ((x spl) (interval number))
"
 Transpose an SPL or an FQL. If val is a number, it will be considered as a ratio,
    otherwise it will be considered as an interval.
 If the VPS is a FQL with amplitudes and bandwidths, they will be copied in the
    transposed object.
 Return: a new SPL or FQL.
 REMARK: the intervals of perfect IV and V are to be written as (4 0) and (5 0),
            otherwise the system will take the argument for a ratio.
"
  (make-instance
    'spl :the-list 
    (mapcar #'(lambda (p)
                (midi->pch (+ (car (ratio->semitones (list interval)))
                              (midi-note p))))
            (sp-list x))))

(defmethod transpose ((x spl) (interval t))
  (if (interval-p interval)
    (make-instance
      'spl :the-list 
      (mapcar #'(lambda (p)
                  (midi->pch (+ (car (itvl->semitones (list interval)))
                                (midi-note p))))
              (sp-list x)))
    (error "WANNA A REAL INTERVAL OR A RATIO, BUT NOT THIS SHITTY ARGUMENT:~a" interval)))

(defmethod transpose ((x fql) (ratio number))
  (if (and (get-amp x) (bwl x))
    (make-instance 'fql :the-list 
                   (mapcar #'(lambda (f) (* ratio f)) (fql x))
                   :amplitudes (get-amp x)
                   :bwl (bwl x))
    (if (get-amp x)
      (make-instance 'fql :the-list 
                     (mapcar #'(lambda (f) (* ratio f)) (fql x))
                   :amplitudes (get-amp x))
      (make-instance 'fql :the-list 
                     (mapcar #'(lambda (f) (* ratio f)) (fql x))))))

(defmethod transpose ((x ptl) (ratio number))
  (make-instance 'ptl :the-list 
                 (mapcar #'(lambda (f) (* ratio f)) (fql x))
                 :amplitudes (get-amp x)
                 :entry-delays (entry-delays x)
                 :durs (durs x)
                 :transp_funs (transp_funs x)
                 :amp_funs (amp_funs x)))
 
(defmethod transpose ((x fql) (ratio t))
  (if (interval-p ratio)
    (if (and (get-amp x) (bwl x))
      (make-instance 'fql :the-list 
                     (mapcar #'(lambda (f)
                                 (* (car (itvl->ratio (list ratio))) f))
                             (fql x))
                     :amplitudes (get-amp x)
                     :bwl (bwl x))
      (if (get-amp x)
        (make-instance 'fql :the-list 
                       (mapcar #'(lambda (f)
                                   (* (car (itvl->ratio (list ratio))) f))
                               (fql x))
                       :amplitudes (get-amp x))
        (make-instance 'fql :the-list 
                       (mapcar #'(lambda (f)
                                   (* (car (itvl->ratio (list ratio))) f))
                               (fql x)))))
    (error "WANNA A REAL INTERVAL OR A RATIO, BUT NOT THIS SHITTY ARGUMENT:~a"
           ratio)))

(defmethod transpose ((x ptl) (ratio t))
  (if (interval-p ratio)
    (make-instance 'fql :the-list 
                   (mapcar #'(lambda (f)
                               (* (car (itvl->ratio (list ratio))) f))
                           (fql x))
                   :amplitudes (get-amp x)
                   :entry-delays (entry-delays x)
                   :durs (durs x)
                   :transp_funs (transp_funs x)
                   :amp_funs (amp_funs x))
    (error "WANNA A REAL INTERVAL OR A RATIO, BUT NOT THIS SHITTY ARGUMENT:~a"
           ratio)))

(defmethod transpose ((x vps) (interval t))
  (declare (ignore interval))
  (error "IF YOU DON'T GIVE ME AN SPL OR A FQL, I'LL GO ON STRIKE: ~a" x))
;---------------------------------------------------------------------------------


;---------------------------------------------------------------------------------
; (mirror spl [starting pitch])
; Return the inversion of an SPL starting on the same pitch or on a given pitch
; If :end is t, the result will finish on the starting-pitch.
; Symmetric VPS's will yield the same result!

(defmethod mirror ((x cil) &optional start &key (end nil))
  (declare (ignore start end))
  (make-instance 'cil :the-list (reverse (the-list x))))

(defmethod mirror ((x spl)  &optional start &key (end nil))
  (let* ((base (car (get-spl x)))
         (vps
          (make-instance
            'spl :the-list
            (get-spl (mirror 
                      (make-instance 'cil :the-list (get-cil x)))
                     :reference base))))
    (if start
      (if end
        (transpose vps (car (pch->itvl (list (car (last (the-list vps))) start))))
        (transpose vps (car (pch->itvl (list (car (the-list vps)) start)))))
      vps)))

(defmethod mirror ((x vps) &optional start  &key (end nil))
  (declare (ignore start end))
  (error "WANNA A SPL OR A CIL, MISTER: ~a" x))
;---------------------------------------------------------------------------------


;---------------------------------------------------------------------------------
(defmethod merge_vps ((x spl) (y spl) &optional (tolerance 1))
  (make-instance 'spl :the-list
                 (spl-remove-unissons
                  (sort (copy-list
                         (append (the-list x) (the-list y)))
                        #'< :key #'pch->fq) tolerance))
  )

(defmethod merge_vps ((x fql) (y fql) &optional (threshold 1.0001))
  (if (null(amplitudes x))
    (make-instance 'fql :the-list
                   (fql-remove-unissons
                    (sort (copy-list(append (the-list x) (the-list y)))
                          #'< ) threshold))

    (if(null(amplitudes y))
      (error "MISSING AMPLITUDES IN ~a" y)
      (if(null(bwl x))
        (let* ((list-to-be-sorted (append
                                   (mapcar #'list (the-list x) (amplitudes x) )
                                   (mapcar #'list (the-list y) (amplitudes y) )))
               (sorted-list (sort (copy-list list-to-be-sorted) #'< :key #'car )))
          (make-instance 'fql :the-list (mapcar #'first sorted-list)
                         :amplitudes (mapcar #'second sorted-list)))
        (if(null(bwl y))
          (error "MISSING BANDWIDTHS IN ~a" y)
          (let* ((list-to-be-sorted (append
                                     (mapcar #'list (the-list x)
                                             (amplitudes x) (bwl x))
                                     (mapcar #'list (the-list y)
                                             (amplitudes y) (bwl y))))
                 (sorted-list (sort
                               (copy-list list-to-be-sorted) #'< :key #'car )))
             (make-instance 'fql :the-list (mapcar #'first sorted-list)
                           :amplitudes (mapcar #'second sorted-list)
                           :bwl (mapcar #'third sorted-list)
                           )))))))

;---------------------------------------------------------------------------------
; UTILITIES FOR MERGE
(defun spl-remove-unissons (l &optional (tolerance 1))
  (let ((result nil))
    (push (pop l) result)
    (loop while l
          do (let ((int (car (pch->semitones (list (car result) (car l))))))
               (if (> int (/ tolerance 100))
                 (push (pop l) result)
                 (pop l))))
    (reverse result)))

(defun fql-remove-unissons (l &optional (seuil 1.0001 ))
  (let ((result nil))
    (push (pop l) result)
    (loop while l
          do (let ((ratio (/  (car l) (car result))))

;(format t "car l: ~a, car result: ~a, ratio: ~a~%" (car l) (car result)
;        (/  (car l) (car result)))
               (if (> ratio seuil)
                 (push (pop l) result)
                 (pop l))))
    (reverse result)))

;---------------------------------------------------------------------------------
(defmethod remove-octaves ((x spl) &key (tolerance 1) (from-bottom nil))
  (if (octave-p_vps x)
    (let ((l (get-spl x))
          (result nil))
      (if from-bottom (setf l (reverse l)))
      (loop while l
            do (push (pop l) result)
            do (loop for y in l
                     do (let ((semitones
                               (car (midi->semitones (list (pch->midi y) 
                                                           (pch->midi
                                                            (car result)))))))
                          (if (< (abs(mod semitones 12)) (/ tolerance 100))
                            (progn (pop result)(return))))))
      (if (not from-bottom) (setf result (reverse result)))
      (make-instance 'spl :the-list  result))
    x))  ; THORNY BUG: IF NO OCTAVES IT RETURNED NIL!
;---------------------------------------------------------------------------------

;---------------------------------------------------------------------------------
; PREDICATS POUR TOUTES LES VPS

; (octave-p_vps vps [threshold])
; Returns t if an octave exists in the VPS even if separated by many octaves.
; The test checks that the octave falls within the threshold (default = 5 cent),
;    including the value of the threshold.

(defmethod octave-p_vps ((x vps) &optional (seuil 0.05))
" Return nil if there are no octaves in the VPS
  Seuil is in semitones "
  (member-if (lambda (y) (<= (abs (mod y 12)) seuil))
             (get-gil x :midi t) ))


; (itvl-mod-p_vps vps itvl [threshold])
; Returns t if the interval "itvl" exists in the VPS even if separated by many
;    octaves.
; The test checks that the octave falls within the threshold (default = 5 cent),
;    including the value of the threshold.
; REMARK: to avoid bizzarre behaviors, the intervals should be notated in one of
;            the following ways: '6+, '(6+ 0) or '(6+ 0 5).
; The testing interval should be inferior to one octave.

(defmethod itvl-mod-p_vps ((x vps) itvl &optional (seuil 0.05))
  " Return nil if the interval does not appear in the VPS even if octaves apart"
  (let ((itvl (if (interval-p itvl)
                
                ; If argument looks like '(6- 0), itvl->semitones will take it as a list
                ;    of intervals, unless argument is in a list.
                
                (if (and (listp itvl) (cdr itvl))
                  (car (itvl->semitones (list itvl)))
                  (car (itvl->semitones itvl)))
                itvl)))
    (when (> itvl 12.0)
      (error "I ONLY WORK WITH INTERVALS < ONE OCTAVE: ~a" itvl))
    (member-if (lambda (y) (< (abs (- (mod y 12) itvl)) seuil))
               (get-gil x :midi t) )))


; (itvl-mod-p_vps vps itvl-list [threshold])
; Returns t if the at least one of the absolute intervals contained in itvl-list
;     exists in the VPS. This test does not look for intervals across octaves.
; The test checks that the octave falls within the threshold (default = 5 cent),
;    including the value of the threshold.
; The testing interval can be any interval in intervallic notation or in semitones.

(defmethod itvl-p_vps ((x vps) itvl-list &optional (seuil 0.05 ))
  " Return t if any of the intervals appears in the VPS "
  (let* ((curr-itvl (car itvl-list))
         (itvl (if (interval-p curr-itvl)
                 (car (itvl->semitones (list curr-itvl))) 
                 curr-itvl))
         (result))
    
    (if (null (cdr itvl-list))
      (setf result
            (member-if (lambda (y) (< (abs (- y itvl)) seuil)
                               (< (abs (- y itvl)) seuil))
                       (get-gil x :midi t)))
      (setf result
            (cons (itvl-p_vps x (list (car itvl-list)) seuil)
                  (itvl-p_vps x (cdr itvl-list) seuil))))
      (flat result)))


; (itvl-mod-p_vps vps itvl [threshold])
; Returns t if the interval "itvl" exists in the VPS's CIL.
; The test checks that the octave falls within the threshold (default = 5 cent),
;    including the value of the threshold.
; REMARK: to avoid bizzarre behaviors, the intervals should be notated in one of
;            the following ways: '6+, '(6+ 0) or '(6+ 0 5) or in semitones.

(defmethod itvl-cil-p_vps ((x vps) itvl &optional (seuil 0.05 ))
" Return nil if the interval does not appear in the VPS's CIL"
  (let ((itvl (if (interval-p itvl)

; If argument looks like '(6- 0), itvl->semitones will take it as a list
;    of intervals, unless argument is in a list.

                (if (and (listp itvl) (cdr itvl))
                  (car (itvl->semitones (list itvl)))
                  (car (itvl->semitones itvl)))
                itvl)))
    (member-if (lambda (y) (< (abs (- y itvl)) seuil))
               (get-gil x :midi t) )))

;---------------------------------------------------------------------------------



;---------------------------------------------------------------------------------
; (stretch_vps vps [:reference :offset :stretching :random])
; (stretch_vps vps [:reference :offset :stretching :random])
; Returns another SPL with stretching.
; Meaning: Reference: reference where to start to strech [Hz or Pitch]
;                        (default = first item of the list)
;           Offset: offset linearly added to each partial of the VPS [% of F0]
;                   (default = 0). An offset of 0.059 means a shift of a semitone.
;           Stretching: Stretching/Compressing factor (default = 2).
;                       [<1.0 = descending frequencies with respect to F0
;                         1.0 = all the frequencies = F0
;                         1.0/2.0 = compressed frequencies
;                        >2.0 = stretched frequencies]
;           Random: random variation added to the computed frequency [0-1]
;                       (default = 0). Attention: a high value for random might
;                       generate frequencies that are no longer in order

;REMARK: the keyword ":reference" is valid only for SPL's.

(defmethod stretch_vps ((x spl) &key (reference (first (get-fql x)))
                         (offset 0) (stretching 2) (random 0))
 (let ((result 
        (get-spl (stretch_vps
                  (make-instance 'arl
                    :the-list (get-arl x :reference (pch->fq reference))
                                 :reference (pch->fq reference))
                  :offset offset :stretching stretching :random random))))
   (make-instance 'spl :the-list result)))

(defmethod stretch_vps ((x fql) &key (offset 0) (stretching 2) (random 0)
                        (reference nil))
  (when reference
    (format t "STRETCH_VPS~%  WARNING: THE KEYWORD REFERENCE IS USED ONLY WITH SPL'S NOT TAKEN INTO ACCOUNT HERE: ~a~%  VPS: ~a~%" reference x))
  ;spsht is in sys:dg:spectrum.lisp
  (let* ((ref (first (get-fql x)))
         (result (spsht  ref (get-arl x :reference ref) offset stretching random)))
    (unless (equal random 0) (setf result (sort (copy-list result) #'<)))
    (make-instance 'fql :the-list result
                   :amplitudes (get-amp x))))

(defmethod stretch_vps ((x arl) &key (offset 0) (stretching 2) (random 0)
                           (reference nil))
  (when reference
    (format t "STRETCH_VPS~%  WARNING: THE KEYWORD REFERENCE IS USED ONLY WITH SPL'S NOT TAKEN INTO ACCOUNT HERE: ~a~%  VPS: ~a~%" reference x))
  ;spsht is in sys:dg:spectrum.lisp
  (let ((result (spsht (reference x) (the-list x) offset stretching random)))
    (unless (equal random 0) (setf result (sort (copy-list result) #'<)))
    (make-instance 'arl :the-list (get-arl
                                   (make-instance 'fql :the-list result)
                                   :reference (reference x))
                   :reference (reference x))))

(defmethod stretch_vps ((x ptl) &key (offset 0) (stretching 2) (random 0)
                           (reference nil))
  (when reference
    (format t "STRETCH_VPS~%  WARNING: THE KEYWORD REFERENCE IS USED ONLY WITH SPL'S NOT TAKEN INTO ACCOUNT HERE: ~a~%  VPS: ~a~%" reference x))
  (let* ((ref (first (get-fql x)))
         (result (spsht ref (get-arl x :reference ref) offset stretching random)))
    (unless (equal random 0) (setf result (sort (copy-list result) #'<)))
    (make-instance 'ptl :the-list result
                   :amplitudes (get-amp x)
                   :entry-delays (entry-delays x)
                   :durs (durs x)
                   :transp_funs (transp_funs x)
                   :amp_funs (amp_funs x))))



;---------------------------------------------------------------------------------
; (main-partials spectrum :diapason :max-nn :threshold)
; Reduce the frequencies of a FQL depending on certain criteria.
;    :max-nn: keep a maximum number of frequencies (default = *MAX-NN*)
;    :threshold: keep only the frequencies => threshold [abs amp]
;                   (default = nil -> use max-nn)
;    :diapason: reference diapason when using fq->pch (default = nil)
;REMARK: this only works when the ampitudes are specified

(defmethod main-partials ((x spectrum) &key reference diapason
                          (max-nn *MAX-NN*)(threshold nil) &allow-other-keys)
  ;threshold : lineaire et absolu
  ;thresholdB : en dB et relatif ˆ max-amp (doit etre negatif) MANQUE!!!!
  (declare(ignore reference))
  (if (null (fql x))(error "MISSING ABSOLUTE FREQUENCY in ~a~%" (class-of x)))
  (let((fq-and-amps nil)
       (max-nn (round max-nn))) ;ms_1109, allow floating pointers
    (if(and(<=(length (fql x)) max-nn)(null threshold))
        x                                       ;return itself
      (progn 
        (if(null diapason)
            (fq->pch (fql x))
          (fq->pch (fql x) diapason))
        (setf fq-and-amps (mapcar #'cons
                                  (copy-list (fql x))(copy-list (get-amp x))))
        (if(null (get-amp x))
            (error "CANNOT PERFORM DATA REDUCTION, MISSING AMPLITUDES"))
        (if (null threshold)
            (progn
              (format t "SELECTING ~a MAIN PARTIALS AMIDST ~a~%"
                      max-nn (length (fql x)))
              (setf fq-and-amps(butlast (sort fq-and-amps  #'> :key #'cdr)
                                        (- (length (fql x))  max-nn))))
          (progn
            (setf fq-and-amps(delete-if #'(lambda (y) (< (cdr y) threshold))
                                        fq-and-amps))
            (format t "SELECTING ~a PARTIALS LOUDER THAN ~a~%"
                    (length fq-and-amps) threshold) ))
        (if fq-and-amps
            (progn
              (setf fq-and-amps (sort fq-and-amps  #'< :key #'car))
              (make-instance 'fql :the-list (mapcar #'car fq-and-amps)
                             :amplitudes (mapcar #'cdr fq-and-amps)))
          nil                               ;return empty vps
          ))
      )))


(defmethod main-partials ((x ptl) &key reference diapason
                             (max-nn *MAX-NN*)(threshold nil)&allow-other-keys)
  ;threshold : lineaire et absolu
  (declare(ignore reference))
  (if (null (fql x))(error "MISSING ABSOLUTE FREQUENCY in ~a~%"(class-of x)))
  (let((max-nn (round max-nn))) ;ms_1109, allow floating pointers
 
   (if(and(<=(length (fql x)) max-nn)(null threshold))
       x                                       ;return itself
     (progn 
       (if(null diapason)
           (fq->pch (fql x))
         (fq->pch (fql x) diapason))
       (let ((fq-and-all (mapcar #'list
                                 (copy-list (get-amp x))
                                 (copy-list (fql x))
                                 (copy-list (entry-delays x))
                                 (copy-list (durs x))
                                 (copy-list (transp_funs x))
                                 (copy-list (amp_funs x)))))
         (if(null (get-amp x))
             (error "CANNOT PERFORM DATA REDUCTION, MISSING AMPLITUDES"))
         (if (null threshold)
             (progn
               (format t "SELECTING ~a MAIN PARTIALS AMIDST ~a~%"
                       max-nn (length (fql x)))
               (setf fq-and-all(butlast (sort fq-and-all  #'> :key #'car)
                                        (- (length (fql x))  max-nn))))
           (progn
             (setf fq-and-all(delete-if #'(lambda (y) (< (car y) threshold))
                                        fq-and-all))
             (format t "SELECTING ~a PARTIALS LOUDER THAN ~a~%"
                     (length fq-and-all) threshold) ))
         (if fq-and-all
             (progn
               (setf fq-and-all (sort fq-and-all  #'< :key #'second))
               (make-instance 'ptl :the-list (mapcar #'second fq-and-all)
                              :amplitudes (mapcar #'first fq-and-all)
                              :entry-delays (mapcar #'third fq-and-all)
                              :durs (mapcar #'fourth fq-and-all)
                              :transp_funs (mapcar #'fifth fq-and-all)
                              :amp_funs (mapcar #'sixth fq-and-all)))
           nil                               ;return empty vps
           ))))))
;---------------------------------------------------------------------------------

  
#|
;TESTS :
(setf spl1 (make-instance 'spl :the-list '(sol2 sold2 (do3 . 2))))
(setf spl2 (make-instance 'spl :the-list '(Fad1 DOd2 sol2 DO3 MI3 do5)))
(setf spl3 (make_vps '(DO3 MI3 SIB3 RE4 MI4 FAD4 SI4)))
(setf spl4 (make_vps '(DO3 MI3 SIB3 RE4 MIB4 SOL4 DOD5)))
(setf spl5 (make_vps '(DO3 (MI3 . 12) SIB3 RE4 (MIB4 . -q) SOL4 DOD5)))


(setf fql1 (make-instance 'fql :the-list '(60 61 150 1000 1222.1)))
(setf fql2 (make-instance 'fql :the-list '(61.5 125 150 1222)))
(setf (amplitudes fql1) '(1. 2. 0.5 3 4))
(apply-gain fql1 :gain 2.)

(setf spl1 (revert spl1 ))

(setf spl1 (transpose spl1 '(7+ 0 0)))
(setf fql (transpose fql 1.5))
(setf cil (mirror cil))
(setf spl2 (mirror spl2))

(octave-p_vps spl3)
(octave-p_vps spl4)

(itvl-mod-p_vps spl4 '3-)
(itvl-mod-p_vps spl3 '(3- 0))
(itvl-mod-p_vps spl5 '(3+ 0 12))
(itvl-mod-p_vps spl5 '(3+ 0 6) 0.07)

(itvl-p_vps spl1 '(6+ 7-))

(itvl-cil-p_vps spl1 '2-)

(get-spl spl1)
(get-spl spl2)
(setf spl3 (merge_vps spl1 spl2 :tolerance 50))
(get-spl spl3)

(setf fql3 (merge_vps fql2 fql1 ))
(get-fql fql3)
(bwl fql3)

(setf spl4(remove-octaves spl2))
(setf spl4(remove-octaves spl2 :tolerance 201))
(setf spl5(remove-octaves spl2 :from-bottom t) )
(get-spl spl4)
(get-spl spl5)

(setf arl(make-instance 'arl :reference 100. :the-list '(.5 1 2 3 4)))
(get-fql arl)
(setf arl(stretch_vps arl))
(setf arl(stretch_vps arl :random 1))
(setf arl(stretch_vps arl :stretching 2.01))
(setf arl(stretch_vps arl :offset 4))
(setf spl2 (stretch_vps spl :random 1))
(get-spl spl2)

(setf fql (stretch_vps fql :stretching 1.9))
(setf fql (stretch_vps fql :random 1))
(get-fql fql)


|#
