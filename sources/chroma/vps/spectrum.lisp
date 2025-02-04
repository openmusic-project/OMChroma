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

(om::defclass! SPECTRUM (VPS)
  ((amplitudes :initform nil
               :initarg :amplitudes
               :accessor amplitudes
               :type list
               :reader get-amp
               :documentation "List of Amplitudes")
   (bwl :initform nil
        :initarg :bwl
        :accessor bwl
        :type list
        :reader get-bw
        :documentation "List of Bandwidths"))
  (:documentation "Spectrum" )
  (:icon 625))


(defmethod check-amp ((x spectrum))
  (when (not (null (amplitudes x)))
  ;  (if (not (eq (length (amplitudes x)) (length (fql x))))
  ;      (error (concatenate 'string "Error in " (string (type-of x)) ": not the same number of FREQUENCIES and AMPLITUDES.")))
    (when (< (reduce #'min (amplitudes x) :initial-value MOST-POSITIVE-LONG-FLOAT) 0)
      ;; amplitudes in dB
      (when (> (reduce #'max (amplitudes x) :initial-value MOST-NEGATIVE-LONG-FLOAT) 0)
        (print (concatenate 'string "Warning positive AND negative values found in amplitudes lists of " (string (type-of x)) "."))
        (print "Positive values will be set to 0 dB before conversion into absolute amplitudes"))
      (loop for a in (amplitudes x) for i = 0 then (+ i 1) do
            ;(when (> a 0) (setf (nth i (amplitudes x)) 0))
            (setf (nth i (amplitudes x)) (dbtolin (min 0 a)))
            )))
  ;quand il n'y a pas d'amplitudes
  (when (null (amplitudes x))
  (let ((lgt (length (fql x))))
    (setf (amplitudes x) (om::repeat-n 0.9 lgt)))))

;form 5.2
#|
(defmethod check-amp ((x spectrum)) )
  (if(not(null (amplitudes x)))
    (if(not(eq(length(amplitudes x))(length(fql x))))
      (error "pas le meme nombre d'amplitudes et de frequences"))))
|#


(defmethod check-bw ((x spectrum))
  (when (not (null (bwl x)))
      (if (not (eq (length (bwl x)) (length (fql x))))
         ; (error (concatenate 'string "Error in " (string (type-of x)) ": not the same number of FREQUENCIES and BANDWIDTHS"))
 ))
  (when (null (bwl x))
  (let ((lgt (length (fql x))))
    (setf (bwl x) (om::repeat-n 0.9 lgt)))))


(defmethod get-max-amp ((x spectrum))
  (let ((l (amplitudes x)))
    (if (not (null l)) (apply #'max l))))

(defmethod get-min-amp ((x spectrum))
  (let ((l (amplitudes x)))
    (if (not (null l)) (apply #'min l))))

(defmethod get-max-bw ((x spectrum))
  (let ((l (bwl x)))
    (if (not (null l)) (apply #'max l))))

(defmethod get-min-bw ((x spectrum))
  (let ((l (bwl x)))
    (if (not (null l)) (apply #'min l))))

(defmethod apply-gain ((x spectrum) gain &key &allow-other-keys)
  "Multiply the amplitudes by the value of gain"
  (setf (amplitudes x) (mapcar #'(lambda (x) (* gain x)) (amplitudes x))))

(defmethod get-spl ((x spectrum)  &key reference approx (max-nn *MAX-NN*)
                           (threshold nil) &allow-other-keys)
  ;threshold : lineaire et absolu
  ;thresholdB : en dB et relatif  max-amp (doit etre negatif)
  (declare(ignore reference))
  (if (null (fql x))(error "MISSING ABSOLUTE FREQUENCY in ~a~%"(class-of x)))
  (if(and(<=(length (fql x))max-nn)(null threshold))
    (if(null approx)
      (fq->pch (fql x))
      (fq->pch (fql x) approx))
    (let ((fq-and-amps (mapcar #'cons (copy-list (fql x))
                               (copy-list (get-amp x)))))
      (if(null (get-amp x))
        (error "CANNOT PERFORM DATA REDUCTION, MISSING AMPLITUDES"))
      (print "PERFORMING DATA REDUCTION")
      (if (null threshold)
        (setf fq-and-amps(butlast (sort fq-and-amps  #'> :key #'cdr)
                                  (- (length (fql x))  max-nn)))
        (setf fq-and-amps(delete-if #'(lambda (y) (< (cdr y) threshold))
                                    fq-and-amps)))
      (setf fq-and-amps (sort fq-and-amps  #'< :key #'car))
      (if(null approx)
        
        ; if threshold is too high, no freqs will be in the list. Marco, 980524
        (when fq-and-amps (fq->pch (mapcar #'car fq-and-amps)))
        (when fq-and-amps (fq->pch (mapcar #'car fq-and-amps) approx)))
      )))

(defmethod get-rpl ((x spectrum) &key reference approx (max-nn *MAX-NN*)
                      threshold)
  (get-rpl (make-instance 'spl
             :the-list
             (get-spl x
                      :reference reference :approx approx
                      :threshold threshold :max-nn max-nn))))

(defmethod get-cil ((x spectrum) &key  approx (max-nn *MAX-NN*) threshold)
  (get-cil (make-instance 'spl
             :the-list (get-spl x :reference 'la4 :approx approx
                                :threshold threshold :max-nn max-nn))))

(defmethod get-ail ((x spectrum) &key reference approx (max-nn *MAX-NN*)
                      threshold &allow-other-keys)
  (get-ail (make-instance 'spl
             :the-list (get-spl x :approx approx :threshold threshold
                                :max-nn max-nn)):reference reference))


(defmethod reorder-vps ((x spectrum))  
  (let ((list (mapcar #'list (get-fql x))))
    
    (when (get-amp x)
      (setf list (mapcar #'(lambda (a b) (append a (list b))) list (get-amp x))))
    (when (get-bw x)
      (setf list (mapcar #'(lambda (a b) (append a (list b))) list (get-bw x))))
    
    (setf list (sort list '< :key 'first))

    (setf list 
          (loop for item in (remove-duplicates list :test '= :key 'first)
                collect (if (= 1 (count (car item) list :test '= :key 'car))
                            item
                          (remove nil (list (car item)
                                            (when (remove nil (mapcar #'second list))
                                              (loop for dup in list when (and (cadr dup) (= (car dup) (car item))) 
                                                  maximize (cadr dup)))
                                            (when (remove nil (mapcar #'third list))
                                              (loop for dup in list when (and (caddr dup) (= (car dup) (car item))) 
                                                    maximize (caddr dup))))))))
   
   (initialize-instance x :the-list (remove nil (mapcar #'first list))
                         :amplitudes (remove nil (mapcar #'second list))
                        :bwl (remove nil (mapcar #'third list)))
   
   (print "--> SPECTRUM reordered OK.")
   ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;FQL;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(om::defclass! FQL (SPECTRUM)
  ()
  (:documentation "Frequencies List")
  (:icon 621))

(defmethod set-freq-list ((x fql))
  (if (the-list x)
      (setf (fql x) (the-list x))
    (setf (the-list x) (fql x))))

(defmethod initialize-instance :after ((x fql) &rest initargs)
  (declare (ignore initargs))
  (check-syntax x)
  (set-freq-list x)
  (check-amp x)
  (check-bw x)
  (check-order x))

(defmethod check-syntax ((x fql))
  (loop for f in (the-list x)
        when (not (numberp f))
        do (error "WRONG FREQUENCY ~a" f)))

(defmethod get-harmonicity ((x fql) &key (f0 ()) (expanded nil)
                               &allow-other-keys)
  (if(null f0)(setf f0 (car (copy-list (fql x)))))
  (let ((result (mapcar #'(lambda (y) (harm-dist-f (pch->fq f0) y)) (fql x))))
    (if expanded result
        (/ (apply #'+ result) (length result)))))

(defmethod get-virt-fund ((x fql) &key (grid-ratio .001)  &allow-other-keys)
  (fond-virt-f (fql x) grid-ratio))

(defmethod print_vs ((x fql))
  (format t "Vertical Structure :~%~a    (~a)
    Frequencies: ~a
    Amplitudes: ~a
    Bandwidths: ~a~%"
          (class-of x)
          (documentation (class-name (class-of x))'type)
          (slot-value x 'the-list)
          (slot-value x 'amplitudes)
          (slot-value x 'bwl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;CRL;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(om::defclass! CRL (SPECTRUM) 
  ()
  (:documentation "Contiguous Ratios List")
  (:icon 622))

(defmethod initialize-instance :after ((x crl) &rest initargs)
  (declare (ignore initargs))
  (check-syntax x)
  (check-order x)
  (check-amp x)
  (check-bw x))

(defmethod check-syntax ((x crl))
  (loop for f in (the-list x)
        when (not (and (numberp f) (> f 1.)))
        do (error "WRONG RATIO ~a" f))
  (get-fql x))

(defmethod get-spl :before ((x crl) &key reference approx
                              (max-nn *MAX-NN*) threshold &allow-other-keys)
(declare (ignore approx max-nn  threshold))
  (get-fql x :reference reference))

(defmethod get-fql :before ((x crl) &key (reference 100))
  (setf (fql x) (ratio->fq (the-list x) (pch->fq reference))))

(defmethod get-ail ((x crl) &key reference approx (max-nn *MAX-NN*) threshold &allow-other-keys)
  (get-ail (make-instance 'spl
                          :the-list (get-spl x :reference reference :approx approx
                                             :threshold threshold :max-nn max-nn))
           :reference reference))

(defmethod get-arl ((x crl) &key reference)
  (freqs-to-arl (get-fql x :reference reference) (pch->fq reference)))

(defmethod number-of-notes ((x crl))
  "Number of Notes"
  (1+ (length (the-list x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;ARL;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(om::defclass! ARL (anchored-vps SPECTRUM)
  ((reference :initform 100.0 :initarg :reference :accessor reference))
  (:documentation "Anchored Ratios List (Spectrum)")
 (:icon 623))

(defmethod initialize-instance :after ((x arl) &rest initargs)
  (declare (ignore initargs))
  (check-reference x)
  (check-syntax x)
  (check-order x)
  (check-amp x)
  (check-bw x))

(defmethod check-syntax ((x arl))
  (loop for f in (the-list x)
        when (not(numberp f))
        do (error "WRONG RATIO ~a" f))
  (setf (fql x)(arl-to-freq(the-list x)(pch->fq(reference x)))))

(defmethod get-spl :before ((x arl) &key approx &allow-other-keys)
  (get-fql x :approx approx))

(defmethod print_vs ((x arl))
  (format t "Vertical Structure :~%~a    (~a)
    Partial's numbers: ~a
    Reference: ~a~%"
          (class-of x)
          (documentation (class-name (class-of x))'type)
          (slot-value x 'the-list)
          (reference x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;PTL;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;vps-spectrum-fql extension for partials model
(om::defclass! PTL (FQL)  
  ((duration :initform 0
             :initarg :duration
             :accessor duration
             :type float
             :documentation "Duration [sec]")
   (origtime :initform 0
             :initarg :origtime
             :accessor origtime
             :type float
             :documentation "Original position [sec]")
   (e-dels :initform nil
                 :initarg :e-dels
                 :accessor e-dels
                 :type list
                 :documentation "List of entry-delays [sec}")
   (durs :initform nil
         :initarg :durs
         :accessor durs
         :type list
         :documentation "List of durs [sec]")
   (transp_funs :initform nil
                :initarg :transp_funs
                :accessor transp_funs
                :type list
                :documentation "List of transposition factor functions (not normalised)")
   (amp_funs :initform nil
             :initarg :amp_funs
             :accessor amp_funs
             :type list
             :documentation "List of amplitude scaler functions (max amp in the fql) (not normalised)"))
  (:documentation "FQL EXTENSION" )
  )


