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
;=====================================================

(in-package :cr)

;--------------------
;CLASSE
;--------------------
(defclass model-cseq
  (chroma-model)
  (
   (fql-list :accessor fql-list
             :initform ()
             :initarg :fql-list 
             :documentation "list of FQL objects")
   )
  (:documentation "Chord Seq Model"))

;--------------------
;METHODES
;--------------------

(defmethod initialize-instance :after ((x model-cseq) &rest initargs
                                           &key fql-list cseq add formant markers (sort t) (weighed-avg nil)(threshold nil)
                                           (threshmod 'fql)(durmin 0.0))
  (declare (ignore initargs))
  (let ((analysis nil))
    (if fql-list
      (check-number-of-regions x))
    (cond 
     (add (setf analysis add))
     (cseq(setf analysis cseq))
     (formant(setf analysis formant)))
    (when analysis
      (compute-model x analysis markers :sort sort :weighed-avg weighed-avg :durmin durmin))
    (if threshold 
      (model-threshold x (get-max-amp analysis) :threshold threshold :threshmod threshmod))
    ))

(defmethod get-max-amp ((x model-cseq))
  (let ((l (mapcar #'(lambda (y)(get-max-amp y))(fql-list x))))
    (max- l)))
(defmethod get-min-amp ((x model-cseq))
  (let ((l (mapcar #'(lambda (y)(get-min-amp y))(fql-list x))))
    (min- l)))
(defmethod get-max-bw ((x model-cseq))
  (let ((l (mapcar #'(lambda (y)(when y (get-max-bw y)))(fql-list x))))
    (max-  l)))
(defmethod get-min-bw ((x model-cseq))
  (let ((l (mapcar #'(lambda (y)(when y (get-min-bw y)))(fql-list x))))
    (min- l)))
(defmethod get-max-fq ((x model-cseq) &key  &allow-other-keys)
  (let ((l (mapcar #'(lambda (y)(get-max-fq y))(fql-list x))))
    (max- l)))
(defmethod get-min-fq ((x model-cseq) &key  &allow-other-keys)
  (let ((l (mapcar #'(lambda (y)(get-min-fq y))(fql-list x))))
    (min- l)))

(defmethod get-norm-amp ((x model-cseq) rang &optional (scaling-factor 1))
"Return the amplitudes of the rang'th VPS's normalized with respect to the whole model.
There are as many ranks as events in a model"
(let ((thefql (nth rang (fql-list x))))
  (if thefql
    (mapcar #'(lambda (y) (/ y (/ (get-max-amp x) scaling-factor ))) (get-amp thefql))
    nil)))

;si time > dernier marker get-fql-from-time retourne nil
(defmethod get-fql-from-time ((x model-cseq) time)
"Return the closest VPS to time or nil if there is none"
  (let ((vps-number (length(member-if (lambda (y) (< time y)) (markers x)))))
    (car (last (fql-list x) vps-number))))

(defmethod copy-model ((x model-cseq))
"Return a copy of the model"
  (make-instance 'model-cseq :markers (copy-tree (markers x)) :fql-list (mapcar #'om::copy-instance  (fql-list x))))


;;;;;;;;;;;;;;;;;;;;;;;
;Private methods :

(defmethod compute-model :before  ((x model-cseq) (y additive-data) markers &key &allow-other-keys)
  (set_markers_to_closest_frame x y markers))

(defmethod compute-model :after  ((x model-cseq) (y additive-data) markers &key &allow-other-keys)
  (declare (ignore markers))
  (restore_markers x))

(defmethod compute-model ((x model-cseq) (y additive-data) markers
                             &key sort weighed-avg durmin)
    (if (not markers) (warn "missing markers"))
    (let ((fql-list nil))
       (loop for (a b) on (markers x) by #'cdr
            do (let ((fql (extract-fql x y a b :sort sort :weighed-avg weighed-avg :durmin durmin)))
                 (if fql (push fql fql-list))))
      (setf (fql-list x) (nreverse fql-list))))

(defmethod extract-fql ((x model-cseq) (y additive-data) a b
                             &key sort weighed-avg durmin)
  (let (npartials freq_moyenne max_amp freq_fun amp_fun time_list triplets subdata dur
                  (amps nil)
                  (freqs nil))
    (when b
      (when (get-gbl 'prnflg)
        (format t "markers-> ~a~%" (list (cdr a) (cdr b))))
    ;  (setf subdata (subseq (data y) (car a) (1+ (car b))))
      (setf subdata (subseq (data y) (car a) (car b)))
      ;pour chaque paire de marker
      (setf npartials0 (nombre_de_partiels_max subdata))
      (setf npartials (numero_de_partiels_max subdata))
     (when (get-gbl 'prnflg)
        (format t "~a partiels (~a ) ~%" npartials npartials0))
      ;pour chaque partiel
      (loop for i from 1 to npartials
            initially (setf amps nil freqs nil)
            do
            (when (get-gbl 'prnflg)
            (format t "partiel n ~a~%" i))
            ;moyenne pondŽrŽe frequence
            (setf freq_fun (get_freq_fun subdata i))
             ;amplitude moyenne
            (setf amp_fun (get_amp_fun subdata i))
            ;temps (X des fun)
            (setf time_list (get_add_time subdata))
            (setf dur (- (car (last time_list))(first time_list)))
            (when (> dur durmin)
              (progn
                (setf triplets (mapcar #'list amp_fun time_list freq_fun))
                (setf triplets (remove nil triplets :key 'car) ) ; ou delete ??
                (when (> (length triplets) 1)
                  ;amplitude moyenne
                  (setf amp_fun (mapcar #'first triplets))
                  (setf max_amp (apply #'max amp_fun))
                  (setf amp_fun (mapcar #'(lambda (x) (/ x max_amp)) amp_fun))
                  ;frequence moyenne
                  (setf freq_fun  (mapcar #'third triplets))
                  (setf freq_moyenne (if weighed-avg 
                                       (moyenne_ponderee freq_fun amp_fun)
                                       (moyenne freq_fun)))
                  (push freq_moyenne freqs)
                  (push max_amp amps)
                  ))))
      (if (not (null freqs))
        (progn (if sort (multiple-value-setq (freqs amps) (sort-partiels freqs amps)))
               (make-instance 'fql 
                 :the-list (nreverse freqs) 
                 :amplitudes (nreverse amps))
               )))))

(defmethod extract-fql ((x model-cseq) (y formant-data) a b
                             &key sort weighed-avg durmin)
  (let (npartials freq_moyenne max_amp bw_moyenne freq_fun amp_fun bw_fun time_list triplets subdata dur
                  (amps nil) (freqs nil) (bws nil))
    (when b
      (when (get-gbl 'prnflg)
        (format t "markers-> ~a~%" (list (cdr a) (cdr b))))
      (setf subdata (subseq (data y) (car a) (1+ (car b))))
      ;pour chaque paire de marker
      (setf npartials (nombre_de_partiels_max subdata))
      (when (get-gbl 'prnflg)
        (format t "~a partiels~%" npartials))
      ;pour chaque partiel
      (loop for i from 1 to npartials
            initially (setf amps nil freqs nil bws nil)
            do
            ;(when (get-gbl 'prnflg)
            ;(format t "partiel n ~a~%" i))
            ;moyenne pondŽrŽe frequence
            (setf freq_fun (get_freq_fun subdata i))
             ;amplitude moyenne
            (setf amp_fun (get_amp_fun subdata i))
            ;bw moyenne
            (setf bw_fun (get_bw_fun subdata i))
            ;temps (X des fun)
            (setf time_list (get_add_time subdata))
            (setf dur (- (car (last time_list))(first time_list)))
            (when (> dur durmin)
              (progn
                (setf triplets (mapcar #'list amp_fun time_list freq_fun bw_fun))
                (setf triplets (remove nil triplets :key 'car) ) ; ou delete ??
                (when (> (length triplets) 1)
                  ;amplitude moyenne
                  (setf amp_fun (mapcar #'first triplets))
                  (setf max_amp (apply #'max amp_fun))
                  (setf amp_fun (mapcar #'(lambda (x) (/ x max_amp)) amp_fun))
                  (push max_amp amps)
                  ;frequence moyenne
                  (setf freq_fun  (mapcar #'third triplets))
                  (setf freq_moyenne (if weighed-avg 
                                       (moyenne_ponderee freq_fun amp_fun)
                                       (moyenne freq_fun)))
                  (push freq_moyenne freqs)
                  ;bw moyenne
                  (setf bw_fun  (mapcar #'fourth triplets))
                  (setf bw_moyenne (moyenne bw_fun))
                  (push bw_moyenne bws)                 
                  ))))
      (if (not (null freqs))
        (progn (if sort (multiple-value-setq (freqs amps) (sort-partiels freqs amps)))
               (make-instance 'fql 
                 :the-list (nreverse freqs) 
                 :amplitudes (nreverse amps)
                 :bwl (nreverse bws)))
        ))))


(defmethod compute-model ((x model-cseq) (y cseq-data) markers &key &allow-other-keys)
(declare (ignore markers))  
(let* (time-end (freqs ())(amps ())(fql-list nil)(markers-list nil)
                  (list (cddr (data y)))
                  (time  (third (car list)))
                  )
     (loop for curr-list in list 
          do(let ((new-time (third curr-list)))
              (if(equal  new-time time)
                (progn(push (om::db->lin (fifth curr-list)) amps)
                      (push (fourth curr-list) freqs))
                (progn (push (make-instance 'fql :the-list (nreverse freqs) :amplitudes (nreverse amps)) fql-list)
                       (push time markers-list)
                       (setf time-end (sixth curr-list))
                       (setf freqs nil)
                       (push (fourth curr-list) freqs)
                       (setf amps nil)
                       (push (om::db->lin (fifth curr-list)) amps)
                       ))
              (setf time new-time)))
    (push time markers-list)
    (push time-end markers-list)
    (push (make-instance 'fql :the-list (nreverse freqs) :amplitudes (nreverse amps))fql-list)
    (setf (markers x) (nreverse markers-list))
    (setf (fql-list x) (nreverse fql-list))
  ))


(defmethod model-threshold ((x model-cseq) analysis-max-amp &key threshold threshmod)
  (let ((fql-list nil)(thresh threshold))
    (loop for thefql in (fql-list x)
          do (let (( triplets (mapcar #'list (amplitudes thefql) (the-list thefql))))
               (case threshmod
                 (abs ())
                 (rel (setf thresh (+ (lin->db analysis-max-amp) threshold)))
                 (fql (setf thresh (+ (lin->db (get-max-amp thefql)) threshold)))
                 (otherwise (warn "unknown threshold mode")))
               (setf triplets (seuillage_cseq triplets thresh))
               (if triplets  
                 (push (make-instance 'fql 
                         :the-list (mapcar #'second triplets)
                         :amplitudes (mapcar #'first triplets))
                       fql-list)
                 (push nil fql-list)))
          finally (setf (fql-list x) (nreverse fql-list)))))

(defmethod set_markers_to_closest_frame ((x model-cseq) (a analysis-data) markers)
  (let*((result nil)
       (f (get_add_time (data a)))
       (f (subseq f 0 (- (length f) 2))) ; attention ˆ la derniere trame d'analyse !
       )
    (loop for mark in markers
          do (let*(( diff_l (mapcar #'(lambda (x) (abs (- x mark))) f))
                   (min (apply #'min diff_l))
                   (pos (position min diff_l)))
               (push (cons pos (nth pos f)) result)))
    (setf result (remove-duplicates result :test #'eq :key #'car ))
    (setf (markers x) (sort result #'< :key #'car))
    ))

(defmethod restore_markers ((x model-cseq))
  (setf (markers x) (mapcar #'cdr (markers x))))

(defmethod check-number-of-regions ((x model-cseq))
  (unless (listp (car (markers x))) ;;??????????????
    (unless (equal (length (markers x)) (1+ (length (fql-list x))))
      (error "THERE SHOULD BE ONE MARKER MORE THAN FQLS~%      Markers = ~a,fqls = ~a~%"
             (length (markers x))(length (fql-list x))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;MODELES - MISC FUNCTIONS


(defun get_add_time (data)
  (mapcar #'cadr (mapcar #'car data)))



#|
(defun localthreshold (amp_fun thresh)
  (let ((amp_fun (remove nil amp_fun)))
    (if amp_fun
      (+ thresh (lin->db(apply #'max amp_fun)))
      ())))


|#