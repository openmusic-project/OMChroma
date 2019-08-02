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

(in-package :chroma)

;--------------------
;CLASSES
;--------------------
(defclass model-partials 
  (model-cseq)
  (
   (ptl-list :accessor ptl-list
             :writer put-ptl-list
             :initform ()
             :initarg :ptl-list 
             :documentation "list of PTL objects")
   )
  (:documentation "Partials Model (functions)"))

(defmethod get-dev+ ((x ptl))
  (mapcar #'y-max_fun  (transp_funs x)))

(defmethod get-dev- ((x ptl))
  (mapcar #'y-min_fun  (transp_funs x)))

(defmethod fql-list ((x model-partials))
  (ptl-list x))

(defmethod copy-model ((x model-partials))
"Return a copy of the model"
  (make-instance 'model-partials :markers (copy-tree (markers x)) :ptl-list (mapcar #'copy-instance (ptl-list x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;PRIVATE METHODS :

(defmethod initialize-instance :after ((x model-partials) &rest initargs &key cseq )
  (declare (ignore initargs))
 ;(inherited !) (if add (compute-model x add markers :sort sort :weighed-avg weighed-avg :threshold threshold))
  (if cseq (error "partials from cseq_analysis = NOT YET ! ")))

(defmethod compute-model ((x model-partials)(y analysis-data) markers &key sort weighed-avg durmin)
  (declare (ignore markers))
  (let ((ptl-list nil))
    (loop for (a b) on (markers x) by #'cdr
          do (let ((ptl (extract-ptl x y a b :sort sort :weighed-avg weighed-avg :durmin durmin)))
               (if ptl (push ptl ptl-list))))
    (setf (ptl-list x) (nreverse ptl-list))))


(defmethod extract-ptl ((x model-partials) (y additive-data) a b
                             &key sort weighed-avg durmin)
 (let (npartials freq_moyenne max_amp freq_fun edel dur amp_fun time_list triplets subdata
                  (amps nil)
                  (edels nil)
                  (durs nil)
                  (freqs nil)
                  (amp_funs nil)
                  (freq_funs nil))
         (when b
          ;(format t "markers->~a~%" (list (cdr a) (cdr b)))
          (setf subdata (subseq(data y) (car a) (1+ (car b))))
          ;pour chaque paire de marker
          (setf npartials0 (nombre_de_partiels_max subdata))
          (setf npartials (numero_de_partiels_max subdata))s
          ;(format t "~a partiels (~a) ~%" npartials npartials0)
          ;pour chaque partiel
          (loop for i from 1 to npartials
                initially (setf amps nil freqs nil amp_funs nil freq_funs nil edels nil durs nil)
                do
                ;(format t "~%partiel n~a~%" i)
                ;moyenne pondŽrŽe frequence
                (setf freq_fun (get_freq_fun subdata i))
                ;amplitude moyenne
                (setf amp_fun (get_amp_fun subdata i))
                ;temps (X des fun)
                (setf time_list (get_add_time subdata))
                (setf triplets  (mapcar #'list amp_fun time_list freq_fun))
                ;delete breakpoints absents de l'analyse
                (setf triplets (delete nil triplets :key 'car))
                (when (> (length triplets) 1) ;nombre minimum de points dans les bpf (pourrait tre 2)
                  (setf time_list (mapcar #'second triplets))
                  (setf dur  (- (car (last time_list)) (first time_list)))
                  (when (> dur durmin)
                    (progn
                      ;amplitude maximum
                      (setf amp_fun (mapcar #'first triplets))
                      (setf max_amp (apply #'max amp_fun))
                      ;normalisation des fonctions d'amplitudes ??
                      ;(setf amp_fun (mapcar #'(lambda (x) (/ x max_amp)) amp_fun))
                      ;frequence moyenne
                      (setf freq_fun  (mapcar #'third triplets))
                      (setf freq_moyenne (if weighed-avg 
                                           (moyenne_ponderee freq_fun amp_fun)
                                           (moyenne freq_fun))) 
                      (push freq_moyenne freqs)
                      (push max_amp amps)
                      (setf freq_fun (mapcar #'(lambda (x) (ratio->semitones(/ x freq_moyenne))) freq_fun))
                      ;entry-delays
                      (setf edel (- (first time_list) (cdr a)))
                      (push edel edels)
                      (push dur durs)
                      ;normalisation des fonctions en temps
                      (setf freq_fun (make_fun(om::flat (mapcar #'(lambda (x y) (list x y)) freq_fun time_list))))
                      (X-resc_fun freq_fun 0 1)
                      (setf amp_fun (make_fun(om::flat (mapcar #'(lambda (x y) (list x y)) amp_fun time_list))))
                      (X-resc_fun amp_fun 0 1)
                      (push freq_fun freq_funs)
                      (push amp_fun amp_funs)
                      ))))
          (if (not (null freqs))
            (progn (if sort (multiple-value-setq (freqs amps edels durs freq_funs amp_funs) 
                              (sort-partiels-and-everything freqs amps edels durs freq_funs amp_funs)))
                   (make-instance 'ptl
                           :the-list (nreverse freqs) 
                           :amplitudes (nreverse amps)                     
                           :duration (- (cdr b) (cdr a))  
                           :origtime (cdr a)
                           :entry-delays (nreverse edels)
                           :durs (nreverse durs)
                           :transp_funs (nreverse freq_funs)
                           :amp_funs (nreverse amp_funs))
                   )))))


(defmethod model-threshold ((x model-partials) analysis-max-amp &key threshold threshmod)
  (let ((ptl-list nil)(thresh threshold))
    (loop for theptl in (ptl-list x)
          do (let (( triplets (mapcar #'list (get-amp theptl)
                                      (fql theptl)
                                      (entry-delays theptl)
                                      (durs theptl)
                                      (transp_funs theptl)
                                      (amp_funs theptl)))
                   )
               (case threshmod
                 (abs ())
                 (rel (setf thresh (+ (lin->db analysis-max-amp) threshold)))
                 (fql (setf thresh (+ (lin->db (get-max-amp theptl)) threshold)))
                 (otherwise (warn "unknown threshold mode")))
               (setf triplets (seuillage_cseq triplets thresh))
               (if triplets  
                 (push (make-instance 'ptl
                         :the-list (mapcar #'second triplets)
                          :amplitudes (mapcar #'first triplets)
                           :entry-delays (mapcar #'third triplets)
                           :durs (mapcar #'fourth triplets)
                           :transp_funs (mapcar #'fifth triplets)
                           :amp_funs (mapcar #'sixth triplets))
                       ptl-list)
                 (push nil ptl-list)))
          finally (setf (ptl-list x) (nreverse ptl-list)))))


(defmethod get-ptl-from-time ((x model-partials) time)
  (let ((vps-number (length(member-if (lambda (y) (< time y)) (mapcar #'cdr (markers x))))))
    (car (last (ptl-list x) vps-number))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;MODELES - MISC FUNCTIONS


(defun partials-analysis-to-model (list)
  (let(time time-end (freqs ())(amps ())(fql-list nil)(markers-list nil))
    (setf list (cddr list))
    (setf time (third(car list)))
    (loop for curr-list in list 
          do(let ((new-time (third curr-list)))
              (if(equal  new-time time)
                (progn(push (db-to-lin (fifth curr-list)) amps)
                      (push (fourth curr-list) freqs))
                (progn (push (make-instance 'fql :the-list (nreverse freqs) :amplitudes (nreverse amps))fql-list)
                       (push time markers-list)
                       (setf time-end (sixth curr-list))
                       (setf freqs nil)
                       (push (fourth curr-list) freqs)
                       (setf amps nil)
                       (push (db-to-lin (fifth curr-list)) amps)))
              (setf time new-time)))
    (push time markers-list)
    (push time-end markers-list)
    (push (make-instance 'fql :the-list (nreverse freqs) :amplitudes (nreverse amps))fql-list)
    (values (nreverse markers-list) (nreverse fql-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


