(in-package :om)

;;;==================
;;; MODEL INSPECT

;;; MAX AMP
(defmethod! model-max-amp ((self list))
  :icon 659
  (list-max (mapcar 'list-max (remove nil (mapcar 'get-vps-amps (remove nil self))))))

(defmethod! model-max-amp ((self cr-model))
  :icon 659
  (max-amp (elements (data self))))

; compat
(defmethod! max-amp (self) (model-max-amp self))


;;; MAX FREQ
(defmethod! model-max-freq ((self list))
  :icon 659
  (list-max (mapcar 'list-max (remove nil (mapcar 'get-vps-freqs (remove nil self))))))

(defmethod! model-max-freq ((self cr-model))
  :icon 659
  (max-freq (elements (data self))))

; compat
(defmethod! max-freq (self) (model-max-freq self))


;;; MIN FREQ
(defmethod! model-min-freq ((self list))
  :icon 659
  (list-min (mapcar 'list-min (remove nil (mapcar 'get-vps-freqs (remove nil self))))))

(defmethod! model-min-freq ((self cr-model))
  :icon 659
  (min-freq (elements (data self))))

; compat
(defmethod! min-freq (self) (model-min-freq self))


;;; NB EVTS
(defmethod! model-nb-evts ((self cr-model))
  :icon 659
  (length (elements (data self))))

; compat
(defmethod! nb-evts (self) (model-nb-evts self))


;;; NB EVTS
(defmethod! model-total-dur ((self cr-model))
  :icon 659
  (cadr (car (last (time-struct self)))))

;;;==================
;;; UTILS CR-FUN

(defmethod! make-cr-fun ((self list))
  :icon 657
  :initvals '((0 0 1 1))
  (cr::make_fun self))

(defmethod! make-cr-fun ((self bpf))
  :icon 657
  (let ((list (loop for x in (x-points self)
                    for y in (y-points self)
                    append (list y x))))
    (cr::make_fun list)))

(defmethod! fun-to-bpf ((self list))
  :icon 657
  (simple-bpf-from-list (cr::x-list_fun self) (cr::y-list_fun self) 'bpf 3))

(defmethod! add-random ((self list) rate)
  :icon 656
  :indoc '("a list of values" "a random rate (0.0->1.0)")
  :initvals '(nil 0.1)
  (mapcar (lambda (x) (cr::ran x (* x rate))) self))

;;;==============================
;;; TEMPORAL PROCESSING

#|
(defmethod! time-map-fun ((markers list) fun)
  :icon 658
  (let ((fun2 (copy-list fun))
        (markers2 (copy-list markers))
        new-durs)
    (cr::x-resc_fun fun2 (first markers2) (car (last markers2)))
    (setf new-durs (mapcar
                    (lambda (time dur) (* dur (cr::y-val_fun fun2 time)))
                    markers2 (cr::markerstodur markers2)))
    (cr::durtotime (first markers2) new-durs)))
|#
