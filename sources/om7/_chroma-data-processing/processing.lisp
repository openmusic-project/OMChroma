(in-package :om)


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
