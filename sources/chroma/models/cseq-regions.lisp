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

(in-package :cr) ;AAA
;--------------------
;CLASSE
;--------------------
(defclass regions-cseq
  (regions model-cseq)
  (
   )
  (:documentation "Chord Seq Regions Model"))

;--------------------
;METHODES
;--------------------
(defmethod initialize-instance :after ((x regions-cseq) &rest initargs &key markers)
  (declare (ignore initargs))
  (check-regions x)
  (check-number-of-regions x)
  (unless markers (error "MISSING MARKERS"))
)

(defmethod copy-model ((x regions-cseq))
"Return a copy of the model"
  (make-instance 'regions-cseq 
                 :markers (copy-tree (region-list x)) 
                 :fql-list (mapcar #'copy_vs (fql-list x))))

(defmethod merge-model ((model1 regions-cseq)(model2 regions-cseq))
  "returns a model made of 2 models"
  (let((regions1 (mapcar #'(lambda (x)(list (car x) (list (+ (get-offset model1) (caadr x))
                                                          (+ (get-offset model1)  (cadadr x)))))
                         (get-regions model1)))
       (regions2 (mapcar #'(lambda (x)(list (car x) (list (+ (get-offset model2) (caadr x))
                                                          (+ (get-offset model2)  (cadadr x)))))
                         (get-regions model2))))
  (make-instance 'regions-cseq :markers (append regions1 regions2)
                 :fql-list (append (mapcar #'copy_vs (fql-list model1))
                                   (mapcar #'copy_vs (fql-list model2))))))


;--------------------
;Private methods :
;--------------------

(defmethod compute-model :before  ((x regions-cseq) (y additive-data) markers &key &allow-other-keys)
  (declare (ignore markers))
  (set_regions_to_closest_frame x y))

(defmethod compute-model ((x regions-cseq) (y additive-data) markers
                             &key sort weighed-avg durmin)
  (if (not markers)(warn "missing markers"))
  (let ((fql-list nil)(region-list nil))
    (loop for region in (region-list x) 
          do (let* ((a (cons (first (third region))(first (second region))))
                    (b (cons (second (third region))(second (second region))))
                    (fql (extract-fql x y a b :sort sort :weighed-avg weighed-avg :durmin durmin)))
               (if fql (progn(push fql fql-list)
                             (push region region-list)))))
    (setf (fql-list x) (nreverse fql-list)
          (region-list x) (nreverse region-list))))

(defmethod set_regions_to_closest_frame ((x regions-cseq) (a analysis-data))
  (let*((result nil)
       (f (get_add_time (data a)))
       (f (subseq f 0 (- (length f) 2))) ; attention a la derniere trame d'analyse !
       )
    (loop for region in (region-list x)
          do (let*((mark1 (first (second region)))
                   (mark2 (second (second region)))
                   (diff_l (mapcar #'(lambda (x) (abs(- x mark1)))f))
                   (min (apply #'min diff_l))
                   (pos1 (position min diff_l))
                   (diff_l (mapcar #'(lambda (x) (abs(- x mark2)))f))
                   (min (apply #'min diff_l))
                   (pos2 (position min diff_l)))
               (push (list (first region) (list (nth pos1 f) (nth pos2 f))(list pos1 pos2)) result)))
    (setf (region-list x) (nreverse result)
    )))

(defmethod set_markers_to_closest_frame ((x regions-cseq) (a analysis-data) markers)
  (declare (ignore markers))
  ;do nothing
  nil)

(defmethod restore_markers ((x regions-cseq))
  ;do nothing
  nil)

(defmethod check-number-of-regions ((x regions-cseq))
 (if (region-list x)
    (if (not (= (length (region-list x))  (length (fql-list x))))
      (error "THERE SHOULD BE AS MANY REGIONS AS FQLS~%      Regions = ~a,fqls = ~a~%"
           (length (region-list x))(length (fql-list x))))
#|
    (if(not (equal (length (markers x)) (1+ (length (fql-list x)))))
        (error "THERE SHOULD BE ONE MARKER MORE THAN FQLS~%      Markers = ~a,fqls = ~a~%"
               (length (markers x))(length (fql-list x))))
|#
)
)



