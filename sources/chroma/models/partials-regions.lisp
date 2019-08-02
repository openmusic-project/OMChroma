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

(in-package :chroma) ;AAA
;--------------------
;CLASSE
;--------------------
(defclass regions-partials
  (regions-cseq model-partials)
  (
   )
  (:documentation "Partials Regions Model"))

;--------------------
;METHODES
;--------------------
(defmethod get-ptl-from-time ((x regions-partials) time)
  (let ((vps-number (length(member-if (lambda (y) (< time y)) (mapcar #'cdr (markers x))))))
    (car (last (ptl-list x) vps-number))))

(defmethod is-regions-partials? ((x regions-partials) )
 t)
(defmethod is-regions-partials? ((x t) )
 nil)

(defmethod copy-model ((x regions-partials))
"Return a copy of the model"
  (make-instance 'regions-partials :markers (copy-tree (markers x)) :ptl-list (mapcar #'copy-instance (ptl-list x))))

(defmethod merge-model ((model1 regions-partials)(model2 regions-partials))
  "returns a model made of 2 models"
  (let((regions1 (mapcar #'(lambda (x)(list (car x) (list (+ (get-offset model1) (caadr x))
                                                          (+ (get-offset model1)  (cadadr x)))))
                         (get-regions model1)))
       (regions2 (mapcar #'(lambda (x)(list (car x) (list (+ (get-offset model2) (caadr x))
                                                          (+ (get-offset model2)  (cadadr x)))))
                         (get-regions model2))))
  (make-instance 'regions-partials :markers (append regions1 regions2)
                 :ptl-list (append (mapcar #'copy-instance (fql-list model1))
                                   (mapcar #'copy-instance (fql-list model2))))))





;;;;;;;;;;;;;;;;;;;;;;;
;Private methods :
(defmethod compute-model ((x regions-partials) (y additive-data) markers
                             &key sort weighed-avg durmin)
  (if (not markers)(warn "missing markers !!"))
  (let ((ptl-list nil)(region-list nil))
    (loop for region in (region-list x) 
          do (let* ((a (cons (first (third region))(first (second region))))
                    (b (cons (second (third region))(second (second region))))
                    (ptl (extract-ptl x y a b :sort sort :weighed-avg weighed-avg :durmin durmin)))
               (if ptl (progn(push ptl ptl-list)
                             (push region region-list)))))
    (setf (ptl-list x) (nreverse ptl-list)
          (region-list x) (nreverse region-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;MODELES - MISC FUNCTIONS


