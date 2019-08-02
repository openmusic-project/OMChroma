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

(defclass model-specenv
  (chroma-model)
  ((timelist :accessor timelist
       :documentation "temps des enveloppes")
   (envlist :accessor envlist
       :documentation "liste de fun"))
  (:documentation "Spectral Surface Model"))



(defmethod initialize-instance :after ((x model-specenv) &rest initargs &key analysis)
  (declare (ignore initargs))
  (if analysis
    (progn (setf (timelist x) (mapcar #'first (cdr (data analysis))))
   (setf (envlist x) (mapcar #'envtofun (cdr (data analysis))))
   )))

(defun envtofun (l)
  (let((result nil)(l (cdr l)))
    (loop for i from 0 to (length l) 
          for e in l
          do (push (lintodb e) result)
          (push i result))
    (make_fun (nreverse result))))


;(envtofun '(1 2 3 4))

