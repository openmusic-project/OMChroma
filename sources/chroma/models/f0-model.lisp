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

(defclass model-f0 
  (chroma-model)
  ((f0 :accessor f0
       :documentation "f0 : type fun"))
  (:documentation "Fundamental Frequency Model"))



(defmethod initialize-instance :after ((x model-f0) &rest initargs &key f0)
  (declare (ignore initargs))
  (setf (f0 x) (make_fun (xy-echange (data f0))))
  )



#|
(defmethod load-from-file ((x model-f0) &optional file)
  (if(null file)(setf file (choose-file-dialog)))
  (let (curr-list (truc nil) (funl nil))
    (with-open-file (in-stream file :direction :input)
      (loop while (not truc) 
            do (multiple-value-bind(s tr)(read-line in-stream nil)
                 (setf truc tr)
                 (setf curr-list
                       (read-from-string (concatenate 'string "(" s ")")))
                 (case (length curr-list)
                   (1(progn(push (car curr-list) funl))))))
    (setf (f0 x) (make_fun(xy-echange  funl)))
    (setf (total-duration x) (car(last funl)))
    )'loaded))
|#

(defun xy-echange (l)
  (let ((result nil))
    (loop for x in l 
          do (push (second x) result)
          do (push (first x)result)
          )
    (reverse result)
    ))

