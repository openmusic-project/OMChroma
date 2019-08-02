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

;to deal with data from formant analysis (from AudioSculpt)
;internal format (data) = ((n t)(1 f1 amp1 bw1)...(n fn ampn bwn))

(in-package :cr)


(defclass formant-data
  (additive-data)
  ()
  (:documentation "Formant analysis data in Ram"))

(defmethod initialize-instance :after ((x formant-data)  &rest initargs &key file)
  (declare (ignore initargs))
 (setf (data x)(load-formant-file file)))



(defun load-formant-file (&optional file)
  (if(null file)(setf file (choose-file-dialog)))
  (format t "LOADING Formant DATA FROM ~a~%"  file)
  (let ((curr-list nil)(result '(x))(result2 nil)(truc nil)(n 0)(time 0))
    (with-open-file (in-stream file :direction :input)
      (loop while (not truc)
            do (multiple-value-bind(s tr)(read-line in-stream nil)
                 (setf truc tr)
                 (setf curr-list
                       (read-from-string (concatenate 'string "(" s ")")))
                 (case (length curr-list)
                   (1 (progn (if(get-gbl 'CTL2-PRINT) (format t "~a~%" curr-list)) 
                             (setf time (car curr-list))))
                   (3 (let ((freq (second curr-list))
                            (amp(db->lin(first curr-list)))
                            (bw(third curr-list)))
                        (unless (= 0 time)
                          (progn (push (nreverse result) result2)
                                 (setf result nil n 0)(push time result)
                                 (setf time 0)))
                        (push (list (incf n) freq amp bw) result))))))
      (push (nreverse result) result2)
      (nombre-de-formants(cdr(nreverse result2))))
))

(defun nombre-de-formants (l)
  (loop for i in l collect (cons (list (1-(length i))(car i)) (cdr i))))

;(nombre-de-formants '((1 (1 2)(3 4))(5 (12 13)(14 15)(200 200))))

