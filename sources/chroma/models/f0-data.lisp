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

;to deal with data from cseq analysis (from audiosculpt)

(in-package :cr)


(defclass f0-data
  (analysis-data)
  ()
  (:documentation "fundamental frequency from Audiosculpt or AddAn data"))


(defmethod initialize-instance :after ((x f0-data)  &rest initargs &key file)
  (declare (ignore initargs))
  (load-file x file)
  )


(defmethod load-file ((x f0-data) &optional file)
"Load f0 data from an Audiosculpt or an AddAn f0 file"
  (if(null file)(setf file (choose-file-dialog)))
  (with-open-file (in-stream file :direction :input)
    (let((line  (om-read-list-from-string (read-line in-stream nil))))
      (loop while (null line) 
            do (setf line (om-read-list-from-string (read-line in-stream nil))))
      (cond
       ((numberp line)(load-file1 x file))
       ((eq 2(length line)) (load-file2 x file))
       (t  (error "unknown f0 file format !!!!"))))))

(defmethod load-file1 ((x f0-data) file)
"Load f0 data from an AddAn f0 file"
  (format t "LOADING DATA FROM ~a (1)~%" (file x))
  (with-open-file (in-stream file :direction :input)
  (let ((result nil)(ti t) va)
    (loop while (not (null ti))
          do (setf ti (read in-stream nil))
          do (setf va (read in-stream nil))
          do (if ti (push (list ti va) result)))
    (setf (data x)(reverse result))
    )))

(defmethod load-file2 ((x f0-data) file)
"Load f0 data from an Audiosculpt f0 file"
  (format t "LOADING DATA FROM ~a (2)~%" (file x))
 (let ((truc nil) (funl nil))
  (with-open-file (in-stream file :direction :input)
      (loop while (not truc) 
            do (multiple-value-bind (s tr) (read-line in-stream nil)
                 (setf truc tr)
                 (if s (push (om-read-list-from-string s) funl)))))
    (setf (data x) (reverse funl))
     ) 'loaded)



