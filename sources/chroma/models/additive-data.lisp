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


;to deal with data from additive analysis (from AddAn or AudioSculpt)
;internal format (data) = ((n t)(1 f1 amp1 phi1)...(n fn ampn phin))

(in-package :cr)


(defclass additive-data (analysis-data)
  ()
  (:documentation "Analysis data in Ram"))


(defmethod initialize-instance :after ((x additive-data)  &rest initargs &key file asfile)
  (declare (ignore initargs))
  (cond 
   (file (setf (data x) (load-add-file file)))
   (asfile (setf (data x)(load-as-file asfile)))
   ))

;traverse l'analyse pour trouver l'amplitude max (normalisation ....)
#|
(defmethod get-max-amp ((x additive-data))
  (apply #'max (remove nil(mapcar #'third(mapcar #'flat (mapcar #'cdr (data x)))))))

|#

;;loop version

(defmethod get-max-amp ((x additive-data))
(let((max 0))
 (loop for l1 in (data x)
       do (loop for l2 in l1
                do (when (eq 4 (length l2))
                  (if (> (third l2) max)(setf max (third l2))))))max))


(defun load-add-file (&optional file)
  (if (null file) (setf file (choose-file-dialog)))
  (format t "LOADING DATA FROM ~a~%"  file)
  (let ((curr-list nil)(result '(x))(result2 nil)(truc nil))
    (with-open-file (in-stream file :direction :input)
      (loop while (not truc)
            do (multiple-value-bind(s tr)(read-line in-stream nil)
                 (setf truc tr)
                 (setf curr-list
                       (read-from-string (concatenate 'string "(" s ")")))
                 (case (length curr-list)
                   (2(progn(push (nreverse result) result2)
                           (setf result nil)
                           (if(get-gbl 'CTL2-PRINT) (format t "~a~%" curr-list)) 
                           (push curr-list result)))
                   (4(push curr-list result)))))
      (push (nreverse result) result2)
      (cdr(nreverse result2))))
)

(defun load-as-file (&optional file)
  (if (null file) (setf file (choose-file-dialog)))
  (format t "LOADING DATA FROM Audiosculpt file ~a~%"  file)
  (with-open-file (in-stream file :direction :input)
    (let* ((data (read in-stream t))
           (data2 (cddr data))
           (data2 (mapcar #'cddr data2)))
      (setf data2 (asd_tripl data2))
      (setf data2 (asd_sort data2))
      (setf data2 (asd_group data2))
      )))

(defun asd_tripl (l)
  (loop for ll in l count ll into i
        append (loop for lll on ll by #'cdddr 
                     collect (list i (first lll)(second lll)(third lll)))))

(defun asd_group (l)
  (let ((curr_time (second(first l)))(prev_time nil)(result nil)(temp nil))
    (loop for ll in l 
          do (setf curr_time (second ll))
          do (if (equal curr_time prev_time)
               (push ll temp)
               (progn 
                 (push (asd_group2 temp) result)
                 (setf temp (list ll) prev_time curr_time)
                 ))
          finally (push (asd_group2 temp) result))
    (cdr(reverse result))))

(defun asd_group2 (l)
  (let ((npart (caar l)) 
        (time (cadar l))
        (triplets (mapcar #'(lambda (x) (list (first x) (third x) (dbtolin (fourth x)) 0.)) l)))
    (cons (list npart time) (reverse triplets))))

;(asd_group '((1 1 1 1)(2 2 2 2)(3 2 3 3)(4 4 4 4)(5 4 5 5)))

(defun asd_sort ( l)
  (sort l #'< :key #'second))

(defun asd_findall (time l)
  (let ((result nil)(curr_pos -1))
    (loop while  curr_pos 
          do (setf curr_pos (position-if #'(lambda (x) (equal time (second x))) l :start (1+ curr_pos)  ))
          do (if curr_pos (push (nth curr_pos l)  result)))
    (reverse result)))


(defun asd_matrice_transpose (l)
  (loop for i from 0 to (1- (length (first l)))
        collect (mapcar #'(lambda (x) (nth i x)) l)
        ))

;(asd_matrice_transpose '((1 2 3 4)(4 5 6 a)(7 8 9 b)))


#|
; Using OpenMusic / SDIF tools we can also read and convert from an SDIF file with:

(make-instance 
 'additive-data
 :file sdif-file
 :data (get-partials-data (om::load-sdif-file file)))


;; Returns a list of partials data (pitch onset offset velocity) 
;; from an sdif file ( 1TRC frames)"
(defmethod get-partials-data ((self om::sdiffile))
  (let ((res nil) (res1 nil)
        mlist time
        (ptrfile (sdif::sdif-open-file self :ereadfile)))
    (sdif::SdifFReadGeneralHeader ptrfile)
    (sdif::SdifFReadAllASCIIChunks ptrfile)
    (loop for item in (om::framesdesc self) do  
          (when (string-equal "1TRC" (car item))
            (setf time (nth 1 item))
            (sdif::sdif-set-pos ptrfile (nth 3 item))
            (setf mlist (nth 4 item))
            (loop for m in mlist do
                  (when (string-equal "1TRC" (car m))
                    (push (list (second m) time) res1)
                    (om::sdif-read-headers ptrfile (nth 3 item) (fifth m))
                    (loop for i = 0 then (+ i 1) while (< i (second m)) do
                          (sdif::SdifFReadOneRow ptrfile)
                          (let* ((ind (floor (sdif::SdifFCurrOneRowCol  ptrfile 1)))
                                 (freq (sdif::SdifFCurrOneRowCol ptrfile 2))
                                 (amp (sdif::SdifFCurrOneRowCol ptrfile 3))
                                 (phi (sdif::SdifFCurrOneRowCol ptrfile 4)))
                            (push (list ind freq amp phi) res1)
                            ))
                    (push (reverse res1) res)(setf res1 nil)))))
    (reverse res)
    ))
|#


