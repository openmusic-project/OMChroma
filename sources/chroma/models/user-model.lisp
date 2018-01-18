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

;---------------------------------------------------------------;
;****** USER-DEFINED FUNCTIONS FOR MODIFYING MODELS ************;
;---------------------------------------------------------------;

; transpose-fql

;---------------------------------------------------------------;
(in-package :chroma)

#|
    ;Template for modifications
(defun insert-frequency (fql &rest args)
"fonction par defaut appelee par modify-fql
insere des composantes entre les composantes du model original"
  (declare (ignore args))
  (let ((result nil)(result-amp nil)
        (list (get-fql fql))
        (list-amp (get-amp fql)))
    (loop while list
          do (let((val (pop list)))
               (if(null result)
                 (push (/ val 2) result)
                 (push (/(+ val (car result))2) result))
               (push val result))
          do (let((val (pop list-amp)))
               (if(null result-amp)
                 (push (/ val 2) result-amp)
                 (push (/(+ val (car result-amp))2) result-amp))
               (push val result-amp)))
    (make-instance 'fql :the-list (nreverse result)
                   :amplitudes (nreverse result-amp))))
|#

;---------------------------------------------------------------;
(defun transpose-fql (fql args &key (xp-mode ()) (oct 1))
  "
Transpose the components of the original model.
Args ca be:
EITHER
   a number = all the components will be multiplied by this number (ratio)
OR
   a list of pairs: (wanted-fq . reference-fq)

   xp-mode = strategy of transposition (list)
       nil: usual transposition (quite radical)
         1: transpose within :oct octaves (never more than :oct octaves)
         2: as 1, but using the stretch's offset to transpose
"
  (let* ((wanted-fq (if (listp args) (car args) 1.0))
         (ref-fq (if (listp args) (cdr args) 1.0))
         (ratio (if (listp args) (/ wanted-fq ref-fq) args)))
    (cond
     ((numberp args) (transpose fql ratio))
     ((null xp-mode) (transpose fql ratio))
     ((= xp-mode 1)
      (transpose fql (keep-within-octave wanted-fq ref-fq oct)))
     ((= xp-mode 2)
; error when loading: Marco, 990906
;      (sp-stretch fql
      (stretch_vps fql
                  :offset (1- (keep-within-octave wanted-fq ref-fq oct))))
     (t (error "UNKNOWN TRANSPOSITION MODE, SIR : ~a~%" xp-mode)))))

(defun keep-within-octave (wanted ref oct)
  (let ((ratio (/ wanted ref))
        (upper-oct (expt 2 oct))
        (lower-oct (/ 1.0 (expt 2 oct))))
    (cond
     ((and (<= ratio upper-oct) (>= ratio lower-oct))
         ratio)
     ((< ratio lower-oct)
      (loop while (< (/ wanted ref) lower-oct)
            do (setf wanted (* wanted 2.0)))
      (/ wanted ref))
     ((> ratio upper-oct)
      (loop while (> (/ wanted ref) upper-oct)
            do (setf wanted (/ wanted 2.0)))
      (/ wanted ref)))))

