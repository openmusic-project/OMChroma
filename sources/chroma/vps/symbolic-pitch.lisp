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
; File author: M. Stroppa
;=====================================================

(in-package :cr)

(defclass symbolic-pitch
  ()
  ((item :initform ""
         :type string
         :initarg :pitch
         :accessor item)
   (note :initform ""
         :type string
         :accessor note)
   (note+ :initform ""
          :type string
          :accessor note+
          :documentation "note + alteration")
   (alteration :initform nil
               :type t
               :accessor alteration)
   (octave :type integer
           :initform 0
           :accessor octave)
   (diapason :initform (get-gbl 'diapason)
             :initarg :diapason
             :accessor diapason)
   (deviation :initform 0
              :initarg :deviation
              :accessor deviation)
   )
  )

(defmethod initialize-instance :after ((x symbolic-pitch) &rest initargs)
  (declare (ignore initargs))
  (let ((*package* (find-package :chroma))
        (curr-string (item x)))
    (setf (note x) (check-symbolic-pitch curr-string))
    (if(null (note x))
      (error "BAD SYMBOLIC PITCH : ~a" curr-string))
    (setf curr-string (subseq curr-string (length(note x)) ))
    (if(not(eq (length curr-string)0))
      (setf (alteration x) (check-alteration curr-string)))
    (if (null(alteration x)) 
      (setf (note+ x)   (note x))
      (setf(note+ x)(format nil "~a~a"(note x)(alteration x))))
    (setf curr-string (subseq curr-string (length(alteration x))))
    (if(not(eq (length curr-string)0))
      (setf (octave x) (read-from-string curr-string)))
    (if (or (null (octave x))(not(typep(octave x) 'fixnum)))
      (error "BAD OCTAVE : ~a in ~a " (octave x)(item x))
    )))

(defmethod midi-note ((x symbolic-pitch))
  (let ((*package* (find-package :chroma))
        (alt (if (null (alteration x)) 
               0 
               (cdr (assoc (internc (alteration x)) *ALTERATIONS-ALIST*))))
        (deviation (if(not(numberp (deviation x)))
                     (cdr (assoc (internc (deviation x)) *DEVIATIONS-ALIST*))
                     (deviation x)))
        (note  (cdr(assoc (internc (note x)) *SPITCH-ALIST*))))
    (+ 69 note alt (/ deviation 100) (* 12 (-(octave x) 4)))) 
  )

(defmethod freq ((x symbolic-pitch))
  (* (diapason x) (expt 2 (/ (- (midi-note x) 69) 12))))


(defmethod transpose ((x symbolic-pitch) (ratio number))
  (let ((deviation 0) pitch)
    (setf pitch (fq->pch (* ratio (freq x))))
    (setf deviation (cdr pitch)
          pitch (car pitch))
    (make-instance 'symbolic-pitch :pitch pitch :deviation deviation))
  )

(defmethod transpose ((x symbolic-pitch) (interval t))
  (let ((deviation 0) pitch)
    (unless (interval-p interval)
      (error "....MMMM...BIZZARRE INTERVAL: ~a~%    COME ON, ~a, TRY TO BE MORE SERIOUS!"
             interval (get-gbl 'USER)))
    (setf pitch (midi->pch (+ (car (itvl->semitones (list interval)))
                              (midi-note x))))
    (setf deviation (cdr pitch)
          pitch (car pitch))
    (make-instance 'symbolic-pitch :pitch pitch :deviation deviation))
  )
;;;;;;;;;;;;;;;
;;SYMBOL PARSING 

(defun check-symbolic-pitch (string)
(loop for x in *SPITCHLIST* 
     when (eq(search x string)0)
     return x))

(defun check-alteration (string)
(loop for x in *ALTERATIONS* 
     when (search x string)
     return x))

;;;;;;;;;;;;;;;;;;;;
;MISC FOR VPS CONVERSIONS

(defun relative-pitch (note &rest diap)
  (let ((diap (ifn diap (get-gbl 'DIAPASON) (car diap))) )
    (cond ((listp note)
           (if(or (memberp (cdr note) *DEVIATIONS*)(numberp (cdr note)))
             (note+ (make-instance 'symbolic-pitch :pitch (string (car note)) 
                                       :diapason diap 
                                       :deviation (cdr note)))
             (mapcar (lambda (n) (relative-pitch n diap)) note)))
          (t
           (note+(make-instance 'symbolic-pitch :pitch (string note) 
                                     :diapason diap ))))))

(defun make-sp-list (note &rest diap)
  (let ((diap (ifn diap (get-gbl 'DIAPASON) (car diap))) )
    (cond ((numberp note) note)
          ((listp note)
           (if(or (memberp (cdr note)  *DEVIATIONS*)(numberp (cdr note)))
             (make-instance 'symbolic-pitch :pitch (string (car note)) 
                                  :diapason diap 
                                  :deviation (cdr note))
           (mapcar (lambda (n) (make-sp-list n diap)) note)))
          (t
           (make-instance 'symbolic-pitch :pitch  (string note) 
                                :diapason diap)))))

(defun freqs-to-arl (l ref)
  (cond((null l) ())
       (t (cons (/ (first l) ref) (freqs-to-arl (cdr l) ref)))))

(defun ail-to-freq (int ref)
  (let ((deviation 0)(octave 0)(intervalle))
    (if (null int)()
        (progn(if(memberp (car int) *INTERVALLES*) 
                (progn (setf intervalle (car int))(setf deviation 0) (setf octave 0))
                (progn (setf intervalle (caar int))
                       (if(null (third (car int)))
                         (progn(setf deviation 0)(setf octave (second (car int))))
                         (progn(setf octave (second (car int)))(setf deviation (third (car int)))))))
              (when (symbolp intervalle)
                 (setf intervalle (internc intervalle)))
              (cons (* ref (expt 2 
                                 (/ 
                                  (+ (cdr(assoc intervalle *INTERVALLES-ALIST*))
                                     (/ deviation 100) 
                                     (* 12  octave )
                                     ) 12))) (ail-to-freq (cdr int) ref)
                    )))))

(defun ail-to-midi (int ref)
  (let ((deviation 0)(octave 0)(intervalle))
    (if (null int)()
        (progn(if(memberp (car int) *INTERVALLES*) 
                (progn (setf intervalle (car int))(setf deviation 0) (setf octave 0))
                (progn (setf intervalle (caar int))
                       (if(null (third (car int)))
                         (progn(setf deviation 0)(setf octave (second (car int))))
                         (progn(setf octave (second (car int)))(setf deviation (third (car int)))))))
              (when (symbolp intervalle)
                 (setf intervalle (internc intervalle)))
              (cons  (+ ref 
                        (cdr(assoc intervalle *INTERVALLES-ALIST*))
                        (/ deviation 100) 
                        (* 12  octave ))(ail-to-midi (cdr int) ref)
                     )))))

(defun arl-to-freq (int ref)
  (if (null int)()
      (cons (* ref (car int))(arl-to-freq (cdr int) ref))))

#|
(setf mynote (make-instance 'symbolic-pitch :pitch (string 'DOd)
                            :deviation 'q))
(setf mynote (make-instance 'symbolic-pitch :pitch "LA4"))

(item mynote)
(note mynote)
(alteration mynote)
(octave mynote)
(deviation mynote)
(freq mynote)
(midi-note mynote)

(transpose mynote :ratio 3/2)
(setf mynote (transpose mynote :ratio 3/2))
(setf mynote (transpose mynote :interval '(2+ 0 0)))

|#
