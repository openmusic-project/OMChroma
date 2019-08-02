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

;*****************************************************************************
;-------| VPS SYSTEM
;-------| This file is: $LLvps/utils.lisp
;-------| By Serge Lemouton and Marco Stroppa
;-------| Version 1.0, May 1998
;-------| Copyright 1998 IRCAM
;*****************************************************************************
(in-package :cr)

(defmethod sort-vps-contents ((freqs list) (amps list))
  (let ((result (sort (mapcar #'list freqs amps) #'< :key #'first)))
    (values (mapcar #'first result)
            (mapcar #'second result))
    ))



;; THIS FILE SPECIFIES MIXED UTILITIES NEEDED WITH THE VPS SYSTEM

; AVAILABLE FUNCTIONS:

;	FROM LELISP:   get-gil-vps
;                      get-ail-vps
;                      get-sd-vps
;                      get-cs-vps

;	FROM ESQUISSE: harm-dist-f
;                      fond-virt-f

;	PREDICATES:    unisons-p
;                      interval-p
;                      pitch-with-octave-p
;                      pitch-without-octave-p

;	UTILITIES:     float-semitones
;                      member-times


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;remnants from sys-vps.ll

(defun get-gil-vps (npl) ; npl = numeric pitch list, i.e. a midi list
    (let ((l npl)
	  (result ()))
	(loop while l
	    do(setq result
		(nconc result
		    (build-one-step (nextl l) l))))
	result))

(defun build-one-step (ref npl)
    (ifn npl
	 ()
	 (cons (- (car npl) ref)
	       (build-one-step ref (cdr npl)))))

(defun get-ail-vps (npl al)		; al = number of semitones from root
  (let ((l npl)
        (result ()))
    (loop while l
          do (newl result
		   (- (nextl l) al)) )
    (nreverse result)) )

(defun get-sd-vps (cil)
    (if (< (length cil) 3)
	'uncalculable	;NEED AT LEAST FOUR PITCHES TO COMPUTE ST DEV
	(let ((delta-nis (build-delta-nis cil)))
	    (let ((sum-dint-2 (apply '+ (mapcar (lambda (x) (expt x 2))
						delta-nis)))
		  (sum-2-dint (expt (apply '+ delta-nis) 2))
		  (nn (length delta-nis)))
		(sqrt (/ (abs (- sum-dint-2
			      (/ sum-2-dint nn)))
			 nn))))))

(defun build-delta-nis (nis)
    (let ((result ())
	  (l nis))
	(loop while (cdr l)
	    do(newl result (abs (- (nextl l) (car l)))))
	(nreverse result)))


(defun get-cs-vps (cil &optional (ss *STABILITY-SPACE*)); default weight field = SS
  (let  ((cs 0.0) )
    (let ((ncil (length cil)))
      (loop for i in cil
            do(setf cs
		    (+ cs
		       (get-weight i ss))))
      (/ cs ncil))))

(defun get-weight (int ss)
  (let ((int-nw (mod (round int) 12))
        (oct (floor (+ int 0.5) 12))
        (nw (car ss))
        (os (cdr ss)))
    (if (> oct 6) (setf oct 6))
    (* (cdr (assoc int-nw nw))
       (cdr (assoc oct os)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;HARMONICITY (from Esquisses)

;; =============================================================================-======

;; ==== harmonic distance ====

;;  f0                   f0*n   freq    f0*(n+1)
;;---|---------------------|------|--------|---------->
;;                         <--d1--><--d2--->
;; ratio:=freq/f0; n:=floor(ratio)
;; ;; d1<=d2 <=> ratio*ratio <= n*(n+1)
;; d1 := freq/(f0*n)   := ratio/n
;; d2 := f0*(n+1)/freq := (n+1)/ratio

(defun harm-dist-f (f0 freq ) 
  "Returns the ratio between the closest harmonic of <f0> and <freq>."
  (if (<= freq f0) (/ f0 freq)
      (let* ((ratio (/ freq f0))
             (n-partial (floor ratio))
             (d1 (/ ratio n-partial))
             (d2 (/ (1+ n-partial) ratio)))
        (min d1 d2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;FONDAMENTALE VIRTUELLE (from Esquisses)


(defun fond-virt-f (freqs approx)
  (tolerant-gcd freqs approx))

;;From Gerard Assayag [93 07 16]

(defun tolerant-gcd (values grid-ratio)
  "floating gcd with tolerance grid-ratio around the values."
  (labels ((grid-above (val) (* val (1+ grid-ratio)))
           (grid-below (val) (/ val (1+ grid-ratio)))
           (gcd-try (values gcd-min gcd-max)
             (when (<= gcd-min gcd-max)
               (if (not values)
                 (/ (+ gcd-min gcd-max) 2.0)
                 (let* ((val-below (grid-below (first values)))
                        (val-above (grid-above (first values)))
                        (quo-min (ceiling (/ val-below gcd-max)))
                        (quo-max (floor (/ val-above gcd-min))))
                   (do* ((quotient quo-min (1+ quotient)) (gcd-interval))
                        ((> quotient quo-max) nil)
                     (setf gcd-interval
                           (gcd-try (rest values)
                                         (max gcd-min (/ val-below quotient))
                                         (min gcd-max (/ val-above quotient))))
                     (when gcd-interval
                       (return-from gcd-try gcd-interval))))))))
    (gcd-try values .1 (grid-above (apply 'min values)))))

; (fond-virt-f '(400. 500. 601.) 64) => ((4 5 6) 100.0763034890829 . 100.0902942798978)
; (fond-virt-f '(400. 500. 601.) 32) => ((4 5 6) 99.98602183067244 . 100.1806700903654)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;PREDICATES

(defun unisons-p (l) 
  (cond ((null (cdr l))nil)
        ((equal(car l)(second l))t)
        (t(unisons-p(cdr l)))))


;;;;;
(defun interval-p (i)
  " Tell if the input arg is a symbolic interval.
 In vicious cases, it may fail."
  (cond
   ((memberp i *INTERVALLES*))
   ((numberp i) nil)
   (t (if(symbolp i)(if (memberp i *INTERVALLES*)t nil)
        (if(and (memberp (car i) *INTERVALLES*)
                (numberp (second i))
                (or(null (third i))
                   (numberp (third i))
                   (memberp (third i) *DEVIATIONS*)))t nil)))))

;(interval-p 4)
;(interval-p 4.0)
;(interval-p 9)
;(interval-p '(4 0 40))
;(interval-p '(4 2))

;;;;;;;
(defun pitch-with-octave-p (i)
  " Tell wheather the input arg is a symbolic pitch with octave number.
 In vicious cases, it may fail."
  (if (numberp i)nil
      (if(symbolp i)
        (let* ((curr-string (copy-seq(string i)))
               (oct (read-from-string(string(elt (string i) (1-(length curr-string)))))))
          (if(check-symbolic-pitch curr-string)
            (if(numberp oct)t nil)
            ))
        (if(and(listp i)(numberp(car i)))nil
           (progn(if(not (listp i)) (setf i (cons i 0)))
                 (let* ((curr-string (copy-seq(string (car i))))
                        (oct (read-from-string(string(elt (string (car i))(1-(length curr-string)))))))
                   (if(and (if(check-symbolic-pitch curr-string)
                             (numberp oct))
                           (or(numberp (cdr i))
                              (member (cdr i) *DEVIATIONS*)))t nil)))))))
  
;;;;;;;;; 
(defun pitch-without-octave-p (i)
  " Tell wheather the input arg is a symbolic pitch without octave number.
 In vicious cases, it may fail."
  (cond
   ((and (listp i) (numberp (car i))) nil)
   ((numberp i) nil)
   ((symbolp i)
        (let* ((curr-string (copy-seq(string i)))
               (oct (read-from-string(string(elt (string i) (1-(length curr-string)))))))
          (if(check-symbolic-pitch curr-string)
              (if(numberp oct) nil t)
            )))
   (t
    (progn(if(not (listp i))(setf i (cons i 0)))
      (let* ((curr-string (copy-seq(string (car i))))
             (oct (read-from-string(string(elt (string (car i))(1-(length curr-string)))))))
        (if(and (if(check-symbolic-pitch curr-string)
                    (not(numberp oct)))
                (or(numberp (cdr i))
                   (member (cdr i) *DEVIATIONS*)))t nil))))))

;(pitch-without-octave-p '(DO))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;USEFUL FUNCTIONS

; member-times
; Returns how many times <item> has positively satisfied <test>  in <list>.
;   Uses the function "member"
; Ex: (member-times 3 '(1 2 3 4 5 6 3 4) '=) -> 2
;     (member-times 3 '(1 2 3 4 5 6 3 4) '<) -> 4
;     (member-times 3 '(1 2 3 4 5 6 3 4) '>) -> 2
(defun member-times (item list test)
  (let ((result 0))
    (loop while list
          do (when (funcall test item (car list))
               (setf result (1+ result)))
          do (nextl list))
    result))

; (float-semitones list-of-symbolic-intervals)
; Return the floating point value of a list of symbolic intervals

(defmethod float-semitones ((x list))
  (if (interval-p (car x))
    (mapcar #'float (itvl->semitones x))
    (error "I WORK ONLY WITH LISTS OF SYMBOLIC INTERVALS: ~a" x)))

(defmethod float-semitones ((x t))
  (error "I WORK ONLY WITH LISTS OF SYMBOLIC INTERVALS: ~a" x))
;*****************************************************************************
