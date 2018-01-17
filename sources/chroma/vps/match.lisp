(in-package chroma)

;---------------------------------------------------------------------------------
; (match SPL1 SPL2 :tolerance :step :notes)
; Match SPL1 against SPL2, i.e. transpose SPL1 by "step" over SPL2 (fixed)
;    :tolerance [cents, default=50] = tolerance accepted when performing the match
;    :step [cents, default=100] = step of transposition of SPL2

; Format of the result: list of items preceded by the number of matched notes
#|
  v-> number of matched notes
((3

     v-> transposed SPL1 that matches the 3 pitches below of SPL2
  #<SPL #x463642E> (("RE5" . 0) ("SOLd4" . 0) ("MI4" . 0))
  #<SPL #x4615C86> (("DO5" . 0) ("SOLd4" . 0) ("MI4" . 0)))
 (2
  #<SPL #x4642AC6> (("DO5" . 0) ("SOLd4" . 0))
  #<SPL #x462FD46> (("DO5" . 0) ("FAd4" . 0))
  #<SPL #x462991E> (("RE5" . 0) ("MI4" . 0))
  #<SPL #x4622F26> (("DO5" . 0) ("SOLd4" . 0))
  #<SPL #x461C576> (("RE5" . 0) ("FAd4" . 0))
  #<SPL #x460839E> (("SOLd4" . 0) ("MI4" . 0))
  #<SPL #x460153E> (("SI4" . 0) ("FAd4" . 0)))
 (1
  #<SPL #x4655E76> (("RE5" . 0))
  #<SPL #x464F52E> (("DO5" . 0))
  #<SPL #x464C2EE> (("SI4" . 0))
  #<SPL #x464903E> (("RE5" . 0))
  #<SPL #x463F836> (("SI4" . 0))
  #<SPL #x463C676> (("FAd4" . 0))
  #<SPL #x462C936> (("SI4" . 0))
  #<SPL #x461F8FE> (("SI4" . 0))
  #<SPL #x4612706> (("SI4" . 0))
  #<SPL #x460F00E> (("FAd4" . 0))
  #<SPL #x460B84E> (("RE5" . 0))
  #<SPL #x4604BBE> (("DO5" . 0))
  #<SPL #x45FA5E6> (("MI4" . 0))
  #<SPL #x45F6CFE> (("SOLd4" . 0))
  #<SPL #x45EFC36> (("FAd4" . 0))
  #<SPL #x45E8C96> (("MI4" . 0)))
 (0
  #<SPL #x465269E> NIL
  #<SPL #x464576E> NIL
  #<SPL #x4638FD6> NIL
  #<SPL #x4632C76> NIL
  #<SPL #x4626056> NIL
  #<SPL #x4618DBE> NIL
  #<SPL #x45FDC66> NIL
  #<SPL #x45F3406> NIL
  #<SPL #x45EC34E> NIL))
|#

(defmethod match ((x spl) (y spl) &key (tolerance 50) (step 100))
;(om::defmethod! match ((x spl) (y spl) &key (tolerance 50) (step 100))
;  :icon 130
  (let* (result
         (x-low (first (get-spl x)))
         (x-high (car(last (get-spl x))))
         (y-low (first (get-spl y)))
         (y-high (car(last (get-spl y))))
         (first-transposition (car(pch->semitones (list y-high x-low))))
         (last-transposition (car(pch->semitones (list y-low x-high)))))
    (loop for trans from first-transposition to last-transposition by (/ step 100)
          do (let ((y-curr (transpose y (semitones->itvl trans))))
               (push (match-spl x y-curr tolerance) result)))
    (setf result (sort result #'< :key #'car))
    (let (result2 (result3 (car result))(n (car (car result))))
      (pop result)
      (loop for r in result
        ;    do (print r)
            do (if (equal (car r) n)
                 (setf result3 (append result3 (cdr r)))
                 (setf n (car r) result2 (cons result3 result2 ) result3 r))
            finally (setf result2 (cons result3 result2)))
      result2)))

(defun match-spl (x y tolerance)
  (let ((nmatch 0)(match-list nil))
  (loop for p1 in (get-spl x)
        do (loop for p2 in (get-spl y)
                 do (if (<(abs (car (pch->semitones (list p1 p2))))(/ tolerance 100))
                   (progn (incf nmatch)
                          (push p2 match-list)
                          (return)))))
  (list nmatch (list y (reverse match-list)))))
;---------------------------------------------------------------------------------

(defmethod match_vps ((x spl) (y spl) &key (matches ()) (tolerance 50) (step 100))
;(om::defmethod! match_vps ((x spl) (y spl) &key (matches ()) (tolerance 50) (step 100))
;  :icon 130
  ; Returns a list of VPS that can be used in a data base
  ; :matches: wanted matching pitches
  ;           nil = only the best match
  ;           number = only the "number" matching pitches (0 = no matching pitches)
  ;REMARK: to have all the matches, use "merge"
  (let ((result (match x y :tolerance tolerance :step step)))
    (if matches
      (cdar (member matches result :test #'= :key #'car))
      (cdr (first result)))))

(defmethod match_vps ((x vps) (y vps) &key &allow-other-keys)
;(om::defmethod! match_vps ((x vps) (y vps) &key &allow-other-keys)
  (error "YOU MUST GIVE ME TWO SPL'S. I CANNOT CONVERT THEM ALONE!
ARG1: ~a
ARG2: ~a" x y))

;---------------------------------------------------------------------------------



#|

(setf my-spl1(make-instance 'spl :the-list '(do1 (re1 . .33))))
(setf my-spl2(make-instance 'spl :the-list '( DO2 sol2 DO3 re3 MI3 do5)))
(match my-spl1 my-spl2 :step 200)
(first(match my-spl1 my-spl2 :step 10))
(second(match my-spl1 my-spl2))
(third(match my-spl1 my-spl2))

|#
