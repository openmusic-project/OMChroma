#|

Further debugged and modified (defun->defmethod) by Marco, May 1998

AVAILABLE PITCH CONVERSIONS
Format <source>-><destination>
Ex: pch->fq = converst pitches into frequencies

 ---->      fq pch midi midic ratio itvl semitones  pch-class
fq          \   *   *     *     *    *      *           0
pch         *   \   *     *     *    *      *           *
midi        *   *   \     *     *    *      *           *
midic       *   *   *     \     *    *      *           \
ratio       *   *   *     *     \    *      *           0
itvl        *   *   *     *     *    \      *           0
semitones   *   *   *     *     *    *      \           0
pch-class   *   *   *     *     *    *      *           \
                                         (* = yes , 0 = no)
|#


(in-package chroma)

;FREQUENCY
;(defmethod fq->pch ((f number) &optional (approx 0))
(om::defmethod! fq->pch ((f number) &optional (approx 0))
  :icon '(141)
  (fq-to-pch f approx))

;(defmethod fq->pch ((f list) &optional (approx 0))
(om::defmethod! fq->pch ((f list) &optional (approx 0))
  :icon '(141)
    (cond ((pitch-with-octave-p f) f)
         ((or (symbolp f) (stringp f))
          (error "CANNOT CALCULATE THE FREQ OF ~a, SIR ~a" f (get-gbl USER)))
         (t (mapcar (lambda (x) (fq-to-pch x approx)) f))))

;(defmethod fq->pch ((f t) &optional (approx 0))
(om::defmethod! fq->pch ((f t) &optional (approx 0))
  :icon '(141)
  (declare (ignore approx))
    (if (pitch-with-octave-p f)
      f
      (error "MYSTERIOUS ARGUMENT, SIR: ~a" f)))

(defmethod fq-to-pch ((f t) &optional (approx 0))
  (if (pitch-with-octave-p f)
    f
    (midi->pch (fq->midi f) approx)))


;(defmethod fq->midi ((l list) &optional (diapason 440.0))
(om::defmethod! fq->midi ((f list))
                :icon '(141)
                (mapcar #'fq-to-midi f))

;(defmethod fq->midi (l)
(om::defmethod! fq->midi ((f number))
                :icon '(141)
                (fq-to-midi f))

(om::defmethod! fq->midi ((f t))
                (error "CANNOT CALCULATE THE MIDI OF ~a, SIR ~a" f (get-gbl USER)))

(defmethod fq-to-midi (f)
  (+ 69 (/(log (/ f (get-gbl 'diapason))) (log(expt 2 (/ 1 12))) )))

;(defmethod fq->midic ((l list))
(om::defmethod! fq->midic ((f list))
  :icon '(141)
 (mapcar #'fq->midic f))

;(defmethod fq->midic (l)
(om::defmethod! fq->midic ((f number))
  :icon '(141)
      (midi->midic (fq-to-midi f)))


(om::defmethod! fq->midic ((f t))
                (error "CANNOT CALCULATE THE MIDIC OF ~a, SIR ~a" f (get-gbl USER)))


;(defmethod fq->ratio ((l list))
(om::defmethod! fq->ratio ((f list))
  :icon '(141)
  (cond ((null (second f))())
        (t ( cons (/(second f)(first f))(fq->ratio(cdr f))))))

;(defmethod fq->ratio (l)
(om::defmethod! fq->ratio ((l t))
  :icon '(141)
  (error "I NEED A LIST OF AT LEAST TWO ARGUMENTS, SIR ~a, AND YOU GAVE ME ~a" (get-gbl USER) l))


; COMPOUND CONVERSIONS, ms0906
;(defmethod fq->itvl (l)
(om::defmethod! fq->itvl (l)
  :icon '(141)
  (ratio->itvl (fq->ratio l)))

;(defmethod fq->semitones (l)
(om::defmethod! fq->semitones (l)
  :icon '(141)
  (ratio->semitones (fq->ratio l)))

#|
(fq->pch 881 )
(fq->pch 129.677 )
(fq->pch 881 t)
(fq->pch 881 10)
(fq->pch '(200 201 204 205 440))
(fq->pch '(200 201 204 205 440) t)
(fq->pch '(200 201 204 205 440) 100)
(loop for i from 220 to 442
     do (print (fq->pch i)))

(fq->midi 442)
(fq->midi '(200 201 204 205 440))
(fq->midic 440)
(fq->midic '(200 201 204 205 440))

(fq->ratio '(100 200 220))

;compound, new 2009
(fq->itvl '(100 200))
(fq->itvl '(100 200 300 400 500 600 700))

(fq->semitones '(100 200))
(fq->semitones '(100 200 300 400 500 600 700))

|#


;PITCH
;(defmethod pch->fq ((note number) &rest diap)        ;nouvelle version pour pch (cf : LLdg-pitch.lisp) 
(om::defmethod! pch->fq ((note number) &rest diap)
  :icon '(141)
  (declare (ignore diap))
  note)

;(defmethod pch->fq ((note list) &rest diap)        ;nouvelle version pour pch (cf : LLdg-pitch.lisp) 
(om::defmethod! pch->fq ((note list) &rest diap)
  :icon '(141)
  (let ((diap (ifn diap (get-gbl DIAPASON) (car diap))) )
    (if(or(memberp (cdr note) *DEVIATIONS*) (numberp (cdr note)))
      (freq (make-instance 'symbolic-pitch :pitch (string (car note)) 
                           :diapason diap 
                           :deviation (cdr note)))
      (mapcar (lambda (n) (pch->fq n diap)) note))))

;(defmethod pch->fq ((note t) &rest diap)        ;nouvelle version pour pch (cf : LLdg-pitch.lisp) 
(om::defmethod! pch->fq ((note t) &rest diap)        ;nouvelle version pour pch (cf : LLdg-pitch.lisp) 
  :icon '(141)
  (let ((diap (ifn diap (get-gbl DIAPASON) (car diap))) )
    (freq (make-instance 'symbolic-pitch :pitch (string note) 
                         :diapason diap))))

;(defmethod pch->midi (note &rest diap)
(om::defmethod! pch->midi (note &rest diap)
  :icon '(141)
  (let ((diap (ifn diap (get-gbl DIAPASON) (car diap))) )
    (cond ((numberp note) note)
          ((listp note)
           (if(or (memberp (cdr note) *DEVIATIONS* ) (numberp (cdr note)))
             (midi-note (make-instance 'symbolic-pitch :pitch (string (car note)) 
                                       :diapason diap 
                                       :deviation (cdr note)))
             (mapcar (lambda (n) (pch->midi n diap)) note)))
          (t
           (midi-note (make-instance 'symbolic-pitch :pitch  (string note) 
                                     :diapason diap))))))


;(defmethod pch->midic (note &rest diap)
(om::defmethod! pch->midic (note &rest diap)
  :icon '(141)
  (ifn diap
       (midi->midic (pch->midi note))
    (midi->midic (pch->midi note diap))))


;(defmethod pch->itvl (note)
(om::defmethod! pch->itvl (note)
  :icon '(141)
  (semitones->itvl(pch->semitones note)))

;(defmethod pch->semitones (note)
(om::defmethod! pch->semitones (note)
  :icon '(141)
  (midi->semitones(pch->midi note)))


;(defmethod pch->pch-class ((note cons)) ; if a cons it is a pitch with deviation
(om::defmethod! pch->pch-class ((note cons)) ; if a cons it is a pitch with deviation
  :icon '(141)
  (if (numberp (cdr note))                ; IF it is a cons with deviation
    (if (pitch-without-octave-p note)             ; if there is no octave, add 48 so as
      (+ 48 (midi->pch-class (pch->midi note)))   ;    to start the list at 0 (DO4)
      (midi->pch-class (pch->midi note)))
    (mapcar #'pch->pch-class note)))      ; ELSE it is a list of notes

;(defmethod pch->pch-class ((note t)) ; default: single pitch without deviation
(om::defmethod! pch->pch-class ((note t)) ; default: single pitch without deviation
  :icon '(141)
  (if (pitch-without-octave-p note)             ; if there is no octave, add 48 so as
    (+ 48 (midi->pch-class (pch->midi note)))   ;    to start the list at 0 (DO4)
    (midi->pch-class (pch->midi note))))


; COMPOUND CONVERSIONS, ms0906
;(defmethod pch->ratio (l)
(om::defmethod! pch->ratio (l)
  :icon '(141)
  (semitones->ratio (pch->semitones l)))

#|
(pch->fq '(la4 (12 34 re3) (211 323 (323)) a4))
(pch->fq '("REd4" "Eb4"))
(pch->fq "LA4" )
(pch->fq 'DO4 )
(pch->fq '(ut2 do2))
(pch->fq '((red4 . 10) mib4) 443.0)
(pch->fq '( red4 (red4 . 10) red4))
(pch->fq '((sol2 . -50) (fad2 . 50)))
(pch->fq 100 )
(pch->fq '(REb2 . q))
(pch->fq 'REb2 )
(pch->fq '((DO . 12) (LAb . -5) 100 RE1 SOL1 (REb2 . -q)))

(pch->itvl '(DO MI))
(pch->itvl '((DO . 12) (LAb . -5) 100 RE1 SOL1 (REb2 . -q)))

(pch->midi 'A4)
(pch->midi '(A4 (la3 . q) si2))

(pch->midic 'A4)
(pch->midic '(A4 (la3 . q) si2))

(pch->semitones '(A4 A5))
(pch->semitones '(A4 A5 Cd6 mi6 (sol6 . 13) (la6 . q) si6))

(pch->pch-class 'Si4)
(pch->pch-class 'Si3)
(pch->pch-class 'LA)
(pch->pch-class '(DO RE MI FA))

;compound, new 2009
(pch->ratio '(do4 sol4))
(pch->ratio '(do4 (sol4 . 2)))
(pch->ratio '((do4 . 25) (sol4 . -25)))
(pch->ratio '(A4 A5 Cd6 mi6 (sol6 . 13) (la6 . q) si6))

|#


;MIDI
;(defmethod midi->pch ((p number) &optional (approx 0))
(om::defmethod! midi->pch ((p number) &optional (approx 0))
  :icon '(141)
; (setf p (from-midic p)) suppressed, Marco 0907, input can only be MIDI
 (let ((*package* (find-package :chroma))
       (octave (floor (+ p .5) 12))
       (fnote (mod p 12)))
    (multiple-value-bind(note deviation)(round fnote)
      (if (eq approx 0)
        (cons (format () "~a~a"(cdr(assoc note *SCPITCH-ALIST*)) (- octave 1)) (* deviation 100))
        (if (numberp approx)
          (cons(format () "~a~a"(cdr(assoc note *SCPITCH-ALIST*)) (- octave 1))
               (float(*(round(/(* 100 deviation) approx))approx)))
          (format () "~a~a"(cdr(assoc note *SCPITCH-ALIST*)) (- octave 1)))
        ))))

;(defmethod midi->pch ((p list) &optional (approx 0))
(om::defmethod! midi->pch ((p list) &optional (approx 0))
  :icon '(141)
  (mapcar #'(lambda (x) (midi->pch x approx)) p))


;(defmethod midi->semitones ((l list))
(om::defmethod! midi->semitones ((l list))
  :icon '(141)
 (cond((null (second l))())
       (t( cons (- (from-midic (second l)) (from-midic (first  l))) (midi->semitones (cdr l))))))

;(defmethod midi->semitones ((l t))
(om::defmethod! midi->semitones ((l t))
  :icon '(141)
  (error "I NEED A LIST OF AT LEAST TWO ARGUMENTS, SIR AND YOU GAVE ME ~a" l))


;(defmethod midi->pch-class ((note number))
(om::defmethod! midi->pch-class ((note number))
  :icon '(141)
 (- (from-midic (pch-reduce note)) 60))

;new, ms0907
(defun pch-reduce (val)
; reduce midi value between 60 and 71 for the conversion to pch-class
  (if (< val 60)
      (let ((curr val))
        (loop while (< curr 60) do
              (setf curr (+ curr 12))))
;        (print curr))
      (let ((curr val))
        (loop while (> curr 71) do
              (setf curr (- curr 12)))
        curr)
      ))

;(pch-reduce 49)
;(midi->pch-class 73)
;(midi->pch-class '(49 73 77))

(defmethod midi->pch-class ((l list))
;(om::defmethod! midi->pch-class ((l list))
  (mapcar #'midi->pch-class l))


(defmethod from-midic ((midi number))
;(om::defmethod! from-midic ((note number))
  (if (< midi 127) midi
      (/ midi 100.0)))
;    (round (/ midi 100.0)))) ; WRONG VERSION, corrected by Marco, Feb 21, 2000

(defmethod from-midic ((midi list))
  (mapcar #'from-midic midi))

;(defmethod midi->midic ((l number))
(om::defmethod! midi->midic ((l number))
  :icon '(141)
  (round (* (from-midic l) 100)))

;(defmethod midi->midic ((l list))
(om::defmethod! midi->midic ((l list))
  (mapcar #'midi->midic l))


; COMPOUND CONVERSIONS, ms0906
(om::defmethod! midi->fq (l)
  :icon '(141)
  (pch->fq (midi->pch l)))

;(defmethod midi->ratio (l)
(om::defmethod! midi->ratio (l)
  :icon '(141)
  (semitones->ratio (midi->semitones l)))

;(defmethod midi->itvl (l)
(om::defmethod! midi->itvl (l)
  :icon '(141)
  (semitones->itvl (midi->semitones l)))

#|
(midi->pch 69)
(midi->pch 69.5)
(midi->pch '(69 70 70.3 70.5))

(midi->midic 60)
(midi->midic '(60 61 62 63))

(midi->semitones '(60 72))
(midi->semitones '(60 67 72 76 79 82 84))
(midi->semitones '(60 66.5 72 75.5 79 83.5 84))

;compound, new0906
(midi->fq 69)
(midi->fq '(69 (70 71 72) 60 60.5 60.333))

(midi->ratio '(60 72))
(midi->ratio '(60 67 72 76 79 82 84))
(midi->ratio '(60 66.5 72 75.5 79 83.5 84))

(midi->itvl '(60 72))
(midi->itvl '(60 67 72 76 79 82 84))
(midi->itvl '(60 66.5 72 75.5 79 83.5 84))

|#


;MIDIC
;(defmethod midic->midi ((l number))
(om::defmethod! midic->midi ((l number))
  :icon '(141)
  (/ l 100.0))

(om::defmethod! midic->midi ((l list))
  :icon '(141)
  (mapcar #'midic->midi l))


; COMPOUND CONVERSIONS, ms 0906
;(defmethod midic->fqmidic- (l)
(om::defmethod! midic->fq (l)
  :icon '(141)
  (midi->fq (midic->midi l)))

;(defmethod midic->pch (l)
(om::defmethod! midic->pch (l)
  :icon '(141)
  (midi->pch (midic->midi l)))

;(defmethod midic->ratio (l)
(om::defmethod! midic->ratio (l)
  :icon '(141)
  (midi->ratio (midic->midi l)))

;(defmethod midic->itvl (l)
(om::defmethod! midic->itvl (l)
  :icon '(141)
  (midi->itvl (midic->midi l)))

;(defmethod midic->semitones (l)
(om::defmethod! midic->semitones (l)
  :icon '(141)
  (midi->semitones (midic->midi l)))

;(defmethod midic->pch-class (l)
(om::defmethod! midic->pch-class (l)
  :icon '(141)
  (midi->pch-class (midic->midi l)))

#|
(midic->midi 6900)
(midic->midi 6950)
(midic->midi '(6933 7843))

;compound, 0906
(midic->pch 6900)
(midic->pch 6950)
(midic->pch '(6900 7000 7030 7050))

(midic->semitones '(6000 7200))
(midic->semitones '(6000 6700 7200 7600 7900 8200 8400))
(midic->semitones '(6000 6650 7200 7550 7900 8350 8400))

(midic->fq 6900)
(midic->fq '(6900 (7000 7100 7200) 6000 6050 6033.3))

(midic->ratio '(6000 7200))
(midic->ratio '(6000 6700 7200 7600 7900 8200 8400))
(midic->ratio '(6000 6650 7200 7550 7900 8350 8400))

(midic->itvl '(6000 7200))
(midic->itvl '(6000 6700 7200 7600 7900 8200 8400))
(midic->itvl '(6000 6650 7200 7550 7900 8350 8400))

|#


;RATIO
;(defmethod ratio->fq ((int list) (ref number))
(om::defmethod! ratio->fq ((int list) (ref number))
  :icon '(141)
 (if (null int)
     (list ref)  
   (cons ref (ratio->fq (cdr int) (* ref  (car int))))))

;(defmethod ratio->fq ((int number) (ref number))
(om::defmethod! ratio->fq ((int number) (ref number))
  :icon '(141)
  (ratio->fq (list int) ref))

;(defmethod ratio->fq (int ref)
(om::defmethod! ratio->fq (int ref)
  :icon '(141)
  (error "MYSTERIOUS ARGUMENTS, SIR ~a: ~a - ~a" (get-gbl USER) int ref))

;(defmethod ratio->itvl (int)
(om::defmethod! ratio->itvl (int)
  :icon '(141)
 (semitones->itvl(ratio->semitones int)))


;(defmethod ratio->semitones ((l list))
(om::defmethod! ratio->semitones ((l list))
  :icon '(141)
 (mapcar #'ratio-to-semitones l))

;(defmethod ratio->semitones ((l number))
(om::defmethod! ratio->semitones ((l number))
  :icon '(141)
     (ratio-to-semitones l))

;(defmethod ratio->semitones (l)
(om::defmethod! ratio->semitones (l)
  :icon '(141)
  (error "MYSTERIOUS ARGUMENT, SIR: ~a" l))

(defmethod ratio-to-semitones (ratio)
;(om::defmethod! ratio-to-semitones (ratio)
  (* 12 (log ratio 2)))


; COMPOUND CONVERSIONS, ms 0906
(om::defmethod! ratio->midi (val ref)
  :icon '(141)
  (itvl->midi (let ((it (ratio->itvl val)))
                (if (listp (car it))
                    it
                  (list it))) ref))

(om::defmethod! ratio->midic (val ref)
  :icon '(141)
  (itvl->midic (let ((it (ratio->itvl val)))
                 (if (listp (car it))
                     it
                   (list it))) ref))

(om::defmethod! ratio->pch (val ref &optional (approx 0))
  :icon '(141)
  (itvl->pch (let ((it (ratio->itvl val)))
                 (if (listp (car it))
                     it
                   (list it))) ref approx))

#|
(ratio->fq  '(2 2 2) 100)
(ratio->itvl 2)
(ratio->itvl '(2 2 1 3/2))
(ratio->semitones '(2 2 1 3/2))
;compound, new 2009
(ratio->midi 1.5 60)
(ratio->midi '(1 2 4) 60)

(ratio->midic '(1.5) 6000)
(ratio->midic 1.5 6000)
(ratio->midic '(1 2 4) 6000)

(ratio->pch '(1.5) 'do4)
(ratio->pch '(1 2 4) 'do4)

(ratio->pch 1.567 'do3)
(ratio->pch 1.567 'do3 50)
(ratio->pch '(1.0432 2.1232 3.452) 'do4)
|#


;INTERVAL
;(defmethod itvl->fq (int (ref number))
(om::defmethod! itvl->fq (int (ref number))
  :icon '(141)
  (let ((deviation 0)(octave 0)(intervalle)(freq)
        (int (if (listp int) int (list int))))
    (if (null int) (list ref)
        (progn (if (memberp (car int) *INTERVALLES*) 
                 (progn (setf intervalle (car int))
                        (setf deviation 0)
                        (setf octave 0))
                 (progn (if (listp (car int))
                          (setf intervalle (caar int))
                          (error "UNREALISTIC INTERVAL, SIR: ~a" (car int)))
                        (unless (memberp intervalle *INTERVALLES*)
                          (error "CAN'T BELIEVE 'THIS IS AN INTERVAL: ~a" intervalle))
                        (if (null (third (car int)))
                          (progn (setf deviation 0)
                                 (setf octave (second (car int))))
                          (progn (setf octave (second (car int)))
                                 (setf deviation (third (car int)))))))
               (when (symbolp intervalle)
                 (setf intervalle (internc intervalle)))
               (setf freq 
                     (* ref (expt 2 (/ (+ (cdr (assoc intervalle *INTERVALLES-ALIST*))
                                          (/ deviation 100) 
                                          (* 12  octave)) 12))))
               (cons ref (itvl->fq (cdr int) freq)
                     )))))

;(defmethod itvl->fq (int (ref t))
(om::defmethod! itvl->fq (int (ref t))
  :icon '(141)
  (declare (ignore int))
          (error "CAN'T UNDERSTAND YOUR REFERENCE FREQUENCY: ~a" ref))

 
;(defmethod itvl->midi (int (ref number))
(om::defmethod! itvl->midi (int (ref number))
  :icon '(141)
 (let ((deviation 0)(octave 0)(intervalle)(pitch)
        (int (if (listp int) int (list int))))
    (if (null int) (list ref)
        (progn (if (memberp (car int) *INTERVALLES*) 
                 (progn (setf intervalle (car int))
                        (setf deviation 0)
                        (setf octave 0))
                 (progn (if (listp (car int))
                          (setf intervalle (caar int))
                          (error "UNREALISTIC INTERVAL, SIR: ~a" (car int)))
                        (unless (memberp intervalle *INTERVALLES*)
                          (error "CAN'T BELIEVE 'THIS IS AN INTERVAL: ~a" intervalle))
                        (if(null (third (car int)))
                          (progn (setf deviation 0)
                                 (setf octave (second (car int))))
                          (progn (setf octave (second (car int)))
                                 (setf deviation (third (car int)))))))
               (when (symbolp intervalle)
                 (setf intervalle (internc intervalle)))
               (setf pitch  (+ ref 
                               (cdr(assoc intervalle *INTERVALLES-ALIST*))
                               (/ deviation 100) 
                               (* 12  octave )))
               (cons ref (itvl->midi (cdr int) pitch)
                     )))))

;(defmethod itvl->midi (int (ref t))
(om::defmethod! itvl->midi (int (ref t))
  :icon '(141)
  (itvl->midi (ratio->itvl val) ref))


;(defmethod itvl->midic (int (ref number))
(om::defmethod! itvl->midic (int (ref number))
  :icon '(141)
 (midi->midic (itvl->midi int (midic->midi ref))))


;(defmethod itvl->midic (int (ref t))
(om::defmethod! itvl->midic (int (ref t))
  :icon '(141)
  (itvl->midic int (pch->midi ref)))


;(defmethod itvl->ratio (int)
(om::defmethod! itvl->ratio (int)
  :icon '(141)
 (semitones->ratio (itvl->semitones int)))

;(defmethod itvl->pch (int note &optional (approx 0))
(om::defmethod! itvl->pch (int note &optional (approx 0))
  :icon '(141)
; if approx is needed, go through a frequency conversion (less precise)
;    otherwise use a midi conversion
 (if (and (numberp approx) (= approx 0))
    (midi->pch (itvl->midi int (pch->midi note)))
    (fq->pch (itvl->fq int (pch->fq note)) approx)))


;(defmethod itvl->semitones (int)
(om::defmethod! itvl->semitones (int)
  :icon '(141)
 (let ((deviation 0)(octave 0)(intervalle)(semitones)
        (int (if (listp int) int (list int))))
    (if (null int) ()
        (progn (if (memberp (car int) *INTERVALLES*) 
                 (setf intervalle (car int) deviation 0 octave 0)
                 (progn (if (listp (car int))
                          (setf intervalle (caar int))
                          (error "UNREALISTIC INTERVAL, SIR: ~a" (car int)))
                        (unless (memberp intervalle *INTERVALLES*)
                          (error "CAN'T BELIEVE 'THIS IS AN INTERVAL: ~a" intervalle))
                        (if (null (third (car int)))
                          (setf deviation 0 octave (second (car int)))
                          (setf octave (second (car int)) deviation (third (car int))))))
               (when (symbolp intervalle)
                 (setf intervalle (internc intervalle)))
               (setf semitones (+
                                (cdr(assoc intervalle *INTERVALLES-ALIST*))
                                (/ deviation 100) 
                                (* 12  octave )))
               (cons semitones (itvl->semitones (cdr int))
                     )))))

#|
(itvl->fq '((1 1)(2- 0)) 100)
(itvl->ratio '((1 0) (2- 0 2) (1 1) 5))
(itvl->semitones '((1 0) (2- 0 2) (1 1) 5))
(itvl->semitones '(2- -3+ 5))
(itvl->semitones '((2- 1) (3+ 1 50) (5 0 33)))
(itvl->semitones '((2- 1)))

(itvl->pch '((2- 1)) 'do4)
(itvl->pch '((2- 1 33)) 'do4)
(itvl->pch '((2- 1 33)) 'do4 50)
(itvl->pch '((2- 1)) 'do4)

(itvl->midi '((2- 1)) 60)
(itvl->midi '((2- 1)) 60)
(itvl->midi '((2- 1 33)) 60)
(itvl->midi '((2- 1 78) (2- 0 78)) 60)

(itvl->midic '((2- 1)) 6000)
(itvl->midic '((2- 1 33)) 6000)
(itvl->midic '((2- 1 78) (2- 0 78)) 6000)
(itvl->midic '((2- 1)) 6000)
|#


;SEMITONES
;(defmethod semitones->ratio ((l list))
(om::defmethod! semitones->ratio ((l list))
  :icon '(141)
 (mapcar #'semitones-to-ratio l))

;(defmethod semitones->ratio ((l number))
(om::defmethod! semitones->ratio ((l number))
  :icon '(141)
  (semitones-to-ratio l))

;(defmethod semitones->ratio (l)
(om::defmethod! semitones->ratio (l)
  :icon '(141)
  (error "CANNOT TRUST YOUR ARGUMENT: ~a" l))

(defmethod semitones-to-ratio (ratio)
     (expt 2 (/ ratio 12)))

;(defmethod semitones->itvl ((l list))
(om::defmethod! semitones->itvl ((l list))
  :icon '(141)
 (mapcar #'semitones-to-itvl l))

;(defmethod semitones->itvl ((l number))
(om::defmethod! semitones->itvl ((l number))
  :icon '(141)
  (semitones-to-itvl l))

;(defmethod semitones->itvl (l)
(om::defmethod! semitones->itvl (l)
  :icon '(141)
  (error "MIND YOUR ARGUMENT, PLEASE: ~a" l))

(defmethod semitones-to-itvl (i)
  (multiple-value-bind (int deviation)(round i)
    (multiple-value-bind (octave interval)(floor int 12)
      (if (<(abs deviation) 0.00001)
        (list(car(rassoc interval *INTERVALLES-ALIST*)) octave)
      (list(car(rassoc interval *INTERVALLES-ALIST*))
           octave
           (round(* 100 deviation)))))))


; COMPOUND CONVERSIONS, ms 0906
(om::defmethod! semitones->fq (val ref)
  :icon '(141)
  (itvl->fq (let ((it (semitones->itvl val)))
              (if (listp (car it))
                  it
                (list it))) ref))


(om::defmethod! semitones->pch (val ref &optional (approx 0))
  :icon '(141)
  (itvl->pch (let ((it (semitones->itvl val)))
               (if (listp (car it))
                   it
                 (list it))) ref approx))


(om::defmethod! semitones->midi (val ref)
  :icon '(141)
  (itvl->midi (let ((it (semitones->itvl val)))
                (if (listp (car it))
                    it
                  (list it))) ref))

(om::defmethod! semitones->midic (val ref)
  :icon '(141)
  (itvl->midic (let ((it (semitones->itvl val)))
                (if (listp (car it))
                    it
                  (list it))) ref))


#|
(semitones->ratio 12)
(semitones->ratio '(12))
(semitones->ratio '(7 12 14))

(semitones->itvl 5)
(semitones->itvl '(5))
(semitones->itvl '(5 6))

;compound, new 2009
(semitones->fq 12 100)
(semitones->fq '(12) 100)
(semitones->fq '(7 12 13) 100)

(semitones->pch 12 'la4)
(semitones->pch '(12) 'la4)
(semitones->pch '(7 12 13) 'la4)
(semitones->pch '(7 12 13) '(la4 . 33))
(semitones->pch '(7 12 13) '(la4 . 33) 50)

(semitones->midi 12 69)
(semitones->midi '(12) 69)
(semitones->midi '(7 12 13) 69)

(semitones->midic 1200 6950)
(semitones->midic '(12) 6900)
(semitones->midic '(7 12 13) 6900)

(itvl->midic '5+ 60)
(midi->midic (itvl->midi '5+ 60))


|#

;PITCH CLASSES
;(defmethod pch-class->pch ((l list) &optional (approx 0))
(om::defmethod! pch-class->pch ((l list) &optional (approx 0))
  :icon '(141)
 (mapcar #'(lambda (x) (pch-class->pch x approx)) l))

;(defmethod pch-class->pch ((l number) &optional (approx 0))
(om::defmethod! pch-class->pch ((l number) &optional (approx 0))
  :icon '(141)
  (midi->pch (pch-class->midi l) approx))

(om::defmethod! pch-class->pch ((l t) &optional (approx 0))
  :icon '(141)
  (declare (ignore approx))
  (error "I HAVE SOME PROBLEMS WITH YOUR ARGUMENT: ~a~%" l))


;(defmethod pch-class->midi ((l list))
(om::defmethod! pch-class->midi ((l list))
  :icon '(141)
 (mapcar #'pch-class->midi l))

;(defmethod pch-class->midi ((l number))
(om::defmethod! pch-class->midi ((l number))
  :icon '(141)
  (+ l 60))

;(defmethod pch-class->midi ((l t))
(om::defmethod! pch-class->midi ((l t))
  :icon '(141)
  (error "MIND YOUR ARGUMENT, PLEASE: ~a" l))


;(defmethod pch-class->fq ((l list))
(om::defmethod! pch-class->fq ((l list))
  :icon '(141)
 (mapcar #'pch-class->fq l))

;(defmethod pch-class->fq ((l number))
(om::defmethod! pch-class->fq ((l number))
  :icon '(141)
  (pch->fq (pch-class->pch l)))

;(defmethod pch-class->fq ((l t))
(om::defmethod! pch-class->fq ((l t))
  :icon '(141)
  (error "I HAVE SOME PROBLEMS WITH YOUR ARGUMENT: ~a~%" l))

#|

|#
