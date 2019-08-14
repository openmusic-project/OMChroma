;******************************************************************
(in-package :om)
;------------------------------------------------------------------

; THIS FILE CONTAINS SOME USEFUL METHODS USED
;    BY THE GENERALIZED USER FUN

;******************************************************************

#|
BEHAVIOUR OF GEN-USER-FUN
;test
a list of functions of 1 argument (component)
outputs
- a component (modified or the same) - the test is successful
- a string                           - the test fails, the string can explains the reason
- a list                             - If the car of the list is a component the test is successful else
                                     - the test fails. All elements of the list will be written in the scr file.
- anything                           - the test fails

;sub-comp
a list of functions of 1 argument (component)
outputs
- a list of component or strings (the list can be empty NIL)
All elements of the list will be written in the scr file

;sub-test
a list of functions of 1 argument (component)
outputs
- a component (modified or the same) - the test is successful
- a string                           - the test fail, the string can explain the reason
- a list                             - If the car of the list is a component the test is successful else
                                     - the test fail. All elements of the list will be written in the scr file
- anything                           - the test fail

;remove-fields
a list of strings for fields that will not be written in the scr file
|#

;******************************************************************
; FUNCTIONS
;******************************************************************

;------------------------------------------------------------------
;	sub-comps
;	sub-comps+
;------------------------------------------------------------------
;	tra-comps
;	tra-comps+
;------------------------------------------------------------------
;	wt-comps
;	(wt-comps+)
;	write-WT-fun
;------------------------------------------------------------------


(defun sub-comps (c)
"
Compute <npart> sub-components with detuning <ston>.
SIMPLE VERSION: uses only <npart>, <ston> and <fq>.
"
  (append
   (list
    (format () " Component No. ~D" (index c)))
   (get-subcomps c)))

;;; *
(defun get-subcomps (c)
  (let ((npart (comp-field c "npart"))
        (ston (comp-field c "ston"))
        (fq (comp-field c "freq")))
    (if (>= ston 0.0)
      (cons
       (format () ";    ~D sub-components (lin freq)" (floor npart))
       (fq-lin-dev c (floor npart) ston fq))
      (cons
       (format () ";    ~D sub-components (itvl freq)" (floor npart))
       (fq-log-dev c (floor npart) (abs ston) fq)))))


;;; *
(defun fq-lin-dev (c npart ston fq)
  (loop for i from 1 to npart
        collect (comp-field (clone c) "freq" (lin-fq fq ston)) into c-list
        finally (return (append c-list		; add 2 empty lines at the end
                               (list (format () "~%"))))))
;;; *
(defun fq-log-dev (c npart ston fq)
  (loop for i from 1 to npart
        collect (comp-field (clone c) "freq" (log-fq fq ston)) into c-list
        finally (return (append c-list		; add 2 empty lines at the end
                               (list (format () "~%"))))))


;;; *
(defun sub-comps+ (c)
"
Compute <npart> sub-components with detuning <ston>.
COMPLETE VERSION: uses not only <npart>, <ston> and <fq>.
   but also <ed2>, <dur2>, <amp2>.
Behaviour:
<current-ed> = e-dels[i] + ed2[i]*k (k = order of sub-component)
   If <current-ed> > durtot[e] - durmin, STOP the loop and return what is possible

<current-dur> = dur2[i] * durtot. If dur2[i] = < 0.0, <current-dur> = durs[i]
<current-amp> = amp2[i] * amptot. If amp2[i] = < 0.0, <current-amp> = amp[i]

ATTENTION
The following tests are performed within this function and ought not to be performed
   in the list of sub-tests (drawback: the sub-component cannot be discarded, and
   the tests will try to find a solution if an error is detected. No error message will
   appear in the score as a result of such tests.

IMPORTANT
The list of subtests should not contain the following subtests: compute-dur!,
   ed-durmin?, amp?, compute-amp!
The following subtests are however recommended: s-fq-sr?, s-fq-min?

"
  (append
   (list
    (format () " Sub-comps+: Component No. ~D" (index c)))
   (get-subcomps+ c)))


(defun get-subcomps+ (c)
  (declare (special cr::durmin))  
  (when (> (comp-field c "npart") 0.0) ; go on only if it is needed!

    (let ((npart (comp-field c "npart"))
          (ston (comp-field c "ston"))
          (fq (comp-field c "freq"))
          (ed (comp-field c "e-dels"))
          (ed2 (comp-field c "ed2"))
          (dur2 (comp-field c "dur2"))
          (amp2 (comp-field c "amp2"))
          (mindur (if (find-package 'cr)	; IF CHROMA WAS LOADED, USE ITS DEFAULT
                  (cr::get-gbl 'cr::durmin)
                  0.01)))

; PERFORM SIMPLE TESTS

; if dur/amp2 = < 0.0, get the value from durs/amp, otherwise compute it
; CHECK (and eventually CORRECT) only amp
      (if (>= dur2 0.0)
        (setf dur2 (* dur2 (durtot (comp-array c))))
        (setf dur2 (comp-field c "durs")))
      (if (>= amp2 0.0)
        (setf amp2 (* (check-amp2 amp2) (amptot (comp-array c))))
        (setf amp2 (comp-field c "amp")))

      (if (>= ston 0.0)
        (cons
         (format () ";    ~D sub-components (lin freq)" (floor npart))
         (fq-lin-dev+ c (floor npart) ston fq ed ed2 dur2 amp2 mindur))
        (cons
         (format () ";    ~D sub-components (itvl freq)" (floor npart))
         (fq-log-dev+ c (floor npart) (abs ston) fq ed ed2 dur2 amp2 mindur))))))

;;; *
(defun fq-lin-dev+ (c npart ston fq ed ed2 dur2 amp2 mindur)
  (let ((continue? t) (result))
    (loop for i from 1 to npart
          while continue? do
          (let ((curr-comp (clone c))
                (curr-ed (+ ed (* ed2 i)))
                (dtot (durtot (comp-array c))))
            (if (> curr-ed (- dtot mindur))
              (progn (setf continue? ())
                     (push (format () ";    Sub-comps+ (fq-lin-dev+): ed2 (~a) too large for durtot = ~a and durmin = ~a
                  Generation of sub-components interrupted at sub-comp n. ~a~%" curr-ed dtot mindur i)
                    result))
              (push (progn
                      (comp-field curr-comp "freq" (lin-fq fq ston))
                      (comp-field curr-comp "e-dels" curr-ed)
                      (comp-field curr-comp "durs" (check-dur2 dur2 dtot curr-ed))
                      (comp-field curr-comp "amp" amp2)) result))))
              
    (append (reverse result)		; add 2 empty lines at the end
            (list (format () "~%")))))

;;; *
(defun fq-log-dev+ (c npart ston fq ed ed2 dur2 amp2 mindur)
  (let ((continue? t) (result))
    (loop for i from 1 to npart
          while continue? do
          (let ((curr-comp (clone c))
                (curr-ed (+ ed (* ed2 i)))
                (dtot (durtot (comp-array c))))
            (when (> curr-ed (- dtot mindur))
              (setf continue? ())
              (push (format () ";    Sub-comps+: ed2 (~a) too large for durtot = ~a and durmin = ~a
                  Generation of sub-components interrupted at sub-comp n. ~a~%" curr-ed dtot mindur i)
                    result))
            (when continue?
              (push (progn
                      (comp-field curr-comp "freq" (log-fq fq ston))
                      (comp-field curr-comp "e-dels" curr-ed)
                      (comp-field curr-comp "durs" (check-dur2 dur2 dtot curr-ed))
                      (comp-field curr-comp "amp" amp2)) result))))
              
    (append (reverse result)		; add 2 empty lines at the end
            (list (format () "~%")))))

; secondary functions
(defun check-amp2 (a)
; added, ms 080130
  (let ((maxamp (if (find-package 'cr)	; IF CHROMA WAS LOADED, USE ITS DEFAULT
                  (cr::get-gbl 'cr::maxamp)
                  1.0)))
    (if (> a maxamp)
      maxamp
      a)))
; endofadded
;(check-amp2 1345)

(defun check-dur2 (dur dtot ed)
  (if (<= (+ ed dur) dtot)
    dur
    (- dtot ed)))


(defun lin-fq (fq ston)
  (+ fq (* fq (cr::ran ston))))

(defun log-fq (fq ston)
  (* fq (cr::semitones->ratio (cr::ran (abs ston)))))

(defun log2-fq (fq ston)
  (+ fq (* (log fq 2) (cr::ran ston))))


;------------------------------------------------------------------
(defun tra-comps (c)
"
TRAIETTORIA VERSION
Compute <npart> sub-components with detuning <ston>.
SIMPLE VERSION USED FOR TRAIETTORIA: uses only <npart>, <ston> and <fq>.

TRAIETTORIA'S SPECIFICITY WITH STON:
if < 0 => linear frequency deviation (0:1 = 0:100% of freq)
if > 0 => maximum random multiplier of log(2) freq
"
  (append
   (list
    (format () " Component No. ~D" (index c)))
   (get-subcomps-tra c)))

;;; *
(defun get-subcomps-tra (c)
  (let ((npart (comp-field c "npart"))
        (ston (comp-field c "ston"))
        (fq (comp-field c "freq")))
    (if (<= ston 0.0)
      (cons
       (format () ";    ~D sub-components (lin freq for Traiettoria)" (floor npart))
       (fq-lin-dev c (floor npart) (abs ston) fq))
      (cons
       (format () ";    ~D sub-components (log2 freq for Traiettoria)" (floor npart))
       (fq-log2-dev c (floor npart) (abs ston) fq)))))

;;; *
(defun fq-log2-dev (c npart ston fq)
  (loop for i from 1 to npart
        collect (comp-field (clone c) "freq" (log2-fq fq ston)) into c-list
        finally (return (append c-list		; add 2 empty lines at the end
                               (list (format () "~%"))))))


(defun tra-comps+ (c)
"
TRAIETTORIA VERSION
Compute <npart> sub-components with detuning <ston>.
COMPLETE VERSION: uses not only <npart>, <ston> and <fq>.
   but also <ed2>, <dur2>, <amp2>.
Behaviour:
<current-ed> = e-dels[i] + ed2[i]*k (k = order of sub-component)
   If <current-ed> > durtot[e] - durmin, STOP the loop and return what is possible

<current-dur> = dur2[i] * durtot. If dur2[i] = (), <current-dur> = durs[i]
<current-amp> = amp2[i] * amptot. If amp2[i] = (), <current-amp> = amp[i]

TRAIETTORIA'S SPECIFICITY WITH STON:
if < 0 => linear frequency deviation (0:1 = 0:100% of freq)
if > 0 => maximum random multiplier of log(2) freq


ATTENTION
The following tests are performed within this function and ought not to be performed
   in the list of sub-tests (drawback: the sub-component cannot be discarded, and
   the tests will try to find a solution if an error is detected. No error message will
   appear in the score as a result of such tests).

IMPORTANT
The list of subtests should not contain the following subtests: compute-dur!,
   ed-durmin?, amp?, compute-amp!
The following subtests are however recommended: s-fq-sr?, s-fqmin?

"
  (append
   (list
    (format () " Tra-comps+: Component No. ~D" (index c)))
   (get-tracomps+ c)))


;;; *
(defun get-tracomps+ (c)
  (declare (special cr::durmin))  
  (when (> (comp-field c "npart") 0.0) ; go on only if it is needed!

    (let ((npart (comp-field c "npart"))
          (ston (comp-field c "ston"))
          (fq (comp-field c "freq"))
          (ed (comp-field c "e-dels"))
          (ed2 (comp-field c "ed2"))
          (dur2 (comp-field c "dur2"))
          (amp2 (comp-field c "amp2"))
          (mindur (if (find-package 'cr)	; IF CHROMA WAS LOADED, USE ITS DEFAULT
                  (cr::get-gbl 'cr::durmin)
                  0.01)))

; PERFORM SIMPLE TESTS

; if dur/amp2 = -1, get the value from durs/amp, otherwise compute it
; CHECK (and eventually CORRECT) only amp
      (if (>= dur2 0.0)
        (setf dur2 (* dur2 (durtot (comp-array c))))
        (setf dur2 (comp-field c "durs")))
      (if (>= amp2 0.0)
        (setf amp2 (* (check-amp2 amp2) (amptot (comp-array c))))
        (setf amp2 (comp-field c "amp")))

      (if (<= ston 0.0)
        (cons
         (format () ";    ~D sub-components (lin freq for Traiettoria)" (floor npart))
         (fq-lin-dev+ c (floor npart) (abs ston) fq ed ed2 dur2 amp2 mindur))
        (cons
         (format () ";    ~D sub-components (log2 freq for Traiettoria)" (floor npart))
         (fq-log2-dev+ c (floor npart) ston fq ed ed2 dur2 amp2 mindur))))))

;;; *
(defun fq-log2-dev+ (c npart ston fq ed ed2 dur2 amp2 mindur)
  (let ((continue? t) (result))
    (loop for i from 1 to npart
          while continue? do
          (let ((curr-comp (clone c))
                (curr-ed (+ ed (* ed2 i)))
                (dtot (durtot (comp-array c))))
            (when (> curr-ed (- dtot mindur))
              (setf continue? ())
              (push (format () ";    Tra-comps+: ed2 (~a) too large for durtot = ~a and durmin = ~a
                  Generation of sub-components interrupted at sub-comp n. ~a~%" curr-ed dtot mindur i)
                    result))
            (when continue?
              (push (progn
                      (comp-field curr-comp "freq" (log2-fq fq ston))
                      (comp-field curr-comp "e-dels" curr-ed)
                      (comp-field curr-comp "durs" (check-dur2 dur2 dtot curr-ed))
                      (comp-field curr-comp "amp" amp2)) result))))
              
    (append (reverse result)		; add 2 empty lines at the end
            (list (format () "~%")))))

;------------------------------------------------------------------
(defun wt-comps (c)
"
User fun employed with the WT object EXCLUSIVELY! It runs first the freezing
algorithm when needed, then computes the sub-components if any.

NB: All the slots but the ones concerned with WT values MUST be initialized
       before (i.e. duration should be absolute!)

Performs several operations:

1) Initialize the class's slots WPHS, WENV, WXIN, WXOUT, WFLT reading the values
      in CWT and FQWT

2) If needed, apply a freezing algorithm

3) Compute <npart> sub-components for each component (with / without freeze)
      with detuning <ston>.
   SIMPLE VERSION: uses only <npart>, <ston> and <fq>.

NB: the current component is only modified BUT NOT returned, since this function is
      called at the sub-comps level (the main component is always maintained if the
      tests are successful.
"
  (let (
        (initialize-comp (compute-WT c))
;        (freeze-list (freeze-WT c))
        )
    (append
     (list
      (format () " Component above no. ~D" (index c)))
      initialize-comp)))

;;; **
(defun compute-WT (c)

; Complicated way to evaluate the name of the WT-object in the package :cr
;    within the package :om. Thank you, Carlos, for the tips!
; Other possibility: (let (list (loop for el in list
;                                   collect (is (symbol el) (intern el :cr) el))))
; ORANGE: dynamically bound variables within the WT object in package :cr


  (let* ((curr-CWT (eval (comp-field c "cwt")))
         (curr-FQWT (eval (comp-field c "fqwt")))
         (my-WT (eval (intern (string (first curr-CWT)) :cr)))
         (my-off (+ (cr::beg-off_wt my-WT)
                    (if (second curr-CWT) (second curr-CWT) 0)))
         (my-Eoff (if (third curr-CWT)
                    (+ (third curr-CWT) (cr::beg-off_wt my-WT))
                    (cr::end-off_wt my-WT)))
         (durmx (abs (- my-Eoff my-off)))
         (sig-si (if (> my-off my-Eoff) -1.0 1.0))
         (cr::current-wt my-WT)
         (cr::current-win (cdr curr-CWT))
         (cr::current-lco (cddr curr-FQWT))
         (curr-fade (cr::fade_wt my-WT))
         (wanted-fq (eval (first curr-FQWT)))
         (ref-fq (if (numberp (second curr-FQWT)) (second curr-FQWT) (cr::freq_wt my-WT))))

    (declare (special cr::current-wt cr::current-win cr::current-lco
                      my-WT durmx sig-si my-off my-Eoff))

#|
    (format t "curr-CWT: ~a~%curr-FQWT: ~a~%my-WT: ~a~%my-off: ~a~%my-Eoff: ~a~%durmx: ~a~%
sig-SI: ~a~%curr-win: ~a~%curr-LCO: ~a~%"
            curr-CWT curr-FQWT my-WT my-off my-Eoff durmx sig-si
            cr::current-win cr::current-LCO)
    (format t "wanted-freq: ~a~%ref-freq: ~a~%" wanted-fq ref-fq)
|#

 
    (comp-field c "wxin" (car curr-fade))
    (comp-field c "wxout" (cadr curr-fade))
    (comp-field c "wflt" (cr::auto-flt_wt))
    (comp-field c "wphs" my-off)

    (if (cr::auto-is-si_wt)
      (comp-field c "freq" (* sig-si wanted-fq ref-fq))
      (comp-field c "freq" (* sig-si (/ wanted-fq ref-fq))))

    (XFCcs c)

    ))


;;; *
(defun XFCCs (c)
   "
HISTORICAL NAME: InterFaCe between a WT object and Csound
   "
   (declare (special my-WT durmx))
   (let* (
          (gen-nam (cr::file-name_wt my-WT))
;          (gen-dir (cr::dir_wt my-WT))
;          (gen-nsm (cr::n-smpls_wt my-WT))
          (cr::current-si (comp-field c "freq"))
          (dur-of-transposed-win (/ durmx (abs cr::current-si)))
          (duration-wanted (comp-field c "durs"))
;          (current-gen-no)
          (frz-mode (cr::auto-frz-mode_wt)))
     (declare (special cr::current-si duration-wanted))


; THE GLOBAL VARIABLE cr::WTL MUST HAVE ALREADY BEEN INITIALIZED BY WRITE-WT-FUN!

     (let ((found? (find gen-nam (cr::get-gbl 'cr::WTL) :test #'equal :key #'first)))
       (if found?
         (comp-field c "wenv" (third found?))
         (print (format () "Unknown table: ~a~%" (first gen-nam)))))
#|
; INITIALIZE THE WT-FUN -> MOVED INTO WRITE-WT-FUN (my-methods.lisp)

     (let ((found? (find gen-nam (cr::get-gbl 'cr::WTL) :test #'equal :key #'first)))
       (if found?
         (comp-field c "wenv" (third found?))
         (progn
           (setf current-gen-no (+ (cr::get-gbl 'cr::WTFO) (cr::get-gbl 'cr::WTIND)))
           (cr::set-gbl 'cr::WTIND (1+ (cr::get-gbl 'cr::WTIND)))
           (cr::set-gbl 'cr::WTL (cons (list gen-nam gen-dir current-gen-no gen-nsm)
                                       (cr::get-gbl 'cr::WTL))))))
|#
     (cond
      ((>= dur-of-transposed-win duration-wanted) ; NO NEED TO FREEZE, PARS OK
       nil)
      ((= frz-mode 1)                             ; NO FREEZE, START AT AT, END BEFORE
       (comp-field c "durs" dur-of-transposed-win)
       (when (cr::get-gbl 'cr::PRNFLG)
         (list
          (format () ";%%% FRZ mode 1~%;     Dur: wanted: ~6F, obtained: ~6F (untransp: ~6F)~%"
                  duration-wanted dur-of-transposed-win
                  (* dur-of-transposed-win cr::current-si)))))
      ((= frz-mode -1)                            ; NO FREEZE, START LATER, END AT END OF NOTE
       (comp-field c "e-dels" (+ (comp-field c "e-dels")
                                 (- duration-wanted dur-of-transposed-win)))
       (comp-field c "durs" dur-of-transposed-win)
       (when (cr::get-gbl 'cr::PRNFLG)
         (list
          (format () ";%%% FRZ mode -1~%;     Dur: wanted: ~6F, obtained: ~6F (untransp: ~6F)~%"
                  duration-wanted dur-of-transposed-win
                  (* dur-of-transposed-win cr::current-si)))))
      (t
       (print (format () "~%Freezing comp. ~D: Wanted dur: ~6F, max dur: ~6F"
               (index c) duration-wanted dur-of-transposed-win))
       (if (cr::get-gbl 'cr::PRNFLG) ; IF NO PRINT-FLAG, PRINT MINIMUM INFORMATION IN SCORE
         (freeze-WT c)
         (append (list
                  (format () "~%; %%%Freezing comp. ~D: Wanted dur: ~6F, max dur: ~6F"
                          (index c) duration-wanted dur-of-transposed-win))
                  (freeze-WT c)))))
  ))


;*****************************************************************************
;-------|              MUSICAL FREEZE ALGORITHM            -------|
;-----------------------------------------------------------------------------
;-------| Designed by Marco Stroppa 
;-------| Implemented in LeLisp by Jan Vandenheede, IRCAM 1991
;-------| Ported to Common Lisp by Serge Lemouton
;-------| Adapted to omChroma by Marco Stroppa, IRCAM 020423
;-------| Version: Apr 23, 2002
;*****************************************************************************

; added 91 08 19, Jan Vandenheede

; Important remark:
; Normally all the time-values (inc-pt, first-xout, last-xin, skip, dur, end-, xin, 
; xout) delivered by the auto-freeze-field_wt selectors,
; are AFTER transposition !!!
; This explains the multiplication by the sample-increment in the calculation of
; the phase.

(defun freeze-WT (c)
  (declare (special my-off my-Eoff sig-si cr::current-si duration-wanted
                    cr::current-wt cr::current-win))
  (catch 'endoffreeze
    (let* (
           (vo (clone c))
           (cr::current-trelative 0.)
           (cr::current-tnorm 0.)
           inc-pt
           first-xout
           cr::last-xin
           skip
           cr::next-dur
           end-
           xin 
           xout
           ampfac
           index
           adaptation-flag
           (start-of-transposed-win (/ my-off (abs cr::current-si)))
           (end-of-transposed-win (/ my-Eoff (abs cr::current-si)))
           (dur-of-transposed-win (abs (- end-of-transposed-win start-of-transposed-win)))
           (reverse (if (equal sig-si -1) t ()))
           result
           (print-flag (cr::get-gbl 'cr::PRNFLG))
           )
      (declare (special inc-pt skip adaptation-flag 
                        xin xout cr::last-xin first-xout
                        cr::next-dur end-
                        cr::current-trelative
                        end-of-transposed-win 
                        dur-of-transposed-win 
                        start-of-transposed-win 
                        cr::current-tnorm reverse print-flag
                        result))
      
      (setf inc-pt (cr::auto-frz-field_wt 'cr::inc-pt))
      (setf first-xout (cr::auto-frz-field_wt 'cr::first-xout))
      (setf end- (cr::auto-frz-field_wt 'cr::end-))
      (setf ampfac (cr::auto-frz-field_wt 'cr::ampfac))
      (setf skip start-of-transposed-win)
      
      (when print-flag
        (push (format () 
                      ";%%% FRZ (1st chunk) inc-pt = ~5F, first-xout = ~5F, ampfac = ~5F~%"
                      inc-pt first-xout ampfac) result ))
      (setf adaptation-flag ())
      (test-inc-pt)
      (test-first-xout)
      (setf cr::next-dur (+ (abs (- inc-pt start-of-transposed-win)) first-xout))
      (test-end-)
      ; (test-cr::next-dur)
      (when adaptation-flag
        (when print-flag 
          (push (format () 
                        ";%%% FRZ (1st chunk) inc-pt = ~5F, first-xout = ~5F, ampfac = ~5F~%"
                        inc-pt first-xout ampfac) result) ))
      (cond 
       ; An abnormal case :
       ; ------------------
       ((>= cr::next-dur duration-wanted)
        (comp-field c "durs" duration-wanted)

;        (setf(svref vo 2) duration-wanted)
        ; unnecessary : (vset vo 22 (svref vi 22))
        ; nice, but perhaps not as good as the previous:
        ; (vset vo 22 (* first-xout 
        ;         (/  duration-wanted 
        ;   	   (+ inc-pt first-xout))))
        result)
       ; The normal case :
       ; -----------------
       (t
        ; 1st chunk :
        ; =========
        ; DUR
        (comp-field c "durs" cr::next-dur)
;        (setf(svref vo 2) next-dur)

        ; AMP
        (comp-field c "amp" ampfac)
;        (setf(svref vo 3) (* (svref vi 3) ampfac))

        ; XOUT
        (comp-field c "wxout" first-xout)
;        (setf(svref vo 22) first-xout)
; NO PRINT: main component

        (setf cr::current-trelative (+ cr::current-trelative (- (comp-field c "durs") end-)))
        (setf cr::current-tnorm (/ cr::current-trelative duration-wanted))       

        ; main freezing loop :
        ; ====
        (setf index 1)
        (loop while (< (+ cr::current-trelative cr::next-dur)
                       duration-wanted)
;do (format t "~a-~a-~a~%"current-trelative next-dur duration-wanted)
              do (progn 
                   
                   ; initialisations:
                   ; ---------------
                   (setf skip  	(cr::auto-frz-field_wt 'cr::skip))
                   (setf cr::next-dur (cr::auto-frz-field_wt 'cr::dur))
                   (setf end-  	(cr::auto-frz-field_wt 'cr::end-))
                   (setf xin 	(cr::auto-frz-field_wt 'cr::xin))
                   (setf xout 	(cr::auto-frz-field_wt 'cr::xout))
                   (setf ampfac (cr::auto-frz-field_wt 'cr::ampfac))

                   ; tests of these initialisations:
                   ; ------------------------------
                   (setf index (+ index 1))
                   (setf adaptation-flag ())
                   (when print-flag 
                     (push (format () 
                             ";%%% FRZ (chunk ~3D) skip = ~5F, dur = ~5F, xin = ~5F, xout = ~5F~%"
                             index skip cr::next-dur xin xout) result))
                   (when print-flag 
                     (push (format () 
                             ";%%%                 end- = ~5F, ampfac = ~5F~%"
                             end- ampfac) result))
                   (test-skip)
                   (test-next-dur)
                   (test-end-)
                   (test-xin-xout)
                   (when adaptation-flag
                     (when print-flag 
                       (push (format () 
                               ";%%% FRZ (chunk ~3D) skip = ~5F, dur = ~5F, xin = ~5F, xout = ~5F~%"
                               index skip cr::next-dur xin xout) result))
                     (when print-flag 
                       (push (format () 
                               ";%%%                 end- = ~5F, ampfac = ~5F~%"
                               end- ampfac) result) ))
                                    
                   ; csound-output with the corrected parameters:
                   ; -------------------------------------------
                   ; AT
                   (comp-field vo "e-dels" (+ (comp-field c "e-dels") cr::current-trelative))
;                   (setf(svref vo 1) (+ (svref vi 1) cr::current-trelative))

                   ; DUR
                   (comp-field vo "durs" cr::next-dur)
;                   (setf(svref vo 2) cr::next-dur)

                   ; AMP
                   (comp-field vo "amp" (* (comp-field c "amp") ampfac))
;                   (setf(svref vo 3) (* (svref vi 3) ampfac))

                   ; The global amplitude is possibly multiplied by a function
                   ; (typically exponentially decaying) during the freeze.
                   ; This function should be defined relatively to
                   ; trelative or tnorm.

                   ; PHS
                   (comp-field vo "wphs" (+ (cr::beg-off_wt (cr::curr_wt)) 
                                             (* skip (abs cr::current-si))))
;                   (setf(svref vo 19) (+ (beg-off_wt (curr_wt)) (* skip (abs cr::current-si))))

                   ; XIN
                   (comp-field vo "wxin" xin)
;                   (setf(svref vo 21) xin)

                   ; XOUT
                   (comp-field vo "wxout" xout)
;                   (setf(svref vo 22) xout)

                   (push (clone vo) result)
;                   (PrINX.wt vo)
                   
                   (setf cr::current-trelative (+ cr::current-trelative
                                              (- (comp-field vo "durs") end-)))
                   (setf cr::current-tnorm (/ cr::current-trelative duration-wanted))
                   )
              )
        ; end of loop.
        ; ===========
        
        ; last chunk :
        ; ==========
        ; the inc-pt may evolve in time !!!!!!!!
        (setf inc-pt (cr::auto-frz-field_wt 'cr::inc-pt))
        (setf cr::last-xin (cr::auto-frz-field_wt 'cr::last-xin))
        (setf ampfac (cr::auto-frz-field_wt 'cr::ampfac))
        
        (when print-flag 
          (push (format () 
                  ";%%% FRZ (last chunk) inc-pt = ~5F, cr::last-xin = ~5F, ampfac = ~5F~%"
                  inc-pt cr::last-xin ampfac) result))
        (setf adaptation-flag ())
        (test-inc-pt)
        (test-last-xin)
        (when adaptation-flag 
          (when print-flag 
            (push (format () 
                    ";%%% FRZ (last chunk) inc-pt = ~5F, cr::last-xin = ~5F, ampfac = ~5F~%"
                    inc-pt cr::last-xin ampfac) result) ))
        
        ; AT
        (comp-field vo "e-dels" (+ (comp-field c "e-dels") cr::current-trelative))
;        (setf(svref vo 1) (+ (svref vi 1) current-trelative)) ;iccci ??

        ; DUR
        (comp-field vo "durs" (- duration-wanted cr::current-trelative))
;        (setf(svref vo 2) (- duration-wanted current-trelative))

        ; AMP
        (comp-field vo "amp" (* (comp-field c "amp") ampfac))
;        (setf(svref vo 3) (* (svref vi 3) ampfac))

        ; PHS
        ; (vset vo 19 
        ; 	  (- (end-off_wt (curr_wt)) 
        ; 	     (* (- duration-wanted current-trelative)
        ; 		current-si)))
        ; The best way is to keep the skip-point of the last segment where it 
        ; ideally should be (i.e. = inc-pt - last-xin) in order to ensure 
        ; the continuity of the sound. Consequence : the sound might be faded out 
        ; earlier than the actual ending of the file.
        (if (null reverse) 
             (comp-field vo "wphs" (+ (cr::beg-off_wt (cr::curr_wt)) 
                                       (* (- inc-pt cr::last-xin) (abs cr::current-si))))

;             (setf(svref vo 19) (+ (beg-off_wt (curr_wt)) (* (- inc-pt cr::last-xin) (abs cr::current-si))))

             ; else
             (comp-field vo "wphs" (+ (cr::beg-off_wt (cr::curr_wt)) 
                                       (* (+ inc-pt cr::last-xin) (abs cr::current-si)))))
;          (setf(svref vo 19) (+ (beg-off_wt (curr_wt)) (* (+ inc-pt cr::last-xin) (abs current-si)))))
        
        ; XIN
        (comp-field vo "wxin" cr::last-xin)
;        (setf(svref vo 21) cr::last-xin)
        
        ; XOUT
        ; At the end, take the fade-out of the window and if this is not
        ; defined the fade-out of the logical file (this is already present
        ; in vi 22 (see LuP.wt)
        ; UNLESS we don't reach the border of the window or the logical file.
        ; In this case we take the cuurent xout of the freeze.
        (setf cr::next-dur (comp-field vo "durs"))
        (comp-field vo "wxout" (comp-field c "wxout"))
;        (setf(svref vo 22) (svref vi 22))
        (when	(<  (* (comp-field vo "durs") cr::current-si)
                    (abs (- (* end-of-transposed-win (abs cr::current-si))
                            (comp-field vo "wphs"))))
          (comp-field vo "wxout" (cr::auto-frz-field_wt 'cr::last-xout))
;          (setf(svref vo 22) (auto-frz-field_wt 'last-xout))
          (let ((xin cr::last-xin)
                (xout (comp-field vo "wxout")))
            (declare (special xin xout))
            (test-xin-xout)))
        (push (clone vo) result)
        (print (format () "~%         Total of ~D chunks~%" (1+ index)))
        (unless print-flag  ; IF NO PRINT-FLAG, PRINT MINIMUM INFORMATION IN SCORE
          (push (format () "~%; %%%         Total of ~D chunks~%" (1+ index)) result))
;        (PrINX.wt vo)
        ))
      (push (format () "~%~%") result)
      (nreverse result)
      ))
  )

; ****************************************************************************
; *     The different TESTS on the PARAMETERS of the chunks of the freeze    *
; ****************************************************************************

(defun test-inc-pt ()
  ( declare (special inc-pt adaptation-flag 
                     reverse 
                     dur-of-transposed-win 
                     start-of-transposed-win 
                     end-of-transposed-win
                     result))
  (if (null reverse)
       (cond 	((< inc-pt start-of-transposed-win)
                 (setf adaptation-flag t)
                 (when print-flag 
                      (push (format () 
                    ";%%% FRZ Warning ::  inc-pt < start / ~5F ~5F~%"
                          inc-pt start-of-transposed-win) result))
                 (setf inc-pt (+ start-of-transposed-win 
                                 (/ dur-of-transposed-win 2))))
		((>= inc-pt end-of-transposed-win)
                 (setf adaptation-flag t)
                 (when print-flag 
                    (push (format () 
                    ";%%% FRZ Warning ::  inc-pt >= end / ~5F ~5F~%"
                          inc-pt end-of-transposed-win) result))
                 (setf inc-pt (+ start-of-transposed-win 
                                 (/ dur-of-transposed-win 2))))
	        )
    (cond 	((> inc-pt start-of-transposed-win)
                 (setf adaptation-flag t)
                 (when print-flag 
                     (push (format () 
                    ";%%% FRZ Warning ::  inc-pt > start / ~5F ~5F~%"
                          inc-pt start-of-transposed-win) result))
                 (setf inc-pt (- start-of-transposed-win 
                                 (/ dur-of-transposed-win 2))))
		((<= inc-pt end-of-transposed-win)
                 (setf adaptation-flag t)
                 (when print-flag 
                    (push (format () 
                    ";%%% FRZ Warning ::  inc-pt <= end / ~5F ~5F~%"
                          inc-pt start-of-transposed-win) result))
                 (setf inc-pt (- start-of-transposed-win 
                                 (/ dur-of-transposed-win 2))))
	        )))

;-----------------------------------------------------------------------------
(defun test-first-xout ()
  ( declare (special inc-pt adaptation-flag 
                     reverse 
                     first-xout
                     start-of-transposed-win 
                     end-of-transposed-win
                     result))
  (if (null reverse)
       (when (> (+ inc-pt first-xout) end-of-transposed-win)
         (setf adaptation-flag t)
         (when print-flag 
           (push (format () 
                   ";%%% FRZ Warning ::  inc-pt + first-xout > end / ~5F ~5F ~5F~%"
                   inc-pt first-xout end-of-transposed-win) result))
         (setf first-xout (- end-of-transposed-win inc-pt)))

    ; else : end-of-transposed-win < start-of-transposed-win !!!!!
    (when (< (- inc-pt first-xout) end-of-transposed-win)
      (setf adaptation-flag t)
      (when print-flag 
        (push (format () 
                ";%%% FRZ Warning ::  inc-pt - first-xout < end / ~5F ~5F ~5F"
                inc-pt first-xout end-of-transposed-win) result))
      (setf first-xout (- inc-pt end-of-transposed-win)))
    )

  (when (< first-xout (cr::auto-frz-field_wt 'cr::min-xout))
    (setf adaptation-flag t)
    (when print-flag 
      (push (format () 
              ";%%% FRZ Warning ::  first-xout < min-xout / ~5F~%" 
              first-xout) result))
    (setf first-xout (cr::auto-frz-field_wt 'cr::min-xout))))

;-----------------------------------------------------------------------------
(defun test-last-xin ()
  ( declare (special inc-pt adaptation-flag 
                     reverse 
                     cr::last-xin
                     first-xout
                     start-of-transposed-win
                     result
                     ))
  (if (null reverse)
       (when (< (- inc-pt cr::last-xin) start-of-transposed-win)
         (setf adaptation-flag t)
         (when print-flag 
           (push (format () 
                   ";%%% FRZ Warning ::  inc-pt - cr::last-xin < start / ~5F ~5F ~5F~%"
                   inc-pt cr::last-xin start-of-transposed-win) result))
         (setf cr::last-xin (- inc-pt start-of-transposed-win)))

    ; else : end-of-transposed-win < start-of-transposed-win !!!!!
    (when (> (+ inc-pt cr::last-xin) start-of-transposed-win)
      (setf adaptation-flag t)
      (when print-flag 
        (push (format () 
                ";%%% FRZ Warning ::  inc-pt + cr::last-xin > start / ~5F ~5F ~5F~%"
                inc-pt cr::last-xin start-of-transposed-win) result))
      (setf cr::last-xin (- start-of-transposed-win inc-pt)))
    )

  (when (< cr::last-xin (cr::auto-frz-field_wt 'cr::min-xin))
    (setf adaptation-flag t)
    (when print-flag 
      (push (format () 
              ";%%% FRZ Warning ::  cr::last-xin < min-xin / ~5F~%" 
              cr::last-xin) result))
    (setf cr::last-xin (cr::auto-frz-field_wt 'cr::min-xin))))

;-----------------------------------------------------------------------------
(defun test-skip ()
 ( declare (special adaptation-flag 
                     reverse skip 
                     start-of-transposed-win 
                     end-of-transposed-win 
                     dur-of-transposed-win
                     result
                     ))
  (if (null reverse)
       (cond 	((< skip start-of-transposed-win)
		 (setf adaptation-flag t)
		 (when print-flag 
		     (push (format () 
                    ";%%% FRZ Warning ::  skip < start / ~5F ~5F~%"
                          skip start-of-transposed-win) result))
		 (setf skip (+ start-of-transposed-win 
                               (/ dur-of-transposed-win 2))))
		((>= skip end-of-transposed-win)
		 (setf adaptation-flag t)
		 (when print-flag 
		    (push (format () 
                    ";%%% FRZ Warning ::  skip >= end / ~5F ~5F~%"
                          skip end-of-transposed-win) result))
		 (setf skip (+ start-of-transposed-win 
                               (/ dur-of-transposed-win 2))))
	        )
    (cond 	((> skip start-of-transposed-win)
		 (setf adaptation-flag t)
		 (when print-flag 
		  (push (format () 
                    ";%%% FRZ Warning ::  skip > start / ~5F ~5F~%"
                          skip start-of-transposed-win) result))
		 (setf skip (- start-of-transposed-win 
                               (/ dur-of-transposed-win 2))))
		((<= skip end-of-transposed-win)
		 (setf adaptation-flag t)
		 (when print-flag 
		     (push (format () 
                    ";%%% FRZ Warning ::  skip <= end / ~5F ~5F~%"
                          skip end-of-transposed-win) result))
		 (setf skip (- start-of-transposed-win 
                               (/ dur-of-transposed-win 2))))
	        )))

;-----------------------------------------------------------------------------
(defun test-next-dur ()
  ( declare (special adaptation-flag reverse skip 
                     start-of-transposed-win 
                     end-of-transposed-win 
                     cr::next-dur
                     result
                     ))
  (when (< cr::next-dur (cr::get-gbl 'cr::DURMIN)) 
    (setf adaptation-flag t)
    (when print-flag 
      (push (format ()  
              ";%%% FRZ Warning ::  cr::next-dur < (get-gbl 'DURMIN) / ~5F ~5F~%"
              cr::next-dur (cr::get-gbl 'cr::DURMIN)) result))
    (cond ((equal (cr::get-gbl 'cr::DEBFLG) 1) 
           (print ";%%%         ::  FREEZE ABORTED")
           (push (format () ";%%%         ::  FREEZE ABORTED") result)
           (throw 'endoffreeze nil))
          ((equal (cr::get-gbl 'cr::DEBFLG) 2) 
           (setf cr::next-dur (cr::get-gbl 'cr::DURMIN)))))

  (if (null reverse)
       (when (> (+ skip cr::next-dur) end-of-transposed-win) 
         (setf adaptation-flag t)
         (when print-flag 
           (push (format () 
                   ";%%% FRZ Warning ::  skip + cr::next-dur > end / ~5F ~5F ~5F~%"
                   skip cr::next-dur end-of-transposed-win) result))
         (setf cr::next-dur (- end-of-transposed-win skip)))

    ; else : end-of-transposed-win < start-of-transposed-win !!!!!
    (when (< (- skip cr::next-dur) end-of-transposed-win) 
      (setf adaptation-flag t)
      (when print-flag 
        (push (format () 
                ";%%% FRZ Warning ::  skip - cr::next-dur < end / ~5F ~5F ~5F~%"
                skip cr::next-dur end-of-transposed-win) result))
      (setf cr::next-dur (- skip end-of-transposed-win)))
    ))

;-----------------------------------------------------------------------------
(defun test-end- ()
  ( declare (special adaptation-flag   
                     end-
                     cr::next-dur
                     result
                     ))
  (when (>= end- cr::next-dur) 
    (setf adaptation-flag t)
    (when print-flag 
      (push (format ()  
              ";%%% FRZ Warning ::  end- >= cr::next-dur / ~5F ~5F~%"
              end- cr::next-dur) result))
    (setf end- (/ cr::next-dur 2)))
  (when (< end- 0.) 
    (setf adaptation-flag t)
    (when print-flag 
      (push (format ()  
              ";%%% FRZ Warning ::  end- < 0. / ~5F~%" end-) result))
    (setf end- 0.)))

;-----------------------------------------------------------------------------
(defun test-xin-xout ()
  ( declare (special adaptation-flag   
                     xin xout cr::next-dur
                     result
                     ))
  (when (< xin (cr::auto-frz-field_wt 'cr::min-xin))
    (setf adaptation-flag t)
    (when print-flag 
       (push (format ()   
       ";%%% FRZ Warning ::  xin < min-xin / ~5F~%" xin) result))
    (setf xin (cr::auto-frz-field_wt 'cr::min-xin)))
  (when (< xout (cr::auto-frz-field_wt 'cr::min-xout))
    (setf adaptation-flag t)
    (when print-flag 
       (push (format ()   
       ";%%% FRZ Warning ::  xout < min-xout / ~5F~%" xin) result))
    (setf xout (cr::auto-frz-field_wt 'cr::min-xout)))
  (when (> (+ xin xout) cr::next-dur) 
    (setf adaptation-flag t)
    (when print-flag 
       (push (format ()  
       ";%%% FRZ Warning ::  xin + xout > cr::next-dur / ~5F ~5F ~5F~%"
             xin xout cr::next-dur) result))
    (setf xin (setf xout (/ cr::next-dur 2))))
  (when (< xin 0.) 
    (setf adaptation-flag t)
    (when print-flag 
       (push (format ()  
       ";%%% FRZ Warning ::  xin < 0. / ~5F~%" xin) result))
    (setf xin 0.))
  (when (< xout 0.) 
    (setf adaptation-flag t)
    (when print-flag 
       (push (format ()  
       ";%%% FRZ Warning ::  xout < 0. / ~5F~%" xout) result))
    (setf xout 0.))
  )

; END OF ADDED 91 08 19

;*****************************************************************************

;******************************************************************
; METHODS
;******************************************************************

;------------------------------------------------------------------
; TESTS FOR THE USER-FUN
; ? = test, ! = modifier (no test)

;------------------------------------------------------------------
; GLOBAL
;	durtot?
;	amptot?

;------------------------------------------------------------------
; LOCAL (MAIN)
;	ed-0?
;	ed-durmin?
;	compute-dur!
;	dur-durmin?
;	ed+dur?
;	amp?
;	compute-amp!
;	fq-sr?
;	fqmin?
;
;	gbl-f0!
;       gbl-n2!
;	n2-ston!
;       set-n1!
;------------------------------------------------------------------
; LOCAL (SUB)
;	s-fq-sr?
;	s-fqmin?
;       s-n2-fm!

;------------------------------------------------------------------


;------------------------------------------------------------------
; GLOBAL SLOTS / DISCARD EVENT
;    TESTS FOR SYNTHESIZE
;------------------------------------------------------------------

; If the test returns (), the matrix will be discarded and the evaluation of the local tests
;    (user-fun) will not be done.
; If the test returns a STRING, the message will be printed in the output file (NOT YET)

(defmethod durtot? ((c class-array))
  "
durtot[e] < durmin => discard event
(real test before sending the list of events to synthesize)
  "
  (declare (special cr::durmin))  
  (let ((mindur (if (find-package 'cr)	; IF CHROMA WAS LOADED, USE ITS DEFAULT
                  (cr::get-gbl 'cr::durmin)
                  0.01))
        (durtot (durtot c)))
    (if (< durtot mindur)

; IT IS NOT PRINTED IN THE SCORE FILE, SINCE THE EVENT IS NOT CREATED
      (print (format () "~%~%~%~%~%;;durtot? --------ERROR: DURTOT (~a) < durmin (~a)~%;            EVENT DISCARDED.~%~%~%~%~%"
              durtot mindur))
      t)))

(defmethod amptot? ((c class-array))
"
amptot[e] > GBLAMP => discard event
Two global variables:
	MAXAMP (for the amplitudes within an event)
	GBLAMP (for the global amplitude)
"
   (if (<= (amptot c) (cr::get-gbl 'cr::gblamp))
     t
; IT IS NOT PRINTED IN THE SCORE FILE, SINCE THE EVENT IS NOT CREATED
     (print (format () "~%~%~%~%~%;;amptot? --------ERROR: AMPTOT ~a > GBLAMP: ~a~%;            EVENT DISCARDED.~%~%~%~%~%"
               (amptot c)
               (cr::get-gbl 'cr::gblamp)))))


;------------------------------------------------------------------
; LOCAL SLOTS / DISCARD COMPONENT (MAIN)
;     TEST WITHIN THE MATRIX
;------------------------------------------------------------------

(defmethod ed-0? ((c component))
"
e-dels[i] < 0.0	=> discard component
"
  (let ((curr-ed (comp-field c "e-dels")))
    (if (< curr-ed 0.0)
        (print (format () ";ed-0? --------ERROR: E-DELS < 0.0: ~a~%;    Component n. ~a discarded~%"
                curr-ed (index c)))
      c)))


(defmethod ed-durmin? ((c component))
"
e-dels[i] > durtot[e] - durmin => discard component
"
(declare (special cr::durmin))
  (let ((curr-ed (comp-field c "e-dels"))
        (durtot (durtot (comp-array c)))
        (mindur (if (find-package 'cr)	; IF CHROMA WAS LOADED, USE ITS DEFAULT
                  (cr::get-gbl 'cr::durmin)
                  0.01))) 		; DEFAULT MINIMUM DURATION
    (if (> curr-ed (- durtot mindur))
;      (progn
        (print (format () ";ed-durmin? --------ERROR: E-DELS [~a] > durtot [~a] - durmin [~a]~%;    Component discarded~%"
                curr-ed durtot mindur))
;        (format () ";--------ERROR: E-DELS [~a] > durtot [~a] - durmin [~a]~%;    Component discarded~%"
;                curr-ed durtot mindur))
      c)))



(defmethod compute-dur! ((c component))
"
(compute realdur[i]) => durs[i] = durtot[e] * durs[i]
"
  (comp-field c "durs" (* (comp-field c "durs") (durtot (comp-array c)))))



(defmethod dur-durmin? ((c component))
"Discard the component if its duration is below durmin"
(declare (special cr::durmin))  
   (let ((curr-dur (comp-field c "durs"))
         (mindur (if (find-package 'cr)
                  (cr::get-gbl 'cr::durmin)
                  0.01)))
     (if (< curr-dur mindur)
;       (progn
         (print (format () ";dur-durmin? --------ERROR: DUR [~a] < durmin [~a]~%;    Component discarded~%"
                 curr-dur mindur))
;         (format () ";--------ERROR: DUR [~a] < durmin [~a]~%;    Component discarded~%"
;                 curr-dur mindur))
       c)))



(defmethod ed+dur? ((c component))
  "
Correct the component if its duration is beyond durtot.
(e-dels[i]+durs[i] > durtot[e]	durs[i] => durtot[e] - e-dels[i])
"
  (let ((curr-dur (comp-field c "durs"))
        (curr-ed (comp-field c "e-dels"))
        (durtot (durtot (comp-array c))))
    (if (<= (+ curr-ed curr-dur) durtot)
      c
      (list 
       (comp-field c "durs" (- durtot curr-ed))
       (format () ";ed+dur? ---> WARNING / Reduced DUR: old-dur = ~a, new-dur = ~a~% "
                  curr-dur (- durtot curr-ed)))
       )))



(defmethod amp? ((c component))
"
amp[i] > MAXAMP => discard component
Two global variables:
	MAXAMP (for the amplitudes within an event)
	GBLAMP (for the global amplitude)
"
  (let ((amp (comp-field c "amp")))
    (if (> amp (cr::get-gbl 'cr::maxamp))
;      (progn
        (print (format () ";amp? --------ERROR: AMP > ~a: ~a~%;    Component discarded~%"
                amp
                (cr::get-gbl 'cr::maxamp)))
;        (format () ";--------ERROR: AMP > ~a: ~a~%;    Component discarded~%"
;                amp
;                (cr::get-gbl 'cr::maxamp)))
      c)))



(defmethod compute-amp! ((c component))
   "
(compute realamp[i]) => amp[i] = amp[i] * amptot[e]
   "
  (comp-field c "amp" (* (amptot (comp-array c))
			 (comp-field c "amp"))))


;;; ***	NEW, ms1908
(defmethod fq-sr? ((c component) &rest mode)
"
IF fq > NYQUIST
(fq-sr?) with no args, discard the fq is > Nyquist
(fq-sr? 0) transpose the fq until it falls within the range of Nyquist
(fq-sr? <i>) transpose the fq <n> octaves down
(fq-sr? <f>) transpose the fq until below <f>

IF fq > val
(fq-sr? (val)) tranpose the fq until below <val> independently on the Nyquist frequency"

(declare (special cr::sr/2))
   (let ((sr2 (if (find-package 'cr)
                  (cr::get-gbl cr::sr/2)
                  22050))
         (mode (car mode)))

     (cond ((null mode)
            (if (> (comp-field c "freq") sr2)
;                (progn
                  (print (format () ";fq-sr? --------ERROR: FQ ~a > Nyquist [~a]~%;    Component n. ~a discarded~%"
                                 (comp-field c "freq") sr2 (index c)))
;       (format () ";ERROR: FQ > Nyquist: ~a [SR = ~a]~%;    Component n. ~a discarded~%"
;               (* sr2 2) (comp-field c "freq") (index c)))
                  c))

           ((and (numberp mode) (= mode 0))
            (let ((currfq (comp-field c "freq"))
                  (oldfq (comp-field c "freq")))
                          (if (> (comp-field c "freq") sr2)
                              (progn
                                (loop while (> currfq sr2) do
                                      (setf currfq (/ currfq 2.0)))
                                (list
                                 (comp-field c "freq" currfq)
                                 (print (format () ";fq-sr? ==========WARNING: FQ ~a > Nyquist [~a], --> transposed to ~a~%" oldfq sr2 currfq))))
                          c)))

           ((integerp mode)
            (let* ((oldfq (comp-field c "freq"))
                   (newfq (/ oldfq (expt 2 mode))))
              (if (> oldfq sr2)
                  (list
                   (comp-field c "freq" newfq)
                   (print (format () ";fq-sr? ==========WARNING: FQ ~a > Nyquist [~a], --> transposed ~a octaves down to ~a~%" oldfq sr2 mode newfq)))
                c)))

           ((floatp mode)
            (let ((currfq (comp-field c "freq"))
                  (oldfq (comp-field c "freq")))
                          (if (> currfq sr2)
                              (progn
                                (loop while (> currfq mode) do
                                      (setf currfq (/ currfq 2.0)))
                                (list
                                 (comp-field c "freq" currfq)
                                 (print (format () ";fq-sr? ==========WARNING: FQ ~a > Nyquist [~a], --> transposed below ~a to ~a~%" oldfq sr2 mode currfq))))
                          c)))

           ((listp mode)
            (let ((currfq (comp-field c "freq"))
                  (oldfq (comp-field c "freq"))
                  (threshold (car mode)))
                          (if (> currfq threshold)
                              (progn
                                (loop while (> currfq threshold) do
                                      (setf currfq (/ currfq 2.0)))
                                (list
                                 (comp-field c "freq" currfq)
                                 (print (format () ";fq-sr? ==========WARNING: FQ ~a > threshold [~a], --> transposed to ~a~%" oldfq threshold currfq))))
                          c)))
           (t
            (error ";fq-sr? *****ILLEGAL MODE ~a in component n. ~a ~%" mode (index c))))
     ))

#|
(defmethod fq-sr? ((c component) &rest mode)
"
If mode = () fq[i] > SR/2 => discard component
   otherwise transpose it by <mode> octaves
   (if mode = 0, until it falls below the Nyquist frequency)
"
(declare (special cr::sr/2))
   (let ((sr2 (if (find-package 'cr)
                  (cr::get-gbl cr::sr/2)
                  22050)))
            (if (> (comp-field c "freq") sr2)
                (progn
                  (print (format () ";fq-sr? - ERROR: FQ ~a > Nyquist [~a]~%;    Component n. ~a discarded~%"
                                 (comp-field c "freq") (* sr2 2) (index c)))
;       (format () ";ERROR: FQ > Nyquist: ~a [SR = ~a]~%;    Component n. ~a discarded~%"
;               (* sr2 2) (comp-field c "freq") (index c)))
                  c))))
|#

;;; ***
(defmethod fqmin? ((c component))
"
fq[i] < fqmin	discard component
"
(declare (special cr::minfq))  
   (let ((fqmin (if (find-package 'cr)
                  (cr::get-gbl 'cr::minfq)
                  15.0)))
     (if (< (comp-field c "freq") fqmin)
       (progn
         (print (format () ";ERROR: FQ < fqmin [~a]: ~a~%;    Component discarded~%"
                 fqmin (comp-field c "freq")))
         (format () ";ERROR: FQ < fqmin [~a]: ~a~%;    Component discarded~%"
                 fqmin (comp-field c "freq")))
       c)))
	
	
(defmethod gbl-f0! ((c component))
   "
Copy the global value of f0 (gbl-f0) into the local field :f0.
Used in marco's classes, to give a unique f0 in a formantic frequency modulation.

If the slot does not exist, be tolerant and do nothing!
   "
   (when (member 'gbl-f0 (fixed-slots-list (comp-array c)))
     (let ((gbl-f0 (gbl-f0 (comp-array c))))
       (comp-field c "f0" gbl-f0))))


(defmethod gbl-N2! ((c component))
   "
Copy the global value of N2 (gbl-N2) into the local field :n2.
Used in marco's classes, to give a unique N2 in a formantic frequency modulation.

If the slot does not exist, be tolerant and do nothing!
   "
   (when (member 'gbl-N2 (fixed-slots-list (comp-array c)))
     (let ((gbl-N2 (gbl-N2 (comp-array c))))
       (comp-field c "n2" gbl-N2))))

;;; *
(defmethod set-N1! ((c component))
   "
Compute the closest value of N1 given the current :f0 and :fq
If the class has a global slot called gbl-f0, call <gbl-f0!> BEFORE
  using this function to initialise this field.
   "

     (let ((f0 (comp-field c "f0"))
           (fq (comp-field c "freq")))
       (comp-field c "freq" (compute-N2-from-f0 f0 fq))))

(defun compute-N2-from-f0 (f0 fq)
  (let* ((scaler (floor (/ fq f0)))
         (low (- fq (* scaler f0)))
         (high (- (* (1+ (floor (/ fq f0))) f0) fq)))
    (if (< (- high low) 0.0)
      (1+ scaler)
      scaler)))


(defmethod N2-ston! ((c component))
   "
Detune the value of N2 by a percentage of STON2
n2[i] = n2[i] + (n2[i] * (ran ston2[i])

If the class has a global slot called gbl-N2, call <gbl->N2!> BEFORE
  using this function to initialize this field.

   "
  (let ((n2 (comp-field c "n2"))
        (ston2 (comp-field c "ston2")))
        (comp-field c "n2"
                    (+ n2 (* n2 (cr::ran ston2))))))


;------------------------------------------------------------------
; LOCAL SLOTS / DISCARD COMPONENT (SUB)
;     TEST WITHIN THE MATRIX
;------------------------------------------------------------------

;;; ***
(defmethod s-fq-sr? ((c component) &rest fqmax)
"
fq[i] > fqmax => discard sub-component
fqmax is either given as an argument, or = Nyquist

"
(declare (special cr::sr/2))
   (let ((sr2 (if (car fqmax)
                  (car fqmax)
                (if (find-package 'cr)
                    (cr::get-gbl cr::sr/2)
                  22050))))
     (if (> (comp-field c "freq") sr2)
         (print (format () ";s-fq-sr? -------- ERROR: FQ ~,2F > MAX FQ ~a  /  Sub-component discarded"
                        (comp-field c "freq") sr2))
       c)))


;;; ***
(defmethod s-fqmin? ((c component) &rest fqmin)
"
fq[i] < fqmin	discard sub-component
fqmin is either given as an argument, or fetched from cr::minfq,
"
(declare (special cr::minfq))  
   (let ((fqmin (if (car fqmin)
                    (car fqmin)
                  (if (find-package 'cr)
                      (cr::get-gbl 'cr::minfq)
                    15.0))))
     (if (< (comp-field c "freq") fqmin)
         (print (format () ";s-fqmin? ------- ERROR: FQ ~a < fqmin [~a]~%;    Sub-component discarded~%"
                 fqmin (comp-field c "freq")))
       c)))


;------------------------------------------------------------------
; MAX AND RECOMMENDED LIST OF TESTS (correct order)

; ADDITIVE SYNTHESIS
; main tests: (ed-0? ed-durmin?  compute-dur! dur-durmin? ed+dur?
;                   amp? compute-amp!  fq-sr? fqmin?)
; sub tests: (s-fq-sr? s-fqmin?)

; FM SYNTHESIS
; main tests: (ed-0? ed-durmin?  compute-dur! dur-durmin? ed+dur?
;                   amp? compute-amp!  fq-sr? fqmin?
;                   [gbl-f0! gbl-N2!] set-N1! N2-ston!)
; sub tests: ([fq-sr? fqmin?  set-N1!] N2-ston)
;******************************************************************




;***************************************************************;
; FUNCTIONS FOR THE GLOBAL SLOTS (OPENMUSIC)
(defun get-user-fun ()
  "(om::gen-user-fun '(om::ed-0? om::ed-durmin? om::compute-dur!
                    om::dur-durmin? om::ed+dur? om::amp? om::compute-amp!
                    om::fq-sr? om::fqmin?) 
                  '(om::sub-comps ) :sub-tests '(om::s-fq-sr?))"
  )

(defun get-user-fun1 (&key (tests ()) (subc '(om::sub-comps)) (subtests ()))
  (let ((tests (loop for el in tests
                     collect (format () "om::~a " el)))
        (subc (loop for el in subc
                     collect (format () "om::~a " el)))
        (subtests (loop for el in subtests
                        collect (format () "om::~a " el))))
    (format () "(om::gen-user-fun '~a '~a :sub-tests '~a)"
            tests subc subtests)))




