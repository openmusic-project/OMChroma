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
; File author: M. Stroppa, S. Lemouton
;=====================================================



;---------------------------------------------------------------;
;*************** USER-DEFINED FUNCTIONS FOR CTL2 ***************;
;---------------------------------------------------------------;

; get-model-amp / -norm-amp / -reversed-amp
; get-model-bw-to-index
; get-model-cwt / -cwt+
; get-model-dur
; get-model-fq / -fq-with-stretch
; get-model-fqwt
; make-fun-from-list
; make-ran / -ran-dB
; make-nplet
; relative-ED
; show-model
; user-defined-function
; yaka-user
;---------------------------------------------------------------;
#|
variables speciales utilisables dans une fonction utilisateur:

my-model : ...
n-fql : ...
my-fql : ...
my-rank : ....
my-time : ...
my-dur : ...
my-nev : ...

.... 
|#
;---------------------------------------------------------------;
(in-package :cr)

;***************************************************************;
; FUNCTIONS FOR THE GLOBAL SLOTS (NOT OPEN MUSIC)
(defun durtot-fun (&key (scaler (make_fun '(1 0 1 1)))
                          (mindur 0.1))
  (declare (special  my-rank my-dur my-nev))
  (let* ((dur_k_fun (X-resc_fun scaler 0 (1- my-nev)))
         (curr-dur ( * (y-val_fun dur_k_fun my-rank) my-dur)))
    (if (< curr-dur mindur)
      mindur
      curr-dur)))


;***************************************************************;
; FUNCTIONS FOR THE GLOBAL SLOTS (OPEN MUSIC)
(defun get-user-fun ()
  "(om::gen-user-fun '(om::ed-0? om::ed-durmin? om::compute-dur!
                    om::dur-durmin? om::ed+dur? om::amp? om::compute-amp!
                    om::fq-sr? om::fqmin?) 
                  '(om::sub-comps ) :sub-tests '(om::s-fq-sr?))"
  )

(defun get-user-fun1 (&key (tests ()) (subc (sub-comps)) (subtests ()))
  (let ((tests (loop for el in tests
                     collect (format () "om::~a " el)))
        (subc (loop for el in subc
                     collect (format () "om::~a " el)))
        (subtests (loop for el in subtests
                        collect (format () "om::~a " el))))
    (format () "(om::gen-user-fun '~a '~a :sub-tests '~a)"
            tests subc subtests)))

;***************************************************************;
; FUNCTIONS FOR THE LOCAL SLOTS
(defun get-model-amp ()
  (declare (special my-fql my-ptl))
  (if my-ptl
    (get-amp my-ptl)
   (get-amp my-fql)))

(defun get-model-amp-db ()
  (declare (special my-fql my-ptl))
  (if my-ptl
    (lintodB (get-amp my-ptl))
   (lintodB (get-amp my-fql))))

(defun get-model-norm-amp ()
  (declare (special my-model my-rank))
  (get-norm-amp my-model my-rank))


(defun get-model-norm-gblamp ()
  (declare (special my-model my-rank))
  (om::om* (get-norm-amp my-model my-rank) (get-gbl gblamp)))


(defun get-model-reversed-amp ()
  (declare (special my-model my-rank))
  (nreverse (get-norm-amp my-model my-rank)))
;---------------------------------------------------------------;
;pour model-partials (2000) :

(defun get-model-amp_fun ()
  (declare (special my-ptl))
   (amp_funs my-ptl))

#|
(defun get-model-amp_bpf (&key (reduction nil))
  (declare (special my-ptl))
  `(fun->bpf (quote ,(mapcar #'(lambda (x) (reduce2_fun x reduction))
                             (amp_funs my-ptl)))  3)) ; precision = 3
|#
; BOTH THIS AND THE MODEL USED IN GET-MODEL-TRANSP_BPF WORK
(defun get-model-amp_bpf (&key (reduction nil))
  (declare (special my-ptl))
  (list 'fun->bpf `(quote ,(mapcar #'(lambda (x) (reduce2_fun x 3))
                                   (amp_funs my-ptl)))  6)) ; precision = 6

(defun get-model-transp_fun ()
  (declare (special my-ptl))
  (transp_funs my-ptl))

(defun get-model-transp_bpf (&key (reduction nil))
  (declare (special my-ptl))
  (let ((rescaled_fun (copy-tree (transp_funs my-ptl))))
     (mapcar #'(lambda (x) (Y-resc_fun  x 0 1)) rescaled_fun)
    `(fun->bpf (quote ,(mapcar #'(lambda (x) (reduce2_fun  x reduction))
                               rescaled_fun )) 3))) ; precision = 3
(defun get-model-dur ()
  (declare (special my-ptl my-dur my-nev))
  (if my-ptl  
    (duration my-ptl)
   ; (rept my-nev my-dur)
     my-dur ))

(defun get-model-ldur ()
  (declare (special my-ptl my-dur my-nev))
  (if my-ptl  
    (list(duration my-ptl))
      (list my-dur )))


(defun get-model-edels ()
  (declare (special my-ptl ))
   (entry-delays my-ptl))

(defun get-model-edel ()
  (declare (special my-ptl ))
  (car (entry-delays my-ptl)))

(defun get-model-durs ()
  (declare (special my-ptl ))
  (durs my-ptl))

(defun get-model-dev+ ()
  (declare (special my-ptl))
    (get-dev+ my-ptl))

(defun get-model-dev- ()
  (declare (special my-ptl))
   (get-dev- my-ptl))

#|
(defun get-model-dev+ ()
  (declare (special my-ptl))
  (list  'quote  (mapcar #'y-max_fun (transp_funs my-ptl))))

(defun get-model-dev- ()
  (declare (special my-ptl))
  (list  'quote  (mapcar #'y-min_fun (transp_funs my-ptl))))
|#


;---------------------------------------------------------------;

(defun get-model-fq (&key (transposition nil) )
  (declare (special my-fql n-fql my-rank))
  (if transposition
    ;transposition must be an objet fun , y en demitons
    (let* ((trans-fun (X-resc_fun transposition 0 n-fql))
           (my-transposition (semitones->ratio (Y-val_fun trans-fun my-rank))))
       (get-fql (transpose my-fql my-transposition)))
    ;pas de transposition :
   (get-fql my-fql)))

(defun get-model-fq-with-stretch (&key (transposition nil)
                                            (offset (make_fun '(0. 0 0. 1)))
                                            (stretching (make_fun '(2.0 0 2.0 1))))
  (declare (special my-fql n-fql my-rank))
  ;offset and stretching must be funs !
  (let* ((offset-fun (X-resc_fun offset 0 n-fql))
         (my-offset (Y-val_fun offset-fun my-rank))
         (stretch-fun (X-resc_fun stretching 0 n-fql))
         (my-stretch (Y-val_fun stretch-fun my-rank)))
    (if transposition
      ;transposition : objet fun , y en demitons
      (let* ((trans-fun (X-resc_fun transposition 0 n-fql))
             (my-transposition (semitones->ratio (Y-val_fun trans-fun my-rank))))
        (get-fql (stretch_vps (transpose my-fql  my-transposition)
                              :stretching my-stretch
                              :offset my-offset)))
      ;pas de transposition :
      (get-fql (stretch_vps my-fql :stretching my-stretch
                            :offset my-offset)))))


;---------------------------------------------------------------;
(defun get-model-cwt (wt-list &key (beg-ratio 0) (beg-ran 0)
                                 (min-ratio 1.) (max-ratio 1.)
                                 (transposition nil) (limits '(.25 . 4)))
  (declare (special my-fql n-fql my-rank))
  (if transposition
;transposition : objet fun , y en demitons
    (let* ((trans-fun (X-resc_fun transposition 0 n-fql))
           (my-transposition (semitones->ratio(Y-val_fun trans-fun my-rank)))
           (result (closest_wt-ran wt-list (get-fql (transpose my-fql  my-transposition))
                                  :beg-ran beg-ran :beg-ratio beg-ratio
                                  :min-ratio min-ratio
                                  :max-ratio max-ratio
                                  :limits limits)))

      (set-gbl '*closest-wt* (cadadr result))
      (eval(cadar result)))
;pas de transposition :
    (let ((result (closest_wt-ran wt-list (get-fql my-fql) 
                                  :beg-ran beg-ran :beg-ratio beg-ratio
                                  :min-ratio min-ratio
                                  :max-ratio max-ratio
                                  :limits limits)))
      (set-gbl '*closest-wt* (cadadr result))
      (eval(cadar result)))))


; get-model-cwt+
; Varying the data base as a function of the current rank (from 0)
; First list: default data base
; Successive args: either a single number or a list of two numbers (evaluated)
;    Single number: call the data base when the current rank = number
;    List of two numbers: call the list when the current rank is within the two
;        numbers (ex: (2 6) -> from 2 to 6)
; If multiple values are specified, the first one will be used.
#|
(CWT  (call get-model-cwt+ 
            '(chin :limits (.5 . 2) :min-ratio .125 :max-ratio 1.3
              )
            '(1  hin :min-ratio .2 :max-ratio 2.0)
            '(fff  hin :min-ratio .2 :max-ratio 2.0)
            ;                    '((/ n-fql 2)  hin :min-ratio .2 :max-ratio 2.0)
            '((4 7)  pil :min-ratio .5 :max-ratio 1.5)
            ))

(get-model-cwt+ '('a  'args1) '(1  b 'args2) '(3  c 'args3) '((5 9)  d 'args4) '(10 e 'args5))
|#

(defun get-model-cwt+ (default-args &rest args)
  (declare (special my-fql n-fql my-rank))
  (let ((wt-alist nil)
        (wt-list nil)
        )
    (loop for wt-elem in args
          do (if (listp (car wt-elem))
               (loop for i from (eval(caar wt-elem)) to (eval(cadar wt-elem))
                     do (push (cons i (cdr wt-elem)) wt-alist))
               (push (cons (eval(car wt-elem))(cdr wt-elem)) wt-alist)))
    (setf wt-alist (nreverse wt-alist))
    (setf wt-list (cdr(assoc my-rank wt-alist)))
    (unless wt-list (setf wt-list default-args))
 (apply 'get-model-cwt (cons (eval (car wt-list))(cdr wt-list))
     )))

;---------------------------------------------------------------;
(defun get-model-fqwt ()
  (let ((result  (get-gbl '*closest-wt*)))
    (set-gbl '*closest-wt* nil)
    (if result
      (eval result)
      (error " get-model-fqwt should be called after get-model-cwt"))
    ))

;---------------------------------------------------------------;
(defun get-model-bw-to-index ()                 ; WRITE YOUR OWN
(declare (special  my-fql))
(if (null (get-bw my-fql)) (error "MISSING BANDWIDTH IN ~a" my-fql)
(mapcar (lambda (y) (/ y 10)) (get-bw my-fql))))   ; RETURNS A LIST


;---------------------------------------------------------------;
(defun make-ran-dB (min max)
  (declare (special my-nev))
  (let ((pivot (/ (+ max min) 2))
        (bernard (/ (abs (- max min)) 2)))
    (dbtolin (rept my-nev (ran pivot bernard)))))

(defun make-ran (min max)
  (declare (special my-nev))
  (let ((pivot (/ (+ max min)2))
        (bernard (/ (abs (- max min)) 2)))
    (rept my-nev (ran pivot bernard))))

;---------------------------------------------------------------;
(defun make-nplet (&rest l)
  " Generate a list of values evaluating each of them.
  Useful when preparing codes for modulation and envelopes"
  (declare (special my-nev))
  (let ((nplet ()) result )
    (loop for i from 0 to my-nev
          do (loop for j in l
                   do (push (eval j) nplet)
                   finally (progn (push (list 'quote
                                              (list 'quote
                                                    (nreverse nplet))) result) 
                                  (setf nplet nil))))
          (nreverse result)))
;---------------------------------------------------------------;
(defun make-nplet-fun (&rest l)
  " Generate a list of values evaluating each of them.
  Useful when preparing codes for modulation and envelopes"
  (declare (special my-nev))
  (let ((nplet ()) result )
    (loop for i from 0 to my-nev
          do (loop for j in l
                     do (push (eval j) nplet)
                     finally (progn (push (list  'quote (cons 'list
                                                 (nreverse nplet))) result) 
                                    (setf nplet nil))))
    (nreverse result)))
;---------------------------------------------------------------;
;????
(defun relative-ED (itp-fun ed-fun)
  " Generate a list of ED relative to the current duration.
    ed-fun : function used within the ED instruction.
    itp-fun: function used to compute the current maximum ED."
  (declare (special my-nev my-rank my-dur n-fql))
  (let* ((itp-fun (X-resc_fun itp-fun 0 n-fql))
         (max-ed (Y-val_fun itp-fun my-rank))
         (ed-fun (Y-resc_fun (copy-list ed-fun) 0.0 max-ed))
         )
    (mapcar (lambda (x) (* x my-dur))
            (lkp my-nev ed-fun))))

;---------------------------------------------------------------;
(defun user-defined-function ()         ; WRITE YOUR OWN
(declare (special  my-nev))
(rept my-nev .06))                      ; RETURN A LIST


(defun yaka-user (&key (dur-scaler 1.0) (min-dur 0.1))
  (declare (special n-fql my-time my-dur my-nev my-rank my-fql my-model))
  ; n-fql = (1- n-fql)
  ; my-<name> refers to current object
  
  ; PUT YOUR OWN CODE HERE
  ; CAN ONLY MODIFY EXISTING INTERNAL VARIABLES
  ; RETURN NOTHING
  
  (when (is_fun dur-scaler)
    (setf dur-scaler (Y-val_fun
                      (X-resc_fun (copy-list dur-scaler) 0 n-fql) my-rank)))
  (setf my-dur (* my-dur dur-scaler))

  (when (< my-dur min-dur) (setf my-dur min-dur))
  )
;---------------------------------------------------------------;
; MIXED FUNCTIONS
(defun make-fun-from-list (list)
  (let ((result nil)(cnt 0))
    (loop for i in list
          do (push i result)
          do (push cnt result)
          do (incf cnt))
    (make_fun (nreverse result))))
;---------------------------------------------------------------;
