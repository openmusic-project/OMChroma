;*****************************************************************************
;-------| VPS SYSTEM
;-------| This file is: $LLvps/user-vps.lisp
;-------| By Marco Stroppa
;-------| Version 1.1, September 1999
;-------| Copyright 1998 IRCAM
;*****************************************************************************
;(in-package chroma)
(in-package om)

;; THIS FILE SPECIFIES THE USER-ACCESSIBLE INTERFACE TO THE VPS
;;    OBJECT SYSTEM BY PROVINDING A SET OF METHODS THAT DO NOT USE
;;    ANY KEYWORDS
;;
;; THE INTERFACE DIRECTLY CALLS OBJECT-ORIENTED METHODS



;__________________________________________________________________
;         AVAILABLE METHODS
;__________________________________________________________________

;     CONSTRUCTORS:   make_vps

;                     make_spl
;                     make_rpl
;                     make_ppl
;                     make_cil
;                     make_ail
;                     make_fql
;                     make_crl
;                     make_arl

;------------------------------------------------------------------
;	 SELECTORS:   spl_vps
;                     rpl_vps
;                     cil_vps
;                     cils_vps
;                     ail_vps
;                     ails_vps
;                     fql_vps
;                     crl_vps
;                     arl_vps

;                     vps_vps
;                     nn_vps
;                     gil / gils_vps
;                     surf / surf-s_vps
;                     dens_vps
;                     hom / hom-s / hom-e / hom-es_vps
;                     sd_vps
;                     cs_vps
;                     harm / harm-e_vps
;                     vf0_vps
;                     max-fq / min-fq_vps
;                     max-amp / min-amp_vps
;                     max-bw / min-bw_vps

;------------------------------------------------------------------
;	PREDICATES:  is_vs/vps
;                    is_spl/rpl/...


;------------------------------------------------------------------
;	 UTILITIES:  spsht_vps
;	             xpose_vps

;------------------------------------------------------------------
;	      INFO:  type-of_vps
;__________________________________________________________________


;;;;;;;;;;;;;; CONSTRUCTORS

;	NAME:	  make_***  (CONSTRUCTOR)
;	TYPE:	  Generic function with 2 or 3 arguments
;	CALL:	  (make_*** '<a list> [a value])
;	FUNCTION: define and initialize a structure of
;                    super-class VPS
;                 make_vps will automatically instanciate the right
;                   class as a function of its input argument(s)
;                   as a function of the type of the argument(s)
;                 the other constructors directly instanciate
;                   a given class
;	VALUE:	  the new structure

;;;;;;;;;;;;;;;;;;;;;;;;; Types of VPS ;;;;;;;;;;;;;;;;;;;;;;;;;
;SPL (Symbolic Pitch List)
;     '(DO4 LAb4 RE5 SOL5 REb6)
;     '(DO4 (LAb4 23) RE5 (SOL5 -12) REb6)

;RPL (Relative Pitch List)
;     '(DO LAb RE1 SOL1 REb2)
;     '((DO . 23) (LAb . -34) RE1 SOL1 (REb2 . 77))

;PPL (Pure Pitch List) - for internal usage only!
;     '(DO LAb RE SOL REb)
;     '((DO . 23) (LAb . -34) RE SOL (REb . 77))

;CIL (Contiguous Interval List)
;     '(6- 4+ 4 4+)
;     '((6- 0 12) (4+ 1 -21) (4 0 7) (4+ 1 -2))

;AIL (Anchored Interval List)
;     '(-6+ -2- 4 7- (3+ 1)) 'LA4
;     '((-6+ 0 12) -2- (4 0 -22) 7- (3+ 1 17)) + reference: 'LA4

;FQL (Frequency List)
;     '(261 415 587 784 1108)

;CRL (Contiguous Ratios)
;     '(0.5873 0.4142 0.3345 0.4145)

;ARL (Spectrum / Anchored Ratios)
;     '(2.61 4.153 5.873 7.84 11.07) + reference: 100.0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod! make-vps ((list list) &optional reference)
   :icon 181
   :indoc '("list of VPS elements" "reference pitch [Hz], default 440.0") 
            (cr::make_vps list reference))

(defmethod make_vps ((list list) &optional reference)
;(om::defmethod! make_vps ((list list) &optional reference)
;  :icon 130
  " Automatically instanciates an object of the super-class VS,
       except PPL's which are internal structures and must be initialized
       on purpose"

  (if reference
    (case (type-of_vps list reference)
      (AIL (make_ail list reference))
      (ARL (make_arl list reference))
      (otherwise (error "UNRECOGNISABLE INPUT STRUCTURE : ~a " list)) )
    (case (type-of_vps list)
      (SPL (make_spl list))
      (RPL (make_rpl list))
      (CIL (make_cil list))
      (FQL (make_fql list))
      (CRL (make_crl list))
      (otherwise (error "UNRECOGNISABLE INPUT STRUCTURE : ~a " list)
                 ))) )

(defmethod make_spl ((list list))
  (make-instance 'spl :the-list list))

(defmethod make_rpl ((list list))
  (make-instance 'rpl :the-list list))

(defmethod make_ppl ((list list))
  (make-instance 'ppl :the-list list))

(defmethod make_cil ((list list))
  (make-instance 'cil :the-list list))

(defmethod make_ail ((list list) reference)
  (make-instance 'ail :the-list list :reference reference))

(defmethod make_fql ((list list))
  (make-instance 'fql :the-list list))

(defmethod make_crl ((list list))
  (make-instance 'crl :the-list list))

(defmethod make_arl ((list list) reference)
  (make-instance 'arl :the-list list :reference reference))


#|

;;;;;;;;;;;;;; AVAILABLE PITCH CONVERSIONS

 ---->      fq pch midi ratio itvl semitones pch-class
fq           \  *    *    *     0      0          0
pch          *  \    *    0     *      *          *
midi         0  *    \    0     0      *          *
ratio        *  0    0    \     *      *          0
itvl         *  *    *    *     \      *          0
semitones    0  0    0    *     *      \          0
pch-class    *  *    *    0     0      0          \
                                                   (* = yes , 0 = no)

|#

;;;;;;;;;;;;;; SELECTORS WITH POSSIBLE TYPE CONVERSION

;;;;;;;;;;;;;; SPL

; (spl_vps vps [reference when needed] [approximation])
;  Return the Symbolic Pitch List of a VPS

; if approximation = number -> number indicates the deviation (cents)
;     Ex: 50 = quarter tones, 33 = sixths of tones, etc.
; if approximation = t -> simple pitch list (without deviation)

; NB: Approximation is always the last argument and optional

;   SPL: (spl_vps spl) -> return the same SPL
;        (spl_vps spl approx)
;                      -> return an SPL with a new approximation
;   RPL: (spl_vps rpl octave)
;        (spl_vps rpl octave approx)

;   CIL: (spl_vps cil reference)
;        (spl_vps cil reference approx)

;   AIL: (spl_vps ail)
;        (spl_vps ail approx)

;   FQL: (spl_vps fql choice)
;                   "-> if positive integer = max-nn
;                   "-> if negative float = threshold (dB) (NOT YET)
;                   "-> if positive float = threshold
;        When max-nn is selected, if data reduction is needed and
;           there are no amplitudes, data reduction is disactivated
;        (spl_vps fql choice approx)

;   CRL: (spl_vps crl reference)
;        (spl_vps crl reference approx)

;   ARL: (spl_vps arl)
;        (spl_vps arl approx)

(defmethod spl_vps ((vps spl) &rest pars)
  (ifn pars
       (get-spl vps)
    (get-spl (make-instance 'FQL
               :the-list (get-fql vps))
             :approx (car pars))))

(defmethod spl_vps ((vps rpl) &rest pars)
  (when (null pars)
    (error "NEED AT LEAST THE OCTAVE NUMBER AS AN ARGUMENT"))
  (let ((octave (car pars))
        (approx (cadr pars)))
    (ifn approx
         (get-spl vps :octave octave)
      (get-spl (make-instance 'FQL
                 :the-list (get-fql vps :octave octave))
               :approx approx))))

(defmethod spl_vps ((vps cil) &rest pars)
  (when (null pars)
    (error "NEED AT LEAST A REFERENCE AS AN ARGUMENT"))
  (let ((ref (car pars))
        (approx (cadr pars)))
    (ifn approx
         (get-spl vps :reference ref)
      (get-spl (make-instance 'FQL
                 :the-list (get-fql vps :reference ref))
               :approx approx))))

(defmethod spl_vps ((vps ail) &rest pars)
  (ifn pars
       (get-spl vps)
    (get-spl (make-instance 'FQL
               :the-list (get-fql vps))
             :approx (car pars))))

(defmethod spl_vps ((vps fql) &rest pars)
  (when (null pars)
    (error "NEED AT LEAST A MAX-NN OR THRESHOLD AS AN ARGUMENT"))
  (let ((choice (car pars))
        (approx (cadr pars)))
    (ifn approx
         (cond ((and (integerp choice) (> choice 0))
                ; DECIDE WHETHER TO DISACTIVATE DATA REDUCTION
                (if (get-amp vps)
                  (get-spl vps :max-nn choice)
                  (get-spl vps :max-nn (number-of-notes vps))))
               ((numberp choice)
                (get-spl vps :threshold choice))
               (t (error "AMAZING CHOICE, SIR: ~a" choice)))
         (cond ((and (integerp choice) (> choice 0))
                (if (get-amp vps)
                  (get-spl vps :max-nn choice :approx approx)
                  (get-spl vps :max-nn (number-of-notes vps)
                            :approx approx)))
               ((numberp choice)
                (get-spl vps :threshold choice :approx approx))
               (t (error "AMAZING CHOICE, SIR: ~a" choice))))))

(defmethod spl_vps ((vps crl) &rest pars)
  (when (null pars)
    (error "NEED AT LEAST A REFERENCE AS AN ARGUMENT"))
  (let ((ref (car pars))
        (approx (cadr pars)))
    (ifn approx
         (get-spl vps :reference ref)
      (get-spl vps :reference ref :approx approx))))

(defmethod spl_vps ((vps arl) &rest pars)
  (ifn pars
       (get-spl vps)
    (get-spl vps :approx (car pars))))

(defmethod spl_vps ((vps t) &rest pars)
  (declare (ignore pars))
  ())
; TO ALLOW FOR EMPTY STRUCTURE: RETURN NIL IF THE DATA IS NOT GOOD
;  (error "CAN USE spl_vps WITH ONE OF THE 7 CLASSES OF VPS ONLY : ~a" vps))


;----------------------------------------------------------------------------
;;;;;;;;;;;;;; RPL

; (rpl_vps vps [reference when needed] [approximation])

;   SPL: (rpl_vps spl)
;        (rpl_vps spl approx)
;   RPL: (rpl_vps rpl)
;        (rpl_vps rpl approx)
;   CIL: (rpl_vps cil reference)
;        (rpl_vps cil reference approx)
;   AIL: (rpl_vps ail)
;        (rpl_vps ail approx)
;   FQL: (rpl_vps fql choice)
;                   "-> if positive integer = max-nn
;                   "-> if negative float = theshold (dB) (NOT YET)
;                   "-> if positive float = threshold
;        (rpl_vps fql choice approx)
;   CRL: (rpl_vps crl reference)
;        (rpl_vps crl reference approx)
;   ARL: (rpl_vps arl)
;        (rpl_vps arl approx)


(defmethod rpl_vps ((vps spl) &rest pars)
;(om::defmethod! rpl_vps ((vps spl) &rest pars)
;  :icon 130
  (ifn pars
       (get-rpl vps)
    (get-rpl (make-instance 'FQL
               :the-list (get-fql vps))
             :approx (car pars))))

(defmethod rpl_vps ((vps rpl) &rest pars)
;(om::defmethod! rpl_vps ((vps rpl) &rest pars)
  (ifn pars
       (get-rpl vps)
    (get-rpl (make-instance 'FQL
               :the-list (get-fql vps :octave 2))
             :approx (car pars))))

(defmethod rpl_vps ((vps cil) &rest pars)
;(om::defmethod! rpl_vps ((vps cil) &rest pars)
(when (null pars)
    (error "NEED AT LEAST A REFERENCE AS AN ARGUMENT"))
  (let ((ref (car pars))
        (approx (cadr pars)))
    (ifn approx
         (get-rpl vps :reference ref)
      (get-rpl (make-instance 'FQL
                 :the-list (get-fql vps :reference ref))
               :approx approx))))

(defmethod rpl_vps ((vps ail) &rest pars)
;(om::defmethod! rpl_vps ((vps ail) &rest pars)
  (ifn pars
       (get-rpl vps)
    (get-rpl (make-instance 'FQL
               :the-list (get-fql vps))
             :approx (car pars))))

(defmethod rpl_vps ((vps fql) &rest pars)
;(om::defmethod! rpl_vps ((vps fql) &rest pars)
  (when (null pars)
    (error "NEED AT LEAST A MAX-NN OR THRESHOLD AS AN ARGUMENT"))
  (let ((choice (car pars))
        (approx (cadr pars)))
    (ifn approx
         (cond ((and (integerp choice) (> choice 0))
                ; DECIDE WHETHER TO DISACTIVATE DATA REDUCTION
                (if (get-amp vps)
                  (get-rpl vps :max-nn choice)
                  (get-rpl vps :max-nn (number-of-notes vps))))
               ((numberp choice)
                (get-rpl vps :threshold choice))
               (t (error "AMAZING CHOICE, SIR: ~a" choice)))
         (cond ((and (integerp choice) (> choice 0))
                (if (get-amp vps)
                  (get-rpl vps :max-nn choice :approx approx)
                  (get-rpl vps :max-nn (number-of-notes vps)
                            :approx approx)))
               ((numberp choice)
                (get-rpl vps :threshold choice :approx approx))
               (t (error "AMAZING CHOICE, SIR: ~a" choice))))))

(defmethod rpl_vps ((vps crl) &rest pars)
;(defmethod rpl_vps ((vps crl) &rest pars)
;  :icon 130
  (when (null pars)
    (error "NEED AT LEAST A REFERENCE AS AN ARGUMENT"))
  (let ((ref (car pars))
        (approx (cadr pars)))
    (ifn approx
         (get-rpl vps :reference ref)
      (get-rpl vps :reference ref :approx approx))))

(defmethod rpl_vps ((vps arl) &rest pars)
;(om::defmethod! rpl_vps ((vps arl) &rest pars)
  (ifn pars
       (get-rpl vps)
    (get-rpl vps :approx (car pars))))

(defmethod rpl_vps ((vps t) &rest pars)
;(om::defmethod! rpl_vps ((vps t) &rest pars)
  (declare (ignore pars))
  ())
; TO ALLOW FOR EMPTY STRUCTURE: RETURN NIL IF THE DATA IS NOT GOOD
;  (error "CAN USE rpl_vps WITH ONE OF THE 7 CLASSES OF VPS ONLY : ~a" vps))


;----------------------------------------------------------------------------
;;;;;;;;;;;;;; CIL

; (cil_vps vps [approximation])
; (cils_vps vps [approximation])

;   cil_vps returns the CIL in symbolic notation
;   cils_vps returns the CIL in semitones forcing a float format

;   SPL: (cil_vps spl)
;        (cils_vps spl)
;        (cil_vps spl approx)
;        (cils_vps spl approx)
;   RPL: (cil_vps rpl)
;        (cils_vps rpl)
;        (cil_vps rpl approx)
;        (cils_vps rpl approx)
;   CIL: (cil_vps cil)
;        (cils_vps cil)
;        (cil_vps cil approx)
;        (cils_vps cil approx)
;   AIL: (cil_vps ail)
;        (cils_vps ail)
;        (cil_vps ail approx)
;        (cils_vps ail approx)
;   FQL: (cil_vps fql choice)
;                       "-> if positive integer = max-nn
;            (NOT YET)  "-> if negative float = threshold [dB]
;                       "-> if positive float = threshold [abs]
;        (cils_vps fql choice)
;        (cil_vps fql choice approx)
;        (cils_vps fql choice approx)
;   CRL: (cil_vps crl)
;        (cils_vps crl)
;        (cil_vps crl approx)
;        (cils_vps crl approx)
;   ARL: (cil_vps arl)
;        (cils_vps arl)
;        (cil_vps arl approx)
;        (cils_vps arl approx)


(defmethod cil_vps ((vps spl) &rest pars)
;(om::defmethod! cil_vps ((vps spl) &rest pars)
;  :icon 130
  (ifn pars
       (get-cil vps :midi nil)
    (get-cil (make-instance 'FQL
               :the-list (get-fql vps))
             :approx (car pars))) )

(defmethod cils_vps ((vps spl) &rest pars)
;(om::defmethod! cils_vps ((vps spl) &rest pars)
;  :icon 130
  (ifn pars
       (mapcar #'float (get-cil vps :midi t))
    (float-semitones
     (get-cil (make-instance 'FQL :the-list (get-fql vps))
              :approx (car pars)))))

(defmethod cil_vps ((vps rpl) &rest pars)
;(om::defmethod! cil_vps ((vps rpl) &rest pars)
;  :icon 130
  (ifn pars
       (get-cil vps)
    (get-cil (make-instance 'FQL
               :the-list (get-fql vps))
             :approx (car pars))) )

(defmethod cils_vps ((vps rpl) &rest pars)
;(om::defmethod! cils_vps ((vps rpl) &rest pars)
;  :icon 130
  (ifn pars
       (float-semitones (get-cil vps))
    (float-semitones
     (get-cil (make-instance 'FQL
                :the-list (get-fql vps :octave 2))
              :approx (car pars)))))

(defmethod cil_vps ((vps cil) &rest pars)
;(om::defmethod! cil_vps ((vps cil) &rest pars)
;    :icon 130
    (ifn pars
         (get-cil vps)
      (get-cil (make-instance 'FQL
                 :the-list (get-fql vps :reference 'DO2))
               :approx (car pars))))

(defmethod cils_vps ((vps cil) &rest pars)
;(om::defmethod! cils_vps ((vps cil) &rest pars)
;    :icon 130
    (ifn pars
         (float-semitones (get-cil vps))
      (float-semitones
               (get-cil (make-instance 'FQL
                          :the-list (get-fql vps :reference 'DO2))
                        :approx (car pars)))))


(defmethod cil_vps ((vps ail) &rest pars)
;(om::defmethod! cil_vps ((vps ail) &rest pars)
;    :icon 130
    (ifn pars
         (get-cil vps)
      (get-cil (make-instance 'FQL
                 :the-list (get-fql vps))
               :approx (car pars))))

(defmethod cils_vps ((vps ail) &rest pars)
;(om::defmethod! cils_vps ((vps ail) &rest pars)
;  :icon 130
  (ifn pars
       (float-semitones (get-cil vps))
    (float-semitones 
     (get-cil (make-instance 'FQL
                :the-list (get-fql vps))
              :approx (car pars)))))

(defmethod cil_vps ((vps fql) &rest pars)
;(om::defmethod! cil_vps ((vps fql) &rest pars)
;  :icon 130
  (when (null pars)
    (error "NEED AT LEAST A MAX-NN OR THRESHOLD AS AN ARGUMENT"))
  (let ((choice (car pars))
        (approx (cadr pars)))
    (ifn approx
         (cond ((and (integerp choice) (> choice 0))
                ; DECIDE WHETHER TO DISACTIVATE DATA REDUCTION
                (if (get-amp vps)
                  (get-cil vps :max-nn choice)
                  (get-cil vps :max-nn (number-of-notes vps))))
               ((numberp choice)
                (get-cil vps :threshold choice))
               (t (error "AMAZING CHOICE, SIR: ~a" choice)))
         (cond ((and (integerp choice) (> choice 0))
                (if (get-amp vps)
                  (get-cil vps :max-nn choice :approx approx)
                  (get-cil vps :max-nn (number-of-notes vps)
                            :approx approx)))
               ((numberp choice)
                (get-cil vps :threshold choice :approx approx))
               (t (error "AMAZING CHOICE, SIR: ~a" choice))))))

(defmethod cils_vps ((vps fql) &rest pars)
;(om::defmethod! cils_vps ((vps fql) &rest pars)
;  :icon 130
  (when (null pars)
    (error "NEED AT LEAST A MAX-NN OR THRESHOLD AS AN ARGUMENT"))
  (let ((choice (car pars))
        (approx (cadr pars)))
    (ifn approx
         (cond ((and (integerp choice) (> choice 0))
                ; DECIDE WHETHER TO DISACTIVATE DATA REDUCTION
                (if (get-amp vps)
                  (float-semitones (get-cil vps :max-nn choice))
                  (float-semitones
                   (get-cil vps :max-nn (number-of-notes vps)))))
               ((numberp choice)
                (float-semitones (get-cil vps :threshold choice)))
               (t (error "AMAZING CHOICE, SIR: ~a" choice)))
         (cond ((and (integerp choice) (> choice 0))
                (if (get-amp vps)
                  (float-semitones
                   (get-cil vps :max-nn choice :approx approx))
                  (float-semitones 
                   (get-cil vps :max-nn (number-of-notes vps)
                            :approx approx))))
               ((numberp choice)
                (float-semitones 
                 (get-cil vps :threshold choice :approx approx)))
               (t (error "AMAZING CHOICE, SIR: ~a" choice))))))


(defmethod cil_vps ((vps crl) &rest pars)
;(om::defmethod! cil_vps ((vps crl) &rest pars)
;    :icon 130
    (ifn pars
         (get-cil vps)
      (get-cil vps :approx (car pars))))

(defmethod cils_vps ((vps crl) &rest pars)
;(om::defmethod! cils_vps ((vps crl) &rest pars)
;    :icon 130
    (ifn pars
         (float-semitones (get-cil vps))
      (float-semitones (get-cil vps :approx (car pars)))))

(defmethod cil_vps ((vps arl) &rest pars)
;(om::defmethod! cil_vps ((vps arl) &rest pars)
;  :icon 130
  (ifn pars
       (get-cil vps)
    (get-cil vps :approx (car pars))))

(defmethod cils_vps ((vps arl) &rest pars)
;(om::defmethod! cils_vps ((vps arl) &rest pars)
;  :icon 130
  (ifn pars
       (float-semitones (get-cil vps))
    (float-semitones (get-cil vps :approx (car pars)))))

(defmethod cil_vps ((vps t) &rest pars)
;(om::defmethod! cil_vps ((vps t) &rest pars)
;  :icon 130
  (declare (ignore pars))
  ())
; TO ALLOW FOR EMPTY STRUCTURE: RETURN NIL IF THE DATA IS NOT GOOD
;  (error "CAN USE cil_vps WITH ONE OF THE 7 CLASSES OF VPS ONLY : ~a" vps))

(defmethod cils_vps ((vps t) &rest pars)
;(om::defmethod! cils_vps ((vps t) &rest pars)
;  :icon 130
  (declare (ignore pars))
  ())
; TO ALLOW FOR EMPTY STRUCTURE: RETURN NIL IF THE DATA IS NOT GOOD
;  (error "CAN USE cils_vps WITH ONE OF THE 7 CLASSES OF VPS ONLY : ~a" vps))


;----------------------------------------------------------------------------
;;;;;;;;;;;;;; AIL

; (ail_vps vps [approximation])
; (ails_vps vps [approximation])

;   ail_vps returns the AIL in symbolic notation
;   ails_vps returns the AIL in semitones forcing a float format

; REMARK: When using a given approximation (eg t) the approximation
;         is computed BEFORE calling "get-ail" with the "reference" by
;         transforming the argument into a FQL and converting it back
;         with a new appraximation.
;         Hence, if the reference does NOT use the same approximation
;         the result will be microtonal, since "get-ail" will compute
;         the distance between THIS REFERENCE and the APPROXIMATED FQL.

;   SPL: (ail_vps spl reference)
;        (ails_vps spl reference)
;        (ail_vps spl reference approx)
;        (ails_vps spl reference approx)
;   RPL: (ail_vps rpl reference octave)
;        (ails_vps rpl reference octave)
;        (ail_vps rpl reference octave approx)
;        (ails_vps rpl reference octave approx)
;   CIL: (ail_vps cil reference)
;        (ails_vps cil reference)
;        (ail_vps cil reference approx)
;        (ails_vps cil reference approx)
;   AIL: (ail_vps ail reference)
;        (ails_vps ail reference)
;        (ail_vps ail reference approx)
;        (ails_vps ail reference approx)
;   FQL: (ail_vps fql reference choice)
;                        "-> if positive integer = max-nn
;                        "-> if negative float = theshold (dB) (NOT YET)
;                        "-> if positive float = threshold
;        (ails_vps fql reference choice)
;        (ail_vps fql reference choice approx)
;        (ails_vps fql reference choice approx)
;   CRL: (ail_vps crl reference)
;        (ails_vps crl reference)
;        (ail_vps crl reference approx)
;        (ails_vps crl reference approx)
;   ARL: (ail_vps arl reference)
;        (ails_vps arl reference)
;        (ail_vps arl reference approx)
;        (ails_vps arl reference approx)


(defmethod ail_vps ((vps spl) &rest pars)
;(om::defmethod! ail_vps ((vps spl) &rest pars)
;  :icon 130
  (when (null pars)
    (error "WANNA A REFERENCE"))
  (let ((ref (car pars))
        (approx (cadr pars)))
  (ifn approx
       (get-ail vps :reference ref :midi nil)
    (get-ail (make-instance 'FQL
               :the-list (get-fql vps))
             :reference ref :midi nil :approx approx)) ))

(defmethod ails_vps ((vps spl) &rest pars)
  (when (null pars)
    (error "WANNA A REFERENCE"))
  (let ((ref (car pars))
        (approx (cadr pars)))
    (ifn approx
         (mapcar #'float (get-ail vps :reference ref :midi t))
      (float-semitones
       (get-ail (make-instance 'FQL
                  :the-list (get-fql vps))
                :reference ref :approx approx)) )))


(defmethod ail_vps ((vps rpl) &rest pars)
;(om::defmethod! ail_vps ((vps rpl) &rest pars)
;  :icon 130
  (when (null (cdr pars))
    (error "WANNA A REFERENCE AND AN OCTAVE NUMBER"))
  (let ((ref (car pars))
        (oct (cadr pars))
        (approx (caddr pars)))
  (ifn approx
       (get-ail vps :reference ref :octave oct)
    (get-ail (make-instance 'FQL
               :the-list (get-fql vps :octave oct))
              :reference ref :approx approx)) ))

(defmethod ails_vps ((vps rpl) &rest pars)
  (when (null (cdr pars))
    (error "WANNA A REFERENCE AND AN OCTAVE NUMBER"))
  (let ((ref (car pars))
        (oct (cadr pars))
        (approx (caddr pars)))
    (ifn approx
         (float-semitones
          (get-ail vps :reference ref :octave oct))
      (float-semitones
       (get-ail (make-instance 'FQL
                  :the-list (get-fql vps :octave oct))
                :reference ref :approx approx)) )))


(defmethod ail_vps ((vps cil) &rest pars)
;(om::defmethod! ail_vps ((vps cil) &rest pars)
;  :icon 130
  (when (null pars)
    (error "WANNA A REFERENCE"))
  (let ((ref (car pars))
        (approx (cadr pars)))
    (ifn approx
         (get-ail vps :reference ref)
      (get-ail (make-instance 'FQL
                 :the-list (get-fql vps :reference ref))
               :reference ref :approx approx))))

(defmethod ails_vps ((vps cil) &rest pars)
  (when (null pars)
    (error "WANNA A REFERENCE"))
  (let ((ref (car pars))
        (approx (cadr pars)))
    (ifn approx
         (float-semitones
          (get-ail vps :reference ref))
      (float-semitones
       (get-ail (make-instance 'FQL
                  :the-list (get-fql vps :reference ref))
                :reference ref :approx approx)))))


(defmethod ail_vps ((vps ail) &rest pars)
;(om::defmethod! ail_vps ((vps ail) &rest pars)
;  :icon 130
  (when (null pars)
    (error "WANNA A REFERENCE"))
  (let ((ref (car pars))
        (approx (cadr pars)))
    (ifn approx
         (get-ail vps :reference ref)
      (get-ail (make-instance 'FQL
                 :the-list (get-fql vps))
               :reference ref :approx approx))))

(defmethod ails_vps ((vps ail) &rest pars)
  (when (null pars)
    (error "WANNA A REFERENCE"))
  (let ((ref (car pars))
        (approx (cadr pars)))
    (ifn approx
         (float-semitones (get-ail vps :reference ref))
      (float-semitones
       (get-ail (make-instance 'FQL
                  :the-list (get-fql vps))
                :reference ref :approx approx)))))


(defmethod ail_vps ((vps fql) &rest pars)
;(om::defmethod! ail_vps ((vps fql) &rest pars)
;  :icon 130
  (when (null (cdr pars))
    (error "WITHOUT A REFERENCE AND MAX-NN OR THRESHOLD AS ARGUMENTS I DON'T WORK!"))
  (let ((ref (car pars))
        (choice (cadr pars))
        (approx (caddr pars)))
    (ifn approx
         (cond ((and (integerp choice) (> choice 0))
                ; DECIDE WHETHER TO DISACTIVATE DATA REDUCTION
                (if (get-amp vps)
                  (get-ail vps :max-nn choice :reference ref)
                  (get-ail vps :max-nn (number-of-notes vps)
                           :reference ref)))
               ((numberp choice)
                (get-ail vps :threshold choice :reference ref))
               (t (error "TRULY AMAZING CHOICE, SIR: ~a" choice)))
      (cond ((and (integerp choice) (> choice 0))
             (if (get-amp vps)
               (get-ail vps :max-nn choice :approx approx
                        :reference ref)
               (get-ail vps :max-nn (number-of-notes vps)
                        :approx approx :reference ref)))
            ((numberp choice)
             (get-ail vps :threshold choice :approx approx
                      :reference ref))
            (t (error "TRULY AMAZING CHOICE, SIR: ~a" choice))))))

(defmethod ails_vps ((vps fql) &rest pars)
  (when (null (cdr pars))
    (error "WITHOUT A REFERENCE AND MAX-NN OR THRESHOLD AS ARGUMENTS I DON'T WORK!"))
  (let ((ref (car pars))
        (choice (cadr pars))
        (approx (caddr pars)))
    (ifn approx
         (cond ((and (integerp choice) (> choice 0))
                ; DECIDE WHETHER TO DISACTIVATE DATA REDUCTION
                (if (get-amp vps)
                  (float-semitones
                           (get-ail vps :max-nn choice :reference ref))
                  (float-semitones
                           (get-ail vps :max-nn (number-of-notes vps)
                                    :reference ref))))
               ((numberp choice)
                (float-semitones
                 (get-ail vps :threshold choice :reference ref)))
               (t (error "TRULY AMAZING CHOICE, SIR: ~a" choice)))
      (cond ((and (integerp choice) (> choice 0))
             (if (get-amp vps)
               (float-semitones
                (get-ail vps :max-nn choice :approx approx
                         :reference ref))
               (float-semitones
                (get-ail vps :max-nn (number-of-notes vps)
                        :approx approx :reference ref))))
            ((numberp choice)
             (float-semitones
              (get-ail vps :threshold choice :approx approx
                      :reference ref)))
            (t (error "TRULY AMAZING CHOICE, SIR: ~a" choice))))))


(defmethod ail_vps ((vps crl) &rest pars)
;(om::defmethod! ail_vps ((vps crl) &rest pars)
;  :icon 130
  (when (null pars)
    (error "WANNA A REFERENCE"))
  (let ((ref (car pars))
        (approx (cadr pars)))
    (ifn approx
         (get-ail vps :reference ref)
      (get-ail vps :reference ref :approx approx))))

(defmethod ails_vps ((vps crl) &rest pars)
  (when (null pars)
    (error "WANNA A REFERENCE"))
  (let ((ref (car pars))
        (approx (cadr pars)))
    (ifn approx
         (float-semitones (get-ail vps :reference ref))
      (float-semitones (get-ail vps :reference ref :approx approx)))))


(defmethod ail_vps ((vps arl) &rest pars)
;(om::defmethod! ail_vps ((vps arl) &rest pars)
;  :icon 130
  (when (null pars)
    (error "WANNA A REFERENCE"))
  (let ((ref (car pars))
        (approx (cadr pars)))
    (ifn approx
       (get-ail vps :reference ref)
    (get-ail vps :reference ref :approx approx))))

(defmethod ails_vps ((vps arl) &rest pars)
  (when (null pars)
    (error "WANNA A REFERENCE"))
  (let ((ref (car pars))
        (approx (cadr pars)))
    (ifn approx
       (float-semitones (get-ail vps :reference ref))
    (float-semitones (get-ail vps :reference ref :approx approx)))))


(defmethod ail_vps ((vps t) &rest pars)
;(om::defmethod! ail_vps ((vps t) &rest pars)
;  :icon 130
  (declare (ignore pars))
  ())
; TO ALLOW FOR EMPTY STRUCTURE: RETURN NIL IF THE DATA IS NOT GOOD
; (error "CAN USE ail_vps WITH ONE OF THE 7 CLASSES OF VPS ONLY : ~a" vps))

(defmethod ails_vps ((vps t) &rest pars)
  (declare (ignore pars))
  ())
; TO ALLOW FOR EMPTY STRUCTURE: RETURN NIL IF THE DATA IS NOT GOOD
;  (error "CAN USE ails_vps WITH ONE OF THE 7 CLASSES OF VPS ONLY : ~a" vps))


;----------------------------------------------------------------------------
;;;;;;;;;;;;;; FQL

; (fql_vps vps [reference when needed])
;  Return the FreQuency List of a VPS

;   SPL: (fql_vps spl)
;   RPL: (fql_vps rpl octave)
;   CIL: (fql_vps cil reference)
;   AIL: (fql_vps ail)
;   FQL: (fql_vps fql)
;   CRL: (fql_vps crl reference)
;   ARL: (fql_vps arl)

(defmethod fql_vps ((vps vps) &rest pars)
;(om::defmethod! fql_vps ((vps vps) &rest pars)
;  :icon 130
  (declare (ignore pars))
  (get-fql vps))

(defmethod fql_vps ((vps rpl) &rest pars)
;(om::defmethod! fql_vps ((vps rpl) &rest pars)
;  :icon 130
  (when (null pars)
    (error "HOW CAN I FO MY JOB IF YOU DON'T GIVE ME THE OCTAVE NUMBER?"))
         (get-fql vps :octave (car pars)))

(defmethod fql_vps ((vps cil) &rest pars)
;(om::defmethod! fql_vps ((vps cil) &rest pars)
;  :icon 130
  (when (null pars)
    (error "GIVE ME THE REFERENCE, PLEASE"))
         (get-fql vps :reference (car pars)))

(defmethod fql_vps ((vps crl) &rest pars)
;(om::defmethod! fql_vps ((vps crl) &rest pars)
;  :icon 130
  (when (null pars)
    (error "GIVE ME THE REFERENCE, PLEASE"))
         (get-fql vps :reference (car pars)))

(defmethod fql_vps ((vps t) &rest pars)
;(om::defmethod! fql_vps ((vps t) &rest pars)
;  :icon 130
  (declare (ignore pars))
  ())
; TO ALLOW FOR EMPTY STRUCTURE: RETURN NIL IF THE DATA IS NOT GOOD
; (error "CAN USE spl_vps WITH ONE OF THE 7 CLASSES OF VPS ONLY : ~a" vps))


;----------------------------------------------------------------------------
;;;;;;;;;;;;;; CRL

; (crl_vps vps)
;  Return the Contiguous Ratio List of a VPS

;   SPL: (crl_vps spl)
;   RPL: (rcl_vps rpl)
;   CIL: (rcl_vps cil)
;   AIL: (rcl_vps ail)
;   FQL: (rcl_vps fql)
;   CRL: (rcl_vps crl)
;   ARL: (rcl_vps arl)

(defmethod crl_vps ((vps vps))
;(om::defmethod! crl_vps ((vps vps))
;  :icon 130
  (get-crl vps))

(defmethod crl_vps ((vps t))
;(om::defmethod! crl_vps ((vps t))
  ())
; TO ALLOW FOR EMPTY STRUCTURE: RETURN NIL IF THE DATA IS NOT GOOD
;  (error "CAN USE spl_vps WITH ONE OF THE 7 CLASSES OF VPS ONLY : ~a" vps))


;----------------------------------------------------------------------------
;;;;;;;;;;;;;; ARL
; (arl_vps vps reference [octave when needed])
;  Return the Anchored Ratio List of a VPS

;   SPL: (arl_vps spl)
;   RPL: (arl_vps rpl octave)
;   CIL: (arl_vps cil)
;   AIL: (arl_vps ail)
;   FQL: (arl_vps fql)
;   CRL: (arl_vps crl)
;   ARL: (arl_vps arl)

(defmethod arl_vps ((vps vps) ref &rest pars)
;(om::defmethod! arl_vps ((vps vps) ref &rest pars)
;  :icon 130
  (declare (ignore pars))
  (get-arl vps :reference ref))

(defmethod arl_vps ((vps rpl) ref &rest pars)
;(om::defmethod! arl_vps ((vps rpl) ref &rest pars)
  (when (null pars)
    (error "HOW CAN I FO MY JOB IF YOU DON'T GIVE ME THE OCTAVE NUMBER?"))
         (get-arl vps :octave (car pars) :reference ref))

(defmethod arl_vps ((vps t) ref &rest pars)
;(om::defmethod! arl_vps ((vps t) ref &rest pars)
  (declare (ignore pars ref))
  ())
; TO ALLOW FOR EMPTY STRUCTURE: RETURN NIL IF THE DATA IS NOT GOOD
;  (error "CAN USE spl_vps WITH ONE OF THE 7 CLASSES OF VPS ONLY : ~a" vps))
;____________________________________________________________________________

;;;;;;;;;;;;;; SIMPLE SELECTORS

; (vps_vps vps)
;  Return the internal list of a VPS
(defmethod vps_vps ((vps vps))
  (the-list vps))


; (nn_vps vps)
;  Return the Number of Notes of a VPS
(defmethod nn_vps ((vps vps))
;(om::defmethod! nn_vps ((vps vps))
;  :icon 130
  (number-of-notes vps))


; (gil_vps vps)
; (gils_vps vps)
;  Return the Global Interval List in a symbolic or semitonal notation
(defmethod gil_vps ((vps vps))
;(om::defmethod! gil_vps ((vps vps))
;    :icon 130
    (get-gil vps))
(defmethod gils_vps ((vps vps))
;(om::defmethod! gils_vps ((vps vps))
;    :icon 130 
    (mapcar #'float (get-gil vps :midi t)))


; (surf_vps vps)
; (surf-s_vps vps)
;  Return the Surface of a VPS as an interval or in semitones
(defmethod surf_vps ((vps vps))
;(om::defmethod! surf_vps ((vps vps))
;    :icon 130
    (get-surface vps))

(defmethod surf-s_vps ((vps vps))
;(om::defmethod! surf-s_vps ((vps vps))
;    :icon 130
    (float (get-surface vps :midi t)))


; (dens_vps vps)
;  Return the Density of a VPS
;  A density of 1.0 = a cluster of semitones, of 2.0 = 1/4 tones,...
(defmethod dens_vps ((vps vps))
;(om::defmethod! dens_vps ((vps vps))
;    :icon 130
    (float (get-density vps)))


; (hom_vps vps)
; (hom-s_vps vps)
; (hom-e_vps vps)
; (hom-es_vps vps)
;  Return the Coefficient of Homogeneity of a VPS as
;      hom: a single symbolic interval
;      hom-s: a single interval in semitones
;      hom-e: a list of of two symbolic intervals
;      hom-es: a list of two intervals in semitones
(defmethod hom_vps ((vps vps))
;(om::defmethod! hom_vps ((vps vps))
;  :icon 130
  (get-homogeneity vps))
(defmethod hom-s_vps ((vps vps))
;(om::defmethod! hom-s_vps ((vps vps))
;  :icon 130
  (float (get-homogeneity vps :midi t)))
(defmethod hom-e_vps ((vps vps))
;(om::defmethod! hom-e_vps ((vps vps))
;  :icon 130
  (get-homogeneity vps :expanded t))
(defmethod hom-es_vps ((vps vps))
;(om::defmethod! hom-es_vps ((vps vps))
;  :icon 130
  (mapcar #'float (get-homogeneity vps :midi t :expanded t)))


; (sd_vps vps)
;  Return the Standard Deviation of a VPS (minimum 4 items)
(defmethod sd_vps ((vps vps))
;(om::defmethod! sd_vps ((vps vps))
;    :icon 130
    (get-sd vps))

; (cs_vps vps [S-Space])
;  Return the Coefficient of Stability of a VPS
;  Take the values of the constant *STABILITY-SPACE* if second argument is nil
(defmethod cs_vps ((vps vps) &optional ss)
;(om::defmethod! cs_vps ((vps vps) &optional ss)
;    :icon 130
    (get-cs vps :space ss))


;----------------------------------------------------------------------------
; (harm_vps vps [f0])
;  Return the Coefficient of Harmonicity of a VPS
;  If f0 is missing, take the first if FQL or the one on ARL
;  Does not work for RPL, CIL and CRL which must be converted into another VPS
(defmethod harm_vps ((vps vps) &optional f0)
;(om::defmethod! harm_vps ((vps vps) &optional f0)
;  :icon 130
  (if f0
    (float (get-harmonicity vps :f0 f0))
    (float (get-harmonicity vps))))

(defmethod harm_vps ((vps rpl) &optional f0)
;(om::defmethod! harm_vps ((vps rpl) &optional f0)
  (declare (ignore f0))
  (error "CANNOT ACCEPT A RPL, SIR. WANNA ANOTHER VPS: ~a" vps))
(defmethod harm_vps ((vps cil) &optional f0)
;(om::defmethod! harm_vps ((vps cil) &optional f0)
  (declare (ignore f0))
  (error "CANNOT ACCEPT A CIL, SIR. WANNA ANOTHER VPS: ~a" vps))
(defmethod harm_vps ((vps crl) &optional f0)
;(om::defmethod! harm_vps ((vps crl) &optional f0)
  (declare (ignore f0))
  (error "CANNOT ACCEPT A CRL, SIR. WANNA ANOTHER VPS: ~a" vps))

(defmethod harm-e_vps ((vps vps) &optional f0)
;(om::defmethod! harm-e_vps ((vps vps) &optional f0)
;  :icon 130
  (if f0
    (mapcar #'float (get-harmonicity vps :f0 f0 :expanded t))
    (mapcar #'float (get-harmonicity vps :expanded t))))

(defmethod harm-e_vps ((vps rpl) &optional f0)
;(om::defmethod! harm-e_vps ((vps rpl) &optional f0)
  (declare (ignore f0))
  (error "CANNOT ACCEPT A RPL, SIR. WANNA ANOTHER VPS: ~a" vps))
(defmethod harm-e_vps ((vps cil) &optional f0)
;(om::defmethod! harm-e_vps ((vps cil) &optional f0)
  (declare (ignore f0))
  (error "CANNOT ACCEPT A CIL, SIR. WANNA ANOTHER VPS: ~a" vps))
(defmethod harm-e_vps ((vps crl) &optional f0)
;(om::defmethod! harm-e_vps ((vps crl) &optional f0)
  (declare (ignore f0))
  (error "CANNOT ACCEPT A CRL, SIR. WANNA ANOTHER VPS: ~a" vps))


;----------------------------------------------------------------------------
; (vf0_vps vps [grid])
;  Return the Virtual Fundamental of a VPS
;  Does not work for RPL, CIL and CRL which must be converted into another VPS
(defmethod vf0_vps ((vps vps) &optional grid)
;(om::defmethod! vf0_vps ((vps vps) &optional grid)
;  :icon 130
  (if grid
    (get-virt-fund vps :grid-ratio grid)
    (get-virt-fund vps)))

(defmethod vf0_vps ((vps rpl) &optional f0)
;(om::defmethod! vf0_vps ((vps rpl) &optional f0)
  (declare (ignore f0))
  (error "CANNOT ACCEPT A RPL, SIR. WANNA ANOTHER VPS: ~a" vps))
(defmethod vf0_vps ((vps cil) &optional f0)
;(om::defmethod! vf0_vps ((vps cil) &optional f0)
  (declare (ignore f0))
  (error "CANNOT ACCEPT A CIL, SIR. WANNA ANOTHER VPS: ~a" vps))
(defmethod vf0_vps ((vps crl) &optional f0)
;(om::defmethod! vf0_vps ((vps crl) &optional f0)
  (declare (ignore f0))
  (error "CANNOT ACCEPT A CRL, SIR. WANNA ANOTHER VPS: ~a" vps))


;----------------------------------------------------------------------------
; (max-fq_vps vps)
; (min-fq_vps vps)
;  Return the Maximum/Minimum Frequency of a VPS
;  Does not work for RPL, CIL and CRL which must be converted into another VPS
(defmethod max-fq_vps ((vps vps))
  (get-max-fq vps))

(defmethod max-fq_vps ((vps rpl))
  (error "NO RPL's, SIR. GIVE ME SOMETHING ELSE, PLEASE: ~a" vps))
(defmethod max-fq_vps ((vps cil))
  (error "NO CIL's, SIR. GIVE ME SOMETHING ELSE, PLEASE: ~a" vps))
(defmethod max-fq_vps ((vps crl))
  (error "NO CRL's, SIR. GIVE ME SOMETHING ELSE, PLEASE: ~a" vps))

(defmethod min-fq_vps ((vps vps))
  (get-min-fq vps))

(defmethod min-fq_vps ((vps rpl))
  (error "NO RPL's, SIR. GIVE ME SOMETHING ELSE, PLEASE: ~a" vps))
(defmethod min-fq_vps ((vps cil))
  (error "NO CIL's, SIR. GIVE ME SOMETHING ELSE, PLEASE: ~a" vps))
(defmethod min-fq_vps ((vps crl))
  (error "NO CRL's, SIR. GIVE ME SOMETHING ELSE, PLEASE: ~a" vps))

;----------------------------------------------------------------------------
; (max-amp_vps vps)
; (min-amp_vps vps)
; (max-bw_vps vps)
; (min-bw_vps vps)
;  Return the Maximum/Minimum Amplitude/Bandwidth of a FQL
;  Active only with FQL's, otherwise nil is returned
(defmethod max-amp_vps ((vps vps))
  (get-max-amp vps))
(defmethod min-amp_vps ((vps vps))
  (get-min-amp vps))
(defmethod max-bw_vps ((vps vps))
  (get-max-bw vps))
(defmethod min-bw_vps ((vps vps))
  (get-min-bw vps))
;____________________________________________________________________________

;;;;;;;;;;;;;; UTILITIES

; SPSHT_VPS
; (spsht_vps F0 NP SH ST [random])
; (spsht F0 NP SH ST [STON])
;	Generate a FQL with a shifted spectrum
;	F0 = fundamental frequency [Hz or pitch]
;	NP = number of components to generate
;	   it may be:
;		a NUMBER : all the components from 1 to NP will be generated
;		a LIST containing a list of the scalers of f0
;	SH = shifting factor (% of f0)
;	ST = stretching/compressing factor (2.0 = octave)
;	STON = % of detuning applied when computing each component [0-1]
; EX: (spsht 440 10 2.1), (spsht 440 10 1.76 0.06)
;	(spsht 440 '(2 3.3 5.1 7) 2.5 0.01)

(defmethod spsht_vps (f0 np sh st &rest ran)
;(om::defmethod! spsht_vps (f0 np sh st &rest ran)
;  :icon 130
  (let ((ran (ifn ran 0.0 (car ran))))
    (make_vps (spsht (pch->fq f0) np sh st ran))))

;----------------------------------------------------------------------------
; XPOSE_VPS
; XPOSE-END_VPS
; (xpose_vps spl starting-pitch)
; (xpose-end_vps spl starting-pitch)
;	Transpose a VPS so that its lowest/highest note begins at starting-pitch
;	SPL = SPL or list of SPL's, if not, error
;	starting-pitch = symbolic pitch
; Ex: (spl_vps (anch-xpose_vps (make_vps '("DO4" "MI4" "SOL4") "FAd3"))

(defmethod xpose_vps ((vps spl) beg-pch)
;(om::defmethod! xpose_vps ((vps spl) beg-pch)
;  :icon 130 
  (unless (pitch-with-octave-p beg-pch)
    (if (pitch-without-octave-p beg-pch)
      (error "~%SORRY, BUT I NEED AN OCTAVE WITH YOUR PITCH, Sir ~a~%"
             (get-gbl 'USER))
      (error "~%~%I'M TROUBLED, SIR,
    HOW CAN I TRANSPOSE SOMETHING TO THIS PITCH, SIR: ~a~%" beg-pch)))
  (let ((cil (cil_vps vps)))
    (make_vps (itvl->pch cil beg-pch))))
   
(defmethod xpose_vps ((l-vps list) beg-pch)
;(om::defmethod! xpose_vps ((l-vps list) beg-pch)
  (loop for vps in l-vps
        collect (xpose_vps vps beg-pch)))
   
(defmethod xpose_vps ((vps t) beg-pch)
;(om::defmethod! xpose_vps ((vps t) beg-pch)
  (declare (ignore beg-pch))
  (error "~%~%FOR THE TIME BEING, I CAN ONLY WORK IF YOU GIVE ME AN SPL, MILORD
    WHY DID YOU GIVE ME THIS? ~%~a~%" vps))


(defmethod xpose-end_vps ((vps spl) end-pch)
;(om::defmethod! xpose-end_vps ((vps spl) end-pch)
;  :icon 130
  (unless (pitch-with-octave-p end-pch)
    (if (pitch-without-octave-p end-pch)
      (error "~%SORRY, BUT I NEED AN OCTAVE WITH YOUR PITCH, Sir ~a~%"
             (get-gbl 'USER))
      (error "~%~%I'M TROUBLED, SIR,
    HOW CAN I TRANSPOSE SOMETHING TO THIS PITCH, SIR: ~a~%" end-pch)))
  (let ((itvl (car (pch->itvl (list (car (last (spl_vps vps)))
                                    end-pch)))))
    (transpose vps itvl)))
   
(defmethod xpose-end_vps ((l-vps list) end-pch)
;(om::defmethod! xpose-end_vps ((l-vps list) end-pch)
  (loop for vps in l-vps
        collect (xpose-end_vps vps end-pch)))
   
(defmethod xpose-end_vps ((vps t) end-pch)
;(om::defmethod! xpose-end_vps ((vps t) end-pch)
  (declare (ignore end-pch))
  (error "~%~%FOR THE TIME BEING, I CAN ONLY WORK IF YOU GIVE ME AN SPL, MILORD
    WHY DID YOU GIVE ME THIS? ~%~a~%" vps))
;____________________________________________________________________________

;____________________________________________________________________________

;;;;;;;;;;;;;; PREDICATES

;            is_vs, is_vps
;            is_spl/rpl/...

; is_vs
(defmethod is_vs (x)
  (typep x 'vs))

; is_vps
(defmethod is_vps (x)
  (typep x 'vps))

; is_chord
(defmethod is_chord (x)
  (typep x 'chord))

; is_spectrum
(defmethod is_spectrum (x)
  (typep x 'spectrum))

; is_spl
(defmethod is_spl (x)
  (typep x 'spl))

; is_rpl
(defmethod is_rpl (x)
  (typep x 'rpl))

; is_ppl
(defmethod is_ppl (x)
  (typep x 'ppl))

; is_cil
(defmethod is_cil (x)
  (typep x 'cil))

; is_ail
(defmethod is_ail (x)
  (typep x 'ail))

; is_fql
(defmethod is_fql (x)
  (typep x 'fql))

; is_crl
(defmethod is_crl (x)
  (typep x 'crl))

; is_arl
(defmethod is_arl (x)
  (typep x 'arl))


;*****************************************************************************
;;;;;;;;;;;;;; INFO

;            type-of_vps

; (type-of_vps list [reference])
; Args: list [reference when needed]

(defmethod type-of_vps (list &optional reference)
  " Automatically finds one of the 7 VPS classes of the input argument.
  Return nil if no match is found.
  For PPL's, internal structure, it does not work"
  
  (cond
   ((not (consp list)) (format nil "WANNA A LIST, SIR! ~a" list))
   ((and reference
         (interval-p (car list)))
    'AIL)
   ((and reference
         (numberp (car list)))
    'ARL)
   ((and (null reference)
         (pitch-with-octave-p (car list)))
    'SPL)
   ((and (null reference)
         (pitch-without-octave-p (car list))
         (pitch-without-octave-p (car (last list))))
    'RPL)
   ((and (null reference)
         (pitch-without-octave-p (car list)))
    'RPL)
   ((and (null reference)
         (interval-p (car list)))
    'CIL)
   ((and (null reference)
         (numberp (car list))
         (>= (get-gbl 'MINFQ) (apply 'max list)))
    'CRL)
   ((and (null reference)
         (numberp (car list))
         (< (get-gbl 'MINFQ) (apply 'max list)))
    'FQL)
   (t (format nil "DUNNO THIS TYPE: ~a" (car list)))
   ))
;*****************************************************************************
