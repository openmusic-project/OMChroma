#|

THOROUGH TESTS OF THE DIFFERENT VPS'S
                                      AND
                 USER'S REFERENCE GUIDE
                                   (UFM)



|#


;ATTENTION: MUST EVALUATE in-package AT THE BEGINNING!!!

(in-package chroma)


;;;;;;CLASSES - INFO;;;;;;;;;;;;;;;;;;;

(class-name (find-class 'vps))
;(class-name (class-of my-vps))
(documentation 'vs 'type)
(documentation 'vps 'type)
(documentation 'spl 'type)
(documentation 'arl 'type)


;;;;;; PITCH CONVERSIONS ;;;;;;;;;;;;;;;;;;;;

#|

;;;;;;;;;;;;;; AVAILABLE PITCH CONVERSIONS

 ---->      fq pch midi   midic   ratio itvl semitones pch-class
fq           \  *    *      *       *     0      0          0
pch          *  \    *      *       0     *      *          *
midi         0  *    \      *       0     0      *          *
midic        0  0    *      \       0     0      0          0 
ratio        *  0    0      0       \     *      *          0
itvl         *  *    *      *       *     \      *          0
semitones    0  0    0      0       *     *      \          0
pch-class    *  *    *      0       0     0      0          \
                                                      (* = yes , 0 = no)
|#


;;;;;;FREQUENCY

; ---->      fq pch midi midic ratio itvl semitones pch-class
;fq           \  *    *      *       *     0      0          0

; fq->pch: correct expressions
(fq->pch 445.4)
(fq->pch '(445.4 550.0 660))
(fq->pch '(445.4 550.0 670) 50.0) ; approximation in cents (here quarter-tones)
(fq->pch '(445.4 550.0 670) 0) ; no approximation
                              ; CANNOT BE 0.0, ERROR (div by 0)
(fq->pch '(445.4 550.0 670) t) ; generates a pitch without a deviation in cents

(fq->pch 443)
(fq->pch 443 50)
(fq->pch '(442 444 446 447))
(fq->pch '(442 450 460 470) 25)
(fq->pch '(442 (SIb6 . 13) 550))
(fq->pch '(442 SIb4 550 ("DO2" . 34)))

; fq->midi: correct expressions
(fq->midi 445.4)
(fq->midi '(445.4 523.0 659.2))
(fq->midi 443)
(fq->midi '(442 444 446 447))

; fq->midic : correct expressions
(fq->midic '(445.6 550.0 1000.0 2000.0 442.0))
(fq->midic '(442 444 446 447))
(fq->midic 446.5)

; fq->ratio: correct expressions
(fq->ratio '(445.6 550.0 1000.0 2000.0))
(fq->ratio '(442 444 446 447))


;;;;;;PITCH

; ---->      fq pch midi midic ratio itvl semitones pch-class
;pch          *  \    *    *     0     *      *         *

; pch->fq: correct expressions
(pch->fq '(345.6 "LA4" ("SI4" . 23) "DO5"))
(pch->fq '(345.6 ("LA4" "SI4") ("SI4" . 23) "DO5")) ; any trees possible as arg
(pch->fq '(345.6 "LA4" ("SI4" . 23) "DO5") 444.0) ; diapason
(pch->fq 'LA3)
(pch->fq 'LA3 442)
(pch->fq 440 442)
(pch->fq '(LA3 C2 Ds4 440 REb5) 442)
(pch->fq '(LA3 (C2 . 12) (Ds4 . -50) 440 (REb5 . q)) 442)

; pch->midi: correct expressions
(pch->midi '("DO4" "LA4" ("SI4" . 23) 71 "DO5")) ; if number, it is taken as midi
(pch->midi '("DO4" "LA4" ("SI4" . 23) 71 "DO5") 444.0) ; diapason, no effect

; pch->midic: correct expressions
(pch->midic '("DO4" "LA4" ("SI4" . 23) 71 "DO5")) ; if number, it is taken as midi
(pch->midic '("DO4" "LA4" ("SI4" . 23) 71 "DO5") 444.0) ; diapason, no effect

; pch->itvl: correct expressions
(pch->itvl '("DO3" "MI3"))
(pch->itvl '(DO)) ; returns nil
(pch->itvl '(DO MI)) ; returns a list of one interval
(pch->itvl '(DO MI SOL)) ; returns a list of two intervals
(pch->itvl '((DO . 50) MI (SOL . 50)))

; pch->semitones: correct expressions
(pch->semitones '("DO3" "MI3")) ; returns a list of one number
(pch->semitones '(DO)) ; returns nil
(pch->semitones '(DO MI))
(pch->semitones '(DO MI SOL)) ; returns a list of two intervals
(pch->semitones '((DO . 50) MI (SOL . 50.0)))
                                 ; returns rational or floating point vals

; pch->pch-class: correct expressions
(pch->pch-class '("DO4" "MI4")) ; returns (0 4)
(pch->pch-class 'DO) ; returns 0 (DO4 by default)
(pch->pch-class "DO") ; returns 0 (DO4 by default)
(pch->pch-class '(DO MI))
(pch->pch-class '(DO ((MI)) SOL)) ; returns (0 ((4)) 7)
(pch->pch-class '((DO4 . 50) MI4 (SOL4 . 50.0)))
                                 ; returns rational or floating point vals
(pch->pch-class '((DO . 87) MI (SOL . 22.0)))
                                 ; returns the same vals as above


;;;;;;MIDI

; ---->      fq pch midi midic ratio itvl semitones pch-class
;midi         0  *    \    *     0     0      *         *

; all these expressions understand BOTH MIDI and midic values
; arg is midi if value < 127

; midi->pch: correct expressions
(midi->pch '60.3)
(midi->pch '(60.3 67.0 69))
(midi->pch '(6030 67.0 6900))
(midi->pch 69)
(midi->pch 69.263)
(midi->pch '(69 71.2 44))
(midi->pch 69.43 10)
(midi->pch '(69 70 7365 43.2) 50)

; midi->semitones: correct expressions
(midi->semitones '(60.3 67.0 69))
(midi->semitones '(69 70 7345 6135))
(midi->semitones '(69 70 73.45 61.35))

; midi->pch-class: correct expressions
(midi->pch-class '(60 64)) ; returns (0 4)
(midi->pch-class 60) ; returns 0 (DO4 by default)
(midi->pch-class 60.234) ; returns 0.234 (not meaningful in set theory)
(midi->pch-class 6023) ; returns 0.23 (not meaningful in set theory)
(midi->pch-class '(6900 71.2 44)) ; values below 60 are negative

; midi->midic: correct expressions
(midi->midic '(60 64))
(midi->midic 60)
(midi->midic 60.234)
(midi->midic 6023.4)
(midi->midic '(69 7120 44))

; from-midic
; if value < 127 return it, otherwise it's a midicent return the integer midi
(from-midic '(60 64.5)) ; returns (60 64.5)
(from-midic '(6000 6450)) ; returns (60 64.5)


;;;;;;midic

; ---->      fq pch midi midic ratio itvl semitones pch-class
;midic        0  0    *    \     0     0      0         0 

; midic->midi
(midic->midi '(6000 6450))
(midic->midi 4567)


;;;;;;RATIO

; ---->      fq pch midi midic ratio itvl semitones pch-class
;ratio        *  0    0    0     \     *      *         0

; ratio->fq: correct expressions
(ratio->fq 2.0 440.0) ; ratio, reference frequency
(ratio->fq '(2.0 3.0 4.0) 100.0)
(ratio->fq 0.5 440)
(ratio->fq () 440)
(ratio->fq '(0.5 1.5 2.0 0.99) 440)

; ratio->itvl: correct expressions
(ratio->itvl 2.0) ; returns the interval corresponding to this ratio (list)
(ratio->itvl '(2.0 0.5 2 1.059468))
(ratio->itvl 0.5)
(ratio->itvl '(0.5))
(ratio->itvl '(0.5 1.6 2.0 0.99))

; ratio->semitones: correct expressions
(ratio->semitones 2)
(ratio->semitones '(2.0 0.5 2 1.059468))
(ratio->semitones 0.5)
(ratio->semitones '(0.5))
(ratio->semitones '(0.5 1.6 2.0 0.99))


;;;;;;INTERVAL

; ---->      fq pch midi midic ratio itvl semitones pch-class
;itvl         *  *    *    *     *     \      *         0

; itvl->fq: correct expressions
(itvl->fq '(3-) 440.0) ; itvl (or list), reference frequency
(itvl->fq '((3- 0 34.2)) 440.0)
(itvl->fq '((3- 0 34.2) (4 1)) 100.0)
(itvl->fq '6+ 440)
(itvl->fq '(6+) 440)
(itvl->fq '(6+ -3- (-2- -1 12) (1 1)) 440)

; itvl->midi: correct expressions
(itvl->midi '(3-) 60) ; itvl (or list), reference midi
(itvl->midi '((3- 0 34.2)) 60)
(itvl->midi '((3- 0 34.2) (4 1)) 60.0)
(itvl->midi '6+ 60)
(itvl->midi '(6+) 60)
(itvl->midi '(6+ -3- (-2- -1 12) (1 1)) 60)

; itvl->midic: correct expressions
(itvl->midic '(3-) 60) ; itvl (or list), reference midi
(itvl->midic '((3- 0 34.2)) 60)
(itvl->midic '((3- 0 34.2) (4 1)) 60.0)
(itvl->midic '6+ 60)
(itvl->midic '(6+) 60)
(itvl->midic '(6+ -3- (-2- -1 12) (1 1)) 60)

; itvl->pch: correct expressions
(itvl->pch '3- "DO4" 50) ; itvl (or list), reference pitch, approximation
                           ; return a list of 2 or + pitches starting at reference
(itvl->pch '((3- 0 34.2)) 100.0)
(itvl->pch '((3- 0 34.2) (4 1)) "LA4")

; itvl->ratio: correct expressions
(itvl->ratio '(3-))
(itvl->ratio '((3- 0 34.2)))
(itvl->ratio '6+)
(itvl->ratio '(6+))
(itvl->ratio '(6+ -3- (-2- -1 12) (1 1)))

; itvl->semitones: correct expressions
(itvl->semitones '3-)
(itvl->semitones '(3-))
(itvl->semitones '((3- 0 34.2)))
(itvl->semitones '6+)
(itvl->semitones '(6+))
(itvl->semitones '(6+ -3- (-2- -1 12) (1 1)))


;;;;;;SENITONES

; ---->      fq pch midi midic ratio itvl semitones pch-class
;semitones    0  0    0    0     *     *      \         0

; semitones->ratio: correct expressions
(semitones->ratio 3)
(semitones->ratio '(3 4.0 5.6))
(semitones->ratio 6)
(semitones->ratio '(6))
(semitones->ratio '(6 1 0 12 12.2))

; semitones->itvl: correct expressions
(semitones->itvl 3)
(semitones->itvl '(3 4.0 5.6))
(semitones->itvl 6)
(semitones->itvl '(6))
(semitones->itvl '(6 1 0 12 12.2))


;;;;;;PITCH CLASSES

; ---->      fq pch midi midic ratio itvl semitones pch-class
;pch-class    *  *    *    0     0     0      0         \

; pch-class->pch: correct expressions
(pch-class->pch 3)
(pch-class->pch 3 t) ; approximation t => pitch without deviation
(pch-class->pch '(0 1 4 5))
(pch-class->pch '(0 1 4 5) t)
(pch-class->pch '(0 1.1 4.5 5) 0)
(pch-class->pch '(0 1.1 4.5 5))
(pch-class->pch '(0 1.1 (4.5 4.6 4.7) 5))
(pch-class->pch '(0 1.1 (4.5 4.6 4.7) 5) 33)

; pch-class->midi: correct expressions
(pch-class->midi 3)
(pch-class->midi '(0 1 4 5))
(pch-class->midi '(0 1.1 4.5 5))
(pch-class->midi '(0 1.1 (4.5 4.6 4.7) 5))

; pch-class->fq: correct expressions
(pch-class->fq 3)
(pch-class->fq '(0 1 4 5))
(pch-class->fq '(0 1.1 4.5 5))
(pch-class->fq '(0 1.1 (4.5 4.6 4.7) 5))



;;;;;; VPS: SINGLE INSTANCIATIONS ;;;;;;;;;;;;;;;;;;;

;;;;;;   Class: CHORD

;SPL
(setf my-spl (make-instance 'spl :the-list '(DO4)))
(setf my-spl (make-instance 'spl :the-list '(DO2  DO3 do5)))
(setf my-spl
      (make-instance 'spl
        :the-list '((DO4 . 31)("LAb4" . -5) RE5 SOL5 (REb6 . -q))))

;RPL
(setf my-rpl (make-instance 'rpl :the-list '(DO LA Sol1)))
(setf my-rpl (make-instance 'rpl :the-list '(DO SOL)))
(setf my-rpl
      (make-instance 'rpl
        :the-list '((DO . 12) (LAb . -5) RE1 SOL1 (REb2 . -q))))

;CIL
(setf my-cil (make-instance 'cil :the-list '(6- 7+ 6- 6-)))
(setf my-cil (make-instance 'cil :the-list '((6- 0 7)(4+ 0 5) 4 (4+ 0 1))))

;AIL
(setf my-ail (make-instance 'ail :reference 200. :the-list '(-6+ -2- 4 7- (3+ 1))))
(setf my-ail
      (make-instance 'ail :reference "DO3" 
                     :the-list '((1 -1 12) (2+ 0 -5) 3+ 7+ (3- 1 -50))))

;PPL (for internal usage only)
(setf my-ppl (make-instance 'ppl :the-list '(DO LA SOL)))
(setf my-ppl
      (make-instance 'ppl
        :the-list '((DO . 12) (LAb . -5) RE SOL (REb . -q))))


;;;;;;   Class: SPECTRUM

;FQL
(setf my-fql
      (make-instance 'fql
        :the-list '(30 50.1 60)))

(setf my-fql1
      (make-instance 'fql
        :the-list '(30 31 32 33 34 35 36 50.1 60 70 80 90
                    91 92 93 94 95 96 97 98 99 100 101 102)))

(setf my-spectrum
      (make-instance 'fql
        :the-list '(30 50.1 60)
        :amplitudes '(101 201 5)
        :bwl '(1001 2001 6)))

(setf my-spectrum1
      (make-instance 'fql
        :the-list '(25 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 52) 
        :amplitudes '(.1 3 .05 .2 .3 .4 .2 .3 .4 .2 .3 .4 .2 .3 .4 .2 .3 .4 .2 .3 .4 .15)
        :bwl '(.5 .2 5.5 .1 .2 5.5 .1 .32 5.5 .01 .2 5.5 .1 .2 5.5 .1 .2 5.5 .1 .2 .5 .5))
      )

;CRL
(setf my-crl (make-instance 'crl :the-list '(1.059 1.1 2.0 1.9)))


;ARL
(setf my-arl (make-instance 'arl :reference 100.0 :the-list '(1 2 3 4 5 6)))


;;;;;;   Internal accessors (usually not for the user)

;Useful variables
(setf my-vs my-spl)
(setf my-vs my-rpl)
(setf my-vs my-cil)
(setf my-vs my-ail)
(setf my-vs my-fql)
(setf my-vs my-crl)
(setf my-vs my-arl)
(setf vs-all '(my-spl my-rpl my-cil my-ail my-fql my-crl my-arl))

; the-list: VS (should use: get_vps)

; reference: VPS
; fql: VPS

; sp-list: SPL (list of symbolic pitches)

; amplitudes: SPECTRUM (should use: get-amp)
; bwl: SPECTRUM (should use: get-bw)

(reference my-ail)
(reference my-arl)

(the-list my-vs)
(sp-list my-spl)
(fql my-vs)


;;;;;; VPS: CLASS CONVERSIONS ;;;;;;;;;;;;;;;;;;;

;VS

(get_vps my-vs) ; return the list 
(mapcar (lambda (x) (get_vps (eval x))) vs-all)

;REMARK: When converting from a SPECTRUM to a CHORD, data reduction is active,
;             and the result can be approximated.
;          When performing data reduction, the AMPLITUDES of a FQL must exist.


;GET-SPL

(get-spl my-spl)
   ; return the same list of symbolic pitches
(get-spl my-rpl :octave 2)
   ; return the list starting on octave 2
(get-spl my-cil :reference 'LA4)
   ; return the list starting at A4
(get-spl my-ail)
   ; return the list of symbolic pitches
 
(get-spl my-fql)
  ; return the list of pitches
  ;   if there are more than *MAX-NN* (default = 20) pitches,
  ;      the amplitudes must also be specified for the data reduction
  ;      otherwise an error is produced
(get-spl my-fql :approx 50)
  ; return the list with the pitches approximated to �50 cents
(get-spl my-fql1 :max-nn 100)
(get-spl my-spectrum1 :max-nn 2)
  ; apply data reduction (--> requires amplitudes) only if the list has > 100 items
(get-spl my-spectrum1 :threshold 0.3)
  ; eliminate the frequencies whose amplitude is below 0.3 (absolute value)
  ;   here keep only 13 frequencies
  ; if the threshold is too high, return nil
;^^^^^ (get-spl my-spectrum1 :threshold -20)
  ; eliminate the frequencies whose amplitude is 20dB below the maximum amplitude
  ;   of the spectrum (NOT IMPLEMENTED YET!)
(get-spl my-spectrum :threshold 100 :approx t)
  ; combining keywords is possible. Approx = t means returning only the well-tempered
  ;    pitches without deviation

(get-spl my-crl :reference 100.0)
(get-spl my-crl :reference 100.0 :approx t)
  ; return the list of pitches starting at (fq->pch 100.0)

(get-spl my-arl)
(get-spl my-arl :approx t)
(get-spl my-arl :approx 50)
  ; return the list of pitches with possible approximation


;GET-RPL

(get-rpl my-spl)
   ; return the RPL of an SPL
(get-rpl my-rpl)
   ; return the same list
(get-rpl my-cil :reference 'LA4)
   ; return the RPL starting at A4
(get-rpl my-ail)
   ; return the RPL
 
(get-rpl my-fql)
  ; return the RPL
  ;   if there are more than *MAX-NN* (default = 20) pitches,
  ;      the amplitudes must also be specified for the data reduction
  ;      otherwise an error is produced
(get-rpl my-fql :approx 50)
  ; return the list with the pitches approximated to �50 cents
(get-rpl my-fql1 :max-nn 100)
(get-rpl my-spectrum1 :max-nn 2)
  ; apply data reduction (--> requires amplitudes) only if the list has > 100 items
(get-rpl my-spectrum1 :threshold 0.3)
  ; eliminate the frequencies whose amplitude is below 0.3 (absolute value)
  ;   here keep only 13 frequencies
  ; if the threshold is too high, return nil
(get-rpl my-spectrum :threshold 100 :approx t)
  ; combining keywords is possible. Approx = t means returning only the well-tempered
  ;    pitches without deviation

(get-rpl my-crl :reference 100.0)
(get-rpl my-crl :reference (pch->fq 'SOL2) :approx t)
  ; return the list of pitches starting at (fq->pch 100.0) or SOL2

(get-rpl my-arl)
(get-rpl my-arl :approx t)
(get-rpl my-arl :approx 50)
  ; return the list of pitches with possible approximation


;GET-CIL

(get-cil my-spl)
(get-cil my-spl :midi t)
   ; return the CIL of an SPL in intervallic or midi notation
(get-cil my-rpl)
   ; return the CIL of an RPL (no midi keyword available here)
(get-cil my-cil)
   ; return the same list
(get-cil my-ail)
   ; return the CIL
 
(get-cil my-fql)
  ; return the CIL
  ;   if there are more than *MAX-NN* (default = 20) pitches,
  ;      the amplitudes must also be specified for the data reduction
  ;      otherwise an error is produced
(get-cil my-fql :approx 50)
  ; return the list with the intervals approximated to �50 cents
(get-cil my-fql1 :max-nn 100)
(get-cil my-spectrum1 :max-nn 2)
  ; apply data reduction (--> requires amplitudes) only if the list has > 100 items
(get-cil my-spectrum1 :threshold 0.3)
  ; eliminate the frequencies whose amplitude is below 0.3 (absolute value)
  ;   here keep only 13 frequencies
  ; if the threshold is too high, return nil
(get-cil my-spectrum :threshold 100 :approx t)
  ; combining keywords is possible. Approx = t means returning only the well-tempered
  ;    intervals without deviation

(get-cil my-crl)
(get-cil my-crl :approx t)
  ; return the list of intervals

(get-cil my-arl)
(get-cil my-arl :approx t)
(get-cil my-arl :approx 50)
  ; return the list of intervals with possible approximation


;GET-AIL

(get-ail my-spl :reference 'DO3)
(get-ail my-spl :reference 'DO3 :midi t)
   ; return the AIL of an SPL starting from DO3 with possible midi notation
(get-ail my-rpl :octave 3 :reference 'LA4)
   ; return the AIL anchored to LA4 of a RPL starting at octave 3
(get-ail my-cil :reference 'RE2)
   ; return the AIL starting at RE2
   ; REMARK: the reference is for later usage, since the list of intervals
   ;            will always be the same and is the conversion of the CIL into an AIL
(get-ail my-ail :reference 'DO2)
   ; return a NEW list with a DIFFERENT ANCHOR point
 
(get-ail my-fql :reference 'DO2)
  ; return the AIL
  ;   if there are more than *MAX-NN* (default = 20) pitches,
  ;      the amplitudes must also be specified for the data reduction
  ;      otherwise an error is produced
(get-ail my-fql :approx 50 :reference 'DO2)
  ; return the list with the pitches approximated to �50 cents
(get-ail my-fql1 :max-nn 100 :reference 'DO2)
(get-ail my-spectrum1 :max-nn 2 :reference 'DO2)
  ; apply data reduction (--> requires amplitudes) only if the list has > 100 items
(get-ail my-spectrum1 :threshold 0.3 :reference 'DO2)
  ; eliminate the frequencies whose amplitude is below 0.3 (absolute value)
  ;   here keep only 13 frequencies
  ; if the threshold is too high, return nil
(get-ail my-spectrum :threshold 100 :approx t :reference 'DO2)
  ; combining keywords is possible. Approx = t (meaning returning only the well-tempered
  ;    pitches without deviation) is not active here.

(get-ail my-crl :reference 'LA2)
(get-ail my-crl :reference 'LA2 :approx t)
   ; REMARK: the reference is for later usage, since the list of intervals
   ;            will always be the same and is the conversion of the CRL into an AIL

(get-ail my-arl :reference 'LA4)
(get-ail my-arl :approx t :reference 'LA4)
(get-ail my-arl :approx 50 :reference 'LA4)
  ; return the AIL with possible approximation with the NEW reference LA4


;GET-FQL

  ; REMARK: the keyword "approx" is not effective when building FQL's
(get-fql my-spl)
   ; return the list of frequencies of an SPL
(get-fql my-rpl :octave 4)
   ; return the FQL starting with octave 4 (if missing, default = 0)
(get-fql my-cil :reference 'LA4)
   ; return the FQL starting at A4
(get-fql my-ail)
   ; return the FQL
 
(get-fql my-fql)
  ; return the FQL

(get-fql my-crl :reference 100.0)
(get-fql my-crl :reference (pch->fq 'SOL2))
  ; return the FQL starting at 100.0 or SOL2

(get-fql my-arl)
  ; return the list of pitches with possible approximation


;GET-CRL

(get-crl my-spl)
   ; return the CRL of an SPL
(get-crl my-rpl)
   ; return the CRL of an RPL
(get-crl my-cil)
   ; return CRL of a CIL (converts itvl into ratios)
(get-crl my-ail)
   ; return the CRL
 
(get-crl my-fql)
(get-crl my-spectrum1)

(get-crl my-crl)
  ; return the same list

(get-crl my-arl)
  ; return the CRL


;GET-ARL

(get-arl my-spl :reference 10.0)
(get-arl my-spl :reference 'SOL2)
   ; return the ARL of an SPL starting from 10 or SOL2
   ; reference can always be either a frequency or a pitch
(get-arl my-rpl :octave 3 :reference 'LA4)
   ; return the ARL anchored to LA4 of a RPL starting at octave 3
(get-arl my-cil :reference 'RE2)
   ; return the ARL starting at RE2
   ; REMARK: the reference is for later usage, since the list of intervals
   ;            will always be the same and is the conversion of the CIL into an AIL
(get-arl my-ail :reference 100)
   ; return a NEW list with a DIFFERENT ANCHOR point
 
(get-arl my-fql :reference 'DO2)

(get-arl my-crl :reference 'LA2)
  ; REMARK: the reference is for later usage, since the list of intervals
  ;            will always be the same and is the conversion of the CRL into an AIL
  ; When converting from a CIL or a CRL, the reference of get-ail is used also for
  ;    the CIL. Hence the corresponding AIL will always start from a unison.
  ; To avoid this, one should perform a double conversion, CIL->AIL with reference
  ;    on the first note and AIL->AIL with change of reference.
(print_vs (make-instance 'arl :the-list (get-arl my-cil :reference 'do2)
                          :reference 'do4))

(get-arl my-arl :reference 'LA4)
(get-arl my-arl :reference 10)
  ; return the ARL with a NEW reference



;;;;;; VPS: METHODS ;;;;;;;;;;;;;;;;;;;

(print_vs my-vs)
   ; Print the list and the name of the class
   ; The argument can either be an object of the super-class VS, or a list that
   ;    can be accepted by "make_vps" as a valid unique argument for building a VPS
(mapcar (lambda (x) (print_vs (eval x))) vs-all)

(number-of-notes my-vs)
   ; Return the list's number of notes
(mapcar #'(lambda (x) (number-of-notes (eval x))) vs-all)


;NOTE-LIST
;Default: SPL (the other representations are converted to SPL)

(note-list my-spl)
   ; return the list of pitches without octave
(note-list my-rpl)
(note-list my-cil :reference 'si4)
(note-list my-cil :reference 1000)
(note-list my-arl)
(note-list my-fql)
(note-list my-spectrum1)
  ; Automatic data reduction activated depending on the value of *MAX-NN* (default = 20)
(note-list my-crl :reference 'la4)
(note-list my-crl :reference 100)
(note-list my-arl)


;GET-GIL
;Default: SPL (the other representations are converted to SPL)

(get-gil my-spl)
(get-gil my-spl :midi t)
  ; Return the Global Interval List
(get-gil my-rpl)
(get-gil my-rpl :midi t)
(get-gil my-cil)
(get-gil my-cil :midi t)
(get-gil my-ail)
(get-gil my-ail :midi t)
(get-gil my-fql)
(get-gil my-fql :midi t)
(get-gil my-spectrum1)
(get-gil my-spectrum1 :midi t)
  ; Automatic data reduction activated depending on the value of *MAX-NN* (default = 20)
(get-gil my-crl)
(get-gil my-crl :midi t)
(get-gil my-arl)
(get-gil my-arl :midi t)


;GET-SURFACE
;Default: SPL (the other representations are converted to SPL)

(get-surface my-spl)
(get-surface my-spl :midi t)
(float (get-surface my-spl :midi t))
  ; Return the distance between the first and the last note
(get-surface my-rpl)
(get-surface my-rpl :midi t)
(get-surface my-cil)
(get-surface my-cil :midi t)
(get-surface my-ail)
(get-surface my-ail :midi t)
(get-surface my-fql)
(get-surface my-fql :midi t)
(get-surface my-spectrum1)
(get-surface my-spectrum1 :midi t)
(get-surface my-crl)
(get-surface my-crl :midi t)
(get-surface my-arl)
(get-surface my-arl :midi t)


;GET-DENSITY
;Default: SPL (the other representations are converted to SPL)

(get-density my-spl)
(float (get-density my-spl))
  ; Return the density of the SPL [number-of-notes / (surface + 1)]
(get-density my-spl)
(get-density my-rpl)
(get-density my-cil)
(get-density my-ail)
(get-density my-fql)
(get-density my-spectrum1)
(get-density my-crl)
(get-density my-arl)


;GET-HOMOGENEITY
;Default: SPL (the other representations are converted to SPL)

(get-homogeneity my-spl)
  ; Return the coefficient of homogeneity as a single symbolic interval
(get-homogeneity my-spl :midi t)
(float (get-homogeneity my-spl :midi t))
  ; Return the coefficient of homogeneity as a single midi interval
(get-homogeneity my-spl :expanded t)
  ; Return the coefficient of homogeneity as a list of two symbolic intervals
  ;    (maximum and minimum interval found in the SPL)
(get-homogeneity my-spl :expanded t :midi t)
  ; Return the coefficient of homogeneity as a list of two midi intervals
  ;    (maximum and minimum interval found in the SPL)
(get-homogeneity my-rpl)
(get-homogeneity my-cil)
(get-homogeneity my-ail)
(get-homogeneity my-fql)
(get-homogeneity my-spectrum1)
(get-homogeneity my-crl)
(get-homogeneity my-arl)


;GET-SD
;Default: SPL (the other representations are converted to SPL)

(get-sd my-spl)
  ; Return the Standard Deviation of the differences between adjacent intervals
  ; This works only if the SPL has at least 4 values.
  ; Otherwise the value UNCALCULABLE is returned.
(get-sd my-spl)
(get-sd my-rpl)
(get-sd my-cil)
(get-sd my-ail)
(get-sd my-fql)
(get-sd my-spectrum1)
(get-sd my-crl)
(get-sd my-arl)


;GET-CS
;Default: SPL (the other representations are converted to SPL)

(get-cs my-spl)
  ; Return the Coefficient of Stability of an SPL
(get-cs my-rpl)
(get-cs my-cil)
(get-cs my-ail)
(get-cs my-fql)
(get-cs my-spectrum1)
(get-cs my-crl)
(get-cs my-arl)


;GET-HARMONICITY
;Default: FQL (the other representations are converted to SPL)

(get-harmonicity my-spl)
(get-harmonicity my-spl :f0 10.0)
  ; Return the harmonicity (average of the harmonic distances) of an SPL
  ;    with respect to a f0 (default = first frequency of the list) (see Esquisse)
(get-harmonicity my-spl :expanded t)
(get-harmonicity my-spl :expanded t :f0 10.0)
  ; Return a list containing the harmonic distance of each frequency of the SPL
  ;    with respect to a f0 (default = first frequency of the list) (from Esquisse)

(get-harmonicity my-rpl :octave 2)
(get-harmonicity my-rpl :octave 2 :f0 50)
(get-harmonicity my-rpl :octave 2 :expanded t)
(get-harmonicity my-rpl :octave 2 :expanded t :f0 50)

(get-harmonicity my-cil :reference 'la2)
(get-harmonicity my-cil :reference 'la2 :f0 50)
(get-harmonicity my-cil :reference 'la2 :expanded t)
(get-harmonicity my-cil :reference 'la2 :expanded t :f0 50)

(get-harmonicity my-ail)
(get-harmonicity my-ail :f0 5)
(get-harmonicity my-ail :expanded t)
(get-harmonicity my-ail :expanded t :f0 5)

(get-harmonicity my-fql)
(get-harmonicity my-fql :f0 5)
(get-harmonicity my-fql :expanded t)
(get-harmonicity my-fql :expanded t :f0 5)

(float (get-harmonicity my-spectrum1))
(float (get-harmonicity my-spectrum1 :f0 100))
(get-harmonicity my-spectrum1 :expanded t)
(get-harmonicity my-spectrum1 :expanded t :f0 10)

(get-harmonicity my-crl :reference 50)
(get-harmonicity my-crl :reference 50 :f0 10)
(get-harmonicity my-crl :reference 50 :expanded t)
(get-harmonicity my-crl :reference 50 :f0 10 :expanded t)

(get-harmonicity my-arl)
(get-harmonicity my-arl :f0 25)
(get-harmonicity my-arl :expanded t)
(get-harmonicity my-arl :f0 30 :expanded t)


;GET-VIRT-FUND
;Default: FQL (the other representations are converted to SPL)

(get-virt-fund my-spl)
(get-virt-fund my-spl :grid-ratio 0.01)
  ; Return the virtual fundamental of an SPL according to a certain grid-ratio
  ;    (default = 0.001) (see Esquisse)

(get-virt-fund my-rpl :octave 2 :grid-ratio 0.01)
(get-virt-fund my-cil :reference 'la2 :grid-ratio 0.01)
(get-virt-fund my-ail :grid-ratio 0.01)
(get-virt-fund my-fql)
(get-virt-fund my-spectrum1 :grid-ratio 0.01)
(get-virt-fund my-crl :reference 50 :grid-ratio 0.01)
(get-virt-fund my-arl :grid-ratio 0.01)


;GET-MAX/MIN-FQ
;Default: FQL (the other representations are converted to SPL)

(get-max-fq my-spl)
(get-min-fq my-spl)
  ; Return the max or min frequency of an SPL

(get-max-fq my-rpl :octave 2)
(get-min-fq my-rpl :octave 2)

(get-max-fq my-cil :reference 'LA4)
(get-min-fq my-cil :reference 'LA4)

(get-max-fq my-ail)
(get-min-fq my-ail)

(get-max-fq my-fql)
(get-min-fq my-fql)

(get-max-fq my-crl :reference 100.0)
(get-min-fq my-crl :reference 100.0)

(get-max-fq my-arl)
(get-min-fq my-arl)


;;;;;;;; SPECTRA ONLY
;        => FQL with AMPLITUDES and BANDWIDTHS, otherwise nil

;GET-MAX/MIN-AMP/BW

(get-max-amp my-fql)
(get-min-amp my-fql)
(get-max-bw my-fql)
(get-min-bw my-fql)
  ; Return nil if the amplitudes are not initialized
(get-max-amp my-spectrum1)
(get-min-amp my-spectrum1)
(get-max-bw my-spectrum1)
(get-min-bw my-spectrum1)

(get-min-bw my-spl)


;;;;;;;; MIXED METHODS

;PRINT

(print_vs my-spl)
(print-spl my-spl)
   ; print an SPL only

;PREDICATES/INFO

(class-of my-rpl)
(typep my-rpl 'rpl)
   ; Return the whole class hierarchy

(unisons-p (get-spl my-spl))
   ; nil if there is a unison in the list, i.e. if two adjacent values are "equal"
   
(interval-p '6+)
   ; Tell if the input is a symbolic interval. In vicious cases it may fail.
   
(pitch-with-octave-p 'DO3)
(pitch-with-octave-p '("DO3" . 12))
   ; Tell if the input is a symbolic pitch with octave number.
   ;    In vicious cases it may fail.
   
(pitch-without-octave-p 'DO)
(pitch-without-octave-p '("DO" . 12))
   ; Tell if the input is a symbolic pitch without octave number.
   ;    In vicious cases it may fail.
   

;MEMBER-TIMES

; Returns how many times <item> has positively satisfied <test>  in <list>.
;   Uses the function "member"
;     (member-times 3 '(1 2 3 4 5 6 3 4) '=) -> 2
;     (member-times 3 '(1 2 3 4 5 6 3 4) '<) -> 4
;     (member-times 3 '(1 2 3 4 5 6 3 4) '>) -> 2


;FLOAT-SEMITONES

; Return the floating point value of a list of symbolic intervals
;    Uses "float" to coerce the fractional representation
(setf itvl-list '(6+ 3- (3- 1 23) (4- 2)))
(float-semitones itvl-list)

;______________________________________________
;;;;;;;;;;;;;;                                                            ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;                          UFM                          ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;  VPS USER-FRIENDLY METHODS ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;______________________________;;;;;;;;;;;;;;

;;;;;;;;;;; UFM: VPS UNIVERSAL INSTANCIATOR ;;;;;;;;;;

;;;;;;;;;;;;;; Automatic selection of the class
;;;;;;;;;;;;;;   as a function of the signature for make_vps

;3 args : AIL (car the-list) = interval
;         ARL (car the-list) = float
;2 args : SPL (car the-list) = pitch with octave
;         RPL (car the-list) = pitch without octave
;         PPL (car the-list) = pitch without octave
;             (car (last the-list)) = pitch without octave
;         CIL (car the-list) = interval symbol
;         FQL (car the-list) = float
;             (max the-list) > MINFQ
;         CRL (car the-list) = float
;             (max the-list) < MINFQ

;REMARK: fql's initialized as spectra with AMP's and BW must be done by calling
;           make-instance directly. No shortcut is provided for this special case.

;(make_vps list [reference when needed])
(make_vps '(DO2 RE3 MI4 (LA4 . 12)))
(make_vps '(DO RE1 MI2 (LA2 . 12)))
(make_vps '(DO RE MI (LA . 12)))
(make_vps '(6+ (2- 0 12) (3- 1)))
(make_vps '(-6+ (2- 0 12) (3- 1)) 'A4)

(make_vps '(100 200 300))
(make_vps '(1.1 1.059 1.23))
(make_vps '(1 2 3 4 5.5 6.6) 100)


;;;;;;;;;; UFM: TYPE CONVERSIONS ;;;;;;;;;;;;;;;;;;;;;;

;SPL_VPS
; (spl_vps vps [reference when needed] [approximation])
;  Return the Symbolic Pitch List of a VPS

; if approximation = number -> number indicates the deviation in cents
;     Ex: 50 = quarter tones, 33 = sixths of tones, etc.
; if approximation = t -> simple pitch list (without deviation)

; Approximation is always the last argument and optional

;   SPL: (spl_vps spl) -> return the same SPL
;        (spl_vps spl approx) -> return the SPL with a new approximation
;   RPL: (spl_vps rpl octave)
;        (spl_vps rpl octave approx)
;   CIL: (spl_vps cil reference)
;        (spl_vps cil reference approx)
;   AIL: (spl_vps ail)
;        (spl_vps ail approx)
;   FQL: (spl_vps fql choice)
;                        "-> if positive integer = max-nn
;                        "-> if negative float = threshold (dB) (NOT YET)
;                        "-> if positive float = threshold
;        When max-nn is selected, if data reduction is needed and there
;           are no amplitudes, data reduction is disactivated
;        (spl_vps fql choice approx)
;   CRL: (spl_vps crl reference)
;        (spl_vps crl reference approx)
;   ARL: (spl_vps arl)
;        (spl_vps arl approx)


;RPL_VPS
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
;                        "-> if positive integer = max-nn
;                        "-> if negative float = theshold (dB) (NOT YET)
;                        "-> if positive float = threshold
;        (rpl_vps fql choice approx)
;   CRL: (rpl_vps crl reference)
;        (rpl_vps crl reference approx)
;   ARL: (rpl_vps arl)
;        (rpl_vps arl approx)


;CIL_VPS / CILS_VPS
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
;                        "-> if positive integer = max-nn
;                        "-> if negative float = theshold (dB) (NOT YET)
;                        "-> if positive float = threshold
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


;AIL_VPS
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


;FQL_VPS
; (fql_vps vps [reference when needed])

;   SPL: (fql_vps spl)
;   RPL: (fql_vps rpl octave)
;   CIL: (fql_vps cil reference)
;   AIL: (fql_vps ail)
;   FQL: (fql_vps fql)
;   CRL: (fql_vps crl reference)
;   ARL: (fql_vps arl)


;CRL_VPS
; (crl_vps vps)
;  Return the Contiguous Ratio List of a VPS


;ARL_VPS
; (arl_vps vps reference [octave when needed])
;  Return the Anchored Ratio List of a VPS

;   SPL: (arl_vps spl)
;   RPL: (arl_vps rpl octave)
;   CIL: (arl_vps cil)
;   AIL: (arl_vps ail)
;   FQL: (arl_vps fql)
;   CRL: (arl_vps crl)
;   ARL: (arl_vps arl)


;;;;;;;;;; UFM: SELECTORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;VPS_VPS
; (vps_vps vps)
;  Return the internal list of a VPS

(vps_vps my-spl)
(vps_vps my-rpl)
(vps_vps my-cil)
(vps_vps my-ail)
(vps_vps my-fql)
(vps_vps my-crl)
(vps_vps my-arl)


;NN_VPS
; (nn_vps vps)
;  Return the Number of Notes of a VPS

(nn_vps my-spl)
(nn_vps my-rpl)
(nn_vps my-cil)
(nn_vps my-ail)
(nn_vps my-fql)
(nn_vps my-crl)
(nn_vps my-arl)


;GIL_VPS
;GIL-S_VPS
; (gil_vps vps)
; (gils_vps vps)
;  Return the Global Interval List of a VPS as an interval or in semitones

(gil_vps my-spl)
(gil_vps my-rpl)
(gil_vps my-cil)
(gil_vps my-ail)
(gil_vps my-fql)
(gil_vps my-crl)
(gil_vps my-arl)

(gils_vps my-spl)
(gils_vps my-rpl)
(gils_vps my-cil)
(gils_vps my-ail)
(gils_vps my-fql)
(gils_vps my-crl)
(gils_vps my-arl)


;SURF_VPS
;SURF-S_VPS
; (surf_vps vps)
; (surf-s_vps vps)
;  Return the Surface of a VPS as an interval or in semitones

(surf_vps my-spl)
(surf_vps my-rpl)
(surf_vps my-cil)
(surf_vps my-ail)
(surf_vps my-fql)
(surf_vps my-crl)
(surf_vps my-arl)

(surf-s_vps my-spl)
(surf-s_vps my-rpl)
(surf-s_vps my-cil)
(surf-s_vps my-ail)
(surf-s_vps my-fql)
(surf-s_vps my-crl)
(surf-s_vps my-arl)


;DENS_VPS
; (dens_vps vps)
;  Return the Density of a VPS
;  A density of 1.0 = a cluster of semitones, of 2.0 = 1/4 tones,...

(dens_vps my-spl)
(dens_vps my-rpl)
(dens_vps my-cil)
(dens_vps my-ail)
(dens_vps my-fql)
(dens_vps my-crl)
(dens_vps my-arl)


;HOM_VPS
;HOM-S_VPS
;HOM-E_VPS
;HOM-ES_VPS
; (hom_vps vps)
; (hom-s_vps vps)
; (hom-e_vps vps)
; (hom-es_vps vps)
;  Return the Coefficient of Homogeneity of a VPS as
;      hom: a single symbolic interval
;      hom-s: a single interval in semitones
;      hom-e: a list of of two symbolic intervals
;      hom-es: a list of two intervals in semitones

(hom_vps my-spl)
(hom_vps my-rpl)
(hom_vps my-cil)
(hom_vps my-ail)
(hom_vps my-fql)
(hom_vps my-crl)
(hom_vps my-arl)

(hom-s_vps my-spl)
(hom-s_vps my-rpl)
(hom-s_vps my-cil)
(hom-s_vps my-ail)
(hom-s_vps my-fql)
(hom-s_vps my-crl)
(hom-s_vps my-arl)

(hom-e_vps my-spl)
(hom-e_vps my-rpl)
(hom-e_vps my-cil)
(hom-e_vps my-ail)
(hom-e_vps my-fql)
(hom-e_vps my-crl)
(hom-e_vps my-arl)

(hom-es_vps my-spl)
(hom-es_vps my-rpl)
(hom-es_vps my-cil)
(hom-es_vps my-ail)
(hom-es_vps my-fql)
(hom-es_vps my-crl)
(hom-es_vps my-arl)


;SD_VPS
; (sd_vps vps)
;  Return the Standard Deviation of a VPS (minimum 4 items)

(sd_vps my-spl)
(sd_vps my-rpl)
(sd_vps my-cil)
(sd_vps my-ail)
(sd_vps my-fql)
(sd_vps my-crl)
(sd_vps my-arl)


;CS_VPS
; (cs_vps vps [S-Space])
;  Return the Coefficient of Stability of a VPS
;  Take the values of the constant *STABILITY-SPACE* if second argument is nil

(cs_vps my-spl)
(cs_vps my-rpl)
(cs_vps my-cil)
(cs_vps my-ail)
(cs_vps my-fql)
(cs_vps my-crl)
(cs_vps my-arl)


;HARM_VPS
;HARM-E_VPS
; (harm_vps vps [f0])
;  Return the Coefficient of Harmonicity of a VPS
;  If f0 is missing, take the first if FQL or the one on ARL
;  Does not work for RPL, CIL and CRL which must be converted into another VPS

(harm_vps my-spl)
(harm_vps my-ail)
(harm_vps my-fql)
(harm_vps my-arl)

(harm-e_vps my-spl)
(harm-e_vps my-ail)
(harm-e_vps my-fql)
(harm-e_vps my-arl)


;VF0_VPS
; (vf0_vps vps [grid])
;  Return the Virtual Fundamental of a VPS
;  Does not work for RPL, CIL and CRL which must be converted into another VPS

(vf0_vps my-spl)
(vf0_vps my-ail)
(vf0_vps my-fql)
(vf0_vps my-arl)

(vf0_vps my-spl 0.01)
(vf0_vps my-ail 0.01)
(vf0_vps my-fql 0.01)
(vf0_vps my-arl 0.01)


;MAX-FQ_VPS
;MIN-FQ_VPS
; (max-fq_vps vps)
; (min-fq_vps vps)

;  Return the Maximum/Minimum Frequency of a VPS
;  Does not work for RPL, CIL and CRL which must be converted into another VPS

(max-fq_vps my-spl)
(max-fq_vps my-ail)
(max-fq_vps my-fql)
(max-fq_vps my-arl)

(min-fq_vps my-spl)
(min-fq_vps my-ail)
(min-fq_vps my-fql)
(min-fq_vps my-arl)


;MAX-AMP_VPS
;MIN-AMP_VPS
;MAX-BW_VPS
;MIN-BW_VPS
; (max-fq_vps vps)
; (min-fq_vps vps)
;  Return the Maximum/Minimum Frequency of a VPS
;  Does not work for RPL, CIL and CRL which must be converted into another VPS

(max-amp_vps my-spl)
(max-amp_vps my-rpl)
(max-amp_vps my-cil)
(max-amp_vps my-ail)
(max-amp_vps my-spectrum)
(max-amp_vps my-crl)
(max-amp_vps my-arl)

(min-amp_vps my-spl)
(min-amp_vps my-rpl)
(min-amp_vps my-cil)
(min-amp_vps my-ail)
(min-amp_vps my-spectrum)
(min-amp_vps my-crl)
(min-amp_vps my-arl)

(max-bw_vps my-spl)
(max-bw_vps my-rpl)
(max-bw_vps my-cil)
(max-bw_vps my-ail)
(max-bw_vps my-spectrum)
(max-bw_vps my-crl)
(max-bw_vps my-arl)

(min-bw_vps my-spl)
(min-bw_vps my-rpl)
(min-bw_vps my-cil)
(min-bw_vps my-ail)
(min-bw_vps my-spectrum)
(min-bw_vps my-crl)
(min-bw_vps my-arl)


;IS_VPS, etc.
(is_vps my-arl)
(is_vps 'my-arl)


;TYPE-OF_VPS
; (type-of_vps list [reference])
;  Return the list of pitches in a pitch-class format (see "Set Theory")
;  VPS = must a chromatic SPL
;        if not, try to convert, else error
;        if not chromatic (with deviation), the deviation will not be taken
;           into account

(type-of_vps '(DO3 RE3 MI3))


;;;;;;;;;; UFM: UTILITIES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SPECIAL INSTANCIATIONS FOR TRANSFORMATION,
;;;      MATCHING AND DATA BASES ;;;

;Messiaen: resonance chord
(setf v1 (make_vps '(DO3 SOL3 MI4 SIb4 RE5 FAd5)))

; Stockhausen: Klavierst�ck IX
(setf v2 (make_vps '(DOd3 FAd3 SOL3 DO4)))

;Stroppa: Traiettoria 1
(setf v3 (make_vps '(DO4 LAb4 RE5 SOL5 DOd6)))

;Stroppa: Traiettoria 2
(setf v4 (make_vps '(RE3 DOd4 LA4 MIb5 LAb5 DO6 MI6 FAd6)))

;Wagner: Tristan
(setf v5 (make_vps '(FA3 SI3 REd4 SOLd4)))

;Messiaen: Quatuor
(setf v6 (make_vps '(FA2 SOL2 SIb2 DO3 MIb3 SI3 MI4)))

;Messiaen: Superposition of IYth's
(setf v7 (make_vps '(REb3 SOL3 DO4 FAd4 SI4 FA5)))

;Messiaen: Pi�ce pour orgue
(setf v8 (make_vps '(D0d1 DOd2 SI2 RE3 FA3 SOL3 SI3 DOd4 REd4 FAd4
                         LA4 DOd5)))
(setf v8a (make_vps '(DOD2 DOd3 SI3 RE4 FA4 SOL4 SI4 DOd5 REd5 FAd5
                         LA5 DOd6)))

;Dominant 9th
(setf v9 (make_vps '(DO4 MI4 SOL4 SIb4 RE5)))

(setf allv (list v1 v2 v3 v4 v5 v6 v7 v8 v9))


;;;;;; TRANSFORMATIONS AND MODIFICATIONS ;;;;;;;;;;;;;

;TRANSPOSE
; (transpose vps val)
; Transpose an SPL or a FQL. If val is a number, it will be considered as a ratio,
;    otherwise it will be considered as an interval.
; If the VPS is a FQL with amplitudes and bandwidths, they will be copied in the
;    transposed object.
; Return: a new SPL or FQL.
; REMARK: the intervals of perfect IV and V are to written as (4 0) and (5 0),
;            otherwise the system will take the argument for a ratio.

(print_vs (transpose v9 '(2+ 0 12)))
(print_vs (transpose v9 '2+))
(print_vs (transpose v9 '(4+ 2)))

(print_vs (transpose v9 0.5))
(print_vs (transpose v9 2.0))
(print_vs (transpose v9 4.0))

(print_vs (transpose my-spectrum 10.0))
(print_vs (transpose my-spectrum 0.5))
(print_vs (transpose my-spectrum 2.0))

(print_vs (transpose my-spectrum '(2+ 0 12)))
(print_vs (transpose my-spectrum '(5 0)))
(print_vs (transpose my-spectrum '(4+ 2)))

(spl_vps (transpose (make_vps '("DO4" "MI4" "SOL4")) '2+))
                           ; to see a simplified representation (without deviation)
(spl_vps (transpose (make_vps '("DO4" "MI4" "SOL4")) '2+) t)


;XPOSE_VPS
; (xpose_vps vps starting_pitch)

; VPS = a single SPL or a tree of SPL's

; Transpose an SPL so that it begins with "starting_pitch".
; Return: a new SPL.

(print_vs (xpose_vps v9 "RE4"))
(spl_vps (xpose_vps v9 "RE4") t) ; to avoid seeing pitches with deviations
(print_vs (xpose_vps v9 '(RE4 . 55)))

(spl_vps (xpose_vps (make_vps '(("DO4" . 12) "MI4" ("SOL4" . 88))) "FAd3"))
(spl_vps (xpose_vps (make_vps '(("DO4" . 12) "MI4" ("SOL4" . 88))) '("FAd3" . 33)))
(spl_vps (xpose_vps (make_vps '("DO4" "MI4" "SOL4")) "FAd3"))
(spl_vps (xpose_vps (make_vps '("DO4" "MI4" "SOL4")) "FAd3") t)


;XPOSE-END_VPS
; (xpose-end_vps vps ending_pitch)

; VPS = a single SPL or a tree of SPL's

; Transpose an SPL so that it ends with "starting_pitch".
; Return: a new SPL.

(print_vs (xpose_vps v9 "RE4"))
(spl_vps (xpose_vps v9 "RE4") t) ; to avoid seeing pitches with deviations
(print_vs (xpose_vps v9 '(RE4 . 55)))

(spl_vps (xpose-end_vps (make_vps '(("DO4" . 12) "MI4" ("SOL4" . 88))) "FAd3"))
(spl_vps (xpose-end_vps (make_vps '(("DO4" . 12) "MI4"
                                    ("SOL4" . 88))) '("FAd3" . 33)))
(spl_vps (xpose-end_vps (make_vps '("DO4" "MI4" "SOL4")) "FAd3"))
(spl_vps (xpose-end_vps (make_vps '("DO4" "MI4" "SOL4")) "FAd3") t)


;REVERT
; (revert spl [starting-pitch] [:end t])
; Return a SPL with the same pitch classes read backwards (produce the
;    complementary intervals) and starting at the same octave or on a given
;    pitch.
; If :end is t, the result will finish on the starting-pitch.
; Ex: (DO3 MI3 SOL4 SIb3 RE4)  ->  (RE3 SIb3 SOL4 MI5 DO6)

(print_vs (revert v9))
(print_vs (revert v9 "DO3"))
(print_vs (revert v9 'A4))

(print_vs (revert v9 "DO3" :end t))
(print_vs (revert v9 'A4 :end t))

(print_vs (revert v9 (first (get_vps v9)) :end t))


;MIRROR
; (mirror spl [starting pitch])
; Return the inversion of an SPL starting on the same pitch or on a given pitch
; If :end is t, the result will finish on the starting-pitch.
; Symmetric VPS's will yield the same result!

(print_vs (mirror v3))
(print_vs (mirror v3 'LA4))
(print_vs (mirror v3 (car (last (the-list v3)))))
(print_vs (mirror v3 (car (last (the-list v3))) :end t))


;MERGE_VPS
; (merge_vps vps1 vps2 [threshold])
; SPL: Merges two SPL's removing unisons and the pitches that fall within the
;         threshold [cents] (default = 1)
; FQL: Merges two FQL's removing the frequencies that fall within the threshold
;         (default = 1.0001). The threshold here is computed by taking the ratio
;         between the new item and the previous one, which is less big. It is
;         therefore always > 1.0 (1.0 = unison). A ratio of 1.059 = semitone.
;      If the first VPS has no ampitudes, merge only the frequencies, independently
;         on the contents of the other one. If it has only the amplitudes, merge
;         frequencies and amplitudes only, otherwise merge frequencies, amplitudes
;         and bandwidths.

(print_vs (merge_vps v3 v4))
(print_vs (merge_vps v3 v9))

; eliminate minor seconds
(print_vs (merge_vps v3 v9 200))

(print_vs (merge_vps my-spectrum my-spectrum1))
(print_vs (merge_vps my-spectrum my-spectrum1 1.01))

(print_vs (merge_vps my-fql my-fql1))
(print_vs (merge_vps my-spectrum my-spectrum1 (itvl->ratio '2-)))

; eliminate minor seconds
(print_vs (merge_vps v3 v9 200))


;;;;;; PREDICATES ;;;;;;;;;;;;;

;OCTAVE-P_VPS
; (octave-p_vps vps [threshold])
; Returns t if an octave exists in the VPS even if separated by many octaves.
; The test checks that the octave falls within the threshold (default = 5 cent),
;    including the value of the threshold.

(octave-p_vps my-spl)
(octave-p_vps my-spl 0.2)


;ITVL-MOD-P_VPS
; (itvl-mod-p_vps vps itvl [threshold])
; Returns t if the interval "itvl" exists in the VPS even if separated by many
;    octaves.
; The test checks that the "itvl" falls within the threshold (default = 5 cent),
;    including the value of the threshold.
; REMARK: to avoid bizzarre behaviors, the intervals should be notated in one of
;            the following ways: '6+, '(6+ 0) or '(6+ 0 5) or in semitones.
; The testing interval should be inferior to one octave.

(itvl-mod-p_vps my-spl '(6+ 0))
(itvl-mod-p_vps my-spl '(4+ 0) 0.04)
(itvl-mod-p_vps my-spl '(4+ 0 5))
(itvl-mod-p_vps my-spl '(4+ 0 6) 0.0)


;ITVL-P_VPS
; (itvl-mod-p_vps vps itvl-list [threshold])
; Returns t if the at least one of the absolute intervals contained in itvl-list
;     exists in the VPS. This test does not look for intervals across octaves.
; The test checks that the "itvl" falls within the threshold (default = 5 cent),
;    including the value of the threshold.
; The testing interval can be any interval in intervallic notation or in semitones.

(itvl-p_vps my-spl '((6+ 0)))
(itvl-p_vps my-spl '((6+ 0)) 0.5)
(itvl-p_vps my-spl '((4+ 0)))
(itvl-p_vps my-spl '(6.05))
(itvl-p_vps my-spl '(6.02))
(itvl-p_vps my-spl '(6.02) 0.01)
(itvl-p_vps my-spl '(4+ 0 5) 0.0)


;ITVL-CIL-P_VPS
; (itvl-cil-p_vps vps itvl [threshold])
; Returns t if the interval "itvl" exists in the VPS's CIL.
; The test checks that the "itvl" falls within the threshold (default = 5 cent),
;    including the value of the threshold.
; REMARK: to avoid bizzarre behaviors, the intervals should be notated in one of
;            the following ways: '6+, '(6+ 0) or '(6+ 0 5) or in semitones.

(itvl-cil-p_vps my-spl '(6+ 0))
(itvl-cil-p_vps my-spl '(4+ 0))
(itvl-cil-p_vps my-spl '(4+ 0 5))
(itvl-cil-p_vps my-spl '(4+ 0 15))
(itvl-cil-p_vps my-spl '(4+ 0 15) 0.1)
(itvl-cil-p_vps my-spl 5.3)
(itvl-cil-p_vps my-spl 5.3 0.21)


;;;;;;;;;;;;;;;;;;;; MIXED ;;;;;;;;;;;;;;;;;;;;

;REMOVE-OCTAVES
; (remove-octaves spl [:tolerance :from-bottom])
; Returns another SPL without octaves. The test for the octave will take into
;    account a precision � tolerance [cents, default = 1]. It "from-bottom" is
;    true, the low octave will be kept, otherwise it will be cut (default = nil).
;EX:
(setf spl3 (make_vps '(DO3 MI3 SIB3 RE4 MI4 FAd4 SI4 SOLb6)))
(the-list (remove-octaves spl3)) ;-> (DO3 SIB3 RE4 MI4 SI4 SOLb6)
(the-list (remove-octaves spl3 :from-bottom t)) ;-> (DO3 MI3 SIB3 RE4 FAd4 SI4)


;STRETCH_VPS
; (stretch_vps vps [:reference :offset :stretching :random])
; Returns another SPL with stretching.
; Meaning: Reference: reference where to start to strech [Hz or Pitch]
;                        (default = first item of the list)
;           Offset: offset linearly added to each partial of the VPS [% of F0]
;                   (default = 0). An offset of 0.059 means a shift of a semitone.
;           Stretching: Stretching/Compressing factor (default = 2).
;                       [<1.0 = descending frequencies with respect to F0
;                         1.0 = all the frequencies = F0
;                         1.0/2.0 = compressed frequencies
;                        >2.0 = stretched frequencies]
;           Random: random variation added to the computed frequency [0-1]
;                       (default = 0). Attention: a high value for random might
;                       generate frequencies that are no longer in order

;REMARK: the keyword ":reference" is valid only for SPL's.

(setf spl1 (make_vps '(SOL1 SOL2 RE3 MI3 SI3 LA4 MIb5)))
(setf fql1 (make_vps '(100 200 300 400 500 600 700 800 900 1000)))
(setf arl1 (make_vps '(1 2 3 4 5 6 7 8 9 10) 100))

(print_vs (stretch_vps spl1 :reference 'SOL1 :offset 0.059))
(print_vs (stretch_vps spl1 :reference 10 :offset 0.0 :stretching 2.2))
(print_vs (stretch_vps spl1 :reference 50 :offset 0.0 :stretching 2.2))
(print_vs (stretch_vps spl1 :offset 0.0 :stretching 2.2))

(print_vs (stretch_vps fql1 :offset 0.059))
(print_vs (stretch_vps fql1 :offset 0.0 :stretching 2.2))
(print_vs (stretch_vps fql1 :offset 0.0 :stretching 2.2 :random 0.1))
(print_vs (stretch_vps fql1 :offset 0.0 :offset 1.5 :stretching 2.2 :random 0.1))


;SPSHT_VPS
; (spsht_vps F0 NP SH ST [random])
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

(print_vs (spsht_vps 100 '(1 2 3 4 5) 0.0 2.01))
(print_vs (spsht_vps 'SOL2 '(1 2 3 4 5) 0.059 2.01))
(print_vs (spsht_vps 100 20 0.0 2.1))
(print_vs (spsht_vps 100 20 0.0 2.1 0.01))


;MAIN-PARTIALS
; (main-partials spectrum :diapason :max-nn / :threshold)
; Reduce the frequencies of a FQL depending on certain criteria.
;    :max-nn: keep a maximum number of frequencies (default = *MAX-NN*)
;    :threshold: keep only the frequencies => threshold [abs amp]
;                   (defaul = nil -> use max-nn)
;    :diapason: reference diapason when using fq->pch (default = nil)
;REMARK: this only works when the ampitudes are specified

(setf fql1 (make-instance
             'fql
             :the-list    '(10 20 30 40 50 60 70 80 90 91 92 93 94 95 96 97 98 99)
             :amplitudes '( 1  2  1  2  3  4  3  2  1  2  3  4  5  6  5  4  5  6)
             ))
(print_vs (main-partials fql1 :max-nn 5))
(print_vs (main-partials fql1 :threshold 4))
(print_vs (main-partials fql1 :threshold 4 :diapason 442.0))


;;;;;;;;;;;;;;;;;;;; MATCHING ;;;;;;;;;;;;;;;;;;;;

;MATCH
; (match SPL1 SPL2 :tolerance :step)
; Match SPL1 against SPL2, i.e. transpose SPL1 by "step" over SPL2 (fixed)
;    :tolerance [cents, default=50] = tolerance accepted when performing the match
;    :step [cents, default=100] = step of transposition of SPL2

; Format of the result: list of items preceded by the number of matched notes
#|
  v-> number of matched notes
((3
  
     v-> transposed SPL1 that matches the 3 pitches below of SPL2
  (#<SPL #x499FE4E> (("MI4" . 0) ("SOLd4" . 0) ("RE5" . 0)))
  (#<SPL #x497F5EE> (("MI4" . 0) ("SOLd4" . 0) ("DO5" . 0))))

 (2
  (#<SPL #x49AC2CE> (("SOLd4" . 0) ("DO5" . 0)))
  (#<SPL #x4999746> (("FAd4" . 0) ("DO5" . 0)))
  (#<SPL #x49932F6> (("MI4" . 0) ("RE5" . 0)))
  (#<SPL #x498C8DE> (("SOLd4" . 0) ("DO5" . 0)))
  (#<SPL #x4985F06> (("FAd4" . 0) ("RE5" . 0)))
  (#<SPL #x4971CBE> (("MI4" . 0) ("SOLd4" . 0)))
  (#<SPL #x496AE36> (("FAd4" . 0) ("SI4" . 0))))

 (1
  (#<SPL #x49BF6D6> (("RE5" . 0))) 
  (#<SPL #x49B8D76> (("DO5" . 0))) 
  (#<SPL #x49B5B26> (("SI4" . 0))) 
  (#<SPL #x49B2866> (("RE5" . 0))) 
  (#<SPL #x49A902E> (("SI4" . 0))) 
  (#<SPL #x49A5E5E> (("FAd4" . 0))) 
  (#<SPL #x4996326> (("SI4" . 0))) 
  (#<SPL #x49892A6> (("SI4" . 0))) 
  (#<SPL #x497C05E> (("SI4" . 0))) 
  (#<SPL #x4978956> (("FAd4" . 0))) 
  (#<SPL #x4975186> (("RE5" . 0))) 
  (#<SPL #x496E4CE> (("DO5" . 0))) 
  (#<SPL #x4963EC6> (("MI4" . 0))) 
  (#<SPL #x49605CE> (("SOLd4" . 0))) 
  (#<SPL #x49594EE> (("FAd4" . 0))) 
  (#<SPL #x4952536> (("MI4" . 0)))) 

 (0
  (#<SPL #x49BBEF6> NIL)
  (#<SPL #x49AEF8E> NIL)
  (#<SPL #x49A27B6> NIL)
  (#<SPL #x499C68E> NIL)
  (#<SPL #x498FA26> NIL)
  (#<SPL #x4982746> NIL)
  (#<SPL #x4967556> NIL)
  (#<SPL #x495CCCE> NIL)
  (#<SPL #x4955BFE> NIL)))
|#


(setf spl1 (make_vps '(DO3 MI3 SIb3 RE4 FAd4 SI4)))
(setf spl2 (make_vps '(MI4 FAd4 SOLd4 SI4 DO5 RE5)))
(setf match-result (match spl2 spl1))


;MATCH_VPS
; (match_vps SPL1 SPL2 :matches :tolerance :step)
; Returns a list of VPS that can be used in a data base
; :tolerance + :step => see above
; :matches: wanted matching pitches
;           nil = only the best match
;           number = only the "number" matching pitches (0 = no matching pitches)

;REMARK: to have all the matches, use "merge"

(match_vps spl1 spl2)
(match_vps spl1 spl2 :matches 2)



;;;;;;;;;;;;;;;;;;;; SET THEORY ;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;; SET THEORY: DOCUMENTATION ;;;;;;;;;;;;;;;;;;;;

;GET-NAME / PRINT-NAE
; (get-name vps/prime-form)
; (print-name vps)
; Returns a list of lists containing the name and vector of "prime-form", and the name,
;    prime form and vector of the complementary form when existing.

; Ex: (get-name (make_vps '(DO4 DOd4 RE4))) / (get-name '(0 1 2))
;     -> ((0 1 2) (3-1 (12)) (2 1 0 0 0 0) (0 1 2 3 4 5 6 7 8) (9-1) (8 7 6 6 6 3))

; first  = Pitch-Classes of Prime Form
; second = Name of Prime Form
; third  = Vector of Prime Form
; fourth = Pitch-Classes of Complementary
; fifth  = Name of Complementary
; sixth  = Vector of Complementary

(setf test (make_vps '(DO4 DOd4 RE4)))
(get-name test)
(get-name '(0 1 2))
(get-name '(0 1 2 4 5 8 9))
(print-name '(0 1 2))
(print-name '(0 1 2 4 5 8 9))

(get-name my-spl)
(get-name v1)
(get-name v2)
(get-name v3)
(get-name my-rpl)
(get-name my-cil)
(get-name my-ail)
(get-name my-spectrum)
(get-name my-crl)
(get-name my-arl)

(print-name my-spl)
(print-name v1)
(print-name v2)
(print-name v3)
(print-name my-rpl)
(print-name my-cil)
(print-name my-ail)
(print-name my-spectrum)
(print-name my-crl)
(print-name my-arl)


;;;;;;;;;;;;;;;;;;;; SET THEORY: UFM ;;;;;;;;;;;;;;;;;;;;

;PITCH-CLASS
; (pitch-class vps)
;  Return the list of pitch classes in a numeric format starting from 0
;  VPS = if not chromatic SPL, convert
;        the deviation will not be taken into account

;NB: the pitches will contain no octaves and will be sorted
;    this method does not look for prime forms

(pitch-class my-spl)
(pitch-class v1)
(pitch-class v2)
(pitch-class v3)
(pitch-class my-rpl)
(pitch-class my-cil)
(pitch-class my-ail)
(pitch-class my-spectrum)
(pitch-class my-crl)
(pitch-class my-arl)


;PCH-CLASS
; (pch-class vps)
;  After computing the list of pitches in a numeric format, convert it into an RPL
;  VPS = if not chromatic SPL, convert
;        the deviation will not be taken into account

;NB: the pitches will start at DO
;    this method does not look for prime forms

(pch-class my-spl)
(pch-class v1)
(pch-class v2)
(pch-class v3)
(pch-class my-rpl)
(pch-class my-cil)
(pch-class my-ail)
(pch-class my-spectrum)
(pch-class my-crl)
(pch-class my-arl)


;PC-SET
; (pc-set vps)
; Return the pitch list of the prime form in a numeric format starting at 0

(pc-set my-spl)
(pc-set v1)
(pc-set v2)
(pc-set v3)
(pc-set my-rpl)
(pc-set my-cil)
(pc-set my-ail)
(pc-set my-spectrum)
(pc-set my-crl)
(pc-set my-arl)


;PCH-SET
; (pch-set vps)
; Return the pitch list of the prime form as an RPL

(pch-set my-spl)
(pch-set v1)
(pch-set v2)
(pch-set v3)
(pch-set my-rpl)
(pch-set my-cil)
(pch-set my-ail)
(pch-set my-spectrum)
(pch-set my-crl)
(pch-set my-arl)


;IPC-SET
; (ipc-set vps)
; Return the pitch list of the inversion of the prime form
;    in a numeric format starting at 0

(ipc-set my-spl)
(ipc-set v1)
(ipc-set v2)
(ipc-set v3)
(ipc-set my-rpl)
(ipc-set my-cil)
(ipc-set my-ail)
(ipc-set my-spectrum)
(ipc-set my-crl)
(ipc-set my-arl)


;IPCH-SET
; (ipch-set vps)
; Return the pitch list of the inversion of the prime form as an RPL

(ipch-set my-spl)
(ipch-set v1)
(ipch-set v2)
(ipch-set v3)
(ipch-set my-rpl)
(ipch-set my-cil)
(ipch-set my-ail)
(ipch-set my-spectrum)
(ipch-set my-crl)
(ipch-set my-arl)

;C-PC-SET
; (c-pc-set vps)
; Return the pitch list of the complement of the prime form
;    in a numeric format starting at 0

(c-pc-set my-spl)
(c-pc-set v1)
(c-pc-set v2)
(c-pc-set v3)
(c-pc-set my-rpl)
(c-pc-set my-cil)
(c-pc-set my-ail)
(c-pc-set my-spectrum)
(c-pc-set my-crl)
(c-pc-set my-arl)


;C-PCH-SET
; (c-pch-set vps)
; Return the pitch list of the complement of the prime form as an RPL

(c-pch-set my-spl)
(c-pch-set v1)
(c-pch-set v2)
(c-pch-set v3)
(c-pch-set my-rpl)
(c-pch-set my-cil)
(c-pch-set my-ail)
(c-pch-set my-spectrum)
(c-pch-set my-crl)
(c-pch-set my-arl)


;;;;;;;;;;;;;;;;;;;; SET THEORY: MAIN METHODS ;;;;;;;;;;;;;;;;;;;;

;REDUCE-TO-PITCH-CLASS
; (reduce-to-pitch-class spl)
; Returns another SPL in the format of a pitch class (see Set Theory).
; Operations: remove octaves, transpose all the pitches within one octave,
;                reorder and start from DO4 (= 0)
;EX:
(setf spl3 (make_vps '(DO3 MI3 SIB3 RE4 MI4 FAd4 SI4 SOLb5 )))
(setf spl4 (make_vps '("DO3" "MI3" "SIB3" "RE4" "MI4" "FAd4" "SI4" "SOLb5")))
(setf spl5 (make_vps '("DO3" "MI3" ("SIB3" . 15) ("RE4" . 50) ("MI4" . 70)
                       "FAd4" "SI4" ("SOLb5" . 15))))
(rpl_vps (reduce-to-pitch-class spl3)) ;-> ("DO" "RE" "MI" "FAd" "SIb" "SI")
(rpl_vps (reduce-to-pitch-class spl4)) ;-> ("DO" "RE" "MI" "FAd" "SIb" "SI")
                                       ; with micro-tonal VPS's it does what it can!
                                       ; ("RE4" . 50) = MIb, ("MI4" . 70) = FA
(rpl_vps (reduce-to-pitch-class spl5)) ;-> ("DO" "MIb" "MI" "FA" "FAd" "SIb" "SI")


;PRIME-FORM-SET
; (prime-form-set spl)
; Returns the prime form of its argument as another SPL.
; A prime form tends to begin with the smallest intervals.
; If a prime form is not found in the list of sets, then it is an inversion
;    of a prime form.
;EX:
(setf spl3 (make_vps '(DO3 MI3 SIB3 RE4 MI4 FAd4 SI4 SOLb5 )))
(setf spl4 (make_vps '("DO3" "MI3" "SIB3" "RE4" "MI4" "FAd4" "SI4" "SOLb5")))
(setf spl5 (make_vps '("DO3" "MI3" ("SIB3" . 15) ("RE4" . 50) ("MI4" . 70)
                       "FAd4" "SI4" ("SOLb5" . 15))))
(rpl_vps (prime-form-set spl3)) ;-> ("DO" "RE" "MI" "FAd" "SIb" "SI")
(rpl_vps (prime-form-set spl4)) ;-> ("DO" "RE" "MI" "FAd" "SIb" "SI")
                                       ; with micro-tonal VPS's it does what it can!
                                       ; ("RE4" . 50) = MIb, ("MI4" . 70) = FA
(rpl_vps (prime-form-set spl5)) ;-> ("DO" "MIb" "MI" "FA" "FAd" "SIb" "SI")


;INVERT-SET
; (invert-set spl)
; Returns the prime form of the inversion of the prime form of its argument

;EX:
(setf spl3 (make_vps '(DO3 MI3 SIB3 RE4 MI4 FAd4 SI4 SOLb5 )))
(setf spl4 (make_vps '("DO3" "MI3" "SIB3" "RE4" "MI4" "FAd4" "SI4" "SOLb5")))
(setf spl5 (make_vps '("DO3" "MI3" ("SIB3" . 15) ("RE4" . 50) ("MI4" . 70)
                       "FAd4" "SI4" ("SOLb5" . 15))))
(rpl_vps (invert-set spl3)) ;-> ("DO" "RE" "MI" "FAd" "SOL" "SOLd")
(rpl_vps (invert-set spl4))
(rpl_vps (invert-set spl5))


;COMPLEMENT-SET
; (complement-set spl)
; Returns the prime form of the complement of the prime form of its argument
; The complement may itself be the inversion of the prime form found in the
;     table with the Pitch-Class Sets.

(setf spl3 (make_vps '(DO3 MI3 SIB3 RE4 MI4 FAd4 SI4 SOLb5 )))
(setf spl4 (make_vps '("DO3" "MI3" "SIB3" "RE4" "MI4" "FAd4" "SI4" "SOLb5")))
(setf spl5 (make_vps '("DO3" "MI3" ("SIB3" . 15) ("RE4" . 50) ("MI4" . 70)
                       "FAd4" "SI4" ("SOLb5" . 15))))
(rpl_vps (complement-set spl3)) ;-> ("DO" "RE" "MI" "FAd" "SOL" "SOLd")
(rpl_vps (complement-set spl4))
(rpl_vps (complement-set spl5))

;;;;;;;;;;;;;;;;;;;; DATA BASES ;;;;;;;;;;;;;;;;;;;;

; A Data Base is simply a list of VPS's, either manually specified, or the result
;    of a computation (ex. coming from situation).

; Two formats are possible: either a list of straighforward VPS's
;    [test: (is_vs (car list))] or a list of lists containing a VPS as the first
;    argument, followed by other values whose meaning will depend on the user.
;    For example: name of the VPS, matching notes, analysis data. This other data
;    is simply accessed by (cdar list), while the VPS is accessed by (caar list)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

