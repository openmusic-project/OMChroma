;;;;;;Instanciations avec selection automatique de la classe ;;;;;;
;;;;;;;;;;;;en fonction de la signature pour make_vps
(in-package chroma)

;;;;;;;;;;;;;;;;;;;;;predicates
; interval-p
; should yield "t"
(interval-p '-6+)
(interval-p '(-6+ 0))
(interval-p '(-6+ 0 -12))
(interval-p '7-)
(interval-p '(7- 0))
(interval-p '(7- 0 56))

; should yield "nil"
(interval-p '+6+)
(interval-p '-6f+)
(interval-p '(-6+ d))
(interval-p '(-6+ 0 d))
(interval-p '(-6+ 0 q))
(interval-p '(-6+ 0 -q))
(interval-p 'a)

; pitch-with-octave-p
(pitch-with-octave-p 'DO4)
(pitch-with-octave-p '(DO4 . -12))
(pitch-with-octave-p '(DO4 . 34))

(pitch-with-octave-p 'DO)
(pitch-with-octave-p 'DOq)
(pitch-with-octave-p 'DOfr4) ;positif ?
(pitch-with-octave-p '(DO . 23))
(pitch-with-octave-p '(DO4 . de))
(pitch-with-octave-p '(DOdi4 . -44));positif ?
(pitch-with-octave-p 'pippo)
(pitch-with-octave-p '(pippo . 23))

; pitch-without-octave-p
(pitch-without-octave-p 'DO)
(pitch-without-octave-p '(MIb . -12))
(pitch-without-octave-p '(SOLb . 34))

(pitch-without-octave-p 'DO4)
(pitch-without-octave-p 'DOq);positif ?
(pitch-without-octave-p 'DOfr)
(pitch-without-octave-p '(DO7 . 23))
(pitch-without-octave-p '(DO2 . de))
(pitch-without-octave-p '(DOdi4 . -44))
(pitch-without-octave-p 'pipp)
(pitch-without-octave-p '(pipp . 23))


;;;;;;;;;;;;;;;;;;;;;type-of_vps
;spl
(type-of_vps '(DO4 LAb4 RE5 SOL5 REb6))
(type-of_vps '(DO4 (LAb4 23) RE5 (SOL5 -12) REb6))

;rpl
(type-of_vps '(DO LAb RE1 SOL1 REb2))
(type-of_vps '((DO . 23) (LAb . -34) RE1 SOL1 (REb2 . 77)) )

;cil
(type-of_vps '(6- 4+ 4 4+))
(type-of_vps '((6- 0 12) (4+ 1 -21) (4 0 7) (4+ 1 -2)) )

;ail
(type-of_vps '(-6+ -2- 4 7- (3+ 1)) 'LA4)
(type-of_vps '((-6+ 0 12) -2- (4 0 -22) 7- (3+ 1 17)) 'LA4)

;fql
(type-of_vps '(261 415 587 784 1108))

;crl
(type-of_vps '(0.5873 0.4142 0.3345 0.4145))

;arl
(type-of_vps '(2.61 4.153 5.873 7.84 11.07) 100.0)

;wrong
(type-of_vps '(DO LAb RE1 SOL1 REb2) 100.0)
(type-of_vps '(2.61 er 5.873 7.84 11.07) 100.0)
(type-of_vps '(6- 4+d 4 4+))
(type-of_vps '(6-d 4+ 4 4+))


;;;;;;;;;;;;;;;;;;;;; instanciations
;spl
(setf my-spl (make_vps '(DO4 LAb4 RE5 SOL5 REb6)) )
(setf my-spl1 (make_spl '(DO4 LAb4 RE5 SOL5 REb6)) )
(setf my-spl2 (make_vps '(DO4 (LAb4 . 23) RE5 (SOL5 . -12) REb6) ))
(setf my-spl3 (make_spl '(DO4 (LAb4 . 23) RE5 (SOL5 . -12) REb6) ))
(print_vs my-spl)

;rpl
(setf my-rpl (make_vps '(DO LAb RE1 SOL1 REb2)) )
(setf my-rpl1 (make_rpl '(DO LAb RE1 SOL1 REb2)) )
(setf my-rpl (make_vps '((DO . 23) (LAb . -34) RE1 SOL1 (REb2 . 77)) ))
(setf my-rpl1 (make_rpl '(DO (LAb . -34) RE1 SOL1 (REb2 . 77)) )
(print_vs my-rpl)
;ppl
(setf my-ppl (make_vps '(DO LAb RE SOL REb)) )
(print_vs my-ppl)

;cil
(setf my-cil (make_vps '(6- 4+ 4 4+)) )
(setf my-cil1 (make_cil '(6- 4+ 4 4+)) )
(setf my-cil2 (make_vps '((6- 0 12) (4+ 1 -21) (4 0 7) (4+ 1 -2)) )
(setf my-cil3 (make_cil '((6- 0 12) (4+ 1 -21) (4 0 7) (4+ 1 -2)) )
(print_vs my-cil)

;ail
(setf my-ail (make_vps '(-6+ -2- 4 7- (3+ 1)) 'LA4) )
(setf my-ail1 (make_ail '(-6+ -2- 4 7- (3+ 1)) 'LA4) )
(setf my-ail2 (make_vps '((-6+ 0 12) -2- (4 0 -22) 7- (3+ 1 17)) 'LA4) )
(setf my-ail3 (make_ail '((-6+ 0 12) -2- (4 0 -22) 7- (3+ 1 17)) 'LA4) )
(print_vs my-ail)

;fql
(setf my-fql (make_vps '(261 415 587 784 1108)) )
(setf my-fql1 (make_fql '(261 415 587 784 1108)) )
(print_vs my-fql)

;crl
(setf my-crl (make_vps '(0.5873 0.4142 0.3345 0.4145)) )
(setf my-crl1 (make_crl '(0.5873 0.4142 0.3345 0.4145)) )
(print_vs my-crl)

;arl
(setf my-arl (make_vps '(2.61 4.153 5.873 7.84 11.07) 100.0) )
(setf my-arl1 (make_arl '(2.61 4.153 5.873 7.84 11.07) 100.0) )
(print_vs my-arl)

;error
(setf my-err (make_vps 'DO4) )
(setf my-err1 (make_vps '(DOf4 LAbq4 RE5 SOL5 REb6)) )
(setf my-err (make_fql '(DO4 LAb4 RE5 SOL5 REb6)) )
(print_vs my-err)

;;;;;;Tests des selecteurs/modificateurs ;;;;;;

