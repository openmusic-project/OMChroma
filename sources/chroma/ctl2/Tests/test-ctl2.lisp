(in-package :cr)

;-----------------------------------------------------------------------------
;		PART 1 : INITIALIZE THE GLOBAL PARAMETERS
;-----------------------------------------------------------------------------
	; SPECIFY WHICH SYNTHESIZER TO USE (CSOUND) AND WHERE IT WILL RUN
(use-csound 'om)
	; THIS IS THE NORMAL MODE FOR ERROR TRACKING ALGORITHMS
(normal-mode)			; (alternative-mode)
	; PRINTOUT IN THE SCORE NOT ONLY ERRORS BUT ALSO WARNINGS
(enable-print)			; (disable-print)
        ; PRINTOUT OF CTL-1 INSTRUCTIONS' HEADERS ON LISTENER WINDOW
(enable-print-ctl1)
        ; PRINTOUT OF CTL-2 INSTRUCTIONS' HEADERS ON LISTENER WINDOW
(enable-print-ctl2)

        ; DIRECTORY WHERE THE ANALYSIS MODELS ARE TO BE FOUND
(init-chroma-home)

;-----------------------------------------------------------------------------
;		PART 2C : LOAD ANALYSIS MODELS
;-----------------------------------------------------------------------------

(setf markers
      (load-markers-file
       (merge-pathnames (getenv LLamod ) "nah-1-a1.mark")))



(setf markers
      (load-markers-file-sdif
       (merge-pathnames (getenv LLamod ) "trombone-C3¶.mrk.sdif")))

(setf add-analysis
      (make-instance 'additive-data
         :file (merge-pathnames (getenv LLamod ) "dens1_aa.add")))

(setf add-analysis
      (make-instance 'additive-data 
         :file (merge-pathnames (getenv LLamod ) "trombone-C3¶.trc.sdif")))

(inspect add-analysis)



(setf model (make-instance 'model-partials :add add-analysis :markers markers))
(inspect model)
;-----------------------------------------------------------------------------
;		PART 2E : SPECIFY SOME AD-HOC CONTROL MODELS
;-----------------------------------------------------------------------------
; dynamic stretch + offset
(setf stretch-fun-a (make_fun '(2.0 0  2.0 1)))
(setf offset-fun-a  (make_fun '(0.0 0  0.0 1)))

(setf stretch-fun-b (make_fun '(2.35 0  1.7 0.4  1.3 1)))
(setf offset-fun-b  (make_fun '(0.0 0  0.0 0.4  2.0 1)))

(setf stretch-fun-c (make_fun '(1.4 0  2.3 1)))
(setf offset-fun-c  (make_fun '(0.0 0  -0.25 1)))

(setf stretch-fun-d (make_fun '(2.0 0  1.3 0.4 2.5 1)))
(setf offset-fun-d  (make_fun '(0.0 0  -0.75 1)))

(setf main-tests '(ed-0? ed-durmin?
                   compute-dur! dur-durmin? ed+dur?
                   amp? compute-amp!
                   fq-sr? fqmin?))

(setf main-subtests '(s-fq-sr? s-fqmin?))


(setf ctl-mod-a
      '(
        (om::user-fun (omfun get-user-fun1
                             :tests main-tests
                             :subc '(sub-comps+)
                             :subtests main-subtests))
;        (om::user-fun (omfun get-user-fun))

        (om::DURTOT (call durtot-fun :scaler (make_fun '(2 0 3 1) )
                          :mindur 0.5))

        (E-DELS '(lambda (x) (ran-from 0.0 0.1)))

        (DURS (from min 1.0 max 0.5)
             (to min 1.0 max 0.1)
             )

        (DUR2 (from min 1.0 max 0.3)
             (to min 1.0 max 0.1)
             )

        (AMP (call get-model-norm-amp))

        (AMP2 (call get-model-norm-amp))

        (BAL (from min 0.0 max 0.1)
             (to min 0.9 max 1.0))

        (FQ (call get-model-fq-with-stretch
                  :stretching stretch-fun-a
                  :offset offset-fun-a
                  ))

        (ATK '(lambda (x) (ran-from 0.01 0.025)))
        (DEC '(lambda (x) (ran-from 0.1 0.2)))

; data base of virtual envelopes
;       (AENV '(lambda (x) (s_ve (p-ve (ran-from 0 32) ve-c))))

; some amplitude modulation
        (JTA '(lambda (x) (ran 0.15 0.1)))
        (TRA '(lambda (x) (ran 0.08 0.03)))
        (TRF '(lambda (x) (ran 6.0 1.5)))

;        (JTV  '(lambda (x) (ran 0.15 0.1)))
;        (VFQ '(lambda (x) (ran 0.08 0.03)))
;        (FDEV '(lambda (x) (ran 6.0 1.5)))
        (FENV 7)
        (PENV 7)

 ;       (PAMP (call get-model-dev+))
 ;       (PENV (call get-model-transp_bpf    
 ;                 :reduction 5
 ;               ))
 ;        (PDUR (call get-model-dur))
 ;       (PFLG 0)
        
        (NPART '(lambda (x) (ran-from 2 4)))

        (STON '(lambda (x) (ran-from 0.01 0.08)))

        ))

(setf ctl-mod-basic
      '(
  ;      (om::user-fun (omfun get-user-fun))

;;        (om::DURTOT (call durtot-fun :scaler (make_fun '(2 0 3 1) )
 ;                         :mindur 0.5))

        (E-DELS '(lambda (x) (ran-from 0.0 0.1)))
        (DURS (from min 1.0 max 0.5)
             (to min 1.0 max 0.1)
             )

;        (DUR2 (from min 1.0 max 0.3)
;             (to min 1.0 max 0.1)
;             )

        (AMP (call get-model-norm-amp))

;        (AMP2 (call get-model-norm-amp))

;        (BAL (from min 0.0 max 0.1)
;             (to min 0.9 max 1.0))

        (FQ (call get-model-fq
                  ))

  ;      (ATK '(lambda (x) (ran-from 0.01 0.025)))
  ;      (DEC '(lambda (x) (ran-from 0.1 0.2)))

        (AENV (call get-model-amp_bpf :reduction 5))

; some amplitude modulation
 ;       (JTA '(lambda (x) (ran 0.15 0.1)))
 ;       (TRA '(lambda (x) (ran 0.08 0.03)))
 ;       (TRF '(lambda (x) (ran 6.0 1.5)))

;        (JTV  '(lambda (x) (ran 0.15 0.1)))
;        (VFQ '(lambda (x) (ran 0.08 0.03)))
;        (FDEV '(lambda (x) (ran 6.0 1.5)))
;        (FENV 7)
;        (PENV 7)

        (PAMP (call get-model-dev+))
        (PENV (call get-model-transp_bpf    
                  :reduction 5
                ))
 ;        (PDUR (call get-model-dur))
 ;       (PFLG 0)
        
 ;       (NPART '(lambda (x) (ran-from 2 4)))

;        (STON '(lambda (x) (ran-from 0.01 0.08)))

        ))


;-----------------------------------------------------------------------------
;		PART 3 : COMPUTE THE ACTUAL EVENT(S)
;-----------------------------------------------------------------------------
(set-gbl 'DURMIN 0.01)

;-----------------------------------------------------------------------------
(setf output-file "test-AD1s")

(open-output-file output-file)


(setf array-list
      (ctl2 (make-instance 'om::ad1m) model ctl-mod-basic))

(close-output-file output-file)
;******************************************************************

;-----------------------------------------------------------------------------
(setf output-file "test-AD1s")

(open-output-file output-file)


(setf array-list
      (ctl2 (make-instance 'om::ad1m) model ctl-mod-a))

(close-output-file output-file)
;******************************************************************
