;;=============================================================================
;;			ADD-1.ORC
;; SIMPLE ADDITIVE SYNTHESIS (FROM ACCCI, 02_01_3.ORC) / MONO
;; AMPLITUDE ENVELOPE WITH OSCILI
;;=============================================================================
;;
;; Timbre:    gong
;; Synthesis: additive same units (02)
;;            basic instrument( 01)
;; Source:    #420, Gong-like Sounds, Risset(1969)
;; Coded:     jpg 8/93, modified ms 9/02, 8/08
;;
;;   AMPLITUDES:
;;   Positive value > 0.0  : linear amplitude (>0.0-1000.0)
;;   0.0 or negative value : amplitude in dB (0 = maximum value)
;;
;; The apparently arbitrary amplitude range (0-1000, rather than 0-1)
;; avoids Lisp printing small values with exponential notation


;-----------------------------------------------------------------------------
; Replaced oscili with poscil (precise oscillator), ms 8/08
; Default SR = 96000, recommended precision: 24 bits
;-----------------------------------------------------------------------------
;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4	= maximum amplitude [linear, 0.0 -> 1000.0]
;	p5	= frequency [Hz]
;	p6	= amplitude envelope [GEN number]


;-----------------------------------------------------------------------------
; PARAM DESCRIPTION :
; @PARAM 2 "e-dels" number 0.0 "entry delays [sec]"
; @PARAM 3 "durs" number 1.0 "duration [sec]"
; @PARAM 4 "amp" number -6.0 "maximum amplitude [linear (0.0,1000.0) or dB (-INF,0.0)]"
; @PARAM 5 "freq" number 440.0 "frequency [Hz]"
; @PARAM 6 "aenv" cr::cs-table (:cs-table cr::gen07 :points ((0 0) (2048 100) (4096 0))) "amplitude envelope [GEN table]"
;
; COMPULSORY GEN FUNCTIONS :
; @GEN f1  0  65537  10  10
;-----------------------------------------------------------------------------


sr	= 96000
kr	= 96000
ksmps	= 1
nchnls	= 1

;0dbfs = 32767  ; 16 bits
0dbfs = 8388607 ; 24 bits

;;=============================================================================

instr 1 

idur		= p3
idurosc		= 1/idur
iamp 		= (p4 > 0.0 ? (p4*0.001*0dbfs) : (ampdbfs(p4)))
ifq		= p5
iaenv		= p6
iaudiofun	= 1

   a2    poscil  iamp, idurosc, iaenv
   a1    poscil  a2, ifq, iaudiofun
         out     a1
endin
