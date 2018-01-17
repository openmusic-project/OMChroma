;;=============================================================================
;;			SUB-1.ORC
;; SUBTRACTIVE SYNTHESIS OF RANDOM NOISE (FROM ACCCI, 50_01_1.ORC) / MONO
;; AMPLITUDE ENVELOPE WITH OSCILI
;;=============================================================================
;;
;; Timbre:    Bands of noise
;; Synthesis: Subtractive synthesis
;;            Basic design
;;            RAND source
;; Coded:     jpg 8/92, modified ms 9/02, 8/08

; NB: NEW STRUCTURE FOR THE AMPLITUDES FROM AUGUST 2008!
;    Positive value > 0.0  : linear amplitude (>0.0-1000.0)
;    0.0 or negative value : amplitude in dB (0 = maximum value)

; The apparently arbitrary amplitude range (0-1000, rather than 0-1)
;         avoids printing small values with exponential notation

; Replaced oscili with poscil (precise oscillator), ms 8/08
; Default SR = 96000, recommended precision: 24 bits
;-----------------------------------------------------------------------------
;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4	= max amp [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= centre freq [Hz]
;	p6	= bandwidth [% of centre freq, 0->1]
;	p7	= amp envelope [GEN]
;-----------------------------------------------------------------------------


;-----------------------------------------------------------------------------
; PARAM DESCRIPTION :
; @PARAM 2 "e-dels" number 0.0 "action time [sec]"
; @PARAM 3 "durs" number 1.0 "duration [sec]"
; @PARAM 4 "amp" number -6.0 "maximum amplitude [linear (0.0,1000.0) or dB (-INF,0.0)]"
; @PARAM 5 "freq" number 2000.0 "center frequency [Hz]"
; @PARAM 6 "bw" number 0.01 "bandwidth [% of centre freq, 0->1]"
; @PARAM 7 "aenv" cr::cs-table (:cs-table cr::gen07 0 0 2048 100 4096 0) "amplitude envelope [GEN table]"
;-----------------------------------------------------------------------------

sr	= 96000
kr	= 96000
ksmps	= 1
nchnls	= 1

;0dbfs = 32767  ; 16 bits
0dbfs = 8388607 ; 24 bits

instr 1 ; --------------------------------------------------------------------
idur	= p3
idurosc	= 1.0/idur
iamp 	= (p4 > 0.0 ? (p4*0.001*0dbfs) : (ampdbfs(p4)))
icfq	= p5
ibw		= p6*icfq
iaenv	= p7
iskip	= 2
;OUTBEG
;OUTEND

 anoise  rand   iamp                      	; white noise
   a1      reson  anoise,  icfq, ibw, iskip	; filter, iskip = 2
   a2      poscil 1.0, idurosc, iaenv
   asound	=    a1*a2

;OUTBEG
	out    asound
;OUTEND

endin
