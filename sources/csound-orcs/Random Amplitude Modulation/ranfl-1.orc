;;=============================================================================
;;			RANFL-1.ORC
;; FREQ LIMITED RANDOM NOISE FILTERED BY A CASCADED RESON AND FOF FILTERS / MONO
;; AMPLITUDE ENVELOPE WITH POSCIL
;;=============================================================================
;;
;; Timbre:    Formantic subtractive synthesis with noise
;; Synthesis: randi with cascaded reson and fofilter
;;            POSCIL envelopes
;; Coded:     ms 2/09

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
;	p5	= freq of the noise [Hz]
;	p6	= centre freq of both filters [Hz]
;	p7	= bandwidth of the reson [Hz]
;	p8	= amp envelope [GEN]
;	p9	= impulse response attack time of the fofilter [sec]
;	p10	= impulse response decay time of the fofilter [sec]
;-----------------------------------------------------------------------------


;-----------------------------------------------------------------------------
; PARAM DESCRIPTION :
; @PARAM 2 "e-dels" number 0.0 "action time [sec]"
; @PARAM 3 "durs" number 1.0 "duration [sec]"
; @PARAM 4 "amp" number -6.0 "maximum amplitude [linear (0.0,1000.0) or dB (-INF,0.0)]"
; @PARAM 5 "f0" number 263.8 "freq of the noise [Hz]"
; @PARAM 6 "freq" number 1600.0 "centre freq of both filters [Hz]"
; @PARAM 7 "bw" number 500.0 "bandwidth of the reson [Hz]"
; @PARAM 8 "aenv" cr::cs-table (:cs-table cr::gen07 :points ((0 0) (2048 1) (4096 0))) "amplitude envelope [GEN]"
; @PARAM 9 "win" number 0.03 "impulse response attack time of the fofilter [sec]"
; @PARAM 10 "wout" number 0.5 "impulse response decay time of the fofilter [sec]"
;-----------------------------------------------------------------------------

sr	= 96000
kr	= 96000
ksmps	= 1
nchnls	= 1

;0dbfs = 32767  ; 16 bits
0dbfs = 8388607 ; 24 bits

instr 1 ; -------------------------------------------------------------
idur	= p3
idurosc	= 1/p3
iamp 	= (p4 > 0.0 ? (p4*0.001*0dbfs) : (ampdbfs(p4)))
if0		= p5
ifq		= p6
ibw		= p7
iaenv	= p8
irise	= p9
idec	= p10
iseed	= 2	; seed from system time
isize	= 1	; 31-bit random numbers
iscl	= 1 ; peak response factor = 1
;OUTBEG
;OUTEND

anoise	randi	1.0, if0, iseed, isize

asig	reson	anoise, ifq, ibw, iscl
afil	fofilter	asig, ifq, irise, idec
asound	balance	afil, anoise

kenv	poscil	iamp, idurosc, iaenv		; amp envelope

;OUTBEG
           out     asound*kenv
;OUTEND

endin
