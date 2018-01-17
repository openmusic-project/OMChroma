;=============================================================================
;			RANFL-1.ORC
; FREQ LIMITED RANDOM NOISE FILTERED BY A CASCADED RESON AND FOF FILTERS / MONO
; AMPLITUDE ENVELOPE WITH POSCIL
;=============================================================================

; Timbre:    Formantic subtractive synthesis with noise
; Synthesis: randi with cascaded reson and fofilter
;            POSCIL envelopes
; Coded:     ms 2/09

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
; COMPULSORY GEN FUNCTIONS
;_____________________________________________________________________________

; CLASS: RANFL-1

;  GLOBAL KEYWORDS (default values within parentheses):
;	NUMROWS		: amount of rows (components) in the event (1)
;	ACTION-TIME	: start time of the whole event [sec] (0.0)
;	USER-FUN	: user-defined parsing function (nil)

;  LOCAL KEYWORDS:
;   E-DELS	: entry delays [sec] (0.0)
;	DURS	: duration [sec] (1.0)
;	AMP 	: ampl [lin, >0.0-1000.0 or dB <- 0.0] (-6.0)
;	F0  	: freq of the random source [Hz] (263.8)
;	FREQ	: centre freq of both filters [Hz] (1600.0)
;	BW  	: bandwidth of the reson [Hz] (500.0)
;	AENV	: amplitude envelope [GEN] (triangle)
;	WIN 	: impulse response attack time of the fofilter [sec, 0.03]
;	WOUT	: impulse response decay time of the fofilter [sec, 0.5]
;*****************************************************************************

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
