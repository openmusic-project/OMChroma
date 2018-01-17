;=============================================================================
;			BUZZ-1.ORC
; DYNAMIC SPECTRUM OSCILLATOR (FROM ACCCI, 43_21_1.ORC) / MONO
; AMPLITUDE ENVELOPE WITH POSCIL
;=============================================================================

; Timbre:    Various controlled noise spectra
; Synthesis: (g)buzz
;            POSCILI envelopes
; Coded:     jpg 8/92, modified ms 9/02, 8/08

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
;	p5	= fundamental freq [Hz]
;	p6	= amp envelope [GEN]
;	p7	= lowest harmonic in the buzz [int]
;	p8	= % of maximum possible harmonic [0-1]
;	p9	= multiplier in the series of amp coeff [0-1]
;	p10	= envelope for the multiplier [GEN]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f5	large cosine
;_____________________________________________________________________________

; CLASS: BUZZ-1

;  GLOBAL KEYWORDS (default values within parentheses):
;	NUMROWS		: amount of rows (components) in the event (1)
;	ACTION-TIME	: start time of the whole event [sec] (0.0)
;	USER-FUN	: user-defined parsing function (nil)

;  LOCAL KEYWORDS:
;   E-DELS	:	entry delays [sec] (0.0)
;	DURS	:	duration [sec] (1.0)
;	AMP		:	amp [lin, >0.0-1000.0 or dB <- 0.0] (-6.0)
;	F0		:	fundamental freq [Hz] (220.0)
;	AENV	:	function number for the amp envlp [GEN] (triangle)
;	BZL		:	lowest harmonic in the buzz [integer] (1)
;	BZH		:	% of maximum possible harmonic [0-1] (1)
;	BZM		:	multiplier in the series of amp coeff [0-1] (0.95)
;	BZMENV	:	function number for the buzz envlp [GEN] (triangle)
;*****************************************************************************

sr	= 96000
kr	= 96000
ksmps	= 1
nchnls	= 1

;0dbfs = 32767  ; 16 bits
0dbfs = 8388607 ; 24 bits

instr 1 ; -------------------------------------------------------------
idur		= p3
idurosc		= 1/p3
iamp 		= (p4 > 0.0 ? (p4*0.001*0dbfs) : (ampdbfs(p4)))
ifq		= p5
iaenv	= p6
inn		= sr/2/ifq			; total possible number of harmonics present
inn		= int(inn * p8)		; % of possible total
ilh		= p7				; lowest harmonic present
ifn		= 5					; stored cosine function
ibzmul	= p9				; multiplier
ibzmenv	= p10				; envelope for the multiplier
;OUTBEG
;OUTEND


  kenv		poscil	iamp, idurosc, iaenv		; amp envelope
   kratio	poscil	ibzmul, idurosc, ibzmenv	; kratio envelope

   asound    gbuzz   kenv,ifq,inn,ilh,kratio,ifn

;OUTBEG
   out     asound
;OUTEND

endin
