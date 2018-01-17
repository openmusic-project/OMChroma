;=============================================================================
;			BZFL-1.ORC
; GBUZZ GENERATOR FILTERED BY A FOF FILTER / MONO
; AMPLITUDE ENVELOPE WITH POSCIL
;=============================================================================

; Timbre:    Formantic subtractive synthesis with buzz
; Synthesis: (g)buzz with fofilter
;            POSCIL envelopes
; Coded:     ms 8/08

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
;	p6	= centre freq of the filter [Hz]
;	p7	= amp envelope [GEN number]
;	p8	= lowest harmonic in the buzz [int]
;	p9	= % of highest harmonic [0-1]
;	p10	= multiplier in the series of amp coeffs [0-1]
;	p11	= envlp for the multiplier in the series of amp coeffs [GEN]
;	p12	= impulse response attack time (krise) [sec]
;	p13	= impulse respons decay time (atten) [sec]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f5	large cosine
;_____________________________________________________________________________

; CLASS: BZFL-1

;  GLOBAL KEYWORDS (default values within parentheses):
;	NUMROWS		: amount of rows (components) in the event (1)
;	ACTION-TIME	: start time of the whole event [sec] (0.0)
;	USER-FUN	: user-defined parsing function (nil)

;  LOCAL KEYWORDS:
;   E-DELS	: entry delays [sec] (0.0)
;	DURS	: duration [sec] (1.0)
;	AMP 	: ampl [lin, >0.0-1000.0 or dB <- 0.0] (-6.0)
;	F0  	: fundamental freq [Hz] (263.8)
;	FREQ	: centre freq of the fiter [Hz] (1600.0)
;	AENV	: function number for the amp envlp [GEN] (triangle)
;	BZL 	: lowest harmonic in the buzz [integer] (1)
;	BZH 	: highest harmonic [%, 0-1] (0.95)
;	BZM 	: multiplier in the series of amp coeff [flt]
;			:    a(i) = AMP(bzm**i)
;	BZMENV	: function number for the buzz envlp [GEN] (triangle)
;	WIN 	: impulse response attack (rise) time [sec, 0.007]
;	WOUT	: impulse response decay time [sec, 0.04]
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
if0		= p5
ifq		= p6
iaenv		= p7
inn		= sr/2/if0			; total possible number of harmonics
ihh		= int(inn*p9)		; % of possible total
ilh		= p8				; lowest harmonic
ibzmul	= p10
ibzmenv	= p11				; envelope for the buzz
irise	= p12
idec	= p13
ifn		= 5					; stored cosine function
;OUTBEG
;OUTEND

   kenv		poscil	iamp, idurosc, iaenv		; amp envelope
   kratio	poscil	ibzmul, idurosc, ibzmenv	; kratio envelope
   
   asig    gbuzz   	kenv,if0,ihh,ilh,kratio,ifn	
   afil	   fofilter	asig, ifq, irise, idec
   asound  balance	afil, asig

;OUTBEG
           out     asound
;OUTEND

endin
