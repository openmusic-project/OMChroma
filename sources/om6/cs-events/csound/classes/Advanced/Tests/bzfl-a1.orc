;=============================================================================
;			BZFL-1.ORC
; GBUZZ GENERATOR FILTERED BY A FOF FILTER / MONO
; AMPLITUDE ENVELOPE WITH POSCIL
;=============================================================================

; Timbre:    Various controlled noise spectra
; Synthesis: (g)buzz with fofilter
;            POSCIL envelopes
; Coded:     ms 8/08

; NB: NEW STRUCTURE FOR THE AMPLITUDES FROM AUGUST 2008!
;    Positive value > 0.0  : linear amplitude (>0.0-1000.0)
;    0.0 or negative value : amplitude in dB (0 = maximum value)

; The apparently arbitrary amplitude range (0-1000, rather than 0-1)
;         avoids Lisp printing small values with exponential notation

; Replaced oscili with poscil (precise oscillator), ms 8/08
; Default SR = 96000, recommended precision: 24 bits
;-----------------------------------------------------------------------------

;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4	= maximum amplitude [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= fundamental frequency [Hz]
;	p6	= centre frequency of the filter [Hz]
;	p7	= amplitude envelope [GEN number]
;	p8	= lowest harmonic present in the buzz [int]
;	p9	= % of maximum possible harmonic present [0-1]
;	p10	= multiplier in the series of amp coeffs [0-1]
;	p11	= envelope for the multiplier in the series of amp coeffs [GEN]
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
;   E-DELS	:	entry delays [sec] (0.0)
;	DURS	:	duration [sec] (1.0)
;	AMP	:	amplitude [lin, >0.0-1000.0 or dB <- 0.0] (-6.0)
;	F0	:	fundamental frequency [Hz] (110.0)
;	FREQ	:	cenre frequency [Hz] (600.0)
;	AENV	:	function number for the amplitude envelope [GEN] (triangle)
;	BZL	:	lowest harmonic present in the buzz [integer] (1)
;	BZH	:	% of maximum possible harmonic present [0-1] (1)
;	BZM	:	multiplier in the series of amp coeff [0-1] (0.95)
;	BZMENV	:	function number for the buzz envelope [GEN] (triangle)
;	WXIN	:	impulse response attack (rise) time [sec, 0.007]
;	WXOUT	:	impulse response decay time [sec, 0.04]
;*****************************************************************************

sr	= 96000
kr	= 96000
ksmps	= 1
nchnls	= 1

;0dbfs = 32767  ; 16 bits
0dbfs = 8388697 ; 24 bits

instr 1; *****************************************************************
idur		= p3
idurosc		= 1/p3
iamp 		= (p4 > 0.0 ? (p4*0.001*0dbfs) : (ampdbfs (p4)))
if0		= p5
ifq		= p6
iaenv		= p7
inn		= sr/2/if0		; total possible number of harmonics present
ihh		= int (inn * p9)	; % of possible total
ilh		= p8			; lowest harmonic present
ibzmul		= p10
ibzmenv		= p11			; envelope for the buzz
irise		= p12
idec		= p13

ifn		= 5			; stored cosine function

   kenv		poscil	iamp, idurosc, iaenv		; amp envelope
   kratio	poscil	ibzmul, idurosc, ibzmenv	; kratio envelope

   asig    gbuzz   	kenv,if0,ihh,ilh,kratio,ifn	
   afil	   fofilter	asig, ifq, irise, idec
   aout	   balance	afil, asig

           out     aout
endin
