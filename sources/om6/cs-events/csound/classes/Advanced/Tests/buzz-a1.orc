;=============================================================================
;			BUZZ-A1.ORC
; DYNAMIC SPECTRUM OSCILLATOR / MONO
; AMPLITUDE ENVELOPE WITH POSCIL, VALUES IN HARMONICS, CONTROLLABLE
;=============================================================================

; Timbre:    Harmonically related cosine partials with many control
; Synthesis: (g)buzz
;            POSCIL envelopes
; Coded:     ms 9/02, 8/08

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
;	p5	= min fundamental frequency [Hz]
;	p6	= max fundamental frequency [Hz]
;	p7	= envelope for the fundamental frequency [GEN]
;	p8	= amplitude envelope [GEN number]
;	p9	= lowest harmonic present in the buzz [int]
;	p10	= % of highest possible harmonic present [0-1]
;	p11	= min multiplier in the series of amplitude coefficients
;	p12	= max multiplier in the series of amplitude coefficients
;	p13	= envelope for the multiplier [GEN]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f5	large cosine
;_____________________________________________________________________________

; CLASS: BUZZ-A1

;  GLOBAL KEYWORDS (default values within parentheses):
;	NUMROWS		: amount of rows (components) in the event (1)
;	ACTION-TIME	: start time of the whole event [sec] (0.0)
;	USER-FUN	: user-defined parsing function (nil)

;  LOCAL KEYWORDS:
;     E-DELS	:	entry delays [sec] (0.0)
;	DURS	:	duration [sec] (1.0)
;	AMP	:	amplitude [lin, >0.0-1000.0 or dB <- 0.0] (-6.0)
;	F0	:	starting fundamental frequency [Hz] (220.0)
;	F0DEV	:	ending fundamental frequency [Hz] (440.0)
;	F0ENV	:	frequency envelope [GEN] (asc. line)
;	AENV	:	function number for the amplitude envelope [GEN] (triangle)
;	BZL	:	lowest harmonic present in the buzz [integer] (1)
;	BZH	:	min % highest possible harmonic present [0-1] (1.0)
;	BZM	:	min multiplier in the series of amp. coeff. [float] (0.1)
;	BZMD	:	max multiplier [float] (0.9)
;	BZMENV	:	function number for the multiplier [GEN] (triangle)
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
if0min		= p5
if0max		= p6
if0dev		= if0max-if0min
if0env		= p7
iaenv		= p8
ilh		= p9			; lowest harmonic present
ihh		= p10			; % of possible total

immin		= p11			; min multiplier
immax		= p12
imdev		= immax-immin
imenv		= p13

ifn		= 5			; stored cosine function

; f0 envelope
   kf0dev	poscil	if0dev, idurosc, if0env
   kf0		= kf0dev+if0min

; mul envelope
   kmdev	poscil	imdev, idurosc, imenv
   kmul		= kmdev+immin

; amp envelope
   kenv		poscil	iamp, idurosc, iaenv		; amp envelope

   asnd    gbuzz   kenv,kf0,(sr/2/kf0)*ihh,ilh,kmul,ifn

           out     asnd

endin
