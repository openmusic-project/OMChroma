;=============================================================================
;		FM-2.ORC
; FREQUENCY MODULATION (FROM ACCCI, 20_10_1.ORC) / MONO
; AMPLITUDE AND INDEX ENVELOPES WITH OSCILI
; SAME AS FM-1.ORC, BUT WITH FREQ INSTEAD OF N1
;=============================================================================

; Synthesis: FM with dynamic spectral evolution
;            Bell settings
; Source:    Chowning (1973)
; Coded:     jpg 8/92, modified ms 1/08, 8/08

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
;	p4	= maximum amp [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= fundamental freq [Hz]
;	p6	= freq of the carrier [Hz]
;	p7	= N2
;	p8	= max index
;	p9	= min index
;	p10	= amp envelope [GEN number]
;	p11	= index envelope [GEN number]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f1	carrier audio wave (sine tone)
;	f2	modulating audio wave (sine tone)
;_____________________________________________________________________________

; CLASS: FM-1
;   Default settings for a bell-like sound

;  GLOBAL KEYWORDS (default values within parentheses):
;	NUMROWS		: amount of rows (components) in the event (1)
;	ACTION-TIME	: start time of the whole event [sec] (0.0)
;	USER-FUN	: user-defined parsing function (nil)

;  LOCAL KEYWORDS:
;	E-DELS	: entry delays [sec] (0.0)
;	DURS	: duration [sec] (10.0)
;	AMP 	: amp [lin, >0.0-1000.0 or dB <- 0.0] (-6.0)
;[	F0  	: fundamental freq [Hz] (100.0)]
;			UNUSED HERE, KEPT FOR COMPATIBILITY WITH FM-1
;	FREQ	: carrier freq (500.0)
;	FMOD	: modulating freq (100.0)
;	IMAX	: max index (5)
;	IMIN	: min index (1)
;	AENV	: function number for the amp envlp [GEN] (triangle)
;	IENV	: function number for the index envlp [GEN] (triangle)
;*****************************************************************************

sr	= 96000
kr	= 96000
ksmps	= 1
nchnls	= 1

;0dbfs = 32767  ; 16 bits
0dbfs = 8388607 ; 24 bits


instr 1 ; -------------------------------------------------------------
idur		= p3
idurosc		= 1/idur
iamp 		= (p4 > 0.0 ? (p4*0.001*0dbfs) : (ampdbfs(p4)))
if0			= p5 ; unused here
icar		= p6			; carrier
imod		= p7			; modulating

imax		= p8
imin		= p9
imindev		= imin*imod
imaxdev		= imax*imod
ivardev		= imaxdev-imindev

iaenv		= p10
ienv		= p11
icarfun		= 1
imodfun		= 2
ieps		= 0.01
						; short fadeout (avoid clicks in exp envelopes)
;OUTBEG
;OUTEND

   ken	poscil  iamp, idurosc, iaenv            ; amplitude envelope
   k1	linseg 1, idur-ieps, 1,  ieps, 0	; avoid clicks
   kenv	=	ken * k1
   
   ki		poscil  ivardev, idurosc, ienv	; dynamic modulator
   kind		=	imindev + ki
   amod		poscil  kind, imod, imodfun

   asound	poscil  kenv, icar+amod, icarfun	; carrier

;OUTBEG
   out     asound
;OUTEND

endin
