;=============================================================================
;				WSHP-1.ORC
; 		WAVESHAPING (FROM ACCCI, 40_02_1.ORC) / MONO
; 		AMPLITUDE ENVELOPE WITH LINEN
;=============================================================================

; Timbre:    Clarinet-like
; Synthesis: Waveshaping
;            Basic instrument with duration dependent envelope
; Source:    Risset(1969)
;            #150, Serial Excerpt with Clarinet-like Sounds by Nonlinearity
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
;	p5	= freq [Hz]
;	p6	= attack time of the amp envlp [sec]
;	p7	= decay time of the amp envlp [sec]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f1	sine tone
;	f10	transfer function for the waveshaper
;_____________________________________________________________________________

; CLASS: WSHP-1

;  GLOBAL KEYWORDS (default values within parentheses):
;	NUMROWS		: amount of rows (components) in the event (1)
;	ACTION-TIME	: start time of the whole event [sec] (0.0)
;	USER-FUN	: user-defined parsing function (nil)

;  LOCAL KEYWORDS:
;	E-DELS	: entry delays [sec] (0.0)
;	DURS	: duration [sec] (1.0)
;	AMP		: amp [lin, >0.0-1000.0 or dB <- 0.0] (-6.0)
;	FREQ	: freq [Hz] (220.0)
;	ATK		: attack time [sec] (0.085)
;	DEC		: decay time [sec] (0.64)
;*****************************************************************************

sr	= 96000
kr	= 96000
ksmps	= 1
nchnls	= 1

;0dbfs = 32767  ; 16 bits
0dbfs = 8388697 ; 24 bits

instr 1 ; --------------------------------------------------------------------
idur		= p3
iamp 		= (p4 > 0.0 ? (p4*0.001*0dbfs) : (ampdbfs(p4)))
ifq			= p5
iaudiofun	= 1
itbloffset	= 0.5	; must be the half of the table size
ixmode		= 1	; index data mode normalized 0-1
ixferfun	= 10

iatk		= p6
idec		= p7

if idur > (iatk+idec) igoto start	; correct atk and dec if too long
iatk	= (((idur-iatk) > 0.01) ? iatk : (idur / 2.0))
idec  =  (((idur-iatk) > 0.01) ? (idur-iatk) : (idur / 2.0))
start:
;OUTBEG
;OUTEND

    aenv    linen    itbloffset-1, iatk, idur, idec		; envelope
    a1      poscil   aenv, ifq, iaudiofun			; sinus
    a1      tablei   a1 + itbloffset, ixferfun, ixmode	; transfer function

   	asound	= a1 * iamp				; scale to amplitude

;OUTBEG
 	out      asound
;OUTEND

endin
