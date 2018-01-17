;=============================================================================
;			FOF-A1.ORC
; FORMANTIC WAVEFORM (FROM ACCCI, 45_01_2.ORC) / MONO
; AMPLITUDE ENVELOPE WITH POSCILI
; COMPLETE CONTROL OF THE DETAILS OF THE FOF FROM THE SCORE
;=============================================================================

; Timbre:    Granular synthesis with fof module, voice-like tones
; Synthesis: FOF (Forme d'Onde Formatique)
;            OSCILI envelopes
; Coded:     jpg 8/92, modified ms 9/04, 8/08

; NB: NEW STRUCTURE FOR THE AMPLITUDES FROM AUGUST 2008!
;    Positive value > 0.0  : linear amplitude (>0.0-1000.0)
;    0.0 or negative value : amplitude in dB (0 = maximum value)

; The apparently arbitrary amplitude range (0-1000, rather than 0-1)
;         avoids printing small values with exponential notation
; Replaced oscili with poscil (precise oscillator), ms 8/08
; Default SR = 96000, recommended precision: 24 bits
; NB1: this implementation works with both audio and sub-audio f0's and
;       allows for an independent control of rise, decay and grain duration
;-----------------------------------------------------------------------------
;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4	= max amplitude [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= fundamental frequency [Hz]
;	p6	= formant frequency [Hz]
;	p7	= bandwidth [Hz]
;	p8	= amplitude envelope [GEN number]
;	p9	= tex or rise time [sec]
;	p10	= total duration of the burst [sec]
;	p11	= decay time of the grain envelope [sec]
;	p12	= octaviation [=>0.0]
;	p13	= phase [0-1]
;	p14	= skip, if not 0, skip initalization (for legato)
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f1	large sine tone
;	f19	sigmoid rise/decay shape
;_____________________________________________________________________________

; CLASS: FOF-A1

;  GLOBAL KEYWORDS (default values within parentheses):
;	NUMROWS		: amount of rows (components) in the event (1)
;	ACTION-TIME	: start time of the whole event [sec] (0.0)
;	USER-FUN	: user-defined parsing function (nil)

;  LOCAL KEYWORDS:
;	E-DEL	: entry delays [sec] (0.0)
;	DURS	: duration [sec] (1.0)
;	AMP 	: max amplitude [lin, >0.0-1000.0 or dB <- 0.0] (-6.0)
;	F0  	: fundamental frequency [Hz] (220.0)
;	FREQ	: centre frequency [Hz] (609.0)
;	BW  	: bandwidth [Hz] (77.0)
;	AENV	: amplitude envelope [GEN] (smooth)
;	WIN 	: rise time of the grain envelope [sec] (0.003)
;	WDUR	: duration of the grain [sec] (0.02)
;	WOUT	: decay time of the grain envelope [sec] (0.007)
;	OCT 	: octaviation factor [=>0, 0.0]
;	PHS 	: starting phase [0-1, 0]
;	PAR1	: if not 0, skip initialization (legato) [0]
;*****************************************************************************

sr	= 96000
kr	= 96000
ksmps	= 1
nchnls	= 1

;0dbfs = 32767  ; 16 bits
0dbfs = 8388607 ; 24 bits

instr 1; *****************************************************************
idur   	= p3
idurosc	= 1/p3
iamp 	= (p4 > 0.0 ? (p4*0.001*0dbfs) : (ampdbfs(p4)))
if0		= p5
ifreq	= p6
ibw		= p7
iaenv	= p8
iwin	= p9
iwdur	= p10
iwout	= p11
ioct	= p12
iphs	= p13
iskip	= p14

imode	= 0
iolaps	= 100000 ; how many simultaneous FOFs can be played (takes little memory if not used)

iafun	= 1
isigfun	= 19
;OUTBEG
;OUTEND

   kenv	poscil	iamp, idurosc, iaenv
   ;                           koct                       		ifna    idur
   ;          xamp  xfund xform     kband kris   kdur    kdec   iolaps   ifnb
asound	fof     1.0, if0,  ifreq, ioct,  ibw, iwin, iwdur, iwout, iolaps, iafun, isigfun, idur,\
   				iphs, imode, iskip

;OUTBEG
      outc     asound*kenv
;OUTEND

endin