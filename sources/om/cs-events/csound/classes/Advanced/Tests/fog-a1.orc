;=============================================================================
;			FOG-A1.ORC
;	GRANULAR SYNTHESIS (NEW INSTRUMENT) / MONO
;	AMPLITUDE ENVELOPE WITH POSCIL, COMPLETE CONTROL OF THE PARAMETRES
;=============================================================================

; Timbre:    Granular synthesis with fog module, voice-like tones
; Synthesis: FOG (Forme d'Onde Granulaire)
;            POSCIL envelopes
; Coded:     ms 2/09

; NB:
; this instrument works AT BEST with samples whose length is a power of 2
;	those which are shorter than the immediately superior power of 2 accepted
;	by the GEN01, will produce silence when looking up the unsued portion of the table

; NB1: NEW STRUCTURE FOR THE AMPLITUDES FROM AUGUST 2008!
;	Positive value > 0.0  : linear amplitude (>0.0-1000.0)
;	0.0 or negative value : amplitude in dB (0 = maximum value)

; The apparently arbitrary amplitude range (0-1000, rather than 0-1)
;	avoids printing small values with exponential notation

; Replaced oscili with poscil (precise oscillator), ms 8/08
; Default SR = 96000, recommended precision: 24 bits
;-----------------------------------------------------------------------------
;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4	= max amp [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= min density of the grains [Hz]
;	p6	= max density of the grains [Hz]
;	p7	= envelope for the density of the grains [GEN]
;	p8	= jitter amp of density [0-1]
;	p9	= min transposition factor [1=original]
;	p10	= max transposition factor [1=original]
;	p11	= envelope for the transposition factor [GEN]
;	p12	= jitter amp of transposition factor [0-1]
;	p13	= audio file [GEN01]
;	p14	= min speed of the starting pointer in the file [1=same as original]
;	p15	= max speed of the starting pointer in the file [1=same as original]
;	p16	= envelope for the speed of the starting pointer in the file [GEN]
;	p17	= min bandwidth (-> exponential decay) [Hz]
;	p18	= max bandwidth (-> exponential decay) [Hz]
;	p19	= envelope for the bandwidth -> exponential decay [GEN]
;	p20	= amplitude envelope [GEN]
;	p21	= min rise time of the grain envelope [sec]
;	p22	= max rise time of the grain envelope [sec]
;	p23	= envelope for the rise time of the grain envelope [GEN]
;	p24	= min overall duration of the grain [sec]
;	p25	= max overall duration of the grain [sec]
;	p26	= envelope for the overall duration of the grain [GEN]
;	p27	= min decay time of the grain envelope [sec]
;	p28	= max decay time of the grain envelope [sec]
;	p29	= envelope for the decay time of the grain envelope [GEN]
;	p30 = min octaviation index [>= 0.0]
;	p31 = max octaviation index [>= 0.0]
;	p32 = envelope for the octaviation index [GEN]
;	p33 = transposition mode [if=0.0, no glissando within each grain]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f31	audio file
;	f19	sigmoid rise/decay shape
;_____________________________________________________________________________

; CLASS: FOG-1

;  GLOBAL KEYWORDS (default values within parentheses):
;	NUMROWS		: amount of rows (components) in the event (1)
;	ACTION-TIME	: start time of the whole event [sec] (0.0)
;	USER-FUN	: user-defined parsing function (nil)

;  LOCAL KEYWORDS:
;	E-DELS	: entry delays [sec] (0.0)
;	DURS	: duration [sec] (1.0)
;	AMP 	: amplitude [lin, >0.0-1000.0 or dB <- 0.0] (-6.0)
;	F0  	: min density of the grains [Hz] (10.0)
;	F0MAX  	: max density of the grains [Hz] (20.0)
;	F0ENV  	: envelope for the density of the grains [GEN] (asc line)
;	F0JTA  	: jitter's amp of the grains' density [0-1] (0.06)
;	FREQ 	: min transposition factor [1=original) (1.0)
;	FQMAX 	: max transposition factor [1=original) (2.0)
;	FQENV 	: envelope for the transposition factor [GEN] (asc line)
;	FQJTA 	: jitter's amp for the transposition factor [0-1] (0.1)
;	AFIL	: audio file [GEN01] (basmba_96.aiff)
;	SPD 	: min speed of the starting point in the sound [1=same as original] (1.0)
;	SPDMAX 	: max speed of the starting point in the sound [1=same as original] (2.0)
;	SPDENV 	: envelope for the speed of the starting point in the sound [GEN] (asc line)
;	BW  	: min bandwidth (-> exponential decay) [Hz] (0)
;	BWMAX  	: max bandwidth (-> exponential decay) [Hz] (0)
;	BWENV  	: bandwidth's envelope [GEN]  (asc line)
;	AENV	: amplitude envelope [GEN] (all 1)
;	WIN 	: min rise time of the grain envelope [sec] (0.05)
;	WINMAX 	: max rise time of the grain envelope [sec] (0.01)
;	WINENV 	: envelope for the rise time of the grain [GEN] (asc line)
;	WDUR	: min duration of the grain [sec] (0.2)
;	WDURMAX	: max duration of the grain [sec] (0.05)
;	WDURENV	: envelope for the duration of the grain [GEN] (asc line)
;	WOUT	: min decay time of the grain envelope [sec] (0.1)
;	WOUTMAX	: max decay time of the grain envelope [sec] (0.05)
;	WOUTENV	: envelope for the decay time of the grain envelope [GEN] (asc line)
;	OCT 	: min octaviation factor [=>0] (0.0)
;	OCTMAX 	: max octaviation factor [=>0] (0.0)
;	OCTENV 	: envelope for the octaviation factor [GEN] (asc line)
;	MODE	: transposition mode [if=0, no gliss. within each grain] (0)
;*****************************************************************************

sr	= 96000
kr	= 96000
ksmps	= 1
nchnls	= 1

;0dbfs = 32767  ; 16 bits
0dbfs = 8388607 ; 24 bits

instr 1 ; -------------------------------------------------------------
idur   	= p3
idurosc	= 1/p3
iamp 	= (p4 > 0.0 ? (p4*0.001*0dbfs) : (ampdbfs (p4)))
idensmin	= p5
idensmax	= p6
idensd  	= idensmax-idensmin
idensenv	= p7
idensjt		= p8

ixpfmin	= p9
ixpfmax	= p10
ixpfd	= ixpfmax-ixpfmin
ixpfenv	= p11
ixpfjt	= p12

iafil	= p13

iskipmin	= p14
iskipmax	= p15
iskipd  	= iskipmax-iskipmin
iskipenv	= p16

ibandmin	= p17
ibandmax	= p18
ibandd  	= ibandmax-ibandmin
ibandenv	= p19

iaenv	= p20

iwinmin	= p21
iwinmax	= p22
iwind	= iwinmax-iwinmin
iwinenv	= p23

iwdurmin	= p24
iwdurmax	= p25
iwdurd  	= iwdurmax-iwdurmin
iwdurenv	= p26

iwoutmin	= p27
iwoutmax	= p28
iwoutd  	= iwoutmax-iwoutmin
iwoutenv	= p29

ioctmin	= p30
ioctmax	= p31
ioctd	= ioctmax-ioctmin
ioctenv	= p32

imode	= p33

iolaps	= 100000
isigfun	= 19
iphs	= 0
iinit	= 0
isize	= 1		; 31-bit random numbers for randi
;OUTBEG
;OUTEND

; jitter for density
; seed>1.0=> seed from the system time
kdensj1	randi	idensjt, 1/0.05, 1.8135, isize
kdensj2	randi	idensjt, 1/0.111, 1.3111, isize
kdensj3	randi	idensjt, 1/1.219, 1.6711, isize
kdensj	=	(kdensj1+kdensj2+kdensj3)/3.0

; density envelope (f0)
kdens		poscil	idensd, idurosc, idensenv
kdens	=	kdens+idensmin
kdensend	= kdens+(kdens*kdensj)

; transposing frequency (freq)
; jitter for xposing fq
kxpfj1	randi	ixpfjt, 1/0.053, 1.5318, isize
kxpfj2	randi	ixpfjt, 1/0.112, 1.1113, isize
kxpfj3	randi	ixpfjt, 1/1.215, 1.7166, isize
kxpfj 	=	(kxpfj1+kxpfj2+kxpfj3)/3.0

; xping freq envelope
kxpfenv		poscil	ixpfd, idurosc, ixpfenv
kxpf		= kxpfenv+ixpfmin
kxpfend	= kxpf+(kxpf*kxpfj)

; starting pointer (skip)
kskip		poscil	iskipd, idurosc, iskipenv
kskip	=	kskip+iskipmin

; bandwidth (band)
kband		poscil	ibandd, idurosc, ibandenv
kband	=	kband+ibandmin

; rise time (win)
kwin		poscil	iwind, idurosc, iwinenv
kwin	=	kwin+iwinmin

; grain duration (wdur)
kwdur		poscil	iwdurd, idurosc, iwdurenv
kwdur	=	kwdur+iwdurmin

; decay (wout)
kwout		poscil	iwoutd, idurosc, iwoutenv
kwout	=	kwout+iwoutmin

; octaviation (oct)
koct		poscil	ioctd, idurosc, ioctenv
koct	=	koct+ioctmin

; amplitude envelope
kamp		poscil	iamp, idurosc, iaenv

i1 = sr/ftlen(iafil)	;scaling to reflect sample rate and table length
a1 		phasor	i1*kskip ;index for speed 

;					   			aspd,
;		   	xamp, xdens, xtrans,     koct, kband, kris,  kdur,  kdec, iolaps, ifna,      
asound	fog	1.0, kdens, kxpf, a1, koct, kband, kwin, kwdur, kwout, iolaps, iafil, \
		isigfun, idur, iphs, imode, iinit
;		ifnb, itotdur

;OUTBEG
	outc     asound*kamp
;OUTEND

endin
