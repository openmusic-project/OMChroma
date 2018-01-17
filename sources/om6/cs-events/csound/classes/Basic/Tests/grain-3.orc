;=============================================================================
;		GRAIN-3.ORC
; GRANULAR SYNTHESIS / MONO
; SEVERAL STATIC USER CONTROLS
;=============================================================================

; Timbre:       Granular synthesis textures
; Synthesis:    grain3
; Coded:     	ms 3/09

; NB: NEW STRUCTURE FOR THE AMPLITUDES FROM AUGUST 2008!
;    Positive value > 0.0  : linear amplitude (>0.0-1000.0)
;    0.0 or negative value : amplitude in dB (0 = maximum value)

; The apparently arbitrary amplitude range (0-1000, rather than 0-1)
;     avoids printing small values with exponential notation
; Default SR = 96000, recommended precision: 24 bits

;-----------------------------------------------------------------------------
;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4	= max amp [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= density of the grains [Hz]
;	p6	= grain freq [Hz]
;	p7	= bipolar random variation in grain freq [Hz]
;	p8	= distribution of the grain freq variation [-1/1]
;	p9	= sound file [GEN01, not normalized]
;	p10	= amp envelope [GEN, straight line (8)]
;	p11	= grain starting phase [0-1]
;	p12	= bipolar random variation in grain phase [0-1]
;	p13	= distribution of the grain phase variation [-1/1]
;	p14	= duration of the grain [sec]
;	p15	= mode [int] (use 88=freq not or 90=freq modified within each grain)
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;	f20	Hanning window for the grain
;_____________________________________________________________________________

; CLASS: GRAIN-3

;  GLOBAL KEYWORDS (default values within parentheses):
;	NUMROWS		: amount of rows (components) in the event (1)
;	ACTION-TIME	: start time of the whole event [sec] (0.0)
;	USER-FUN	: user-defined parsing function (nil)

;  LOCAL KEYWORDS:
;   E-DELS	: entry delays [sec] (0.0)
;	DURS	: duration [sec] (1.0)
;	AMP		: max amplitude [lin, >0.0-1000.0 or dB <- 0.0] (0.0)
;	F0  	: density of the grain [Hz] (100.0)
;			: if k->synchronous granular synthesis
;			: if random added-> asynchronous granular synthesis
;	FREQ	: grain freq [1=same as file] (1.0)
;	AFIL	: file name [GEN01] (cym_96.aiff)
;	AENV	: amplitude envelope [GEN] (fast up/down)
;	AMPD	: amp deviation for AMP [same as AMP, additive] (-10.0)
;			: global amp for each grain = AMP <= gblamp => AMP+AMPD
;	FQD 	: freq deviation for FREQ [same as FREQ, additive] (0.5)
;			: global freq for each grain = FREQ <= gblfq => FREQ+FQD
;	WDUR 	: duration of the grain [sec] (0.1)
;	GRND	: grain offset randomness [0=off, <>0=on] (0)
;			: if on, all grains start reading from random positions in AFIL
;			: if off, all grains start reading from the beginning of AFIL
;*****************************************************************************

sr	= 96000
kr	= 96000
ksmps	= 1
nchnls	= 1

;0dbfs = 32767  ; 16 bits
0dbfs = 8388607 ; max amp of the sound file

instr 1 ; -------------------------------------------------------------
 
idur	= p3
idurosc	= 1/idur
iamp 	= (p4 > 0.0 ? (p4*0.001) : (10 ^ (p4/20.0)))
;iamp	= iamp/0dbfs
	; amplitude relative to the sound file (to avoid multiplying twice)
if0 	= p5 ; density
ifreq	= p6 ; grain frequency
ifqd	= p7
ifqdist	= p8
ifile	= p9
ifreq	= ifreq * sr / ftlen (ifile) ; 1=original freq of sound file

iaenv	= p10	; global amplitude envelope
iph 	= p11
iphd	= p12
iphdist	= p13
iwdur	= p14	; current grain duration
imode	= 88	; synchronize ph start to freq+do not render if AT<0

iwinfun	= 20	; Hanning window
iovlp	= 1000000
iseed	= 0		; seed from current time
ifade	= 0.050	; automatic fade out for the last grain
;OUTBEG
;OUTEND
 
; global envelope
kamp	poscil	iamp, idurosc, iaenv
kfade	linseg	1.0, idur-ifade, 1.0, ifade, 0.0

; file reading
a1 grain3 ifreq, iph, ifqd, iphd, iwdur, if0, iovlp, ifile, iwinfun, ifqdist, iphdist, iseed, imode 
asound = a1*kamp*kfade

;OUTBEG
         outc     asound
;OUTEND

endin
