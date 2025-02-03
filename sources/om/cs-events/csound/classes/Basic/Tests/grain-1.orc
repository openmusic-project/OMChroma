;=============================================================================
;		GRAIN-1.ORC
; GRANULAR SYNTHESIS / MONO
; BASIC CONTROLS
;=============================================================================

; Timbre:       Granular synthesis textures
; Synthesis:    grain
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
;	p6	= grain frequency [1=same as file]
;	p7	= sound file [GEN01, not normalized]
;	p8	= amp envelope [GEN, straight line (8)]
;	p9	= max amp deviation for max amp [same as amp, additive]
;	p10	= max freq deviation [same as freq]
;	p11	= duration of the grain [sec]
;	p12	= grain randomness [0=off, non 0=on]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;	f20	Hanning window for the grain
;_____________________________________________________________________________

; CLASS: GRAIN-1

;  GLOBAL KEYWORDS (default values within parentheses):
;	NUMROWS		: amount of rows (components) in the event (1)
;	ACTION-TIME	: start time of the whole event [sec] (0.0)
;	USER-FUN	: user-defined parsing function (nil)

;  LOCAL KEYWORDS:
;   E-DELS	: entry delays [sec] (0.0)
;	DURS	: duration [sec] (1.0)
;	AMP		: max amplitude [lin, >0.0-1000.0 or dB <= 0.0] (0.0)
;	F0  	: density of the grain [Hz] (100.0)
;			: if k->synchronous granular synthesis
;			: if random added-> asynchronous granular synthesis
;	FREQ	: grain freq [1=same as file] (1.0)
;	AFIL	: file name [GEN01] (cym_96.aiff)
;	AENV	: amplitude envelope [GEN] (fast up/down)
;	AMPD	: amp deviation for AMP [lin or db, additive] (-10.0)
;			: NB. HERE 0=silence, for max val, set -0.01
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
iamp 	= (p4 > 0.0 ? (p4*0.001) : (10^(p4/20.0)))
	; relative amplitude to the sound file (to avoid multiplying twice)
if0 	= p5 ; density
ifreq	= p6 ; grain frequency
ifile	= p7
ifreq	= ifreq * sr / ftlen (ifile) ; 1=original freq of sound file

iaenv	= p8	; global amplitude envelope
iampd	= (p9 >= 0.0 ? (p9*0.001) : (10^(p9/20.0))) ; amp randomness
ifqd	= p10	; freq randomness
iwdur	= p11	; current grain duration
igrnd	= p12	; grain randomness (0=yes)

imxgdur	= iwdur	; max duration of grain
iwinfun	= 20	; Hanning window
ifade	= 0.050	; automatic fade out at the end of the last grain
;OUTBEG
;OUTEND
 
; global envelope
kamp	poscil	iamp, idurosc, iaenv
kfade	linseg	1.0, idur-ifade, 1.0, ifade, 0.0

; file reading
a1 grain iamp, ifreq, if0, iampd, ifqd, iwdur, ifile, iwinfun, imxgdur, igrnd 
asound = a1*kamp*kfade

;OUTBEG
         outc     asound
;OUTEND

endin
