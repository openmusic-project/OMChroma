;=============================================================================
;		GRAIN-a1.ORC
; GRANULAR SYNTHESIS / MONO
; DYNAMIC CONTROLS
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
;	p5	= min density of the grains [Hz]
;	p6	= max density of the grains [Hz]
;	p7	= envelope for the density of the grains [GEN]
;	p8	= jitter amp for the density of the grains [0-1]
;	p9	= jitter max freq for the density of the grains [Hz]
;	p10	= min grain freq [1=same as file]
;	p11	= max grain freq [1=same as file]
;	p12	= envelope for the grain frequency [GEN]
;	p13	= jitter amp for the grain frequency [0-1]
;	p14	= jitter max freq for the grain frequency [Hz]
;	p15	= sound file [GEN01, not normalized]
;	p16	= amp envelope [GEN, straight line (8)]
;	p17	= min amp deviation for max amp [same as amp, additive]
;	p18	= max amp deviation for max amp [same as amp, additive]
;	p19	= envelope for the amp deviation for max amp [GEN]
;	p20	= min freq deviation [same as freq]
;	p21	= max freq deviation [same as freq]
;	p22	= envelope for the freq deviation [GEN]
;	p23	= min duration of the grain [sec]
;	p24	= max duration of the grain [sec]
;	p25	= envelope for the duration of the grain [GEN]
;	p26	= grain randomness [0=off, non 0=on]

;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;	f20	Hanning window for the grain
;_____________________________________________________________________________

; CLASS: GRAIN-A1

;  GLOBAL KEYWORDS (default values within parentheses):
;	NUMROWS		: amount of rows (components) in the event (1)
;	ACTION-TIME	: start time of the whole event [sec] (0.0)
;	USER-FUN	: user-defined parsing function (nil)

;  LOCAL KEYWORDS:
;   E-DELS	: entry delays [sec] (0.0)
;	DURS	: duration [sec] (1.0)
;	AMP		: max amplitude [lin, >0.0-1000.0 or dB <- 0.0] (-6.0)
;	F0  	: min density of the grain [Hz] (100.0)
;			: if k->synchronous granular synthesis
;			: if random added-> asynchronous granular synthesis
;	F0MAX  	: max density of the grain [Hz] (10.0)
;	F0ENV  	: envelope for the grain's density [GEN] (asc line)
;	F0JTA  	: jitter amp for the grain's density [0-1] (0.1)
;	F0JTF 	: jitter's max freq for the grain's density [Hz] (100.0)
;			: the actual jitter combines 3 freqs, appx 1/10 and 1/20 of F0JTF
;	FREQ	: min grain freq [1=same as file] (1.0)
;	FQMAX	: max grain freq [1=same as file] (2.0)
;	FQENV 	: envelope for the grain freq [GEN] (asc line)
;	FQJTA 	: jitter's amp for the grain freq [0-1] (0.1)
;	F0JTF 	: jitter's max frequency for the grain's density [Hz] (100.0)
;			: the actual jitter combines 3 freqs, appx 1/10 and 1/20 of FQJTF
;	AFIL	: file name [GEN01] (cym_96.aiff)
;	AENV	: amplitude envelope [GEN] (fast up/down)
;	AMPD	: amp deviation for AMP [lin or db, additive] (-10.0)
;			: NB. HERE 0=silence, for max val, set -0.01
;			: global amp for each grain = AMP <= gblamp => AMP+AMPD
;	AMPDMAX	: max amp deviation for AMP [same as AMP, additive] (-10.0)
;	AMPDENV	: envelope for the amp deviation [GEN] (asc line)
;	FQD 	: min freq deviation for FREQ [same as FREQ, additive] (0.0)
;			: global freq for each grain = FREQ <= gblfq => FREQ+FQD
;	FQDMAX 	: max freq deviation for FREQ [same as FREQ, additive] (0.5)
;	FQDENV 	: envelope for the freq deviation [GEN] (asc line)
;	WDUR 	: duration of the grain [sec] (0.1)
;	WDURMAX	: duration of the grain [sec] (0.1)
;	WDURENV	: duration of the grain [sec] (0.1)
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
	; relative amplitude to the sound file (to avoid multiplying twice)

ifile	= p15
iscal	= (sr / ftlen (ifile))	; 1=original freq of sound file

if0min 	= p5 ; density
if0max 	= p6
if0dev	= if0max-if0min
if0env 	= p7
if0jta 	= p8
if0jtf 	= p9

ifqmin	= p10*iscal	; grain frequency
ifqmax	= p11*iscal
ifqdev	= ifqmax-ifqmin
ifqenv	= p12
ifqjta	= p13
ifqjtf	= p14

ifile	= p15
iaenv	= p16	; global amplitude envelope

iampdmin	= (p17 >= 0.0 ? (p17*0.001) : (10 ^ (p17/20.0))) ; amp randomness
iampdmax	= (p18 >= 0.0 ? (p18*0.001) : (10 ^ (p18/20.0)))
iampddev	= iampdmax-iampdmin
iampdenv	= p19

ifqdmin	= p20	; freq randonmess
ifqdmax	= p21
ifqddev	= ifqdmax-ifqdmin
ifqdenv	= p22

iwdurmin	= p23	; current grain duration
iwdurmax	= p24
iwdurdev	= iwdurmax-iwdurmin
iwdurenv	= p25

igrnd	= p26	; grain randomness (0=yes)

imxgdur	= idur	; max duration of grain
iwinfun	= 20	; Hanning window
isize	= 1	; 31-bit random number for randi
ifade	= 0.050	; automatic fade out at the end of the last grain
;OUTBEG
;OUTEND

;f0
; jitter for f0
; seed>1.0=> seed from the system time
; combine 3 frequencies, same weight
kf0j1	randi	if0jta, if0jtf, 1.8135, isize
kf0j2	randi	if0jta, if0jtf*0.109364, 1.3111, isize
kf0j3	randi	if0jta, if0jtf*0.211329, 1.6711, isize
kf0j	=	(kf0j1+kf0j2+kf0j3)/3.0

;f0 envelope
kf0env	poscil	if0dev, idurosc, if0env
kf0		= kf0env+if0min
kf0end	= kf0+(kf0*kf0j)

;fq
; jitter for fq
; seed>1.0=> seed from the system time
; combine 3 frequencies, same weight
kfqj1	randi	ifqjta, ifqjtf, 1.8135, isize
kfqj2	randi	ifqjta, ifqjtf*0.111064, 1.3111, isize
kfqj3	randi	ifqjta, ifqjtf*0.207321, 1.6711, isize
kfqj	=	(kfqj1+kfqj2+kfqj3)/3.0

;fq envelope
kfqenv	poscil	ifqdev, idurosc, ifqenv
kfq		= kfqenv+ifqmin
kfqend	= kfq+(kfq*kfqj)


;iampd envelope
kampdenv	poscil	iampddev, idurosc, iampdenv
kampdend	= kampdenv+iampdmin

;ifqd envelope
kfqdenv	poscil	ifqddev, idurosc, ifqdenv
kfqdend	= kfqdenv+ifqdmin

;iwdurb envelope
kwdurenv	poscil	iwdurdev, idurosc, iwdurenv
kwdurend	= kwdurenv+iwdurmin


; global envelope
kamp	poscil	iamp, idurosc, iaenv
kfade	linseg	1.0, idur-ifade, 1.0, ifade, 0.0

; file reading
a1 grain kamp, kfqend, kf0end, kampdend, kfqdend, kwdurend, ifile, iwinfun, imxgdur, igrnd 
asound = a1*kamp*kfade

;OUTBEG
         outc     asound
;OUTEND

endin
