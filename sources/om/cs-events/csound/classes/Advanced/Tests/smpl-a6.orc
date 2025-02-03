;=============================================================================
;		SMPL-A6.ORC
; SAMPLER READING FROM A DEFERRED TABLE WITH LOOP / MONO
; AMPLITUDE ENVELOPE WITH POSCIL
; FIXED TRANSPOSITION
; DYNAMIC TRANSPOSITION AND AMPLITUDE CONTROL + JITTER
; SAME AS SMPL-a3.ORC, BUT WITH RELATIVE STARTING AND LOOP POINTS
;=============================================================================

; Timbre:       Reading a sound file into a deferred table, with transposition
; Synthesis:    Sampler, flooper2
; Coded:     	ms 3/09

; This instrument will loop through a deferred GEN01 table which will match
;   the exact duration of the sound file.
; If the duration in the score is longer than the file, it will read the file
;   until loop-end, then loop between loop-beg and loop-end until the end
;   of the duration.

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
;	p4	= max amplitude [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= min transposition factor [>0, 1=same as the file, 0.5=oct down, 2=oct up]
;	p6	= max transposition factor
;	p7	= envelope for the transposition factor [GEN]
;	p8	= jitter amplitude for the transposition factor [0-1]
;	p9	= duration of the transposition envelope [sec, if=0 use p3]
;	p10	= jitter amplitude for the max amp [0-1]
;	p11	= sound file [GEN01]
;	p12	= starting point in the table [%]
;	p13	= amplitude envelope [GEN]
;	p14	= initial beginning of loop [[%]]
;	p15	= initial end of loop [[%]]
;	p16	= initial crossfade length [[%]]
;	p17	= final end of loop [[%]]
;	p18	= final beginning of loop [[%]]
;	p19	= final crossfade length [[%]]
;	p20	= loop mode: 0=fwd, 1=bkwd, 2=fwd+bkwd
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;	f19	bell-shaped curve
;_____________________________________________________________________________

; CLASS: SMPL-a5

;  GLOBAL KEYWORDS (default values within parentheses):
;	NUMROWS		: amount of rows (components) in the event (1)
;	ACTION-TIME	: start time of the whole event [sec] (0.0)
;	USER-FUN	: user-defined parsing function (nil)

;  LOCAL KEYWORDS:
;   E-DELS	: entry delays [sec] (0.0)
;	DURS	: duration [sec] (1.0)
;	AMP 	: amplitude [lin, >0.0-1000.0 or dB <- 0.0] (0.0)
;	F0  	: transposition factor [1=same frequency] (1.0)
;	F0MAX  	: max transposition factor (2.0)
;	F0ENV  	: envelope for the transposition factor [GEN] (asc line)
;	F0JTA  	: jitter amp for the transposition factor [0-1] (0.1)
;	F0DUR  	: duration of the transposition envelope [sec] (1.0, if=0, use p3)
;	JTA  	: jitter amplitude for max amp [0-1] (0.5)
;	AFIL	: file name [GEN01] (santuri_96.aiff)
;	SKIP	: starting point in the file [%] (0.0)
;	AENV	: fun number for the amp envlp [GEN] (straight line=1)
;	LPBEG	: initial starting loop point [%] (0.0)
;	LPEND	: initial ending loop point [%] (0.2)
;	WIN 	: initial crossfade length [%] (0.1)
;	LPBEG1	: final starting loop point [%] (0.5)
;	LPEND1	: final ending loop point [%] (0.7)
;	WIN1 	: final crossfade length [%] (0.1)
;	MODE	: loop mode [0=fwd, 1=bkwd, 2=fwd+bkwd] (2)
;*****************************************************************************

sr	= 96000
kr	= 96000
ksmps	= 1
nchnls	= 1

;0dbfs = 32767	; 16 bits
0dbfs = 8388607	; 24 bits

instr 1 ; -------------------------------------------------------------

idur	= p3
idurosc	= 1/idur
iamp 	= (p4 > 0.0 ? (p4*0.001*0dbfs) : (ampdbfs (p4)))
iamp	= iamp/0dbfs ; since sfile is not normalized, reduce amp between 0 and 1

if0min	= p5
if0max	= p6
if0d	= if0max-if0min
if0env	= p7
if0jt	= p8
if0dur	= (p9 = 0 ? p3 : p9) ; duration of xposition env, if 0, take p3

ijta	= p10
ifile	= p11	; must be a GEN01, non normalized
ilen  	= ftlen(ifile)/sr ; length of table [sec]
iskip	= p12*ilen
iaenv	= p13	; global amplitude envelope

ilpbeg	= p14*ilen
ilpend	= p15*ilen
ilpbeg1	= p16*ilen
ilpend1	= p17*ilen
ilpxf	= p18*ilen
ilpxf1	= p19*ilen
ilpmode	= p20

isize	= 1	; 31-bit random number for randi
isigfun	= 19	; sigmoid function
iskp	= 0 ; do not skip initialization
;OUTBEG
;OUTEND

;f0
; jitter for f0
; seed>1.0=> seed from the system time
kf0j1	randi	if0jt, 1/0.061, 1.8135, isize
kf0j2	randi	if0jt, 1/0.103, 1.3111, isize
kf0j3	randi	if0jt, 1/1.2009, 1.6711, isize
kf0j	=	(kf0j1+kf0j2+kf0j3)/3.0

;f0 envelope
kf0env	poscil	if0d, 1/if0dur, if0env
kf0		= kf0env+if0min
kf0end	= kf0+(kf0*kf0j)

; jitter for amp
; seed>1.0=> seed from the system time
kjta1	randi	ijta, 1/0.061, 1.8195, isize
kjta2	randi	ijta, 1/0.109, 1.3011, isize
kjta3	randi	ijta, 1/1.221, 1.6793, isize
kjta	=	(kjta1+kjta2+kjta3)/3.0

; global envelope
k3	poscil	iamp, idurosc, iaenv
kamp	= k3+(k3*kjta)

; initial/final loops
klpbeg 	line	ilpbeg, idur, ilpbeg1
klpend 	line	ilpend, idur, ilpend1
klpxf 	line	ilpxf, idur, ilpxf1

a1	flooper2	1.0, kf0end, klpbeg, klpend, klpxf, ifile, iskip, ilpmode, isigfun, iskp

asound	= a1*k3

;OUTBEG
out     asound
;OUTEND

endin