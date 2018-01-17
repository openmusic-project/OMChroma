;=============================================================================
;		SMPL-4.ORC
; SAMPLER STORING A SOUND FILE INTO A DEFERRED TABLE [GEN01] / MONO
; READING SAMPLES FROM THE TABLE / NOT NORMALIZED
; AMPLITUDE ENVELOPE WITH POSCIL3 + COSINE IN-OUT
; THIS SAMPLER WILL START THE FILE AGAIN, IF IT REACHES THE END OF THE TABLE
;   SO BEWARE OF LONG NOTES OR HIGH TRANSPOSITION FACTORS
; SAME AS SMPL-3.ORC, BUT WITH RELATIVE STARTING POINT
;=============================================================================

; Timbre:       Reading from a sound file, with transposition
; Synthesis:    Sampler
; Coded:     	ms 3/09

; NB: NEW STRUCTURE FOR THE AMPLITUDES FROM AUGUST 2008!
;    Positive value > 0.0  : linear amplitude (>0.0-1000.0)
;    0.0 or negative value : amplitude in dB (0 = maximum value)

; The apparently arbitrary amplitude range (0-1000, rather than 0-1)
;         avoids printing small values with exponential notation

; Default SR = 96000, recommended precision: 24 bits
;-----------------------------------------------------------------------------
;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4	= max amplitude [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= transposition factor [1=same freq as original]
;	p6	= sound file [GEN01]
;	p7	= starting point in file [sec]
;	p8	= amp envelope [GEN]
;	p9	= duration of the local attack/decay time [sec]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;	f19	sigmoid rise/decay
;_____________________________________________________________________________

; CLASS: SMPL-4

;  GLOBAL KEYWORDS (default values within parentheses):
;	NUMROWS		: amount of rows (components) in the event (1)
;	ACTION-TIME	: start time of the whole event [sec] (0.0)
;	USER-FUN	: user-defined parsing function (nil)

;  LOCAL KEYWORDS:
;	E-DELS	: entry delays [sec] (0.0)
;	DURS	: duration [sec] (1.0)
;	AMP 	: amplitude [lin, >0.0-1000.0 or dB <- 0.0] (0.0)
;	F0  	: transposition factor [1=same freq as original file] (-1.0)
;	AFIL	: file name [GEN01] (santuri_96.aiff, non normalized!)
;	SKIP	: starting point in the file [%, 0-1] (0.5)
;	AENV	: fun number for the amp envlp [GEN] (straight line=1)
;	WIN 	: duration of the local attack/decay [sec] (0.01)
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
iamp		= iamp/0dbfs ; since sfile is not normalized, reduce amp between 0 and 1
ifile		= p6	; must be a GEN01
ixpf		= p5*(sr/ftlen(ifile)) ; to match the SI of the table with transposition

iskip		= p7	; 0-1
iaenv		= p8	; global amplitude envelope
ixin		= (p9 > idur/2 ? idur/2 : p9) ; local envelope
ixout		= (p9 > idur/2 ? idur/2 : p9)

ixmode		= 1 ; index between 0 and 1
ixoff		= 0 ; index offset
iwrap		= 0 ; do not wrap around if the end of the table is reached

isigfun		= 19	; sigmoid function
;OUTBEG
;OUTEND
 
; local envelope (for the grain)
k1	linen	1,ixin,idur,ixout
k2	tablei  k1,isigfun,ixmode,ixoff,iwrap

; global envelope
k3	poscil	iamp, idurosc, iaenv

; file reading
a1	poscil3 1.0, ixpf, ifile, iskip
asound	= a1*k2*k3

;OUTBEG
	outc     asound
;OUTEND

endin
