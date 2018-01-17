;=============================================================================
;			FOF-4.ORC
; FOF-BASED GRANULAR SYNTHESIS / MONO
; SAMPLED GRAIN RATHER THAN A SINE TONE IN THE FOF
; AMPLITUDE ENVELOPE WITH POSCIL
;=============================================================================

; Timbre:    Granular synthesis with a FOF (based on Byrne Villez's work)
; Synthesis: FOF (Forme d'Onde Formatique)
;            POSCIL envelopes
; Coded:     ms 2/09

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
;	p5	= fundamental freq [Hz]
;	p6	= formant freq [Hz]
;	p7	= bandwidth [Hz]
;	p8	= amp envelope [GEN number]
;	p9	= tex or krise [sec]
;	p10	= total duration of the burst, see debatt [sec]
;	p11	= decay of the FOF [sec]
;	p12	= octaviation [>=0.0]
;	p13	= sound file [name, sound, string, pathname or GEN01] (2.0)
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f19	sigmoid rise/decay shape
;	f32	short sample (grain), here 0.64 sec appx.
;_____________________________________________________________________________

; CLASS: FOF-4

;  GLOBAL KEYWORDS (default values within parentheses):
;	NUMROWS		: amount of rows (components) in the event (1)
;	ACTION-TIME	: start time of the whole event [sec] (0.0)
;	USER-FUN	: user-defined parsing function (nil)

;  LOCAL KEYWORDS:
;	E-DELS	: entry delays [sec] (0.0)
;	DURS	: duration [sec] (1.0)
;	AMP 	: amp [lin, >0.0-1000.0 or dB <- 0.0] (-6.0)
;	F0  	: fundamental freq [Hz] (220.0)
;	FREQ	: formant freq [Hz] (5.0)
;	BW  	: bandwidth [Hz] (0)
;	AENV	: fun number for the amp envlp [GEN]
;	WIN 	: excitation (rise) time of the FOF [sec, 0.003]
;	WDUR	: total duration of the FOF [sec, 0.02]
;	WOUT	: decay of the FOF [sec, 0.005]
;	OCT 	: octaviation factor [=>0, 0.0]
;	AFIL	: file name [int, sound, string, pathname or GEN] (tambura_96.aiff)
;*****************************************************************************

sr	= 96000
kr	= 96000
ksmps	= 1
nchnls	= 1

;0dbfs = 32767  ; 16 bits
0dbfs = 8388607 ; 24 bits
;OUTBEG
;OUTEND

instr 1 ; -------------------------------------------------------------
idur   	= p3
idurosc	= 1/p3
iamp 	= (p4 > 0.0 ? (p4*0.001*0dbfs) : (ampdbfs(p4)))
if0		= p5
ifreq	= p6
ibw		= p7
iaenv	= p8
iwxin	= p9
iwxdur	= p10
iwxout	= p11
ioct	= p12
ifile	= p13

iolaps	= 100000	; how many simultaneous FOFs can be played
					; (takes little memory if not used)
imode	= 0
iphs	= 0
iskip	= 0

isigfun	= 19
;OUTBEG
;OUTEND

; amplitude envelope
   kenv		poscil	iamp, idurosc, iaenv

;				xamp  xfund xform oct  kband kris   kdur    kdec   iolaps   ifna   ifnb
asig	fof 	1.0, if0, ifreq, ioct,  ibw, iwxin, iwxdur, iwxout, iolaps, ifile, isigfun, idur, iphs, imode, iskip
asound	= asig*kenv

;OUTBEG
      outc     asound
;OUTEND

endin