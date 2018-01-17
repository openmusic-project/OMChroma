;;=============================================================================
;;			FOG-1.ORC
;;	GRANULAR SYNTHESIS (NEW INSTRUMENT) / MONO
;;	AMPLITUDE ENVELOPE WITH POSCIL
;;=============================================================================
;;
;; Timbre:    Granular synthesis with fog module, voice-like tones
;; Synthesis: FOG (Forme d'Onde Granulaire)
;;            POSCIL envelopes
;; Coded:     ms 9/02, 2/09
;;
;; NB:
;;	this instrument works AT BEST with samples whose length is a power of 2
;;	  those which are shorter than the immediately superior power of 2 accepted
;;	  by the GEN01, will produce silence when looking up the unsued portion of the table

; NB1: NEW STRUCTURE FOR THE AMPLITUDES FROM AUGUST 2008!
;    Positive value > 0.0  : linear amplitude (>0.0-1000.0)
;    0.0 or negative value : amplitude in dB (0 = maximum value)

; The apparently arbitrary amplitude range (0-1000, rather than 0-1)
;    avoids printing small values with exponential notation

; Replaced oscili with poscil (precise oscillator), ms 8/08
; Default SR = 96000, recommended precision: 24 bits
;-----------------------------------------------------------------------------
;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4	= maximum amp [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= density of the grains [Hz]
;	p6	= transposition factor [1=original]
;	p7	= bandwidth -> exponential decay [Hz]
;	p8	= audio file [GEN01]
;	p9	= speed of the starting pointer in the file [1=same as original]
;	p10	= amplitude envelope [GEN]
;	p11	= rise time of the grain envelope [sec]
;	p12	= overall duration of the grain [sec]
;	p13	= decay time of the grain envelope [sec]
;	p14 = octaviation index [>= 0.0]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f31	audio file
;	f19	sigmoid rise/decay shape
;_____________________________________________________________________________

;-----------------------------------------------------------------------------
; PARAM DESCRIPTION :
; @PARAM 2 "e-dels" number 0.0 "action time [sec]"
; @PARAM 3 "durs" number 1.0 "duration [sec]"
; @PARAM 4 "amp" number -6.0 "maximum amplitude [linear (0.0,1000.0) or dB (-INF,0.0)]"
; @PARAM 5 "f0" number 100.0 "density of the grains [Hz]"
; @PARAM 6 "freq" number 1.0 "transposition factor [1=original]"
; @PARAM 7 "bw" number 0.0 "bandwidth -> exponential decay [Hz]"
; @PARAM 8 "afil" cr::cs-table 31 "sound file [name, sound, string, pathname or GEN01]"
; @PARAM 9 "spd" number 1.0 "speed of the starting pointer in the file [1=same as original]"
; @PARAM 10 "aenv" cr::cs-table (:cs-table cr::gen07 0 1 4096 1) "amplitude envelope [GEN]"
; @PARAM 11 "win" number 0.01 "rise time of the grain envelope [sec]"
; @PARAM 12 "wdur" number 0.1 "total duration of the grain [sec]"
; @PARAM 13 "wout" number 0.05 "decay time of the grain envelope [sec]"
; @PARAM 14 "oct" number 0.0 "octaviation [>=0.0]"
; 
; COMPULSORY GEN FUNCTIONS :
; @GEN ; sigmoid rise/decay
; @GEN f19 0  8192  19 .5 .5 270 .5
; @GEN ; short grain
; @GEN f31  0  524288  1 "basmba.aiff"  0 0 0
;-----------------------------------------------------------------------------

sr	= 96000
kr	= 96000
ksmps	= 1
nchnls	= 1

;0dbfs = 32767  ; 16 bits
0dbfs = 8388607 ; 24 bits

instr 1 ; -------------------------------------------------------------
idur   	= p3
idurosc	= 1/p3
iamp 	= (p4 > 0.0 ? (p4*0.001*0dbfs) : (ampdbfs(p4)))
idens	= p5
ixpf	= p6
iband	= p7
iafil	= p8
iskip	= p9
iaenv	= p10
iwin	= p11
iwdur	= p12
iwout	= p13
ioct	= p14
imode	= 0

iolaps	= 100000
isigfun	= 19
iphs	= 0
iinit	= 0
;OUTBEG
;OUTEND

; amplitude envelope
amp		poscil	iamp, idurosc, iaenv

i1 = sr/ftlen(iafil)	;scaling to reflect sample rate and table length
a1 		phasor	i1*iskip ;index for speed 

;					   			aspd,
;		   	xamp, xdens, xtrans,     koct, kband, kris,  kdur,  kdec, iolaps, ifna,      
asound	fog	1.0, idens, ixpf, a1, ioct, iband, iwin, iwdur, iwout, iolaps, iafil, \
		isigfun, idur, iphs, imode, iinit
;		ifnb, itotdur

;OUTBEG
	outc     asound*amp
;OUTEND

endin
