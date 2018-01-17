;=============================================================================
;		ADD4.ORC
; SIMPLE ADDITIVE SYNTHESIS ADAPTED TO READ PARTIAL ANALYSIS DATA / MONO
; AMPLITUDE ENVELOPE WITH POSCIL AND ATTACK/RELEASE VALUES TO AVOID CLICKS
; CONTROL OF THE INITIAL PHASE OF THE AUDIO OSCILLATOR
;=============================================================================

; Timbre:    simple additive synthesis with variable amplitude and frequency
; Synthesis: additive same units
; Coded:     ms 17/07

; This class reads absolute values for amplitudes and frequencies
;   coming from analysis data. In this case, set freq to 1.0 (scaler) and amplitude
;   to 1000.0 or 0.0 (max dB). Note that GEN functions should have a negative number
;   in order not to be rescaled (GEN 0 -7)
; It can also be used with normalized amp and freq functions, and, in this case,
;   amp and freq should have a reasonable value.

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
;	p5	= frequency [Hz or scaler]
;	p6	= amplitude envelope [GEN number]
;	p7	= frequency envelope [GEN number]
;	p8	= attack time of the amp envlp [sec]
;	p9	= decay time of the amp envlp [sec]
;	p10 = initial phase of the audio oscillator [rad]

;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;	f1	audio wave
;_____________________________________________________________________________

; CLASS: ADD-4

;  GLOBAL KEYWORDS (default values within parentheses):
;	NUMROWS		: amount of rows (components) in the event (1)
;	ACTION-TIME	: start time of the whole event [sec] (0.0)
;	USER-FUN	: user-defined parsing function (nil)

;  LOCAL KEYWORDS:
;	E-DELS	:	entry delays [sec] (0.0)
;	DURS	:	duration [sec] (1.0)
;	AMP		:	amplitude [lin, >0.0-1000.0 or dB <- 0.0] (1000.0)
;	FREQ	:	frequency scalre [0-1 or Hz] (1.0)
;	AENV	:	fun number for the amp envlp [absolute GEN = negative GEN07] (trapezoid)
;	FENV	:	fun number for frequency env [absolute GEN = negative GEN07] (upward gliss)
;	ATK		:	attack time [sec] (0.01)
;	DEC		:	decay time [sec] (0.01)
;	PHS		:	initial phase [rad] (0.0)
;*****************************************************************************
 
sr	= 96000
kr	= 96000
ksmps	= 1
nchnls	= 1

;0dbfs = 32767  ; 16 bits
0dbfs = 8388607 ; 24 bits
gipi	=	3.141592653589793

instr 1 ; -------------------------------------------------------------
 
idur		= p3
idurosc		= 1/idur
iamp 		= (p4 > 0.0 ? (p4*0.001*0dbfs) : (ampdbfs(p4)))
ifq			= p5
iaenv		= p6
ifenv	 	= p7
irise   	= p8					; steep rise  (10 ms)
idec    	= p9					; steep decay (10 ms)
;idec	= (((irise+idec)>idur) ? idur-irise : idec) ; correct idec if idur is too short
if irise+idec <= idur goto nochange
printf_i "WARNING: irise (%f) + idec (%f) > idur (%f); irise reduced to %f\n", 1, irise, idec, idur, (idur-irise)
idec = idur-irise
nochange:

if idec >= 0.0 goto goon
; very short duration
printf_i "WARNING: idur (%f) is too short, set irise/idec to %f\n", 1, idur, (idur/2)
irise = idur/2
idec = idur/2
goon:

iphs		= p10
;iphs	= ((iphs>0.0) ? iphs/gipi : (gipi-(iphs/gipi))) ; rescale phase between 0 and 1

;print iphs

;OUTBEG
;OUTEND

iaudiofun	= 1
 
   	kfq		poscil  1, idurosc, ifenv ; fq envelope
   	kfq		= (kfq * ifq)
   	kenv	poscil  iamp, idurosc, iaenv
	klin	linen   1, irise, idur, idec

   	asound	poscil  kenv*klin, kfq, iaudiofun, iphs

;OUTBEG
   out     asound
;OUTEND

endin
