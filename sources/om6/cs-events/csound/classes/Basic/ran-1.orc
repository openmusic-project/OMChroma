;=============================================================================
;			RAN-1.ORC
; RANDOM NUMBER GENERATION MODULATING THE AMPLITUDE OF AN OSCILLATOR
;    (FROM ACCCI, 10_02_2.ORC) / MONO
; AMPLITUDE ENVELOPE WITH LINEN, CONTROL OF AUDIO FUN FROM THE SCORE
;=============================================================================

; Timbre:       Noise spectra, with control of bandwidth and center freq
; Synthesis:    Random Number Generation
;               RANDI(02)
;               LINEN envelope on RANDI ring modulates an oscillator (2)
; Source:       Dodge(1985), p.92
; Coded:        jpg 8/92. modified ms 9/02, 8/08

; NB: NEW STRUCTURE FOR THE AMPLITUDES FROM AUGUST 2008!
;    Positive value > 0.0  : linear amplitude (>0.0-1000.0)
;    0.0 or negative value : amplitude in dB (0 = maximum value)


; The apparently arbitrary amplitude range (0-1000, rather than 0-1)
;         avoids printing small values with exponential notation
; Replaced oscili with poscil (precise oscillator), ms 8/08
; Default SR = 96000, recommended precision: 24 bits

; NB 1: this instrument works better with short notes.
;-----------------------------------------------------------------------------
;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4	= maximum amp [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= freq [Hz]
;	p6	= freq of the noise module [Hz]
;	p7	= attack time of the amp envlp [sec]
;	p8	= decay time of the amp envlp [sec]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f1	sine tone
;_____________________________________________________________________________

; CLASS: RAN-1

;  GLOBAL KEYWORDS (default values within parentheses):
;	NUMROWS		: amount of rows (components) in the event (1)
;	ACTION-TIME	: start time of the whole event [sec] (0.0)
;	USER-FUN	: user-defined parsing function (nil)

;  LOCAL KEYWORDS:
;	E-DELS	: entry delays [sec] (0.0)
;	DURS	: duration [sec] (1.0)
;	AMP 	: amp [lin, >0.0-1000.0 or dB <- 0.0] (-6.0)
;	FREQ	: freq [Hz] (440.0)
;	JTF 	: freq of the random source [Hz] (50.0)
;	ATK 	: attack time [sec] (0.2)
;	DEC 	: decay time [sec] (0.3)
;*****************************************************************************

sr	= 96000
kr	= 96000
ksmps	= 1
nchnls	= 1

;0dbfs = 32767  ; 16 bits
0dbfs = 8388607 ; 24 bits

instr 1 ; -------------------------------------------------------------

idur		= p3
iamp 	= (p4 > 0.0 ? (p4*0.001*0dbfs) : (ampdbfs(p4)))
ifq		= p5
iran_fq	= p6
imindec	= 0.01 ; minimum allowed decay
iatk	= p7
idec	= p8
idec	= (((iatk+idec)>idur) ? idur-iatk : idec)

if idec > imindec goto goon
; very short duration
iatk = idur/2
idec = idur/2
goon:
iaudiofun	= 1

;OUTBEG
;OUTEND

   kenv  	linen    iamp, iatk, idur, idec		; envelope
   kran  	randi    kenv, iran_fq				; random numbers
   asound	poscil   kran, ifq, iaudiofun		; applied to amplitude slot

;OUTBEG
         out      asound
;OUTEND

endin

