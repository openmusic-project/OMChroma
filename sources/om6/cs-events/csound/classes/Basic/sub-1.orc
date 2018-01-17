;=============================================================================
;			SUB-1.ORC
; SUBTRACTIVE SYNTHESIS OF RANDOM NOISE (FROM ACCCI, 50_01_1.ORC) / MONO
; AMPLITUDE ENVELOPE WITH OSCILI
;=============================================================================

; Timbre:    Bands of noise
; Synthesis: Subtractive synthesis
;            Basic design
;            RAND source
; Coded:     jpg 8/92, modified ms 9/02, 8/08

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
;	p5	= centre freq [Hz]
;	p6	= bandwidth [% of centre freq, 0->1]
;	p7	= amp envelope [GEN]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f7	triangle function for amplitude envelope
;_____________________________________________________________________________

; CLASS: SUB-1

;  GLOBAL KEYWORDS (default values within parentheses):
;	NUMROWS		: amount of rows (components) in the event (1)
;	ACTION-TIME	: start time of the whole event [sec] (0.0)
;	USER-FUN	: user-defined parsing function (nil)

;  LOCAL KEYWORDS:
;	E-DELS	: entry delays [sec] (0.0)
;	DURS	: duration [sec] (1.0)
;	AMP 	: amp [lin, >0.0-1000.0 or dB <- 0.0] (-6.0)
;	FREQ	: centre freq [Hz] (2000.0)
;	BW  	: bandwidth [% of FQ, 0->1] (0.05)
;	AENV	: amplitude envelope [GEN] (triangle)
;*****************************************************************************

sr	= 96000
kr	= 96000
ksmps	= 1
nchnls	= 1

;0dbfs = 32767  ; 16 bits
0dbfs = 8388607 ; 24 bits

instr 1 ; --------------------------------------------------------------------
idur	= p3
idurosc	= 1.0/idur
iamp 	= (p4 > 0.0 ? (p4*0.001*0dbfs) : (ampdbfs(p4)))
icfq	= p5
ibw		= p6*icfq
iaenv	= p7
iskip	= 2
;OUTBEG
;OUTEND

 anoise  rand   iamp                      	; white noise
   a1      reson  anoise,  icfq, ibw, iskip	; filter, iskip = 2
   a2      poscil 1.0, idurosc, iaenv
   asound	=    a1*a2

;OUTBEG
	out    asound
;OUTEND

endin
