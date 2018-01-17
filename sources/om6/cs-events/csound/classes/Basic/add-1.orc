;=============================================================================
;			ADD-1.ORC
; SIMPLE ADDITIVE SYNTHESIS (FROM ACCCI, 02_01_3.ORC) / MONO
; AMPLITUDE ENVELOPE WITH OSCILI
;=============================================================================

; Timbre:    gong
; Synthesis: additive same units (02)
;            basic instrument( 01)
; Source:    #420, Gong-like Sounds, Risset(1969)
; Coded:     jpg 8/93, modified ms 9/02, 8/08

; NB: NEW STRUCTURE FOR THE AMPLITUDES FROM AUGUST 2008!
;    Positive value > 0.0  : linear amplitude (>0.0-1000.0)
;    0.0 or negative value : amplitude in dB (0 = maximum value)

; The apparently arbitrary amplitude range (0-1000, rather than 0-1)
;         avoids Lisp printing small values with exponential notation

; Replaced oscili with poscil (precise oscillator), ms 8/08
; Default SR = 96000, recommended precision: 24 bits
;-----------------------------------------------------------------------------

;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4	= maximum amplitude [linear, 0.0 -> 1000.0]
;	p5	= frequency [Hz]
;	p6	= amplitude envelope [GEN number]

;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;	f1	audio wave
;_____________________________________________________________________________

; CLASS: ADD-1

;  GLOBAL KEYWORDS (default values within parentheses):
;	NUMROWS		: amount of rows (components) in the event (1)
;	ACTION-TIME	: start time of the whole event [sec] (0.0)
;	USER-FUN	: user-defined parsing function (nil)

;  LOCAL KEYWORDS:
;   	E-DELS	:	entry delays [sec] (0.0)
;	DURS	:	duration [sec] (1.0)
;	AMP	:	amplitude [lin, >0.0-1000.0 or dB <- 0.0] (-6.0)
;	FREQ	:	frequency [Hz] (440.0)
;	AENV	:	function number for the amplitude envelope [GEN] (triangle)

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
ifq		= p5
iaenv		= p6
iaudiofun	= 1

   a2    poscil  iamp, idurosc, iaenv
   a1    poscil  a2, ifq, iaudiofun
         out     a1
endin
