;=============================================================================
;			ADD-A1.ORC
; ADDITIVE SYNTHESIS WITH FREQUENCY AND AMPLITUDE ENVELOPES / MONO
; ENVELOPES WITH POSCIL - FREQUENCY MODULATION IN SEMITONES (SYMETRIC)
;=============================================================================

; Timbre:    nothing special
; Synthesis: additive, same units
; Source:    new instrument
; Coded:     ms 0503, 0808

;-----------------------------------------------------------------------------
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
;	p5	= reference frequency [Hz]
;	p6	= amplitude envelope [GEN number]
;	p7	= maximum frequency deviation [Hz]
;	p8	= frequency envelope [GEN number]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;	f1	audio wave
;_____________________________________________________________________________

; CLASS: ADD-A1

;  GLOBAL KEYWORDS (default values within parentheses):
;	NUMROWS		: amount of rows (components) in the event (1)
;	ACTION-TIME	: start time of the whole event [sec] (0.0)
;	USER-FUN	: user-defined parsing function (nil)

;  LOCAL KEYWORDS:
;   E-DELS	: entry delays [sec] (0.0)
;	DURS	: duration [sec] (1.0)
;	AMP 	: amplitude [sec] (-6.0)
;	FREQ	: frequency [Hz] (440.0)
;	AENV	: function number for the amplitude envelope [GEN] (triangle)
;	FDEV	: maximum frequency deviation [semitones] (1.0)
;	FENV	: function number for the frequency envelope [GEN] (triangle)

;*****************************************************************************

sr	= 96000
kr	= 96000
ksmps	= 1
nchnls	= 1

;0dbfs = 32767  ; 16 bits
0dbfs = 8388697 ; 24 bits

instr 1 ; -------------------------------------------------------------

idur		= p3
idurosc		= 1/idur
iamp 		= (p4 > 0.0 ? (p4*0.001*0dbfs) : (ampdbfs(p4)))
ifq			= p5
; use the fractional-octave representation to have a symmetric frequency modulation
ifroct		= octcps(ifq)
iaenv		= p6
iaudiofun	= 1

; 1 semitone = 1/12 of the oct representation
idfq		= p7 / 12.0
ifenv		= p8


; FREQUENCY ENVELOPE (IN SEMITONES)
   kfq1		poscil  idfq, idurosc, ifenv
   kfq		= cpsoct(kfq1 + ifroct)

; AMPLITUDE ENVELOPE
   kamp    poscil  iamp, idurosc, iaenv

; AUDIO MODULE
   a1    poscil  kamp, kfq, iaudiofun
         out     a1
endin
