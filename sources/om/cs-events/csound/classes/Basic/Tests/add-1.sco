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

; GEN functions **********************************************************

; waveform
f1 0 4097 9 1 1 0                ; sinus with amplitude 1, start phase 0

; amplitude envelope
f51  0   513   7  0 256 1  256 0


;score *******************************************************************
; instr 1	idur	iamp 	ifq		iaenv	

i1  0		1.0		500.0	440.0	51
i1  0.5		1.0		500.0	550.0	51
i1  1		1.0		500.0	660.0	51
i1  1.5		1.0		500.0	770.0	51

e
