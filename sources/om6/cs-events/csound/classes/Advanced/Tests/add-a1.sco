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
 
; audio and vibrato
f1  0   4097   9  1  1  0

; GEN functions **********************************************************

; glissando
f6  0   513   7  0 512 1
f51  0   513   7  0 10 1 492 1 10 0

; score ******************************************************************
;  start  idur   iamp	 ifq	iaenv	fdev	ifenv
i1   0	   3	-6.0	440.0	51	1.0	6
i1   3	   3	-6.0	440.0	51	-1.0	6
i1   6     4    -10.0    440.0   51	1.0	6
i1   6     4    -10.0    440.0   51	0.0	6
i1   6     4    -10.0    440.0   51	-1.0	6
i1   10    3    -6.0    440.0   51      7.0     6
e
