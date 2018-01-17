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

; amplitude envelope
f7 0 513 7 0  256 1 256 0


; score ******************************************************************

;    atime idur  iamp   ipch   ibw	iaenv
i1    0     1     200  2093   .001	7
i1    1.1   .      .   1046   .	 .
i1    2.3   8      .    523   .	 .
i1    3     2     100  1567   .	 .
i1    3     2     125   261   .	 .
i1    3     2    87.5  5273   .	 .
i1    6     .3    100 25086   .	 .
i1    6     .     125  8372   .	 .
i1    6     .    87.5 10547   .	 .
i1    6.3   .4    100  1567   .	 .
i1    6.3   .     125  1046   .	 .
i1    6.3   .    87.5  2636   .	 .
i1    8.1   1     250  2636   .	 .
e
