;=============================================================================
;			BUZZ-1.ORC
; DYNAMIC SPECTRUM OSCILLATOR (FROM ACCCI, 43_21_1.ORC) / MONO
; AMPLITUDE ENVELOPE WITH POSCIL
;=============================================================================

; Timbre:    Various controlled noise spectra
; Synthesis: (g)buzz
;            POSCILI envelopes
; Coded:     jpg 8/92, modified ms 9/02, 8/08

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
;	p4	= max amp [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= fundamental freq [Hz]
;	p6	= amp envelope [GEN]
;	p7	= lowest harmonic in the buzz [int]
;	p8	= % of maximum possible harmonic [0-1]
;	p9	= multiplier in the series of amp coeff [0-1]
;	p10	= envelope for the multiplier [GEN]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f5	large cosine
;_____________________________________________________________________________

; GEN functions **********************************************************
; single large cosine wave for the buzz
f5 0 16777216 11 1 1

f11 0 513 7 0 256 1 256 0

; score ******************************************************************

;        idur   iamp   ifqc	iaenv	ilh	in%	ibzmul	ibzenv
i1    0  2      250    220	 11	1	 0.3	0.5	11
i1    +  .      250    220	 11	1	 0.3	0.4	11
i1    +  .      250    220	 11	1	 0.3	0.3	11
i1    +  .      250    220	 11	1	 0.3	0.2	11
i1    +  .      250    220	 11	1	 0.3	0.1	11
i1    +  .      250    220	 11	1	 0.3	0.5	11
i1    +  .      250    220	 11	1	 0.3	0.6	11
i1    +  .      250    220	 11	1	 0.3	0.7	11
i1    +  .      250    220	 11	1	 0.3	0.8	11
i1    +  .      250    220	 11	1	 0.3	0.9	11
i1    +  .      250    220	 11	1	 0.3	0.95	11
i1    +  .      250    220	 11	1	 0.1	0.5	11
i1    +  .      250    220	 11	1	 0.4	0.5	11
i1    +  .      250    220	 11	1	 0.8	0.5	11
i1    +  .      250    220	 11	1	 0.1	0.5	11
i1    +  .      250    220	 11	1	 0.2	0.95	11
i1    +  .      250    220	 11	1	 0.4	0.95	11
i1    +  .      250    220	 11	1	 0.8	0.95	11
i1    +  .      250    220	 11	1	 1	0.95	11
i1    +  .      250    220	 11	1	 1	0.5	11
i1    +   1       .      .	  .	2	 .	0.95	.
i1    +   2       .      .	  .	4	 .	 .	.
i1    .   4       .    	 .	  .	10	 .	 .	.

e
