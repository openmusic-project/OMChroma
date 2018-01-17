;=============================================================================
;			BZFL-1.ORC
; GBUZZ GENERATOR FILTERED BY A FOF FILTER / MONO
; AMPLITUDE ENVELOPE WITH POSCIL
;=============================================================================

; Timbre:    Formantic subtractive synthesis with buzz
; Synthesis: (g)buzz with fofilter
;            POSCIL envelopes
; Coded:     ms 8/08

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
;	p5	= fundamental freq [Hz]
;	p6	= centre freq of the filter [Hz]
;	p7	= amp envelope [GEN number]
;	p8	= lowest harmonic in the buzz [int]
;	p9	= % of highest harmonic [0-1]
;	p10	= multiplier in the series of amp coeffs [0-1]
;	p11	= envlp for the multiplier in the series of amp coeffs [GEN]
;	p12	= impulse response attack time (krise) [sec]
;	p13	= impulse respons decay time (atten) [sec]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f5	large cosine
;_____________________________________________________________________________

; GEN functions **********************************************************
; single large cosine wave for the buzz
f5 0 16777216 11 1 1

f11 0 513 7 0 256 1 256 0
f12 0 513 7 1 512 1

; score ******************************************************************

;        idur   iamp   if0	ifq	iaenv	ilh	ihh%	ibzmul	ibzenv	irise	idec
i1    0  1	-6    220	350	11	1	 1.0	0.9	11	0.003	0.07
i1    +  1	-6    220	600	11	1	 .	0.9	11	.	.
i1    +  1	-6    220	2700	11	1	 .	0.9	11	.	.
i1    +  1	-6    220	2900	11	1	 .	0.9	11	.	.
i1    +  1	-6    220	3300	11	1	 .	0.9	11	.	.
s
i1    0  5	-10    220	350	11	1	 1.0	0.95	11	0.003	0.07
i1    0  5	-30    220	600	11	1	 1.0	0.95	11	.	.
i1    0  5	-27    220	2700	11	1	 1.0	0.95	11	.	.
i1    0  5	-24    220	2900	11	1	 1.0	0.95	11	.	.
i1    0  5	-36    220	3300	11	1	 1.0	0.95	11	.	.
s
i1    0  5	-10    10	600	11	1	 1.0	0.95	12	0.3	0.7
i1    0  5	-17    .	1040	11	1	 1.0	0.95	12	.	.
i1    0  5	-19    .	2250	11	1	 1.0	0.95	12	.	.
i1    0  5	-19    .	2450	11	1	 1.0	0.95	12	.	.
i1    0  5	-30    .	2750	11	1	 1.0	0.95	12	.	.
s
i1    0  5	-10    	50	600	11	1	 1.0	0.95	12	0.003	0.07
i1    0  5	-17    .	1040	11	1	 1.0	0.95	12	.	.
i1    0  5	-19    .	2250	11	1	 1.0	0.95	12	.	.
i1    0  5	-19    .	2450	11	1	 1.0	0.95	12	.	.
i1    0  5	-30    .	2750	11	1	 1.0	0.95	12	.	.
s
i1    0  5	-10    110	600	11	1	 1.0	0.95	12	0.003	0.07
i1    0  5	-17    .	1040	11	1	 1.0	0.95	12	.	.
i1    0  5	-19    .	2250	11	1	 1.0	0.95	12	.	.
i1    0  5	-19    .	2450	11	1	 1.0	0.95	12	.	.
i1    0  5	-30    .	2750	11	1	 1.0	0.95	12	.	.

e
