;=============================================================================
;		SMPL-1.ORC
; SAMPLER READING FROM A SOUND FILE WITH AUTOMATIC SR CONVERSION / MONO
; READING SAMPLES THROUGH DISKIN2, NO LOOP
; CONTROLLABLE WRAP
; AMPLITUDE ENVELOPE WITH POSCILI + COSINE IN-OUT
; FIXED TRANSPOSITION
;=============================================================================

; Timbre:       Reading from a sound file, with transposition
; Synthesis:    Sampler
; Coded:     	ms 3/09

; NB: NEW STRUCTURE FOR THE AMPLITUDES FROM AUGUST 2008!
;    Positive value > 0.0  : linear amplitude (>0.0-1000.0)
;    0.0 or negative value : amplitude in dB (0 = maximum value)

; The apparently arbitrary amplitude range (0-1000, rather than 0-1)
;     avoids printing small values with exponential notation
; Default SR = 96000, recommended precision: 24 bits

;-----------------------------------------------------------------------------
;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4	= max amp [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= transposition factor [1=same as original]
;	p6	= sound file [name]
;	p7	= starting point in file [sec]
;	p8	= amp envelope [GEN]
;	p9	= duration of the local attack/decay [sec]
;	p10	= wrap [0 or non 0]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;	f19	sigmoid rise/decay
;_____________________________________________________________________________

; OTHER GEN FUNCTIONS

; straight line
f8	0 513	7 1 512 1
f19 0  65537  19 .5 .5 270 .5

; triangle line
f7	0 513	7 0 256 1 256 0
;_____________________________________________________________________________


; score ******************************************************************

;            iamp    if0   ifile			  iskip iaenv iwin iwrap
i1   0   2   0.0    1.0   "santuri.aiff"	0.0	8	1.0 0
i1   +   2   0.0    0.5   "santuri.aiff"	0.0	8	1.0 .
i1   +   4   0.0    0.1   "santuri.aiff"	0.0	8	1.0 .
i1   +   4   0.0    2.0   "santuri.aiff"	0.0	8	1.0 .
i1   +   4   0.0    3.0   "santuri.aiff"	0.0	8	1.0 .
s
i1   0   1   0.0    2.0   "santuri.aiff"	 0.0	8	0.0 0
i1   +   1   0.0    2.0   "santuri.aiff"	-0.5	8	0.0 .
i1   +   5   0.0   -2.0   "santuri.aiff"	 0.0	8	0.0 .
s
i1   0   5   0.0    2.0   "santuri.aiff"	 0.0	7	0.1 0
i1   +   5   0.0    2.0   "santuri.aiff"	 2.5	7	0.1 .
i1   +   5   0.0   -2.0   "santuri.aiff"	 2.5	7	0.1 .
s
i1   0   10   1000.0    1.0   "santuri.aiff"	 0.0	7	1.0 1
i1   0    10  1000.0    1.0   "santuri.aiff"	 2.5	7	1.0 1
i1   +   10  1000.0   -1.0   "santuri.aiff"	 0.0	7	1.0 1
s
i1   0  5     -10.0    2.0   "santuri.aiff"	0.0		7	0.1 0
i1   .  2     -10.0    3.0   "santuri.aiff"	0.0		.	.	.
i1   .  1.7   -10.0    4.0   "santuri.aiff"	0.0		.	.	.
i1   .  1.3   -10.0    5.0   "santuri.aiff"	0.0		.	.	.
i1   .  1     -10.0    6.0   "santuri.aiff"	0.0		.	.	.
i1   .  0.5   -10.0    7.0   "santuri.aiff"	0.0		.	.	.

e