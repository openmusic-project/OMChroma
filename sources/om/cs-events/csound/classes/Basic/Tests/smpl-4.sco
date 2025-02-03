;=============================================================================
;		SMPL-4.ORC
; SAMPLER STORING A SOUND FILE IN A TABLE [GEN01] / MONO
; READING SAMPLES FROM THE TABLE / NORMALIZED BETWEEN 0 AND 1
; AMPLITUDE ENVELOPE WITH POSCILI + COSINE IN-OUT
; THIS SAMPLER WILL START THE FILE AGAIN, IF IT REACHES THE END OF THE TABLE
;   SO BEWARE OF LONG NOTES OR HIGH TRANSPOSITION FACTORS
; SAME AS SMPL-3.ORC, BUT WITH RELATIVE STARTING POINT
;=============================================================================

; Timbre:       Reading from a sound file, with transposition
; Synthesis:    Sampler
; Coded:     	ms 3/09

; NB: NEW STRUCTURE FOR THE AMPLITUDES FROM AUGUST 2008!
;    Positive value > 0.0  : linear amplitude (>0.0-1000.0)
;    0.0 or negative value : amplitude in dB (0 = maximum value)

; The apparently arbitrary amplitude range (0-1000, rather than 0-1)
;         avoids printing small values with exponential notation

; Default SR = 96000, recommended precision: 24 bits
;-----------------------------------------------------------------------------
;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4	= max amplitude [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= transposition factor [1=same frequency as the file]
;	p6	= sound file [GEN01] (tbn_96.aif)
;	p7	= starting point in file [sec, 0.0]
;	p8	= amp envelope [GEN, straight line (8)]
;	p9	= duration of the local attack/decay time [sec, 0.01]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;	f19	sigmoid rise/decay
;_____________________________________________________________________________

; OTHER GEN FUNCTIONS
;sound file
f31  0 0 -1  "santuri.aiff"	0	0	0

; straight line
f8	0 513	7 1 512 1
f19 0  65537  19 .5 .5 270 .5

; triangle line
f7	0 513	7 0 256 1 256 0
;_____________________________________________________________________________


; score ******************************************************************

;            iamp    if0   ifile iskip iaenv iwin
i1   0   2   0.0    1.0   31	0.0	8	1.0
i1   +   2   0.0    0.5   31	0.0	8	1.0
i1   +   2   0.0    0.1   31	0.0	8	1.0
i1   +   2   0.0    -1.0   31	0.0	8	1.0
i1   +   2   0.0    -0.5   31	0.0	8	1.0
s
i1   0   2   0.0    -1.0   31	 0.5	8	0.0
i1   +   2   0.0    -1.0   31	 1.0	8	0.0
i1   +   2   0.0    -1.0   31	 0.1	8	0.0
s
i1   0   5   0.0    1.0   31	 0.0	7	0.1
i1   +   5   0.0    1.0   31	 -0.5	7	0.1
i1   +   5   0.0   -1.0   31	 -1.0	7	0.1
i1   +   5   0.0   -1.0   31	 1.0	7	0.1 ; same as above
s
i1   0   4   1000.0    1.0   31	 0.0	7	1.0
i1   +   4   1000.0    1.0   31	 0.5	7	1.0
i1   +   4   1000.0   -1.0   31	 0.0	7	1.0
s
i1   0  2.5     -10.0    2.0   31	0.0		7	0.1
i1   .  1.4     -10.0    3.0   31	0.0		.	.
i1   .  1.   -10.0    4.0   31	0.0		.	.
i1   .  0.5   -10.0    5.0   31	0.0		.	.
i1   .  0.4     -10.0    6.0   31	0.0		.	.
i1   .  0.3   -10.0    7.0   31	0.0		.	.

e