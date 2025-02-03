;=============================================================================
;		PLUCK-2.ORC
; PLUCKED STRING USING THE KARPLUS-STRONG MODEL
;    (FROM ACCCI, 15_01_3.ORC) / MONO
; AMPLITUDE ENVELOPE WITH LINSEG, CONTROL OVER INTIALIZATION AND DECAY MODES
;=============================================================================

; Timbre:       Plucked string
; Synthesis:    Karplus-Strong algorithm
;               PLUCK
;               LINSEG envelope, cembalo sounds
; Coded:     	jpg 8/93, modified ms 9/02, 8/08

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
;	p4	= maximum amp [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= freq [Hz]
;	p6	= intended freq (buffer) [Hz]
;	p7	= % of the total duration used for the last decay [0-1]
;	p8	= table number of a stored function [GEN or 0=random]
;	p9	= method of natural decay
;		  1: simple averaging
;		  2: stretched averaging. ipar1=stretch factor of smoothing time
;		  3: simple drum. ipar1=roughness factor [0-1]
;				        0=plucked string
;						1=reverse the polarity of each sample (oct down, odd harms)
;						0.5=optimum drum
;		  4: stretched drum. ipar1=roughness, ipar2=stretch factor
;		  5: weighted averaging. ipar1=weight for the current sample
;					 ipar2: weight for the previous adjacent one.
;					 NB: ipar1+ipar2<=1.0
;		  6: 1st order recursive filter with coef 0.5
;	p10	= ipar1 (used by p9, see above)
;	p11	= ipar2 (used by p9, see above)
;NB: plucked strings (1,2,5,6) are best realised by starting with random noise
;      (p8=0). Drum sounds (3,4) work best with a flat source (wide pulse).
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
; none
;_____________________________________________________________________________

; score ******************************************************************

;            iamp    ifq   ibuf idec 	iranfun	imet	ipar1	ipar2
i1   0   1    -6.0    220   220  0.1	0	1	1	1
i1   +   1    -6.0    220   440  . 	0	1	1	1
i1   +   1    -6.0    220   1000   . 	0	1	1	1
i1   +   1    -6.0    220   10000   .	0	1	1	1
i1   +   1    -6.0    220   20000   .	0	1	1	1
i1   +   1    -6.0    220   100   .	0	1	1	1
i1   +   1    -6.0    220   50   .	0	1	1	1
i1   +   1    -6.0    220   10   .	0	1	1	1
s
i1   0   2    -6.0    220   220  0.1	0	1	1	1
s

i1   0   2    -6.0    220   220  0.1	0	2	1	1
s
i1   0   1    -6.0    220   220  0.1	0	2	2	1
s
i1   0   1    -6.0    220   220  0.1	0	2	3	1
s
i1   0   1    -6.0    220   220  0.1	0	2	5	1
s
i1   0   1    -6.0    220   220  0.1	0	2	10	1
s
i1   0   2    -6.0    220   220  0.1	0	2	100	1
s

i1   0   2    -6.0    220   220  0.1	0	3	0	1
s
i1   0   1    -6.0    220   220  0.1	0	3	0.1	1
s
i1   0   1    -6.0    220   220  0.1	0	3	0.2	1
s
i1   0   1    -6.0    220   220  0.1	0	3	0.3	1
s
i1   0   1    -6.0    220   220  0.1	0	3	0.4	1
s
i1   0   1    -6.0    220   220  0.1	0	3	0.5	1
s
i1   0   1    -6.0    220   220  0.1	0	3	0.6	1
s
i1   0   1    -6.0    220   220  0.1	0	3	0.7	1
s
i1   0   1    -6.0    220   220  0.1	0	3	0.8	1
s
i1   0   1    -6.0    220   220  0.1	0	3	0.9	1
s
i1   0   1    -6.0    220   220  0.1	0	3	1	1
s
i1   0   2    -6.0    220   220  0.1	0	3	0.0	1
s

i1   0   2    -6.0    220   220  0.1	0	4	0.5	1
s
i1   0   1    -6.0    220   220  0.1	0	4	0.5	2
s
i1   0   1    -6.0    220   220  0.1	0	4	0.5	3
s
i1   0   1    -6.0    220   220  0.1	0	4	0.5	5
s
i1   0   1    -6.0    220   220  0.1	0	4	0.5	10
s
i1   0   2    -6.0    220   220  0.1	0	4	0.5	100
s

i1   0   2    -6.0    220   220  0.1	0	5	0.5	0.5
s
i1   0   1    -6.0    220   220  0.1	0	5	0.25	0.5
s
i1   0   1    -6.0    220   220  0.1	0	5	0	0.5
s
i1   0   1    -6.0    220   220  0.1	0	5	0.5	0.25
s
i1   0   2    -6.0    220   220  0.1	0	5	0.5	0
s

i1   0   5    -6.0    220   220  0.1	0	6	0	0

e