;=============================================================================
;			RANFL-1.ORC
; RANDOM NOISE GENERATOR FILTERED BY A FOF FILTER / MONO
; AMPLITUDE ENVELOPE WITH POSCIL
;=============================================================================

; Timbre:    Formantic subtractive synthesis with buzz
; Synthesis: randi with fofilter
;            POSCIL envelopes
; Coded:     ms 2/09

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
;	p5	= freq of the noise [Hz]
;	p6	= centre freq of the filters [Hz]
;	p7	= amp envelope [GEN]
;	p8	= impulse response attack time [sec]
;	p9	= impulse response decay time [sec]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;_____________________________________________________________________________

; GEN functions **********************************************************
; aenvs
f11 0 513 7 0 256 1 256 0
f12 0 513 7 1 512 1

; score ******************************************************************

;      idur iamp if0	ifq		ibw	iaenv	irise	idec
i1    0  1	-6  220		350	1000	11	0.5	2.0
i1    +  1	-6  220		600	.		11	.	.
i1    +  1	-6  220		2700 .		11	.	.
i1    +  1	-6  220		2900 .		11	.	.
i1    +  1	-6  220		3300 .		11	.	.
s
i1    0  5	-15  220	350	 1000	11	0.3	0.5
i1    .  .	-15  220	600	.		11	.	.
i1    .  .	-15  220	2700 .		11	.	.
i1    .  .	-15  220	2900 .		11	.	.
i1    .  .	-15  220	3300 .		11	.	.
s
i1    0  5	-15  220	350 1000	11	0.03 0.05
i1    .  .	-15  220	600	.		11	.	.
i1    .  .	-15  220	2700 .		11	.	.
i1    .  .	-15  220	2900 .		11	.	.
i1    .  .	-15  220	3300 .		11	.	.
s
i1    0  5	-15  220	350	1000	11	0.003	0.02
i1    .  .	-15  220	600 .		11	.	.
i1    .  .	-15  220	2700 .		11	.	.
i1    .  .	-15  220	2900 .		11	.	.
i1    .  .	-15  220	3300 .		11	.	.
s
i1    0  5	-15  220	350	50		11	0.003	0.02
i1    .  .	-15  220	600 .		11	.	.
i1    .  .	-15  220	2700 .		11	.	.
i1    .  .	-15  220	2900 .		11	.	.
i1    .  .	-15  220	3300 .		11	.	.
s
i1    0  5	-15  220	350	5		11	0.003	0.02
i1    .  .	-15  220	600 .		11	.	.
i1    .  .	-15  220	2700 .		11	.	.
i1    .  .	-15  220	2900 .		11	.	.
i1    .  .	-15  220	3300 .		11	.	.
e
