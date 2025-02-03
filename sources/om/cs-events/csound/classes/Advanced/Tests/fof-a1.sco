;=============================================================================
;			FOF-A1.ORC
; FORMANTIC WAVEFORM (FROM ACCCI, 45_01_2.ORC) / MONO
; AMPLITUDE ENVELOPE WITH POSCILI
; COMPLETE CONTROL OF THE DETAILS OF THE FOF FROM THE SCORE
;=============================================================================

; Timbre:    Granular synthesis with fof module, voice-like tones
; Synthesis: FOF (Forme d'Onde Formatique)
;            OSCILI envelopes
; Coded:     jpg 8/92, modified ms 9/04, 8/08

; NB: NEW STRUCTURE FOR THE AMPLITUDES FROM AUGUST 2008!
;    Positive value > 0.0  : linear amplitude (>0.0-1000.0)
;    0.0 or negative value : amplitude in dB (0 = maximum value)

; The apparently arbitrary amplitude range (0-1000, rather than 0-1)
;         avoids printing small values with exponential notation
; Replaced oscili with poscil (precise oscillator), ms 8/08
; Default SR = 96000, recommended precision: 24 bits
; NB1: this implementation works with both audio and sub-audio f0's and
;       allows for an independent control of rise, decay and grain duration
;-----------------------------------------------------------------------------
;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4	= max amplitude [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= fundamental frequency [Hz]
;	p6	= formant frequency [Hz]
;	p7	= bandwidth [Hz]
;	p8	= amplitude envelope [GEN number]
;	p9	= tex or rise time [sec]
;	p10	= total duration of the burst [sec]
;	p11	= decay time of the grain envelope [sec]
;	p12	= octaviation [=>0.0]
;	p13	= phase [0-1] (0)
;	p14	= skip, i ­ 0, skip initalization (for legato) (0)
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f1	large sine tone
;	f19	sigmoid rise/decay shape
;_____________________________________________________________________________

; large sine tone
f1  0 16777216  10  1
; sigmoid rise/decay
f19 0  65536  19 .5 .5 270 .5

; amplitude envelope
f51 0 513 7 0.0  20 1  90 1  402 0.0  1 0.0
f51 0 513 7 0.0  256 1 256 0.0
f52 0 513 7 0.0  1 1  511 1.0  1 0.0

; score ******************************************************************

; default vowel "a" from the old Chant program in Fortran
; ---> NO VIBRATO, NO RULES WHATSOEVER...!@!

;  iatk idur   iamp	if0	ifreq	ibw	iaenv	irise itotdur idec	ioct	iphs	iskip
i1  0    1    278	220	609.0	77.6	51	0.003	0.02	0.007	0.0	 0.0	0
i1  +    1    137	 .	1000.0	88.4	.	.	.	.	.	0.25	0
i1  +    1     70	 .	2450.0	122.9	.	.	.	.	.	0.5	0
i1  +    1     78	 .	2700.0	127.8	.	.	.	.	.	0.75	0
i1  +    1     18	 .	3240.0	137.7	.	.	.	.	.	0.9	0
s
i1  0    3    278	220	609.0	77.6	51	0.003	0.02	0.007	0.0	 0.0	0
i1  0    3    137	 .	1000.0	88.4	.	.	.	.	.	0.25	0
i1  0    3     70	 .	2450.0	122.9	.	.	.	.	.	0.5	0
i1  0    3     78	 .	2700.0	127.8	.	.	.	.	.	0.75	0
i1  0    3     18	 .	3240.0	137.7	.	.	.	.	.	0.9	0
s
i1  0    1    278	220	609.0	77.6	51	0.003	0.02	0.007	1.0	0.0	1
i1  +    1    137	 .	1000.0	88.4	.	.	.	.	.	.	1
i1  +    1     70	 .	2450.0	122.9	.	.	.	.	.	.	1
i1  +    1     78	 .	2700.0	127.8	.	.	.	.	.	.	1
i1  +    1     18	 .	3240.0	137.7	.	.	.	.	.	.	1
s
i1  0    3    278	220	609.0	77.6	51	0.003	0.02	0.007	1.0	0.0	1
i1  0    3    137	 .	1000.0	88.4	.	.	.	.	.	.	1
i1  0    3     70	 .	2450.0	122.9	.	.	.	.	.	.	1
i1  0    3     78	 .	2700.0	127.8	.	.	.	.	.	.	1
i1  0    3     18	 .	3240.0	137.7	.	.	.	.	.	.	1

e
