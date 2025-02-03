;=============================================================================
;			FOF-2.ORC
; FORMANTIC WAVEFORM (FROM ACCCI, 45_01_2.ORC) / MONO
; AMPLITUDE ENVELOPE WITH OSCILI
; CONTROL OF THE DETAILS OF THE FOF FROM THE SCORE
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

; NB1: this implementation works with both audio and sub-audio f0's
;         and allows for an independent control of tex, debatt and atten
;-----------------------------------------------------------------------------
;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4	= maximum amplitude [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= fundamental frequency [Hz]
;	p6	= formant frequency [Hz]
;	p7	= bandwidth [Hz]
;	p8	= amplitude envelope [GEN number]
;	p9	= tex [sec]
;	p10	= debatt or krise [sec]
;	p11	= atten or kdur [sec]
;	p12	= octaviation [=>0.0]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f1	large sine tone
;	f19	sigmoid rise/decay shape
;_____________________________________________________________________________

; large sine tone
f4  0 16777216  10  1
; sigmoid rise/decay
f19 0  65536  19 .5 .5 270 .5

; amplitude envelope
f31 0 513 7 0.0  20 1  90 1  402 0.0  1 0.0
f32 0 513 7 0.0  1 1  511 1.0  1 0.0

; score ******************************************************************

; default vowel "a" from the old Chant program in Fortran
; ---> NO VIBRATO, NO RULES WHATSOEVER...!@!

;  iatk idur   iamp	if0	ifreq	ibw	iaenv	itex idebatt iatten	ioct
i1  0    2    278	220	609.0	77.6	31	0.003	0.02	0.007	0.0
i1  0    4    137	 .	1000.0	88.4	.	.	.	.	.
i1  0    5     70	 .	2450.0	122.9	.	.	.	.	.
i1  0    3     78	 .	2700.0	127.8	.	.	.	.	.
i1  0    1     18	 .	3240.0	137.7	.	.	.	.	.
s1
i1  0    1    278	220	609.0	77.6	31	0.003	0.02	0.007	0.0
i1  0    1    137	 .	1000.0	88.4	.	.	.	.	.
i1  0    1     70	 .	2450.0	122.9	.	.	.	.	.
i1  0    1     78	 .	2700.0	127.8	.	.	.	.	.
i1  0    1     18	 .	3240.0	137.7	.	.	.	.	.
s1
i1  0    1    278	220	609.0	77.6	31	0.003	0.02	0.007	0.5
i1  0    1    137	 .	1000.0	88.4	.	.	.	.	.
i1  0    1     70	 .	2450.0	122.9	.	.	.	.	.
i1  0    1     78	 .	2700.0	127.8	.	.	.	.	.
i1  0    1     18	 .	3240.0	137.7	.	.	.	.	.
s1
i1  0    1    278	220	609.0	77.6	31	0.003	0.02	0.007	1.0
i1  0    1    137	 .	1000.0	88.4	.	.	.	.	.
i1  0    1     70	 .	2450.0	122.9	.	.	.	.	.
i1  0    1     78	 .	2700.0	127.8	.	.	.	.	.
i1  0    1     18	 .	3240.0	137.7	.	.	.	.	.
s1
i1  0    1    278	220	609.0	77.6	31	0.003	0.02	0.007	2.0
i1  0    1    137	 .	1000.0	88.4	.	.	.	.	.
i1  0    1     70	 .	2450.0	122.9	.	.	.	.	.
i1  0    1     78	 .	2700.0	127.8	.	.	.	.	.
i1  0    1     18	 .	3240.0	137.7	.	.	.	.	.
s1
; simle sinusoidal percussive sound (poly-rhythmic)
i1  0     10    100	1.0	609.0	0.6	32	0.2	4.0	1.0	0.0
i1  1     10    100	2.0	1000.0	0.6	32	0.1	2.0	1.0	.
i1  2	  10    100	3.0	2450.0	0.6	32	0.05	2.0	1.0	.
i1  3     10    100	4.0	2700.0	0.6	32	0.01	1.5	0.5	.
i1  4     10    100	5.0	3240.0	0.6	32	0.005	1.0	0.3	.
e
