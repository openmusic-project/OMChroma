;=============================================================================
;			FOF-1.ORC
; FORMANTIC WAVEFORM (FROM ACCCI, 45_01_2.ORC) / MONO
; AMPLITUDE ENVELOPE WITH POSCIL
; NO CONTROL OF THE DETAILS OF THE FOF
;=============================================================================

; Timbre:    Granular synthesis with fof module, voice-like tones
; Synthesis: FOF (Forme d'Onde Formatique)
;            POSCIL envelopes
; Coded:     jpg 8/92, modified ms 9/02, 8/08

; NB: NEW STRUCTURE FOR THE AMPLITUDES FROM AUGUST 2008!
;    Positive value > 0.0  : linear amplitude (>0.0-1000.0)
;    0.0 or negative value : amplitude in dB (0 = maximum value)

; The apparently arbitrary amplitude range (0-1000, rather than 0-1)
;         avoids printing small values with exponential notation

; Replaced oscili with poscil (precise oscillator), ms 8/08
; Default SR = 96000, recommended precision: 24 bits
; NB1: this implementation works better with audio fundamental frequencies
;-----------------------------------------------------------------------------
;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4	= max amp [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= fundamental freq [Hz]
;	p6	= formant freq [Hz]
;	p7	= bandwidth [Hz]
;	p8	= amp envelope [GEN number]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f4	large non interpolating sine tone
;	f19	sigmoid rise/decay shape
;_____________________________________________________________________________


; large sine tone
f4  0 16777216  10  1
; sigmoid rise/decay
f19 0  65536  19 .5 .5 270 .5

; amplitude envelope
f31 0 513 7 0.0  20 1  90 1  402 0.0

; score ******************************************************************

; default vowel "a" from the old Chant program in Fortran
; ---> NO VIBRATO, NO RULES WHATSOEVER...!@!

;  iatk idur   iamp	if0	ifreq	ibw		iaenv
i1  0    10    278	220	609.0	77.6	31
i1  0     8    137	 .	1000.0	88.4	.
i1  0     6     70	 .	2450.0	122.9	.
i1  0     4     78	 .	2700.0	127.8	.
i1  0     2     18	 .	3240.0	137.7	.

s1

i1  0     2    278	220	609.0	0.6	31
i1  0     4    137	 .	1000.0	0.4	.
i1  0     6     70	 .	2450.0	3.9	.
i1  0     8     78	 .	2700.0	2.8	.
i1  0    10     18	 .	3240.0	0.7	.

e