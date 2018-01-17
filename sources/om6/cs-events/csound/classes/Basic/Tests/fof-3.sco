;=============================================================================
;			FOF-3.ORC
; FOF-BASED GLISSON GRANULAR SYNTHESIS / MONO
; AMPLITUDE ENVELOPE WITH POSCIL
;=============================================================================

; Timbre:    Glisson granular synthesis with fof module
; Synthesis: FOF2 (Forme d'Onde Formatique)
;            POSCIL envelopes
; Coded:     ms 9/04, 8/12

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
;	p4	= max amp [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= fundamental freq [Hz]
;	p6	= formant freq [Hz]
;	p7	= bandwidth [Hz]
;	p8	= amp envelope [GEN number]
;	p9	= tex or krise [sec]
;	p10	= total duration of the burst, see debatt [sec]
;	p11	= atten [sec]
;	p12	= octaviation [=>0.0]
;	p13	= frequency deviation for each grain [semitones] (2.0)
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
f8 0 513 7 1.0 512 1.0

; score ******************************************************************

;  iatk idur   iamp	if0	ifreq ibw iaenv iwxin iwxdur iwxout 	ioct ifdev
i1  0    2    -6.0	1.0	440.0	1.6	8	0.1	  2.0	   0.2		1.0	  1.0
s
i1  0    2    -6.0	1.0	440.0	0.6	8	0.1	  2.0	   0.2		1.0	 12.0
s
i1  0    2    -6.0	110.0 609.0 77.6 8	0.003 0.02 0.007 1.0 1.0
s
i1  0    2    -6.0	110.0 609.0 77.6 8	0.003 0.02 0.007 1.0 12.0
s
; default for class
i1  0    1    -6.0	10.0 880.0 1.5 8	0.1 0.5 0.2 1.0 1.0
s

e
