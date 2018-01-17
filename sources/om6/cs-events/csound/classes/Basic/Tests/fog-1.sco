;=============================================================================
;			FOG-1.ORC
;	GRANULAR SYNTHESIS (NEW INSTRUMENT) / MONO
;	AMPLITUDE ENVELOPE WITH POSCIL
;=============================================================================

; Timbre:    Granular synthesis with fog module, voice-like tones
; Synthesis: FOG (Forme d'Onde Granulaire)
;            POSCIL envelopes
; Coded:     ms 9/02, 2/09

; NB:
;	this instrument works AT BEST with samples whose length is a power of 2
;	  those which are shorter than the immediately superior power of 2 accepted
;	  by the GEN01, will produce silence when looking up the unsued portion of the table

; NB1: NEW STRUCTURE FOR THE AMPLITUDES FROM AUGUST 2008!
;    Positive value > 0.0  : linear amplitude (>0.0-1000.0)
;    0.0 or negative value : amplitude in dB (0 = maximum value)

; The apparently arbitrary amplitude range (0-1000, rather than 0-1)
;    avoids printing small values with exponential notation

; Replaced oscili with poscil (precise oscillator), ms 8/08
; Default SR = 96000, recommended precision: 24 bits
;-----------------------------------------------------------------------------
;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4	= maximum amp [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= density of the grains [Hz]
;	p6	= transposition factor [1=original]
;	p7	= bandwidth -> exponential decay [Hz]
;	p8	= audio file [GEN01]
;	p9	= speed of the starting pointer in the file [1=same as original]
;	p10	= amplitude envelope [GEN]
;	p11	= rise time of the grain envelope [sec]
;	p12	= overall duration of the grain [sec]
;	p13	= decay time of the grain envelope [sec]
;	p14 = octaviation index [>= 0.0]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f31	audio file
;	f19	sigmoid rise/decay shape
;_____________________________________________________________________________

; audio file
f31  0  524288  1   "basmba.aiff"    0    0  0


; sigmoid rise/decay
f19 0  65536  19 .5 .5 270 .5

; amplitude envelope
f7 0 513 7 1.0 512 1.0
f8 0 513 7 0.0  20 1  90 1  402 0.0

; score ******************************************************************

;  iatk idur   amp	f0(dens) freq(xpf) bw afil spd	aenv win wdur wout oct
i1  0    2.0	-6.0 10.0	  1.0		0 31   0   7	 0.01 0.1 0.05 0.0
s 3.0
i1  0    2.0	-6.0 10.0	  1.0		0 31   1   7	 0.01 0.1 0.05 0.0
s 3.0
i1  0    2.0	-6.0 10.0	  1.0		0 31   2   7	 0.01 0.1 0.05 0.0
s 3.0
i1  0    2.0	-6.0 10.0	  1.0		0 31   -1   7	 0.01 0.1 0.05 0.0
s 3.0
i1  0    2.0	-6.0 10.0	  1.0		0 31   -2   7	 0.01 0.1 0.05 0.0
s 3.0

i1  0    2.0	-6.0 10.0	  2.0		0 31   1   7	 0.01 0.1 0.05 0.0
s 3.0
i1  0    2.0	-6.0 10.0	  0.5		0 31   1   7	 0.01 0.1 0.05 0.0
s 3.0
i1  0    2.0	-6.0 10.0	  4.0		0 31   1   7	 0.01 0.1 0.05 0.0
s 3.0
i1  0    2.0	-25.0 10.0	  100.0		0 31   1   7	 0.01 0.1 0.05 0.0
s 3.0

i1  0    2.0	-25.0 100.0	  1.0		0 31   1   7	 0.01 0.1 0.05 0.0
s 3.0
i1  0    2.0	-40.0 1000.0  1.0		0 31   1   7	 0.01 0.1 0.05 0.0
s 3.0
i1  0    2.0	-50.0 2000.0  1.0		0 31   1   7	 0.01 0.1 0.05 0.0
s 3.0

i1  0    2.0	-6.0 10.0	  1.0		1 31   1   7	 0.01 0.1 0.05 0.0
s 3.0
i1  0    2.0	-6.0 10.0	  1.0		10 31   1   7	 0.01 0.1 0.05 0.0
s 3.0

i1  0    2.0	-15.0 10.0	  1.0		0 31   1   7	 0.01 0.5 0.05 0.0
s 3.0
i1  0    2.0	-15.0 10.0	  1.0		0 31   1   7	 0.1 0.5 0.2 0.0
s 3.0

i1  0    2.0	-10.0 10.0	  1.0		0 31   1   7	 0.1 0.3 0.1 1.0
s 3.0
i1  0    2.0	-10.0 10.0	  1.0		0 31   1   7	 0.1 0.3 0.1 2.0
s 3.0
i1  0    2.0	-10.0 10.0	  1.0		0 31   1   7	 0.1 0.3 0.1 4.0
e
