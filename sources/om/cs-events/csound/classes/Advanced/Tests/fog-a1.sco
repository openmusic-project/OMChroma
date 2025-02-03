;=============================================================================
;			FOG-A1.ORC
;	GRANULAR SYNTHESIS (NEW INSTRUMENT) / MONO
;	AMPLITUDE ENVELOPE WITH POSCIL, COMPLETE CONTROL OF THE PARAMETRES
;=============================================================================

; Timbre:    Granular synthesis with fog module, voice-like tones
; Synthesis: FOG (Forme d'Onde Granulaire)
;            POSCIL envelopes
; Coded:     ms 2/09

; NB:
; this instrument works AT BEST with samples whose length is a power of 2
;	those which are shorter than the immediately superior power of 2 accepted
;	by the GEN01, will produce silence when looking up the unsued portion of the table

; NB1: NEW STRUCTURE FOR THE AMPLITUDES FROM AUGUST 2008!
;	Positive value > 0.0  : linear amplitude (>0.0-1000.0)
;	0.0 or negative value : amplitude in dB (0 = maximum value)

; The apparently arbitrary amplitude range (0-1000, rather than 0-1)
;	avoids printing small values with exponential notation

; Replaced oscili with poscil (precise oscillator), ms 8/08
; Default SR = 96000, recommended precision: 24 bits
;-----------------------------------------------------------------------------
;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4	= max amp [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= min density of the grains [Hz]
;	p6	= max density of the grains [Hz]
;	p7	= envelope for the density of the grains [GEN]
;	p8	= jitter amp of density [0-1]
;	p9	= min transposition factor [1=original]
;	p10	= max transposition factor [1=original]
;	p11	= envelope for the transposition factor [GEN]
;	p12	= jitter amp of transposition factor [0-1]
;	p13	= audio file [GEN01]
;	p14	= min speed of the starting pointer in the file [1=same as original]
;	p15	= max speed of the starting pointer in the file [1=same as original]
;	p16	= envelope for the speed of the starting pointer in the file [GEN]
;	p17	= amplitude envelope [GEN]
;	p18	= min bandwidth (-> exponential decay) [Hz]
;	p19	= max bandwidth (-> exponential decay) [Hz]
;	p20	= envelope for the bandwidth -> exponential decay [GEN]
;	p21	= min rise time of the grain envelope [sec]
;	p22	= max rise time of the grain envelope [sec]
;	p23	= envelope for the rise time of the grain envelope [GEN]
;	p24	= min overall duration of the grain [sec]
;	p25	= max overall duration of the grain [sec]
;	p26	= envelope for the overall duration of the grain [GEN]
;	p27	= min decay time of the grain envelope [sec]
;	p28	= max decay time of the grain envelope [sec]
;	p29	= envelope for the decay time of the grain envelope [GEN]
;	p30 = min octaviation index [>= 0.0]
;	p31 = max octaviation index [>= 0.0]
;	p32 = envelope for the octaviation index [GEN]
;	p33 = transposition mode [if=0.0, no glissando within each grain]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f31	audio file
;	f19	sigmoid rise/decay shape
;_____________________________________________________________________________

; audio file
f31  0  524288  1   "basmba_96.aiff"    0    0  0


; sigmoid rise/decay
f19 0  65536  19 .5 .5 270 .5

; amplitude envelope
f7 0 513 7 1.0 512 1.0
f8 0 513 7 0.0  20 1  480 1  12 0.0
f9 0 513 7 0.0  512 1.0
f10 0 513 7 1.0  512 0.0

; score ******************************************************************

;  iatk idur amp f0mn/mx/ev/jt(dens) fqmn/mx/ev/jt(xpf) afil skpmn/mx/ev aenv
i1  0	3.0	-15.0	5.0	20.0 9 0.5	  1.0 2.0 9	0.5	31   0	2	9   8 \
; bwmn/mx/ev winmn/mx/ev wdurmn/mx/ev woutmn/mx/ev octmn/mx/ev	imode
	0 10 9  0.5 0.01 9	 1.0 0.1 9	 0.5 0.1 9	 0.0 2.0 9		1
s 4.0

;  iatk idur amp f0mn/mx/ev/jt(dens) fqmn/mx/ev/jt(xpf) afil skpmn/mx/ev aenv
i1  0	3.0	-10.0	5.0	20.0 10 0.5	  1.0 2.0 10 0.5	31   0	2	10   8 \
; bwmn/mx/ev winmn/mx/ev wdurmn/mx/ev woutmn/mx/ev octmn/mx/ev	imode
	0 10 10  0.5 0.01 10 1.0 0.1 10	 0.5 0.1 10 0.0 2.0 10		1
s 4.0

;  iatk idur amp f0mn/mx/ev/jt(dens) fqmn/mx/ev/jt(xpf) afil skpmn/mx/ev aenv
i1  0	3	-6.0	10.0 10.0 9 0.5	  1.0 1.0 9	0.5		31   1	1	9	8 \
; bwmn/mx/ev winmn/mx/ev wdurmn/mx/ev woutmn/mx/ev octmn/mx/ev	imode
	0 0 9  0.01 0.05 9	 0.1 0.5 9	 0.05 0.01 9	 0.0 8.0 9		1
s 4.0

;  iatk idur amp f0mn/mx/ev/jt(dens) fqmn/mx/ev/jt(xpf) afil skpmn/mx/ev aenv
i1  0	3	-15.0 100.0	1.0 9 0.5	  2.0 2.0 9 0.5		31   2	0	9   8 \
; bwmn/mx/ev winmn/mx/ev wdurmn/mx/ev woutmn/mx/ev octmn/mx/ev	imode
	0 0 9  0.01 0.01 9	 0.1 0.1 9	 0.01 0.01 9	 0.0 0.0 9		1
s 4.0
e
