;=============================================================================
;		FM-2.ORC
; FREQUENCY MODULATION (FROM ACCCI, 20_10_1.ORC) / MONO
; AMPLITUDE AND INDEX ENVELOPES WITH OSCILI
; SAME AS FM-1.ORC, BUT WITH FREQ INSTEAD OF N1
;=============================================================================

; Synthesis: FM with dynamic spectral evolution
;            Bell settings
; Source:    Chowning (1973)
; Coded:     jpg 8/92, modified ms 1/08, 8/08

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
;	p4	= maximum amp [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= fundamental freq [Hz]
;	p6	= freq of the carrier [Hz]
;	p7	= N2
;	p8	= max index
;	p9	= min index
;	p10	= amp envelope [GEN number]
;	p11	= index envelope [GEN number]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f1	carrier audio wave (sine tone)
;	f2	modulating audio wave (sine tone)
;_____________________________________________________________________________


; GEN functions **********************************************************
; carrier
f1  0  4097  10  1

; modulating
f2  0  4097  10  1

; envelopes
f51 0 513 7 0 10 1 490 0.3 12 0
f52 0 513 5 1 513 .1

; score ******************************************************************

;       idur iamp  [if0]	icar 	imod 	imax 	imin 	iaenv 	ienv
i1   0   2   -6.0  100		55	55	10	  0	   51   52
i1   2   2   -6.0  100		440	110	10	  0	   51   52
i1   4 	 2   -6.0  100		440	100	10	  3	   51   52
e
