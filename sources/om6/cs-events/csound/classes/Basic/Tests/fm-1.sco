;=============================================================================
;		FM-1.ORC
; FREQUENCY MODULATION (FROM ACCCI, 20_10_1.ORC) / MONO
; AMPLITUDE AND INDEX ENVELOPES WITH OSCILI
;=============================================================================

; Synthesis: FM with dynamic spectral evolution
;            Bell settings
; Source:    Chowning (1973)
; Coded:     jpg 8/92, modified ms 9/02, modified ms 9/02, 8/08

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
;	p6	= N1
;	p7	= N2
;	p8	= max index
;	p9	= min index
;	p10	= amp envelope [GEN, exponential]
;	p11	= index envelope [GEN, exponential]
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
f51 0 513 5 1 513 .0001
f52 0 513 5 1 513 .1

; score ******************************************************************
;       idur iamp  if0 in1 	in2 	imax 	imin 	iaenv 	ienv
i1   0   10   0.0  55	5	7	10	  0	   51   52
i1   10  10   0.0  55	8	2	7	  3	   52   51
e
