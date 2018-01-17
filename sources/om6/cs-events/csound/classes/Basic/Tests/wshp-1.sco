;=============================================================================
;				WSHP-1.ORC
; 		WAVESHAPING (FROM ACCCI, 40_02_1.ORC) / MONO
; 		AMPLITUDE ENVELOPE WITH LINEN
;=============================================================================

; Timbre:    Clarinet-like
; Synthesis: Waveshaping
;            Basic instrument with duration dependent envelope
; Source:    Risset(1969)
;            #150, Serial Excerpt with Clarinet-like Sounds by Nonlinearity
; Coded:     jpg 8/92, modified ms 9/02, 8/08

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
;	p5	= freq [Hz]
;	p6	= attack time of the amp envlp [sec]
;	p7	= decay time of the amp envlp [sec]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f1	sine tone
;	f10	transfer function for the waveshaper
;_____________________________________________________________________________

; GEN functions **********************************************************
f1   0 4097 10 1                           ; sinus

; transfer function waveshaper
f10  0  524289  7 -1.0 204800 -.5 114688 .5 204800 1.0

; score ******************************************************************
;           idur   iamp   ipch	iatk	idec
i1  0.000   0.750 -10.0   165.6	0.085	0.64
i1  0.750   0.250  .      197	.	.
i1  1.000   1.000  .      262.8	.	.
i1  2.000   0.200  .      295	.	.
i1  2.200   0.200  .      331	.	.
i1  2.400   0.200  .      350.8	.	.
i1  2.600   0.200  .      525.6	.	.
i1  2.800   0.200  .      662.2	.	.
i1  3.000   0.250  .      701.6	.	.
i1  3.250   0.250  .      525.6	.	.
i1  3.500   0.250  .      350.8	.	.
i1  3.750   0.250  .      262.8	.	.
i1  4.000   1.000  .      165.6	.	.
i1  5.000   0.125  .      197	.	.
i1  5.125   0.125  .      262.8	.	.
i1  5.250   0.125  .      295	.	.
i1  5.375   0.125  .      331	.	.
i1  5.500   0.125  .      350.8	.	.
i1  5.625   0.125  .      525.6	.	.
i1  5.750   0.125  .      662.2	.	.
i1  5.875   0.125  .      701.6	.	.
s

i1  0.000   0.750 -10.0   165.6	0.3	0.64
i1  0.750   0.250  .      197	.	.
i1  1.000   1.000  .      262.8	.	.
i1  2.000   0.200  .      295	.	.
i1  2.200   0.200  .      331	.	.
i1  2.400   0.200  .      350.8	.	.
i1  2.600   0.200  .      525.6	.	.
i1  2.800   0.200  .      662.2	.	.
i1  3.000   0.250  .      701.6	.	.
i1  3.250   0.250  .      525.6	.	.
i1  3.500   0.250  .      350.8	.	.
i1  3.750   0.250  .      262.8	.	.
i1  4.000   1.000  .      165.6	.	.
i1  5.000   0.125  .      197	.	.
i1  5.125   0.125  .      262.8	.	.
i1  5.250   0.125  .      295	.	.
i1  5.375   0.125  .      331	.	.
i1  5.500   0.125  .      350.8	.	.
i1  5.625   0.125  .      525.6	.	.
i1  5.750   0.125  .      662.2	.	.
i1  5.875   0.125  .      701.6	.	.
e
