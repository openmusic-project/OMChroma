;=============================================================================
;			8CH-2.ORC
;-----------------------------------------------------------------------------

;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4 	= input (file)
;	p5	= Ch 1 level [0-1]
;	p6	= Ch 2 level [0-1]
;	p7	= Ch 3 level [0-1]
;	p8	= Ch 4 level [0-1]
;	p9	= Ch 5 level [0-1]
;	p10	= Ch 6 level [0-1]
;	p11	= Ch 7 level [0-1]
;	p12	= Ch 8 level [0-1]

;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;_____________________________________________________________________________

; GEN functions **********************************************************


;score *******************************************************************
; instr 1  idur  ifile 						ch1	2	3	4	5	6	7	8

i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	1	0	0	0	0	0	0	0
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	0.5	0.5	0	0	0	0	0	0
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	0	1	0	0	0	0	0	0
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	0	0.5	0.5	0	0	0	0	0
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	0	0	1	0	0	0	0	0
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	0	0	0.5	0.5	0	0	0	0
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	0	0	0	1	0	0	0	0
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	0	0	0	0.5	0.5	0	0	0
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	0	0	0	0	1	0	0	0
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	0	0	0	0	0.5	0.5	0	0
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	0	0	0	0	0	1	0	0
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	0	0	0	0	0	0.5	0.5	0
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	0	0	0	0	0	0	1	0
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	0	0	0	0	0	0	0.5	0.5
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	0	0	0	0	0	0	0	1
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	0.5	0	0	0	0	0	0	0.5
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	1	0	0	0	0	0	0	0

e

