;=============================================================================
;			STEREO-1.ORC
;-----------------------------------------------------------------------------

;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4 	= input (file)
;	p5	= stereo panning, equal power [-1=L, 1=R]


;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;_____________________________________________________________________________

; GEN functions **********************************************************


;score *******************************************************************
; instr 1  idur  ifile 						pan

i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-1
i1  0.5		0.5    "WhiteNoise_1s-6dB.aiff"	-0.9
i1  1      	0.5    "WhiteNoise_1s-6dB.aiff"	-0.8
i1  1.5		0.5    "WhiteNoise_1s-6dB.aiff"	-0.7
i1  2.0		0.5    "WhiteNoise_1s-6dB.aiff"	-0.6
i1  2.5		0.5    "WhiteNoise_1s-6dB.aiff"	-0.5
i1  3.0		0.5    "WhiteNoise_1s-6dB.aiff"	-0.4
i1  3.5		0.5    "WhiteNoise_1s-6dB.aiff"	-0.3
i1  4.0		0.5    "WhiteNoise_1s-6dB.aiff"	-0.2
i1  4.5		0.5    "WhiteNoise_1s-6dB.aiff"	-0.1
i1  5.0		0.5    "WhiteNoise_1s-6dB.aiff"	0.0
i1  5.5		0.5    "WhiteNoise_1s-6dB.aiff"	0.1
i1  6.0		0.5    "WhiteNoise_1s-6dB.aiff"	0.2
i1  6.5		0.5    "WhiteNoise_1s-6dB.aiff"	0.3
i1  7.0		0.5    "WhiteNoise_1s-6dB.aiff"	0.4
i1  7.5		0.5    "WhiteNoise_1s-6dB.aiff"	0.5
i1  8.0		0.5    "WhiteNoise_1s-6dB.aiff"	0.6
i1  8.5		0.5    "WhiteNoise_1s-6dB.aiff"	0.7
i1  9.0		0.5    "WhiteNoise_1s-6dB.aiff"	0.8
i1  9.5		0.5    "WhiteNoise_1s-6dB.aiff"	0.9
i1  10.0	0.5    "WhiteNoise_1s-6dB.aiff"	1.0

e

