;=============================================================================
;			4CH-1.ORC : panning with 4 channels
;-----------------------------------------------------------------------------

;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4 	= input (file)
;	p5	= Panning, equal power [Azimuth, degrees; 0=1, 90=2, 180=3, 270=4]


;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;_____________________________________________________________________________

; GEN functions **********************************************************


;score *******************************************************************
; instr 1  idur  ifile 						pan

i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	0
i1  0.5		0.5    "WhiteNoise_1s-6dB.aiff"	22.5
i1  1      	0.5    "WhiteNoise_1s-6dB.aiff"	45.
i1  1.5		0.5    "WhiteNoise_1s-6dB.aiff"	67.5
i1  2.0		0.5    "WhiteNoise_1s-6dB.aiff"	90.0
i1  2.5		0.5    "WhiteNoise_1s-6dB.aiff"	112.5
i1  3.0		0.5    "WhiteNoise_1s-6dB.aiff"	135.0
i1  3.5		0.5    "WhiteNoise_1s-6dB.aiff"	157.5
i1  4.0		0.5    "WhiteNoise_1s-6dB.aiff"	180.
i1  4.5		0.5    "WhiteNoise_1s-6dB.aiff"	202.5
i1  5.0		0.5    "WhiteNoise_1s-6dB.aiff"	225.0
i1  5.5		0.5    "WhiteNoise_1s-6dB.aiff"	247.5
i1  6.0		0.5    "WhiteNoise_1s-6dB.aiff"	270.0
i1  6.5		0.5    "WhiteNoise_1s-6dB.aiff"	292.5
i1  7.0		0.5    "WhiteNoise_1s-6dB.aiff"	315.0
i1  7.5		0.5    "WhiteNoise_1s-6dB.aiff"	337.5
i1  8.0		0.5    "WhiteNoise_1s-6dB.aiff"	360.0
s 9.0

i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	0
i1  0.5		0.5    "WhiteNoise_1s-6dB.aiff"	-22.5
i1  1      	0.5    "WhiteNoise_1s-6dB.aiff"	-45.
i1  1.5		0.5    "WhiteNoise_1s-6dB.aiff"	-67.5
i1  2.0		0.5    "WhiteNoise_1s-6dB.aiff"	-90.0
i1  2.5		0.5    "WhiteNoise_1s-6dB.aiff"	-112.5
i1  3.0		0.5    "WhiteNoise_1s-6dB.aiff"	-135.0
i1  3.5		0.5    "WhiteNoise_1s-6dB.aiff"	-157.5
i1  4.0		0.5    "WhiteNoise_1s-6dB.aiff"	-180.
i1  4.5		0.5    "WhiteNoise_1s-6dB.aiff"	-202.5
i1  5.0		0.5    "WhiteNoise_1s-6dB.aiff"	-225.0
i1  5.5		0.5    "WhiteNoise_1s-6dB.aiff"	-247.5
i1  6.0		0.5    "WhiteNoise_1s-6dB.aiff"	-270.0
i1  6.5		0.5    "WhiteNoise_1s-6dB.aiff"	-292.5
i1  7.0		0.5    "WhiteNoise_1s-6dB.aiff"	-315.0
i1  7.5		0.5    "WhiteNoise_1s-6dB.aiff"	-337.5
i1  8.0		0.5    "WhiteNoise_1s-6dB.aiff"	-360.0

e

