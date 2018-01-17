;=============================================================================
;			8CH-1.ORC : panning with 8 channels
;-----------------------------------------------------------------------------

;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4 	= input (file)
;	p5	= panning, equal power [Azimuth, degrees]
;		  [0=1, 45/-315=2, 90/-270=3, 135/-225=4,
;			180/-180=5, 225/-135=6, 270/-90=7 315/-45=8]

;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;_____________________________________________________________________________

; GEN functions **********************************************************


;score *******************************************************************
; instr 1  idur  ifile 						pan

i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	0
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	22.5
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	45
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	67.5
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	90
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	112.5
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	135
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	157.5
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	180
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	202.5
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	225
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	247.5
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	270
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	292.5
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	315
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	337.5
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	360
s

i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-22.5
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-45
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-67.5
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-90
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-112.5
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-135
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-157.5
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-180
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-202.5
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-225
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-247.5
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-270
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-292.5
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-315
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-337.5
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-360

e

