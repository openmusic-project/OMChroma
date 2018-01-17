;=============================================================================
;			5CH-1.ORC : panning with 5 channels
;-----------------------------------------------------------------------------

;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4 	= input (file)
;	p5	= Panning, equal power
;		  [Azimuth, degrees; 0=1, 90/-270=2, 180/-180=3, 270/-90=4, 288/-72]

;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;_____________________________________________________________________________

; GEN functions **********************************************************


;score *******************************************************************
; instr 1  idur  ifile 						pan

i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	0
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	36
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	72
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	108
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	144
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	180
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	216
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	252
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	288
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	324
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	360
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	0
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-36
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-72
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-108
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-144
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-180
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-216
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-252
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-288
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-324
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-360

e

