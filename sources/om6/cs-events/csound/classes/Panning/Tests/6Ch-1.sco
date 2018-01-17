;=============================================================================
;			6CH-1.ORC : panning with 6 channels
;-----------------------------------------------------------------------------

;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4 	= input (file)
;	p5	= Panning, equal power [Azimuth, degrees]
;		  [0=1, 60/-300=2, 120/-240=3, 180/-180=4, 240/-120=5, 300/-60=6]

;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;_____________________________________________________________________________

; GEN functions **********************************************************


;score *******************************************************************
; instr 1  idur  ifile 						pan

i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	0
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	30
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	60
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	90
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	120
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	150
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	180
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	210
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	240
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	270
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	300
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	330
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	360
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	0
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-30
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-60
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-90
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-120
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-150
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-180
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-210
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-240
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-270
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-300
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-330
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-360

e

