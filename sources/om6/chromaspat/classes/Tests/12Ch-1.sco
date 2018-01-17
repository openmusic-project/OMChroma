;=============================================================================
;			12CH-1.ORC : panning with 12 channels
;-----------------------------------------------------------------------------

;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4 	= input (file)
;	p5	= panning, equal power [Azimuth, degrees]
;		  [0=1, 30/-330=2, 60/-300=3, 90/-270=4, 120/-240=5, 150/-210=6,
;			180/-180=7, 210/-150=8, 240/-120=9 270/-90=10, 300/-60=11, 330/-30=12]

;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;_____________________________________________________________________________

; GEN functions **********************************************************


;score *******************************************************************
; instr 1  idur  ifile 						pan

i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	0
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	15
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	30
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	45
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	60
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	75
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	90
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	105
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	120
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	135
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	150
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	165
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	180
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	195
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	210
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	225
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	240
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	255
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	270
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	285
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	300
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	315
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	330
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	345
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	360
s

i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	0
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-15
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-30
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-45
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-60
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-75
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-90
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-105
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-120
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-135
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-150
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-165
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-180
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-195
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-210
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-225
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-240
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-255
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-270
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-285
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-300
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-315
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-330
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-345
s
i1  0      	0.5    "WhiteNoise_1s-6dB.aiff"	-360

e
