;=============================================================================
;		GRAIN-3.ORC
; GRANULAR SYNTHESIS / MONO
; SEVERAL STATIC USER CONTROLS
;=============================================================================

; Timbre:       Granular synthesis textures
; Synthesis:    grain3
; Coded:     	ms 3/09

; NB: NEW STRUCTURE FOR THE AMPLITUDES FROM AUGUST 2008!
;    Positive value > 0.0  : linear amplitude (>0.0-1000.0)
;    0.0 or negative value : amplitude in dB (0 = maximum value)

; The apparently arbitrary amplitude range (0-1000, rather than 0-1)
;     avoids printing small values with exponential notation
; Default SR = 96000, recommended precision: 24 bits

;-----------------------------------------------------------------------------
;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4	= max amp [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= density of the grains [Hz]
;	p6	= grain freq [Hz]
;	p7	= bipolar random variation in grain freq [Hz]
;	p8	= distribution of the grain freq variation [-1/1]
;	p9	= sound file [GEN01, not normalized]
;	p10	= amp envelope [GEN, straight line (8)]
;	p11	= grain starting phase [0-1]
;	p12	= bipolar random variation in grain phase [0-1]
;	p13	= distribution of the grain phase variation [-1/1]
;	p14	= duration of the grain [sec]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;	f20	Hanning window for the grain
;_____________________________________________________________________________
; Hanning window
f20 0  65536  20 2

; sound file
f31 0 524288 -1 "cym_96.aiff" 0 0 0
;f31 0 1048576 -1 "sine220_96.aiff" 0 0 0

; simple lines
; ascending
f6	0 513	7 0 512 1
; triangle
f7	0 513	7 0 256 1 256 0
; straight=1
f8	0 513	7 1 512 1
; fast up/down
f9	0 513	7 0 10 1 492 1 10 0
;_____________________________________________________________________________

; score ******************************************************************

; vary phase dist
;        amp   f0	freq fqd fqdis afil aenv	ph phd phdis	wdur
i1 0 5.0 -6.0 10.0	2.0 0.0 4.0		31	8 		0.0 0.9 4.0		0.1
s 6
i1 0 5.0 -6.0 10.0	2.0 0.0 -4.0	31	8 		0.0 0.9 -4.0	0.1
s 6
i1 0 5.0 -6.0 10.0	2.0 0.0 2.0		31	8 		0.0 0.9 2.0		0.1
s 6
i1 0 5.0 -6.0 10.0	2.0 0.0 -2.0	31	8 		0.0 0.9 -2.0	0.1
s 6
i1 0 5.0 -6.0 10.0	2.0 0.0 0.66666	31	8 		0.0 0.9 0.66666	0.1
s 6
i1 0 5.0 -6.0 10.0	2.0 0.0 -0.33333	31	8 	0.0 0.9 -0.3333	0.1
s 6
i1 0 5.0 -6.0 10.0	2.0 0.0 0.8			31	8 	0.0 0.9 0.8		0.1
s 6
i1 0 5.0 -6.0 10.0	2.0 0.0 -0.2	31	8 		0.0 0.9 -0.2	0.1
s 6
i1 0 5.0 -6.0 10.0	2.0 0.0 1.0		31	8 		0.0 0.9 1.0		0.1

; vary ph start
s 6
i1 0 5.0 -6.0 10.0	2.0 0.0 4.0		31	8 		0.5 0.0 4.0		0.1
s 6
i1 0 5.0 -6.0 10.0	2.0 0.0 -4.0	31	8 		0.5 0.1 -4.0	0.1
s 6
i1 0 5.0 -6.0 10.0	2.0 0.0 2.0		31	8 		0.5 0.2 2.0		0.1
s 6
i1 0 5.0 -6.0 10.0	2.0 0.0 -2.0	31	8 		0.5 0.3 -2.0	0.1
s 6
i1 0 5.0 -6.0 10.0	2.0 0.0 0.66666	31	8 		0.5 0.4 0.66666	0.1
s 6
i1 0 5.0 -6.0 10.0	2.0 0.0 -0.33333	31	8 	0.5 0.5 -0.3333	0.1
s 6
i1 0 5.0 -6.0 10.0	2.0 0.0 0.8			31	8 	0.5 0.67 0.8	0.1
s 6
i1 0 5.0 -6.0 10.0	2.0 0.0 -0.2	31	8 		0.5 0.9 -0.2	0.1
s 6
i1 0 5.0 -6.0 10.0	2.0 0.0 1.0		31	8 		0.0 0.9 1.0		0.1

; vary wdur
s 6
i1 0 5.0 -10.0 10.0	2.0 0.0 4.0		31	8 		0.0 0.5 4.0		0.5
s 6
i1 0 5.0 -10.0 10.0	2.0 0.0 -4.0	31	8 		0.0 0.5 -4.0	0.5
s 6
i1 0 5.0 -10.0 10.0	2.0 0.0 2.0		31	8 		0.0 0.5 2.0		1.0
s 6
i1 0 5.0 -10.0 10.0	2.0 0.0 -2.0	31	8 		0.0 0.5 -2.0	1.0
e