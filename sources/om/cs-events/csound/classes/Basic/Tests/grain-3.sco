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
;f31 0 524288 -1 "cym_96.aiff" 0 0 0
;f31 0 1048576 -1 "sine220_96.aiff" 0 0 0
f31 0 262144 -1 "fox_96.aiff" 0 0 0

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

; f0 = 10Hz
; small fqd
;        amp   f0	freq fqd fqdis afil aenv	ph phd phdis	wdur
i1 0 5.0 0.0 10.0	1.0 0.02 4.0	31	9 		0.0 0.02 4.0	0.2
s 6
i1 0 5.0 0.0 10.0	1.0 0.02 -4.0	31	9 		0.0 0.02 -4.0	0.2
s 6
; medium fqd
;        amp   f0	freq fqd fqdis afil aenv	ph phd phdis	wdur
i1 0 5.0 0.0 10.0	1.0 0.1 4.0		31	9 		0.0 0.1 4.0		0.2
s 6
i1 0 5.0 0.0 10.0	1.0 0.1 -4.0	31	9 		0.0 0.1 -4.0	0.2
s 6
; large fqd
;        amp   f0	freq fqd fqdis afil aenv	ph phd phdis	wdur
i1 0 5.0 0.0 10.0	1.0 0.5 4.0		31	9 		0.0 0.5 4.0		0.2
s 6
i1 0 5.0 0.0 10.0	1.0 0.5 -4.0	31	9 		0.0 0.5 -4.0	0.2
s 7

; f0 = 100Hz
; small fqd
;        amp   f0	freq fqd fqdis afil aenv		ph phd phdis	wdur
i1 0 5.0 -3.0 100.0	1.0 0.02 4.0	31	9 		0.0 0.02 4.0	0.05
s 6
i1 0 5.0 -3.0 100.0	1.0 0.02 -4.0	31	9 		0.0 0.02 -4.0	0.05
s 6
; medium fqd
;        amp   f0	freq fqd fqdis afil aenv		ph phd phdis	wdur
i1 0 5.0 -3.0 100.0	1.0 0.06 4.0	31	9 		0.0 0.1 4.0		0.05
s 6
i1 0 5.0 -3.0 100.0	1.0 0.06 -4.0	31	9 		0.0 0.1 -4.0	0.05
s 6
; large fqd
;        amp   f0	freq fqd fqdis afil aenv		ph phd phdis	wdur
i1 0 5.0 -3.0 100.0	1.0 0.2 4.0		31	9 		0.0 0.5 4.0		0.05
s 6
i1 0 5.0 -3.0 100.0	1.0 0.2 -4.0	31	9 		0.0 0.5 -4.0	0.05
e