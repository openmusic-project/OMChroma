;=============================================================================
;		GRAIN-1.ORC
; GRANULAR SYNTHESIS / MONO
; BASIC CONTROLS
;=============================================================================

; Timbre:       Granular synthesis textures
; Synthesis:    grain
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
;	p6	= grain frequency [1=same as file]
;	p7	= sound file [GEN01, not normalized]
;	p8	= amp envelope [GEN, straight line (8)]
;	p9	= max amp deviation for max amp [same as amp, additive]
;	p10	= max freq deviation [same as freq]
;	p11	= duration of the grain [sec]
;	p12	= grain randomness [0=off, non 0=on]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;	f20	Hanning window for the grain
;_____________________________________________________________________________
; Hanning window
f20 0  65536  20 2

; sound file
;f31 0 524288 -1 "cym_96.aiff" 0 0 0
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

;          amp	  f0 freq afil  aenv ampd fqd	wdur grnd
i1 0 3.0 -6.0	100.0 1.0 31	9 -10.0 0.5 	0.1 0
s 4
; no ampd
i1 0 3.0 -6.0	100.0 1.0 31	9 0.0 0.5 	0.1 0
s 4
; no fqd
i1 0 3.0 -6.0	100.0 1.0 31	9 -10.0 0.0 	0.1 0
s 4
; no ampd+fqd
i1 0 3.0 -6.0	100.0 1.0 31	9 0.0 0.0		0.1 0
s 4
; idem, grnd=off
i1 0 3.0 -6.0	100.0 1.0 31	9 0.0 0.0 	0.1 1
s 4
; freq=10
i1 0 3.0 -6.0	100.0 10.0 31	9 -10.0 0.5 	0.1 0
s 4
; f0=10
i1 0 3.0 -6.0	10.0 1.0 31	9 -10.0 0.5 	0.1 0
s 4
; f0=10, wdur=1
i1 0 3.0 -6.0	5.0 1.0 31	9 -10.0 0.5 	1.0 0
s 4
; f0=10, wdur=0.01
i1 0 3.0 -6.0	5.0 1.0 31	9 -10.0 0.5 	0.01 0
s 4
; f0=5, amp/fqd off
i1 0 3.0 -6.0	5.0 1.0 31	9 0.0 0.0 	0.1 0
s 4
; f0=5, wdur=1
i1 0 3.0 -6.0	5.0 1.0 31	9 0.0 0.0 	1.0 0
s 4
; f0=50, wdur=1
i1 0 3.0 -6.0	50.0 1.0 31	9 -10.0 0.5 	1.0 0
e