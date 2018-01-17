;=============================================================================
;		GRAIN-a1.ORC
; GRANULAR SYNTHESIS / MONO
; DYNAMIC CONTROLS
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
;	p5	= min density of the grains [Hz]
;	p6	= max density of the grains [Hz]
;	p7	= envelope for the density of the grains [GEN]
;	p8	= jitter amp for the density of the grains [0-1]
;	p9	= jitter max freq for the density of the grains [Hz]
;	p10	= min grain freq [1=same as file]
;	p11	= max grain freq [1=same as file]
;	p12	= envelope for the grain frequency [GEN]
;	p13	= jitter amp for the grain frequency [0-1]
;	p14	= jitter max freq for the grain frequency [Hz]
;	p15	= sound file [GEN01, not normalized]
;	p16	= amp envelope [GEN, straight line (8)]
;	p17	= min amp deviation for max amp [same as amp, additive]
;	p18	= max amp deviation for max amp [same as amp, additive]
;	p19	= envelope for the amp deviation for max amp [GEN]
;	p20	= min freq deviation [same as freq]
;	p21	= max freq deviation [same as freq]
;	p22	= envelope for the freq deviation [GEN]
;	p23	= min duration of the grain [sec]
;	p24	= max duration of the grain [sec]
;	p25	= envelope for the duration of the grain [GEN]
;	p26	= grain randomness [0=off, non 0=on]

;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;	f20	Hanning window for the grain
;_____________________________________________________________________________
; Hanning window
f20 0  65536  20 2

; sound file
;f31 0 524288 -1 "cym_96.aiff" 0 0 0
f31 0 1048576 -1 "sine220_96.aiff" 0 0 0

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

; iamp/fqd 0, f0 slows down, freq up 1 oct
;          amp f0mn/mx/ev/jta/jtf		fqmn/mx/ev/jta/jta
i1 0 5.0 -6.0	100.0 10.0 6 0.3 100.0	1.0 2.0 6 0.5 30.0 \
; afil aenv	ampdmn/mx/ev fqdmn/mx/ev	wdurmn/mx/ev	grnd
	31	9	0.0 0.06	 0.0 0.0 6		0.1 0.1 6		0
s 6
; use random with iamp/fqd, jta=0
;          amp f0mn/mx/ev/jta/jtf		fqmn/mx/ev/jta/jta
i1 0 5.0 -6.0	100.0 10.0 6 0.0 100.0	1.0 2.0 6 0.0 30.0 \
; afil aenv	ampdmn/mx/ev fqdmn/mx/ev	wdurmn/mx/ev	grnd
	31	9	0.0 -6.0 6	 0.0 0.5 6		0.1 0.1 6		0
s 6
; combine both randoms, iamp/fqd+jta
;          amp f0mn/mx/ev/jta/jtf		fqmn/mx/ev/jta/jta
i1 0 5.0 -6.0	100.0 10.0 6 0.0 100.0	1.0 2.0 6 0.0 30.0 \
; afil aenv	ampdmn/mx/ev fqdmn/mx/ev	wdurmn/mx/ev	grnd
	31	9	0.0 -6.0 6	 0.0 0.5 6		0.1 0.1 6		0
s 6

; vary random max freq
; strong random, very high freq, iamp/fqd=0
;          amp f0mn/mx/ev/jta/jtf		fqmn/mx/ev/jta/jta
i1 0 5.0 -6.0	100.0 10.0 6 0.5 500.0	1.0 2.0 6 0.5 500.0 \
; afil aenv	ampdmn/mx/ev fqdmn/mx/ev	wdurmn/mx/ev	grnd
	31	9	0.0 0.06	 0.0 0.5 6		0.1 0.1 6		0
s 6
; strong random, mid freq, iamp/fqd=0
;          amp f0mn/mx/ev/jta/jtf		fqmn/mx/ev/jta/jta
i1 0 5.0 -6.0	100.0 10.0 6 0.5 50.0	1.0 2.0 6 0.5 50.0 \
; afil aenv	ampdmn/mx/ev fqdmn/mx/ev	wdurmn/mx/ev	grnd
	31	9	0.0 0.06	 0.0 0.5 6		0.1 0.1 6		0
s 6
; strong random, low freq, iamp/fqd=0
;          amp f0mn/mx/ev/jta/jtf		fqmn/mx/ev/jta/jta
i1 0 5.0 -6.0	100.0 10.0 6 0.5 5.0	1.0 2.0 6 0.5 5.0 \
; afil aenv	ampdmn/mx/ev fqdmn/mx/ev	wdurmn/mx/ev	grnd
	31	9	0.0 0.06	 0.0 0.5 6		0.1 0.1 6		0
s 6
; vary wdur
;          amp f0mn/mx/ev/jta/jtf		fqmn/mx/ev/jta/jta
i1 0 10.0 -6.0	100.0 1.0 6 0.5 100.0	1.0 10.0 6 0.5 100.0 \
; afil aenv	ampdmn/mx/ev fqdmn/mx/ev	wdurmn/mx/ev	grnd
	31	9	0.0 0.06	 0.0 0.5 6		0.01 1.0 6		0
e