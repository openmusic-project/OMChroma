;=============================================================================
;		SMPL-a5.ORC
; SAMPLER READING FROM A DEFERRED TABLE WITH LOOP / MONO
; AMPLITUDE ENVELOPE WITH POSCIL
; FIXED TRANSPOSITION
; DYNAMIC TRANSPOSITION AND AMPLITUDE CONTROL + JITTER
;=============================================================================

; Timbre:       Reading a sound file into a deferred table, with transposition
; Synthesis:    Sampler, flooper2
; Coded:     	ms 3/09

; This instrument will loop through a deferred GEN01 table which will match
;   the exact duration of the sound file.
; If the duration in the score is longer than the file, it will read the file
;   until loop-end, then loop between loop-beg and loop-end until the end
;   of the duration.

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
;	p4	= max amplitude [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= min transposition factor [>0, 1=same as the file, 0.5=oct down, 2=oct up]
;	p6	= max transposition factor
;	p7	= envelope for the transposition factor [GEN]
;	p8	= jitter amplitude for the transposition factor [0-1]
;	p9	= duration of the transposition envelope [sec, if=0, use p3]
;	p10	= jitter amplitude for the max amp [0-1]
;	p11	= sound file [GEN01]
;	p12	= starting point in the table [sec]
;	p13	= amplitude envelope [GEN]
;	p14	= initial beginning of loop [sec]
;	p15	= initial end of loop [sec]
;	p16	= initial crossfade length [sec]
;	p17	= final end of loop [sec]
;	p18	= final beginning of loop [sec]
;	p19	= final crossfade length [sec]
;	p20	= loop mode: 0=fwd, 1=bkwd, 2=fwd+bkwd
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;	f19	bell-shaped curve
;_____________________________________________________________________________

; deferred GEN01
; sine220
f31 0 0 -1 "sine.aiff" 0 0 0 ; try with this one to check the quality of the loop
; santuri
f32 0 0 -1 "santuri.aiff" 0 0 0


; sigmoid rise/decay
f19 0  65537  19 .5 .5 270 .5

; up-tenuto-down
f9	0 513	7 0 100 1 300 1 112 0

; straight line
f8	0 513	7 1 512 1

; triangle line
f7	0 513	7 0 256 1 256 0

; ascending line
f6	0 513	7 0 512 1

;_____________________________________________________________________________

; score ******************************************************************

; with sine220_96:
; xfdur=lpdur, long, no difference
;         amp  f0mn/mx/ev/jt/dr		jta	file	skip aenv
i1	0 10.0 0.0 1.0 2.0 6 0.1 10.0 	0.1	31		0.0  9 \
;	lpbeg/end/beg1/end1/xf/xf1	lpmode
	1.0 2.0 2.0 4.0 0.5 1.0		0
s 11
; with santuri
;         amp  f0mn/mx/ev/jt/dr		jta	file	skip aenv
i1	0 10.0 0.0 1.0 2.0 6 0.1 10.0 	0.1	32		0.0  8 \
;	lpbeg/end/beg1/end1/xf/xf1/env/mode
	1.0 2.0 2.0 4.0 1.0 2.0 2

e