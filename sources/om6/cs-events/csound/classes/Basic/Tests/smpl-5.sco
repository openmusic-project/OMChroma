;=============================================================================
;		SMPL-5.ORC
; SAMPLER READING FROM A DEFERRED TABLE WITH LOOP / MONO
; AMPLITUDE ENVELOPE WITH POSCIL
; FIXED TRANSPOSITION
;=============================================================================

; Timbre:       Reading a sound file into a deferred table, with transposition
; Synthesis:    Sampler, flooper2
; Coded:     	ms 3/09

; This instrument will loop through a deferred GEN01 table (deferred tables
;   exactly match the duration of the sound file).
; If the duration in the score is longer than the file, it will read the file
;   until loop-end, then loop between loop-beg and loop-end until the end
;   of the note's duration.

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
;	p5	= frequency [>0, 1=same as original, 0.5=octave below, 2=octave above]
;	p6	= sound file [GEN01]
;	p7	= starting point in the table [sec]
;	p8	= amplitude envelope [GEN]
;	p9	= beginning of loop [sec]
;	p10	= end of loop [sec]
;	p11	= crossfade length [sec]
;	p12	= loop mode: 0=fwd, 1=bkwd, 2=fwd+bkwd
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;	f19	bell-shaped curve
;_____________________________________________________________________________

; deferred GEN01
f31 0 0 -1 "santuri.aiff" 0 0 0
f32 0 0 -1 "sine.aiff" 0 0 0 ; try with this one to check the quality of the loop

; sigmoid rise/decay
f19 0  65537  19 .5 .5 270 .5

; straight line
f8	0 513	7 1 512 1

; triangle line
f7	0 513	7 0 256 1 256 0

; ascending line
f6	0 513	7 0 512 1
f19	0 513	7 0 512 1

;_____________________________________________________________________________

; score ******************************************************************

;         amp  f0 file	skip aenv lpbeg/end/xfdur/env/mode
; with sine (5 sec):
; xfdur=lpdur, long, no difference
i1	0 10.0 -6  1.0	32	0.0  8	1.0 3.0 1.0 0
s 11
; xfdur=lpdur, long, no difference, mode=1
i1	0 10.0 -6  1.0	32	0.0  8	1.0 3.0 1.0 1
s 11
; xfdur=lpdur, long, no difference, mode=2
i1	0 10.0 -6  1.0	32	0.0  8	1.0 3.0 1.0 2
s 11

; xfdur<lpdur, very short, xfd is heard
i1	0 5.0 -6 1.0	32	0.0  8	1.0 2.0 0.01 0
s 6
; xfdur<lpdur, very short, xfd is heard, mode=1
i1	0 5.0 -6 1.0	32	0.0  8	1.0 2.0 0.01 1
s 6
; xfdur<lpdur, very short, xfd is heard, mode=2
i1	0 5.0 -6 1.0	32	0.0  8	1.0 2.0 0.01 2
s 6

; xfdur<lpdur, very short, xfd is heard, fast repetitions (0.1 sec)
i1	0 5.0 -6 1.0	32	0.0  8	2.1 2.2 0.01 0
s 6
i1	0 5.0 -6 1.0	32	0.0  8	2.1 2.2 0.01 1
s 6
i1	0 5.0 -6 1.0	32	0.0  8	2.1 2.2 0.01 2
s 6

; xfdur=lpdur, short, ok
i1	0 5.0 -6 1.0	32	0.0  8	2.1 2.2 0.1 0
s 6
i1	0 5.0 -6 1.0	32	0.0  8	2.1 2.2 0.1 1
s 6
i1	0 5.0 -6 1.0	32	0.0  8	2.1 2.2 0.1 2
s 6

; xfdur=lpdur=2 cycles, ok
i1	0 5.0 -6 1.0	32	0.0  8	2.0 2.1 0.1 0
s 6
i1	0 5.0 -6 1.0	32	0.0  8	2.0 2.1 0.1 1
s 6
i1	0 5.0 -6 1.0	32	0.0  8	2.0 2.1 0.1 2
s 6

; with santuri
f32 0 0 -1 "santuri_96.aiff" 0 0 0
; xfdur=lpdur, long, no difference
i1	0 10.0 0.0  1.0	32	0.0  8	1.0 3.0 1.0 0
s 11
; xfdur=lpdur, long, no difference, mode=1
i1	0 10.0 0.0  1.0	32	0.0  8	1.0 3.0 1.0 1
s 11
; xfdur=lpdur, long, no difference, mode=2
i1	0 10.0 0.0  1.0	32	0.0  8	1.0 3.0 1.0 2
s 11

; xfdur<lpdur, very short, xfd is heard
i1	0 5.0 0.0 1.0	32	0.0  8	1.0 2.0 0.01 0
s 6
; xfdur<lpdur, very short, xfd is heard, mode=1
i1	0 5.0 0.0 1.0	32	0.0  8	1.0 2.0 0.01 1
s 6
; xfdur<lpdur, very short, xfd is heard, mode=2
i1	0 5.0 0.0 1.0	32	0.0  8	1.0 2.0 0.01 2
s 6

; xfdur<lpdur, very short, xfd is heard, fast repetitions (0.1 sec)
i1	0 5.0 0.0 1.0	32	0.0  8	2.1 2.2 0.01 0
s 6
i1	0 5.0 0.0 1.0	32	0.0  8	2.1 2.2 0.01 1
s 6
i1	0 5.0 0.0 1.0	32	0.0  8	2.1 2.2 0.01 2
s 6

; xfdur=lpdur, short, ok
i1	0 5.0 0.0 1.0	32	0.0  8	2.1 2.2 0.1 0
s 6
i1	0 5.0 0.0 1.0	32	0.0  8	2.1 2.2 0.1 1
s 6
i1	0 5.0 0.0 1.0	32	0.0  8	2.1 2.2 0.1 2
s 6

; xfdur=lpdur=2 cycles, ok
i1	0 5.0 0.0 1.0	32	0.0  8	2.0 2.1 0.1 0
s 6
i1	0 5.0 0.0 1.0	32	0.0  8	2.0 2.1 0.1 1
s 6
i1	0 5.0 0.0 1.0	32	0.0  8	2.0 2.1 0.1 2
s 6

i1	0 5.0 -1.0 1.0	31	2.0  7	2.5 3.5 1.0 2 ; skip 2"
e