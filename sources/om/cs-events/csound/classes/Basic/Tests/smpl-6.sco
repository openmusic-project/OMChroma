;=============================================================================
;		SMPL-6.ORC
; SAMPLER READING FROM A DEFERRED TABLE WITH LOOP / MONO
; AMPLITUDE ENVELOPE WITH POSCIL
; FIXED TRANSPOSITION
; SAME AS SMPL-5.ORC, BUT WITH RELATIVE STARTING AND LOOP POINTS
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
;	p5	= frequency [>0, 1=same as original, 0.5=octave below, 2=octave above]
;	p6	= sound file [GEN01]
;	p7	= starting point in the table [%]
;	p8	= amplitude envelope [GEN]
;	p9	= beginning of loop [%] (0.0=beg of file)
;	p10	= end of loop [%] (1.0=end of file)
;	p11	= crossfade length [%]
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

;_____________________________________________________________________________

; score ******************************************************************

;         amp  f0 file	skip aenv lpbeg/end/xfdur/env/mode
; with sine220_96:
i1	0 10.0 0.0 2.0	32	0.0  7	0.0 1.0 1.0 1 ; bkwd loop
s 11
i1	0 10.0 0.0 2.0	32	0.0  7	0.5 0.6 0.1 0 ; short xf from mid
s 11
i1	0 10.0 0.0 2.0	32	0.5  7	0.7 0.9 0.2 2 ; short xf @ end, mode 2
s 11

; with santuri
i1	0 7.0 0.0 2.0	31	0.0  7	0.3 0.7 0.4 1 ; bkwd loop
s 8
; xfdur<lpdur, very short, xfd is heard
i1	0 5.0 0.0 2.0	31	0.0  7	0.5 0.6 0.1 0
s 6
; xfdur<lpdur, very short, xfd is heard, fast repetitions (0.2 sec)
i1	0 5.0 0.0 2.0	31	0.5  7	0.7 0.9 0.2 2
e