;=============================================================================
;		SMPL-a3.ORC
; SAMPLER STORING A SOUND FILE INTO A DEFERRED TABLE [GEN01] / MONO
; READING SAMPLES FROM THE TABLE / NOT NORMALIZED
; AMPLITUDE ENVELOPE WITH POSCIL + COSINE IN-OUT
; THIS SAMPLER WILL START THE FILE AGAIN, IF IT REACHES THE END OF THE TABLE
;   SO BEWARE OF LONG NOTES OR HIGH TRANSPOSITION FACTORS
;=============================================================================

; Timbre:       Reading from a sound file, with transposition
; Synthesis:    Sampler, poscil3
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
;	p4	= max amplitude [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= min transposition factor [1=same as the file]
;	p6	= max transposition factor
;	p7	= envelope for the transposition factor [GEN]
;	p8	= jitter amplitude for the transposition factor [0-1]
;	p9	= duration of the transposition envelope [sec, if=0, use p3]
;	p10	= jitter amplitude for the max amplitude [0-1]
;	p11	= sound file [GEN01] (santuri_96.aif)
;	p12	= starting point in file [sec]
;	p13	= amp envelope [GEN]
;	p14	= duration of the local attack/decay time [sec]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;	f19	sigmoid rise/decay
;_____________________________________________________________________________

;	f19	sigmoid rise/decay
f19 0  65537  19 .5 .5 270 .5
; deferred sound file, not normalized
f31  0 0  -1  "santuri_96.aif"	0	0	0

; OTHER GEN FUNCTIONS
; straight line
f8	0 513	7 1 512 1

; triangle line
f7	0 513	7 0 256 1 256 0

; ascending line
f6	0 513	7 0 512 1

; sine
f5	0 513	10 1
;_____________________________________________________________________________


; score ******************************************************************

;      amp f0mn/mx/ev/jt/dur jta	sf skip aenv win
; from oct to unison, 2", no jitter, straight aenv, win = 1/2 durnote
i1 0 2 0.0 1.0 2.0 6 0.0 2.0 0.0	31	0.0	8	1.0
s 3
; idem, 2", 50% jitter, straight aenv, win = 0.01
i1 0 2 0.0 1.0 2.0 6 0.5 2.0 0.0	31	0.0	8	0.01
s 3
; idem, 2", no jitter, durf0env = 1.0, straight aenv, win = 0.01
i1 0 2 0.0 1.0 2.0 6 0.0 1.0 0.0	31	0.0	8	0.01
s 3
; idem, 2", no jitter, durf0env = 1.0, triangle aenv, win = 1.0
i1 0 2 0.0 1.0 2.0 6 0.0 1.0 0.0	31	0.0	7	1.0
s 3
; idem, 2", 50% ajit, durf0env = 0.0->p3, triangle aenv, win = 0.01
i1 0 2 0.0 1.0 2.0 6 0.0 0.0 0.5	31	0.0	7	1.0
s 3

; inverse mn/mx, 5", tgl fenv, fjit=50%, ajit=10%, triangle aenv, win = 0.01
i1 0 5 0.0 2.0 1.0 7 0.0 2.0 0.1	31	0.0	7	0.01
s 6
; idem, 5", from 2", triangle fenv, fjit=50%, ajit=10%, triangle aenv, win = 0.0
i1 0 5 0.0 2.0 1.0 7 0.5 2.0 0.1	31	2.0	8	0.0
s 6

; straight mn/mx, 10", from 2", tgl fenv, no jit, straight aenv, win = 0.0
i1 0 5 0.0 1.0 2.0 7 0.0 2.0 0.0	31	2.0	8	0.0
s 6
; octave down (bkwrds), 10", from 5" (end), no jit, straight aenv, win = 0.0
i1 0 5 0.0 -0.5 -0.5 7 0.0 2.0 0.0	31	5.0	8	0.0
s 6

; vibrato, amp=-6dB, no jit
i1 0 10 -6 0.95 1.05 5	0.0 0.2	0.0	31	0.0	7	0.0
s 11

; idem, very strong
i1 0 10 0.0 0.5 2.0 5 	0.0 0.2 0.0	31	0.0	7	0.01
s 11

; idem, very strong
i1 0 10 0.0 -5.0 -0.5 6 0.1 10.0 0.2	31	0.0	8	0.01
e