;=============================================================================
;		SMPL-a2.ORC
; SAMPLER READING FROM A SOUND FILE WITH AUTOMATIC SR CONVERSION / MONO
; READING SAMPLES THROUGH DISKIN2, NO LOOP
; CONTROLLABLE WRAP
; AMPLITUDE ENVELOPE WITH POSCILI + COSINE IN-OUT
; DYNAMIC TRANSPOSITION AND AMPLITUDE CONTROL + JITTER
; SAME AS SMPL-A1, BUT WITH RELATIVE SKIP TIME (USEFUL FOR REVERSE)
;=============================================================================

; Timbre:       Reading from a sound file, with transposition
; Synthesis:    Sampler
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
;	p5	= min transposition factor [1=same as the file]
;	p6	= max transposition factor [1=same as the file]
;	p7	= envelope for the transposition factor [GEN]
;	p8	= jitter amplitude for the transposition factor [0-1]
;	p9	= duration of the transposition envelope [sec]
;	p10	= jitter amplitude for the max amplitude [0-1]
;	p11	= sound file [name]
;	p12	= starting point in file [%]
;	p13	= amp envelope [GEN, straight line (8)]
;	p14	= duration of the local attack/decay [sec, 0.01]
;	p15	= wrap [0 or non 0]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;	f19	sigmoid rise/decay
;_____________________________________________________________________________

; OTHER GEN FUNCTIONS

; sigmoid
f19 0  65537  19 .5 .5 270 .5

; triangle line
f7	0 513	7 0 256 1 256 0

; straight line
f8	0 513	7 1 512 1

; ascending line
f9	0 513	7 0 512 1

; sine
f10 0 513 10 1
;_____________________________________________________________________________


; score ******************************************************************

; start from end always
;   amp f0mn/mx/ev/jt/dur		jta afile 			 	skip aenv win wrap
i1	0 3 0.0	-1.0 -2.0 9 0.1 3.0	0.1 "santuri_96.aiff"	1.0	8	1.0 0 ; gliss one octave
i1	+ 2 0.0	-0.97 -1.03 10 0.1 0.2	0.1 "santuri_96.aiff"	1.0	8	1.0 0 ; vibrato
i1	+ 3 0.0	-1.0 -1.0	9 0.5 3		0.0 "santuri_96.aiff"	1.0	8	1.0 0 ; big fq jitter
i1	+ 3 0.0	-1.0 1.0	9 0.0 3		1.0 "santuri_96.aiff"	1.0	8	1.0 0 ; big amp jitter+stop
i1	+ 5 0.0	-1.0 -1.0	9 0.0 3		1.0 "santuri_96.aiff"	1.0	8	1.0 1 ; idem from 4" with warp
i1	0 5 0.0	1.0 1.0	9 0.0 3		1.0 "santuri_96.aiff"	-0.5	8	1.0 1 ; idem with silence
e