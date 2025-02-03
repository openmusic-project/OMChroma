;=============================================================================
;		DKGR-1.ORC
; SYNCHRONOUS GRANULAR SYNTHESIS USING A SOUNDFILE AS A SOURCE / MONO
; AMPLITUDE ENVELOPE WITH POSCILI + COSINE IN-OUT
;=============================================================================

; Timbre:       Reading a sound file into a deferred table, with transposition
; Synthesis:    Sampler
; Coded:     	ms 8/08

; This instrument will loop through a deferred GEN01 table which will match
;   the exact duration of the sound file.
; If the duration in the score is longer than the file, it will read the file
;   until loop-end, then loop between loop-beg and loop-end until the end
;   of the duration.
; There is no release loop in this version.
; Loop-beg and loop-end may be given either in samples or in % [0-1]

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
;	p4	= maximum amplitude [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= frequency [1=same as the file, 0.5=octave below, 2=octave above]
;	p6	= sound file [GEN01]
;	p7	= starting point in the table [sec, 0.0]
;	p8	= amplitude envelope [GEN, straight line]
;	p9	= duration of the local attack/decay time [sec, 0.01]
;	p10	= beginning of loop [smpls or %] (0.0 = beg of file)
;	p11	= end of loop [smpls or %] (1.0 = end of file)
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;	f19	sigmoid rise/decay
;_____________________________________________________________________________

; sigmoid rise/decay
f18 0  65536  20 2 1

; straight line
f8	0 513	7 1 512 1
; triangle line
f7	0 513	7 0 256 1 256 0

;_____________________________________________________________________________

; score ******************************************************************
; timescale pitchscale 
;               f0 freq afil             skp spd aenv wdur wenv
i1 0.0 5.0 -6.0 5.0 1.0 "basmba.aiff" 0.0 1.0 8 	  1.0 18 
;i 1 + 5 2 1 
;i 1 + 5 1 0.75 
;i 1 + 5 1.5 1.5 
;i 1 + 5 0.5 1.5 
e 
