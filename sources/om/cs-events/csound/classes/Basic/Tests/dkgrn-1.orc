;=============================================================================
;		SMPL-6.ORC
; SAMPLER READING FROM A DEFERRED TABLE WITH LOOP / MONO
; AMPLITUDE ENVELOPE WITH POSCILI + COSINE IN-OUT
; FIXED TRANSPOSITION
; SAME AS SMPL-5, BUT WITH RELATIVE STARTING POINT
;=============================================================================

; Timbre:		Reading a sound file into a deferred table, with transposition
; Synthesis:	Sampler
; Coded:     	ms 8/09

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
;	p5	= density of the grain generation [grains/sec]
;	p6	= frequency [1=same as file, 0.5=octave down, <0=backwards, 0=freeze]
;	p7	= sound file [GEN01]
;	p8	= start offset from beginning of file [sec]
;	p9	= readout pointer rate [1=1 grain ahead, <1=compress, >1=expand]
;	p10	= amplitude envelope [GEN, straight line]
;	p11	= duration of the grain [sec]
;	p12	= envelope for the grain [GEN20]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;	f18	hanning window
;_____________________________________________________________________________

; CLASS: SMPL-6

;  GLOBAL KEYWORDS (default values within parenthesis):
;	NUMROWS		: amount of rows (components) in the event (1)
;	ACTION-TIME	: start time of the whole event [sec] (0.0)
;	USER-FUN	: user-defined parsing function (nil)

;  LOCAL KEYWORDS:
;   E-DELS	: entry delays [sec] (0.0)
;	DURS	: duration [sec] (1.0)
;	AMP 	: amplitude [lin, >0.0-1000.0 or dB <- 0.0] (-6.0)
;	F0  	: transposition factor [1=same frequency] (-1.0)
;	AFIL	: file name [int, string or pathname] (santuri_96.aif)
;	SKIP	: starting point in the file [%] (1.0)
;	AENV	: fun number for the amp envlp [GEN] (straight line=1)
;	WIN 	: duration of the local attack/decay [sec] (0.01)
;	LPBEG	: starting loop point [% or samples] (0.0)
;	LPEND	: ending loop point [% or samples] (0.0)
;*****************************************************************************

sr	= 96000
kr	= 96000
ksmps	= 1
nchnls	= 1

;0dbfs = 32767	; 16 bits
0dbfs = 8388607	; 24 bits

instr 1 ; -------------------------------------------------------------

idur   	= p3
idurosc	= 1/p3
iamp 	= (p4 > 0.0 ? (p4*0.001*0dbfs) : (ampdbfs(p4)))
idens	= p5
ifreq	= p6
Safil	= p7
iskip	= p8
ispd	= p9
iaenv	= p10
iwdur	= p11
iwenv	= p12

iolaps	= 100000
imxgrsz	= p3
;OUTBEG
;OUTEND

;ifreq = iolaps/igrsize 
;ips = 1/iolaps 
;istr = p4 /* timescale */ 
;ipitch = p5 /* pitchscale */ 

a1 diskgrain Safil, iamp, idens, ifreq, iwdur, ispd, iwenv, iolaps 

out a1
endin 
