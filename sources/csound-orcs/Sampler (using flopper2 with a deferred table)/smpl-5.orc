;;=============================================================================
;;		SMPL-5.ORC
;; SAMPLER READING FROM A DEFERRED TABLE WITH LOOP / MONO
;; AMPLITUDE ENVELOPE WITH POSCIL
;; FIXED TRANSPOSITION
;;=============================================================================
;;
;; Timbre:       Reading a sound file into a deferred table, with transposition
;; Synthesis:    Sampler, flooper2
;; Coded:     	ms 3/09
;;
;; This instrument will loop through a deferred GEN01 table (deferred tables
;;   exactly match the duration of the sound file).
;; If the duration in the score is longer than the file, it will read the file
;;   until loop-end, then loop between loop-beg and loop-end until the end
;;   of the note's duration.

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
;	p5	= frequency [>=0, 1=same as original, 0.5=octave below, 2=octave above]
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

;-----------------------------------------------------------------------------
; PARAM DESCRIPTION :
; @PARAM 2 "e-dels" number 0.0 "action time [sec]"
; @PARAM 3 "durs" number 1.0 "duration [sec]"
; @PARAM 4 "amp" number 0.0 "maximum amplitude [linear (0.0,1000.0) or dB (-INF,0.0)]"
; @PARAM 5 "f0" number 2.0 "transposition factor [1=same as original]"
; @PARAM 6 "afil" t 31 "sound file [GEN01]"
; @PARAM 7 "skip" number 0.5 "starting point in file [%, 0-1]"
; @PARAM 8 "aenv" cr::cs-table (:cs-table cr::gen07 :points ((0 1) (4096 1))) "amplitude envelope [GEN] (straight line=1)"
; @PARAM 9 "lpbeg" number 0.2 "beginning of loop [sec]"
; @PARAM 10 "lpend" number 0.7 "end of loop [sec]"
; @PARAM 11 "win" number 0.5 "crossfade length [sec]"
; @PARAM 12 "mode" number 2 "loop mode [0:fwd, 1:bkwd, 2:fwd+bkwd]"
; 
; GEN FUNCTIONS :
; @GEN ; default audio file
; @GEN f31  0  524288  -1 "santuri.aiff"  0 0 0
; @GEN ; sigmoid rise/decay
; @GEN f19 0  65537  19 .5 .5 270 .5
;-----------------------------------------------------------------------------



sr	= 96000
kr	= 96000
ksmps	= 1
nchnls	= 1

;0dbfs = 32767	; 16 bits
0dbfs = 8388607	; 24 bits

instr 1 ; -------------------------------------------------------------

idur	= p3
idurosc	= 1/idur
iamp 	= (p4 > 0.0 ? (p4*0.001*0dbfs) : (ampdbfs (p4)))
iamp	= iamp/0dbfs ; since sfile is not normalized, reduce amp between 0 and 1
ixpf	= p5
ifile	= p6	; must be a GEN01
iskip	= p7	; secs

iaenv	= p8	; global amplitude envelope
ilpbeg	= p9
ilpend	= p10
ilpxf	= p11
ilpmode	= p12

iskp	= 0 ; do not skip initialization

isigfun	= 19	; sigmoid function
;OUTBEG
;OUTEND

; global amplitude envelope
k3	poscil	iamp, idurosc, iaenv ; file reading

;a1	lposcil	1.0, ixpf, ilpbeg, ilpend, ifile, iphs
a1	flooper2	1.0, ixpf, ilpbeg, ilpend, ilpxf, ifile, iskip, ilpmode, isigfun, iskp

asound	= a1*k3

;OUTBEG
out     asound
;OUTEND

endin