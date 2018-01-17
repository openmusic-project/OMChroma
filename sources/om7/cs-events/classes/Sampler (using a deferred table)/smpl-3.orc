;;=============================================================================
;;		SMPL-3.ORC
;; SAMPLER STORING A SOUND FILE INTO A DEFERRED TABLE [GEN01] / MONO
;; READING SAMPLES FROM THE TABLE / NOT NORMALIZED
;; AMPLITUDE ENVELOPE WITH POSCIL3 + COSINE IN-OUT
;; THIS SAMPLER WILL START THE FILE AGAIN, IF IT REACHES THE END OF THE TABLE
;;   SO BEWARE OF LONG NOTES OR HIGH TRANSPOSITION FACTORS
;;=============================================================================
;;
;; Timbre:       Reading from a sound file, with transposition
;; Synthesis:    Sampler
;; Coded:     	ms 3/09

; NB: NEW STRUCTURE FOR THE AMPLITUDES FROM AUGUST 2008!
;    Positive value > 0.0  : linear amplitude (>0.0-1000.0)
;    0.0 or negative value : amplitude in dB (0 = maximum value)

; The apparently arbitrary amplitude range (0-1000, rather than 0-1)
;         avoids printing small values with exponential notation

; Default SR = 96000, recommended precision: 24 bits
;-----------------------------------------------------------------------------
;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4	= max amplitude [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= transposition factor [1=same freq as the file]
;	p6	= sound file [GEN01]
;	p7	= starting point in file [sec]
;	p8	= amp envelope [GEN]
;	p9	= duration of the local attack/decay time [sec]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;	f19	sigmoid rise/decay
;_____________________________________________________________________________

;-----------------------------------------------------------------------------
; PARAM DESCRIPTION :
; @PARAM 2 "e-dels" number 0.0 "action time [sec]"
; @PARAM 3 "durs" number 1.0 "duration [sec]"
; @PARAM 4 "amp" number 0.0 "maximum amplitude [linear (0.0,1000.0) or dB (-INF,0.0)]"
; @PARAM 5 "f0" number 1.0 "transposition factor [1=same as original]"
; @PARAM 6 "afil" t 31 "sound file [GEN01]"
; @PARAM 7 "skip" number 0.0 "starting point in file [sec]"
; @PARAM 8 "aenv" cr::cs-table (:cs-table cr::gen07 0 1 4096 1) "amplitude envelope [GEN]"
; @PARAM 9 "win" number 0.01 "duration of the local attack/decay [sec]"
; 
; GEN FUNCTIONS :
; @GEN ; default audio file
; @GEN f31  0  0  -1 "santuri.aiff"  0 0 0
; @GEN ; sigmoid rise/decay
; @GEN f19 0  65537  19 .5 .5 270 .5
;-----------------------------------------------------------------------------
 
sr	= 96000
kr	= 96000
ksmps	= 1
nchnls	= 1

;0dbfs = 32767  ; 16 bits
0dbfs = 8388607 ; 24 bits

instr 1 ; -------------------------------------------------------------
 
idur		= p3
idurosc		= 1/idur
iamp 		= (p4 > 0.0 ? (p4*0.001*0dbfs) : (ampdbfs(p4)))
iamp		= iamp/0dbfs ; since sfile is not normalized, reduce amp between 0 and 1
ifile		= p6	; must be a GEN01, non normalized
ixpf		= p5*(sr/ftlen(ifile)) ; to match the SI of the table with transposition

iskip		= p7				; in secs
ifilesize	= nsamp(ifile)		; size of the sound file (samples)
ifiledur	= ifilesize/sr		; duration of the sound file
iphs		= iskip/ifiledur	; convert iskip to phase (0-1)

iaenv		= p8				; global amplitude envelope
ixin		= (p9 > idur/2 ? idur/2 : p9) ; local envelope
ixout		= ixin

ixmode		= 1 ; index between 0 and 1
ixoff		= 0 ; index offset
iwrap		= 0 ; do not wrap around if the end of the table is reached

isigfun		= 19	; sigmoid function
;OUTBEG
;OUTEND
 
; local envelope (for the grain)
k1	linen	1,ixin,idur,ixout
k2	tablei  k1,isigfun,ixmode,ixoff,iwrap

; global envelope
k3	poscil	iamp, idurosc, iaenv

; file reading
a1	poscil3 1.0, ixpf, ifile, iphs
asound	= a1*k2*k3

;OUTBEG
	outc     asound
;OUTEND

endin
