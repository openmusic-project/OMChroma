;;=============================================================================
;;		SMPL-A3.ORC
;; SAMPLER STORING A SOUND FILE INTO A DEFERRED TABLE [GEN01] / MONO
;; READING SAMPLES FROM THE TABLE / NOT NORMALIZED
;; AMPLITUDE ENVELOPE WITH POSCIL + COSINE IN-OUT
;; THIS SAMPLER WILL START THE FILE AGAIN, IF IT REACHES THE END OF THE TABLE
;;   SO BEWARE OF LONG NOTES OR HIGH TRANSPOSITION FACTORS
;; DYNAMIC TRANSPOSITION AND AMPLITUDE CONTROL + JITTER
;;=============================================================================
;;
;; Timbre:       Reading from a sound file, with transposition
;; Synthesis:    Sampler, poscil3
;; Coded:     	ms 3/09

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

;-----------------------------------------------------------------------------
; PARAM DESCRIPTION :
; @PARAM 2 "e-dels" number 0.0 "action time [sec]"
; @PARAM 3 "durs" number 1.0 "duration [sec]"
; @PARAM 4 "amp" number 0.0 "maximum amplitude [linear (0.0,1000.0) or dB (-INF,0.0)]"
; @PARAM 5 "f0" number 1.0 "min transposition factor [1=same as original]"
; @PARAM 6 "f0max" t 2.0 "max transposition factor [1=same]"
; @PARAM 7 "f0env" cr::cs-table (:cs-table cr::gen07 0 0 4096 100) "envelope for the transposition factor [GEN]"
; @PARAM 8 "f0jta" number 0.1 "jitter amplitude for the transposition factor [0-1]"
; @PARAM 9 "f0dur" number 1.0 "duration of the transposition envelope [sec]"
; @PARAM 10 "jta" number 0.5 "jitter amplitude for the max amplitude [0-1]"
; @PARAM 11 "afil" number 31 "sound file [GEN01]"
; @PARAM 12 "skip" number 0.0 "starting point in file [sec]"
; @PARAM 13 "aenv" cr::cs-table (:cs-table cr::gen07 0 0 2048 100 4096 0) "amplitude envelope [GEN]"
; @PARAM 14 "win" number 0.01 "duration of the local attack/decay [sec]"
;
; COMPULSORY GEN FUNCTIONS :
; @GEN ; sigmoid rise/decay
; @GEN f19 0  65537  19 .5 .5 270 .5
; @GEN ; audio file
; @GEN f31  0  0  -1 "santuri.aiff" 0 0 0
;-----------------------------------------------------------------------------
 
sr	= 96000
kr	= 96000
ksmps	= 1
nchnls	= 1

;0dbfs = 32767  ; 16 bits
0dbfs = 8388607 ; 24 bits

instr 1 ; -------------------------------------------------------------
 
idur	= p3
idurosc	= 1/idur
iamp 	= (p4 > 0.0 ? (p4*0.001*0dbfs) : (ampdbfs(p4)))
iamp	= iamp/0dbfs ; since sfile is not normalized, reduce amp between 0 and 1
ifile	= p11	; must be a GEN01, non normalized
if0min	= p5*(sr/ftlen(ifile)) ; to match the SI of the table with transposition
if0max	= p6*(sr/ftlen(ifile))
if0d	= if0max-if0min
if0env	= p7
if0jt	= p8
if0dur	= (p9 = 0 ? p3 : p9) ; duration of xposition env, if 0, take p3

ijta	= p10
iskip	= p12
iaenv	= p13	; global amplitude envelope
iwin	= (p14 > idur/2 ? idur/2 : p14) ; local envelope
iwout	= iwin

ixmode	= 1 ; index between 0 and 1
ixoff	= 0 ; index offset
ixrap	= 0 ; no wraparound in table reading

ifilesize	= nsamp(ifile)	; size of the sound file (samples)
ifiledur	= ifilesize/sr	; duration of the sound file
iphs		= iskip/ifiledur	; convert iskip to phase (0-1)

isize		= 1	; 31-bit random number for randi
isigfun		= 19	; sigmoid function
;OUTBEG
;OUTEND
 
;f0
; jitter for f0
; seed>1.0=> seed from the system time
kf0j1	randi	if0jt, 1/0.058, 1.8135, isize
kf0j2	randi	if0jt, 1/0.117, 1.3111, isize
kf0j3	randi	if0jt, 1/1.2109, 1.6711, isize
kf0j	=	(kf0j1+kf0j2+kf0j3)/3.0

;f0 envelope
kf0env	poscil	if0d, 1/if0dur, if0env
kf0		= kf0env+if0min
kf0end	= kf0+(kf0*kf0j)

; local envelope (for the grain)
k1	linen	1,iwin,idur,iwout
k2	tablei  k1,isigfun,ixmode,ixoff,ixrap

; jitter for amp
; seed>1.0=> seed from the system time
kjta1	randi	ijta, 1/0.061, 1.8195, isize
kjta2	randi	ijta, 1/0.109, 1.3011, isize
kjta3	randi	ijta, 1/1.221, 1.6793, isize
kjta	=	(kjta1+kjta2+kjta3)/3.0

; global envelope
k3	poscil	iamp, idurosc, iaenv
kamp	= k3+(k3*kjta)

; file reading
a1	poscil3 1.0, kf0end, ifile, iphs
asound	= a1*k2*k3

;OUTBEG
	outc     asound
;OUTEND

endin
