;;=============================================================================
;;		SMPL-a1.ORC
;; SAMPLER READING FROM A SOUND FILE WITH AUTOMATIC SR CONVERSION / MONO
;; READING SAMPLES THROUGH DISKIN2, NO LOOP
;; CONTROLLABLE WRAP
;; AMPLITUDE ENVELOPE WITH POSCILI + COSINE IN-OUT
;; DYNAMIC TRANSPOSITION AND AMPLITUDE CONTROL + JITTER
;;=============================================================================
;;
;; Timbre:       Reading from a sound file, with transposition
;; Synthesis:    Sampler
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
;	p4	= max amp [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= min transposition factor [1=same as file]
;	p6	= max transposition factor
;	p7	= envelope for the transposition factor [GEN]
;	p8	= jitter amplitude for the transposition factor [0-1]
;	p9	= duration of the transposition envelope [sec]
;	p10	= jitter amplitude for the max amplitude [0-1]
;	p11	= sound file [name]
;	p12	= starting point in file [sec]
;	p13	= amp envelope [GEN]
;	p14	= duration of the local attack/decay [sec]
;	p15	= wrap [0 or non 0]
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
; @PARAM 6 "f0max" number 2.0 "max transposition factor [1=same]"
; @PARAM 7 "f0env" cr::cs-table (:cs-table cr::gen07 0 0 32768 100 65536 0) "envelope for the transposition factor [GEN]"
; @PARAM 8 "f0jta" number 0.1 "jitter amplitude for the transposition factor [0-1]"
; @PARAM 9 "f0dur" number 1.0 "duration of the transposition envelope [sec]"
; @PARAM 10 "jta" number 0.5 "jitter amplitude for the max amplitude [0-1]"
; @PARAM 11 "afil" t "santuri.aiff" "file name [int, sound, string, pathname or GEN]"
; @PARAM 12 "skip" number 0.0 "starting point in file [sec / if < 0: adds -(SKIP/XPF) seconds]"
; @PARAM 13 "aenv" cr::cs-table (:cs-table cr::gen07 0 0 65536 100) "amplitude envelope [GEN]"
; @PARAM 14 "win" number 0.01 "duration of the local attack/decay [sec]"
; @PARAM 15 "wrap" number 1 "wrap [0: locations beyond the file produce silence / other: locations wrapped to the duration of the sound]"
;
; COMPULSORY GEN FUNCTIONS :
; @GEN ; sigmoid rise/decay
; @GEN f19 0  2097152  19 .5 .5 270 .5
;-----------------------------------------------------------------------------



sr	= 96000
kr	= 96000
ksmps	= 1
nchnls	= 1

;0dbfs = 32767  ; 16 bits
0dbfs = 8388607 ; max amp of the sound file

instr 1 ; -------------------------------------------------------------
 
idur	= p3
idurosc	= 1/idur
iamp 	= (p4 > 0.0 ? (p4*0.001) : (10^(p4/20.0)))
	; amplitude relative to the sound file (to avoid multiplying twice)
if0min 	= p5 ; min transposition factor
if0max	= p6
if0d	= if0max-if0min
if0env	= p7
if0jt	= p8
if0dur	= (p9 > 0 ? p9 : idur)

ijta	= p10
ifile	= p11
iskip	= (p12 < 0.0 ? (p12*if0min) : p12)
	; to make the value not depend on the xposition factor if it's negative
iskip	= p12
iaenv	= p13	; global amplitude envelope
iwin	= (p14 > idur/2 ? idur/2 : p14) ; local envelope
iwout	= iwin
iwrap	= p15

print iskip

ixmode	= 1 ; index between 0 and 1
ixoff	= 0 ; index offset
ixrap	= 0 ; no wraparound in table reading

iformat		= 8 ; 24-bit int, ignored if the sound file has a header
iwsize		= 512 ; the bigger, the better the quality of the transposition
ibufsize	= 262144	; maximum = 1048576, higher makes less disk access
iskipinit	= 0 ; do not skip initialization
isize		= 1	; 31-bit random number for randi
iaudiofun	= 1
isigfun		= 19 ; sigmoid function
;OUTBEG
;OUTEND
 
;f0
; jitter for f0
; seed>1.0=> seed from the system time
kf0j1	randi	if0jt, 1/0.05, 1.8135, isize
kf0j2	randi	if0jt, 1/0.111, 1.3111, isize
kf0j3	randi	if0jt, 1/1.219, 1.6711, isize
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
a1	diskin2	ifile, kf0end, iskip, iwrap, iformat, iwsize, ibufsize, iskipinit
asound = a1*k2*kamp
;OUTBEG
         outc     asound
;OUTEND

endin
