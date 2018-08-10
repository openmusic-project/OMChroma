;;=============================================================================
;;		SMPL-2.ORC
;; SAMPLER READING FROM A SOUND FILE WITH AUTOMATIC SR CONVERSION / MONO
;; READING SAMPLES THROUGH DISKIN2, NO LOOP
;; CONTROLLABLE WRAP
;; AMPLITUDE ENVELOPE WITH POSCILI + COSINE IN-OUT
;; FIXED TRANSPOSITION
;; SAME AS SMPL-1, BUT WITH RELATIVE SKIP TIME (USEFUL FOR REVERSE)
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
;	p5	= transposition factor [1=same freq as the file]
;	p6	= sound file [name]
;	p7	= starting point in file [sec]
;	p8	= amp envelope [GEN]
;	p9	= duration of the local attack/decay [sec]
;	p10	= wrap [0 or non 0]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;	f19	sigmoid rise/decay
;_____________________________________________________________________________

;-----------------------------------------------------------------------------
; PARAM DESCRIPTION :
; @PARAM 2 "e-dels" number 0.0 "action time [sec]"
; @PARAM 3 "durs" number 1.0 "duration [sec]"
; @PARAM 4 "amp" number 0.0 "maximum amplitude [linear (0.0,1000.0) or dB (-INF,0.0)]"
; @PARAM 5 "f0" number -1.0 "transposition factor [1=same as original]"
; @PARAM 6 "afil" t "santuri.aiff" "sound file [name]"
; @PARAM 7 "skip" number 1.0 "starting point in file [% / if < 0: adds -(SKIP/XPF) seconds]"
; @PARAM 8 "aenv" cr::cs-table (:cs-table cr::gen07 :points ((0 0) (2048 100) (4096 0))) "amplitude envelope [GEN]"
; @PARAM 9 "win" number 0.01 "duration of the local attack/decay [sec]"
; @PARAM 10 "wrap" number 1 "wrap [0: locations beyond the file produce silence / other: locations wrapped to the duration of the sound]"
;
; COMPULSORY GEN FUNCTIONS :
; @GEN ; sigmoid rise/decay
; @GEN f19 0  65537  19 .5 .5 270 .5
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
	; relative amplitude to the sound file (to avoid multiplying twice)
if0 	= p5 ; transposition factor
ifile	= p6
;ifilepeak	filepeak ifile	; take away the semicolons if you want to read the
;print ifilepeak			;	max amp of the file on the terminal

ilen  filelen ifile
itmpsk	= p7*ilen
iskip	= (itmpsk < 0.0 ? (itmpsk*if0) : itmpsk)
	; to make the value not depend on the xposition factor if it's negative
iaenv	= p8	; global amplitude envelope
iwin	= (p9 > idur/2 ? idur/2 : p9) ; local envelope
iwout	= iwin
ixmode	= 1 ; index between 0 and 1
ixoff	= 0 ; index offset
ixrap	= 0 ; no wraparound in table reading
iwrap		= p10

iformat		= 8 ; 24-bit int, ignored if the sound file has a header
iwsize		= 512 ; the bigger, the better the quality of the transposition
ibufsize	= 262144	; maximum = 1048576, higher makes less disk access
iskipinit	= 0 ; do not skip initialization

isigfun		= 19 ; sigmoid function
;OUTBEG
;OUTEND
 
; local envelope (for the grain)
k1	linen	1,iwin,idur,iwout
k2	tablei  k1,isigfun,ixmode,ixoff,ixrap

; global envelope
k3	poscil	iamp, idurosc, iaenv

; file reading
a1	diskin2	ifile, if0, iskip, iwrap, iformat, iwsize, ibufsize, iskipinit
asound = a1*k2*k3
;OUTBEG
         outc     asound
;OUTEND

endin
