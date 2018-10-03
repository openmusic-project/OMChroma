;;=============================================================================
;;			FOF-4.ORC
;; FOF-BASED GRANULAR SYNTHESIS / MONO
;; SAMPLED GRAIN RATHER THAN A SINE TONE IN THE FOF
;; AMPLITUDE ENVELOPE WITH POSCIL
;;=============================================================================
;;
;; Timbre:    Granular synthesis with a FOF (based on Byrne Villez's work)
;; Synthesis: FOF (Forme d'Onde Formatique)
;;            POSCIL envelopes
;; Coded:     ms 2/09

; NB: NEW STRUCTURE FOR THE AMPLITUDES FROM AUGUST 2008!
;    Positive value > 0.0  : linear amplitude (>0.0-1000.0)
;    0.0 or negative value : amplitude in dB (0 = maximum value)

; The apparently arbitrary amplitude range (0-1000, rather than 0-1)
;         avoids printing small values with exponential notation

; Replaced oscili with poscil (precise oscillator), ms 8/08
; Default SR = 96000, recommended precision: 24 bits
;-----------------------------------------------------------------------------
;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4	= max amp [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= fundamental freq [Hz]
;	p6	= formant freq [Hz]
;	p7	= bandwidth [Hz]
;	p8	= amp envelope [GEN number]
;	p9	= tex or krise [sec]
;	p10	= total duration of the burst, see debatt [sec]
;	p11	= decay of the FOF [sec]
;	p12	= octaviation [>=0.0]
;	p13	= sound file [name, sound, string, pathname or GEN01] (2.0)
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f19	sigmoid rise/decay shape
;	f32	short sample (grain), here 0.64 sec appx.
;_____________________________________________________________________________

;-----------------------------------------------------------------------------
; PARAM DESCRIPTION :
; @PARAM 2 "e-dels" number 0.0 "action time [sec]"
; @PARAM 3 "durs" number 1.0 "duration [sec]"
; @PARAM 4 "amp" number -6.0 "maximum amplitude [linear (0.0,1000.0) or dB (-INF,0.0)]"
; @PARAM 5 "f0" number 220.0 "fundamental frequency [Hz]"
; @PARAM 6 "freq" number 5.0 "formant freq [Hz]"
; @PARAM 7 "bw" number 0.0 "bandwidth [Hz]"
; @PARAM 8 "aenv" cr::cs-table (:cs-table cr::gen07 :points ((0 1) (4096 1))) "amplitude envelope [GEN]"
; @PARAM 9 "win" number 0.003 "tex or krise [sec]"
; @PARAM 10 "wdur" number 0.02 "total duration of the burst, see debatt [sec]"
; @PARAM 11 "wout" number 0.05 "decay of the FOF [sec]"
; @PARAM 12 "oct" number 0.0 "octaviation [>=0.0]"
; @PARAM 13 "afil" cr::cs-table 32 "sound file [name, sound, string, pathname or GEN01]"
;
; COMPULSORY GEN FUNCTIONS :
; @GEN ; sigmoid rise/decay
; @GEN f19 0  65536  19 .5 .5 270 .5
; @GEN ; short grain
; @GEN f32 0  32768  1 "basmba.aiff" 0 0 0
;-----------------------------------------------------------------------------

sr	= 96000
kr	= 96000
ksmps	= 1
nchnls	= 1

;0dbfs = 32767  ; 16 bits
0dbfs = 8388607 ; 24 bits
;OUTBEG
;OUTEND

instr 1 ; -------------------------------------------------------------
idur   	= p3
idurosc	= 1/p3
iamp 	= (p4 > 0.0 ? (p4*0.001*0dbfs) : (ampdbfs(p4)))
if0		= p5
ifreq	= p6
ibw		= p7
iaenv	= p8
iwxin	= p9
iwxdur	= p10
iwxout	= p11
ioct	= p12
ifile	= p13

iolaps	= 100000	; how many simultaneous FOFs can be played
					; (takes little memory if not used)
imode	= 0
iphs	= 0
iskip	= 0

isigfun	= 19
;OUTBEG
;OUTEND

; amplitude envelope
   kenv		poscil	iamp, idurosc, iaenv

;				xamp  xfund xform oct  kband kris   kdur    kdec   iolaps   ifna   ifnb
asig	fof 	1.0, if0, ifreq, ioct,  ibw, iwxin, iwxdur, iwxout, iolaps, ifile, isigfun, idur, iphs, imode, iskip
asound	= asig*kenv

;OUTBEG
      outc     asound
;OUTEND

endin