;;=============================================================================
;;			FOF-3.ORC
;; FOF-BASED GLISSON GRANULAR SYNTHESIS / MONO
;; AMPLITUDE ENVELOPE WITH POSCIL
;;=============================================================================
;;
;; Timbre:    Glisson granular synthesis with fof module
;; Synthesis: FOF2 (Forme d'Onde Formatique)
;;            POSCIL envelopes
;; Coded:     ms 9/04, 8/12

; NB: NEW STRUCTURE FOR THE AMPLITUDES FROM AUGUST 2008!
;    Positive value > 0.0  : linear amplitude (>0.0-1000.0)
;    0.0 or negative value : amplitude in dB (0 = maximum value)

; The apparently arbitrary amplitude range (0-1000, rather than 0-1)
;         avoids printing small values with exponential notation

; Replaced oscili with poscil (precise oscillator), ms 8/08
; Default SR = 96000, recommended precision: 24 bits
; NB1: this implementation works with both audio and sub-audio f0's
;         and allows for an independent control of tex, debatt and atten
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
;	p11	= atten [sec]
;	p12	= octaviation [=>0.0]
;	p13	= freq deviation for each grain [semitones] (2.0)
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f1	large sine tone
;	f19	sigmoid rise/decay shape
;_____________________________________________________________________________

;-----------------------------------------------------------------------------
; PARAM DESCRIPTION :
; @PARAM 2 "e-dels" number 0.0 "action time [sec]"
; @PARAM 3 "durs" number 1.0 "duration [sec]"
; @PARAM 4 "amp" number -6.0 "maximum amplitude [linear (0.0,1000.0) or dB (-INF,0.0)]"
; @PARAM 5 "f0" number 2.0 "fundamental frequency [Hz]"
; @PARAM 6 "freq" number 880.0 "formant freq [Hz]"
; @PARAM 7 "bw" number 1.5 "bandwidth [Hz]"
; @PARAM 8 "aenv" cr::cs-table (:cs-table cr::gen07 0 1 512 1) "amplitude envelope [GEN table]"
; @PARAM 9 "win" number 0.1 "tex or krise [sec]"
; @PARAM 10 "wdur" number 1.0 "total duration of the burst, see debatt [sec]"
; @PARAM 11 "wout" number 0.2 "atten (decay of the FOF) [sec]"
; @PARAM 12 "oct" number 1.0 "octaviation [>=0.0]"
; @PARAM 13 "fdev" number 1.0 "freq deviation for each grain [semitones] (2.0)"
;
; COMPULSORY GEN FUNCTIONS :
; @GEN ; large sine tone
; @GEN f4  0 16777216  10  1
; @GEN ; sigmoid rise/decay
; @GEN f19 0  65536  19 .5 .5 270 .5
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
igls	= p13/12 ; 1/12 = 1 semitone
iolaps	= 100000 ; how many simultaneous FOFs can be played (takes little memory if not used)
iphs	= 0
iskip	= 0

iafun	= 4
isigfun	= 19
;OUTBEG
;OUTEND

; amplitude envelope
   kenv		poscil	iamp, idurosc, iaenv

;			xamp  xfund xform     	 kband kris   kdur    kdec   iolaps   ifna   ifnb
asig fof2	1.0, if0,  ifreq, ioct,  ibw, iwxin, iwxdur, iwxout, iolaps, iafun, isigfun, idur, iphs, igls, iskip
asound	= asig*kenv

;OUTBEG
      outc     asound
;OUTEND

endin