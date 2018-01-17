;;=============================================================================
;;			FOF-1.ORC
;; FORMANTIC WAVEFORM (FROM ACCCI, 45_01_2.ORC) / MONO
;; AMPLITUDE ENVELOPE WITH POSCIL
;; NO CONTROL OF THE DETAILS OF THE FOF
;;=============================================================================
;;
;; Timbre:    Granular synthesis with fof module, voice-like tones
;; Synthesis: FOF (Forme d'Onde Formatique)
;;            POSCIL envelopes
;; Coded:     jpg 8/92, modified ms 9/02, 8/08

; NB: NEW STRUCTURE FOR THE AMPLITUDES FROM AUGUST 2008!
;    Positive value > 0.0  : linear amplitude (>0.0-1000.0)
;    0.0 or negative value : amplitude in dB (0 = maximum value)

; The apparently arbitrary amplitude range (0-1000, rather than 0-1)
;         avoids printing small values with exponential notation

; Replaced oscili with poscil (precise oscillator), ms 8/08
; Default SR = 96000, recommended precision: 24 bits
; NB1: this implementation works better with audio fundamental frequencies
;-----------------------------------------------------------------------------
;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4	= max amp [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= fundamental freq [Hz]
;	p6	= formant freq [Hz]
;	p7	= bandwidth [Hz]
;	p8	= amp envelope [GEN number]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f4	large non interpolating sine tone
;	f19	sigmoid rise/decay shape
;_____________________________________________________________________________


;-----------------------------------------------------------------------------
; PARAM DESCRIPTION :
; @PARAM 2 "e-dels" number 0.0 "action time [sec]"
; @PARAM 3 "durs" number 1.0 "duration [sec]"
; @PARAM 4 "amp" number -6.0 "maximum amplitude [linear (0.0,1000.0) or dB (-INF,0.0)]"
; @PARAM 5 "f0" number 220.0 "fundamental frequency [Hz]"
; @PARAM 6 "freq" number 609.0 "formant freq [Hz]"
; @PARAM 7 "bw" number 77.0 "bandwidth [Hz]"
; @PARAM 8 "aenv" cr::cs-table (:cs-table cr::gen07 0 0 80 1 3600 1 4096 0) "amplitude envelope [GEN table]"
;
; COMPULSORY GEN FUNCTIONS :
; @GEN ; large non interpolating sine tone
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

instr 1 ; -------------------------------------------------------------
idur   	= p3
idurosc	= 1/p3
iamp	= (p4 > 0.0 ? (p4*0.001*0dbfs) : (ampdbfs(p4)))
if0 	= p5
ifreq	= p6
ibw 	= p7
iaenv	= p8
iolaps	= 100000 ; how many simultaneous FOFs can be played (takes little memory if not used)

; fixed values for vocal synthesis
iris	= 0.003
idebatt	= 0.02
iatten	= 0.007
iafun	= 4
isigfun	= 19
;OUTBEG
;OUTEND

   kenv	poscil	iamp, idurosc, iaenv

   ;             	              koct                       			ifna    idur
   ;          	xamp  xfund xform     kband kris  kdur kdec iolaps   ifnb
   asound fof     1.0, if0,  ifreq, 0,  ibw, iris, idebatt, iatten, iolaps, iafun, isigfun, idur


;OUTBEG
      outc     asound*kenv
;OUTEND

endin
