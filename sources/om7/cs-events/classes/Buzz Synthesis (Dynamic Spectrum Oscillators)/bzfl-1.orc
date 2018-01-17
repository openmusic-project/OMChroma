;;=============================================================================
;;			BZFL-1.ORC
;; GBUZZ GENERATOR FILTERED BY A FOF FILTER / MONO
;; AMPLITUDE ENVELOPE WITH POSCIL
;;=============================================================================
;;
;; Timbre:    Formantic subtractive synthesis with buzz
;; Synthesis: (g)buzz with fofilter
;;            POSCIL envelopes
;; Coded:     ms 8/08

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
;	p6	= centre freq of the filter [Hz]
;	p7	= amp envelope [GEN number]
;	p8	= lowest harmonic in the buzz [int]
;	p9	= % of highest harmonic [0-1]
;	p10	= multiplier in the series of amp coeffs [0-1]
;	p11	= envlp for the multiplier in the series of amp coeffs [GEN]
;	p12	= impulse response attack time (krise) [sec]
;	p13	= impulse respons decay time (atten) [sec]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f5	large cosine
;_____________________________________________________________________________

;-----------------------------------------------------------------------------
; PARAM DESCRIPTION :
; @PARAM 2 "e-dels" number 0.0 "action time [sec]"
; @PARAM 3 "durs" number 1.0 "duration [sec]"
; @PARAM 4 "amp" number -6.0 "maximum amplitude [linear (0.0,1000.0) or dB (-INF,0.0)]"
; @PARAM 5 "f0" number 263.8 "frequency [Hz]"
; @PARAM 6 "freq" number 1000.0 "frequency [Hz]"
; @PARAM 7 "aenv" cr::cs-table (:cs-table cr::gen07 0 0 2048 1 4096 0) "amplitude envelope [GEN table]"
; @PARAM 8 "bzl" number 1 "lowest harmonic in the buzz [int]"
; @PARAM 9 "bzh" number 440.0 "% of highest harmonic [0-1]"
; @PARAM 10 "bzm" number 440.0 "multiplier in the series of amp coeffs [0-1]"
; @PARAM 11 "bzmenv" cr::cs-table (:cs-table cr::gen07 0 0 2048 1 4096 0) "envlp for the multiplier in the series of amp coeffs [GEN]"
; @PARAM 12 "win" number 0.03 "impulse response attack time (krise) [sec]"
; @PARAM 13 "wout" number 0.07 "impulse respons decay time (atten) [sec]"
;
; COMPULSORY GEN FUNCTIONS :
; @GEN ; single cosine(!) wave for the buzz
; @GEN f5 0 16777216 11 1 1
;-----------------------------------------------------------------------------

sr	= 96000
kr	= 96000
ksmps	= 1
nchnls	= 1

;0dbfs = 32767  ; 16 bits
0dbfs = 8388607 ; 24 bits

instr 1 ; -------------------------------------------------------------
idur		= p3
idurosc		= 1/p3
iamp 		= (p4 > 0.0 ? (p4*0.001*0dbfs) : (ampdbfs(p4)))
if0		= p5
ifq		= p6
iaenv		= p7
inn		= sr/2/if0			; total possible number of harmonics
ihh		= int(inn*p9)		; % of possible total
ilh		= p8				; lowest harmonic
ibzmul	= p10
ibzmenv	= p11				; envelope for the buzz
irise	= p12
idec	= p13
ifn		= 5					; stored cosine function
;OUTBEG
;OUTEND

   kenv		poscil	iamp, idurosc, iaenv		; amp envelope
   kratio	poscil	ibzmul, idurosc, ibzmenv	; kratio envelope
   
   asig    gbuzz   	kenv,if0,ihh,ilh,kratio,ifn	
   afil	   fofilter	asig, ifq, irise, idec
   asound  balance	afil, asig

;OUTBEG
           out     asound
;OUTEND

endin
