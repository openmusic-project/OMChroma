;=============================================================================
;			SNARE-1.ORC
; SIMPLE SIMULATION OF A SNARE DRUM (FROM ACCCI, 03_01_1.ORC) / MONO
;=============================================================================

; Timbre:    Drum and snare drum
; Synthesis: Additive different units
;            Units: noise / inharm / fundamental
; Source:    #400, Drum and Snare-drum like Sounds, Risset (1969)
; Coded:     jpg 8/93, modified ms 9/02, 8/08

; NB: NEW STRUCTURE FOR THE AMPLITUDES FROM AUGUST 2008!
;    Positive value > 0.0  : linear amplitude (>0.0-1000.0)
;    0.0 or negative value : amplitude in dB (0 = maximum value)

; The apparently arbitrary amplitude range (0-1000, rather than 0-1)
;         avoids printing small values with exponential notation

; Replaced oscili with poscil (precise oscillator), ms 8/08
; Default SR = 96000, recommended precision: 24 bits

; NB1: this instrument works better with short durations
;-----------------------------------------------------------------------------
;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4	= maximum amp [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= fundamental freq [Hz]
;	p6	= amp of pseudo inharmonic tone [linear, 0.0 -> 1000.0]
;	p7	= amp of the noise [linear, >0.0-1000.0 or dB, <= 0.0]
;	p8	= centre freq for the noise band [Hz]
;	p9	= noise's 1/2 bandwidth [Hz]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;	f11	noise-modulated sine wave
;	f12	sine wave with only one high partial (10th)
;	f13	pseudo-inharmonic spectrum made of high partials
;	f21	slowly descending exponential envelope
;	f22	rapidly descending exponential envelope
;_____________________________________________________________________________

; CLASS: SNARE-1

;  GLOBAL KEYWORDS (default values within parentheses):
;	NUMROWS		: amount of rows (components) in the event (1)
;	ACTION-TIME	: start time of the whole event [sec] (0.0)
;	USER-FUN	: user-defined parsing function (nil)

;  LOCAL KEYWORDS:
;	E-DELS	: entry delays [sec] (0.0)
;	DURS	: duration [sec] (0.5)
;	AMP 	: amp of sine tone [lin, >0.0-1000.0 or dB <- 0.0] (200.0)
;	FREQ	: fundamental freq [Hz] (20.0)
;	INHA	: amp of pseudo inharmonic tone [lin, >0.0-1000.0 or dB <- 0.0] (75.0)
;	JTA 	: amp of the noise [lin, >0.0-1000.0 or dB <- 0.0] (250.0)
;	JTF 	: centre freq for the noise band [Hz] (4000.0)
;	BW  	: noise's 1/2 bandwidth [Hz] (1500.0)
;*****************************************************************************

sr	= 96000
kr	= 96000
ksmps	= 1
nchnls	= 1

;0dbfs = 32767  ; 16 bits
0dbfs = 8388607 ; 24 bits


instr 1 ; --------------------------------------------------------------------

idur		=  p3
idurosc		= 1/idur
isin_amp	= (p4 > 0.0 ? (p4*0.001*0dbfs) : (ampdbfs(p4)))
if0			=  p5
inh_amp		=  (p6 > 0.0 ? (p6*0.001*0dbfs) : (ampdbfs(p6)))
iran_amp	=  (p7 > 0.0 ? (p7*0.001*0dbfs) : (ampdbfs(p7)))
iran_cfq	=  p8
iran_bw		=  p9
iranfun		=  11
inhfun		=  12
iaudiofun	=  13
islowenv	=  21
ifastenv	=  22
;OUTBEG
;OUTEND

; noise
         a4     rand    iran_amp, iran_bw
         a5     poscil  a4, idurosc, ifastenv
         a6     poscil  a5, iran_cfq, iranfun

; pseudo-inharmonic spectrum
         a3     poscil  iran_amp, idurosc, ifastenv
         a4     poscil  a3, if0, inhfun

; sine tone
         a1     oscili  isin_amp, idurosc, islowenv
         a2     poscil  a1, if0, iaudiofun

 	  asound	= a2+a4+a6

;OUTBEG
	out     asound
;OUTEND

endin
