;=============================================================================
;		ADD-2.ORC
; SIMPLE ADDITIVE SYNTHESIS (FROM ACCCI, 02_41_1.ORC) / MONO
; AMPLITUDE ENVELOPE WITH LINEN
;=============================================================================
; Timbre:    Brass
; Synthesis: Additive, same building blocks units
;            Basic instrument with added random frequency variation
; Source:    #200, Brass-like Sounds through Independent Control of
;            Harmonics, Risset (1969)
; Coded:     jpg 8/93, modified ms 9/02, 8/08, 11/15

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
;	p6	= attack time of the amp envlp [sec]
;	p7	= decay time of the amp envlp [sec]
;	p8	= spectral scaler for the fund freq
;	p9	= % of jitter (low-freq random freq variation) [0-1]
;	p10	= frequency of the jitter [Hz]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;	f1	audio wave
;_____________________________________________________________________________

; CLASS: ADD2

;  GLOBAL KEYWORDS (default values within parentheses):
;	NUMROWS		: amount of rows (components) in the event (1)
;	ACTION-TIME	: start time of the whole event [sec] (0.0)
;	USER-FUN	: user-defined parsing function (nil)

;  LOCAL KEYWORDS:
;   E-DELS	: entry delays [sec] (0.0)
;	DURS	: duration [sec] (1.0)
;	AMP		: amp [lin, >0.0-1000.0 or dB <- 0.0] (-6.0)
;	F0		: fundamental freq [Hz] (440.0)
;	ATK		: attack time [sec] (0.003)
;	DEC		: decay time [sec] (0.01)
;	SCAL	: spectral scaler for the fund freq (1.0)
;	JTA		: amplitude of the jitter [% of fund freq] (0.06)
;	JTF		: freq jitter [Hz] (10.0)
;*****************************************************************************

sr	= 96000
kr	= 96000
ksmps	= 1
nchnls	= 1

;0dbfs = 32767  ; 16 bits
0dbfs = 8388607 ; 24 bits

instr 1 ; -------------------------------------------------------------

idur    =  p3
iamp = (p4 > 0.0 ? (p4*0.001*0dbfs) : (ampdbfs(p4)))
ifund   =  p5
ifq	=  ifund * p8
imindec	= 0.01
irise   =  p6					; steep rise  (.5 to 3 ms)
idec    =  p7					; steep decay (4 to 11 ms)
idec	= (((irise+idec)>idur) ? idur-irise : idec)
if idec > imindec goto goon
; very short duration
irise = idur/2
idec = idur/2
goon:
;OUTBEG
;OUTEND

iran    =  p9
irfq	= p10
iampran =  ifund * iran

	afqr	randi   iampran, irfq
	aenv	linen   iamp, irise, idur, idec
	asound	poscil  aenv, ifq + afqr, 1

;OUTBEG
	out     asound
;OUTEND

endin
