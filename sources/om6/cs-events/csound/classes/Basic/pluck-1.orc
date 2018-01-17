;=============================================================================
;		PLUCK-1.ORC
; PLUCKED STRING USING THE KARPLUS-STRONG MODEL
;    (FROM ACCCI, 15_01_3.ORC) / MONO
; AMPLITUDE ENVELOPE WITH LINSEG
;=============================================================================

; Timbre:       Plucked string
; Synthesis:    Karplus-Strong algorithm
;               PLUCK
;               LINSEG envelope, cembalo sounds
; Coded:     	jpg 8/93, modified ms 9/02, 8/08

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
;	p4	= maximum amp [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= frequency [Hz]
;	p6	= % of the total duration used for the last decay [0-1]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
; none
;_____________________________________________________________________________

; CLASS: PLUCK-1

;  GLOBAL KEYWORDS (default values within parentheses):
;	NUMROWS		: amount of rows (components) in the event (1)
;	ACTION-TIME	: start time of the whole event [sec] (0.0)
;	USER-FUN	: user-defined parsing function (nil)

;  LOCAL KEYWORDS:
;	E-DELS	: entry delays [sec] (0.0)
;	DURS	: duration [sec] (1.0)
;	AMP 	: amp [lin, >0.0-1000.0 or dB <- 0.0] (-6.0)
;	FREQ	: resampling freq [Hz] (440.0)
;	DEC 	: % of the total duration used for the last decay [0-1] (0.8)
;*****************************************************************************

sr	= 96000
kr	= 96000
ksmps	= 1
nchnls	= 1

;0dbfs = 32767  ; 16 bits
0dbfs = 8388607 ; 24 bits

instr 1 ; -------------------------------------------------------------

idur		= p3
iamp 	= (p4 > 0.0 ? (p4*0.001*0dbfs) : (ampdbfs(p4)))
ifq		= p5			; frequency
ibuf	= ifq			; buffer size = frequency

						; if > freq, sounds more muffled

						; if very high, gets lower in freq
						; if < freq, sounds more brilliant

						; if very low, start of sound is distorted
idec	= p6			; percentage [0-1] of idur for the last decay
						; affect the nature of damping
iranfun		= 0				; random noise
imeth		= 1				; simple averaging
iseg1		= (1-idec)*idur
iseg2		= idec*idur
;OUTBEG
;OUTEND

      kamp   	linseg   iamp,iseg1,iamp,iseg2,0
      asound     pluck   kamp, ifq, ibuf, iranfun, imeth

;OUTBEG
             out      asound
;OUTEND

endin
