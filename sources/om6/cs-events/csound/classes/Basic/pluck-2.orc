;=============================================================================
;		PLUCK-2.ORC
; PLUCKED STRING USING THE KARPLUS-STRONG MODEL
;    (FROM ACCCI, 15_01_3.ORC) / MONO
; AMPLITUDE ENVELOPE WITH LINSEG, CONTROL OVER INTIALIZATION AND DECAY MODES
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
;	p5	= freq [Hz]
;	p6	= intended freq (buffer) [Hz]
;	p7	= % of the total duration used for the last decay [0-1]
;	p8	= table number of a stored function [GEN or 0=random]
;	p9	= method of natural decay
;		  1: simple averaging
;		  2: stretched averaging. ipar1=stretch factor of smoothing time
;		  3: simple drum. ipar1=roughness factor [0-1]
;				        0=plucked string
;						1=reverse the polarity of each sample (oct down, odd harms)
;						0.5=optimum drum
;		  4: stretched drum. ipar1=roughness, ipar2=stretch factor
;		  5: weighted averaging. ipar1=weight for the current sample
;					 ipar2: weight for the previous adjacent one.
;					 NB: ipar1+ipar2<=1.0
;		  6: 1st order recursive filter with coef 0.5
;	p10	= ipar1 (used by p9, see above)
;	p11	= ipar2 (used by p9, see above)
;NB: plucked strings (1,2,5,6) are best realised by starting with random noise
;      (p8=0). Drum sounds (3,4) work best with a flat source (wide pulse).
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
; none
;_____________________________________________________________________________

; CLASS: PLUCK-2

;  GLOBAL KEYWORDS (default values within parentheses):
;	NUMROWS		: amount of rows (components) in the event (1)
;	ACTION-TIME	: start time of the whole event [sec] (0.0)
;	USER-FUN	: user-defined parsing function (nil)

;  LOCAL KEYWORDS:
;	E-DELS	: entry delays [sec] (0.0)
;	DURS	: duration [sec] (1.0)
;	AMP 	: amp [lin, >0.0-1000.0 or dB <- 0.0] (-6.0)
;	FREQ	: resampling freq [Hz] (440.0)
;	BUF 	: intended freq (buffer) [Hz] (220.0)
;	DEC%	: % of the total dur used for the last decay [0-1] (0.8)
;	RANFUN	: table number of a stored function [GEN or 0=random] (0)
;	METH	: method of natural decay [1,2,3,4,5,6] (4)
;	PAR1	: parameter required by some values of METH (0.5)
;	PAR2	: parameter required by some values of METH (10.0)
;*****************************************************************************

sr	= 96000
kr	= 96000
ksmps	= 1
nchnls	= 1

;0dbfs = 32767  ; 16 bits
0dbfs = 8388607 ; 24 bits

instr 1 ; -------------------------------------------------------------

idur	= p3
iamp 	= (p4 > 0.0 ? (p4*0.001*0dbfs) : (ampdbfs(p4)))
ifq		= p5		; frequency
ibuf	= p6		; buffer size

					; if > freq, sounds more muffled
	
				; if very high, gets lower in freq
					; if < freq, sounds more brilliant
					; if very low, start of sound is distorted
idec	= p7		; percentage [0-1] of idur for the last decay
					; affect the nature of damping
iranfun	= p8				; random noise
imeth	= p9				; simple averaging
ipar1	= p10
ipar2		= p11

iseg1	= (1-idec)*idur
iseg2	= idec*idur
;OUTBEG
;OUTEND

      kamp   	linseg   iamp,iseg1,iamp,iseg2,0
      asound	pluck    kamp, ifq, ibuf, iranfun, imeth, ipar1, ipar2

;OUTBEG
             out      asound
;OUTEND

endin
