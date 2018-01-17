;;=============================================================================
;;		PLUCK-2.ORC
;; PLUCKED STRING USING THE KARPLUS-STRONG MODEL
;;    (FROM ACCCI, 15_01_3.ORC) / MONO
;; AMPLITUDE ENVELOPE WITH LINSEG, CONTROL OVER INTIALIZATION AND DECAY MODES
;;=============================================================================
;;
;; Timbre:       Plucked string
;; Synthesis:    Karplus-Strong algorithm
;;               PLUCK
;;               LINSEG envelope, cembalo sounds
;; Coded:     	jpg 8/93, modified ms 9/02, 8/08
;;
;; DECAY METHODS (<meth>):
;;		  1: simple averaging
;;		  2: stretched averaging. par1=stretch factor of smoothing time
;;		  3: simple drum. par1=roughness factor [0-1]
;;				        0=plucked string
;;						1=reverse the polarity of each sample (oct down, odd harms)
;;						0.5=optimum drum
;;		  4: stretched drum. par1=roughness, par2=stretch factor
;;		  5: weighted averaging. par1=weight for the current sample
;;					 par2: weight for the previous adjacent one.
;;					 NB: par1+par2<=1.0
;;		  6: 1st order recursive filter with coef 0.5

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
;	p10	= par1 (used by p9, see above)
;	p11	= par2 (used by p9, see above)
;
; NB: plucked strings (1,2,5,6) are best realised by starting with random noise
;      (p8=0). Drum sounds (3,4) work best with a flat source (wide pulse).
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
; none
;_____________________________________________________________________________

;-----------------------------------------------------------------------------
; PARAM DESCRIPTION :
; @PARAM 2 "e-dels" number 0.0 "action time [sec]"
; @PARAM 3 "durs" number 1.0 "duration [sec]"
; @PARAM 4 "amp" number -6.0 "maximum amplitude [linear (0.0,1000.0) or dB (-INF,0.0)]"
; @PARAM 5 "freq" number 440.0 "frequency [Hz]"
; @PARAM 6 "buf" number 220.0 "intended freq (buffer) [Hz]"
; @PARAM 7 "dec" number 0.8 "% of the total duration used for the last decay [0-1]"
; @PARAM 8 "ranfun" number 0 "table number of a stored function [GEN or 0=random]"
; @PARAM 9 "meth" number 4 "method of natural decay [1,2,3,4,5,6]"
; @PARAM 10 "par1" number 0.5 "parameter required by some values of <meth>"
; @PARAM 11 "par2" number 10.0 "parameter required by some values of <meth>"
;-----------------------------------------------------------------------------

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
