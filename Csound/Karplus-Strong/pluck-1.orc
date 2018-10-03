;;=============================================================================
;;		PLUCK-1.ORC
;; PLUCKED STRING USING THE KARPLUS-STRONG MODEL
;;    (FROM ACCCI, 15_01_3.ORC) / MONO
;; AMPLITUDE ENVELOPE WITH LINSEG
;;=============================================================================
;;
;; Timbre:       Plucked string
;; Synthesis:    Karplus-Strong algorithm
;;               PLUCK
;;               LINSEG envelope, cembalo sounds
;; Coded:     	jpg 8/93, modified ms 9/02, 8/08

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


;-----------------------------------------------------------------------------
; PARAM DESCRIPTION :
; @PARAM 2 "e-dels" number 0.0 "action time [sec]"
; @PARAM 3 "durs" number 1.0 "duration [sec]"
; @PARAM 4 "amp" number -6.0 "maximum amplitude [linear (0.0,1000.0) or dB (-INF,0.0)]"
; @PARAM 5 "freq" number 440.0 "frequency [Hz]"
; @PARAM 6 "dec" number 0.8 "% of the total duration used for the last decay [0-1]"
;-----------------------------------------------------------------------------

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
