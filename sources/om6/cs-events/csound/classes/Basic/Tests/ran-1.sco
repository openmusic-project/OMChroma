;=============================================================================
;			RAN-1.ORC
; RANDOM NUMBER GENERATION MODULATING THE AMPLITUDE OF AN OSCILLATOR
;    (FROM ACCCI, 10_02_2.ORC) / MONO
; AMPLITUDE ENVELOPE WITH LINEN, CONTROL OF AUDIO FUN FROM THE SCORE
;=============================================================================

; Timbre:       Noise spectra, with control of bandwidth and center freq
; Synthesis:    Random Number Generation
;               RANDI(02)
;               LINEN envelope on RANDI ring modulates an oscillator (2)
; Source:       Dodge(1985), p.92
; Coded:        jpg 8/92. modified ms 9/02, 8/08

; NB: NEW STRUCTURE FOR THE AMPLITUDES FROM AUGUST 2008!
;    Positive value > 0.0  : linear amplitude (>0.0-1000.0)
;    0.0 or negative value : amplitude in dB (0 = maximum value)


; The apparently arbitrary amplitude range (0-1000, rather than 0-1)
;         avoids printing small values with exponential notation
; Replaced oscili with poscil (precise oscillator), ms 8/08
; Default SR = 96000, recommended precision: 24 bits

; NB 1: this instrument works better with short notes.
;-----------------------------------------------------------------------------
;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4	= maximum amp [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= freq [Hz]
;	p6	= freq of the noise module [Hz]
;	p7	= attack time of the amp envlp [sec]
;	p8	= decay time of the amp envlp [sec]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f1	sine tone
;_____________________________________________________________________________

; GEN functions **********************************************************
; AUDIO FUNCTIONS
f1  0  4097  10  1			; sinus


; score ******************************************************************

; NB: control of pitched-ness in % can be added
;    idur  iamp	ifq	iran_fq iatk idec

i1  0  10	250	400	80	0.2	0.3
i1  2  20   	.     	.	40  	.  .
i1  4  40   	.     	.	20  	 .  .

					; mainly odd partials (-> 13)
f1  6  4097  10  1  0 .3 0 .5 0 .2 .03 .1 .02 .05 .005 .02
i1  6  1	250	400	50	0.2	0.3	2
i1  8  1   	.     .	25   .  .  .
i1 10  1   	.     .	10   .  .  .

e