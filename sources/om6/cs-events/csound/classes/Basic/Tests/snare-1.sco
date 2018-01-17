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

; GEN functions **********************************************************
; waveforms
f11  0  4097  9   1  1  0
f12  0  8193  9  10  1  0
f13  0  8193  9  10  1  0  16  1  0  22  1  0  23  1  0

; envelopes
f21  0  513  5   256  512  1
f22  0  513  5  4096  512  1				; steeper slope

; score ******************************************************************

; section 1                                   
;   a-t	    dur	isin_amp	if0		inh_amp		iran_amp 	iran_cfq 	iran_bw

i1    .4    .2	180		20 		70		230		4000		1500
i1    .8    .2	.		.		.		.		.		.
i1   1.1    .15	.		.		.		.		.		.
i1   1.2    .2	.		.		.		.		.		.
i1   1.6    .2	.		.		.		.		.		.
i1   1.9    .15	.		.		.		.		.		.
i1   2.0    .2	.		.		.		.		.		.
i1   2.4    .2	.		.		.		.		.		.
i1   2.8    .2	.		.		.		.		.		.
i1   3.1    .15	.		.		.		.		.		.
i1   3.2    .2	.		.		.		.		.		.
i1   3.6    .2	.		.		.		.		.		.
i1   3.9    .15	.		.		.		.		.		.
i1   4      .2	.		.		.		.		.		.
i1   4.4    .2	.		.		.		.		.		.
i1   4.8    .2	.		.		.		.		.		.
i1   5.2    .2	.		.		.		.		.		.
i1   5.6    .2	.		.		.		.		.		.
i1   6      . 	.		.		.		.		.		1300
s

; section 2               the noise unit is inactive (iran_amp = 0.000001)
; cannot be 0, otherwise it is taken as a value in dB

;   a-t	    dur	isin_amp if0	inh_amp	iran_amp 	iran_cfq 	iran_bw
i1    .4    .3	350	12	100	0.00001       	0   		2.5
i1    .8    .2   .    16	.	.		.		.
i1   1.07   .2   .    12	.	.		.		.
i1   1.2    .2   .    16	.	.		.		.
i1   1.6    .3   .    12	.	.		.		.
i1   2.0    .25  .    14	.	.		.		.
i1   2.4    .23  .    15	.	.		.		.
i1   2.6    .27  .	.	.	.		.		.
i1   3.07   .23  .	.	.	.		.		.
i1   3.2      .  .	.	.	.		.		.
i1   3.6      .  .	.	.	.		.		.
i1   4        .  .	.	.	.		.		.
s

; section 3
;   a-t	    dur	isin_amp if0	inh_amp	iran_amp iran_cfq 	iran_bw
i1    .4    .15	200	20	175	250	4000		1500
i1    .6    .20  .	.	.	.	.		.
i1   1.07   .20  .	.	.	.	.		.
i1   1.2   .20  .	.	.	.	.		.
i1   1.6   .20  .	.	.	.	.		.
i1   2.0   .20  .	.	.	.	.		.
i1   2.4    .25  .	.	.	.	.		.
i1   2.9    .15  .	.	.	.	.		.
i1   3	     .  .	.	.	.	.		.
i1   3.1     .	  .	.	.	.	.		.
i1   3.2    .20  .	.	.	.	.		.
i1   3.55   .15   400	.	.	.	.		.
i1   3.6    .20  .	.	.	.	.		.
i1   4      .15   500	.	.	.	.		.
i1   4.06   .  	.	.	.	.	.		.
i1   4.13   .  	.	.	.	.	.		.
i1   4.20   .  	.	.	.	.	.		.
i1   4.27   .  	.	.	.	.	.		.
i1   4.33   .  	.	.	.	.	.		.
i1   4.40   .22  600.	.	.	.		.

e
