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
; Coded:     jpg 8/93, modified ms 9/02, 8/08

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
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;	f1	audio wave
;_____________________________________________________________________________

; GEN functions **********************************************************

; waveform
f1 0 4097 9 1 1 0                ; sinus with amplitude 1, start phase 0


;score *******************************************************************
; instr 1  idur  iamp 	ifund    irise   idec   iscal	iran

											;*** first note ***
											;*** 9 harmonics **

i1  0      .17    61  	554  	.0006  .0112      1.0	   0.06     
i1  0      .17    49 	.    	.0006  .0088      2.0		.      
i1  0      .17    106 	.    	.00096 .0068      3.0		.
i1  0      .15    94 	.    	.00112 .0064      4.0		.
i1  0      .14    49 	.    	.00192 .0052      5.0		.
i1  0      .14    61 	.   	.00216 .0048      6.0		.
i1  0      .14    30 	.    	.00256 .0048      7.0		.
i1  0      .14    61 	.    	.0024  .0048      8.0		.
i1  0      .14    24 	.    	.0028  .0048      9.0		.


											;*** second note ***
											;*** 14 harmonics **
											
i1  1      .15     15  	293  	.0008  .0112	 1.0	   0.08
i1  1      .15     24  	.    	.0008  .0112     2.0		.
i1  1      .15     31  	.    	.00096 .0068     3.0		.
i1  1      .15     53 	.    	.00136 .0064     4.0		.
i1  1      .15     55 	.   	 .002   .0052    5.0		.
i1  1.01   .15     46 	.    	.0024  .0048     6.0		.
i1  1.01   .15     31 	.    	.0028  .0048     7.0		.
i1  1.01   .13     24 	.    	.0028  .0048     8.0		.
i1  1.01   .14     15 	.    	.0032  .008	 9.0		.
i1  1.01   .14     24 	.    	.0032  .008     10.0		.
i1  1.01   .14     43 	.    	.0036  .008     11.0		.
i1  1.01   .13     27 	.    	.0036  .008     12.0		.
i1  1.01   .13     14 	.    	.0032  .008     13.0		.
i1  1.01   .13     7.6 	.    	.0032  .0072    14.0		.

e

