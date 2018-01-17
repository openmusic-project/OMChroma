;=============================================================================
;			BUZZ-2.ORC
; DYNAMIC SPECTRUM OSCILLATOR / MONO
; AMPLITUDE ENVELOPE WITH POSCIL, VALUES IN HZ
;=============================================================================

; Timbre:    Various controlled noise spectra
; Synthesis: (g)buzz
;            POSCIL envelopes
; Coded:     jpg 8/92, modified ms 9/02, 8/08

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
;	p6	= amplitude envelope [GEN]
;	p7	= lowest frequency in the buzz [Hz]
;	p8	= highest frequency [Hz]
;	p9	= multiplier in the series of amp coeff for the buzz [0-1]
;	p10	= envelope for the multiplier [GEN]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f5	large cosine
;_____________________________________________________________________________

; GEN functions **********************************************************
; single cosine(!) wave for the buzz
f5 0 16777216 11 1 1

f11 0 513 7 0 256 1 256 0
f12 0 513 -7 0 256 -1 256 0

; score ******************************************************************

;        idur   iamp   ifqc	iaenv	ilh	in	ibzmul	ibzenv
i1    0  2      -6    220	 11	220.0	8800	0.9	11
i1    +  2      -6    220	 11	440.0	17600	0.5	11
i1    +  2      -6    220	 11	1000.0	17600	0.2	11
i1    +  2      -6    220	 11	880.0	5000	0.99	11

e
