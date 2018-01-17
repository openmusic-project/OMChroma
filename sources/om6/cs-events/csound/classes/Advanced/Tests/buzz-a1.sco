;=============================================================================
;			BUZZ-A2.ORC
; DYNAMIC SPECTRUM OSCILLATOR / MONO
; AMPLITUDE ENVELOPE WITH POSCIL, VALUES IN HZ, CONTROLLABLE
;=============================================================================

; Timbre:    Harmonically related cosine partials with many control
; Synthesis: (g)buzz
;            POSCIL envelopes
; Coded:     ms 9/02, 8/08

; NB: NEW STRUCTURE FOR THE AMPLITUDES FROM AUGUST 2008!
;    Positive value > 0.0  : linear amplitude (>0.0-1000.0)
;    0.0 or negative value : amplitude in dB (0 = maximum value)

; The apparently arbitrary amplitude range (0-1000, rather than 0-1)
;         avoids Lisp printing small values with exponential notation

; Replaced oscili with poscil (precise oscillator), ms 8/08
; Default SR = 96000, recommended precision: 24 bits
;-----------------------------------------------------------------------------
;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4	= maximum amplitude [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= min fundamental frequency [Hz]
;	p6	= max fundamental frequency [Hz]
;	p7	= envelope for the fundamental frequency [GEN]
;	p8	= amplitude envelope [GEN number]
;	p9	= lowest harmonic present in the buzz [Hz]
;	p10	= highest possible harmonic present [Hz]
;	p11	= min multiplier in the series of amplitude coefficients
;	p12	= max multiplier in the series of amplitude coefficients
;	p13	= envelope for the multiplier [GEN]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f5	large cosine
;_____________________________________________________________________________

; GEN functions **********************************************************; single cosine(!) wave for the buzzf5 0 16777216 11 1 1

; trianglef11 0 513 7 0 256 1 256 0

; ascending line
f12 0 513 7 0 512 1; score ******************************************************************i1    0  10   -6    220 440 12   11   440  8000   0.1 1.0 12e