;=============================================================================
;		ADD3.ORC
; SIMPLE ADDITIVE SYNTHESIS WITH FREQUENCY MODULATION (GLISS OR VIBR) / MONO
; AMPLITUDE ENVELOPE WITH OSCILI
; NO ENVELOPE FOR THE AMPLITUDE OF THE VIBRATO
;=============================================================================

; Timbre:    simple additive glissando
; Synthesis: additive same units
; Coded:     ms 12/07, 8/08

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
;	p5	= frequency [Hz]
;	p6	= amplitude envelope [GEN number]
;	p7	= amplitude of the vibrato [0-1]
;	p8	= frequency of the vibrato [Hz]
;	p9	= vibrato envelope [GEN number]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;	f1	audio wave
;	f2	vibrato function
;	f5	raising segment for glissandi
;_____________________________________________________________________________

; GEN functions **********************************************************
; audio and vibrato
f1  0   4097   9  1  1  0
f2  0   4097   9  1  1  0

; glissando
f6  0   513   7  0 512 1

; envelopes
f51  0   513   7  0 1 1 510 1 1 0
f52  0   513   7  0 150 1 150 1 212 0


; score ******************************************************************
;  start  idur   iamp	 ifq	iaenv	ivibamp	ivibfq	ifenv
i1   0	   10	-6.0	440.0	51	0.06	0.2	6
i1   5	   5	-6.0	440.0	51	0.0	5.0	6
i1   10	   2	-6.0	440.0	51	0.03	5.0	52
e
