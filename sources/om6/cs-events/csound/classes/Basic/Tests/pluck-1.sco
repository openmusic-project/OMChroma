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
;	p6	= intended freq value [Hz]
;	p7	= % of the total duration used for the last decay [0-1]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
; none
;_____________________________________________________________________________


; GEN functions **********************************************************


; score ******************************************************************

;            iamp    ifq   idec
i1   0   1   -6.0    440   0.8       ; pluck-made random numbers
i1   +   1   -6.0    550   0.8       ; pluck-made random numbers
i1   +   1   -6.0    660   0.8       ; pluck-made random numbers
i1   +   1   -6.0    880   0.8       ; pluck-made random numbers
i1   +   1   -6.0    .     0.8       ; pluck-made random numbers
i1   +   1   -6.0    .     0.8       ; pluck-made random numbers
i1   +   1   -6.0    .     0.8       ; pluck-made random numbers
i1   +   1   -6.0    .     0.8       ; pluck-made random numbers
s
i1   0   1   -6.0    440   0.99       ; pluck-made random numbers
i1   +   1   -6.0    550   .        ; pluck-made random numbers
i1   +   1   -6.0    660   .        ; pluck-made random numbers
i1   +   1   -6.0    880   .        ; pluck-made random numbers
i1   +   1   -6.0    .     .        ; pluck-made random numbers
i1   +   1   -6.0    .     .        ; pluck-made random numbers
i1   +   1   -6.0    .     .        ; pluck-made random numbers
i1   +   1   -6.0    .     .        ; pluck-made random numbers
s
i1   0   1   -6.0    440   0.1       ; pluck-made random numbers
i1   +   1   -6.0    550   .        ; pluck-made random numbers
i1   +   1   -6.0    660   .        ; pluck-made random numbers
i1   +   1   -6.0    880   .        ; pluck-made random numbers
i1   +   1   -6.0    .     .        ; pluck-made random numbers
i1   +   1   -6.0    .     .        ; pluck-made random numbers
i1   +   1   -6.0    .     .        ; pluck-made random numbers
i1   +   1   -6.0    .     .        ; pluck-made random numbers
s
i1   0   1   -6.0    440   0.3       ; pluck-made random numbers
i1   +   1   -6.0    550    .        ; pluck-made random numbers
i1   +   1   -6.0    660    .        ; pluck-made random numbers
i1   +   1   -6.0    880    .        ; pluck-made random numbers
i1   +   1   -6.0    .      .        ; pluck-made random numbers
i1   +   1   -6.0    .      .        ; pluck-made random numbers
i1   +   1   -6.0    .      .        ; pluck-made random numbers
i1   +   1   -6.0    .      .        ; pluck-made random numbers
s
i1   0   1   -6.0    440   0.5       ; pluck-made random numbers
i1   +   1   -6.0    550   .        ; pluck-made random numbers
i1   +   1   -6.0    660   .        ; pluck-made random numbers
i1   +   1   -6.0    880   .        ; pluck-made random numbers
i1   +   1   -6.0    .     .        ; pluck-made random numbers
i1   +   1   -6.0    .     .        ; pluck-made random numbers
i1   +   1   -6.0    .     .        ; pluck-made random numbers
i1   +   1   -6.0    .     .        ; pluck-made random numbers
s
i1   0   1   -6.0    440  0.99       ; pluck-made random numbers
i1   +   1   -6.0    550   .        ; pluck-made random numbers
i1   +   1   -6.0    660   .        ; pluck-made random numbers
i1   +   1   -6.0    880   .        ; pluck-made random numbers
i1   +   1   -6.0    .     .        ; pluck-made random numbers
i1   +   1   -6.0    .     .        ; pluck-made random numbers
i1   +   1   -6.0    .     .        ; pluck-made random numbers
i1   +   1   -6.0    .     .        ; pluck-made random numbers
e
