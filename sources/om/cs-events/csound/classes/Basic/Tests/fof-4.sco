;=============================================================================
;			FOF-4.ORC
; FOF-BASED GRANULAR SYNTHESIS / MONO
; SAMPLED GRAIN RATHER THAN A SINE TONE IN THE FOF
; AMPLITUDE ENVELOPE WITH POSCIL
;=============================================================================

; Timbre:    Granular synthesis with a FOF (based on Byrne Villez's work)
; Synthesis: FOF (Forme d'Onde Formatique)
;            POSCIL envelopes
; Coded:     ms 9/04, 2/09

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
;	p6	= formant freq [Hz]
;	p7	= bandwidth [Hz]
;	p8	= amp envelope [GEN number]
;	p9	= tex or krise [sec]
;	p10	= total duration of the burst, see debatt [sec]
;	p11	= decay of the FOF [sec]
;	p12	= octaviation [>=0.0]
;	p13	= sound file [name, sound, string, pathname or GEN01] (2.0)
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f19	sigmoid rise/decay shape
;	f32	short sample (grain)
;_____________________________________________________________________________

; short grain
f32  0 32768  1  "tambura.aiff"	0	0	0

; sigmoid rise/decay
f19 0  65536  19 .5 .5 270 .5

; amplitude envelope
f8 0 513 7 1.0 512 1.0

; score ******************************************************************

;  iatk idur  iamp	if0	ifq 	ibw iaenv 	iwin iwdur iwout 	ioct iafil
i1  0    2    -6.0	220	1.0	 0	  8	  0.003	 0.02	0.005	0.0	  32
s
i1  0    2    -6.0	220	10.0	 0	  8	  0.003	 0.02	0.005	0.0	  32
s
i1  0    2    -6.0	220	10.0	 0	  8	  0.003	 0.02	0.005	1.0	  32
s
i1  0    2    -6.0	220	10.0	 0	  8	  0.003	 0.02	0.005	2.0	  32
s
i1  0    2    -6.0	220	10.0	 0	  8	  0.003	 0.02	0.005	4.0	  32
s
i1  0    2    -6.0	220	10.0	 0	  8	  0.003	 0.02	0.005	8.0	  32
e
