;=============================================================================
;			FOF-a4.ORC
; FOF-BASED GRANULAR SYNTHESIS / MONO
; SAMPLED GRAIN RATHER THAN A SINE TONE IN THE FOF
; SEVERAL DYNAMIC CONTROLS OVER THE P-FIELDS + JITTER
; AMPLITUDE ENVELOPE WITH POSCIL
;=============================================================================

; Timbre:    Granular synthesis with a FOF (based on Byrne Villez's work)
; Synthesis: FOF (Forme d'Onde Formatique)
;            POSCIL envelopes
; Coded:     ms 2/09

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
;	p5	= min fundamental freq [Hz]
;	p6	= max fundamental freq [Hz]
;	p7	= fundamental freq envelope [GEN]
;	p8	= jitter amp of fundamental freq [0-1]
;	p9	= min formant freq [Hz]
;	p10	= max formant freq [Hz]
;	p11	= formant freq envelope [GEN]
;	p12	= jitter amp of formant freq [0-1]
;	p13	= min bandwidth [Hz]
;	p14	= max bandwidth [Hz]
;	p15	= bandwidth envelope [GEN]
;	p16	= amp envelope [GEN]
;	p17	= min tex or krise [sec]
;	p18	= max tex or krise [sec]
;	p19	= tex or krise envelope [GEN]
;	p20	= min total dur of the burst [sec]
;	p21	= max total duration of the burst [sec]
;	p22	= envelope of the total duration of the burst [GEN]
;	p23	= min decay of the FOF [sec]
;	p24	= max decay of the FOF [sec]
;	p25	= envelope of decay of the FOF [GEN]
;	p26	= min octaviation [>=0.0]
;	p27	= max octaviation [>=0.0]
;	p28	= octaviation envelope [GEN]
;	p29	= sound file [name, sound, string, pathname or GEN01] (32)
;	p30	= formant frequency mode [0=no gliss]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f19	sigmoid rise/decay shape
;	f32	short sample (grain)
;_____________________________________________________________________________

; short grain
f32  0 65536  1  "short_96.aiff"	0	0	0

; sigmoid rise/decay
f19 0  65536  19 .5 .5 270 .5

; amplitude envelope
f8 0 513 7 1.0 512 1.0

; ascending line
f9 0 513 7 0.0 512 1.0
; triangle
f10 0 513 7 0.0 256 1.0 250 0.0

; score ******************************************************************

;  iatk idur  iamp	if0mn/mx/ev/jt	ifqmn/mx/ev/jt ibwmn/mx/ev iaev
i1  0    10    -20.0 149 149 9 0.0	 2.15 2.0 9 0.0  0 0 9 		8 \
;  iwinmn/mx/ev 	iwdurmn/mx/ev iwoutmn/mx/ev ioctmn/mx/ev iafil mode
	0.003 0.003 9 		1.0 1.0 9	  1.0 1.0 9		5.0	0.0 9 32 	1
s
;  iatk idur  iamp	if0mn/mx/ev/jt	ifqmn/mx/ev/jt ibwmn/mx/ev iaev
i1  0    10    -25.0 149 298 9 0.03	 2.15 1.0 9 0.003  0 0 9 		8 \
;  iwinmn/mx/ev 	iwdurmn/mx/ev iwoutmn/mx/ev ioctmn/mx/ev iafil mode
	0.003 0.003 9 		1.0 1.0 9	  1.0 1.0 9		0.0	1.0 9 32 	1
s
;  iatk idur  iamp	if0mn/mx/ev/jt	ifqmn/mx/ev/jt ibwmn/mx/ev iaev
i1  0    10    -20.0 149 149 9 0.1	 10.1 2.0 9 0.1  0 0 9 		8 \
;  iwinmn/mx/ev 	iwdurmn/mx/ev iwoutmn/mx/ev ioctmn/mx/ev iafil mode
	0.003 0.003 9 		0.1 1.0 9	  0.1 1.0 9		0.0	2.0 9 32 	1
s
;  iatk idur  iamp	if0mn/mx/ev/jt	ifqmn/mx/ev/jt ibwmn/mx/ev iaev
i1  0    10    -20.0 149 149 9 0.0	 5.15 1.0 9 0.3  0 0 8 	8 \
;  iwinmn/mx/ev 	iwdurmn/mx/ev iwoutmn/mx/ev ioctmn/mx/ev iafil mode
	0.003 0.3 9 		0.1 1.0 9	  0.0 1.0 9		0.0	0.0 9 32 	1
s
e
