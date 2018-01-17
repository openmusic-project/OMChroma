;=============================================================================
;			FOF-A4.ORC
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
;	p29	= formant frequency mode [0=no gliss]
;	p30	= sound file [name, sound, string, pathname or GEN01] (32)
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS
;	f19	sigmoid rise/decay shape
;	f32	short sample (grain)
;_____________________________________________________________________________

; CLASS: FOF-a4

;  GLOBAL KEYWORDS (default values within parentheses):
;	NUMROWS		: amount of rows (components) in the event (1)
;	ACTION-TIME	: start time of the whole event [sec] (0.0)
;	USER-FUN	: user-defined parsing function (nil)

;  LOCAL KEYWORDS:
;	E-DELS	: entry delays [sec] (0.0)
;	DURS	: duration [sec] (1.0)
;	AMP 	: amp [lin, >0.0-1000.0 or dB <- 0.0] (-6.0)
;	F0  	: min fundamental freq [Hz] (131.0)
;	F0MAX  	: max fundamental freq [Hz] (165.0)
;	F0ENV  	: envelope for the fund freq [GEN] (asc line)
;	F0JTA  	: jitter's amp of the fund freq [0-1] (0.06)
;	FREQ	: min formant freq [Hz] (2.0)
;	FQMAX 	: max formant freq [Hz] (1.0)
;	FQENV	: envelope for the formant freq [Hz] (asc line)
;	FQJTA 	: jitter's amp for the formant freq [0-1] (0.1)
;	BW  	: min bandwidth [Hz] (0)
;	BWMAX  	: max bandwidth [Hz] (0)
;	BWENV  	: bandwidth's envelope [GEN]  (asc line)
;	AENV	: fun number for the amp envlp [GEN] (all 1)
;	WIN 	: min rise time of the FOF [sec] (0.003)
;	WINMAX 	: max rise time of the FOF [sec] (0.1)
;	WINENV 	: envelope for the rise time of the FOF [GEN] (asc line)
;	WDUR	: min duration of the FOF [sec] (0.1)
;	WDURMAX	: max duration of the FOF [sec] (1.0)
;	WDURENV	: envelope for the duration of the FOF [GEN] (asc line)
;	WOUT	: min decay of the FOF [sec] (0.02)
;	WOUTMAX	: max decay of the FOF [sec] (0.2)
;	WOUTENV	: envelope for the decay of the FOF [GEN] (asc line)
;	OCT 	: min octaviation factor [=>0] (0.0)
;	OCTMAX 	: max octaviation factor [=>0] (1.0)
;	OCTENV 	: octaviation factor envelope [GEN] (asc line)
;	MODE	: formant frequency mode [0=no gliss within each grain] (1)
;	AFIL	: file name [int, sound, string, pathname or GEN] (or_96.aif)
;*****************************************************************************

sr	= 96000
kr	= 96000
ksmps	= 1
nchnls	= 1

;0dbfs = 32767  ; 16 bits
0dbfs = 8388607 ; 24 bits
;OUTBEG
;OUTEND

instr 1 ; -------------------------------------------------------------
idur   	= p3
idurosc	= 1/p3
iamp 	= (p4 > 0.0 ? (p4*0.001*0dbfs) : (ampdbfs(p4)))

if0min	= p5
if0max	= p6
if0d	= if0max-if0min
if0env	= p7
if0jt	= p8/3.0	; three modules

ifqmin	= p9
ifqmax	= p10
ifqd 	= ifqmax-ifqmin
ifqenv	= p11
ifqjt	= p12/3.0

ibwmin	= p13
ibwmax	= p14
ibwd	= ibwmax-ibwmin
ibwenv	= p15

iaenv	= p16

iwinmin	= p17
iwinmax	= p18
iwind	= iwinmax-iwinmin
iwinenv	= p19

iwdurmin= p20
iwdurmax= p21
iwdurd	= iwdurmax-iwdurmin
iwdurenv= p22

iwoutmin= p23
iwoutmax= p24
iwoutd	= iwoutmax-iwoutmin
iwoutenv= p25

ioctmin	= p26
ioctmax	= p27
ioctd	= ioctmax-ioctmin
ioctenv	= p28

ifile	= p29
imode 	= p30

iolaps	= 100000	; how many simultaneous FOFs can be played
					; (takes little memory if not used)
iphs	= 0
iskip	= 0
isize	= 1		; 31-bit random numbers for randi

isigfun	= 19
;OUTBEG
;OUTEND

;f0
; jitter for f0
; seed>1.0=> seed from the system time
kf0j1	randi	if0jt, 1/0.05, 1.8135, isize
kf0j2	randi	if0jt, 1/0.111, 1.3111, isize
kf0j3	randi	if0jt, 1/1.219, 1.6711, isize
kf0j	=	(kf0j1+kf0j2+kf0j3)

;f0 envelope
kf0env		poscil	if0d, idurosc, if0env
kf0		= kf0env+if0min
kf0end	= kf0+(kf0*kf0j)

;freq
; jitter for freq
kfqj1	randi	ifqjt, 1/0.053, 1.5318, isize
kfqj2	randi	ifqjt, 1/0.112, 1.1113, isize
kfqj3	randi	ifqjt, 1/1.215, 1.7166, isize
kfqj 	=	(kfqj1+kfqj2+kfqj3)

;freq envelope
kfqenv		poscil	ifqd, idurosc, ifqenv
kfq		= kfqenv+ifqmin
kfqend	= kfq+(kfq*kfqj)

;bw
kbw		poscil	ibwd, idurosc, ibwenv
kbwend	=	ibwmin+kbw

;krise (win)
kwin		poscil	iwind, idurosc, iwinenv
kwinend	=	iwinmin+kwin

;kdur (wdur)
kwdur		poscil	iwdurd, idurosc, iwdurenv
kwdurend	=	iwdurmin+kwdur

;kdec (wout)
kwout		poscil	iwoutd, idurosc, iwoutenv
kwoutend	=	iwoutmin+kwout

;koct
koct		poscil	ioctd, idurosc, ioctenv
koctend	=	ioctmin+koct

; amplitude envelope
kenv		poscil	iamp, idurosc, iaenv

;				xamp  xfund xform 		oct  	kband 	kris   	kdur    	kdec
;	iolaps   ifna   ifnb
asig	fof 	1.0, kf0end, kfqend, koctend,  kbwend, kwinend, kwdurend, kwoutend, \
	iolaps, ifile, isigfun, idur, iphs, imode, iskip
asound	= asig*kenv

;OUTBEG
      outc     asound
;OUTEND

endin