;;=============================================================================
;;		ADD3.ORC
;; SIMPLE ADDITIVE SYNTHESIS WITH FREQUENCY MODULATION (GLISS OR VIBR) / MONO
;; AMPLITUDE ENVELOPE WITH OSCILI
;; NO ENVELOPE FOR THE AMPLITUDE OF THE VIBRATO
;;=============================================================================
;;
;; Timbre:    simple additive glissando
;; Synthesis: additive same units
;; Coded:     ms 12/07, 8/08

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

;-----------------------------------------------------------------------------
; PARAM DESCRIPTION :
; @PARAM 2 "e-dels" number 0.0 "action time [sec]"
; @PARAM 3 "durs" number 1.0 "duration [sec]"
; @PARAM 4 "amp" number -6.0 "maximum amplitude [linear, >0.0-1000.0 or dB, <= 0.0]"
; @PARAM 5 "freq" number 440.0 "fundamental frequency [Hz]"
; @PARAM 6 "aenv" cr::cs-table (:cs-table cr::gen07 :points ((0 0) (2048 100) (4096 0))) "amplitude envelope [GEN table]"
; @PARAM 7 "fdev" number 0.06 "decay time of the amp envlp [sec]"
; @PARAM 8 "vfq" number 1.0 "spectral scaler for the fund freq"
; @PARAM 9 "fenv" cr::cs-table (:cs-table cr::gen07 :points ((0 0) (4096 1))) "vibrato envelope [GEN table]"
;
; COMPULSORY GEN FUNCTIONS :
; @GEN ; audio wave
; @GEN f1  0   65537   10 1
;-----------------------------------------------------------------------------

sr	= 96000
kr	= 96000
ksmps	= 1
nchnls	= 1

;0dbfs = 32767  ; 16 bits
0dbfs = 8388607 ; 24 bits

instr 1 ; -------------------------------------------------------------
 
idur		= p3
idurosc		= 1/idur
iamp 		= (p4 > 0.0 ? (p4*0.001*0dbfs) : (ampdbfs(p4)))
ifq		= p5
iaenv		= p6
ivibamp 	= p7
ivibfq 		= p8
ifenv	 	= p9
;OUTBEG
;OUTEND

iaudiofun	= 1
 
   a2    poscil  iamp, idurosc, iaenv

   k1		poscil  ivibamp, ivibfq, ifenv
   k1		= (k1 * ifq) + ifq
   asound	poscil  a2, k1, iaudiofun

;OUTBEG
   out     asound
;OUTEND

endin
