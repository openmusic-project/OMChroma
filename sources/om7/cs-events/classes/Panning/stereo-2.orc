;;=============================================================================
;;			STEREO-2.ORC 
;; INDEPENDENT PANNING, 1 SLOT/CHANNEL 
;;=============================================================================

;-----------------------------------------------------------------------------
; PARAM DESCRIPTION :
; @PARAM 2 "e-dels" number 0.0 "action time [sec]"
; @PARAM 3 "durs" number 1.0 "duration [sec]"
; @PARAM 4 "afil" t nil "sound input"
; @PARAM 5 "channel1" number 1.0 "amplitude [0.0-1.0]"
; @PARAM 6 "channel2" number 0.0 "amplitude [0.0-1.0]"
;-----------------------------------------------------------------------------

;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4 	= input (file)
;	p5	= Ch 1 level [0-1]
;	p6	= Ch 2 level [0-1]

sr	= 96000
kr	= 96000
ksmps	= 1
nchnls	= 2

;0dbfs = 32767  ; 16 bits
0dbfs = 8388697 ; 24 bits

instr 1 ; -------------------------------------------------------------
 isp_dur		= p3
 isp_file		= p4
 isp_ch1		= p5
 isp_ch2		= p6
 isp_mode		= 0 ; equal power (harmonic) panning
 isp_pch		= 1 ; no transposition
 isp_skptim		= 0 ; no skip time
 isp_wrap		= 0 ; no wrapping around file
 isp_format		= 0 ; 16-bit short integers (for headerless files)
 isp_wsize		= 128 ; point sinc interpolation with anti-aliasing (slow)
 isp_bufsize	= 262144 ; buffer size (larger values improve the efficiency of disk reads)
 isp_skpinit	= 0 ; do not skip initialization

 a1	diskin2	p4, isp_pch, isp_skptim, isp_wrap, isp_format, isp_wsize, isp_bufsize, isp_skpinit

 al = a1*isp_ch1
 ar = a1*isp_ch2

 outs     al, ar
endin
