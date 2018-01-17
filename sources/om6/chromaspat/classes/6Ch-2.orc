;=============================================================================
;			6CH-2.ORC
; INDEPENDENT PANNING, 1 SLOT/CHANNEL 
;-----------------------------------------------------------------------------

;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4 	= input (file)
;	p5	= Ch 1 level [0-1]
;	p6	= Ch 2 level [0-1]
;	p7	= Ch 3 level [0-1]
;	p8	= Ch 4 level [0-1]
;	p9	= Ch 5 level [0-1]
;	p10	= Ch 6 level [0-1]

sr	= 96000
kr	= 96000
ksmps	= 1
nchnls	= 6

;0dbfs = 32767  ; 16 bits
0dbfs = 8388697 ; 24 bits

instr 1 ; -------------------------------------------------------------
 isp_dur		= p3
 isp_file		= p4
 isp_ch1		= p5
 isp_ch2		= p6
 isp_ch3		= p7
 isp_ch4		= p8
 isp_ch5		= p9
 isp_ch6		= p10
 
 isp_mode		= 0 ; equal power (harmonic) panning
 isp_pch		= 1 ; no transposition
 isp_skptim		= 0 ; no skip time
 isp_wrap		= 0 ; no wrapping around file
 isp_format		= 0 ; 16-bit short integers (for headerless files)
 isp_wsize		= 128 ; point sinc interpolation with anti-aliasing (slow)
 isp_bufsize	= 262144 ; buffer size (larger values improve the efficiency of disk reads)
 isp_skpinit	= 0 ; do not skip initialization

 adsk		diskin2	isp_file, isp_pch, isp_skptim, isp_wrap, isp_format, isp_wsize, isp_bufsize, isp_skpinit

 a1 = adsk*isp_ch1
 a2 = adsk*isp_ch2
 a3 = adsk*isp_ch3
 a4 = adsk*isp_ch4
 a5 = adsk*isp_ch5
 a6 = adsk*isp_ch6

 outc     a1, a2, a3, a4, a5, a6


endin
