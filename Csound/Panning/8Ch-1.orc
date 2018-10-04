;;=============================================================================
;;			8CH-1.ORC : panning with 8 channels
;;=============================================================================
;;
;; Speakers position (azimuth):
;;  1: 0, 
;;  2: 45/-315 
;;  3: 90/-270
;;  4: 135/-225
;;  5: 180/-180
;;  6: 225/-135
;;  7: 270/-90
;;  8: 315/-45

;-----------------------------------------------------------------------------
; PARAM DESCRIPTION :
; @PARAM 2 "e-dels" number 0.0 "action time [sec]"
; @PARAM 3 "durs" number 1.0 "duration [sec]"
; @PARAM 4 "afil" t nil "sound input"
; @PARAM 5 "bal" number 0.0 "panning, equal power [Azimuth, degrees]"
;-----------------------------------------------------------------------------

;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4 	= input (file)
;	p5	= panning, equal power [Azimuth, degrees]
;		  [0=1, 45/-315=2, 90/-270=3, 135/-225=4,
;			180/-180=5, 225/-135=6, 270/-90=7 315/-45=8]

sr	= 96000
kr	= 96000
ksmps	= 1
nchnls	= 8

;0dbfs = 32767  ; 16 bits
0dbfs = 8388697 ; 24 bits


instr 1 ; -------------------------------------------------------------
 isp_dur		= p3
 isp_file		= p4
 isp_bal		= p5 % 360 ;reduce to 0-360º
 isp_bal		= (isp_bal < 0.0 ? (360+isp_bal) : isp_bal) ; turn negative vals into positive ones
 isp_mode		= 0 ; equal power (harmonic) panning
 isp_pch		= 1 ; no transposition
 isp_skptim		= 0 ; no skip time
 isp_wrap		= 0 ; no wrapping around file
 isp_format		= 0 ; 16-bit short integers (for headerless files)
 isp_wsize		= 128 ; point sinc interpolation with anti-aliasing (slow)
 isp_bufsize	= 262144 ; buffer size (larger values improve the efficiency of disk reads)
 isp_skpinit	= 0 ; do not skip initialization

aut1 init 0.0
aut2 init 0.0
aut3 init 0.0
aut4 init 0.0
aut5 init 0.0
aut6 init 0.0
aut7 init 0.0
aut8 init 0.0

 asound		diskin2	isp_file, isp_pch, isp_skptim, isp_wrap, isp_format, isp_wsize, isp_bufsize, isp_skpinit

; PANNING BEG
if ((isp_bal >= 0.0) && (isp_bal < 45.0)) goto quad1

if ((isp_bal >= 45.0) && (isp_bal < 90.0)) goto quad2

if ((isp_bal >= 90.0) && (isp_bal < 135.0)) goto quad3

if ((isp_bal >= 135.0) && (isp_bal < 180.0)) goto quad4

if ((isp_bal >= 180.0) && (isp_bal < 225.0)) goto quad5

if ((isp_bal >= 225.0) && (isp_bal < 270.0)) goto quad6

if ((isp_bal >= 270.0) && (isp_bal < 315.0)) goto quad7

if ((isp_bal >= 315.0) && (isp_bal < 360.0)) goto quad8

quad1:
aut1, aut2	pan2	asound, ((isp_bal % 45.0) / 45.0)
goto end

quad2:
aut2, aut3	pan2	asound, ((isp_bal % 45.0) / 45.0)
goto end

quad3:
aut3, aut4	pan2	asound, ((isp_bal % 45.0) / 45.0)
goto end

quad4:
aut4, aut5	pan2	asound, ((isp_bal % 45.0) / 45.0)
goto end

quad5:
aut5, aut6	pan2	asound, ((isp_bal % 45.0) / 45.0)
goto end

quad6:
aut6, aut7	pan2	asound, ((isp_bal % 45.0) / 45.0)
goto end

quad7:
aut7, aut8	pan2	asound, ((isp_bal % 45.0) / 45.0)
goto end

quad8:
aut8, aut1	pan2	asound, ((isp_bal % 45.0) / 45.0)

end:
         outc     aut1, aut2, aut3, aut4, aut5, aut6, aut7, aut8

; PANNING END


endin
