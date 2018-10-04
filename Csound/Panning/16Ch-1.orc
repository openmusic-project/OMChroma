;;=============================================================================
;;			12CH-1.ORC : panning with 12 channels
;;=============================================================================
;;
;; Speakers position (azimuth):
;;  1: 0/-360
;;  2: 22.5/-337.5 
;;  3: 45/-315
;;  4: 67.5/-292.5
;;  5: 90/-270
;;  6: 112.5/-247.5
;;  7: 135/-225
;;  8: 157.5/-202.5
;;  9: 180/-180
;; 10: 202.5/-157.5
;; 11: 225/-135
;; 12: 247.5/-112.5
;; 13: 270/-90
;; 14: 292.5/-67.5
;; 15: 315/-45
;; 16: 337.5/-22.5

;-----------------------------------------------------------------------------
; PARAM DESCRIPTION :
; @PARAM 2 "e-dels" number 0.0 "action time [sec]"
; @PARAM 3 "durs" number 1.0 "duration [sec]"
; @PARAM 4 "afil" t nil "sound input"
; @PARAM 5 "bal" number 0.0 "panning, equal power (azimuth) [degrees]"
;-----------------------------------------------------------------------------


sr	= 96000
kr	= 96000
ksmps	= 1
nchnls	= 16
	
0dbfs = 8388607 ; 24 bits

instr 1 ; -------------------------------------------------------------
 isp_dur		= p3
 isp_file		= p4
 isp_bal		= p5 % 360 ;reduce to 0-360
 isp_bal		= (isp_bal < 0.0 ? (360+isp_bal) : isp_bal) ; turn negative vals into positive ones
 isp_mode		= 0 ; equal power (harmonic) panning
 isp_pch		= 1 ; no transposition
 isp_skptim		= 0 ; no skip time
 isp_wrap		= 0 ; no wrapping around file
 isp_format		= 0 ; 16-bit short integers (for headerless files)
 isp_wsize		= 128 ; point sinc interpolation with anti-aliasing (slow)
 isp_bufsize	= 262144 ; buffer size (larger values improve the efficiency of disk reads)
 isp_skpinit	= 0 ; do not skip initialization

aut1	init 0.0
aut2	init 0.0
aut3	init 0.0
aut4	init 0.0
aut5	init 0.0
aut6	init 0.0
aut7	init 0.0
aut8	init 0.0
aut9	init 0.0
aut10	init 0.0
aut11	init 0.0
aut12	init 0.0
aut13	init 0.0
aut14	init 0.0
aut15	init 0.0
aut16	init 0.0

 asound		diskin2	isp_file, isp_pch, isp_skptim, isp_wrap, isp_format, isp_wsize, isp_bufsize, isp_skpinit

; PANNING BEG
if ((isp_bal >= 0.0) && (isp_bal < 22.5)) goto quad1

if ((isp_bal >= 22.5) && (isp_bal < 45.0)) goto quad2

if ((isp_bal >= 45.0) && (isp_bal < 67.5)) goto quad3

if ((isp_bal >= 67.5) && (isp_bal < 90.0)) goto quad4

if ((isp_bal >= 90.0) && (isp_bal < 112.5)) goto quad5

if ((isp_bal >= 112.5) && (isp_bal < 135.0)) goto quad6

if ((isp_bal >= 135.0) && (isp_bal < 157.5)) goto quad7

if ((isp_bal >= 157.5) && (isp_bal < 180.0)) goto quad8

if ((isp_bal >= 180.0) && (isp_bal < 202.5)) goto quad9

if ((isp_bal >= 202.5) && (isp_bal < 225.0)) goto quad10

if ((isp_bal >= 225.0) && (isp_bal < 247.5)) goto quad11

if ((isp_bal >= 247.5) && (isp_bal < 270.0)) goto quad12

if ((isp_bal >= 270.0) && (isp_bal < 292.5)) goto quad13

if ((isp_bal >= 292.5) && (isp_bal < 315.0)) goto quad14

if ((isp_bal >= 315.0) && (isp_bal < 337.5)) goto quad15

if ((isp_bal >= 337.5) && (isp_bal < 360.0)) goto quad16

quad1:
aut1, aut2		pan2	asound, ((isp_bal % 22.5) / 22.5)
goto end

quad2:
aut2, aut3		pan2	asound, ((isp_bal % 22.5) / 22.5)
goto end

quad3:
aut3, aut4		pan2	asound, ((isp_bal % 22.5) / 22.5)
goto end

quad4:
aut4, aut5		pan2	asound, ((isp_bal % 22.5) / 22.5)
goto end

quad5:
aut5, aut6		pan2	asound, ((isp_bal % 22.5) / 22.5)
goto end

quad6:
aut6, aut7		pan2	asound, ((isp_bal % 22.5) / 22.5)
goto end

quad7:
aut7, aut8		pan2	asound, ((isp_bal % 22.5) / 22.5)
goto end

quad8:
aut8, aut9		pan2	asound, ((isp_bal % 22.5) / 22.5)
goto end

quad9:
aut9, aut10		pan2	asound, ((isp_bal % 22.5) / 22.5)
goto end

quad10:
aut10, aut11	pan2	asound, ((isp_bal % 22.5) / 22.5)
goto end

quad11:
aut11, aut12	pan2	asound, ((isp_bal % 22.5) / 22.5)
goto end

quad12:
aut12, aut13	pan2	asound, ((isp_bal % 22.5) / 22.5)

quad13:
aut13, aut14	pan2	asound, ((isp_bal % 22.5) / 22.5)

quad14:
aut14, aut15	pan2	asound, ((isp_bal % 22.5) / 22.5)

quad15:
aut15, aut16	pan2	asound, ((isp_bal % 22.5) / 22.5)

quad16:
aut16, aut1		pan2	asound, ((isp_bal % 22.5) / 22.5)

end:
    outc     aut1, aut2, aut3, aut4, aut5, aut6, aut7, aut8, aut9, aut10, aut11, aut12, aut13, aut14, aut15, aut16

; PANNING END


endin
