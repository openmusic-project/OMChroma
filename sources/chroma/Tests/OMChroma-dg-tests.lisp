;***********************************
; COMPLETE TESTS OF OMCHROMA ALONE |
;***********************************

;******************************************
;TESTS OF THE DG (DATA GENERATION) FOLDER |
;*****************************************

;____________________________________________________________________________
; OMCHROMA LOADED FILES (load.lisp)
; NO DEPENDENCIES FROM OM!!!
;____________________________________________________________________________
(in-package :cl-user)
  
;;; clean-sources is defined in OM load utils
; (clean-sources (make-pathname :directory (pathname-directory *load-pathname*)))

#|
;;; requires compile&load defined in OM
 '(
        ;SYS
   "init-chroma"
   "globals"
   "utils"
  
        ;PLS
   "pls/utils-pls"
   "pls/all-purp"
   "pls/type"
   "pls/tbl"
   "pls/fun"
   "pls/ctl"
   "pls/raw"
   "pls/ve"
   "pls/wt"
   "pls/wt-frz"
                      
        ;DG - TESTED HERE
   "dg/utils-dg"
   "dg/df"
   "dg/dv"
   "dg/spectrum"
                        
        ; VPS
   "vps/globals-vps"
   "vps/symbolic-pitch"
   "vps/pitch-conversions"
   "vps/utils-vps"
   "vps/vps"
   "vps/chord"
   "vps/spectrum"
   "vps/user-vps"
   "vps/modif-vps"
   "vps/modif-empty-vps"
   "vps/match"
     
        ; MODELS
   "models/analysis-data"
   "models/f0-data"
   "models/cseq-data"
   "models/additive-data"
   "models/formant-data"
   "models/specenv-data"
   "models/model"
   "models/regions"
   "models/f0-model"
   "models/specenv-model"
   "models/cseq-model"                     
   "models/cseq-regions"
   "models/partials-model"
   "models/partials-regions"
   "models/modif-models"
   "models/modif-regions"
   "models/user-model"
   "models/time-processing"
   "models/utils-model"
  
        ; CTL2
   "ctl2/globals-ctl2"
   "ctl2/ctl2001"
   "ctl2/utils-ctl2001"

   )
 )
|#
    
;(cl-user::decode-local-path "OMChroma-tests.lisp")

;____________________________________________________________________________
; chroma/dg/utils-dg.lisp
;____________________________________________________________________________
;=====================================================
; CHROMA 
;=====================================================
;******************************************************************
;-------| CTL1 SYSTEM
;-------| This file is: $LLdg/utils.ll
;-------| Version V1.0: Jan 22, 1990
;-------| By Marco Stroppa
;-------| Copyright 1990 IRCAM
;******************************************************************

; THIS FILE CONTAINS SOME USEFUL GENERAL-PURPOSE FUNCTIONS FOR DG SYSTEM

(in-package :cr)

;-----------------------------------------------------------------------------
; AVAILABLE FUNCTIONS
;	advance
;	build-cwt
;	build-fqwt
;	firstn
;	load-db / load-ve
;-----------------------------------------------------------------------------
;firstn
(setf ms-test-list '(0 1 2 3 4 5))
(firstn 3 ms-test-list)

;-----------------------------------------------------------------------------
; (advance n l)
;"Return the list l without the first n els when n = 0, return the same list"
(advance 3 ms-test-list)
;-----------------------------------------------------------------------------
;____________________________________________________________________________
; chroma/dg/df.lisp
;____________________________________________________________________________
;*****************************************************************************
;-------| DG SYSTEM
;-------| This file is: $LLdg/DF.ll
;-------| Version V1.0: Jan 22, 1990, V2.0, Aug 2000
;-------| Rev. 3.0, Marco, Feb 2010
;-------| By Marco Stroppa
;-------| Copyright 1990 IRCAM
;*****************************************************************************

; THIS FILES CONTAINS ALL THE FUNCTIONS THAT GENERATE DATA PASSED AS TABLE-
;    -LOOKUP FUNCTIONS THAT WILL BE SAMPLED A CERTAIN AMOUNT OF TIMES

; ADDED AUGUST 2000: VARIOUS KINDS OF INTERPOLATION (SEE FUN.LISP)

(in-package :cr)

;-----------------------------------------------------------------------------
; GLOBALS
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
; AVAILABLE FUNCTIONS (in alphabetical order):
;	lkp / fix-lkp	: linear table-lookup (fix-point output)
;	lkpr / fix-lkp	: linear table-lookup with rescaling of y values
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (lkp NEV FUN) / (fix-lkp NEV FUN)
; 	sample FUN NEV equally spaced times
;	FUN must be of type FUN
;	the input format is NO LONGER compatible with lkpr
; EX: (setf fun (make_fun '(0 0 1 1))) (setf a '(0 0 1 1)),
; (lkp 11 fun) ===> (0 .1 .2 .3 .4 .5 .6 .7 .8 .9 1)
;ADDED 0008, Marco: generalize other forms of interpolation

(lkp 11 (make_fun '(0.0 0 1.0 1 0.0 2)))
(lkp 11 (make_fun '(0.0 0 1.0 1 0.0 2)) -1.0)
(lkp 11 (make_fun '(0.0 0 1.0 1 0.0 2)) 1.0)
(lkp 11 (make_fun '(0.0 0 1.0 1 0.0 2)) '(exp 1.0))
(lkp 11 (make_fun '(0.0 0 1.0 1 0.0 2)) '(exp -1.0))

(lkp 11 (make_fun '(0.0 0 1.0 1 0.0 2)) 'sin1)
(lkp 11 (make_fun '(0.0 0 1.0 1 0.0 2)) 'sin2)
(lkp 11 (make_fun '(0.0 0 1.0 1 0.0 2)) 1.0)

(fix-lkp 11 (make_fun '(0.0 0 1000.0 1 0.0 2)))
(fix-lkp 11 (make_fun '(0.0 0 1000.0 1 0.0 2)) -1.0)
(fix-lkp 11 (make_fun '(0.0 0 1000.0 1 0.0 2)) 1.0)
(fix-lkp 11 (make_fun '(0.0 0 1000.0 1 0.0 2)) '(exp -1.0))
(fix-lkp 11 (make_fun '(0.0 0 1000.0 1 0.0 2)) '(exp 1.0))

(fix-lkp 11 (make_fun '(0.0 0 1000.0 1 0.0 2)) 'sin1)
(fix-lkp 11 (make_fun '(0.0 0 1000.0 1 0.0 2)) 'sin2)
(fix-lkp 11 (make_fun '(0.0 0 1000.0 1 0.0 2)) 1.0)

;-----------------------------------------------------------------------------
;(defun lkpr (nev fun ymin ymax &optional (exp 0.0) &key (offset 0.0))
; (lkpr NEV FUN YMIN YMAX [OFFS]) / (fix-lkpr NEV FUN YMIN YMAX [OFFS])
; 	sample FUN NEV equally spaced times
;	Y values will be rescaled to from YMIN and YMAX and offset by OFFS
;	FUN must be of type FUN
;	to make the format compatible with lkp, if YMIN and YMAX are not
;	   specified, lkpr behaves like lkp
; EX: (setf fun (make_fun '(0 0 1 1))), (lkpr 5 fun -10 10) ===> (-10 -5 0 5 10)

(lkpr 10 (make_fun '(0.0 0 1000.0 1 0.0 2)) -10.0 20.0)
(lkpr 5 (make_fun '(0 0 1 1)) -10 10)
(lkpr 5 (make_fun '(0 0 1 1)) -10 10 1.0)
(lkpr 5 (make_fun '(0 0 1 1)) -10 10 -1.0)
(lkpr 5 (make_fun '(0 0 1 1)) -10 10 'sin1)
(lkpr 5 (make_fun '(0 0 1 1 0 2)) -10 10 'sin2)
(lkpr 5 (make_fun '(0 0 1 1 0 2)) -10 10 'sin3)

(lkpr 5 (make_fun '(0 0 1 1)) -10 10 0.0 :offset 100.0)

(fix-lkpr 10 (make_fun '(0.0 0 1000.0 1 0.0 2)) -10.0 20.0)
(fix-lkpr 5 (make_fun '(0 0 1 1)) -10 10)
(fix-lkpr 5 (make_fun '(0 0 1 1)) -10 10 1.0)
(fix-lkpr 5 (make_fun '(0 0 1 1)) -10 10 -1.0)
(fix-lkpr 5 (make_fun '(0 0 1 1)) -10 10 'sin1)
(fix-lkpr 5 (make_fun '(0 0 1 1 0 2)) -10 10 'sin2)
(fix-lkpr 5 (make_fun '(0 0 1 1 0 2)) -10 10 'sin3)

(fix-lkpr 5 (make_fun '(0 0 1 1)) -10 10 0.0 :offset 100.0)

;-----------------------------------------------------------------------------



;____________________________________________________________________________
; chroma/dg/dv.lisp
;____________________________________________________________________________
;*****************************************************************************
;-------| DG SYSTEM
;-------| This file is: $LLdg/DV.ll
;-------| Version V1.0: Jan 22, 1990
;-------| By Marco Stroppa
;-------| Copyright 1990 IRCAM
;*****************************************************************************

; THIS FILES CONTAINS ALL THE FUNCTIONS THAT GENERATE DATA PASSED AS DIRECT
;     VALUES TO BE GENERATED A CERTAIN AMOUNT OF TIMES

(in-package :cr)

;-----------------------------------------------------------------------------
; GLOBALS
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
; AVAILABLE FUNCTIONS
;	l-val		: list of values
;	lp		: loop
;	bkwd-lp		: backward loop
;	rept		: repetition
;	rept-lp		: repetition with loop at the end
;	bkwd-rept-lp	: repetition with backward loop et the end
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (l-val NEV LIST)
; 	if (length LIST) < NEV, then repeat the last value until NEV
;	   else give back the first NEV els of LIST
; EX: (setf a '(1 2 3 4 5)), (l-val 10 a) ===> '(1 2 3 4 5 5 5 5 5 5)
(l-val 10 '(1 2 3))
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (lp NEV LIST)
; 	 loop along the values contained in LIST
; EX: (setf a '(1 2 3)), (lp 10 a) ===> '(1 2 3 1 2 3 1 2 3 1)
(lp 10 '(1 2 3))
;-----------------------------------------------------------------------------
; (bkwd-lp NEV LIST)
; (bkwd1-lp NEV LIST)
; 	loop along the values contained in LIST going backward and forward
; 	bkwd1: do not repeat the border values if the list has > 2 els
; EX: (setf a '(1 2 3)), (bkwd-lp 10 a) ===> '(1 2 3 3 2 1 1 2 3 3)
; EX: (setf a '(1 2 3 4)), (bkwd1-lp 10 a) ===> '(1 2 3 4 3 2 1 2 3 4)
(bkwd-lp 20 '(1 2 3))
(bkwd1-lp 20 '(1 2 3))
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (rept NEV VAL)
; 	repeat VAL NEV times
; EX: (rept 10 'a) ===> (a a a a a a a a a a)
(rept 10 'a)
(rept 10 '(1))
;-----------------------------------------------------------------------------
; (rept-lp NEV LIST)
; 	loop along the last values contained in LIST
;	LIST has the structure: (val1 ... valM (l1 ... lN)), loop is between
;	   l1 and lN
; EX: (setf a '(1 2 (3 4 5))), (rept-lp 10 a) ===> '(1 2 3 4 5 3 4 5 3 4)
(rept-lp 19 '(1 2 (3 4 5)))
(rept-lp 19 '(1 2))
;-----------------------------------------------------------------------------
; (bkwd-rept-lp NEV LIST)
; (bkwd1-rept-lp NEV LIST)
; 	loop along the last values contained in LIST backward and forward
;	LIST has the structure: (val1 ... valM (l1 ... lN)), loop is between
;	   l1 and lN
;	bkwd1: do not repeat the border values if the list has > 2 els
; EX: (setf a '(1 2 (3 4 5))), (bkwd-rept-lp 10 a)  ===> '(1 2 3 4 5 5 4 3 3 4)
; EX: (setf a '(1 2 (3 4 5))), (bkwd1-rept-lp 10 a) ===> '(1 2 3 4 5 4 3 4 5 4)
(bkwd-rept-lp 19 '(1 2 (3 4 5)))
(bkwd1-rept-lp 19 '(1 2 (3 4 5)))
;-----------------------------------------------------------------------------
;____________________________________________________________________________
; chroma/dg/spectrum.lisp
;____________________________________________________________________________
;*****************************************************************************
;-------| DG SYSTEM
;-------| This file is: $LLdg/spectrum.ll
;-------| Version V1.0: Jan 22, 1990, Rev. 2.0, Feb 2010
;-------| By Marco Stroppa
;-------| Copyright 1990 IRCAM
;*****************************************************************************

; THIS FILES CONTAINS VARYING FUNCTIONS TO GENERATE HARMONIC, SHIFTED,
;	AND STRETCHED SPECTRA ACCORDING TO MC ADAMS' FORMULAS

(in-package :cr)
;-----------------------------------------------------------------------------
; AVAILABLE FUNCTIONS
;	sp
;	spsh
;	spst
;	spsht
;	xpose / xpose-n
;	xp-int / xp-int-1
;	stretch-factor
;	correct-shift
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (sp F0 NP [STON])
;	generate a harmonic spectrum
;	F0 = fundamental frequency
;	NP = number of components to generate
;	   it may be:
;		a NUMBER : all the components from 1 to NP will be generated
;		a LIST containing a list of the scalers of f0
;	STON = % of detuning applied when computing each component [0-1]
; EX: (sp 440 10), (sp 440 10 0.06), (sp 440 '(2 3.3 5.1 7) 0.01)
(sp 100.0 10)
(sp 100.0 10 0.1)

(sp 100.0 '(1 3 5 7))
(sp 100.0 '(1 3 5 7) 0.1)
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (spsh F0 NP SH [STON])
;	generate a shifted spectrum
;	F0 = fundamental frequency
;	NP = number of components to generate
;	   it may be:
;		a NUMBER : all the components from 1 to NP will be generated
;		a LIST containing a list of the scalers of f0
;	SH = shifting factor (% of f0)
;	STON = % of detuning applied when computing each component [0-1]
; EX: (spsh 440 10 0.06), (spsh 440 10 0.1 0.06)
;	(spsh 440 '(2 3.3 5.1 7) 0.5 0.01)
(spsh 100.0 10 0.06)
(spsh 100.0 10 0.06 0.1)

(spsh 100.0 '(1 3 5 7) 0.06)
(spsh 100.0 '(1 3 5 7) 0.06 0.1)
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (spst F0 NP ST [STON])
;	generate a stretched/compressed spectrum
;	F0 = fundamental frequency
;	NP = number of components to generate
;	   it may be:
;		a NUMBER : all the components from 1 to NP will be generated
;		a LIST containing a list of the scalers of f0
;	ST = stretching/compressing factor (2.0 = octave)
;	STON = % of detuning applied when computing each component [0-1]
; EX: (spst 440 10 2.1), (spst 440 10 1.76 0.06)
;	(spst 440 '(2 3.3 5.1 7) 2.5 0.01)
(spst 100.0 10 2.1)
(spst 100.0 10 1.9)
;(spst 100.0 10 1.0) ; same vals
;(spst 100.0 10 0.5) ; going down (not really meaningful)

(spst 100.0 '(1 3 5 7) 2.1)
(spst 100.0 '(1 3 5 7) 1.9)
;(spst 100.0 '(1 3 5 7) 1.0) ; same vals
;(spst 100.0 '(1 3 5 7) 0.5) ; going down (not really meaningful)
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (spsht F0 NP SH ST [STON])
;	generate a shifted spectrum
;	F0 = fundamental frequency
;	NP = number of components to generate
;	   it may be:
;		a NUMBER : all the components from 1 to NP will be generated
;		a LIST containing a list of the scalers of f0
;	SH = shifting factor (% of f0)
;	ST = stretching/compressing factor (2.0 = octave)
;	STON = % of detuning applied when computing each component [0-1]
; EX: (spsht 440 10 0.06 2.0) , (spsht 440 10 1.76 0.06)
;	(spsht 440 '(2 3.3 5.1 7) 2.5 0.01)
(spsht 100.0 10 0.0 2.0 0.0)
(spsht 100.0 10 0.1 2.0 0.0)
(spsht 100.0 10 0.0 2.1 0.0)
(spsht 100.0 10 0.0 2.0 0.1)
(spsht 100.0 10 0.1 1.9 0.0)
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (xpose SP INT)
;	transpose a spectrum (list of freq) by an interval INT (scaler of f0)

; (xpose-n N fq SP)
;	transpose a spectrum (list of freq) so that it's N'th component matches fq

; EX: (xpose (spsht 440 10 0.0 2.0) 0.5) --> octave below
;     (xpose (spsht 440 10 0.0 2.0) 2) --> fifth above
;     (xpose-n 3 (pch->fq 'do4) (spsht 100 10 0.0 2.0)) --> 3rd ovtn to C4
;     (xpose-n 3 400.0 (spsht 100 10 0.0 2.0)) --> 3rd ovtn to 400.0
;-----------------------------------------------------------------------------
(xpose (sp 100.0 10) 0.5)
(xpose (sp 100.0 10) 1.06)
(xpose (sp 100.0 10) 2.0)

(xpose-n (sp 100.0 10) 2 222.0)
(xpose-n (sp 100.0 10) 20 222.0)    
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (xp-int INT) / (xp-int-1 INT)
;	generate a transposing interval of INT (in semitones, either positive
;	   or negative) to use with xpose and other functions
;	xp-int will work with functions needing with absolute values (e.g. xpose),
;	   whereas xp-int-1 is suited to functions in which the transposing
;	   factor will then be multiplied by the reference values (e.g. the
;	   sh factor of spsh[t])
;	no transposition : xp-int = 1, xp-int-1 = 0
;	one octave : xp-int = 2, xp-int-1 = 1

; EX:     (xpose (spsht 440 10 1.76 0.06) (xp-int 7)) --> fifth above
;-----------------------------------------------------------------------------
(xp-int 12.0)
(xp-int -12.0)
(xpose (spsht 100.0 10 0.0 2.0) (xp-int 12))
(xpose (spsht 100.0 10 0.0 2.0) (xp-int -12))


(xp-int-1 12.0)
(xp-int-1 -12.0)
;-----------------------------------------------------------------------------
; (stretch-factor stretched_up stretched_down ref_up ref_down)
;	generate the stretching factor needed by spsht in order to have
;	   partials j, i correspond to the ratio fj / fi in the stretched
;	   spectrum.
;Computes the streching factor needed by spsht in order to have the usually harmonic
;partials i, j match fi, fj in the stretched or compressed spectrum.
;2.0 = harmonic spectrum
;1.0 < x < 2.0 = compressed spectrum
;x > 2.0 = stretched spectrum
;Beware: values <= 1.0 are meaningless.
;-----------------------------------------------------------------------------
;changed order of args, 1002, ms
(stretch-factor 5 3 5 3)
(stretch-factor 3 5 (car (spsht 1 '(3) 0 2.2)) (car (spsht 1 '(5) 0 2.2)) )
(stretch-factor 5 3 (car (spsht 1 '(5) 0 1.2)) (car (spsht 1 '(3) 0 1.2)) )

; (stretch-factor 5 3 5 3) -> 2.0
; (stretch-factor 3 5 (car (spsht 1 '(3) 0 2.2)) (car (spsht 1 '(5) 0 2.2)) ) -> 2.2
; (stretch-factor 5 3 (car (spsht 1 '(5) 0 1.2)) (car (spsht 1 '(3) 0 1.2)) ) -> 1.2

;-----------------------------------------------------------------------------
;added 1002, ms
(correct-shift 100.0 0.1)
(spsh (correct-shift 100.0 0.1) 10 0.1)
;-----------------------------------------------------------------------------



;**********************************************
(print "Finished loading OMChroma-dg-tests")
;**********************************************