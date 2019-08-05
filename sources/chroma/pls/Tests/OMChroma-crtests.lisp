;***********************************
; COMPLETE TESTS OF OMCHROMA ALONE |
;***********************************

;*****************************
;INITIAL FILE: OMChroma.lisp |
;*****************************

; OMChroma
; High-level control of sound synthesis in OM
;
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307,10 USA.
;
; (c) Ircam 2000 - 2019
;Authors: M. Stroppa, C. Agon, J. Bresson, S. Lemouton

;;;================================
;;; Lib loader for OM 6
;;;================================

; NB: all the references to OM are placed outside of the Chroma folder, for reasons of conceptual cleannes

(in-package :om)

#|
(load (merge-pathnames "sources/package" *load-pathname*)) - TESTED
(load (merge-pathnames "sources/chroma/load" *load-pathname*)) - TESTED

;FILES BELOW NOT TESTED IN THIS FILE
 '(
   "sources/om6/chroma-init"
   "sources/om6/chroma-fun"
   "sources/om6/cs-events/general-parsing"
                        
   "sources/om6/cs-events/csound/cs-utils"
   "sources/om6/cs-events/csound/cs-tables"
   "sources/om6/cs-events/csound/csound-evt"
   "sources/om6/cs-events/csound/csound-tools"
   "sources/om6/cs-events/csound/csound-parsing"

   "sources/om6/cs-events/user-funs/user-funs"
   "sources/om6/cs-events/user-funs/methods"
   
   "sources/om6/cs-events/chromaspat"

   "sources/om6/chroma-reference"
   "sources/om6/doc-chroma"

   "sources/om6/cr-models/vps-tools"
   "sources/om6/cr-models/vpseditor"
   "sources/om6/cr-models/models"
   "sources/om6/cr-models/processing"
   "sources/om6/cr-models/controls"
   "sources/om6/cr-models/gen-model-data"                      
                        
   ))
|#

;____________________________________________________________________________
; package.lisp
;____________________________________________________________________________
#|
(defpackage "CHROMA" 
  (:use "COMMON-LISP")
  (:nicknames "CR"))

;____________________________________________________________________________
; OMCHROMA LOADED FILES (load.lisp)
; NO DEPENDENCIES FROM OM!!!
;____________________________________________________________________________
(in-package :cl-user)
  
;;; clean-sources is defined in OM load utils
; (clean-sources (make-pathname :directory (pathname-directory *load-pathname*)))

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
                      
        ;DG
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
; chroma/init-chroma.lisp
; initializations and export to package ::om
;____________________________________________________________________________

(let ((lib (om:find-library "OMChroma")))
  (unless (om::loaded? lib)
    (om::load-om-lib lib)))

(in-package :cr)
*cr-root*
*cr-out-dir*
*cr-csfun-dir*
*cr-wt-dir*
*cr-models-dir*
*cr-userfun-dir*
*cr-tmp-dir*

;(defun get-cr-path (dir &key name type subdirs)
(get-cr-path :tmp)
(get-cr-path :tmp :name "foo" :type "bar")
(get-cr-path :tmp :name "foo" :type "bar" :subdirs '("baz" "ttt"))

;(choose-file-dialog)
(cr-beep)
(cr-beep "hello")
(cr-beep 'hello)

(sound-file-get-info (merge-pathnames (get-cr-path :wt) "wt-tpt.aif"))

; EXPORT
;vps/pitch-conversions
;'(fq->pch fq->midi fq->ratio fq->midic  fq->itvl fq->semitones
;                  pch->fq pch->midi pch->midic pch->itvl pch->semitones pch->pch-class pch->ratio
;                  midi->pch midi->semitones midi->pch-class midi->midic midi->fq midi->ratio midi->itvl
;                  midic->midi midic->fq midic->pch midic->ratio midic->itvl midic->semitones midic->pch-class
;                  ratio->fq ratio->itvl ratio->semitones ratio->midi ratio->midic ratio->pch
;                  itvl->fq itvl->midi itvl->midic itvl->ratio itvl->pch itvl->semitones
;                  semitones->ratio semitones->itvl semitones->fq semitones->midi semitones->midic 
;                  pch-class->pch pch-class->midi pch-class->fq)

;vps/vps
;'(fql ptl ail spl cil crl arl rpl)
;(pathname-directory *load-pathname*)

;____________________________________________________________________________
; chroma/globals.lisp
; various global variables, some are OM-dependent
;____________________________________________________________________________
;*******************************************************************
;-------| CHROMA SYSTEM
;-------| This file is: $LLsys/globals.lisp
;-------| Designed and implemented in LeLisp by Marco Stroppa 
;-------| Ported to Common Lisp by Serge Lemouton
;-------| Version: Nov 28, 1996
;-------| Modified March 2000 (added dynamic functions in csound)
;-------| ADAPTED to omChroma 050221, Marco Stroppa
;*******************************************************************

;-----------------------------------------------------------------------------
(get-gbl 'DEBFLG)
(set-gbl 'DEBFLG 0)

;-----------------------------------------------------------------------------
; DEBUGGING FLAGS
; default value
(deb-mode)
(normal-mode)
(alternative-mode)

;-----------------------------------------------------------------------------
; PRINTOUT
; default values

(set-gbl 'PRNFLG t) 
(set-gbl 'CTL1-PRINT t) 
(set-gbl 'CTL2-PRINT t) 

(enable-print)
(disable-print)
(enable-print-ctl1)
(disable-print-ctl1)
(enable-print-ctl2)
(disable-print-ctl2)

;-----------------------------------------------------------------------------
; SYNTHESIS ENABLE FLAG
; kept for historical compatibility
(get-gbl 'PROBLM)
(unable-synthesis)
(enable-synthesis)
(disable-synthesis)


;******************************************************************
; GLOBAL VARIABLES FOR WHOLE SYSTEM
;------------------------------------------------------------------
*MONTHS*
;-----------------------------------------------------------------------------

; NEW SETUP, UP TO 1000.0 ABSOLUTE, FOR REASONS OF DECIMAL QUALITY
(set-gbl 'MAXAMP 1000.0)		; MAXIMUM AMPLITUDE FOR SYNTHESIZER
(set-gbl 'GBLAMP 1000.0)		; GLOBAL AMPLITUDE FOR LOCAL EVENT

(get-gbl 'SR)		; SAMPLING RATE
(get-gbl 'SR/2)		; NYQUIST FREQUENCY
(get-gbl 'KR)		; CONTROL RATE FOR CSOUND
(get-gbl 'NCH)		; NUMBER OF CHANNELS
(get-gbl 'DURMIN)		; MINIMUM DURATION FOR A NOTE
(get-gbl 'MINFQ)		; MINIMUM FREQUENCY FOR A NOTE
(get-gbl 'MAXSI)		; MAXIMUM SAMPLE INCREMENT WHEN READING A WT

(get-gbl 'DEF-GEN-SIZE)	; DEFAULT GEN SIZE
(get-gbl 'EXPZERO)	; REPLACE 0 IN EXPONENTIAL GENS

(get-gbl 'PMAX) ; MAXIMUM NUMBER OF CSOUND P FIELDS
;(set-gbl 'PMAX om::*cs-max-points*) ; MAXIMUM NUMBER OF CSOUND P FIELDS
                            ; USED xWHEN COMPILING FUNCTIONS (= MAX - 2,
                            ;    USED AT THE BEGINNING)
                            ; IF () NO CONTROL
(get-gbl 'DIAPASON)	; CURRENT DIAPASON


;-----------------------------------------------------------------------------
(get-gbl 'PRT-FM)		; PORTAMENTO FLAG FOR FM (USED IN CIF D. BASE)
;				   0 = PORTAMENTO AFFECTS CARRIER FREQ
;				   1 = PORTAMENTO AFFECTS MODULATING FREQ
(get-gbl 'PRNFLG)		; PRINTOUT FLAG
;				   t = ENABLE ALL PRINTOUTS
;				  () = PRINT ONLY ERROR AND DISCARDED PARTIALS
;-----------------------------------------------------------------------------

(get-gbl '*chroma-output*)


(get-gbl 'USER)	; USER'S NAME FOR PERSONALIZED MESSAGES

;-----------------------------------------------------------------------------;-----------------------------------------------------------------------------
; Temporary place for the dynamically computed GENS of a WT object
(get-gbl 'WTL)		; A-LIST OF WT OBJECTS
(get-gbl 'WTIND)		; INDEX OF CURRENT WT OBJECT

;		CSOUND STRUCTURE : '(	(sf1 dir1 f1 . n-smpls1)
;					(sf2 dir2 f2 . n-smpls2) ...)
;		WHERE:	sfX = name of sound file 1, 2, etc.
;			f1 = function number associated to that sound file
;			for csound a f{fX} function will be associated to
;			   input sf sndin.{position-in-A-list} [symbolic link]
				; DIR OF SOUND FILES ON MARC TO LOAD ON MOON

(get-gbl 'WTFO)		; OFFSET WHEN WRITING CSOUND GEN 1 TABLES
(get-gbl 'WTFOMAX)		; MAXIMUM OFFSET WHEN WRITING CSOUND GEN 1
                                ; PAY ATTENTION THAT IT IS NOT OVERLAPPING WITH
                                ;  THE AUTOMATIC TABLE NUMBER IN OM PREFERENCES
;(set-gbl 'DEFXFC ())		; DEFAULT CONTROL FOR INTERFACE OF WT SYSTEM

;-----------------------------------------------------------------------------
; CONTROL OF THE FREEZE ALGORITHM FOR WT OBJECTS
;  PARAMETERS ARE RELATIVE TO A BASIC OVERLAP, SET TO 10% OF THE DURATION
;    OF THE CURRENT WT OBJECT
; SEE FILE wt-frz.ll IN $LLctl1 FOR DETAILS
(get-gbl 'DEF-FRZ)		;  DEFAULT FREEZE PARAMETERS

;TEST
;(set-gbl 'DEF-FRZ		;  DEFAULT FREEZE PARAMETERS
;     ''(
;	 (inc-pt 0.1)
;	 (dur 0.5)
;	 (skip 0.2)
;	 (end- 0.25)
;	 (ampfac 0.5)
;	 (xin 0.1)
;	 (xout 0.15)
;	 (first-xout 0.25)
;	 (last-xin 0.2)
;    ))
(get-gbl 'FRZ-MODE)		; DEFAULT FREEZE MODE = RUN FREEZE WHEN NEEDED
;*****************************************************************************

;____________________________________________________________________________
; chroma/utils-cr.lisp
; Main utilities
;____________________________________________________________________________
;******************************************************************
;-------| CHROMA SYSTEM
;-------| This file is: $LLsys/utils-cr.lisp
;-------| Implemented by Marco Stroppa
;-------| Version: 000817, updated 050221, 060814
;******************************************************************

; THIS FILE CONTAINS THE DEFINITION OF MIXED FUNCTIONS GENERALLY USEFUL EITHER
;   FOR THE CHROMA SYSTEM INTERNALLY ONLY OR BETWEEN CHROMA AND OM

(in-package :cr)

;------------------------------------------------------------------
; FUNCTIONS (in alphabetical order):
;	bpf->fun
;	fun->bpf
;	fun->gen-cs-table
;	ran / ran-from / ran%
;	select
;       within-p
;------------------------------------------------------------------
; MACROS (in alphabetical order):
;	cassq
;	ifn
;	nextl
;	newl
;------------------------------------------------------------------
; METHODS! (in alphabetical order):
;	find-chroma-file
;------------------------------------------------------------------

(lintodb 0.5)
(lintodb 0.0)
(dbtolin 0.0)
(dbtolin -40.0)

;;;================================================================
;;;; MACROS
; (cassq s l)
;(defun cassq (sym l)
(setf tmp-list '((a 1) (b 2) (c 3)))
(cassq 'b tmp-list)

; (ifn test body)
;(defmacro ifn (testform elseform &body body)
(ifn t 'a 'b)
(ifn nil 'a 'b)

;(defmacro nextl (lst &optional symb)
(nextl tmp-list)
tmp-list
(nextl tmp-list tmp-symb)
tmp-symb

;(defmacro newl (lst elem) `(push ,elem ,lst))
(newl tmp-list '(d 4))
;------------------------------------------------------------------

(flat '((0) ((1)) (2) 3 4 (((5)))))

(mat-trans '((0 10) (1 11) (2 12) (3)))
(mat-trans '((0 1 2 3) (10 11 12 13)))

;------------------------------------------------------------------
; select (rewrites a function that disappeared in OM 5)
;	NAME:		select  (SELECTOR)
;	TYPE:		Expr with 2 arguments
;	CALL:		(select <list> <pos>)
;	FUNCTION:	extracts the elements at position <pos> in <list> (1 = first element)
;	VALUE:		the list with the selected elements
;	SOURCE:		$LLsys/utils.lisp

;(defun select (list pos)
;"Extracts the elements at position <pos> from <list>.
; Contrary to OM's posn-match the first element is number 1."
(select '(1 2 3 4 5) '(2 3))

;------------------------------------------------------------------
; within-p (el range)
;ms_1109
;(defun within-p (el range &optional max)
;" If range is a list of two values: return t, if el is contained within range.
;  If it is a number, it indicated the min value, then optional is needed,
;    and the function returns t if el is between min and max.
;If el is a list, test each el of the list.
;Ex: (within-p 6000 '(5000 7000))
;    (within-p 6000 5000 7000)
;    (within-p '(6000 8000) '(5000 7000))
;"
(within-p 6000 '(5000 7000))
(within-p 6000 5000 7000)
(within-p '(6000 8000) '(5000 7000))
;------------------------------------------------------------------

; (ran [itvl] / [itvl var] )
; (ran% [itvl] / [itvl var] )
; (ran-from from to)
;	ran returns a random number comprised between -itvl and +itvl
;	   (-1/+1 if itvl is not present), or between itvl-var and itvl+var
;	ran% is the same as ran, but var is expressed in % [0-1] of itvl

;(defun ran (&rest itvl)
(ran)
(ran 10.0)

;(defun ran-from (from to)
(ran-from 10 20)
(ran-from 10.0 20.0)

;(defun ran% (&rest itvl)
(ran%)
(ran% 20)

(printdate ())
(stringdate)

;------------------------------------------------------------------
;(export '(fun->bpf bpf->fun fun->gen-cs-table within-p)
;        :chroma)
;------------------------------------------------------------------
;____________________________________________________________________________
; chroma/pls/utils-pls.lisp
; Main utilities for the type-based PLS system
;____________________________________________________________________________

;******************************************************************
;                PULS SYSTEM - chroma/pls
;******************************************************************
;-------| PULS SYSTEM
;-------| This file is: $LLpls/basic.ll
;-------| Version V1.1: Jan 17, 1990
;-------| By Marco Stroppa
;-------| Copyright 1986 MIT
;-------| ADAPTED to omChroma, 050221
;******************************************************************

; THIS FILE CONTAINS THE BASIC FUNCTIONS FOR THE PULS SYSTEM
;      IT SHOULD BE LOADED AT THE BEGINNING

(in-package :cr)

;-----------------------------------------------------------------------------
; AVAILABLE GENERAL FUNCTIONS (in alphabetical order):
;	attach
;	cassq (moved to utils-cr.lisp)
;	catenate/concat
;       choose-in-list
;	error-type
;	pls-check-type
;	prin
;	pwr2
;	rplac
;-----------------------------------------------------------------------------

; (attach el l)
;(defun attach (s l)
;";Physically attach element <el> on top of list <l>
;NOTE: <l> cannot be empty"

(setf my-list '(10 20 30))
(attach 'a '(1 2 3))
(attach 'aa my-list)
my-list

;-----------------------------------------------------------------------------
; (pls-check-type typ str fun-name)

;(defun pls-check-type (typ str fun-name)
;"Check that structure <str> is of type <typ>.
;<fun-name> is the name of the function calling pls-check-type."

(setf my-str (make_tbl '(0 0 1 1)))
(setf my-fun (make_fun '(0 0  1 1  0 2)))
(is_fun my-str)
(is_tbl my-str)
(pls-check-type 'tbl my-str 'my-fun)
;error
;(pls-check-type 'fun my-str 'my-fun)

;-----------------------------------------------------------------------------
; (error-type f l)
;(defun error-type (f l)
;"Signal an error of type for function f"
;(error-type 'foo 'bar)

;-----------------------------------------------------------------------------
; (choose-in-list l)
;(defun choose-in-list (list)
;  "Random selection in list"
(choose-in-list '(1 2 3 a 4 5 6))

;-----------------------------------------------------------------------------
;pwr2, same as closest-pwr2
;(defun pwr2 (n)
;"RETURN THE POWER OF TWO IMMEDIATELY > n"
(pwr2 1000)

;-----------------------------------------------------------------------------
;rplac
;(defun rplac (l s1 s2)
(setf my-list '(a b c))
(rplac my-list 1 '(2 3 a))

;-----------------------------------------------------------------------------
;prin
;(defun prin (&rest args)

(prin '(a b c))
(prin 1111 2222 3333)

;(defun prin_ (list-args)
(prin_ '(22 33 44))
(print nil)
;-----------------------------------------------------------------------------
;catenate/concat

;(defun catenate (x y)
;"String any number of args"
(catenate 1 2)
(catenate 1 2 3 4 5)
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; HISTORICAL AND IN-OUT FUNCTIONS (in alphabetical order):
;	build-cwt / build-fqwt
;	error-synth
;	load-wt
;       load-comp-wt
;	save-wt
;	get-sndinfo
;	out-wt
;	out-wt-cs
;-----------------------------------------------------------------------------
; error-synth / DEPRECATED
;(defun error-synth (function synth)
;"Print an error message if the wrong synthesizer is selected"
;(error-synth 'foo 'bar)

;-----------------------------------------------------------------------------
; build-cwt
; (build-cwt L-CWT)
;  EX: (build-cwt 'wt-a1 '(wt-a2 0 1) '(wt-a3 (att_wt wt-a3) '(dur_wt wt-a3)))
;(defun build-cwt (&rest l-cwt)
;"B(uild) a CWT field for CTL1 level (shortcut)
;where L-CWT: any wt objects (just the name if alone, the list (name val1 val2) if start and end offs are set)"
(setf my-cwt (build-cwt 'wt-a1 '(wt-a2 0 1) '(wt-a3 (att_wt wt-a3) '(dur_wt wt-a3))))

;-----------------------------------------------------------------------------
; build-fqwt
; (build-fqwt L-FQ)
;  EX: (build-fqwt (1.0) (1.059 (nth-freq_wt wt-a2 2)) (440 261))
;(defun build-fqwt (&rest l-fqwt)
;"B(uild) a FQ field for CTL1 level WTsys (shortcut).
;where L-FQ: list of (out-fq [ref-fq]) by default, ref-fq = 1.0."
(build-fqwt '(1.0) '(1.059 (nth-freq_wt wt-a2 2)) '(440 261))

;-----------------------------------------------------------------------------
;---------> TOTEST
; load-wt load-comp-wt / save-wt
;	load the data base containing the definitions of the WT objects used
;	   by the WT system (original or compiled)
;	save an already defined object into its internal, compiled form
;	   (so that when one loads it again one will not compute it)
;	the name of the file to be computed is : "name"_WT.lisp
;	the name of the file of compiled objects is:  CP_"name"_WT.lisp
;	the file will be looked for in $LLwt
; (load-[comp-]wt name) / (load-[comp-]wt '(<name1> ... <nameN>))
; (save-wt file-name obj-1 ... obj-n)

;(defun load-wt (name &optional (subdirs nil))
;"Load a data base of WT objects from a fixed directory specified in the environment.

;<name>: basic name of the file (string or symbol) or list of names
;<subdir> [nil]: list of subdirectories (strings)

;The file will be looked for in the directory specified by (getenv 'LLwt) and
;   is called <name>_WT.lisp"

;-----------------------------------------------------------------------------
;---------> TOTEST
; modified by Marco, 020325
;(defun load-comp-wt (name &optional (subdirs nil))
;"Load a data base of compiled WT objects from a fixed directory.

;<name>: basic name of the file (string or symbol) or list of names
;<subdir>: subdirectories (string)

;The file will be looked for in the directory specified by (getenv 'LLwt) and
;   is called <name>_CP-wt.lisp

;TO SAVE A COMPILED VERSION OF A DATA BASE OF WT objects TYPE:
;      (save-wt <name> 'WT-OBJ-1 ... 'WT-OBJ-N) 
;      THE REAL NAME OF COMPILED VERSION IS: <name>_CP-wt.ll"


;-----------------------------------------------------------------------------
;---------> TOTEST
; modified by Marco, 020325
;(defun save-wt (file &rest objs)
;"Compile the WT-objects onto a file named <name>_CP-wt.lisp in the directory
;specified by the environment variable LLwt"

;-----------------------------------------------------------------------------
;---------> TOTEST
;(defun get-sndinfo (file)
;  '((file    . "/u/tutors/marco/snd/pb2.sf")
;    (SR      . 44100)
;    (dur     .  1.5761)
;    (n-ch    . 2)
;    (pk-mode . float)
;    (smpl-type . float)
;    (smpl-size . 24)
;    (n-smpl  . 139008)
;    (snd-type . "aif")))

; no longer available
;    (peak-amp  . #(522.44421	33.34753))
;    (peak-smpl . #(44933	0]))
;    (peak-time . #( 1.019	 0.000]))
;    ) )
;    (list (cons 'file file-in)
;          (cons 'sr sr)
;          (cons 'dur dur)
;          (cons 'n-ch n-ch)
;          (cons 'pk=mode pk-mode)
;          (cons 'smpl-type pk-mode)
;          (cons 'smpl-size smpl-size)
;          (cons 'n-smpl n-smpl)
;          (cons 'snd-type snd-type))


;(multiple-value-bind (buffer format n-channels sample-rate sample-size size skip)  (audio-io::om-get-sound-info (om::om-choose-file-dialog))
;  (list buffer format n-channels sample-rate sample-size size skip))
;("AIFF(int)" 2 48000 24 11771540 1 nil)
;("AIFF(float)" 1 96000 32 96000 1 nil)
;("Wav(int)" 1 48000 24 100878986 1 nil)

;(defun read-sound-format (format)
; separate the buffer+format string coming from om-get-sound-info and return the two values separately (type and size)
; not very elegant...:-(!!
; if none are found, return the whole format

; new version, using libsndfile, 1801, ms
;(defun get-sndinfo (&rest file-in)

;(get-sndinfo)


;-----------------------------------------------------------------------------
;---------> TOTEST
;auxiliary functions, for debugging purposes (OBSOLETE NOW)
;(defun read-aiff-header (file-in)

;(read-aiff-header (om::om-choose-file-dialog) )

;(defun get-bin-double (stream)

;(defun dec-to-nib (x)
 
;(defun dec-to-bin (x  size )

;-----------------------------------------------------------------------------
;---------> TOTEST
; WRITE A FILE CONTAING THE CORRECT INSTRUCTION TO LOAD THE WAVE TABLES
; THE LIST OF THE WT OBJECTS TO WRITE DOWN IS IN THE GLOBAL "WTL"
;(defun out-wt (file-name &rest dir)

;-----------------------------------------------------------------------------
;---------> TOTEST
;(defun out-wt-cs (file-name &optional (dir (get-gbl 'CSfun)))
;"New version ONLY working with omChroma.
;Older versions are below (grayed).
;filename: name of file, without extension.
;          the file will have the automatic extensions <.fun>.
;dir: directory where the file is to be written (default: value of CSfun)."  
;-----------------------------------------------------------------------------

;____________________________________________________________________________
; chroma/pls/all-purp.lisp
; General functions for the type-based PLS system
;____________________________________________________________________________
;*********************************************************************
;-------| PULS SYSTEM
;-------| This file is: $LLpls/allpurp.ll
;-------| Version V2.0: Apr 18, 2002 / NOW compatible with omchroma
;-------| By Marco Stroppa
;-------| Copyright 1986 MIT
;*********************************************************************

(in-package :cr)

; GENERAL-PURPOSE FUNCTIONS INDEPENDENT OF THE PARTICULAR TYPE

; Each of the functions automatically dispatches on the type of the data
;	structure to a particular function called: "<ALL-PURPOSE-NAME>_<DATA-TYPE>"

; Recognized types are contained in #:marco:pls:*TYPES* (see default.ll)


; AVAILABLE FUNCTIONS:
;	PREDICATES:	is-empty
;	INFO:		sizeof
;			prnt
;			short-prnt
;-----------------------------------------------------------------------------
#|
(defun is-empty (datum)
"Build a function called is-empty_<type-of datum> and call it.

;	NAME:		is-empty  (PREDICATE)
;	TYPE:		Expr with 1 argument (list with 1 or more elements)
;	CALL:		(is-empty datum)
;	FUNCTION:	test whether datum is empty
;	VALUE:		t or () depending on the test
;	SOURCE:		$PLSsys/allpurp.ll"
|#
(setf my-tbl (make_tbl '(a 1 b 2)))
(setf my-tbl1 (make_tbl))
(is-empty my-tbl)
(is-empty my-tbl1)


;-----------------------------------------------------------------------------
#|
(defun sizeof (datum)
"
;	NAME:		sizeof  (INFO)
;	TYPE:		Expr with 1 or more arguments
;	CALL:		(sizeof datum)
;	FUNCTION:	return the size (or number of elements) of datum
;	VALUE:		the number above
;	SOURCE:		$PLSsys/allpurp.ll"
|#
(setf my-ctl (make_ctl '(a 1) '(b 2) '(c 3)))
(sizeof my-ctl)


;-----------------------------------------------------------------------------
#|
(defun prnt (&rest data)
"
;	NAME:		prnt/short-prnt  (INFO)
;	TYPE:		Expr with n arguments
;	CALL:		(prnt datum1 ... datumN)
;			(short-prnt datum1 ... datumN)
;	FUNCTION:	nicely or shortely print data structures
;	VALUE:		the string 'done
;	SOURCE:		$PLSsys/allpurp.ll"
|#
(prnt my-ctl)
(prnt my-fun)
;-----------------------------------------------------------------------------
#|
(defun short-prnt (&rest data)
"
;	NAME:		prnt/short-prnt  (INFO)
;	TYPE:		Expr with n arguments
;	CALL:		(prnt datum1 ... datumN)
;			(short-prnt datum1 ... datumN)
;	FUNCTION:	nicely or shortely print data structures
;	VALUE:		the string 'done
;	SOURCE:		$PLSsys/allpurp.ll"
|#
(short-prnt my-ctl)
(short-prnt my-fun)
;-----------------------------------------------------------------------------

;____________________________________________________________________________
; chroma/pls/type.lisp
; PLS system: generator of abstract types
;____________________________________________________________________________
;*****************************************************************************
;-------| PULS SYSTEM
;-------| This file is: $LLpls/type.ll
;-------| Version V1.1: Jan 17, 1990
;-------| By Marco Stroppa
;-------| Copyright 1986 MIT
;*****************************************************************************

; CONSTRUCTOR AND SELECTORS FOR MANIFEST TYPING ARCHITECTURE


; DESCRIPTION OF THE STRATEGY:
;    A detailed and complete description of this architecture is contained
;	in the file $LLpls/README.

; CURRENT IMPLEMENTATION:
;    A tagged datum is always a cons of a type and its contents.

(in-package :cr)

; AVAILABLE FUNCTIONS:
;	CONSTRUCTORS: attach-type
;	SELECTORS:    pls-type
;		      contents
;	PREDICATES:   is-tagged


; DESCRIPTION OF THE PACKAGE:

;-----------------------------------------------------------------------------
;	NAME:		attach-type  (CONSTRUCTOR)
;	TYPE:		Expr with 2 arguments
;	CALL:		(attach-type type contents)
;	FUNCTION:	define and initialize a data structure of type "type"
;			   and contents "contents"
;	VALUE:		the new data structure
;	SOURCE:		$LLpls/type.ll

(setf my-type (attach-type 'RAW '(1 2 3)))

;-----------------------------------------------------------------------------
;	NAME:		pls-type/contents  (SELECTORS)
;	TYPE:		Expr with 1 argument
;	CALL:		(pls-type datum) / (contents datum)
;	FUNCTION:	returns the type/contents of datum
;	VALUE:		the above values
;	SOURCE:		$LLpls/type.ll

(pls-type my-type)
(pls-type my-fun)

(contents my-type)
(contents my-fun)
(contents my-ctl)

;-----------------------------------------------------------------------------
;	NAME:		is-tagged  (PREDICATE)
;	TYPE:		Expr with 1 argument
;	CALL:		(is-tagged datum)
;	FUNCTION:	test whether datum is a known type
;	VALUE:		the TYPE name or () according to the test
;	SOURCE:		$LLpls/type.ll

(is-tagged 'a)
(is-tagged '(a))
(is-tagged my-fun)
(is-tagged my-ctl)

;-----------------------------------------------------------------------------
; DEFAULT STRUCTURES:
;	*TYPES*	    :  list of all the types known by the tagged architecture
;-----------------------------------------------------------------------------

; types DVE, RAW added on March 2000
;(defvar *TYPES* '(WT CTL TBL FUN VE DVE RAW))
;-----------------------------------------------------------------------------

;____________________________________________________________________________
; chroma/pls/tbl.lisp
; PLS system: generator of tables
;____________________________________________________________________________
;*****************************************************************************
;-------| PULS SYSTEM
;-------| This file is: $LLsys/tbl.ll
;-------| Version V1.1: Aug 13, 1991
;-------| By Marco Stroppa
;-------| Copyright 1986 MIT
;*****************************************************************************

(in-package :cr)

; PACKAGE TO DEAL WITH ONE- OR TWO-DIMENSIONAL TABLES
;                 ASSOCIATED TYPE NAME: TBL


; DESCRIPTION OF THE DATA STRUCTURE:
;    The structure consists of a one- or two-dimensional table, the elements
;       of which are reached through one or two keys.  Their contents can
;	be anything at all and is retrieved in the same conditions it has been
;	put in.
;    Note: insertion of new els is done at the beginning of the structure
;          there is no consideration for any kind of orders (see lstep.ll).

; CURRENT IMPLEMENTATION:
;    The structure of type TBL is an A-list of sub-tables with type prefix.
;       Each sub-table is an A-list of keys/values and is preceded by its key.


; AVAILABLE FUNCTIONS:

;	CONSTRUCTORS: make_tbl
;	SELECTORS:    lookup_tbl
;		      soft_lkp_tbl
;		      lkeys_tbl
;	MODIFIERS:    insert_tbl
;		      rm_tbl
;		      merge_tbl
;	PREDICATES:   is_tbl
;		      is-key_tbl
;		      is-empty_tbl
;	INFO:	      print_tbl
;		      short-print_tbl


; DESCRIPTION OF THE PACKAGE:

;-----------------------------------------------------------------------------
;	NAME:		make_tbl  (CONSTRUCTOR)
;	TYPE:		Expr with 0, 1, 2 or 3 arguments
;	CALLS:		(make_tbl)
;			(make_tbl key1)
;			(make_tbl key1 val1)
;			(make_tbl key1 key2)
;			(make_tbl key1 key2 val2)
;	FUNCTION:	define a structure of type TBL and fills it if args are given
;	VALUE:		the new structure
;	SOURCE:		$LLsys/tbl.ll

(setf my-tbl0 (make_tbl))
(setf my-tbl1 (make_tbl 'a))
(setf my-tbl2 (make_tbl 'a 10))
(setf my-tbl3 (make_tbl 'a 'b 21))


;-----------------------------------------------------------------------------
;	NAME:		lookup_tbl  (SELECTOR)
;	TYPE:		Expr with 2 or 3 arguments
;	CALL:		(lookup_tbl tbl key1 [key2])
;	FUNCTION:	return the value associated to key1 (and key2 if the
;			   structure is two-dimensional)
;	VALUE:		the above value
;	NOTE:		if there is no key1 (and key2) an error is produced
;	SOURCE:		$LLsys/tbl.ll

;(lookup_tbl my-tbl0 'a)
(lookup_tbl my-tbl1 'a)
(lookup_tbl my-tbl2 'a)
;(lookup_tbl my-tbl3 'a)
(lookup_tbl my-tbl3 'a 'b)

;-----------------------------------------------------------------------------
;	NAME:		soft-lkp_tbl  (SELECTOR)
;	TYPE:		Expr with 2 or 3 arguments
;	CALL:		(soft-lkp_tbl tbl key1 [key2])
;	FUNCTION:	return the value associated to key1 (and key2 if the
;			   structure is two-dimensional)
;	VALUE:		the above value
;	NOTE:		if there is no key1 (and key2) nil is returned
;	SOURCE:		$LLsys/tbl.ll

(soft-lkp_tbl my-tbl0 'a)
(soft-lkp_tbl my-tbl1 'a)
(soft-lkp_tbl my-tbl2 'a)
(soft-lkp_tbl my-tbl3 'a)
(soft-lkp_tbl my-tbl3 'a 'b)

;-----------------------------------------------------------------------------
;	NAME:		lkeys/lels_tbl  (PREDICATE)
;	TYPE:		Expr with 1 argument
;	CALL:		(lkeys/lels_tbl tbl)
;	FUNCTION:	return a list with all the keys/elements belonging to table tbl
;	VALUE:		the list above (ex. (k1 k2 (k3 k31 k32) k4) )
;	SOURCE:		$LLsys/tbl.ll

(lkeys_tbl my-tbl0)
(lels_tbl my-tbl0)
(lkeys_tbl my-tbl1)
(lels_tbl my-tbl1)
(lkeys_tbl my-tbl2)
(lels_tbl my-tbl2)
(lkeys_tbl my-tbl3)
(lels_tbl my-tbl3)

;-----------------------------------------------------------------------------
;	NAME:		insert_/rm_tbl  (MODIFIERS)
;	TYPE:		Expr with 3 or 4/2 or 3 arguments
;	CALL:		(insert_tbl tbl key1 [key2] val)
;			(rm_tbl tbl key1 [key2])
;	FUNCTION:	respectively insert a new element or delete an already
;			   existing one from a structure of type TBL
;			if the element to insert already exists, only its
;			   value will be changed
;	VALUE:		the symbol 'ok if element exists, () otherwise
;	SOURCE:		$LLsys/tbl.ll

(setf my-tbl4 (copy-list my-tbl3))
(insert_tbl my-tbl4 'bb 'bb1 222)
(insert_tbl my-tbl4 'aa 111)
(insert_tbl my-tbl4 'cc)
(insert_tbl my-tbl4 'dd 'dd1 335)
(insert_tbl my-tbl4 'ee 'ee1 '(this is my list))
(lkeys_tbl my-tbl4)
(lels_tbl my-tbl4)
my-tbl4
(rm_tbl my-tbl4 'dd 'dd1)
(lkeys_tbl my-tbl4)
(lels_tbl my-tbl4)

(assoc 'aa (contents my-tbl4))

;(rm_tbl my-tbl4 'g)

;-----------------------------------------------------------------------------
;	NAME:		merge_tbl  (MODIFIERS)
;	TYPE:		Expr with at least arguments
;	CALL:		(merge_tbl tbl1 tbl2l [tblN])
;	FUNCTION:	merge the keys and contents of two or more tables
;			if the same key is found, it will overwrite the
;			   previously existing value
;	VALUE:		the new tbl structure
;	SOURCE:		$LLsys/tbl.ll
(merge_tbl my-tbl0 my-tbl1)
(merge_tbl my-tbl1 my-tbl2)
(merge_tbl my-tbl2 my-tbl3)
(setf my-ttbl1 (merge_tbl my-tbl0 my-tbl1 my-tbl2 my-tbl3 my-tbl4))
(lkeys_tbl my-ttbl1)
(lels_tbl my-ttbl1)

;-----------------------------------------------------------------------------
;	NAME:		is_tbl  (PREDICATE)
;	TYPE:		Expr with 1 argument
;	CALL:		(is_tbl tbl)
;	FUNCTION:	test whether the argument is a structure of type TBL
;	VALUE:		t or nil according to the test
;	SOURCE:		$LLsys/tbl.ll

(is_tbl my-ttbl1)

;-----------------------------------------------------------------------------
;	NAME:		is-key_tbl  (PREDICATE)
;	TYPE:		Expr with 2/3 arguments
;	CALL:		(is-key_tbl tbl key1 [key2])
;	FUNCTION:	test whether the argument is a valid key for a
;			   structure of type TBL
;	VALUE:		key1 [or key2] or nil according to the test
;	SOURCE:		$LLsys/tbl.ll

(is-key_tbl my-ttbl1 'a)
(is-key_tbl my-ttbl1 'a 'b)
(is-key_tbl my-ttbl1 'k)

;-----------------------------------------------------------------------------
;	NAME:		is-empty_tbl  (PREDICATE)
;	TYPE:		Expr with 1 argument
;	CALL:		(is-empty_tbl tbl)
;	FUNCTION:	test whether a structure of type TBL is empty
;	VALUE:		t or nil according to the test
;	SOURCE:		$LLsys/tbl.ll

(is-empty_tbl my-tbl0)
(is-empty_tbl my-ttbl1)

;-----------------------------------------------------------------------------
;	NAME:		sizeof_tbl  (PREDICATE)
;	TYPE:		Expr with 1 argument
;	CALL:		(sizeof_tbl tbl)
;	FUNCTION:	returns the number of primary elements in the table
;	VALUE:		one element = 1
;	SOURCE:		$LLsys/tbl.ll
;NOTICE THAT SIZEOF WILL GIVE THE AMOUNT OF ELEMENTS IN THE MAIN TABLE, NOT IN THE SUBTABLES, WHILE
; SIZEOF-ALL WILL GIVE ALL THE ELEMENTS, INCLUDING THE SUBTABLES
(sizeof_tbl my-tbl4)
(sizeof_tbl my-ttbl1)
(sizeof-all_tbl my-ttbl1)

;-----------------------------------------------------------------------------
;	NAME:		print_/short-print_tbl  (INFO)
;	TYPE:		Expr with 1 argument
;	CALL:		(print_tbl tbl)
;			(short-print_tbl tbl)
;	FUNCTION:	nicely or shortely print a structure of type TBL
;	VALUE:		the string 'done
;	SOURCE:		$LLsys/tbl.ll
(print_tbl my-ttbl1)
(print_tbl my-tbl4)

(short-print_tbl my-ttbl1)
(short-print_tbl my-tbl4)
;-----------------------------------------------------------------------------


;____________________________________________________________________________
; chroma/pls/ctl.lisp
; PLS system: generator of control structures based on tables
;____________________________________________________________________________
;*****************************************************************************
;-------| PULS SYSTEM
;-------| This file is: $LLsys/ctl.ll
;-------| Version V1.1: Aug 19, 1991
;-------| By Marco Stroppa
;-------| Copyright 1991 IRCAM
;*****************************************************************************

; PACKAGE TO DEAL WITH CONTROL DATA
;                 ASSOCIATED TYPE NAME: CTL

(in-package :cr)

; DESCRIPTION OF THE DATA STRUCTURE:
; Control data are characterized by a KEY and a VALUE associated to the key.
; They are quite similar to mono-dimensional tables, but the retrieval
;    mechanism is more souple and never produces error or warning messages
;    when the specified key does not exist.

; CURRENT IMPLEMENTATION:
; Type CTL is structure of type TBL with built-in controls to avoid error
;    messages.


; AVAILABLE FUNCTIONS:

;	CONSTRUCTORS: make_ctl
;	SELECTORS:    get_ctl
;		      list_ctl
;	MODIFIERS:    set_ctl
;		      rm_ctl
;	PREDICATES:   is_ctl
;		      is-key_ctl
;		      is-empty_ctl
;	INFO:	      print_ctl
;		      short-print_ctl

; DESCRIPTION OF THE PACKAGE:
;-----------------------------------------------------------------------------
;	NAME:		make_ctl  (CONSTRUCTOR)
;	TYPE:		Expr with N arguments
;	CALL:		(make_ctl '(key1 val1) ... '(keyN valN))
;	FUNCTION:	define a of type CTL
;	VALUE:		the new structure
;	SOURCE:		$LLsys/ctl.ll

(setf my-ctl (make_ctl '(a 10) '(b 20) '(c (30 31 32)) '(d 40)))


;-----------------------------------------------------------------------------
;	NAME:		get_ctl  (SELECTOR)
;	TYPE:		Expr with 2 arguments
;	CALL:		(get_ctl ctl key)
;	FUNCTION:	return the value associated to key1; it key does not
;			   exist return ()
;	VALUE:		the above value
;	SOURCE:		$LLsys/ctl.ll

(get_ctl my-ctl 'v)
(get_ctl my-ctl 'c)

;-----------------------------------------------------------------------------
;	NAME:		list_ctl  (PREDICATE)
;	TYPE:		Expr with 1 argument
;	CALL:		(list_ctl ctl)
;	FUNCTION:	return a list with all the control keys belonging
;			   to ctl
;	VALUE:		the list above
;	SOURCE:		$LLsys/ctl.ll

(list_ctl my-ctl)

;-----------------------------------------------------------------------------
;	NAME:		sizeof_ctl  (PREDICATE)
;	TYPE:		Expr with 1 argument
;	CALL:		(sizeof_ctl ctl)
;	FUNCTION:	return a the amount of elements
;	VALUE:		the list above
;	SOURCE:		$LLsys/ctl.ll

(sizeof_ctl my-ctl)

;-----------------------------------------------------------------------------
;	NAME:		set_/rm_ctl  (MODIFIERS)
;	TYPE:		Expr with 3 / 2 arguments
;	CALL:		(set_ctl ctl key val)
;			(rm_ctl ctl key)
;	FUNCTION:	respectively create (if needed) and set a control key
;			   or delete an already existing key from a structure
;			   of type CTL
;	VALUE:		the symbol 'ok if the control existed, () otherwise
;	SOURCE:		$LLsys/ctl.ll

(set_ctl my-ctl 'v 1000)
(set_ctl my-ctl 'b 2222)
(prnt my-ctl)
(rm_ctl my-ctl 'h)
(rm_ctl my-ctl 'v)

;-----------------------------------------------------------------------------
;	NAME:		is_ctl  (PREDICATE)
;	TYPE:		Expr with 1 argument
;	CALL:		(is_ctl ctl)
;	FUNCTION:	test whether the argument is a structure of type CTL
;	VALUE:		t or nil according to the test
;	SOURCE:		$LLsys/ctl.ll

(is_ctl my-ctl)
(is_ctl (contents my-ctl))

;-----------------------------------------------------------------------------
;	NAME:		is-key_ctl  (PREDICATE)
;	TYPE:		Expr with 2 arguments
;	CALL:		(is-key_ctl ctl key)
;	FUNCTION:	test whether the argument is a valid key for a
;			   structure of type TBL
;	VALUE:		key1 [or key2] or nil according to the test
;	SOURCE:		$LLsys/ctl.ll

(is-key_ctl my-ctl 'v)
(is-key_ctl my-ctl 'c)

;-----------------------------------------------------------------------------
;	NAME:		is-empty_ctl  (PREDICATE)
;	TYPE:		Expr with 1 argument
;	CALL:		(is-empty_ctl ctl)
;	FUNCTION:	test whether a structure of type CTL is empty
;	VALUE:		t or nil according to the test
;	SOURCE:		$LLsys/ctl.ll

(is-empty_ctl my-ctl)
(setf my-ctl0 (make_ctl))
(is-empty_ctl my-ctl0)

;-----------------------------------------------------------------------------
;	NAME:		print_/short-print_ctl  (INFO)
;	TYPE:		Expr with 1 argument
;	CALL:		(print_ctl ctl)
;			(short-print_ctl ctl)
;	FUNCTION:	nicely or shortely print a structure of type CTL
;	VALUE:		the string 'done
;	SOURCE:		$LLsys/ctl.ll

(print_ctl my-ctl)
(short-print_ctl my-ctl)
;-----------------------------------------------------------------------------

;____________________________________________________________________________
; chroma/pls/raw.lisp
; PLS system: generator of raw data
;____________________________________________________________________________
;*****************************************************************************
;-------| PULS SYSTEM
;-------| This file is: $LLsys/raw.ll
;-------| Version V1.0: Mar 29, 2000
;-------| By Marco Stroppa
;-------| Copyright 2000 IRCAM
;*****************************************************************************

; PACKAGE TO DEAL WITH RAW DATA
;                 ASSOCIATED TYPE NAME: raw

(in-package :cr)

; DESCRIPTION OF THE DATA STRUCTURE:

; Raw data are context-dependent and are stored and retrieved as such without
;    any special operation

; CURRENT IMPLEMENTATION:
; Type RAW is a simple cons of type and data


; AVAILABLE FUNCTIONS:

;	CONSTRUCTORS: make_raw
;     [ SELECTORS   : contents ]
;	PREDICATES  : is_raw
;		      is-empty_raw
;	INFO        : print_raw
;		      short-print_raw



; DESCRIPTION OF THE PACKAGE:

;-----------------------------------------------------------------------------
;	NAME:		make_raw  (CONSTRUCTOR)
;	TYPE:		Expr with N arguments
;	CALL:		(make_raw data)
;	FUNCTION:	define a of type RAW, data can be anything
;	VALUE:		the new structure with the data contained in a list
;	SOURCE:		$LLsys/raw.ll

(setf my-raw (make_raw 0 1 2))
(contents my-raw)

;-----------------------------------------------------------------------------
;	NAME:		is_raw  (PREDICATE)
;	TYPE:		Expr with 1 argument
;	CALL:		(is_raw data)
;	FUNCTION:	test whether the argument is a structure of type RAW
;	VALUE:		t or nil according to the test
;	SOURCE:		$LLsys/raw.ll

(is_raw my-raw)
(is_raw my-ctl)

;-----------------------------------------------------------------------------
;	NAME:		is-empty_raw  (PREDICATE)
;	TYPE:		Expr with 1 argument
;	CALL:		(is-empty_raw data)
;	FUNCTION:	test whether a structure of type RAW is empty
;	VALUE:		t or nil according to the test
;	SOURCE:		$LLsys/raw.ll

(is-empty_raw my-raw)
(is-empty_raw (make_raw))

;-----------------------------------------------------------------------------
;	NAME:		sizeof_raw  (PREDICATE)
;	TYPE:		Expr with 1 argument
;	CALL:		(sizeof_raw data)
;	FUNCTION:	return the amount of items in data
;	VALUE:		a number
;	SOURCE:		$LLsys/raw.ll

(sizeof_raw my-raw)
(sizeof_raw (make_raw))
;-----------------------------------------------------------------------------
;	NAME:		print_/short-print_raw  (INFO)
;	TYPE:		Expr with 1 argument
;	CALL:		(print_raw raw)
;			(short-print_raw raw)
;	FUNCTION:	nicely or shortely print a structure of type RAW
;	VALUE:		the string 'done
;	SOURCE:		$LLsys/raw.ll

(print_raw my-raw)
(short-print_raw my-raw)
;-----------------------------------------------------------------------------

;____________________________________________________________________________
; chroma/pls/fun.lisp
; PLS system: generator of breakpoint functions
;____________________________________________________________________________
;*****************************************************************************
;-------| PULS SYSTEM
;-------| This file is: $LLpls/fun.ll
;-------| Version V1.0: Feb 6, 1990, rev. 2.0 (IRCAM, May-August 2000)
;-------| Rev. 3.0, IRCAM, Feb 2010
;-------| By Marco Stroppa
;-------| Copyright 1990 IRCAM
;*****************************************************************************
(in-package :cr)

; PACKAGE TO DEAL WITH BREAK-POINT FUNCTIONS
;                 ASSOCIATED TYPE NAME: FUN


; DESCRIPTION OF THE DATA STRUCTURE:
;    The structure consists of a breakpoint function defined between two
;       intervals.  Out-of-range values will bear the value of the closest
;	point defined in the function.
;    Note: values are contained into a list of couples, Yn Xn;
;    	   X values should always be different and in ascending order
;	   the type of interpolation used depends on the optional parameter
;	      given to y-val_fun.  Possible values are:
;	      	    () or nothing : first-order interpolation
;		    2  	  	  : second-order interpolation
;		    'EXP k	  : exponential interpolation (e**kt)

; CURRENT IMPLEMENTATION:
;    The structure of type FUN is nothing but a list with type prefix.


; AVAILABLE FUNCTIONS:

;	CONSTRUCTORS: make_fun
;	              sample_fun
;	SELECTORS:    x-beg_fun
;		      y-beg_fun
;		      x-end_fun
;		      y-end_fun
;		      y-val_fun
;		      y-min_fun
;		      y-max_fun
;		      y-list_fun
;		      x-list_fun
;		      reduce_fun
;		      reduce2_fun
;		      interpol_fun
;		      interpol-sample_fun
;	MODIFIERS:    resc_fun
;		      x-resc_fun
;		      y-resc_fun
;	PREDICATES:   is_fun
;	INFO:	      print_fun
;		      short-print_fun



; DESCRIPTION OF THE PACKAGE:

;-----------------------------------------------------------------------------
;	NAME:		make_fun  (CONSTRUCTOR)
;	TYPE:		Expr with 2*N arguments
;	CALL:		(make_fun '(y1 x1 ... yN xN))
;	FUNCTION:	define and initialize a structure of type FUN
;		        arguments must be in even number (couples)
;			see above for more info
;	VALUE:		the new structure
;	SOURCE:		$LLpls/fun.ll

(setf my-fun (make_fun '(0 0  100.0 10.0  20.0 20.0  70.0 40.0   0 100.0)))

;-----------------------------------------------------------------------------
;	NAME:		sample_fun  (CONSTRUCTOR)
;	TYPE:		Expr with 2 arguments
;	CALL:		(sample_fun number-of-points fun)
;	FUNCTION:	sample a structure of type FUN "number-of-points" times
;	VALUE:		the new structure (always of type FUN)
;	SOURCE:		$LLpls/fun.lisp

(sample_fun my-fun 101)
(sample_fun my-fun 101 1.0)

; EXP: > 0.0, ascending = exp, descending = log
; EXP: < 0.0, ascending = log, descending = exp


;(sample_fun 101 (make_fun '(0 0 200 1)) -0.1)

;-----------------------------------------------------------------------------
;	NAME:		x-beg_/x-end_/y-beg_/y-end_fun  (SELECTORS)
;	TYPE:		Expr with 1 argument
;	CALL:		(x-beg_fun fun)
;			(x-end_fun fun)
;			(y-beg_fun fun)
;			(y-end_fun fun)
;	FUNCTION:	return respectively the initial and the final values
;			   for the X and Y axes of a structure of type FUN
;	VALUE:		the values described above
;	SOURCE:		$LLpls/fun.ll

(x-beg_fun my-fun)
(x-end_fun my-fun)
(y-beg_fun my-fun)
(y-end_fun my-fun)

;-----------------------------------------------------------------------------
;	NAME:		y-min_/y-max_fun  (SELECTORS)
;	TYPE:		Expr with 1 argument
;	CALL:		(y-min_fun fun)
;			(y-max_fun fun)
;	FUNCTION:	return respectively the minimum and maximum value
;			   for the Y axis of a structure of type FUN
;	VALUE:		the values described above
;	SOURCE:		$LLpls/fun.ll
(y-min_fun my-fun)
(y-max_fun my-fun)

;-----------------------------------------------------------------------------
;	NAME:		y-val_fun  (SELECTOR)
;	TYPE:		Expr with 2 or more arguments
;	CALL:		(y-val_fun fun x-val [optional paramters])
;	FUNCTION:	return the Y value corresponding to the given X value
;			   x-val
;			optional parameters are needed so as to have an
;			   interpolation other than a first-order [CAN BE EXPANDED]
;			correct calls:
;                          (y-val_fun fun X) -> linear interpolation
;                          (y-val_fun fun X -1.0) -> exponential interpolation (log-shaped
;                               going up, exp-shaped coming down)
;                          (y-val_fun fun X 1.0) -> exponential interpolation (exp-shaped
;                               going up, logp-shaped coming down)
;                          (y-val_fun fun X '(exp 0.5)) -> same as above
;                          (y-val_fun fun X '(exp2 1.0)) -> symetric exponential/log curves
;                               (exp-shaped going up and down if exp > 0.0; log-shaped both ways
;                               if exp<0.0)
;                          (y-val_fun fun X '(sin)) -> same as (y-val_fun fun X 'sin)
;                             complete sinusoidal shape (from -pi/2 to pi/2)
;                          (y-val_fun fun X '(sin2)) -> same as (y-val_fun fun X 'sin2)
;                             use only 1/2 sin, from 0 to pi/2, slightly log up, exp down
;                          (y-val_fun fun X '(sin3)) -> same as (y-val_fun fun X 'sin3)
;                             use only 1/2 sin, from 0 to pi/2, symetric shapes
;	VALUE:		the value of which above
;	SOURCE:		$LLpls/fun.lisp

(y-val_fun (make_fun '(200 0  100.0 100)) 50)
(y-val_fun (make_fun '(200 0 100.0 100)) 50 -1.0)
(y-val_fun (make_fun '(0 0 100.0 100 0 200)) 150 '(exp 1.0))
(y-val_fun (make_fun '(0 0 100.0 100 0 200)) 150 '(exp1 -1.0))
(y-val_fun (make_fun '(0 0 100.0 100 0 200)) 150 '(exp2 1.0))
(y-val_fun (make_fun '(0 0 100.0 100 0 200)) 150 'sin1)
(y-val_fun (make_fun '(0 0 100.0 100 0 200)) 150 'sin2)
(y-val_fun (make_fun '(0 0 100.0 100 0 200)) 150 'sin3)


;-----------------------------------------------------------------------------
;	NAME:		y-list_/x-list_fun  (SELECTORS)
;	TYPE:		Expr with 1 argument
;	CALL:		(y-list_fun fun)
;			(x-list_fun fun)
;	FUNCTION:	return respectively a list containing all the
;			   Y and X values of a structure of type FUN
;	VALUE:		the values described above
;	SOURCE:		$LLpls/fun.ll
(y-list_fun my-fun)
(x-list_fun my-fun)

;-----------------------------------------------------------------------------
;	NAME:		resc_/x-resc_/y-resc_fun  (MODIFIER)
;	TYPE:		Expr with 3 or 5 arguments
;	CALL:		(resc_fun fun Y-min Y-max X-begin X-end)
;			(x-resc_fun fun X-begin X-end)
;			(y-resc_fun fun Y-min Y-max)
;	FUNCTION:	physically resc all the values (or respectively
;			   only the X or Y values) of the break points of a
;			   function of type FUN
;	VALUE:		the string 'ok
;	SOURCE:		$LLpls/fun.ll
(setf my-fun1 (copy-list my-fun))
(resc_fun my-fun1 -100.0 1000.0 10.0 20.0)
(x-resc_fun my-fun1 -1000.0 1000.0)
(y-resc_fun my-fun1 -500.0 500.0)
;-----------------------------------------------------------------------------
#|
(defun reduce_fun (fun factor)
;	NAME:		reduce_fun  (SELECTOR)
;	TYPE:		Expr with 2 arguments
;	CALL:		factor = 1 <-> maximum reduction
;			factor = 0 <-> reduction without loss
;			Reduces <points> by removing all points closer than [<factor> * the amplitude range of the function] to the corresponding interpolated values.
;                       <factor> = 1 means the maximum reduction (all intermediate points are removed)
;                       <factor> = 0 means reduction without loss (only exact matching points are removed)
;	FUNCTION:	returns a reduced fun 
|#
(setf my-fun2 (sample_fun my-fun 101))
(reduce_fun my-fun2 0.02)

;(setf testfun (make_fun '(0 0 10 1 15 2 20 3 0 4)))
;(setf testfun (make_fun '(1 0 1 4 1 10 1 20)))
;(setf testfun'(fun 0.5 0 0.5 1 0.5 2) )
;(reduce_fun testfun 0.0)

;-----------------------------------------------------------------------------
#|
(defun reduce2_fun (fun npoints &optional (precision 10))
"
;	NAME:		reduce2_fun  (SELECTOR)
;	TYPE:		Expr with 2 or 3 arguments
;	CALL:		npoints : MAXIMUM number of points of the reduced function
;                       precision : amount of recursive steps
;			
;	FUNCTION:	 reduces the number of break points of a
;
			   function of type FUN
By Serge Lemouton, July 2001
"
|#

;(setf testfun (make_fun '(0.5 0 10 1 15 2 20 3 0 4)))
;(reduce2_fun testfun 3)

(reduce2_fun my-fun2 10)
  

;-----------------------------------------------------------------------------
#|
(defun interpol_fun (fun1 fun2 factor)
  "
;	NAME:		interpol_fun  (SELECTOR)
;	TYPE:		Expr with 3 args
;	CALL:		factor = 1 -> fun2
;			factor = 0 -> fun1
;	FUNCTION:	returns a fun lying between fun1 and fun2
;	NB: since the x-values of each fun are merged, a function
;	      with a smaller size will keep its last value until the end
By Serge Lemouton, July 2001"
|#

(interpol_fun '(fun 0 0 1 1) '(fun 1 0 0 1 2 2 3 4.5) 0.5)

;-----------------------------------------------------------------------------
#|
(defun interpol-sample_fun (fun1 fun2 factor points &optional itp)
  "
;	NAME:		interpol-sample_fun  (SELECTOR)
;	TYPE:		Expr with 4-5 args
;	CALL:		factor = 1 -> fun2
;			factor = 0 -> fun1
;	FUNCTION:	returns an interpolated fun between fun1 and fun2
;	NB: both funs are sampled before being interpolated
; ms_1002"
|#

(interpol-sample_fun (make_fun '(0 0 1 1)) (make_fun '(1 0 0 1)) 0.4 100 :sin1)
(interpol-sample_fun (make_fun '(0 0 1 1)) (make_fun '(1 0 0 1)) 0.4 100)

;-----------------------------------------------------------------------------
;	NAME:		is_fun  (PREDICATE)
;	TYPE:		Expr with 1 argument
;	CALL:		(is_fun fun)
;	FUNCTION:	test whether the argument is a structure of type FUN
;	VALUE:		t or nil according to the test
;	SOURCE:		$LLpls/fun.ll

(is_fun my-fun)

;-----------------------------------------------------------------------------
;	NAME:		print_/short-print_fun  (INFO)
;	TYPE:		Expr with 1 argument
;	CALL:		(print_fun fun)
;			(short-print_fun fun)
;	FUNCTION:	nicely or shortely print a structure of type FUN
;	VALUE:		the string 'done
;	SOURCE:		$LLpls/fun.ll

(print_fun my-fun)
(short-print_fun my-fun)
;-----------------------------------------------------------------------------

;____________________________________________________________________________
; chroma/pls/ve.lisp
; PLS system: generator of virtual envelopes
;____________________________________________________________________________
;*****************************************************************************
;-------| PULS SYSTEM
;-------| This file is: $LLpls/ve.lisp
;-------| Version V1.0: Feb 6, 1990
;-------| Modified: Mar 2000 (added type DVE), revised 1003
;-------| Revised 1003, eliminated DVE, changed dyn allocation
;-------| By Marco Stroppa
;-------| Copyright 1990 IRCAM
;*****************************************************************************
(in-package :cr)

; PACKAGE TO DEAL WITH VIRTUAL ENVELOPES
;                 ASSOCIATED TYPE NAME: VE


; DESCRIPTION OF THE DATA STRUCTURE:
; A Virtual Envelope consists of a break-point function of type FUN
;   with an associated internal number that will be used when converting
;   virtual envelopes to structures recognized by real synthesizers.

;MODIF 2000
; In case the structure is not recognized (e.g. an object of type "fun" 
;    and not "ve" for csound), appropriate synthesizer-dependent actions
;    are taken (new 1003):
; csound:
;    transform "fun" into a gen-cs-table of type 7 or 5 and use OMChroma
;       dynamic table allocation

; NB: the old variables GEN-MIN, GEN-MAX, GEN-CURR, GEN-FILE and the type DVE
;        have been deactivated


; CURRENT IMPLEMENTATION:
; cons of FUN and number


; AVAILABLE FUNCTIONS:

;	CONSTRUCTORS: make_ve
;	SELECTORS:    fun_ve
;		      num_ve
;		      s_ve
;	PREDICATES:   is_ve
;                     is-empty_ve
;	INFO:	      print_ve
;		      short-print_ve

; DESCRIPTION OF THE PACKAGE:
;-----------------------------------------------------------------------------
#|
(defun make_ve (fun &optional num)
  "
;	NAME:		make_ve  (CONSTRUCTOR)
;	TYPE:		Expr with 1 or 2 arguments
;	CALL:		(make_ve fun [num])
;	FUNCTION:	define and initialize a structure of type VE
;			<fun> must be of type FUN
;	VALUE:		the new structure
"
|#
(setf my-ve (make_ve (make_fun '(0 0  10 1  0.5  30)) 1000))
(setf my-ve0 (make_ve (make_fun '(0 0  10 1  0.5  30))))

;-----------------------------------------------------------------------------
#|
(defun fun_ve (ve)
"
;	NAME:		fun_ / num_ / name_ve  (SELECTORS)
;	TYPE:		Expr with 1 argument
;	CALL:		(fun ... _ve ve)
;	FUNCTION:	return the function, the number and a string derived
;			   from the number of ve
;	VALUE:		the new data"
|#
(fun_ve my-ve)

;-----------------------------------------------------------------------------
#|
(defun num_ve (ve)
"
;	NAME:		fun_ / num_ / name_ve  (SELECTORS)
;	TYPE:		Expr with 1 argument
;	CALL:		(fun ... _ve ve)
;	FUNCTION:	return the function, the number and a string derived
;			   from the number of ve
;	VALUE:		the new data"
|#
(num_ve my-ve)
(num_ve my-ve0)

;-----------------------------------------------------------------------------
#|
(defun name_ve (ve)
"
;	NAME:		fun_ / num_ / name_ve  (SELECTORS)
;	TYPE:		Expr with 1 argument
;	CALL:		(fun ... _ve ve)
;	FUNCTION:	return the function, the number and a string derived
;			   from the number of ve
;	VALUE:		the new data"
|#
(name_ve my-ve)
(name_ve my-ve0)

;-----------------------------------------------------------------------------
;	NAME:		is_ve  (PREDICATE)
;	TYPE:		Expr with 1 argument
;	CALL:		(is_ve ve)
;	VECTION:	test whether the argument is a structure of type VE
;	VALUE:		t or nil according to the test
;	SOURCE:		$LLpls/ve.ll

(is_ve my-ve)
(is_ve (fun_ve my-ve))

;-----------------------------------------------------------------------------
;	NAME:		print_/short-print_ve  (INFO)
;	TYPE:		Expr with 1 argument
;	CALL:		(print_ve ve)
;			(short-print_ve ve)
;	VECTION:	nicely or shortely print a structure of type VE
;	VALUE:		the string 'done
;	SOURCE:		$LLpls/ve.ll

(print_ve my-ve)
(short-print_ve my-ve)
;-----------------------------------------------------------------------------


;____________________________________________________________________________
; chroma/pls/wt.lisp
; PLS system: generator of sampler controls (Wave Table)
;____________________________________________________________________________
;*****************************************************************************
;-------| PULS SYSTEM
;-------| This file is: $LLpls/wt.ll
;-------| Version V1.0: Aug 19, 1991 / Rev. Apr. 2002 (omChroma compatible)
;-------| By Marco Stroppa
;-------| Copyright 1990 IRCAM
;*****************************************************************************

; DESCRIPTION OF AN INPUT SOUND FILE TO BE USED BY A LISP-BASED SYSTEM

;                 ASSOCIATED TYPE NAME: WT

; DESCRIPTION OF THE DATA STRUCTURE:
;    Simple structure containing a set of fields with fixed name

;    A WT always points to a physical soundfile with a name and an optional
;	directory. A soundfile has a physical start time (always 0.0 sec)
;	and end (its duration in sec. as read by sndinfo).

;    However, such physically-driven description might reveal inadequate
;	from a perceptual point of view (silence at the beginning and/or
;	end, for instance).
;    A more perceptually-oriented segmentation of the sound file can be
;	done by using the fields beg-off (distance of the perceptual beginning
;	of the file from the physical beginning) and end-off (distance of
;	the perceptual end of the file from the physical beginning).
;    Commands referring to time, duration and window will always refer to
;	these perceptual fields, unless they are preceded by "phys-".

;   Finally a window of a given size can be defined inside the soundfile
;	and advanced in such a way that one never gets inconsistent results.
;   Windows are similar to beg-off and end-off, but can be moved around the
;	the soundfile automatically (advance-win_wt) and are more tolerant
;	of time errors (beyond beginning and end of file). Windows will
;	adjust a wrong value and send a simple warning to the user rather
;	than giving off error messages.
;   Each soundfile has also a minimum window (set to 0.05 by default) that
;	limits the physical size of the window.
;   There is also a virtual field (win-to) which gives the position of the
;       pointer at the end of the window. This field is computed on the fly
;       and is not stored in the structure (only win-from and win-size are).

; The frequency field may be either a frequency (when specified using
;    set-freq_wt) or a sample increment (set-si_wt or set-freq_wt with
;    a third argument set to 'si).
; In case of a list of values, they must all be of the same kind.
; To test whether the freq field is a frequency or a sample increment, use
;    "is-si_wt", which returns true if it is a sample increment.
; By default a structure is initalized to "si" with a value of "1.0".
; Remark: the selectors of the freq/si field always have the name "freq", even
;    though they may refer to a sample increment.

; ATTENTION: detailed description of each real field is still missing.

; ATTACK (att) [sec]
; One can specify an "attack" portion for a WT-object, which could be subject
;    to a special behavior (for example, the freeze algorithm might start to
;    be active after the value of att), although this must be explicitely
;    specified by the user.
; When retrieving the value of the attack (att_wt ...) a jitter can be applied,
;    i.e. the value might oscillate around the value of (var-att_wt ...) [sec],
;    clipped to 0 if it goes below (beg-off_wt ...).

; FADE IN/OUT ENVELOPE
; List of 2 values corresponding to the xin/xout fields (p23, p24) of the 
;    WT instrument (superposing a janus envelope in and out to the whole
;    sound)
; Ex. (set-fade_wt xxx '(0.1 0.2)) 

; FLT (FILTER) [Hz]
; In the current implementation, it determines the behavior of a first-order
;   filter (csound's "tone"). Its value sets the frequency at which the
;   signal's spectral amplitude will have decreased by 6dB from 0Hz.
; Hence, the lower the frequency, the stronger the effect of the filter.

; All the temporal commands can accept an optional "si" field, corresponding
;    to the "sample increment" at which the soundfile will be written.
;    All the values inside the object will be written in absolute time,
;    ie as if si = 1, but can be set and/or retrieved with any si.

;  Besides real fields (ie fields set automatically and always present),
;	fields can be dynamically added and removed from a WT object at will.

; CURRENT IMPLEMENTATION:
;    Structure of type TBL.
;    Real fields are:
;	file 	/ name, dir
;	sf	/ sr, nchan
;	freq	/ val, mode
;	dur	/ physical, att, var-att
;	off	/ beg, end
;	win	/ from, size, min-size
;	flt
;	fade-env

(in-package :cr)

;(set-colon 'PLS)

; AVAILABLE FUNCTIONS:
;	CONSTRUCTORS:		make_wt
;				add-field_wt
;				rm-field_wt
;	SELECTORS/[MODIFIERS]:	[set-]file-name_wt
;				[set-]dir_wt
;				abs-file_wt

;				[set-]field_wt

;				[set-]freq_wt
;				set-si_wt
;				nth-freq_wt
;				first/last/ran-freq_wt
;				close-freq_wt
;				si_wt

;				[set-]flt_wt
;				[set-]fade_wt

;				[set-]phys-dur_wt
;				dur_wt
;				[set-]beg-off_wt
;				[set-]end-off_wt
;				[set-]phys-dur-att_wt
;				[set-]dur-att_wt
;				[set-]var-att_wt
;				att_wt

;				n-ch_wt
;				sr_wt
;				pk-mode_wt
;				n-smpls_wt

;				[set-]win-phys-from_wt
;				[set-]win-from_wt
;				[set-]win-to_wt
;				[set-]win-size_wt
;				[set-]win-min-size_wt
;				[set-]win_wt
;				win-phys-to_wt
;				win-to_wt
;	MODIFIERS:		set-phys-win_wt
;				set-win_wt
;				phys-advance-win_wt
;				advance-win_wt
;	PREDICATES:		is_wt
;				is-field_wt
;	INFO:			print_wt
;				short-print_wt

;	MIXED FUNCTIONS:	closest_wt (SELECTOR)

; DESCRIPTION OF THE PACKAGE:

;-----------------------------------------------------------------------------
;	NAME:		make_wt / reinit_wt / add-field_wt  (CONSTRUCTOR)
;	TYPE:		Expr with 1-2 / 2-3 arguments
;	CALL:		(make_wt file-name [file-dir])
;			(reinit_wt wt file-name [file-dir])
;			(add-field_wt wt field-name [value])
;			(rm-field_wt wt field-name)
;	FUNCTION:	make_wt: define and initialize all the default fields
;			reinit_wt: reinitialize only those fields depending on
;			   the external soundfile (name, dir, sndinfo)
;			add-field_wt: add a new dynamic field to structure WT
;			rm-field_wt: remove current dynamic field
;	VALUE:		the new data structure
;	SOURCE:		$LLpls/wt.ll

; MAKE_WT
;(setf my-wt (make_wt (merge-pathnames (get-cr-path :wt) "wt-tpt.aif")))
(setf my-wt (make_wt "wt-tpt.aif" (get-cr-path :wt)))
(setf my-wt1 (make_wt "wt-tpt.aif" (get-cr-path :wt)))
(reinit_wt my-wt1 "my-sopr1.aif" (get-cr-path :wt))
my-wt1
(reinit_wt my-wt1 "wt-sopr1.aif" (get-cr-path :wt))
my-wt1
;             (get-sndinfo (merge-pathnames (get-cr-path :wt) "wt-tpt.aif"))
;(disable-print)
(add-field_wt my-wt1 'TESTFIELD 111.0)
(rm-field_wt my-wt1 'TESTFIELD)

;-----------------------------------------------------------------------------
;	NAME:		SELECTORS / MODIFIERS
;	TYPE:		Expr with 1/2 arguments
;	CALL:		(file-name_wt wt) / (set-file-name_wt wt val) / etc.
;	FUNCTION:	select / modify the value of a given field
;	VALUE:		the above value / the symbol 'ok
;	SOURCE:		$LLpls/wt.ll

(file-name_wt my-wt)
(set-file-name_wt my-wt "test")
(set-file-name_wt my-wt "wt-tpt.aif")

(dir_wt my-wt)
(set-dir_wt my-wt "test-dir")
(set-dir_wt my-wt (get-cr-path :wt))

;-----------------------------------------------------------------------------
; FREQ: RETURN/SET THE FREQUENCY OR SI FIELD (SINGLE VALUE OR A LIST)
; SET-FREQ_WT: automatically set a frequency or si, unless the 3rd optional arg is 'si
; SET-SI_WT: set a sample increment, always
"
Set the FREQ and SI field of a WT object.
New version (omChroma): automatic test for FREQ or SI.

wt: WT object
val: frequency OR sampling increment
mode: if present, if = SI force mode to SI, if = FREQ, force mode to FREQ, otherwise, ERROR.
Use the global variable 'MAXSI.

Corrected insidious BUG with test of third argument!
"   

(freq_wt my-wt)
(set-freq_wt my-wt 207.9)
(set-freq_wt my-wt 1.5 'si)
(set-freq_wt my-wt 30.0 'si)
(set-si_wt my-wt 15.0)
(set-freq_wt my-wt 207.9)
;(set-freq_wt my-wt 15.0 'ss)


;-----------------------------------------------------------------------------
; FLT: RETURN/SET THE FREQUENCY OF THE FILTER ASSOCIATED TO THE OBJECT
;	BY DEFAULT IT IS SET TO THE NYQUIST FREQUENCY
; FADE-ENV: RETURN/SET THE DURATION OF FADE-IN AND FADE-OUT ENVELOPES THAT
;	ARE SUPERPOSED TO THE OTHER ENVELOPES (BY DEFAULT = 0.0)
(flt_wt my-wt)
(set-flt_wt my-wt 1000.0)
(flt_wt my-wt)

(fade_wt my-wt)

(set-fade_wt my-wt '(0.005 0.01))
(fade_wt my-wt)
(set-fade_wt my-wt 0.11)
(fade_wt my-wt)
(set-fade_wt my-wt '(0.0055))
;(set-fade_wt my-wt 's)
(fade_wt my-wt)


;-----------------------------------------------------------------------------
; BEG-OFF: RETURN/SET THE OFFSET FROM THE BEGINNING OF THE SOUND FILE THAT
;    WILL BE CONSIDERED AS THE (REAL) BEGINNING
; WHEN SI IS PASSED AS AN ARG, TIME VALUE IS SCALED ACCORDINGLY
; THIS MEANS THAT WITH A SI OF 2.0, THE ABSOLUTE TIME OF 1.0 WILL BECOME 2.0
;   (IT TAKES 2.0 absolute secs TO GENERATE THE DURATION OF 1"
; ERROR: BEG-OFF CANNOT BE > END-OFF
; IF DUR-ATT < BEG-OFF, SET IT TO BEG-OFF

(beg-off_wt my-wt)
(set-beg-off_wt my-wt 1.0 0.5)
(set-beg-off_wt my-wt 1.0)
;(set-beg-off_wt my-wt 10.0)
;(set-phys-dur-att_wt my-wt 0.5)
;-----------------------------------------------------------------------------
; END-OFF: RETURN/SET THE DISTANCE BETWEEN THE PHYSICAL BEGINNING OF THE
;    SOUND FILE THAT WILL BE CONSIDERED AS THE (REAL) END
; ERROR: END-OFF CANNOT BE < BEG-OFF
; IF DUR-ATT > END-OFF, SET IT TO END-OFF
(end-off_wt my-wt)
(end-off_wt my-wt 2.0) ; TWICE AS SHORT

(set-end-off_wt my-wt 7.322)
;(set-end-off_wt my-wt 0.5)
;(set-beg-off_wt my-wt 10.0)


;-----------------------------------------------------------------------------
; [PHYS-]DUR-ATT: RETURN/SET THE DURATION OF THE ATTACK PORTION OF
;    THE SOUND FILE EITHER FROM THE BEGINNING OF THE SOUND FILE (PHYS) OR
;    FROM BEG-OFF
; REMARK: DUR-ATT CANNOT BE < BEG-OFF
(set-dur-att_wt my-wt 0.2)
(dur-att_wt my-wt)
(phys-dur-att_wt my-wt)
;(disable-print)
(enable-print)
;(set-phys-dur-att_wt my-wt 0.3)
(dur-att_wt my-wt)
(set-dur-att_wt my-wt 0.3)

;-----------------------------------------------------------------------------
; VAR-ATT: RETURN/SET THE RANDOM VARIATION THAT WILL BE APPLIED WHEN
;    COMPUTING THE ATTACK PORTION OF THE SOUND FILE
(set-var-att_wt my-wt 0.1)
(var-att_wt my-wt)

;-----------------------------------------------------------------------------
;	NAME:		win-... / set-win-... (SELECTORS / MODIFIERS)
;	TYPE:		Expr with 1 / 2-3 arguments
;	CALL:		see below
;	FUNCTION:	WINDOW PACKAGE
;	VALUE:		see below
;	SOURCE:		$LLpls/wt.ll

; WIN-[PHYS-]FROM: RETURN/SET THE STARTING POINT OF A WINDOW
; THE MODIFIER RETURNS 'ok IF THE "FROM" ARGUMENT HAS NOT BEEN CHANGED TO
;    ADAPT IT TO THE CONSTRAINTS IMPOSED BY THE SOUNDFILE, OTHERWISE IT
;    RETURNS THE NEW CHANGED VALUE
; ALL VALUES ARE CLIPPED TO BEG-OFF AND END-OFF
; PRIORITY OF THE CHANGES IS FOR WIN-SIZE
; CHANGES IF FROM IS:
;	BEYOND END-OFF:		NEW-FROM = END-OFF - MIN-WIN
;				SIZE = MIN-SIZE
;	BEYOND BEG-OFF:		NEW-FROM = BEG-OFF / SIZE UNCHANGED
;	FROM+SIZE = BEYOND END-OFF:	ADAPT SIZE / FROM UNCHANGED
; IMPORTANT: ANY CHANGE TO FROM OR SIZE ALSO CHANGES TO (AND VICEVERSA)
(set-win-from_wt my-wt 2.0)
(win-from_wt my-wt)
(win-phys-from_wt my-wt)
(set-win-phys-from_wt my-wt 0.5) ; set it automatically to beg-off
(win-from_wt my-wt)
(set-win-from_wt my-wt 0.1)

(win-size_wt my-wt)
(win-to_wt my-wt)

;-----------------------------------------------------------------------------
; WIN-[MIN-]SIZE: RETURN/SET THE SIZE AND THE MINIMUM SIZE OF A WINDOW
; THE MODIFIER RETURNS 'ok IF THE "SIZE" ARGUMENT HAS NOT BEEN CHANGED TO
;    ADAPT IT TO THE CONSTRAINTS IMPOSED BY THE SOUNDFILE, OTHERWISE IT
;    RETURNS THE NEW CHANGED VALUE
; PRIORITY OF THE CHANGES IS FOR WIN-FROM
; CHANGES IF SIZE IS:
;	< MIN-SIZE:		NEW SIZE = MIN-SIZE
;	> DUR:			NEW SIZE = DUR / FROM = BEG-OFF
;	> END-OFF:		NEW FROM = END-OFF - SIZE
; set-win-size2_wt:
; PRIORITY OF THE CHANGES IS FOR WIN-SIZE (DO NOT TO CHANGE WIN-FROM)
; CHANGES IF SIZE IS:
;	< MIN-SIZE:		NEW SIZE = MIN-SIZE
;	> DUR:			NEW SIZE = BEG-OFF - WIN-FROM
;	> END-OFF:		NEW SIZE = END-OFF - WIN-FROM
(enable-print)
(win-size_wt my-wt)
(win-min-size_wt my-wt)
(set-win-size_wt my-wt 0.001)
(set-win-size_wt my-wt 2.0)

(set-win-size_wt my-wt 12.0)
(win-from_wt my-wt)
(dur_wt my-wt)
(beg-off_wt my-wt)
(end-off_wt my-wt)
(set-win-from_wt my-wt 1.0)
(set-win-size_wt my-wt 6.0)
(win-from_wt my-wt)
(dur_wt my-wt)
(beyond-eof_wt my-wt 7.0)
(beyond-eof_wt my-wt 8.0)

(set-win_wt my-wt 1.0 3.0)
(win-from_wt my-wt)
(win-size_wt my-wt)
;(set-win_wt my-wt 4.0 3.0)
(set-win_wt my-wt 1.0 1.0001)
(win-size_wt my-wt)

;(set-win_wt my-wt 10.0 11.0)
;(set-win_wt my-wt -1.0 1.0)
;(win-from_wt my-wt)

(phys-win_wt my-wt)
(win_wt my-wt)

;-----------------------------------------------------------------------------
; WIN-[PHYS-]TO: RETURN THE END POINT OF A WINDOW
(set-win-to_wt my-wt 3.0)
(win-to_wt my-wt)

(set-win-phys-to_wt my-wt 3.0)
(win-phys-to_wt my-wt)

(set-win-phys-to_wt my-wt 0.0)
(win-phys-to_wt my-wt)

(set-win-phys-to_wt my-wt 10.0)
(win-phys-to_wt my-wt)

;-----------------------------------------------------------------------------
; [PHYS-]ADVANCE-WIN: ADVANCE THE CURRENT WINDOW RESETTING THE FROM FIELD
;    RETURN THE NEW WINDOW OBJECT (PHYSICALLY DEFINED OR NOT)
(set-win_wt my-wt 1.0 3.0)
(win-from_wt my-wt)
(win-to_wt my-wt)
(win-size_wt my-wt)

(advance-win_wt my-wt 0.5)
(win-from_wt my-wt)
(win-to_wt my-wt)

;-----------------------------------------------------------------------------
;	NAME:		phys-dur_wt / set-phys-dur_wt  (SELECTOR / MODIFIER)
;	TYPE:		Expr with 1 / 2-3 arguments
;	CALL:		(phys-dur_wt wt) / (set-phys-dur_wt wt file [dir])
;	FUNCTION:	return / set the physical duration of the sfile
;			   with optional directory (note that this is a
;			   value that belongs to the soundfile)
;	VALUE:		the above value / 'ok
;	SOURCE:		$LLpls/wt.ll

(phys-dur_wt my-wt)
(set-phys-dur_wt my-wt (merge-pathnames *cr-wt-dir* "wt-sopr1.aif") )
(phys-dur_wt my-wt)
;(set-end-off_wt my-wt 4.0)
(end-off_wt my-wt)
(set-phys-dur_wt my-wt (merge-pathnames *cr-wt-dir* "wt-tpt.aif") )
(phys-dur_wt my-wt)

;-----------------------------------------------------------------------------
;	NAME:		abs-file_wt / etc.  (SELECTORS)
;	TYPE:		Expr with 1 or more arguments
;	CALL:		(abs-file_wt wt) / etc.
;	FUNCTION:	return the values computed depending on the state of
;			   the internal fields
;	VALUE:		the above values
;	SOURCE:		$LLpls/wt.ll

; ABS-FILE: RETURN THE ABSOLUTE NAME OF THE FILE
(abs-file_wt my-wt)

; NTH-FREQ: RETURN THE NTH FREQUENCY OF A LIST
(nth-freq_wt my-wt)
(nth-freq_wt my-wt 3)
(set-freq_wt my-wt '(200 300 400 500))
(nth-freq_wt my-wt 4)

;-----------------------------------------------------------------------------
; FIRST/RAN/LAST-FREQ: RETURN THE FIRST, ONE RANDOMLY CHOSEN OR LAST FREQUENCY OF A LIST
;                  IF THERE IS NO LIST, RETURN THE ONLY FREQ FOUND
(first-freq_wt my-wt)
(ran-freq_wt my-wt)
(last-freq_wt my-wt)

;-----------------------------------------------------------------------------
; CLOSE-FREQ: RETURN THE FREQUENCY OF A LIST THAT IS THE CLOSEST ONE TO REF-FQ
;             IF THERE IS NO FREQUENCY LIST, RETURN THE ONLY FREQ AVAILABLE
(close-freq_wt my-wt 11.0)
(close-freq_wt my-wt 211.0)
(close-freq_wt my-wt 251.0)
(close-freq_wt my-wt 900.0)
(close-freq_wt my-wt 470.0)
;-----------------------------------------------------------------------------
; FIELD: GET THE VALUE OF A DYNAMICAL FIELD
(field_wt my-wt '(off beg))
(field_wt my-wt '(beg off)) ; unexistent, wrong order

(set-field_wt my-wt '(off beg) 2.2)
(set-field_wt my-wt '(beg off) 2.2)
(set-field_wt my-wt 'fade-env 2.4)


;-----------------------------------------------------------------------------
; DUR: RETURN THE PERCEPTUAL DURATION (TIME GAP BETWEEN BEG-OFF AND END-OFF)
(dur_wt my-wt)
(dur_wt my-wt 2.0)

;-----------------------------------------------------------------------------
; [PHYS-]ATT: RETURN THE REAL ATTACK, IE THE ATTACK INCLUDING THE
;	RANDOM VARIATION
(att_wt my-wt)
(phys-att_wt my-wt)

;-----------------------------------------------------------------------------
; SNDINFO: NUMBER OF CHANNELS, SAMPLING RATE, PACKING MODE (2 = SHORT,
;    4 = LONG), NUMBER OF SAMPLES
(n-ch_wt my-wt)
(sr_wt my-wt)
(pk-mode_wt my-wt)
(n-smpls_wt my-wt)

;-----------------------------------------------------------------------------
; SI: RETURN THE SAMPLE INCREMENT NEEDED TO OBTAIN A FREQUENCY OF OUT-FQ
;   IF THE FREQUENCY THAT IS STORED IN THE SOUND FILE IS REF-FQ
(si_wt 100.0 200.0)
(si_wt 300.0 200.0)

;-----------------------------------------------------------------------------
;	NAME:		is_wt / is-si_wt / is-field_wt (PREDICATES)
;	TYPE:		Expr with 1 / 1/ 2 arguments
;	CALL:		(is_wt wt) / (is-si_wt wt) / (is-field_wt wt field)
;	FUNCTION:	test whether the argument is a structure of type WT
;			test whether the freq field is a frequency in Hz or a
;			   sample increment (1.0 = same speed as input)
;			test whether dynamic field "field" exists in WT
;	VALUE:		t or nil according to the test
;	SOURCE:		$LLpls/tbl.ll
(is_wt my-wt)
(is_wt my-tbl)

(is-si_wt my-wt)
(is-field_wt my-wt '(off beg))
(is-field_wt my-wt 'fade-env)

;-----------------------------------------------------------------------------
;	NAME:		print_/short-print_wt  (INFO)
;	TYPE:		Expr with 1 argument
;	CALL:		(print_wt wt)
;			(short-print_wt wt)
;	FUNCTION:	nicely or shortely print a structure of type TBL
;	VALUE:		the string 'done
;	SOURCE:		$LLpls/wt.ll

(print_wt my-wt)
(short-print_wt my-wt)

;-----------------------------------------------------------------------------
;	NAME:		closest_wt-ran (SELECTOR FOR SAMPLING MODE)
;	TYPE:		Expr with 2 or 3 arguments
;	CALL:		(closest_wt wt-list freq [transposition limits])
;	FUNCTION:	return the WT object whose frequency is closest to <freq> 
;			<wt-list> must be a list of names of WT objects
;                       <limits>: cons of 2 values giving the minimum and maximum transposition
;                                     factors [ex. (0.5 . 2.0) = octave below and above]. When the
;                                     SI is beyond this, a warning is issued.
;	VALUE:		the name of a WT object
;	SOURCE:		$LLpls/wt.lisp


#|
(defun closest_wt (wt-list fq &optional limits)
  "Return the object with the closest frequency to fq.
 Limits is a cons of min and max freq allowed.
 If fq beyond limits, send a warning and return closest item."
  ; return: (list <wt-name> <fq> (freq_wt x))
  (let* ((diff-list (mapcar #'(lambda (x) (abs (- (freq_wt0 x) fq))) wt-list))
         (s-list (sort (mapcar #'cons diff-list wt-list) #'< :key #'car))
         (freq-of-wt (freq_wt (eval(cdar s-list)))))
    (when limits
      (let ((ratio (/ fq freq-of-wt)))
        (when (or (> ratio (cdr limits))(< ratio (car limits)))
          (format t "WARNING ! TRANSPOSITION OF ~a EXCEEDS LIMITS.~%transposition ~a limits ~a~%"
                  (cdar s-list) ratio limits))))
    (list (cdar s-list) fq freq-of-wt))
  )



(closest_wt Sopr 500  '(.5 . 2))
(closest_wt Sopr 10000)
(closest_wt2 Sopr '(200 201))



(defun closest_wt2 (wt-list fq-list &key (limits ()) (beg-ratio 0.)(beg-ran 0))
  "Return the object with the closest frequency to fq.
 Limits is a cons of min and max freq allowed.
 If fq beyond limits, send a warning and return closest item."
  ; return: expression for WT
  (let ((cwt-list nil)
        (fqwt-list nil))
    (loop for fq in fq-list
          do
          (let* ((diff-list (mapcar #'(lambda (x) (abs (- (freq_wt0 x) fq))) wt-list))
                 (s-list (sort (mapcar #'cons diff-list wt-list) #'< :key #'car))
                 (freq-of-wt (freq_wt (eval(cdar s-list)))))      
            (when limits
              (let ((ratio (/ fq freq-of-wt)))
                (when (or (> ratio (cdr limits))(< ratio (car limits)))
                  (format t "WARNING ! TRANSPOSITION OF ~a EXCEEDS LIMITS.~%transposition ~a limits ~a~%"
                          (cdar s-list) ratio limits))))
            (push (cdar s-list) cwt-list)
            (push (list fq freq-of-wt) fqwt-list)))
    (read-from-string
     (with-output-to-string (s)
       (loop for cwt in cwt-list
             initially (format s "((cwt '(~%" )
             do (let*((end (dur_wt (eval cwt)))
                      (begin (abs (+ (ran (* beg-ran end))(* beg-ratio end)))))
                  (format s "'(list '~a ~a ~a)~%" cwt begin end))
             finally (format s "))~%" ))
       (loop for fqwt in fqwt-list
             initially (format s "(fqwt '(~%" )
             do (format s "'(list ~a ~a)~%" (car fqwt)(cadr fqwt))
             finally (format s ")))~%" ))
       ))))

(closest_wt-ran S-m '(600) :min-ratio .9 :max-ratio 1.1)
(closest_wt-ran S-m '(20) :min-ratio 1. :max-ratio 1.)
(closest_wt-ran S-m '(200 201 3000) :min-ratio .5 :max-ratio 1.5 
                :limits '(.5 . 2))

(closest_wt-ran S-m '(400) :min-ratio .5 :max-ratio 2. 
                :limits '(.5 . 2))


(defun freq_wt0 (x)
  "if multi-frequences object returns the first frequency"
  (let ((result (freq_wt (eval x))))
    (if (listp result)(choose-in-list result) result)))

(defun closest_wt-ran (wt-list fq-list &key (limits '(.25 . 4)) (min-ratio 1.)(max-ratio 1)(beg-ratio 0.)(beg-ran 0))
  "Choose a wt object between fq * min-ratio and fq * max-ratio.
 Limits is a cons of min and max freq allowed.
 If fq beyond limits, send a warning and return closest item."
  ; return: expression for WT
  (let ((cwt-list nil)
        (fqwt-list nil) ratio-list s-list diff-list freq-of-wt)
    (loop for fq in fq-list
          do
          (progn (setf ratio-list (mapcar #'(lambda (x)  (/ (ran-freq_wt (eval x)) fq)) wt-list))
                 (setf s-list (sort (mapcar #'cons ratio-list wt-list) #'< :key #'car))
                 (setf s-list (member-if #'(lambda (x)(>= x min-ratio)) s-list :key #'car))
                 (setf s-list (member-if #'(lambda (x)(<= x max-ratio)) (nreverse s-list) :key #'car))
                 (if  (null s-list)
                   (setf diff-list (mapcar #'(lambda (x) (abs (- (ran-freq_wt (eval x)) fq))) wt-list)
                         s-list (sort (mapcar #'cons diff-list wt-list) #'< :key #'car)
                         freq-of-wt (freq_wt (eval(cdar s-list)))
                         )  
                   (setf s-list (list (choose-in-list s-list))
                         freq-of-wt (freq_wt (eval(cdar s-list)))
                        ))
                 (when limits
                   (let ((ratio (/ fq freq-of-wt)))
                     (when (or (> ratio (cdr limits))(< ratio (car limits)))
                       (format t "WARNING ! TRANSPOSITION OF ~a EXCEEDS LIMITS.~%      Transposition : ~a / limits : ~a~%"
                               (cdar s-list) ratio limits))))
                 (push (cdar s-list) cwt-list)
                 (push (list fq freq-of-wt) fqwt-list)))
    (setf cwt-list (nreverse cwt-list))
    (setf fqwt-list(nreverse fqwt-list))
    (read-from-string
     (with-output-to-string (s)
       (loop for cwt in cwt-list
             initially (format s "((cwt '(~%" )
             do (let*((end (dur_wt (eval cwt)))
                      (begin (abs (+ (ran (* beg-ran end))(* beg-ratio end)))))
                  (format s "'(list '~a ~a ~a)~%" cwt begin end))
             finally (format s "))~%" ))
       (loop for fqwt in fqwt-list
             initially (format s "(fqwt '(~%" )
             do (format s "'(list ~a ~a)~%" (car fqwt)(cadr fqwt))
             finally (format s ")))~%" ))
       ))))

(defun closest_wt-ran (wt-list fq-list &key (limits '(.25 . 4)) (min-ratio 1.)(max-ratio 1)(beg-ratio 0.)(beg-ran 0))
  "Choose a wt object between fq * min-ratio and fq * max-ratio.
 Limits is a cons of min and max freq allowed.
 If fq beyond limits, send a warning and return closest item.
  ; return: expression for the filed CWT of a WT instrument"
; change for omchroma / Marco, 020416
  (let ((cwt-list nil)
        (fqwt-list nil) ratio-list s-list diff-list freq-of-wt)
    (loop for fq in fq-list
          do
          (progn (setf ratio-list (mapcar #'(lambda (x)  (/ fq (ran-freq_wt (eval x)))) wt-list))
                 (setf s-list (sort (mapcar #'cons ratio-list wt-list) #'< :key #'car))
                 (setf s-list (member-if #'(lambda (x)(>= x min-ratio)) s-list :key #'car))
                 (setf s-list (member-if #'(lambda (x)(<= x max-ratio)) (nreverse s-list) :key #'car))
                 (if  (null s-list)
                   (setf diff-list (mapcar #'(lambda (x)  (- (ran-freq_wt (eval x)) fq)) wt-list)
                         s-list (sort (mapcar #'cons diff-list wt-list) #'< :key #'(lambda (x)(abs(car x))))
                         freq-of-wt (+ fq (caar s-list))
                         )  
                   (setf s-list (list (choose-in-list s-list))
                          freq-of-wt (/ fq (caar s-list) )
                        ))
                 (when limits
                   (let ((ratio (/ fq freq-of-wt)))
                     (when (or (> ratio (cdr limits))(< ratio (car limits)))
                       (format t "WARNING ! TRANSPOSITION OF ~a EXCEEDS LIMITS.~%      Transposition : ~a / limits : ~a~%Wanted freq  = ~a WT Freq = ~a ~%"
                               (cdar s-list) ratio limits fq freq-of-wt))))
                 (push (cdar s-list) cwt-list)
                 (push (list fq freq-of-wt) fqwt-list)))
    (setf cwt-list (nreverse cwt-list))
    (setf fqwt-list(nreverse fqwt-list))
    (read-from-string
     (with-output-to-string (s)
       (loop for cwt in cwt-list
;OLD FORMAT   initially (format s "((cwt '(~%" )
             initially (format s "((cwt ''(~%" )
             do (let*((end (dur_wt (eval cwt)))
                      (begin (abs (+ (ran (* beg-ran end))(* beg-ratio end)))))
                  (format s "(list '~a ~a ~a)~%" cwt begin end))
;OLD FORMAT                  (format s "'(list '~a ~a ~a)~%" cwt begin end))
             finally (format s "))~%" ))
       (loop for fqwt in fqwt-list
; OLD            initially (format s "(fqwt '(~%" )
             initially (format s "(fqwt ''(~%" )
; OLD            do (format s "'(list ~a ~a)~%" (car fqwt)(cadr fqwt))
             do (format s "(list ~a ~a)~%" (car fqwt)(cadr fqwt))
             finally (format s ")))~%" ))
       ))))
|#
;-----------------------------------------------------------------------------

;____________________________________________________________________________
; chroma/pls/wt-frz.lisp
; PLS system: generator of sampler's freeze
;____________________________________________________________________________
;*****************************************************************************
;-------| PULS SYSTEM
;-------| This file is: $LLpls/wt-frz.ll
;-------| Version V1.0: Aug 19, 1991 /  
;-------| By Marco Stroppa
;-------| Copyright 1991 IRCAM
;*****************************************************************************
(in-package :cr)

; SPECIFICATION AND CONTROL OF A FREEZING ALGORITHM FOR WT OBJECTS

;                 MUST BE ASSOCIATED TO TYPE WT

; DESCRIPTION OF THE DATA STRUCTURE:
; Set of functions to set and vary control parameters of the freezing
;    algorithm embedded by the function "freeze" in WTsys.ll. The algorithm
;    was designed and implemented by Jan Vandenheede at IRCAM in 1991.
; The algorithm stretches the duration of a sound file so as to fit the
;    duration specified in the score.
; The algorithm is controlled by two parameters: FRZ-CTL, FRZ-MODE.

; FRZ-CTL (MAIN FREEZE CONTROL) [A-list with reserved fields]
; FRZ-CTL specifies all the parameters required by the freeze algorithm to
;   function properly. They consist of the following fields:
;	inc-pt		: incrustation point, i.e. where in the original
;			    sound new segments are to be inserted
;	dur		: duration of the new segment
;	skip		: skip around inc-pt for the new segment
;	end-		: overlap between the new and the current segments
;	ampfac		: amplitude scaler
;	xin/xout	: crossfade at beginning/end of segment
;	first-xout
;	last-xin
;	last-xout	: special cases of the above crossfades
;	min-xin
;	min-xout	: local minimum values - distinct from the ones
;			    specified by (fade_wt ...) which are referred
;			    to the physical sound file - used during the
;			    test of some extreme cases, to avoid too small
;			    values that might produce clicks

; When retrieving the value of any field, both the entire list and the current
;    value are evaluated. This allows for the definition of dynamic controls
;    with some jitter or other functional dependencies.

; Therefore an absolute definition of a control list, such as the one below,
;    is rare:
;	( (inc-pt <sec>)	INCRUSTATION POINT
;	  (dur <sec>)		DURATION OF INCRUSTED SEGMENTS
;	  (skip <sec>)		SKIP POINT FOR EACH NEW SEGMENT
;	  (end- <sec>)		FINAL PORTION OF THE SOUND
;	  (ampfac <0:1>)	AMPLITUDE SCALER FOR EACH NEW SEGMENT
;	  (xin <sec>)		IN/OUT CROSSFADES
;	  (xout <sec>)
;	  (first-xout <sec>)
;	  (last-xin <sec>)
;	  (last-xout <sec>)
;	)

; Most often the control values are defined relative to a global variable,
;   usually set proportional to the duration of the the wt object, like
;   the ovlp (shortcut for overlap) below:
;   (let ((ovlp (* (dur_wt (curr_wt) (my-si_wt)) 0.1)) )
;     `(				OVLP SET TO 10% OF THE DURATION
;	 (inc-pt (* ,ovlp 4.0))		4 TIMES OVLP
;	 (dur (ran (* ,ovlp 2.0)	RANDOM VARIATION AROUND 1/2 OVLP
;		   (* ,ovlp 0.5)) )
;	 (skip (ran (* ,ovlp 4.0)	
;		    ,ovlp) )
;	 (end- ,ovlp)			
;	 (ampfac 1.0)			
;	 (xin ,ovlp)			
;	 (xout ,ovlp)
;	 (first-xout ,ovlp)
;	 (last-xin ,ovlp)
;	 (last-xout ,ovlp)
;	 (min-xin (* (get-gbl 'DURMIN) 0.5) )
;	 (min-xout (* (get-gbl 'DURMIN) 0.5) )
;	)
;    )

; It is important to specify whether a control parameter is defined absolutely
;   (ie once for all with respect to the current wt object) or must vary as
;   a function of the current sample increment.
; In this latter case, the function (my-si_wt) is to be used. It means that
;   the value of the sample increment will be dynamically bound when the
;   wt object is instantiated by WTsys. This function, however, cannot be
;   used outside of that context (see "SPECIAL FEATURES" below).
; There may be any number of freeze control structures associated to a WT
;    object. They will be ordered according to their definition and can be
;    independently retrieved using nth-frz_wt.

; FRZ-MODE (FREEZE MODE) [-1 or 0 or 1]
; It determines the behavior of the freeze algorithm with respect to the
;     current WT object.
;	mode = 1 : call the freeze algorithm when needed
;	mode = 0 : do not freeze, START object at the right action time
;	mode = -1: do not freeze, END object at the right time
;       mode = floating number, call the freeze only when the duration of the snd file
;                to freeze is longer than <mode>

; LCO (LOCAL OVERWRITING OF GLOBAL CONTROLS)
; Before being used by the freeze algorithm a WT object must be initialized
;    with the freeze controls. This is usually done by calling
;    (set-frz_wt ...) and (set-frz-mode_wt ...) when defining the object.
; However, all the controls can be locally overwritten by adding a third
;    argument to the FQ field, whose first and second arguments mean the
;    resulting frequency and the reference frequency.
; Those local fields must belong to a structure of type CTL (see ctl.ll).
;	si	 : if t the value of the FQ field is a sample increment,
;		      otherwise it is a frequency
;	flt	 : object's filter
;	frz-mode : freeze mode
;	frz-ctl	 : freeze local control overwrite. This may be:
;			NUM : use the freeze control structure number NUM
;				using nth-frz_wt
;			CTL : control structure as specified above (FRZ-CTL);
;				if not complete, the missing values will be
;				retrieved from the current WT object
; For the usage and the interpretation of LCO see here below.
; Ex. (FQ '(list 1.0 1.0 (make_ctl '(si t) '(flt '(/ (get-gbl 'SR/2 4)))
;				'(frz-ctl 2))) ... )

; SPECIAL FEATURES
; Besides the usual selectors, modifiers, predicates and suppliers of
;    information - constructors do not exist, since this is not a new
;    structure, but some added features to a WT structure - there are some
;    LOCAL and AUTOMATIC SELECTORS.
; Both selectors can be used only from within the freeze algorithm, since they
;    dynamically bind to some local variables linked to the currently selected
;    (active) WT object. They implement the meaning of "my" in object-oriented
;    languages.
; LOCAL SELECTORS supply the value of the currently active WT object
;    (curr_wt), its sample increment (my-si_wt), computed after all the
;    modifications, its added control structure (see above) (my-lco_wt) and
;    the absolute and time with respect to the current beginning of the
;    object (after the computation of any local offsets, if existing)
;    (my-trel_wt, my-tnorm_wt).
; AUTOMATIC SELECTORS perform higher-order computations that are directly used
;    by the freeze algorithm. They all first look whether a LCO exist for the
;    current active object and, if so, whether a local field is defined. If
;    not, they will look at the object's field. If the field is still not
;    defined, they will return the global default.
;    (auto-is-si_wt)	: return t if the current value is a sample increment
;    (auto-frz-mode_wt)
;    (auto-flt_wt)	: return the current value
;    (auto-frz-field)   : if it is a number, look for the field in the
;			   control structure of that number in the current
;			   WT object, else it must be a control structure
;			   (not necessarily complete) in the correct format
; REMARK: si, mode and flt are evaulated once when retrieved
;	: the frz-field is evaluated twice, as usual (see above).

; CURRENT IMPLEMENTATION:
;  Added fields to an already existing structure of type WT.
;    Added fields are:
;	frz-ctl
;	frz-mode

; set global variables in package #:marco:pls
;(set-colon 'PLS)

; AVAILABLE FUNCTIONS:
;	SELECTORS/[MODIFIERS]:	[set-]frz_wt
;				[set-]frz-mode_wt

;	SELECTORS:		first-frz_wt
;				last-frz_wt
;				nth-frz_wt
;				frz-field_wt

;	MODIFIERS:		[fast-freeze_wt]

;	LOCAL SELECTORS:	curr_wt
;				curr-win_wt
;				my-si_wt
;				my-trel_wt
;				my-tnorm_wt
;				seg-dur_wt
;				seg-last-xin_wt

;	AUTOMATIC SELECTORS:	auto-frz-flt_wt
;				auto-frz-mode_wt
;				auto-frz-field_wt
;				auto-is-si_wt

;	PREDICATES:		is-frz_wt

;	INFO:			length-frz_wt
;				prnt-frz_wt
;				short-print-frz_wt


;-----------------------------------------------------------------------------
"
;	NAME:		set-frz_wt / fast-frz_wt (CONSTRUCTORS)
;	TYPE:		Expr with 2+ arguments
;	CALL:		(set-frz_wt wt freeze1 ... freezeN)
;			(fast-frz_wt wt freeze1 ... freezen)
;	FUNCTION:	set-frz_wt: define an arbitrary number of freeze
;			   control lists
;			fast-frz_wt: faster specification of controls
;	VALUE:		'ok
;	SOURCE:		$LLpls/freeze-wt.ll

; SET-FRZ_WT
; FOR THE TIME BEING THE CONTROL LIST MUST BE COMPLETE
; IN THE FUTURE A MORE INTELLIGENT ANALYSIS OF THE INPUT PARAMETER WILL BE
;   IMPLEMENTED
; ANY NUMBER OF CONTROL LISTS, SPECIFYING DIFFERENT CONTROL PARAMETERS OF THE
;    FREEZE ALGORITHM, IS POSSIBLE"

(setf my-frz1
   ''(let ((ovlp (* (dur_wt (curr_wt) (my-si_wt)) 0.1)) )
       `(
	 (inc-pt (* ,ovlp 4.0))		; INCRUSTATION POINT
	 (dur (ran (* ,ovlp 2.0)	; DURATION OF INCRUSTED SEGMENTS
		   (* ,ovlp 0.5)) )
	 (skip (ran (* ,ovlp 4.0)	; SKIP POINT FOR EACH NEW SEGMENT
		    ,ovlp) )
	 (end- ,ovlp)			; FINAL PORTION OF THE SOUND
	 (ampfac 1.0)			; AMPLITUDE SCALER FOR EACH NEW SEGMENT
	 (xin ,ovlp)			; IN/OUT CROSSFADES
	 (xout ,ovlp)
	 (first-xout ,ovlp)
	 (last-xin ,ovlp)
					; LAST-XOUT SET TO THE WHOLE DURATION
					;    OF THE LAST SEGMENT
	 (last-xout (- (seg-dur_wt) (seg-last-xin_wt)) )
	 (min-xin (* (get-gbl 'DURMIN) 0.5) )	; MIN-XIN / XOUT FOR EXTREME
	 (min-xout (* (get-gbl 'DURMIN) 0.5) )	;  TESTS
	)
     ))

(setf my-frz2
     ''(
	 (inc-pt 0.1)
	 (dur 0.5)
	 (skip 0.2)
	 (end- 0.25)
	 (ampfac 0.5)
	 (xin 0.1)
	 (xout 0.15)
	 (first-xout 0.25)
	 (last-xin 0.2)
    ))

(set-frz_wt my-wt my-frz1 my-frz2)

;-----------------------------------------------------------------------------
; ****** TO IMPLEMENT ****!
; FAST-FRZ_WT
; ARGUMENTS ARE A LIST CONTAINING THE INCRUSTATION POINT AND THE VALUE OF
;    THE OVERLAP
; VALUES WILL NOT BE EVALUATED (SO FUNCTIONS WILL BE KEPT AS SUCH AND
;    EVALUATED ONLY WHEN NEEDED)

;(defun fast-freeze_wt (wt &rest fast-frz-lst)
;	(print "NOT IMPLEMENTED YET")
;	'BYE)
;   (pls-check-type 'WT wt 'fast-frz_wt)
;   (let ((result ()) )
;     (while fast-frz-lst
;	(lets ( (curr-ctl (nextl fast-frz-lst))
;		(curr-inc-pt (car curr-ctl))
;		(curr-ovlp (cadr fast-frz-lst))) ))))

;-----------------------------------------------------------------------------
;	NAME:		frz-mode_wt (SELECTOR / MODIFIER)
;	TYPE:		Expr with 1/2 arguments
;	CALL:		(frz-mode_wt wt) / (set-frz-mode_wt wt mode)
;	FUNCTION:	get / set the freeze mode (see above)
;	VALUE:		the above value / the symbol 'ok
;	SOURCE:		$LLpls/wt-frz.ll

(frz-mode_wt my-wt)
(set-frz-mode_wt my-wt 0)
(frz-mode_wt my-wt)

;-----------------------------------------------------------------------------
;	NAME:		first/last/nth-frz_wt (SELECTORS)
;	TYPE:		Expr with 1/1/2 arguments
;	CALL:		(first/last-frz_wt wt) / (nth-frz_wt wt pos)
;	FUNCTION:	retrieve a whole control list
;	VALUE:		control list
;	SOURCE:		$LLpls/wt-frz.ll

;FIRST-FRZ_WT
(first-frz_wt my-wt)
(last-frz_wt my-wt)
(nth-frz_wt my-wt 0)
(nth-frz_wt my-wt 3)

#|
;-----------------------------------------------------------------------------
;	NAME:		frz-field_wt (SELECTOR)
;	TYPE:		Expr with 2 arguments
;	CALL:		(frz-field frz-ctl field)
;	FUNCTION:	retrieve the value of field in frz-ctl
;	VALUE:		double evaluation: frz-ctl and field
;	SOURCE:		$LLpls/wt-frz.ll
(frz-field_wt (first-frz_wt my-wt) 'inc-pt)

(cassq 'inc-pt (eval(last-frz_wt my-wt))))


(defun frz-field_wt (ctl field)
   (eval
     (car (cassq field (eval ctl)))) )


;-----------------------------------------------------------------------------
;	NAME:		auto-frz-field_wt / etc. (SELECTORS)
;	TYPE:		Expr with 0 / 1 arguments
;	CALL:		(auto-frz-field field) / (auto-is-si_wt) / etc.
;	FUNCTION:	retrieve the value after intelligent search
;	VALUE:		the above value
;	SOURCE:		$LLpls/wt-frz.ll

; AUTO-IS-SI_WT: RETURN T IF THE VALUE OF THE FQ FIELD IS EXPRESSED IN SI
(defun auto-is-si_wt ()
   (if (and (my-lco_wt)			; IF THERE IS AN OVERWRITE STRUCTURE
	    (is-key_ctl (my-lco_wt) 'si) )	;    AND SI IS DEFINED
       (eval (get_ctl (my-lco_wt) 'si))	; RETRIEVE AND EVAL FIELD SI
       (is-si_wt (curr_wt))	; ELSE LOOK AT THE VALUE IN THE CURR OBJECT
   ))

; AUTO-FLT_WT: RETURN THE VALUE OF THE FILTER FIELD
(defun auto-flt_wt ()
   (if (and (my-lco_wt)			; LOCALLY OVERWRITTEN
	    (get_ctl (my-lco_wt) 'flt))
	(eval (get_ctl (my-lco_wt) 'flt))
	(flt_wt (curr_wt))) )

; AUTO-FRZ-MODE_WT: RETURN THE FREEZE MODE
(defun auto-frz-mode_wt ()
   (cond
   	((and (my-lco_wt)			; LOCALLY OVERWRITTEN
	      (get_ctl (my-lco_wt) 'frz-mode))
	 (eval (get_ctl (my-lco_wt) 'frz-mode)) )
	((is-frz_wt (curr_wt))			; LOOK IN CURRENT OBJECT
	 (eval (frz-mode_wt (curr_wt))) )
	(t					; OTHERWISE GET GLOBAL
	 (get-gbl 'FRZ-MODE))
   ))

;-----------------------------------------------------------------------------
; AUTO-FRZ-FIELD_WT: RETURN THE CURRENT FIELD
; REMARK: WHEN THE OBJECT IS FROZEN (I.E. FRZ-CTL AND MODE ARE DEFINED),
;	    IF THE CURRENT FIELD DOES NOT EXIST, IT WILL BE FETCHED FROM
;	    THE DEFAULT CONTROL STRUCTURE
(defun auto-frz-field_wt (field)
"
; AUTO-FRZ-FIELD_WT: RETURN THE CURRENT FIELD
; REMARK: WHEN THE OBJECT IS FROZEN (I.E. FRZ-CTL AND MODE ARE DEFINED),
;	    IF THE CURRENT FIELD DOES NOT EXIST, IT WILL BE FETCHED FROM
;	    THE DEFAULT CONTROL STRUCTURE
"
   (let ((tmp-frz (cond ((null (my-lco_wt)) ())
			((numberp (my-lco_wt)) (my-lco_wt))
			(t (get_ctl (my-lco_wt) 'frz-ctl))) )
	)

				; IF THERE IS NO FRZ-CTL STRUCTURE
      (ifn tmp-frz
			; GET THE VALUE FROM THE OBJECT OR THE GLOBAL DEFAULT
	   (if (is-frz_wt (curr_wt))
	       (let ((fld (frz-field_wt (first-frz_wt (curr_wt)) field) )
		    )
		  (ifn fld		; OBJECT FROZEN, BUT FIELD NOT DEFINED
		       (frz-field_wt (get-gbl 'DEF-FRZ) field)
		       fld)
		)
	       (frz-field_wt (get-gbl 'DEF-FRZ) field) )

				; ELSE THE STRUCTURE IS A NUMBER
	   (if (numberp tmp-frz)

	       (let ((fld (frz-field_wt (nth-frz_wt (curr_wt) tmp-frz) field))
		     )
		   (ifn fld
		       (frz-field_wt (get-gbl 'DEF-FRZ) field)
			fld)
		)
	       
			;    OR A CTL STRUCTURE WHICH MAY CONTAIN OR NOT FIELD
	       (if (cassq field (eval tmp-frz))		; MUST ALWAYS EVALUATE
		   (frz-field_wt tmp-frz field)		; FIELD EXISTS

				; FIELD MUST BE FETCHED FROM OBJ OR DEFAULT
		   (if (is-frz_wt (curr_wt))
		       (let ((fld
				(frz-field_wt (first-frz_wt (curr_wt)) field))
			    )
		       (ifn fld		; OBJECT FROZEN, BUT FIELD UNDEFINED
		          (frz-field_wt (get-gbl 'DEF-FRZ) field)
			  fld)
		       )
		       (frz-field_wt (get-gbl 'DEF-FRZ) field))
		)
	    )
	)
   ))
		   

;-----------------------------------------------------------------------------
;				seg-dur_wt
;				seg-last-xin_wt
;	NAME:		curr_wt / my-lco/si/time_wt / etc. (LOCAL SELECTORS)
;	TYPE:		Expr with 0 arguments
;	CALL:		(curr_wt) / (my-si_wt) / (my-trel_wt) / etc.
;	FUNCTION:	return the currently bound WT object / window / LCO
;			       the current sample increment
;			       the starting time [sec] of the current element
;			          with respect to the the beginning of the
;			          object after local adjustements
;			       the starting time normalized with respect to
;				  the total duration
;			REMARK: LOCAL SELCTORS can only be used within
;			   the freezing algorithm, where the needed variables
;			   will be dynamically bound
;	VALUE:		the above value
;	SOURCE:		$LLpls/wt-frz.ll

; CURR_WT
(defun curr_wt ()
  (declare (special current-wt))
    (ifn (boundp 'current-wt)	; BOUND ONLY WITHIN THE ALGORITHM IN WTsys
	 (error "WRONG CONTEXT: THIS FUNCTION CAN ONLY BE USED WHILE FREEZING"
	 'SORRY)
	 current-wt) )

;
; CURR-WIN_WT: RETURN THE CURRENT WINDOW, WHEN SPECIFIED, () OTHERWISE
;	THE WINDOW IS WHAT FOLLOWS THE "wanted-si" "reference-si" IN THE
;	   CWT FIELD OF THE WT INSTRUCTION
;	IT CONTAINS: "local beg-off" "local end-off" ['(xin xout)]
(defun curr-win_wt ()
 (declare (special current-win))
    (ifn (boundp 'current-win)	; BOUND ONLY WITHIN THE ALGORITHM IN WTsys
	 (error  "WRONG CONTEXT: THIS FUNCTION CAN ONLY BE USED WHILE FREEZING"
	 'SORRY)
	 current-win) )

;
;-----------------------------------------------------------------------------
; MY-SI_WT : CURRENT SAMPLE INCREMENT
(defun my-si_wt ()
 (declare (special current-si))
    (ifn (boundp 'current-si)
	(error "WRONG CONTEXT: THIS FUNCTION CAN ONLY BE USED WHILE FREEZING"
	'SORRY)
	current-si) )

; MY-LCO_WT : CURRENT CONTROL STRUCTURE (TYPE CTL, OR () IF UNDEFINED)
(defun my-lco_wt ()
 (declare (special current-lco))
    (ifn (boundp 'current-lco)
	(error "WRONG CONTEXT: THIS FUNCTION CAN ONLY BE USED WHILE FREEZING"
	'SORRY)
	current-lco) )

;
; MY-TREL_WT
(defun my-trel_wt ()
 (declare (special current-trelative))
    (ifn (boundp 'current-trelative)
	(error "WRONG CONTEXT: THIS FUNCTION CAN ONLY BE USED WHILE FREEZING"
	'SORRY)
	current-trelative) )
;
; MY-TNORM_WT
(defun my-tnorm_wt ()
   (declare (special current-tnorm))
    (ifn (boundp 'current-tnorm)
	(error "WRONG CONTEXT: THIS FUNCTION CAN ONLY BE USED WHILE FREEZING"
	'SORRY)
	current-tnorm) )


;-----------------------------------------------------------------------------
;	NAME:		seg-dur_wt / seg-last-xin_wt (LOCAL SELECTORS)
;	TYPE:		Expr with 0 arguments
;	CALL:		(seg-dur_wt) / (seg-last-xin_wt)
;	FUNCTION:	return the duration and the value of the last xin
;			   of the current segment used by the freeze algorithm
;			REMARK: LOCAL SELCTORS can only be used within
;			   the freezing algorithm, where the needed variables
;			   will be dynamically bound
;	VALUE:		the above value
;	SOURCE:		$LLpls/wt-frz.ll

; SEG-DUR_WT
(defun seg-dur_wt ()
 (declare (special next-dur))
    (ifn (boundp 'next-dur)	; BOUND ONLY WITHIN THE ALGORITHM IN WTsys
	 (error "WRONG CONTEXT: THIS FUNCTION CAN ONLY BE USED WHILE FREEZING"
	 'SORRY)
	 next-dur) )

;
; SEG-LAST-XIN_WT
(defun seg-last-xin_wt ()
  (declare (special last-xin))
    (ifn (boundp 'last-xin)	; BOUND ONLY WITHIN THE ALGORITHM IN WTsys
	 (error "WRONG CONTEXT: THIS FUNCTION CAN ONLY BE USED WHILE FREEZING"
	 'SORRY)
	 last-xin) )



;-----------------------------------------------------------------------------
(defun is-frz_wt (wt)
"
;	NAME:		is-frz_wt (PREDICATE)
;	TYPE:		Expr with 1 argument
;	CALL:		(is-frz_wt wt)
;	FUNCTION:	test whether the wt object has a field for freeze
;			   control; if frz-mode = 0, do not check whether
;                          a freeze control structure is present and return t;
;                          otherwise, both structures must be present
;	VALUE:		t or nil according to the test
;	SOURCE:		$LLpls/wt-frz.ll
"
  (pls-check-type 'WT wt 'is-frz_wt)
  (cond ((null (is-field_wt wt 'frz-mode)) nil)
        ((= (frz-mode_wt wt) 1)(is-field_wt wt 'frz-ctl))
        (t t)))


;-----------------------------------------------------------------------------
;	NAME:		length-frz_wt/print_/short-print_wt  (INFO)
;	TYPE:		Expr with 1 argument
;	CALL:		(length-frz_wt wt)
;			(print_wt wt)
;			(short-print_wt wt)
;	FUNCTION:	compute how many methods for freeze control exist
;			nicely or shortely print a structure of type TBL
;	VALUE:		the amount of methods or the string 'done
;	SOURCE:		$LLpls/wt-frz.ll

(defun length-frz_wt (wt)
    (pls-check-type 'WT wt 'length-frz_wt)
    (if (is-field_wt wt 'frz-ctl)
	(length (field_wt wt 'frz-ctl))
	(error "I'M NOT SURE THAT A FREEZE CONTROL IS DEFINED FOR ~a"wt)) )

(defun print-frz_wt (wt)
    (pls-check-type 'WT wt 'print-frz_wt)
   ; (print (frz_wt wt)) 
    (print wt)
    )

(defun short-print-frz_wt (wt)
    (pls-check-type 'WT wt 'short-print-frz_wt)
  ;  (print (frz_wt wt))
    (print wt)
    )

;-----------------------------------------------------------------------------
|#


;**********************************************
(print "Finished loading OMChroma-tests")
;**********************************************
