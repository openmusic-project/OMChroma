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
(defun make_wt (file &rest dir)
   (let ((wtbl (make_tbl)))
	(insert_tbl wtbl 'file 'name file)
	(ifn dir (insert_tbl wtbl 'file 'dir ())
		 (insert_tbl wtbl 'file 'dir (car dir)))
	(let ((file (ifn dir file 
                       (merge-pathnames (car dir) file)
                      ))
                      )
	    (let* ((sndinfo (get-sndinfo file))
		   (dur (cassq 'dur sndinfo)))
		(insert_tbl wtbl 'sf 'sr (cassq 'SR sndinfo))
		(insert_tbl wtbl 'sf 'nchan  (cassq 'n-ch sndinfo))
		(insert_tbl wtbl 'sf 'pk-mode  (cassq 'pk-mode sndinfo))
		(insert_tbl wtbl 'sf 'n-smpls  (cassq 'n-smpl sndinfo))
		(insert_tbl wtbl 'dur 'physical dur)

		(insert_tbl wtbl 'dur 'att () )		; BEST COMPROMISE
		(insert_tbl wtbl 'dur 'var-att 0.0)
		(insert_tbl wtbl 'win 'from 0.0)
		(insert_tbl wtbl 'win 'size (- dur (get-gbl 'WINMIN)))
		(insert_tbl wtbl 'win 'min-size (get-gbl 'WINMIN))
		(insert_tbl wtbl 'freq 'val 1.0)
		(insert_tbl wtbl 'freq 'mode 'si)
		(insert_tbl wtbl 'flt (* (cassq 'SR sndinfo) 0.5))
		(insert_tbl wtbl 'fade-env '(0.0 0.0) )
		(insert_tbl wtbl 'off 'beg 0.0)
		(insert_tbl wtbl 'off 'end (cassq 'dur sndinfo))))
	(attach-type 'WT wtbl)))

; REINIT_WT
(defun reinit_wt (wt file &rest dir)
	(insert_tbl (contents wt) 'file 'name file)
	(ifn dir (insert_tbl (contents wt) 'file 'dir ())
		 (insert_tbl (contents wt) 'file 'dir (car dir)))
	(let ((file (ifn dir file (catenate (car dir) file))))
	    (let ((sndinfo (get-sndinfo file))
		  (wtbl (contents wt)) )
		(insert_tbl wtbl 'sf 'sr (cassq 'SR sndinfo))
		(insert_tbl wtbl 'sf 'nchan  (cassq 'n-ch sndinfo))
		(insert_tbl wtbl 'sf 'pk-mode  (cassq 'pk-mode sndinfo))
		(insert_tbl wtbl 'sf 'n-smpls  (cassq 'n-smpl sndinfo))
		(insert_tbl wtbl 'dur 'physical  (cassq 'dur sndinfo))))
wt)

; ADD/RM-FIELD_WT
(defun add-field_wt (wt fld &rest val)
	(pls-check-type 'WT wt 'add-field_wt)
	(insert_tbl (contents wt) fld (car val)))

(defun rm-field_wt (wt fld)
	(pls-check-type 'WT wt 'rm-field_wt)
	(if (is-field_wt wt fld)
	   (rm_tbl (contents wt) fld)
	   (when (get-gbl 'PRNFLG)
             (progn (capi::beep-pane nil)
               (print (format () "         WARNING: CAN'T REMOVE AN INEXISTENT FIELD, SIR: ~a~%            OUFF...." fld))))))


;	NAME:		SELECTORS / MODIFIERS
;	TYPE:		Expr with 1/2 arguments
;	CALL:		(file-name_wt wt) / (set-file-name_wt wt val) / etc.
;	FUNCTION:	select / modify the value of a given field
;	VALUE:		the above value / the symbol 'ok
;	SOURCE:		$LLpls/wt.ll

; FILE-NAME: RETURN/SET THE (RELATIVE) NAME OF THE FILE
(defun file-name_wt (wt)
	(pls-check-type 'WT wt 'file-name_wt)
	(lookup_tbl (contents wt) 'file 'name))
;
(defun set-file-name_wt (wt name)
	(pls-check-type 'WT wt 'set-file-name_wt)
	(insert_tbl (contents wt) 'file 'name name))


; DIR: RETURN/SET THE DIRECTORY CONTAINING FILENAME
(defun dir_wt (wt)
	(pls-check-type 'WT wt 'dir_wt)
	(lookup_tbl (contents wt) 'file 'dir))
;
(defun set-dir_wt (wt val)
	(pls-check-type 'WT wt 'set-dir_wt)
	(insert_tbl (contents wt) 'file 'dir val))


; FREQ: RETURN/SET THE FREQUENCY OR SI FIELD (SINGLE VALUE OR A LIST)
; SET-FREQ_WT: automatically set a frequency or si, unless the 3rd optional arg is 'si
; SET-SI_WT: set a sample increment, always
(defun freq_wt (wt)
	(pls-check-type 'WT wt 'freq_wt)
	(lookup_tbl (contents wt) 'freq 'val))
;
(defun set-freq_wt (wt val &optional (mode))
   "
Set the FREQ and SI field of a WT object.
New version (omChroma): automatic test for FREQ or SI.

wt: WT object
val: frequency OR sampling increment
mode: if present, if = SI force mode to SI, if = FREQ, force mode to FREQ, otherwise, ERROR.

Use the global variable 'MAXSI.
Corrected insidious BUG with test of third argument!
"   

   (pls-check-type 'WT wt 'set-freq_wt)
   (insert_tbl (contents wt) 'freq 'val val)
   (cond
    ((and mode (string-equal (string mode) "si"))
     (insert_tbl (contents wt) 'freq 'mode 'si) )
    ((and mode (string-equal (string mode) "freq"))
     (insert_tbl (contents wt) 'freq 'mode 'freq) )
    (mode
     (error "WHAT A BIZZARE VALUE, SIR ~a: ~a. I REGRET I CAN ONLY ACCEPT 'si OR 'freq.
NOBODY'S PERFECT!~%" (get-gbl 'USER) mode ))
    ((<= val (get-gbl 'MAXSI))
     (insert_tbl (contents wt) 'freq 'mode 'si) )
    (t
     (insert_tbl (contents wt) 'freq 'mode 'freq)) ))

;
(defun set-si_wt (wt val)
	(pls-check-type 'WT wt 'set-si_wt)
	(insert_tbl (contents wt) 'freq 'val val)
	(insert_tbl (contents wt) 'freq 'mode 'si) )


; FLT: RETURN/SET THE FREQUENCY OF THE FILTER ASSOCIATED TO THE OBJECT
;	BY DEFAULT IT IS SET TO THE NYQUIST FREQUENCY
; FADE-ENV: RETURN/SET THE DURATION OF FADE-IN AND FADE-OUT ENVELOPES THAT
;	ARE SUPERPOSED TO THE OTHER ENVELOPES (BY DEFAULT = 0.0)
(defun flt_wt (wt)
	(pls-check-type 'WT wt 'flt_wt)
	(lookup_tbl (contents wt) 'flt) )
;
(defun set-flt_wt (wt val)
   (pls-check-type 'WT wt 'set-flt_wt)
   (insert_tbl (contents wt) 'flt val) )
  
;
(defun fade_wt (wt)
	(pls-check-type 'WT wt 'fade_wt)
	(lookup_tbl (contents wt) 'fade-env) )
;
(defun set-fade_wt (wt val)
   (pls-check-type 'WT wt 'set-fade_wt)
   (if (consp val)
       (insert_tbl (contents wt) 'fade-env val)
       (error "ILLEGAL VALUE. MUST BE A LIST OF TWO NUMBERS: ~a"
	      val)) ) 

; BEG-OFF: RETURN/SET THE OFFSET FROM THE BEGINNING OF THE SOUND FILE THAT
;    WILL BE CONSIDERED AS THE (REAL) BEGINNING
; WHEN SI IS PASSED AS AN ARG, TIME VALUE IS SCALED ACCORDINGLY
; THIS MEANS THAT WITH A SI OF 2.0, THE ABSOLUTE TIME OF 1.0 WILL BECOME 2.0
;   (IT TAKES 2.0 absolute secs TO GENERATE THE DURATION OF 1"
; ERROR: BEG-OFF CANNOT BE > END-OFF
; IF DUR-ATT < BEG-OFF, SET IT TO BEG-OFF
(defun beg-off_wt (wt &rest si)
    (pls-check-type 'WT wt 'beg-off_wt)
    (ifn si
	(lookup_tbl (contents wt) 'off 'beg)
	(/ (lookup_tbl (contents wt) 'off 'beg) (car si))))
;
(defun set-beg-off_wt (wt val &rest si)
    (pls-check-type 'WT wt 'set-beg-off_wt)
    (let ((si (ifn si 1.0 (car si))))
	(let ((real-off (* val si)))
	    (cond
		((beyond-phys-begof_wt wt real-off)
		 (error-begof_wt 'set-beg-off_wt val 0.0))
		((beyond-eof_wt wt real-off)
		 (error-eof_wt 'set-beg-off_wt val (end-off_wt wt si)))
		(t
		 (let ((drat (dur-att_wt wt)))
		   (unless (eq drat 'undefined)	; IF ATT IS NOT SPECIFIED
		      (when (< (phys-dur-att_wt wt) real-off)	; DO NOTHING
			(set-phys-dur-att_wt wt real-off) ; DUR-ATT = BEG-OFF
                        (when (get-gbl 'PRNFLG)
                          (capi::beep-pane nil)
                          (print (format () "set-beg-off_wt - WARNING: DUR-ATT < BEG-OFF~%       PHYS-DUR-ATT MODIFIED TO ~a~%" real-off)))
		      )
		   ))
		 (insert_tbl (contents wt) 'off 'beg real-off))))) )


; END-OFF: RETURN/SET THE DISTANCE BETWEEN THE PHYSICAL BEGINNING OF THE
;    SOUND FILE THAT WILL BE CONSIDERED AS THE (REAL) END
; ERROR: END-OFF CANNOT BE < BEG-OFF
; IF DUR-ATT > END-OFF, SET IT TO END-OFF
(defun end-off_wt (wt &rest si)
    (pls-check-type 'WT wt 'end-off_wt)
    (ifn si
	(lookup_tbl (contents wt) 'off 'end)
	(/ (lookup_tbl (contents wt) 'off 'end) (car si))))
;
(defun set-end-off_wt (wt val &rest si)
  (pls-check-type 'WT wt 'set-end-off_wt)
  (let ((si (ifn si 1.0 (car si))))
    (let ((real-off (* val si)))
      (cond
       ((beyond-phys-eof_wt wt real-off)
        (error-eof_wt 'set-end-off_wt val (phys-dur_wt wt si)))
       ((<= real-off (beg-off_wt wt))
        (capi::beep-pane nil)
        
        (error "set-end-off_wt: CAN'T SET END OFFSET <= BEG OFFSET. END-OFF: ~d, BEG-OFF: ~d~%" val (beg-off_wt wt si)))
       (t
        (let ((drat (dur-att_wt wt)))
          (unless (eq drat 'undefined)	; IF ATT IS NOT SPECIFIED
            (when (>= (phys-dur-att_wt wt) real-off)
              (set-phys-dur-att_wt wt real-off)
              (when (get-gbl 'PRNFLG)
                (capi::beep-pane nil)
                (print "set-end-off_wt - WARNING: DUR-ATT >= END-OFF~%            PHYS-DUR-ATT MODIFIED TO ~a~%" real-off))
              )
            ))
        (insert_tbl (contents wt) 'off 'end real-off))))) )


; [PHYS-]DUR-ATT: RETURN/SET THE DURATION OF THE ATTACK PORTION OF
;    THE SOUND FILE EITHER FROM THE BEGINNING OF THE SOUND FILE (PHYS) OR
;    FROM BEG-OFF
; REMARK: DUR-ATT CANNOT BE < BEG-OFF
(defun phys-dur-att_wt (wt &rest si)
    (pls-check-type 'WT wt 'phys-dur-att_wt)
    (let ((drat (lookup_tbl (contents wt) 'dur 'att)) )
       (ifn drat
	 'undefined
	 (ifn si
	      drat
	      (/ drat (car si)))) ))

(defun dur-att_wt (wt &rest si)
    (pls-check-type 'WT wt 'dur-att_wt)
    (let ((drat (lookup_tbl (contents wt) 'dur 'att)) )
       (ifn drat
	 'undefined
	 (ifn si
	      (- (lookup_tbl (contents wt) 'dur 'att) (beg-off_wt wt))
	      (/ (- (lookup_tbl (contents wt) 'dur 'att) (beg-off_wt wt))
	         (car si)))) ))
;
(defun set-phys-dur-att_wt (wt val &rest si)
    (pls-check-type 'WT wt 'set-phys-dur-att_wt)
    (let ((si (ifn si 1.0 (car si))))
	(let ((real-att (* val si)))
	    (cond
		((beyond-eof_wt wt real-att)
		 (error-eof_wt 'set-phys-dur-att_wt val (end-off_wt wt si)))
		((beyond-begof_wt wt real-att)
		 (error-begof_wt 'set-phys-dur-att_wt val (beg-off_wt wt si)))
		(t
		 (insert_tbl (contents wt) 'dur 'att real-att))))))

(defun set-dur-att_wt (wt val &rest si)
    (pls-check-type 'WT wt 'set-dur-att_wt)
    (ifn si
	(set-phys-dur-att_wt wt (+ val (beg-off_wt wt)))
	(set-phys-dur-att_wt wt (+ val (beg-off_wt wt)) (car si))))


; VAR-ATT: RETURN/SET THE RANDOM VARIATION THAT WILL BE APPLIED WHEN
;    COMPUTING THE ATTACK PORTION OF THE SOUND FILE
(defun var-att_wt (wt)
	(pls-check-type 'WT wt 'var-att_wt)
	(lookup_tbl (contents wt) 'dur 'var-att))
;
(defun set-var-att_wt (wt val)
	(pls-check-type 'WT wt 'set-var-att_wt)
	(insert_tbl (contents wt) 'dur 'var-att val))


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
(defun win-phys-from_wt (wt &rest si)
    (pls-check-type 'WT wt 'win-phys-from_wt)
    (ifn si
	(lookup_tbl (contents wt) 'win 'from)
	(/ (lookup_tbl (contents wt) 'win 'from)
	   (car si))))

(defun win-from_wt (wt &rest si)
    (pls-check-type 'WT wt 'win-from_wt)
    (ifn si
	(- (lookup_tbl (contents wt) 'win 'from)
	   (beg-off_wt wt))
	(/ (- (lookup_tbl (contents wt) 'win 'from)
	      (beg-off_wt wt))
	   (car si))))

; INTERNAL FUNCTION
(defun set-win-phys-from_wt (wt from &rest si)
    (pls-check-type 'WT wt 'set-win-phys-from_wt)
    (let ((si (ifn si 1.0 (car si))))
	(let ((real-from (* from si))
	      (real-min-win (win-min-size_wt wt)))
	    (cond
		((beyond-eof_wt wt (+ real-from real-min-win))
		 (let ((new-from (- (end-off_wt wt) real-min-win)))
		     (insert_tbl (contents wt) 'win 'from new-from)
		     wt))
;		     (/ new-from si)))
		((beyond-begof_wt wt real-from)
		 (let ((new-from (beg-off_wt wt)))
		     (insert_tbl (contents wt) 'win 'from new-from)
		     wt))
;		     (/ new-from si)))
		((beyond-eof_wt wt (+ real-from (win-size_wt wt)))
		 (let ((new-size (- (end-off_wt wt) real-from)))
		     (insert_tbl (contents wt) 'win 'from real-from)
		     (capi::beep-pane nil)
		     (print (format () "SET-WIN-PHYS-FROM_WT: WARNING ... WINDOW SIZE MODIFIED~%       OLD SIZE = ~a, NEW SIZE = ~a~%" (win-size_wt wt si) (/ new-size si)))
		     (set-win-size_wt wt new-size)
		     wt))
		(t
		 (insert_tbl (contents wt) 'win 'from real-from)
		 wt)))))

(defun set-win-from_wt (wt from &rest si)
    (pls-check-type 'WT wt 'set-win-from_wt)
    (ifn si
	(set-win-phys-from_wt wt (+ from (beg-off_wt wt)))
	(set-win-phys-from_wt wt (+ from (beg-off_wt wt)) (car si))))


;---------------------------------------------------------------------------------------

; WIN-[MIN-]SIZE: RETURN/SET THE SIZE AND THE MINIMUM SIZE OF A WINDOW
; THE MODIFIER RETURNS 'ok IF THE "SIZE" ARGUMENT HAS NOT BEEN CHANGED TO
;    ADAPT IT TO THE CONSTRAINTS IMPOSED BY THE SOUNDFILE, OTHERWISE IT
;    RETURNS THE NEW CHANGED VALUE
; PRIORITY OF THE CHANGES IS FOR WIN-FROM
; CHANGES IF SIZE IS:
;	< MIN-SIZE:		NEW SIZE = MIN-SIZE
;	> DUR:			NEW SIZE = DUR / FROM = BEG-OFF
;	> END-OFF:		NEW FROM = END-OFF - SIZE
(defun win-size_wt (wt &rest si)
    (pls-check-type 'WT wt 'win-size_wt)
    (ifn si
	(lookup_tbl (contents wt) 'win 'size)
	(/ (lookup_tbl (contents wt) 'win 'size) (car si))))
;
(defun set-win-size_wt (wt size &rest si)
    (pls-check-type 'WT wt 'set-win-size_wt)
    (let ((si (ifn si 1.0 (car si)))
	  (min-size (win-min-size_wt wt)))
	(let ((real-size (* size si)) (end (end-off_wt wt))
	      (old-from (win-phys-from_wt wt)))
	    (cond
		((< real-size min-size)
                 (when (get-gbl 'PRNFLG) (print (format () "set-win-size_wt: TOO SMALL WINDOW: ~d.. Set to WIN-MIN: ~d~%" real-size min-size))) 
		 (insert_tbl (contents wt) 'win 'size min-size)
		 wt)
;		 (/ min-size si))
		((> real-size (dur_wt wt))
		 (let ((new-size (dur_wt wt)))
		     (insert_tbl (contents wt) 'win 'size new-size)
		     wt))
;		     (/ new-size si)))
		((beyond-eof_wt wt (+ real-size old-from))
		 (let ((new-from (- end real-size)))
		    (insert_tbl (contents wt) 'win 'size real-size)
		    (capi::beep-pane nil)
		    (print (format () "SET-WIN-SIZE_WT: WARNING ... WINDOW FROM MODIFIED~%     OLD FROM = ~a, NEW FROM = ~a~%" (win-phys-from_wt wt si) (/ new-from si)))
		    (set-win-phys-from_wt wt new-from)
		    wt))
		(t
		 (insert_tbl (contents wt) 'win 'size real-size)
		 wt)))))


;
(defun win-min-size_wt (wt &rest si)
    (pls-check-type 'WT wt 'win-min-size_wt)
    (ifn si
	(lookup_tbl (contents wt) 'win 'min-size)
	(/ (lookup_tbl (contents wt) 'win 'min-size) (car si))))
;
(defun set-win-min-size_wt (wt size &rest si)
    (pls-check-type 'WT wt 'set-win-min-size_wt)
    (let ((si (ifn si 1.0 (car si))))
	(let ((real-size (* size si)))
	    (cond
		((> real-size (dur_wt wt))
		 (capi::beep-pane nil)

	 (error_wt 'set-win-min-size_wt "MIN-SIZE > DUR. KEEP IT BELOW "
			(dur_wt wt si)))
		(t
		 (insert_tbl (contents wt) 'win 'min-size real-size))))))


; SET-[PHYS-]WIN: SET A WHOLE WINDOW BY GIVING FROM AND TO
; [PHYS-]WIN: RETURN A CONS WITH FROM AND SIZE
(defun set-phys-win_wt (wt from to &rest si)
    (pls-check-type 'WT wt 'set-phys-win_wt)
    (let ((si (ifn si 1.0 (car si)))
	  (min-win (win-min-size_wt wt)))
	(let ((real-from (* from si))
	      (real-to (* to si)))
	    (cond
		((< (- real-to real-from) min-win)
		 (capi::beep-pane nil)

	 (error_wt 'set-phys-win_wt
			"TOO SMALL WINDOW. CURRENT MINIMUM SIZE IS "
			(/ min-win si)))
		((beyond-begof_wt wt real-from)
		 (terpri)
		 (print "WRONG FROM")
		 (error-begof_wt 'set-phys-win_wt from (beg-off_wt wt si)))
		((beyond-eof_wt wt real-to)
		 (terpri)
		 (print "WRONG TO")
		 (error-eof_wt 'set-phys-win_wt to (end-off_wt wt si)))
		(t
		 (set-win-size_wt wt (- to from) si)
		 (set-win-phys-from_wt wt from si))))))

(defun set-win_wt (wt from to &rest si)
    (pls-check-type 'WT wt 'set-win_wt)
    (let* ((si (ifn si 1.0 (car si))) (beg-off (beg-off_wt wt si)))
	(set-phys-win_wt wt (+ from beg-off) (+ to beg-off) si))) 

;
(defun phys-win_wt (wt &rest si)
    (pls-check-type 'WT wt 'phys-win_wt)
    (let ((si (ifn si 1.0 (car si))))
	(cons (win-phys-from_wt wt si) (win-size_wt wt si))))

(defun win_wt (wt &rest si)
    (pls-check-type 'WT wt 'win_wt)
    (let ((si (ifn si 1.0 (car si))))
	(cons (win-from_wt wt si) (win-size_wt wt si))))


; WIN-[PHYS-]TO: RETURN THE END POINT OF A WINDOW
(defun win-phys-to_wt (wt &rest si)
    (pls-check-type 'WT wt 'win-phys-to_wt)
    (ifn si
	(+ (win-phys-from_wt wt) (win-size_wt wt))
	(/ (+ (win-phys-from_wt wt) (win-size_wt wt)) (car si))))

(defun win-to_wt (wt &rest si)
    (pls-check-type 'WT wt 'win-to_wt)
    (ifn si
	(+ (win-from_wt wt) (win-size_wt wt))
	(/ (+ (win-from_wt wt) (win-size_wt wt)) (car si))))

(defun set-win-to_wt (wt from &rest si)
    (pls-check-type 'WT wt 'set-win-to_wt)
    (ifn si
	(set-win-phys-to_wt wt (+ from (beg-off_wt wt)))
	(set-win-phys-to_wt wt (+ from (beg-off_wt wt)) (car si))))

; INTERNAL FUNCTION
(defun set-win-phys-to_wt (wt from &rest si)
    (pls-check-type 'WT wt 'set-win-phys-from_wt)
    (let ((si (ifn si 1.0 (car si))))
	(let ((real-from (* from si))
	      (real-min-win (win-min-size_wt wt)))
	    (cond
		((beyond-eof_wt wt (+ real-from real-min-win))
		 (let ((new-from (- (end-off_wt wt) real-min-win)))
		     (insert_tbl (contents wt) 'win 'from new-from)
		     (/ new-from si)))
		((beyond-begof_wt wt real-from)
		 (let ((new-from (beg-off_wt wt)))
		     (insert_tbl (contents wt) 'win 'from new-from)
		     (/ new-from si)))
		((beyond-eof_wt wt (+ real-from (win-size_wt wt)))
		 (let ((new-size (- (end-off_wt wt) real-from)))
		     (insert_tbl (contents wt) 'win 'from real-from)
		     (capi::beep-pane nil)
		     (print (format () "SET-WIN-PHYS-FROM_WT: WARNING ... WINDOW SIZE MODIFIED~%"))
                     (print (format () "                        OLD SIZE = ~a, NEW SIZE = ~a~%" (win-size_wt wt si) (/ new-size si)))
		     (set-win-size_wt wt new-size)
		     'ok))
		(t
		 (insert_tbl (contents wt) 'win 'from real-from)
		 'ok)))))

; [PHYS-]ADVANCE-WIN: ADVANCE THE CURRENT WINDOW RESETTING THE FROM FIELD
;    RETURN THE NEW WINDOW OBJECT (PHYSICALLY DEFINED OR NOT)
(defun advance-win_wt (wt time &rest si)
    (pls-check-type 'WT wt 'win-advance_wt)
    (let ((si (ifn si 1.0 (car si))))
	(win-do-it_wt wt time si)
	(win_wt wt si)))

(defun phys-advance-win_wt (wt time &rest si)
    (pls-check-type 'WT wt 'win-advance_wt)
    (let ((si (ifn si 1.0 (car si))))
	(win-do-it_wt wt time si)
	(phys-win_wt wt si)))

(defun win-do-it_wt (wt time si)
    (let* ((real-time (* time si))
	  (size (win-size_wt wt)) (end (end-off_wt wt))
	  (old-from (win-phys-from_wt wt)) (new-from (- end size)))
	(if (<= (+ old-from size real-time)
		end)
	    (set-win-phys-from_wt wt (+ old-from real-time))
	    (set-win-phys-from_wt wt new-from))))



;	NAME:		phys-dur_wt / set-phys-dur_wt  (SELECTOR / MODIFIER)
;	TYPE:		Expr with 1 / 2-3 arguments
;	CALL:		(phys-dur_wt wt) / (set-phys-dur_wt wt file [dir])
;	FUNCTION:	return / set the physical duration of the sfile
;			   with optional directory (note that this is a
;			   value that belongs to the soundfile)
;	VALUE:		the above value / 'ok
;	SOURCE:		$LLpls/wt.ll

(defun phys-dur_wt (wt &rest si)
    (pls-check-type 'WT wt 'phys-dur_wt)
    (ifn si
	(lookup_tbl (contents wt) 'dur 'physical)
	(/ (lookup_tbl (contents wt) 'dur 'physical) (car si))))
;
(defun set-phys-dur_wt (wt file &rest dir)
	(pls-check-type 'WT wt 'set-_wt)
	(let ((file (ifn dir file (catenate (car dir) file))))
	   (insert_tbl wt 'dur 'physical (cdr (assoc 'dur (get-sndinfo file))) 'd)))


;	NAME:		abs-file_wt / etc.  (SELECTORS)
;	TYPE:		Expr with 1 or more arguments
;	CALL:		(abs-file_wt wt) / etc.
;	FUNCTION:	return the values computed depending on the state of
;			   the internal fields
;	VALUE:		the above values
;	SOURCE:		$LLpls/wt.ll

; ABS-FILE: RETURN THE ABSOLUTE NAME OF THE FILE
(defun abs-file_wt (wt)
    (pls-check-type 'WT wt 'abs-file_wt)
    (let ((dir (dir_wt wt)) (file (file-name_wt wt)))
	(ifn dir
	     file
	     (format () "~a~a" dir file))))


; NTH-FREQ: RETURN THE NTH FREQUENCY OF A LIST
(defun nth-freq_wt (wt pos)
  (pls-check-type 'WT wt 'nth-freq_wt)
  (let ((fq (freq_wt wt))
        (pos (if (< pos 1) 1 pos)) )
    (if (listp fq)
      (nth (1- pos) fq)
      (progn (capi::beep-pane nil)
             (error_wt 'nth-freq_wt
	               "THERE IS NO LIST OF FREQ FOR THE OBJECT ASSOCIATED TO FILE"
		       (abs-file_wt wt))))))


; FIRST/RAN/LAST-FREQ: RETURN THE FIRST, ONE RANDOMLY CHOSEN OR LAST FREQUENCY OF A LIST
;                  IF THERE IS NO LIST, RETURN THE ONLY FREQ FOUND
(defun first-freq_wt (wt)
  (pls-check-type 'WT wt 'first-freq_wt)
  (let ((fq (freq_wt wt)))
    (if (listp fq)
      (car fq)
      fq)))

(defun ran-freq_wt (wt)
  (pls-check-type 'WT wt 'ran-freq_wt)
  (let ((fq (freq_wt wt)))
    (if (listp fq)
      (choose-in-list fq)
      fq)))

(defun last-freq_wt (wt)
  (pls-check-type 'WT wt 'last-freq_wt)
  (let ((fq (freq_wt wt)))
    (if (listp fq)
      (car (last fq))
      fq)))



#| commented out, Marco, 970515
      (progn (capi::beep-pane nil)
             (error_wt 'last-freq_wt
	               "THERE IS NO LIST OF FREQ FOR THE OBJECT ASSOCIATED TO FILE"
		       (abs-file_wt wt))))))

|#

; CLOSE-FREQ: RETURN THE FREQUENCY OF A LIST THAT IS THE CLOSEST ONE TO REF-FQ
;             IF THERE IS NO FREQUENCY LIST, RETURN THE ONLY FREQ AVAILABLE
(defun close-freq_wt (wt ref-fq)
  (pls-check-type 'WT wt 'close-freq_wt)
  (let ((fq (freq_wt wt)) (curr-close-fq 0.0))
    (ifn (listp fq)
         fq
#|
         (progn
           (capi::beep-pane nil)
           
           (error_wt 'close-freq_wt 
	             "THERE IS NO LIST OF FREQ FOR THE OBJECT ASSOCIATED TO FILE"
		     (abs-file_wt wt)))

|#
      
      (loop while fq
            do (let ((curr-fq (nextl fq))
                     (curr-distance (abs (- curr-close-fq ref-fq))))
                 (when (< (abs (- curr-fq ref-fq))
                          curr-distance)
                   (setq curr-close-fq curr-fq))))
      curr-close-fq)))


; FIELD: GET THE VALUE OF A DYNAMICAL FIELD
(defun field_wt (wt fld)
  (pls-check-type 'WT wt 'set-field_wt)
  (if (is-field_wt wt fld)
    (lookup_tbl (contents wt) fld)
    (progn (capi::beep-pane nil)
	   (print (format () "          WARNING: UNEXISTENT FIELD ~a~%" fld)))))

(defun set-field_wt (wt fld val)
  (pls-check-type 'WT wt 'set-field_wt)
  (if (is-field_wt wt fld)
    (insert_tbl (contents wt) fld val)
    (progn (capi::beep-pane nil)
	   (print (format () "          WARNING: UNEXISTENT FIELD ~a~%" fld)))))


; DUR: RETURN THE PERCEPTUAL DURATION (TIME GAP BETWEEN BEG-OFF AND END-OFF)
(defun dur_wt (wt &rest si)
    (pls-check-type 'WT wt 'dur_wt)
    (ifn si
	(-  (end-off_wt wt)
	    (beg-off_wt wt))
	(/  (-  (end-off_wt wt)
		(beg-off_wt wt))
	    (car si))))


; [PHYS-]ATT: RETURN THE REAL ATTACK, IE THE ATTACK INCLUDING THE
;	RANDOM VARIATION
(defun att_wt (wt &rest si)
    (pls-check-type 'WT wt 'att_wt)
    (let ((si (ifn si 1.0 (car si))))
	(let ((real-att (+ (dur-att_wt wt) (ran (var-att_wt wt)))))
	    (cond
		((> real-att (dur_wt wt))		; BEYOND END-OFF
		 (dur_wt wt) )
		((< real-att 0.0)			; BEYOND BEG-OFF
		 0.0)
		(t
		 (/ real-att si))))))

;
(defun phys-att_wt (wt &rest si)
   (ifn si
	(+ (att_wt wt) (beg-off_wt wt))
	(+ (att_wt wt (car si)) (beg-off_wt wt (car si)))))



; SNDINFO: NUMBER OF CHANNELS, SAMPLING RATE, PACKING MODE (2 = SHORT,
;    4 = LONG), NUMBER OF SAMPLES
(defun n-ch_wt (wt)
    (pls-check-type 'WT wt 'n-ch_wt)
    (lookup_tbl (contents wt) 'sf 'nchan))

(defun sr_wt (wt)
    (pls-check-type 'WT wt 'sr_wt)
    (lookup_tbl (contents wt) 'sf 'sr))

(defun pk-mode_wt (wt)
    (pls-check-type 'WT wt 'pk-mode_wt)
    (lookup_tbl (contents wt) 'sf 'pk-mode))

(defun n-smpls_wt (wt)
    (pls-check-type 'WT wt 'n-smpls_wt)
    (lookup_tbl (contents wt) 'sf 'n-smpls))


; SI: RETURN THE SAMPLE INCREMENT NEEDED TO OBTAIN A FREQUENCY OF OUT-FQ
;   IF THE FREQUENCY THAT IS STORED IN THE SOUND FILE IS REF-FQ
(defun si_wt (out-fq ref-fq)
    (/ out-fq ref-fq))


;	NAME:		is_wt / is-si_wt / is-field_wt (PREDICATES)
;	TYPE:		Expr with 1 / 1/ 2 arguments
;	CALL:		(is_wt wt) / (is-si_wt wt) / (is-field_wt wt field)
;	FUNCTION:	test whether the argument is a structure of type WT
;			test whether the freq field is a frequency in Hz or a
;			   sample increment (1.0 = same speed as input)
;			test whether dynamic field "field" exists in WT
;	VALUE:		t or nil according to the test
;	SOURCE:		$LLpls/tbl.ll

(defun is_wt (wt)
  (when (is-tagged wt)
   (eq (pls-type wt) 'WT)))

(defun is-si_wt (wt)
    (pls-check-type 'WT wt 'is-si_wt)
    (eq (lookup_tbl (contents wt) 'freq 'mode) 'si) )

(defun is-field_wt (wt fld)
    (pls-check-type 'WT wt 'is-field_wt)
    (is-key_tbl (contents wt) fld))


;	NAME:		print_/short-print_wt  (INFO)
;	TYPE:		Expr with 1 argument
;	CALL:		(print_wt wt)
;			(short-print_wt wt)
;	FUNCTION:	nicely or shortely print a structure of type TBL
;	VALUE:		the string 'done
;	SOURCE:		$LLpls/wt.ll

(defun print_wt (wt)
    (pls-check-type 'WT wt 'print_wt)
    (prin "   Structure of type : " (pls-type wt) " made of : ")
    (prnt (contents wt)))

(defun short-print_wt (wt)
   (prin (pls-type wt) " / ")
   (short-prnt (contents wt)))


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

|#

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




;-----------------------------------------------------------------------------
; USEFUL INTERNAL FUNCTIONS
;-----------------------------------------------------------------------------
(defun beyond-eof_wt (wt time)
    (> time (end-off_wt wt)))

(defun beyond-phys-eof_wt (wt time)
    (> time (phys-dur_wt wt)))

(defun error-eof_wt (fun val dur)
   (capi::beep-pane nil)
   (error (format () "~a: THE TIME ~d IS BEYOND THE END OF THE FILE. PLEASE, KEEP IT BELOW ~d." fun val dur) ))

;-----------------------------------------------------------------------------
(defun beyond-begof_wt (wt time)
    (< time (beg-off_wt wt)))

(defun beyond-phys-begof_wt (wt time)
  (declare (ignore wt))
;  (phys-dur_wt wt) ; THIS INSTRUCTION IS USELESS, BUT ELIMINATES A COMPILER'S WARNING
  (< time 0.0))

;-----------------------------------------------------------------------------
(defun error-begof_wt (fun val dur)
   (capi::beep-pane nil)
   (error (format () "~a: THE TIME ~d IS BEYOND THE BEGINNING OF THE FILE. PLEASE, KEEP IT ABOVE ~d." fun val dur) ))

; DEPRECATED
;(defun error_wt (fun msg val)
;   (error msg val fun))
;-----------------------------------------------------------------------------
