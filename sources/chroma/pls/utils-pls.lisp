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
;	cassq
;	catenate/concat
;       choose-in-list
;	error-type
;	pls-check-type
;	prin
;	pwr2
;	rplac
;       beep
;-----------------------------------------------------------------------------

; (attach el l)
(defun attach (s l)
"Physically attach element <el> on top of list <l>
NOTE: <l> cannot be empty"

    (rplac l s (cons (car l) (cdr l))))

;-----------------------------------------------------------------------------
; (pls-check-type typ str fun-name)

(defun pls-check-type (typ str fun-name)
"Check that structure <str> is of type <typ>.
<fun-name> is the name of the function calling check-type.
"
;	(unless (funcall (read-from-string (concatenate 'string "is_" (string-downcase typ))) str)
; CORRECTED BY CARLOS, 000823, TO AVOID PACKAGE CONFLICTS WITH SYMBOLS
	(unless (funcall (intern (concatenate 'string "IS_" (string typ)) :chroma) str)
		(error-type fun-name str))
	'ok)

;-----------------------------------------------------------------------------
; (error-type f l)
(defun error-type (f l)
"
Signal an error of type for function f"

(error "Wrong type for structure ~%~a~% in function ~a~%" l f))

;-----------------------------------------------------------------------------
; (choose-in-list l)
;
(defun choose-in-list (list)
  "Random selection in list"
  (nth (random (length list))list))


(defun closest-pwr2 (val)
  "Return the closest larger power of two) of val, ex. 3.4 --> 4. Useful for csound audio tables."
  (let ((size 2))
    (loop while (> val size) do
        (setf size (* size 2)))
    size))


;-----------------------------------------------------------------------------
;pwr2, same as closest-pwr2
(defun pwr2 (n)
"RETURN THE POWER OF TWO IMMEDIATELY > n"
  (closest-pwr2 n))

;  (let ((exp 0))
;    (loop while (< (expt 2 (incf exp)) n) )
;    (expt 2 exp)) )



;-----------------------------------------------------------------------------
;rplac
(defun rplac (l s1 s2)
  (if (atom l)
    (error "The first argument cannot be an atom: ~a" l)
    (progn (rplaca l s1)
           (rplacd l s2)
           l)))

;-----------------------------------------------------------------------------
;cassq - already defined in utils-cr.lisp
;(defun cassq (sym l)
;  (cdr (assoc sym l)))

;-----------------------------------------------------------------------------
;prin
(defun prin (&rest args)
  (cond 
   ((null args) ())
   (t (princ (car args) (get-gbl '*chroma-output*))
      (prin_ (cdr args)))))

(defun prin_ (list-args)
  (cond 
   ((null list-args)())
   (t
    (princ (car list-args) (get-gbl '*chroma-output*))
    (prin_ (cdr list-args)))))
;-----------------------------------------------------------------------------
;catenate/concat
;new, generalised, 1907, ms
(defun catenate (x &rest y)
"String any number of args"
(apply #'concatenate (cons 'string (cons (format () "~s" x) (mapcar #'(lambda (x) (format () "~s" x)) y)) )))

; does not work any longer... 1801, ms
;(defun concat (&rest lpname) 
;  (coerce (mapcan 'pname lpname) 'symbol))
;-----------------------------------------------------------------------------
;beep
(defun beep ()
   (capi::beep-pane nil))
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
; error-synth
(defun error-synth (function synth)
"Print an error message if the wrong synthesizer is selected
"
  (error  "HEY, WHAT A CURIOUS SYNTHI YOU WANNA ME TO PLAY ...!~%COME ON, ~a!~%       in function ~a"
          synth function))

;-----------------------------------------------------------------------------
; build-cwt
; (build-cwt L-CWT)
;  EX: (build-cwt wt-a1 (wt-a2 0 1) (wt-a3 (att_wt wt-a3) (dur_wt wt-a3)))
(defun build-cwt (&rest l-cwt)
"B(uild) a CWT field for CTL1 level (shortcut)
where L-CWT: any wt objects (just the name if alone, the list (name val1 val2) if start and end offs are set)
"
    (mapcar (lambda (wt)
		(if (symbolp wt)
		    `'(list ',wt)
		    `'(list ',(nextl wt) ,.wt)) )
	    l-cwt) )

;-----------------------------------------------------------------------------
; build-fqwt
; (build-fqwt L-FQ)
;  EX: (build-fqwt (1.0) (1.059 (nth-freq_wt wt-a2 2)) (440 261))
(defun build-fqwt (&rest l-fqwt)
"B(uild) a FQ field for CTL1 level WTsys (shortcut).
where L-FQ: list of (out-fq [ref-fq]) by default, ref-fq = 1.0.
"
    (mapcar (lambda (fq)
		(let ((out-fq (nextl fq))
		      (ref-fq (ifn fq 1.0 (nextl fq))) )
		    `'(list ,out-fq ,ref-fq)) )
	    l-fqwt) )



;-----------------------------------------------------------------------------
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

(defun load-wt (name &optional (subdirs nil))
"Load a data base of WT objects from a fixed directory specified in the environment.

<name>: basic name of the file (string or symbol) or list of names
<subdir> [nil]: list of subdirectories (strings)

The file will be looked for in the directory specified by (getenv 'LLwt) and
   is called <name>_WT.lisp"

  (cond
   ((null name) 'done)
   ((stringp name)    
      (let* ((wt-dir (get-cr-path :wt :subdirs subdirs))
              (wt-file (merge-pathnames wt-dir (format () "~a_WT.lisp" name))))
      (format t "Loading ~a~%" wt-file)
      (load wt-file)) )
   ((symbolp name) ; FOR COMPATIBILITY WITH OLD VERSIONS
    (load-wt (string name) subdirs))
   ((listp name)
      (load-wt (car name))
      (load-wt (cdr name)))
   (t (error "ILLEGAL ARGUMENT: ~a" name))))

;-----------------------------------------------------------------------------
; modified by Marco, 020325
(defun load-comp-wt (name &optional (subdirs nil))
"Load a data base of compiled WT objects from a fixed directory.

<name>: basic name of the file (string or symbol) or list of names
<subdir>: subdirectories (string)

The file will be looked for in the directory specified by (getenv 'LLwt) and
   is called <name>_CP-wt.lisp

TO SAVE A COMPILED VERSION OF A DATA BASE OF WT objects TYPE:
      (save-wt <name> 'WT-OBJ-1 ... 'WT-OBJ-N) 
      THE REAL NAME OF COMPILED VERSION IS: <name>_CP-wt.ll
"
  (cond
   ((null name) 'done)
   ((stringp name)    
      (let* ((wt-dir (get-cr-path :wt :subdirs subdirs))
             (wt-file (merge-pathnames wt-dir (format () "~a_CP-wt.lisp" name))))
      (format t "Loading ~a~%" wt-file)
      (load wt-file)) )
   ((symbolp name) ; FOR COMPATIBILITY WITH OLD VERSIONS
    (load-comp-wt (string name) subdirs))
   ((listp name)
      (load-comp-wt (car name))
      (load-comp-wt (cdr name)))
   (t (error "ILLEGAL ARGUMENT: ~a" name))))


;-----------------------------------------------------------------------------
; modified by Marco, 020325
(defun save-wt (file &rest objs)
"Compile the WT-objects onto a file named <name>_CP-wt.lisp in the directory
specified by the environment variable LLwt"
    (let ((old-dir (get-gbl 'OUTDIR))	; SAVE PREVIOUS STATE
	  (old-ext (get-gbl 'EXT))
	  (curr-dir (get-cr-path :wt))
	  (curr-file (format () "~a_CP-wt" file)))

	(set-gbl 'OUTDIR curr-dir)
	(set-gbl 'EXT "lisp")

	(format t "COMPILING WT OBJECTS~%")
	(open curr-file :direction :output)	; OPEN WITH MARGIN = 500
        (format (get-gbl '*chroma-output*) "(in-package cr)~%")
	(mapc (lambda (obj)
                (let* ((name (second obj))
                       (val (eval name))) 
		  (format t "; Writing ~a~%" name)
		  (format (get-gbl '*chroma-output*) "; ~a~%" name)
		  (format (get-gbl '*chroma-output*) "(setf ~a '~S )~%" name val)
		  (format (get-gbl '*chroma-output*) "~%")))
	        objs)
	(close curr-file)

	
	(set-gbl 'OUTDIR old-dir)	; RESTORE PREVIOUS STATE
	(set-gbl 'EXT old-ext))
    'done)

;-----------------------------------------------------------------------------
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


;(multiple-value-bind (buffer format n-channels sample-rate sample-size size skip)  (sound-file-get-info (choose-file-dialog))
;  (list buffer format n-channels sample-rate sample-size size skip))
;("AIFF(int)" 2 48000 24 11771540 1 nil)
;("AIFF(float)" 1 96000 32 96000 1 nil)
;("Wav(int)" 1 48000 24 100878986 1 nil)

(defun read-sound-format (format)
; separate the buffer+format string coming from om-get-sound-info and return the two values separately (type and size)
; not very elegant...:-(!!
; if none are found, return the whole format
  (let* ((buf (subseq format 0 1))
         (smpltype (if (string-equal buf "A") (subseq format 5 6) (subseq format 4 5))))
;AIFF=4 characters, Wav=3 characters
    (list (cond ((string-equal buf "A") "aif")
                ((string-equal buf "W") "wav")
                (t format))
          (cond ((string-equal smpltype "f") "float")
                ((string-equal smpltype "i") "int")
          (t format)))))


; new version, using libsndfile, 1801, ms
(defun get-sndinfo (&rest file-in)
;(defun get-sndinfo (file-in)
  (let* ((l-results
         (multiple-value-bind (buffer format n-channels sample-rate sample-size size skip)
             (ifn file-in (sound-file-get-info (choose-file-dialog "Choose an audio file"))
               (sound-file-get-info file-in))
           (list buffer format n-channels sample-rate sample-size size skip)))
         
         (sr (third l-results))
         (n-ch (second l-results))
         (frmt (first l-results))
         (n-smpl (fifth l-results))
         (smpl-size (fourth l-results))
         (pk-mode (second (read-sound-format frmt)))
         (snd-type (first (read-sound-format frmt)))
         (dur (/ n-smpl sr n-ch 1.0)))
    (list (cons 'file file-in)
          (cons 'sr sr)
          (cons 'dur dur)
          (cons 'n-ch n-ch)
          (cons 'pk-mode pk-mode)
          (cons 'smpl-type pk-mode)
          (cons 'smpl-size smpl-size)
          (cons 'n-smpl n-smpl)
          (cons 'snd-type snd-type))
    ))

; (with-open-file  (stream  file-in :direction :input)
;    (if(not(equal  (get-id stream) "FORM"))
;      (error "Unrecognized AIFF file FORM")
;      (progn (get-long stream)               ; chunkSize
;             (if(not(equal  (get-id stream) "AIFF"))
;               (error "Unrecognized AIFF file AIFF")
;               (if(not(equal  (get-id stream) "COMM"))
;                 (error "Unrecognized AIFF file COMM")
;                 (let ((sr) (dur) (n-ch) (pk-mode) (n-smpl))
;                   (get-long stream)           ; chunkSize
;                   (setf n-ch (get-integer stream))
;                   (setf n-smpl (get-long stream))
;                   (setf pk-mode (get-integer stream))         ; en fait SampleSize
;                   (setf sr (get-double stream))
;                   (setf dur (float (/ n-smpl n-ch sr)))
;                   (list (cons 'file file-in)
;                         (cons 'SR sr)
;                         (cons 'dur dur)
;                         (cons 'n-ch n-ch)
;                         (cons 'pk-mode pk-mode)
;                         (cons 'n-smpl n-smpl)
;                         ))))))))

;(get-sndinfo (om:choose-file-dialog) )

;(defun get-integer (stream)
;(decs-to-bignum (list (char-int(read-char stream))(char-int(read-char stream) ))))

;(defun get-long (stream)
;(decs-to-bignum (list (char-int(read-char stream))
;                      (char-int(read-char stream))
;                      (char-int(read-char stream))
;                      (char-int(read-char stream) ))))

;(defun get-id (stream)
;  (format nil "~a~a~a~a"  (read-char stream)
;          (read-char stream)
;          (read-char stream)
;          (read-char stream) ))

;(defun get-double (stream)
;  (let*((int1 (get-integer stream))
;        (int2 (get-integer stream)))
;    (*(expt 2 (- int1 16398))int2)))

;from mikhail malt:
;(defun decs-to-bignum (liste)
;"conversion d'une liste d'entiers en un bignum.
;Le premier élément est le plus representatif et le dernier le moins"
;(let ((long (1- (length liste))))
;  (cond
;   ((null liste) 0)
;   (t (+ (ash (car liste) (* long 8))
;         (decs-to-bignum (rest liste)))))))


#|
;-----------------------------------------------------------------------------
;auxiliary functions, for debugging purposes (OBSOLETE NOW)
(defun read-aiff-header (file-in)
    (with-open-file (stream  file-in :direction :input)
      (if (not(equal (get-id stream) "FORM"))
        (error "unrecognized AIFF file")
          (progn
            (format t "chunkSize-~a-~%"(get-long stream)) 
            (format t "-~a-~%"(get-id  stream))
            (format t "-~a-~%"(get-id stream))
            (format t "chunkSize-~a-~%"(get-long stream))
            (format t "numChans~a~%"(get-integer stream))
            (format t "numSampleFrames-~a-~%"(get-long stream))
            (format t "SampleSize~a~%"(get-integer stream))
            (format t "SampleRate-~a-~%"(get-double stream))
            ))))

;(read-aiff-header (om::om-choose-file-dialog) )
|#


(defun get-bin-double (stream)
  (format nil "~a~a~a~a"  (dec-to-bin (char-int (read-char stream))8)
          (dec-to-bin(char-int(read-char stream))8)
          (dec-to-bin(char-int(read-char stream))8)
          (dec-to-bin(char-int(read-char stream) )8)))

(defun dec-to-nib (x)
  (cond((equal x 0)())
       (t(multiple-value-bind (div rem)(floor x 2)
           (cons rem (dec-to-nib div))))))

(defun dec-to-bin (x  size )
  (let ((result (reverse (dec-to-nib x))))
    (replace (make-list size :initial-element 0)
             result :start1 (- size (length result)))))

;-----------------------------------------------------------------------------
; WRITE A FILE CONTAING THE CORRECT INSTRUCTION TO LOAD THE WAVE TABLES
; THE LIST OF THE WT OBJECTS TO WRITE DOWN IS INN THE GLOBAL "WTL"
(defun out-wt (file-name &rest dir)
   (let ((synthi (get-gbl 'SYNTH)) )
	(case synthi
	    (csound
		(out-wt-cs file-name (car dir)) )
;	    (moon
;		(out-wt-moon file-name (car dir)) )
	    (t
		(error-synth 'out-wt synthi)))) )

;-----------------------------------------------------------------------------
(defun out-wt-cs (file-name &optional (dir (get-gbl 'CSfun)))
"
New version ONLY working with omChroma.
Older versions are below (grayed).
filename: name of file, without extension.
          the file will have the automatic extensions <.fun>.
dir: directory where the file is to be written (default: value of CSfun).
"  

  (unless (equal (get-gbl 'MACHINE) 'om)
    (error "~%For the time being I can only work with om !
    Why did you give me this machine, eh: ~a!~%" (get-gbl 'MACHINE)))

  (let ((complete-filename (merge-pathnames dir (format () "~a.fun" file-name)))
        (wt-list (get-gbl 'WTL))
        )
    (when wt-list
      (format t "~%WRITING WT TABLES FOR CSOUND IN~%   ~a~%" complete-filename)
      (format t "   ON ")
      (om::printdate)
      (format t "~%~%")
      (with-open-file (out-stream complete-filename :direction :output 
                                :if-does-not-exist :create 
                                :if-exists :supersede)
        (format out-stream "~%;  WRITING WT TABLES FOR CSOUND ON ")
        (printdate out-stream)
        (format out-stream "~%~%")

        (mapc (lambda (wt)
                (let* ((name (car wt))
                       (dir (cadr wt))
                       (f-num (caddr wt))
                       (n-smpls (cadddr wt))
                       (f-size (1+ (pwr2 n-smpls)))
                       (sndfilein (merge-pathnames dir name))
                       )
                     
                     (format t "    f~a 0 ~a 1 ~%" f-num f-size)
                     (format t "        \"~a\" 0 4 1~%" sndfilein )
                     (when (> f-num (get-gbl 'WTFOMAX))
                       (format t "            WARNING: function number > upper limit of ~a~%" (get-gbl 'WTFOMAX)))
                     (format out-stream ";  Number of samples: ~a~%" n-smpls)
                     (format out-stream "(om::ScSt \"f ~a 0 ~a 1 " f-num f-size)
                     (format out-stream "\\\"~a\\\" 0 4 1\")~%"sndfilein ))) wt-list))))
  "done")
;-----------------------------------------------------------------------------

