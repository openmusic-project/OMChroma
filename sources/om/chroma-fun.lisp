;******************************************************************
;-------| CHROMA SYSTEM
;-------| This file is: $LLsys/utils-cr.lisp
;-------| Implemented by Marco Stroppa
;-------| Version: 000817, updated 050221, 060814
;******************************************************************

(in-package :cr)

;------------------------------------------------------------------
; FUNCTIONS (in alphabetical order):
;	bpf->fun
;	fun->bpf
;	fun->gen-cs-table
;	csgen->fun
;------------------------------------------------------------------



;------------------------------------------------------------------
; fun->bpf

;	NAME:		fun->bpf  (CONSTRUCTOR)
;	TYPE:		Expr with 1 or 2 arguments
;	CALL:		(fun->bpf fun [precision])
;	FUNCTION:	return a bpf object of a given precision (default 0)
;	VALUE:		the bpf
;	SOURCE:		$LLsys/utils.lisp

(defun fun->bpf (fun &optional (precision 5))
"POLYMORPHIC FUNCTION:
   Convert a CHROMA FUN object into a BPF with optional precision.
   If a list of FUNs is given, return a list of BPFs with the same optional precision."
  (if (is_fun fun)
    (om::simple-bpf-from-list (x-list_fun fun) (y-list_fun fun) 'om::bpf precision)
    (fun-list->bpf fun precision)))

(defun fun-list->bpf (fun-list prec)
  (loop for f in fun-list
        collect (fun->bpf f prec)))

; (fun->bpf (make_fun '(0 0 1 1 .5 2)) 3)
; (fun->bpf (list (make_fun '(0 0 1 1 .5 2)) (make_fun '(0 0 1 1 .5 2 1 3))))

;------------------------------------------------------------------
; bpf->fun
;new, ms_1109
;	NAME:		bpf->fun  (CONSTRUCTOR)
;	TYPE:		Expr with 1 argument
;	CALL:		(bpf->fun bpf)
;	FUNCTION:	return a CHROMA FUN object
;	VALUE:		the FUN
;	SOURCE:		$LLsys/utils-cr.lisp

(defun bpf->fun (bpf &optional (precision 5))
  "POLYMORPHIC FUNCTION:
   Convert a BPF into a CHROMA FUN object.
   If a list of BPFs is given, return a list of FUNs."
  
  (if (listp bpf)
      
      (loop for f in bpf collect (bpf->fun f precision))
    
    (make_fun
     (loop for x in (om::x-points bpf) for y in (om::y-points bpf) append (list y x))
     )
    ))


;(bpf->fun (om::simple-bpf-from-list '(0 10.002342 20 30 40) '(0 100 50 200 0) 'om::bpf 3))

;(bpf->fun (list (om::simple-bpf-from-list '(0 10.002342 20 30 40) '(0 100 50 200 0) 'om::bpf 3)
;                (om::simple-bpf-from-list '(0 10 20) '(0 100 0) 'om::bpf 3)))

;------------------------------------------------------------------
; fun->cs-gen-table

;	NAME:		fun->gen-cs-table  (CONSTRUCTOR)
;	TYPE:		Expr with 1 or 2 arguments
;	CALL:		(fun->gen-cs-table fun gentype)
;	FUNCTION:	return a gen-cs-table object of a given type
;	VALUE:		the object
;	SOURCE:		$LLsys/utils.lisp

(defun fun->gen-cs-table (fun &optional (gentyp 7) &key (gensize (get-gbl 'DEF-GEN-SIZE)) (expzero (get-gbl 'EXPZERO)))
  "POLYMORPHIC FUNCTION:
   Convert a CHROMA FUN object into a GEN-CS-TABLE of type <gentyp>.
   In case of GEN 5 (exponential bpf), replace 0 by expzero.
   If a list of FUNs is given, return a list of objects."
  
  (if (is_fun fun)
      (make-instance 'om::gen-cs-table
                     :size gensize
                     :gen-num gentyp
                     :param-list (fun-points fun gensize))
    (fun-list->gen-cs-table fun gentyp)
    ))


(defun fun-list->gen-cs-table (fun-list gentyp)
  (loop for f in fun-list
        collect (fun->gen-cs-table f gentyp)))

(defmethod fun-points ((fun list) (size integer))
   (let* ((pointx (x-list_fun fun))
          (pointy (y-list_fun fun)))
     (setf pointx (cdr (mapcar 'round (om::om-scale pointx 0 (- size 1)))))
     
     (append (loop for y in pointy
                   for last = 0 then x
                   for x in pointx
                   append (list y (- x last))) (last pointy)) ))

(defun sum-xpts (funpoints)
  "(sum-xpts (fun-points fun size)) should give size-1 if the conversion from
bpf to incremental x-points is correct.
"
  (apply '+ (car (om::list-modulo (cdr funpoints) 2))))


;(setf aaa (fun->gen-cs-table (make_fun '(0 0 100 1000)) 5))
;(sum-xpts (fun-points (make_fun '(0 0 100 1000000 11 29999999)) 513))
;(om::cs-table-string (fun->gen-cs-table (make_fun '(0 0 1 1)) 5) )

; (fun->gen-cs-table (make_fun '(0 0 1 1 .5 2)))
; (fun->gen-cs-table (list (make_fun '(0 0 1 1 .5 2)) (make_fun '(0 0 1 1 .5 2 1 3))))
;------------------------------------------------------------------


;========================================
; CREATE A GEN-CS-TABLE FROM VE
;========================================
;====================
; auxiliary functions
;====================
(defun zero-exp (fun zeroexp)
  "Replace 0 with very small values when using the exponential GEN 5"
  (let ((y (y-list_fun fun)))
    (if (member 0 y :test '=)
        (make_fun (om::flat
                   (om::mat-trans (list (replace-zeros y zeroexp) (x-list_fun fun)))))
      fun)))

(defun replace-zeros (list zeroexp)
  "Replace zeros with very small values when using exponential GENs"
  (loop for y in list
       collect (if (= y 0) zeroexp y)))


(defun cs-fun-points (fun size)
  "Prepare the points in the format of a GEN table. From OM, cs-bpf-points
   "
   (let* ((pointx (x-list_fun fun))
          (pointy (y-list_fun fun)))
     (setf pointx (cdr (mapcar 'round (om::om-scale pointx 0 (- size 1)))))
     
     (append (loop for y in pointy
                   for last = 0 then x
                   for x in pointx
                   append (list y (- x last))) (last pointy)) ))


(defun syn-fun (fun synth gentype size zeroexp)
  "Turns a fun into a OMChroma compatible, synth-dependent table.
  "
;(print synth) (print gentype) (print size) (print zeroexp)
  (case synth
    (csound
     (let ((pts (if (= gentype 5)
                    (cs-fun-points (zero-exp fun zeroexp) size)
                  (cs-fun-points fun size))))
       (make-instance 'om::gen-cs-table
                      :id "?"
                      :size size
                      :stime 0
                      :gen-num gentype
                      :param-list pts)))
    (t
     (error-synth 's_ve synth))))


(defun s_ve (ve &key (gentype 7) (synth 'csound) (size (get-gbl 'DEF-GEN-SIZE)) (zeroexp (get-gbl 'EXPZERO)))
  "
;	NAME:		s_ve (SELECTOR)
;	TYPE:		Expr with 1 argument and 4 keys
;	CALL:		(s_ve ve :gentype 7 :synth 'csound :size 513 :zeroexp 0.00001)
;	FUNCTION:	return the format of the function envelope appropriate
;			   to each known synthesizer
;			if synth is not specified, the current synthesizer's
;			   name is used
;                       if the structure is not of type VE, it is a dynamic
;                          function -> convert the structure into a cs-gen-table
;	VALUE:		the new data
"
  (case (pls-type ve)
    
    (VE
     (if (num_ve ve)
         (num_ve ve)
       (syn-fun (fun_ve ve) synth gentype size zeroexp)))
    
    (FUN
     (syn-fun ve synth gentype size zeroexp))
    
    (otherwise
     (error "Dunno what I'm gonna do with this: ~a, Sir ~a" ve (get-gbl 'USER)))
    ))



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
      (printdate)
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
                     (format out-stream "\\\"~a\\\" 0 4 1\")~%" sndfilein ))) wt-list))))
  "done")

;; better ?:
;; (format nil "~A" `(om::ScSt ,(format nil "f ~a 0 ~a 1 \"~a\" 0 4 1" f-num f-siz sndfilein)))

;-----------------------------------------------------------------------------


;*****************************************************************************
; Convert a csound GEN 5 or 7 (string) into a Chroma FUN object
; If it is a GEN 5 and the Y value =< EXPZERO, replace it with 0
(defun csgen->fun (csgen)
  (let* ((str (string-to-list csgen))
         (type (fourth str)))
    (if (or (= type 7) (= type 5))
        (make-crfun (fifth str) (cdr (cddddr str)) (third str) type)
    (progn (print (format () "I can only use GEN 5 or 7. You gave me a GEN ~a~%" type))
      str))))

;-------------------------------------------------------------------
(defun make-crfun (begval othervals gensize type)
  (let ((l-vals (list (dezeroexp begval type) 0.0)) ; first pair, y1 x1
        (numels (1- (length othervals)))
        (cnt 0) ; X
        (result ()))
    (make_fun
     (append l-vals
             (loop for i from 0 to numels by 2 do
                   append (list (dezeroexp (nth (1+ i) othervals) type) (setf cnt (+ cnt (nth i othervals)))))))))

(defun dezeroexp (val type)
      (if (and (= type 5) (<= val (cr::get-gbl 'cr::EXPZERO)))
          0.0
        val))
;
#|
(dezeroexp 12 5)
(dezeroexp 0.0001 5)
(dezeroexp 0.00001 5)
(dezeroexp 0.00001 7)
(streamp "f11 0 513 7  -1 513 0")
(stringp "f11 0 513 7  -1 513 0")
(listp '(1 2 3))
(consp "f11 0 513 7  -1 513 0")
(make-string-input-stream "f11 0 513 7  -1 513 0")
(defun test-gen2fun ()
  '(
   "f10 0 4097 7  -1 2048 0 2048 1"
   "f11 0 513 7  -1 256 0 256 1"
   "f12 0 513 7   1 512 0"
   "f13 0 513 7   0 512 1"
   "f14 0 513 5   0 512 1"
   "f15 0 513 1   0 512 1"
   "f16 0 513 7   0 512 -1"
   "f17 0 513 7  0.00001 256 1   128 0.001  64 1  64 0.00001"
   "f18 0 513 5  0.00001 256 1   128 0.001  64 1  64 0.00001"
   ) )
(csgen->fun (nth 7 (test-gen2fun))  )
(csgen->fun (nth 8 (test-gen2fun))  )
(first (string-to-list "f11 0 7 513 0 2 3 4 5"))
(fun->gen-cs-table (make_fun '(-1 0  0 2048  1 4097)) )
(fun-points (make_fun '(-1 0  0 2048  1 4096)) 4097)
|#

;-------------------------------------------------------------------
; if str is a string, convert it to a list, otherwise do nothing
(defun string-to-list (str)
  (cond ((consp str)
         str)
        ((stringp str)
         (internal-string-to-list str))
        (t (error "Need a string, please: ~a:" str))))

(defun internal-string-to-list (str)
         (if (not (streamp str))
             (internal-string-to-list (make-string-input-stream str))
           (if (listen str)
               (cons (read str) (internal-string-to-list str))
             nil)))
;*****************************************************************************
; transform a csgen into a Virtual Envelope
; if gennum is given, use this number as the VE num,
;   otherwise, derive it from the strinf fN (where N is the function number)
(defun csgen->ve (csgen &optional gennum)
  (if gennum
      (make_ve (csgen->fun csgen) gennum)
    (make_ve (csgen->fun csgen) (getnum csgen))))

(defun getnum (csgen)
  (read-from-string (subseq csgen 1)))

#|
(defun test-gen2fun ()
  '(
   "f10 0 4097 7  -1 2048 0 2048 1"
   "f11 0 513 7  -1 256 0 256 1"
   "f12 0 513 7   1 512 0"
   "f13 0 513 7   0 512 1"
   "f14 0 513 5   0 512 1"
   "f15 0 513 1   0 512 1"
   "f16 0 513 7   0 512 -1") )
(csgen->ve (nth 0 (test-gen2fun))  )
(csgen->ve (nth 0 (test-gen2fun)) 111 )
|#
;*********************************************************************


