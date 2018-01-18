(in-package :om)

;;; PARSING CSOUND INSTRUMENTS

;=========================
; GENERAL PARSING TOOLS
;=========================

;;; return T if char is a delimitator char
(defun delimator-char-p (char)
  (or (char-equal char #\Space) 
      (char-equal char #\Newline) (char-equal char #\Tab) (char-equal char #\,) (char-equal char #\() (char-equal char #\))
      (char-equal char #\*) (char-equal char #\+) (char-equal char #\-) (char-equal char #\/) (char-equal char #\=)))

;(defun delete-equal (string)
;  (let ((pos (position-if #'(lambda (x) (not (equal x #\=))) string)))
;    (when pos
;      (subseq string pos))))
     
;; shift FILE pointer to next line beginning with NAME
(defun look-for-line (file name)
   (let ((rep nil) 
         (size (length name)))
     (loop while (and (not rep) (not (stream-eofp file))) do
           (let* ((newl (delete-spaces (read-line file))))
             (if (and (>= (length newl) size) (search name newl :start2 0 :end2 (length name)))
                 (setf rep newl))))
     rep))


;;;------------
;;; ORC MACROS
;;;------------
;;; returns all macros
;(defun search-macro-lines (file)
;   (let* ((name "#define")
;          (rep nil) 
;          (size (length name)))
;     (loop while (not (stream-eofp file)) do
;           (let* ((newl (delete-spaces (read-line file))))
;             ;(print newl)
;             (when (and (>= (length newl) size) (search name newl :start2 0 :end2 (length name)))
;               ; debut de macro
;               (let ((onemac "")
;                     (cpt 0) (pos 7))
;                 (loop while (and (< cpt 2) (not (stream-eofp file))) do
;                       (setf pos (search "#" newl :start2 (+ pos 1)))
;                       (if pos (setf cpt (+ cpt 1))
;                           (setf onemac (string+ onemac (format nil "~D~%" newl))
;                                 newl (delete-spaces (read-line file))
;                                 pos 0)))
;                 (setf onemac (string+ onemac (subseq newl 0 (+ pos 1))))
;                 (push onemac rep)))))
;     rep))


;;;=====================================================
; NEW CSOUND HEADER PARSER (ALL IN ONE)
; returns (NEXT-INSTR-NUMBER GLOBALS NCHANNELS MACROS)

;;; finds channel number if appropriate
(defun orc-channel-line (string)
  (let ((substring (delete-spaces string))
        rep)
    (when substring
      (if (and (> (length substring) 6) (search "nchnls" substring :start2 0 :end2 7))
        (setf rep (read-from-string (delete-spaces (subseq (delete-spaces (subseq substring 6)) 1))))
        ))
    rep))

;; T if line is to be ignored
(defun orc-ignored-line (string)
  (let ((substring (delete-spaces string)) rep)
    (if (and substring (> (length substring) 0))
        (let* ((index (position-if #'delimator-char-p substring))
               (name (subseq substring 0 index)))
          (or (equal ";" (subseq name 0 1))
              (string-equal name "sr")
              (string-equal name "kr")
              (string-equal name "ksmps")))
          t ; ignore
          )))

; default : accepts all valid global form
; fileptr : sometimes global can be of several lines
; global = (nom body)
(defun orc-global-line (string fileptr)
  (let ((substring (delete-spaces string))
        rep)
    (when substring
      (let ((index (position-if #'delimator-char-p substring))
            name value)
        (when index
          (setf name (subseq substring 0 index))
          (setf value (om-correct-line (subseq substring index)))
          (loop while (equal #\\ (elt value (- (length value) 1))) do
                (setf value (string+ (subseq value 0 (- (length value) 1)) " " (om-correct-line (delete-spaces (read-line fileptr))))))
          (setf rep (list name value))
          )))
    rep))

; macro: #define...
; fileptr : sometimes global can be of several lines
; macro = (nom params-or-nil body)
(defun orc-macro-line (string fileptr) 
  (let* ((searchname "#define")
         (size (length searchname))
         (substring (delete-spaces string))
         rep)
    (when (and (>= (length substring) size) (search searchname substring :start2 0 :end2 size))
        ; debut de macro
      (let (macro name args body p)
        (setf macro (delete-spaces (subseq substring size)))
        (setf p (position-if #'delimator-char-p (delete-spaces macro)))
        (if p 
            (progn 
              (setf name (subseq macro 0 p))
              (setf macro (delete-spaces (subseq macro p))))
          (setf name macro))
        (when (string-equal "(" (subseq macro 0 1))
          (setf p (position ")" macro :test 'string-equal))
          (if p
            (setf args (subseq macro 0 (+ p 1))
                  macro (delete-spaces (subseq macro (+ p 1))))
            (print (string+ "Error in MACRO " name ": no ) in macro arguments"))))
        
        ; cherche le texte entre 2 # (peut etre sur plusieurs lignes)
        (setf p (search "#" macro))
        (if p 
            ; macro starts on the same line
            (setf body (subseq macro (+ p 1)))
          ; macro starts later
          (let ((line (look-for-line fileptr "#")))
            (when line (setf body (subseq line 1)))))
        
        (if (null body)
            (print (string+ "Error in MACRO " name ": no # body # found"))
          (let ((endfound nil))
            ; search end of macro
            (loop while (and (not endfound) (not (stream-eofp fileptr))) do
                  (setf p (search "#" body))
                  (if p 
                       ; macro ends on this line
                      (setf body (subseq body 0 p)
                            endfound t)
                    ; add next line to body
                    (setf body (string+ body (format nil "~%") (delete-spaces (read-line fileptr))))))
            (unless endfound
              (string+ "Error in MACRO " name ": no end # found")
              (setf body nil))))

        (setf rep (list name args body))
        ))
    rep))


; UDO: opcode name, xxx, yyy body endop
; fileptr : sometimes global can be of several lines
; return = (name (xxx yyy) body)
(defun orc-udo-line (string fileptr) 
  (when (and (>= (length (delete-spaces string)) 6) (search "opcode" (delete-spaces string) :start2 0 :end2 6))
        ; debut d'opcode   
    (let ((line (delete-spaces (subseq (delete-spaces string) 6)))
          name in out body)
      (multiple-value-setq (name line) (string-until-char line ","))
      (multiple-value-setq (in line) (string-until-char (delete-spaces line) ","))
      (setq out (delete-spaces line))
      (if (not (and name in out))
          (om-beep-msg (string+ "Error in UDO" 
                                (if name (string+ " " name) "")
                                ": unsupported or wrong format"))
        (let ((endfound nil)) 
          (setf line (delete-spaces (read-line fileptr)))
          (loop while (and (not endfound) (not (stream-eofp fileptr))) do
                (if (search "endop" line :start2 0 :end2 5)
                    (setf endfound t)
                  (setf body (if body (string+ body (format nil "~%") line) line)
                        line (delete-spaces (read-line fileptr)))))
          (if (null body)
              (om-beep-msg (string+ "Error in UDO " name ": no opcode source found"))
            (unless endfound
              (string+ "Error in UDO " name ": no 'endop' found")
              (setf body nil)))
          (list name (list in out) body))
        ))))


;; (get-cs-inits "/Users/bresson/Desktop/test.orc")

;;;returns (inum globals channels macros opcodes)
(defun parse-csound-head (file)
   (let* ((name "instr")
          (size (length name))
         (rep nil) globals chans macros opcodes)
     (loop while (and (not rep) (not (stream-eofp file))) do
           (let* ((newl (delete-spaces (read-line file)))
                  (val nil) 
                  (current-opcode nil))
             (cond
              ((orc-ignored-line newl) nil)
              ((setf val (orc-channel-line newl))
               (setf chans val))
              ((setf val (orc-macro-line newl file))
               (push val macros))
              ((setf val (orc-udo-line newl file))
               (push val opcodes))
              ((and (> (length newl) size) (search name newl :start2 0 :end2 (length name)))
               (setf rep (read-from-string (subseq newl 5))))
              ((setf val (orc-global-line newl file))
               (push val globals))             
              (t nil))))
     (list rep (reverse globals) chans (reverse macros) (reverse opcodes))))
     

; (setf ppp #P"/Users/bresson/Desktop/macro/macro-2.orc")
; (setf ppp #P"/Users/bresson/Desktop/add-x.orc")
; (with-open-file (file ppp) 
;   (let ((head (parse-csound-head file))
;         (code (get-csound-source file)))
;     (list head code)
;     ))



;;;------------
;;; ORC P-FIELDS
;;;------------

(defun get-rep-char (file etat str)
  (unless (stream-eofp file)
    (let ((newchar (om-correct-line (read-char file))))
      (cond
       ((char-equal newchar #\;) (read-line file nil) (get-rep-char file 'valid ""))  ;; new test to avoid considering commented p-fields 
       ((equal etat 'e) (get-endin file newchar #\n 'en))
       ((equal etat 'en) (get-endin file newchar #\d 'end))
       ((equal etat 'end) (get-endin file newchar #\i 'endi))
       ((equal etat 'endi)
        (cond
         ((char-equal newchar #\n) "endin")
         ((delimator-char-p newchar) 
          (get-rep-char file 'valid ""))
         (t (get-rep-char file 'noway ""))))
       ((equal etat 'p)
        (cond
         ((digit-char-p newchar)
          (get-rep-char file 'p (string+ str (format () "~C" newchar))))
         ((or (delimator-char-p newchar)
              (char-equal newchar #\,) (char-equal newchar #\)))
          (read-from-string (string+ str (format () "~C" newchar))))
         (t (get-rep-char file 'noway ""))))
       ((equal etat 'noway)
        (loop while (and (not (stream-eofp file)) (not (delimator-char-p newchar))) do
              (setf newchar (read-char file)))
              (get-rep-char file 'valid ""))
       ((equal etat 'valid) 
        (cond
         ((char-equal newchar #\p) (get-rep-char file 'p ""))
         ((char-equal newchar #\e) (get-rep-char file 'e ""))
         ((delimator-char-p newchar)
          (get-rep-char file 'valid ""))
         (t (get-rep-char file 'noway ""))))
       ))))

(defun get-endin (file newchar char next-etat)
  (cond
   ((char-equal newchar char) (get-rep-char file next-etat ""))
   ((delimator-char-p newchar) 
    (get-rep-char file 'valid ""))
   (t (get-rep-char file 'noway ""))))


;;; FILE pointer supposed at INSTR begin
;;; searches the number of p-fields in orc
(defun get-orc-numins (file)
  (let ((numins 0) end?)
    (loop while (and (not end?) (not (stream-eofp file))) do
          (let ((tokem-rep (get-rep-char file 'valid "")))
            (cond
             ((integerp tokem-rep)
              (setf numins (max numins tokem-rep)))
             ((stringp tokem-rep)
              (setf end? t)))))
    numins))


;;;------------
;;; ORC BODY
;;;------------
;; returns the source code of the instrument 
;; from POS or current position to "endin"
(defun get-csound-source (file &optional pos)
  (when pos (file-position file pos))
  (let (rep final)
    (loop while (and (not final) (not (stream-eofp file))) do
          (let ((newl (delete-spaces (om-correct-line (read-line file)))))
            (if (string-equal (delete-spaces newl) "endin")
                (setf final t)
              (push newl rep))))
    (when final (reverse rep))))


;;;====================================================================
; FILLS THE SLOTS OF A LISP-WRITTEN CLASS READING A CSOUND INSTRUMENT
;;;====================================================================

(defun get-orc-source (name)
  (let((result nil))
    (if name 
        (if (string-equal (pathname-type name) "orc")
            (with-open-file (file name)
              (loop while (not (stream-eofp file)) do
                    (let* ((rep (parse-csound-head file))
                           (j (car rep)))
                      (when (integerp j) (setf result (get-csound-source file)))))
              )
          (om-beep-msg "Sorry only .ORC files can be parsed."))
      (om-beep-msg "ORC file not found..."))
   result))

;; return (globals macros)
(defun get-orc-globals (name)
  (let ((globals ()))
    (if name
        (with-open-file (file name)
          (setf globals (nth 1 (parse-csound-head file))))
      (om-beep-msg "ORC file not found..."))
    globals))

(defun get-orc-channels (name)
  (let ((channel ()))
    (if name
        (with-open-file (file name)
          (setf channel (third (parse-csound-head file)) ))
      (om-beep-msg (format nil "ORC file not found...")))
  channel))

(defun get-orc-macros (name)
  (let ((macs ()))
    (if name 
        (with-open-file (file name)
          (setf macs (nth 3 (parse-csound-head file))))
      (om-beep-msg "ORC file not found..."))
  macs))

;;;====================================================================
; NEW CS-INIT SYSTEM
;;;====================================================================

(defclass! CS-INIT ()
  ((init-type :initform :global :accessor init-type :initarg :init-type)
   (name :initform nil :accessor name :initarg :name)
   (params :initform nil :accessor params :initarg :params)
   (value :initform nil :accessor value :initarg :value))
  (:icon 601))
   
(defmethod get-slot-in-out-names ((self CS-INIT))
   (values '("self" "init-type" "name" "params" "value")
           '(nil :global nil nil nil)
           '("" "" "" "(optional)" "")
           '(nil ((1 (("global variable" :global) ("macro" :macro) ("user-defined opcode" :opcode) ("num channels" :channels)))) nil nil nil)))


(defun get-inits-type (list type)
  (loop for item in list 
        when (equal (init-type item) type) 
        collect item))

(defmethod print-object ((self cs-init) stream)
  (format stream "#<cs-init>~A: ~A [~A] ~A" 
          (init-type self) (name self) (params self) (value self)))

(defun get-cs-inits (name)
  (let (parsed-strings inits)
    (if name 
        (with-open-file (file name)
          (setf parsed-strings (parse-csound-head file))
          (loop for glob in (nth 1 parsed-strings) do 
                (push (make-instance 'CS-INIT :init-type :global :name (car glob) :value (cadr glob)) inits))
          (loop for mac in (nth 3 parsed-strings) do 
                (push (make-instance 'CS-INIT :init-type :macro :name (car mac) :params (cadr mac) :value (caddr mac)) inits))
          (loop for oc in (nth 4 parsed-strings) do 
                (push (make-instance 'CS-INIT :init-type :opcode :name (car oc) :params (cadr oc) :value (caddr oc)) inits))
          )
      (om-beep-msg "ORC file not found..."))
  (reverse inits)
  ))




 


