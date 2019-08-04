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
; (c) Ircam 2000 - 2017
;Authors: C. Agon, M. Stroppa, J. Bresson, S. Lemouton

(in-package :cr)
 
;;;=========================
;;; CSOUND INSTRUMENT PARSER
;=========================

;=========================
; UTILS
;=========================

;;; Returns T if char is a delimitator char
(defun delimator-char-p (char)
  (or (char-equal char #\Space) 
      (char-equal char #\Newline) (char-equal char #\Tab) (char-equal char #\,) (char-equal char #\() (char-equal char #\))
      (char-equal char #\*) (char-equal char #\+) (char-equal char #\-) (char-equal char #\/) (char-equal char #\=)))

;; Shifts the file pointer to next line beginning with NAME
(defun look-for-line (fileptr name)
   (let ((rep nil) 
         (size (length name)))
     (loop while (and (not rep) (not (oa:om-stream-eof-p fileptr))) do
           (let* ((newl (om::delete-spaces (read-line fileptr))))
             (if (and (>= (length newl) size) (search name newl :start2 0 :end2 (length name)))
                 (setf rep newl))))
     rep))

;;;=====================================================
;;; HEADER PARSER
;;;=====================================================

;;; If <line> is the channel number line of the header, returns the number of channels
;;; otherwise NIL
(defun orc-channel-line (line)
  (let ((substring (om::delete-spaces line))
        rep)
    (when substring
      (if (and (> (length substring) 6) (search "nchnls" substring :start2 0 :end2 7))
        (setf rep (read-from-string (om::delete-spaces (subseq (om::delete-spaces (subseq substring 6)) 1))))
        ))
    rep))

;; T if <line> is to be ignored
(defun orc-ignored-line (line)
  (let ((substring (om::delete-spaces line)))
    (if (and substring (> (length substring) 0))
        (let* ((index (position-if #'delimator-char-p substring))
               (name (subseq substring 0 index)))
          (or (equal ";" (subseq name 0 1))
              (string-equal name "sr")
              (string-equal name "kr")
              (string-equal name "ksmps")))
          t ; ignore
          )))

; Returns a global variable description if a global variable is declared in <line>
; Global variable description = (name body)
; Accepts all valid global forms
; <fileptr> is passed because sometimes global variable declarations can span several lines
(defun orc-global-line (line fileptr)
  (let ((substring (om::delete-spaces line))
        rep)
    (when substring
      (let ((index (position-if #'delimator-char-p substring))
            name value)
        (when index
          (setf name (subseq substring 0 index))
          (setf value (oa:om-clean-line (subseq substring index)))
          (loop while (equal #\\ (elt value (- (length value) 1))) do
                (setf value (concatenate 'string (subseq value 0 (- (length value) 1)) 
                                     " " 
                                     (oa:om-clean-line (om::delete-spaces (read-line fileptr))))))
          ;; (setf rep (list name value))
          (setf rep (make-instance 'cs-init :init-type :global :name name :value value))
          )))
    rep))

; macro: #define...
; Returns a macro description if a Csound macro is declared in <line>
; Macro description = (name params body)
; <fileptr> is passed because sometimes macro declarations can span several lines
(defun orc-macro-line (line fileptr) 
  (let* ((searchname "#define")
         (size (length searchname))
         (substring (om::delete-spaces line))
         rep)
    (when (and (>= (length substring) size) (search searchname substring :start2 0 :end2 size))
      ; => beginning of a macro
      (let (macro name args body p)
        (setf macro (om::delete-spaces (subseq substring size)))
        (setf p (position-if #'delimator-char-p (om::delete-spaces macro)))
        (if p 
            (progn 
              (setf name (subseq macro 0 p))
              (setf macro (om::delete-spaces (subseq macro p))))
          (setf name macro))
        (when (string-equal "(" (subseq macro 0 1))
          (setf p (position ")" macro :test 'string-equal))
          (if p
            (setf args (subseq macro 0 (+ p 1))
                  macro (om::delete-spaces (subseq macro (+ p 1))))
            (print (format nil "Error in MACRO ~S: no ) in macro arguments" name))))
        
        ; search text between 2 # (can span several lines)
        (setf p (search "#" macro))
        (if p 
            ; macro starts on the same line
            (setf body (subseq macro (+ p 1)))
          ; macro starts later
          (let ((line (look-for-line fileptr "#")))
            (when line (setf body (subseq line 1)))))
        
        (if (null body)
            (print (format nil "Error in MACRO ~S: no # body # found" name))
          (let ((endfound nil))
            ; search end of macro
            (loop while (and (not endfound) (not (oa:om-stream-eof-p fileptr))) do
                  (setf p (search "#" body))
                  (if p 
                       ; macro ends at this line
                      (setf body (subseq body 0 p)
                            endfound t)
                    ; add next line to body
                    (setf body (concatenate 'string body (format nil "~%") (om::delete-spaces (read-line fileptr))))))
            (unless endfound
              (print (format nil "Error in MACRO ~S: no end # found" name))
              (setf body nil))))

        ; (setf rep (list name args body))
        (setf rep (make-instance 'cs-init :init-type :macro :name name :params args :value body))

        ))
    rep))


; UDO (user-defined opcode): "opcode name, xx, yy body endop"
; Returns a UDO description if a UDO is declared in <line>
; UDO description = (name (xx yy) body)
; <fileptr> is passed because sometimes UDO declarations can span several lines
(defun orc-udo-line (string fileptr) 
  (when (and (>= (length (om::delete-spaces string)) 6) (search "opcode" (om::delete-spaces string) :start2 0 :end2 6))
    ; beginning of UDO   
    (let ((line (om::delete-spaces (subseq (om::delete-spaces string) 6)))
          name in out body)
      (multiple-value-setq (name line) (om::string-until-char line ","))
      (multiple-value-setq (in line) (om::string-until-char (om::delete-spaces line) ","))
      (setq out (om::delete-spaces line))
      (if (not (and name in out))
          (om::om-beep-msg (concatenate 'string "Error in UDO" (if name (string+ " " name) "") ": unsupported or wrong format"))
        (let ((endfound nil)) 
          (setf line (om::delete-spaces (read-line fileptr)))
          (loop while (and (not endfound) (not (oa:om-stream-eof-p fileptr))) do
                (if (string-equal "endop" (subseq line 0 (min 5 (length line))))
                    (setf endfound t)
                  (setf body (if body (concatenate 'string body (format nil "~%") line) line)
                        line (om::delete-spaces (read-line fileptr)))))
          (if (null body)
              (om-beep-msg (concatenate 'string "Error in UDO " name ": no opcode source found"))
            (unless endfound
              (print (format nil "Error in UDO ~S: no 'endop' found" name))
              (setf body nil)))
          ;(list name (list in out) body)
          (make-instance 'cs-init :init-type :opcode :name name :params (list in out) :value body)
          )
        ))))

;;; param = (num name type defval doc)
(defun orc-doc-params-line (line)
  (when (and (> (length line) 2)
             (equal #\; (elt line 0)))
    (let* ((pos (search "@PARAM " line)))
      (when pos
        (om::om-read-list-from-string (subseq line (+ pos 7)))
        ))))


(defun orc-doc-gen-line (line)
  (when (and (> (length line) 2)
             (equal #\; (elt line 0)))
    (let* ((pos (search "@GEN " line)))
      (when pos
        (subseq line (+ pos 5))
        ))))



;;; by conventions the doc text start with ;;
(defun orc-doc-text-line (line)
  (and (>= (length line) 2)
       (equal #\; (elt line 0))
       (equal #\; (elt line 1))
       (subseq line 2)))


;;; MAIN FUNCTION:
;;; returns (inum globals n-channels macros UDOs docparams docgens doctext) starting at <fileptr> position
(defun parse-csound-head (fileptr)
  
  (let ((rep nil) globals chans macros opcodes docparams docgens doctext)
    
    (loop while (and (not rep) (not (oa:om-stream-eof-p fileptr))) do
          
          (let* ((newl (om::delete-spaces (read-line fileptr)))
                 (val nil))
            
            (cond
             ;;; in commented lines...
             ((setf val (orc-doc-params-line newl))
              (push val docparams))
             ((setf val (orc-doc-gen-line newl))
              (push val docgens))
             ((setf val (orc-doc-text-line newl))
              (push val doctext))
             ;;; other ignored lines
             ((orc-ignored-line newl) nil)
             
             ;;; end of header / doc:
             ((and (> (length newl) (length "instr")) (search "instr" newl :start2 0 :end2 (length "instr")))
              (setf rep (read-from-string (subseq newl 5))))
             
             ;;; declarations
             ((setf val (orc-channel-line newl))
              (setf chans val))
             ((setf val (orc-macro-line newl fileptr))
              (push val macros))
             ((setf val (orc-udo-line newl fileptr))
              (push val opcodes))
             ((setf val (orc-global-line newl fileptr))
              (push val globals))     

             (t nil))))
    
    (list rep (reverse globals) chans (reverse macros) (reverse opcodes)
          (reverse docparams) (reverse docgens) (reverse doctext))
    ))
     

;;;=====================================================
;;; CODE PARSER
;;;=====================================================

(defun get-rep-char (fileptr state str)
  (unless (oa:om-stream-eof-p fileptr)
    (let ((newchar (read-char fileptr)))
      (cond
       ((char-equal newchar #\;) (read-line fileptr nil) (get-rep-char fileptr 'valid ""))  ;; new test to avoid considering commented p-fields 
       ((equal state 'e) (get-endin fileptr newchar #\n 'en))
       ((equal state 'en) (get-endin fileptr newchar #\d 'end))
       ((equal state 'end) (get-endin fileptr newchar #\i 'endi))
       ((equal state 'endi)
        (cond
         ((char-equal newchar #\n) "endin")
         ((delimator-char-p newchar) 
          (get-rep-char fileptr 'valid ""))
         (t (get-rep-char fileptr 'noway ""))))
       ((equal state 'p)
        (cond
         ((digit-char-p newchar)
          (get-rep-char fileptr 'p (concatenate 'string str (format () "~C" newchar))))
         ((or (delimator-char-p newchar)
              (char-equal newchar #\,) (char-equal newchar #\)))
          (read-from-string (concatenate 'string str (format () "~C" newchar))))
         (t (get-rep-char fileptr 'noway ""))))
       ((equal state 'noway)
        (loop while (and (not (oa:om-stream-eof-p fileptr)) (not (delimator-char-p newchar))) do
              (setf newchar (read-char fileptr)))
              (get-rep-char fileptr 'valid ""))
       ((equal state 'valid) 
        (cond
         ((char-equal newchar #\p) (get-rep-char fileptr 'p ""))
         ((char-equal newchar #\e) (get-rep-char fileptr 'e ""))
         ((delimator-char-p newchar)
          (get-rep-char fileptr 'valid ""))
         (t (get-rep-char fileptr 'noway ""))))
       ))))


(defun get-endin (fileptr newchar char next-state)
  (cond
   ((char-equal newchar char) (get-rep-char fileptr next-state ""))
   ((delimator-char-p newchar) 
    (get-rep-char fileptr 'valid ""))
   (t (get-rep-char fileptr 'noway ""))))


;;; file pointer is supposed at INSTR begin
;;; i.e. after a call to parse-csound-head
;;; returns the number of p-fields in orc
(defun get-orc-num-p-fields (fileptr)
  (let ((numpfields 0) end?)
    (loop while (and (not end?) (not (oa:om-stream-eof-p fileptr))) do
          (let ((tokem-rep (get-rep-char fileptr 'valid "")))
            (cond
             ((integerp tokem-rep)
              (setf numpfields (max numpfields tokem-rep)))
             ((stringp tokem-rep)
              (setf end? t)))))
    numpfields))


;;;------------
;;; ORC BODY
;;;------------
;; returns the source code of the instrument 
;; from POS or current position to "endin"
(defun parse-csound-body (fileptr &optional pos)
  (when pos (file-position fileptr pos))
  (let (rep final)
    (loop while (and (not final) (not (oa:om-stream-eof-p fileptr))) do
          (let ((newl (om::delete-spaces (oa:om-clean-line (read-line fileptr)))))
            (if (string-equal (om::delete-spaces newl) "endin")
                (setf final t)
              (push newl rep))))
    (when final (reverse rep))))

; (setf ppp #P"/Users/bresson/Desktop/macro/macro-2.orc")
; (setf ppp #P"/Users/bresson/Desktop/add-x.orc")
; (with-open-file (file ppp) 
;   (let ((head (parse-csound-head file))
;         (code (parse-csound-body file)))
;     (list head code)
;     ))


;;;------------
;;; FILE PARSER
;;;------------

(defstruct cs-description 
  (instr-num) (body-lines) (num-pfields) (global-vars) (channels) (macros) (opcodes)
  (params) (gens) (doc-lines))

(defstruct cs-param-description (num) (name) (type) (defval) (doc))

;; returns a list of cs-instr-descriptions (instr-num body-lines num-pfields globals channels macros)
;; for each instrument in the file
(defun parse-csound-instruments (fileptr)
 
  (let ((rep nil))
    
    (multiple-value-bind (instr-num global-vars channels macros opcodes params gens doc-lines)
        (values-list (parse-csound-head fileptr))
      ;;; WE CONSIDER _ALL THESE HEADER DATA IS SHARED BY ALL INSTRUMENTS_
      ;;; instr-num is the id of the first instrument found
         
      (loop while (and (not (oa:om-stream-eof-p fileptr)) 
                       (integerp instr-num))
            do
            (let* ((instrument-begin-pos (file-position fileptr))
                   (num-pfields (get-orc-num-p-fields fileptr))
                   (body-lines (parse-csound-body fileptr instrument-begin-pos)))
            
              (push (make-cs-description 
                     :instr-num instr-num 
                     :body-lines body-lines 
                     :num-pfields num-pfields
                     :global-vars global-vars 
                     :channels channels 
                     :macros macros
                     :opcodes opcodes
                     :gens gens
                     :doc-lines doc-lines
                     :params (loop for p in params collect 
                                   (make-cs-param-description 
                                    :num (nth 0 p) 
                                    :name (nth 1 p)
                                    :type (nth 2 p)
                                    :defval (nth 3 p)
                                    :doc (nth 4 p)))
                     )
                    rep)
              ;;; go to next instrument
              (setf instr-num (car (parse-csound-head fileptr)))
              )
            )
      (reverse rep)
      
      )))
  

