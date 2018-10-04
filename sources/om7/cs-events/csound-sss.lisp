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

;;;===================================================================
;;; CONNECTION WITH SPATIALIZATION CLASSES FOR SPATIAL SOUND SYNTHESIS
;;; File author: J. Bresson
;;;===================================================================

(in-package :cr)


;;; PFIELDS TO IGNORE (in addition to p2-p3-p4) WHEN DOING SPATIAL SOUND SYNTH
(defmethod ignore-pfields ((self cs-evt)) nil)
;;(defmethod ignore-pfields ((self om::dbap.discrete)) '("p5" "p6" "p7"))



(defparameter *csound-separators* '(#\) #\( #\[ #\] #\{ #\} #\, #\* #\+ #\- #\/ #\? #\: #\< #\> #\= #\;))

(defun tokenize-line (line)
  (let* ((str line))
    (remove nil 
            (loop while (not (string-equal str ""))
                  collect
                  (let ((cc (elt str 0)))
                    (cond ((or (char-equal cc #\Space) (char-equal cc #\Tab))
                           (let ((token ""))
                             (loop while (and (not (string-equal str ""))
                                              (or (char-equal cc #\Space) (char-equal cc #\Tab)))
                                   do
                                   (setf token (concatenate 'string token (string cc)))
                                   (setf str (subseq str 1))
                                   (unless (string-equal str "") (setf cc (elt str 0)))
                                   )
                             token))
                          ((member cc *csound-separators*)
                           (setf str (subseq str 1))
                           (string cc))
                          (t (let ((token ""))
                               (loop while (and (not (string-equal str ""))
                                                (not (char-equal cc #\Space))
                                                (not (char-equal cc #\Tab))
                                                (not (member cc *csound-separators*))) do
                                     (setf token (concatenate 'string token (string cc)))
                                     (setf str (subseq str 1))
                                     (unless (string-equal str "") (setf cc (elt str 0)))
                                     )
                               token)))))
            )))


(defun start-at-first (token) 
  (if (string-equal (om::delete-spaces (car token)) "")
      (cdr token)
    token))

(defun find-pn (tokens)
  (let ((rep nil))
    (loop for item in tokens 
          for pos = 0 then (+ pos 1) do
          (when (and (char-equal (elt item 0) #\p)
                     (> (length item) 1)
                     (numberp (read-from-string (subseq item 1))))
            (push (list pos (read-from-string (subseq item 1))) rep)))
    (reverse rep)))



(defmethod merge-orchestras ((synth cs-evt) (spat cs-evt))

  (let* ((synth-lines (cs-description-body-lines (cs-instr synth)))
         (spat-lines (cs-description-body-lines (cs-instr spat)))
         (p-index (- (length (cs-description-params (cs-instr synth))) 2)) ;;; nb params (from p4)
         (intername "asound")
         (spatvars '("p4"))      ;; by convention p4 is the audio input 
         (ignore-fields (ignore-pfields spat))
         (ignore-aux nil)
         (continue t))
    
    (append  
     
     (loop for sline in synth-lines
           collect 
           (let ((srep (and (not (string-equal "" (om::delete-spaces sline)))
                            (let ((firstword (string (read-from-string sline nil nil))))
                              (when (and (>= (length firstword) 3) 
                                         (string-equal "out" (subseq firstword 0 3)))
                                (search "out" firstword :test 'string-equal)
                                )))))
             (if srep 
                 ;;; replace the "outX" var by an intermediate var
                 (multiple-value-bind (outword ll) (read-from-string sline nil nil :start srep)
                   (let ((outval (subseq sline ll (position #\, sline))))
                     (concatenate 'string intername "  =  " outval)))
               sline)
             ))

     (loop for sline in spat-lines
           collect 
           (let ((tokens (start-at-first (tokenize-line sline))))
             ;(print (list tokens spatvars ignore-aux))
             (loop for spatvar in spatvars do
                   (when (find spatvar tokens :test 'string-equal)
                     ;;; one of the variables containing input is in this line
                     (let ((pos (position spatvar tokens :test 'string-equal)))
                            ;(print tokens)
                       (if (> pos 0) 
                           ;; something is probably set using a "spatvar"
                           (let* ((oppos (if (string-equal (om::delete-spaces (nth (- pos 1) tokens)) "") 2 1))
                                  (op (nth (- pos oppos) tokens))
                                  (varpos (if (string-equal (om::delete-spaces (nth (- pos oppos 1) tokens)) "")
                                              (+ oppos 2) (+ oppos 1)))
                                  (var (nth (- pos varpos) tokens)))

                             (if (and continue 
                                      (or (string-equal op "=")
                                          (search "diskin" op :test 'string-equal)))
                                 (setf spatvars (append spatvars (list var))
                                       tokens nil)
                               (setf tokens (substitute intername spatvar tokens :test 'string-equal))
                               )
                             (when (search "diskin" op :test 'string-equal)
                               (setf continue nil))
                             )
                         ;; the spatvar is set to something else
                         ;(setf (nth pos tokens) intername)
                         (setf tokens (substitute intername spatvar tokens :test 'string-equal))
                         ))
                     ))

            
             (loop for ig in (append ignore-fields ignore-aux)
                   while tokens do
                   (when (find ig tokens :test 'string-equal)
                     (let ((pos (position ig tokens :test 'string-equal))
                           (vvv nil))
                       (when (> pos 0) 
                         (loop for spatvar in spatvars while (not vvv) do
                               (setf vvv (find spatvar tokens :test 'string-equal)))
                         (if vvv 
                             (push (car tokens) spatvars)
                           (unless (or (member (car tokens) (cons intername spatvars) :test 'string-equal)
                                       (member (car tokens) *csound-separators* :test 'string-equal :key 'string))
                             (push (car tokens) ignore-aux)))
                         (setf tokens nil)))
                     ))
                  
             (when tokens (loop for pfield in (find-pn tokens) do
                                (when (> (cadr pfield) 3) 
                                  (setf (nth (car pfield) tokens) 
                                        (format nil "p~D" (- (+ p-index (cadr pfield))
                                                             (length ignore-fields)
                                                             1)) ;; -1 because p4 disappears
                                        ))))
             (apply 'concatenate (cons 'string tokens))
             ))
     )
     
    ))





(defparameter *prisma-spat-classes* nil)

(defmethod merged-class-superclass (class)
  (let ((clist (mapcar 'class-name (om::class-precedence-list class))))
    (or (remove nil 
                (list (find-if #'(lambda (class) (find class *prisma-spat-classes*)) clist)
                      (find 'spat-trajectory-evt-3D clist)
                      (find 'spat-trajectory-evt-2D clist)
                      (find 'cs-spat-evt clist)))
        '(cs-evt))))


;;; ADD e-dels, durs, and soundfile/afil
(defmethod number-of-ignored-slots-in-merge ((self cs-evt))
  (+ (length (ignore-pfields self)) 3))


(defmethod merge-get-define-slot ((self om::omslot) newname)
  (let ((name (or newname (om::name self))))
    (list (intern name (slot-package self)) 
          :initform  (om::theinitform self)  
          :initarg (om::intern-k name)
          :type (read-thetype self) 
          :allocation (om::alloc self) 
          :documentation (om::doc self)
          :accessor (intern name (slot-package self)))))
   
  

#|
 ,.(mapcar #'(lambda (slot) `(,(slot-name slot) :accessor ,(slot-name slot) :initarg ,(slot-name slot) 
                                                :type ,(slot-definition-type slot) :initform ,(slot-definition-initform slot)))
           (class-direct-instance-slots (class-of synth)))
                
 ,.(mapcar #'(lambda (slot) 
               (let ((newname (name slot)))
                 (loop while (find newname (class-direct-instance-slots (class-of synth)) :test 'string-equal :key 'slot-name) do
                       (setf newname (string+ newname "_+")))
                 (merge-get-define-slot slot newname)))
           (nthcdr (number-of-ignored-slots-in-merge spat)
                   (get-all-instance-initargs (type-of spat))
                   ))
|#


(defmethod chroma-spat-defclass (classname (synth cs-evt) (spat cs-evt))  
  
  (let* ((inits (solve-init-duplicates (append (get-all-inits synth) (get-all-inits spat))))
         (instr-desc (make-cs-description  ;;; generate a "fake" cs orc description...
                      :body-lines (merge-orchestras synth spat) 
                      :channels (cs-description-channels (cs-instr spat))
                      :gens (append (cs-description-gens (cs-instr synth)) (cs-description-gens (cs-instr spat)))
                      :num-pfields (+ (cs-description-num-pfields (cs-instr synth)) (cs-description-num-pfields (cs-instr spat)) -4) 
                      :global-vars (solve-init-duplicates (append (cs-description-global-vars (cs-instr synth))
                                                                  (cs-description-global-vars (cs-instr spat))))
                      :macros (solve-init-duplicates (append (cs-description-macros (cs-instr synth))
                                                             (cs-description-macros (cs-instr spat))))
                      :opcodes (solve-init-duplicates (append (cs-description-opcodes (cs-instr synth)) 
                                                              (cs-description-opcodes (cs-instr spat))))
                      :params (append (cs-description-params (cs-instr synth))
                                      (loop for param in (nthcdr 3 (cs-description-params (cs-instr spat)))
                                            for n = (+ (length (cs-description-params (cs-instr synth))) 2) ;;; we start at p2
                                            then (+ n 1) 
                                            collect
                                            (make-cs-param-description :num n 
                                                                       :name (cs-param-description-name param) 
                                                                       :type (cs-param-description-type param) 
                                                                       :defval (cs-param-description-defval param) 
                                                                       :doc (cs-param-description-doc param))))
                      ))
         
         (superclasses (merged-class-superclass (class-of spat)))
         
         (class
          (eval `(om::defclass! ,classname (,.superclasses)
                   ((om::elts :initarg :elts :accessor om::elts :initform 1 :type integer 
                              :documentation "number of elements (components) for the event")
                    (action-time :initarg :action-time :accessor action-time :type number :initform 0 
                                 :documentation "start time of the whole event (in sec)")
                    (user-fun :initarg :user-fun :accessor user-fun :initform nil 
                              :documentation "a lambda patch or function to process internal elements at synthesis time (lambda/fun-name)")
                    (cs-instr :accessor cs-instr :allocation :class :initform nil)
                    (instr-num :accessor instr-num :initform 1 :allocation :class)))                    
                )))
    
    (setf (slot-value (make-instance classname) 'cs-instr) instr-desc)
    
    class))
        



(om::defmethod! merge-cs-events ((synth cs-evt) (spat cs-evt) &key name force-redefine)
    
  (let ((classname (or name 
                       (intern (concatenate 'string 
                                            (string (om::name (class-of synth))) 
                                            "-to-"
                                            (string (om::name (class-of spat))))
                               :om)))
        (elts (max (om::elts synth) (om::elts spat)))
        (at (action-time synth))
        (uf (or (and (user-fun synth) (user-fun spat)
                       #'(lambda (self i)
                           (when (functionp (user-fun synth))
                             (funcall (user-fun synth) self i))
                           (when (functionp (user-fun spat))
                             (funcall (user-fun spat) self i))))
                  (user-fun synth)
                  (user-fun spat))))
    
    (when (or (not (find-class classname nil)) force-redefine)
      (chroma-spat-defclass classname synth spat))

    (let ((instance (om::om-init-instance 
                     (make-instance classname 
                                    :elts elts
                                    :action-time at
                                    :user-fun uf))))
        
      (setf (om::data instance) (append (om::data synth)
                                        (nthcdr 3 (om::data spat))))

      instance)))






