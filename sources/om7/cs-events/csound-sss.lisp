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


;;; PFIELDS TO IGNORE WHEN DOING SPATIAL SOUND SYNTH
(defmethod ignore-pfields ((self cs-evt)) nil)
             
(defmethod merge-orchestras ((synth cs-evt) (spat cs-evt))
  (let* ((synthorc (source-code synth))
         (spatorc (source-code spat))
         (p-index (length (class-direct-instance-slots (class-of synth)))) ;;; nb params (from p4)
         (intername "asound")
         (spatvars '("p4"))
         (ignore-fields (ignore-pfields spat))
         (ignore-aux nil)
         (continue t)
         (synthlines nil) (spatlines nil))
    (setf synthlines 
          (loop for sline in (om-buffer-lines (buffer-text synthorc))
                collect 
                (let ((srep (and (not (string-equal "" (delete-spaces sline)))
                                 (let ((firstword (string (read-from-string sline nil nil))))
                                   (when (and (>= (length firstword) 3) 
                                              (string-equal "out" (subseq firstword 0 3)))
                                     (search "out" firstword :test 'string-equal)
                                     )))))
                  (if srep 
                      (multiple-value-bind (outword ll) (read-from-string sline nil nil :start srep)
                        (let ((outval (subseq sline ll (position #\, sline))))
                          (string+ intername "  =  " outval)))
                    sline)
                  )))
    (setf spatlines 
          (loop for sline in (om-buffer-lines (buffer-text spatorc))
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
                                (let* ((oppos (if (string-equal (delete-spaces (nth (- pos 1) tokens)) "") 2 1))
                                       (op (nth (- pos oppos) tokens))
                                       (varpos (if (string-equal (delete-spaces (nth (- pos oppos 1) tokens)) "")
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
                  ;(print (list ignore-fields ignore-aux))
                  (loop for ig in (append ignore-fields ignore-aux)
                        while tokens do
                        (when (find ig tokens :test 'string-equal)
                          ;(print (list ig tokens))
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
                                                                  1)))))) ;; -1 because p4 disappears
                  (reduce 'string+ tokens :initial-value "")
                  )))

    (append synthlines spatlines)
    ))


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
  (if (string-equal (delete-spaces (car token)) "")
      (cdr token)
    token))

(defun find-pn (tokens)
  (let ((rep nil))
    (loop for item in tokens 
          for pos = 0 then (+ pos 1) do
          (when (and (char-equal (elt item 0) #\p)
                     (> (length item) 1)
                     (numberp (read-from-string (subseq item 1))))
            (pushr (list pos (read-from-string (subseq item 1))) rep)))
    rep))


(defparameter *prisma-spat-classes* nil)

(defmethod merged-class-superclass (class)
  (let ((clist (mapcar 'class-name (get-class-precedence-list class))))
    (or (remove nil 
                (list (find-if #'(lambda (class) (find class *prisma-spat-classes*)) clist)
                      (find 'spat-trajectory-evt-3D clist)
                      (find 'spat-trajectory-evt-2D clist)
                      (find 'cs-spat-evt clist)))
        'cs-evt)))


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
   
  
(om::defmethod! chroma-spat-defclass (classname (synth cs-evt) (spat cs-evt))
  
  :icon 322
  
  (let ((orc (merge-orchestras synth spat))
        (nout (numchan spat))
        (inits (append (cs-inits synth) (cs-inits spat)))
        (gens (append (orc-header synth) (orc-header spat)))
        (superclasses (merged-class-superclass (class-of spat)))
        classdef class)
    
      (let ((newinits (solve-init-duplicates inits)))
            
        (setf classdef
              (print `(defclass! ,classname (,.superclasses)
                          ((source-code :initform 
                                       (let ((orcbuffer (make-instance 'textfile)))
                                         (add/replace-to-buffer orcbuffer ',orc)
                                         orcbuffer)
                                       :allocation :class :type textfile :accessor source-code)
                          (numchan :initform ,nout :allocation :class  :accessor numchan)
                          (cs-inits :initform ',newinits :allocation :class :type list :accessor cs-inits)
                          (orc-header :initform ',gens :allocation :class :type list :accessor orc-header)
                          (InstID :initform 1  :allocation :class  :accessor InstID)
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
                          
                          ))))
        (setf class (eval classdef))
        (let ((instance (make-instance class)))
          (setf (source-code instance) (let ((orcbuffer (make-instance 'textfile)))
                                         (add/replace-to-buffer orcbuffer orc)
                                         orcbuffer))
          (setf (numchan instance) nout)
          (setf (cs-inits instance) newinits)
          (setf (orc-header instance) gens)
          )
        
        class)))



(om::defmethod! merge-cs-events ((synth cs-evt) (spat cs-evt) &key name force-redefine)
  
  :icon 322
  
  (let ((classname (or name 
                       (intern (concatenate 'string 
                                            (string (om::name (class-of synth))) 
                                            "-to-"
                                            (string (om::name (class-of spat))))
                               :om)))
        (ncols (max (om::elts synth) (om::elts spat)))
        (onset (action-time synth))
        (pfun (or (and (user-fun synth) (user-fun spat)
                       #'(lambda (self i)
                           (when (functionp (user-fun synth))
                             (funcall (user-fun synth) self i))
                           (when (functionp (user-fun spat))
                             (funcall (user-fun spat) self i))))
                  (user-fun synth)
                  (user-fun spat)))
        (instance nil))
    
    (when (or (not (find-class classname nil)) force-redefine)
      (chroma-spat-defclass classname synth spat))

    (setf instance (make-instance classname :elts ncols))
    #|
    (setf rep (cons-array instance 
                    (list nil ncols onset pfun)
                    (append 
                      (list 'e-dels (slot-value synth 'e-dels)
                            'durs (slot-value synth 'durs))
                      (loop for s1 in (class-direct-instance-slots (class-of synth)) append
                            (list (slot-name s1) (slot-value synth (slot-name s1))))
                      (loop for s2 in 
                             (nthcdr (number-of-ignored-slots-in-merge spat)
                                    (get-all-instance-initargs (type-of spat)))
                            append
                            (let ((sl-name (internp (name s2) (slot-package s2))))
                              (list sl-name (slot-value spat sl-name)))
                            )
                      )
                     ))
    (setf (Lcontrols rep) (clone (append (Lcontrols synth) (Lcontrols spat))))
    (setf (precision rep) (max (list-max (list! (precision synth))) (list-max (list! (precision spat)))))
    (set-data rep)
    rep
    |#
    instance
    ))





