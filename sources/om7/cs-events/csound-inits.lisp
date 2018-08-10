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

;;;================================================================ 
;;; A structure to handle reading/writing of Csound inits
;;; (macros, user-defined opcodes, channels declarations and global variables)
;;;================================================================ 

(om::defclass! cs-init ()
  ((init-type :initform :global :accessor init-type :initarg :init-type)
   (name :initform nil :accessor name :initarg :name)
   (params :initform nil :accessor params :initarg :params)
   (value :initform nil :accessor value :initarg :value))
  )
 
;;; (("global variable" :global) ("macro" :macro) ("user-defined opcode" :opcode) ("num channels" :channels))

(defmethod print-object ((self cs-init) stream)
  (format stream "#<cs-init>~A: ~A [~A] ~A" 
          (init-type self) (name self) (params self) (value self)))

(defun get-inits-of-type (inits type)
  (loop for item in inits 
        when (equal (init-type item) type) 
        collect item))


;;; SOME SPECIFIC CLASS OF INITS CAN BE RE FORMATTED ACCORDING TO THE CLASS(ES) SYNTHESIZED
;;; SEE FOR INSTANCE OMPRISMA PRISMA-INIT
;;; => MUST RETURN ONE OR LIST OF CS-INITS

(defmethod format-inits (inits cs-events)
  (om::flat 
   (mapcar #'(lambda (init) 
               (format-init init cs-events))
           inits)))

;;; to be redefined for specific pairs of init/event
(defmethod format-init (inits cs-events) inits)


;;; WRITING CS-INITS

;;; CS-INITS CAN COME FROM CS-EVT CLASSES OR FROM SYNTHESIS PARAMS


;;; THE SYNTHESIS HAVE PRIORITY AND CAN REDEFINE THE CS-INITS FROM THE CS-EVTS
 
(defun overwrite-cs-inits (originals news)
  (mapcar #'(lambda (new) (setf (name new) (string-simplify (name new)))) news)
  (let ((not-redefined nil))
    (loop for item in originals do 
          (let ((redefined (find item news 
                                 :test #'(lambda (a b) 
                                           (and (equal (init-type a) (init-type b))
                                                (string-equal (string-simplify (name a))
                                                              (string-simplify (name b))))))))
            (unless redefined (push item not-redefined))))
    (append (reverse not-redefined) news)))


;;; UTILS TO SIMPLIFY / COMPARE / SOLVE DUPLICATES

(defun remove-tabs (str)
  (let ((pos (position #\Tab str)))
    (when pos 
      (setf (elt str pos) #\Space)
      (setf str (remove-tabs str)))
    str))

(defun remove-line-spaces (str &optional (from 0))   
  (let ((spacepos (search " " str :start2 from)))
    (cond ((null spacepos)  str)
          ((= spacepos 0)  (remove-line-spaces (subseq str 1)))
          ((= spacepos (- (length str) 1))  (remove-line-spaces (subseq str 0 spacepos)))
          ((string-equal " " (elt str (+ spacepos 1)))         
           (remove-line-spaces (concatenate 'string (subseq str 0 spacepos) (subseq str (+ spacepos 1)))))
          (t  (remove-line-spaces str (+ spacepos 1))))))
    
(defun string-simplify (str)
  (let ((lineend (or (search ";" str)
                     (length str))))
    (remove-line-spaces (remove-tabs (subseq str 0 lineend)))))


(defun init-equal (init1 init2)
  (and (equal (init-type init1) (init-type init2))
       (string-equal (string-simplify (name init1)) (string-simplify (name init2)))
       (equal (mapcar 'string-simplify (params init1)) (mapcar 'string-simplify (params init2)))
       (string-equal (string-simplify (value init1)) (string-simplify (value init2)))))


(defun init-dup-fatal-error (a b)
  (let ((t1 (init-type a))
        (t2 (init-type b)))
    (if (equal t1 t2) 
        (cond 
         ((equal t1 :macro)
           (if (and (string-equal (string-simplify (name a)) (string-simplify (name b)))
                   (not (equal (mapcar 'string-simplify (params a)) (mapcar 'string-simplify (params b)))))
              (progn 
                (om-message-dialog (string+ "Error: Macro " (name a) " is defined in various classes with incompatible arguments !"))
                t)
            nil))
         (t nil))
      nil)))


(defun init-duplicates-dialog (initdup-list type)
  (let* ((typename (cond ((equal type :global) "Global Variable")
                         ((equal type :macro) "Macro")
                         ((equal type :opcode) "UDO")
                         (t "??")))
                         
         (thedialog (om-make-window 'om-dialog  
                                    :window-title (string+ typename " conflicts")
                                    :size (om-make-point 500 450) 
                                    :close nil :resizable nil :maximize nil))
         (scrol (om-make-view 'om-scroller
                  :owner thedialog
                  :size (om-make-point 480 300)
                  :scrollbars :v
                  :field-size (om-make-point 270 (* 70 (length initdup-list)))
                  :position (om-make-point 10 80))) 
         tablelist)
    (loop for duplicates in initdup-list
          for i = 0 then (+ 1 i) do
          (let ((table (om-make-dialog-item 'om-single-item-list 
                                            (om-make-point 120 (+ 6 (* 65 i))) 
                                            (om-make-point 300 60) ""
                                         :font (om-make-font "Courier" 12)
                                         :scrollbars :v
                                         :range (loop for init in duplicates collect (flat-string (value init))))))
            (push table tablelist)
            (om-select-item-index table 0)
            (om-add-subviews scrol (om-make-dialog-item 'om-static-text
                                                        (om-make-point 5 (+ 18 (* 65 i))) (om-make-point 70 20)
                                                        (name (car duplicates))
                                                        :font (om-make-font "Courier" 13 :style '(:bold)))
                             table)
          ))
    
    
    (om-add-subviews thedialog
                     (om-make-dialog-item 'om-static-text
                                          (om-make-point 10 10) (om-make-point 400 60)
                                          (string+ "Some conflicting " typename
                                                   "s were detected between the different classes. Please choose one for each declared "
                                                   typename ".")
                                          :font *controls-font*
                                          )
      (om-make-dialog-item 'om-button (om-make-point 250 390) (om-make-point  80 24) "Cancel" 
                        :di-action (om-dialog-item-act item
                                     (declare (ignore item)) 
                                     (om-return-from-modal-dialog thedialog nil)))
     (om-make-dialog-item 'om-button (om-make-point 350 390) (om-make-point  80 24) "OK" 
                        :di-action (om-dialog-item-act item
                                    (declare (ignore item)) 
                                     (om-return-from-modal-dialog thedialog (loop for item in initdup-list
                                                                                  for cel in tablelist
                                                                                  collect (nth (om-get-selected-item-index cel) item)))) 
                        :default-button t :focus t))
    
    (om-modal-dialog thedialog)
    ))

#|

(init-duplicates-dialog )

|#

(defun solve-init-duplicates (init-list &optional type)
  (let* ((original (remove-duplicates init-list :test 'init-equal))
        (copy (copy-list original)))
    (let (duplicatas autosolve)
      (loop while copy do
            (let ((first (pop copy)) dup)
              (loop for item in copy do
                    (when (string-equal (string-simplify (name first)) (string-simplify (name item)))
                       (cond ((init-equal first item)
                              (push item autosolve)
                              (setf copy (remove item copy)))
                             ((init-dup-fatal-error first item)
                              (abort))
                             (t 
                              (push item dup)
                              (setf copy (remove item copy))))))
              (when dup 
                (push first dup)
                (push dup duplicatas))))
      (when duplicatas
        (or (setf duplicatas (init-duplicates-dialog (reverse duplicatas) type))
            (om-abort)))
        
      (let ((finalista (append autosolve duplicatas)))
        (loop for item in finalista do
              (setf original (remove (name item) original :key 'name :test 'string-equal)))
        (setf original (append original finalista)))
      original)))


(defun write-cs-globals (file global-inits)
  (when global-inits
    (let ((globallist (solve-init-duplicates global-inits :global)))
      (when globallist
        (format file "; GLOBAL VARIABLES~%~%")
        (loop for gvar in globallist do
              (format file "~D  ~D~%" (name gvar) (value gvar)))
        (format file "~%~%"))
      )))

(defun write-cs-macros (file mac-inits)
  (when mac-inits
    (let ((maclist (solve-init-duplicates mac-inits :macro)))
      (when maclist 
        (format file "; MACROS~%~%")
        (loop for mac in maclist do
              (format file "#define ~D~D #~D#~%" (name mac) (or (params mac) "") (value mac)))
        (format file "~%~%"))
      )))

(defun write-cs-opcodes (file opcode-inits)
  (when opcode-inits
    (let ((oclist (solve-init-duplicates opcode-inits :opcodes)))
      (when oclist
        (format file "; USER-DEFINED OPCODES~%~%")    
        (loop for oc in oclist do
              (format file "opcode ~A, ~A, ~A~% ~A~%endop" (name oc)  (car (params oc)) (cadr (params oc)) (value oc))
              (format file "~%~%"))
        ))))


(defun get-all-inits (instlist &optional type)
  (loop for item in instlist append 
        (if type (get-inits-type (cs-inits item) type)
          (cs-inits item))))

;;; main function called from synthesize
(defun write-cs-inits (outstream instlist inits)
  (let ((new-list (overwrite-cs-inits (get-all-inits instlist) inits)))
    (write-cs-globals outstream (get-inits-type new-list :global))
    (write-cs-macros outstream (get-inits-type new-list :macro))
    (write-cs-opcodes outstream (get-inits-type new-list :opcode))))


