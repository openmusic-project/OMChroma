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
;;; abstract class to be subclassed with specific Csound orchestra
;;;================================================================ 

(defclass cs-evt (cs-array om::synthesisevt)
  (;;; these slots are inherited from cs-array but cs-evt have them class-allocated
   (cs-instr :accessor cs-instr :initform nil :allocation :class)
   (instr-num :accessor instr-num :initform 1 :allocation :class)
   
   ;;; repeat this slot so that it appears on the box
   (om::elts :initarg :elts :accessor om::elts :initform 1 :type integer :documentation "number of elements (csound 'notes')")
 
   ;;; specific slots of cs-evt
   (action-time :initarg :action-time :accessor action-time :initform 0 :documentation "an offset used to time several events together")
   (user-fun :initarg :user-fun :accessor user-fun :initform nil 
             :documentation "a lambda patch or function to process internal elements at synthesis time")
   ))




(defun defclass-from-cs-orc (pathname)
  
  (let ((class-name (intern (string-upcase (pathname-name pathname))))
        (instr-desc (car (get-cs-descriptions pathname))))
    
    (om::om-print (format nil "- Defining CS class: ~A" class-name) "OMCHROMA")
    
    (eval 
     `(om::defclass! ,class-name (cs-evt)
        ,(append 
          '((om::elts :initarg :elts :accessor om::elts :initform 1 :type integer :documentation "number of elements (components) for the event")
            (action-time :initarg :action-time :accessor action-time :type number :initform 0 :documentation "start time of the whole event (in sec)")
            (user-fun :initarg :user-fun :accessor user-fun :initform nil 
                      :documentation "a lambda patch or function to process internal elements at synthesis time (lambda/fun-name)")
            ;;; class-allocates the orc data with the first instrument in the file
            (cs-instr :accessor cs-instr :allocation :class :initform nil)
            (instr-num :accessor instr-num :initform 1 :allocation :class))
          
          ;;; slots corresponding to the pfields from p2 on
          ;;; these slot are not bound to actual values but used to programmatically access 
          ;;; the contens (data) of the array
          ;(loop for p in (cs-description-params instr-desc) collect
          ;      (let ((slot-name (intern (string-upcase (cs-param-description-name p)))))
          ;        `(,slot-name :initarg ,(om::intern-k slot-name) :allocation :class 
          ;          ;; :documentation ,(cs-param-description-doc p) ;; already stored in the array-fields
          ;          )))
          )
        ))


      ;;; a 'fake' accessor to allow using param-names reading in the array data 
      (loop for p in (cs-description-params instr-desc) collect
            (let ((slot-name (intern (string-upcase (cs-param-description-name p)))))
              (eval `(defmethod ,slot-name ((self ,class-name)) 
                       (om::get-field self ,(string slot-name))))
              ))

     ;;; will (re)set the class-allocated cs-instr
     (setf (slot-value (make-instance class-name) 'cs-instr) instr-desc)
  
    class-name))

; (defclass-from-cs-orc #P"/Users/bresson/SRC/OM7/ircam/LIBRARIES/omchroma/sources/cs-events/classes/basic/add-a1.orc")
  
(defun def-all-cs-classes (folder &key name inpackage)

  (om::om-print-dbg "Defining classes from: ~A" (list folder) "OMCHROMA")

  (let ((package (om::AddPackage2Pack (or name (car (last (pathname-directory folder))))
                                      (or inPackage (om::find-library "OMChroma")))))

  (let ((files (om-api:om-directory folder :type "orc" :directories nil))
        (subfolders (om-api:om-directory folder :files nil :directories t)))
      (loop for cs-file in (sort files 'string< :key 'namestring) do 
            (let ((class (defclass-from-cs-orc cs-file)))
              (om::addclass2pack class package)
              ))
      (loop for dir in (sort subfolders 'string< :key 'namestring) do 
            (def-all-cs-classes dir :inPackage package))
      )))







