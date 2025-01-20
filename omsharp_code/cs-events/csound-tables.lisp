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

(om::add-preference-section :libraries "OMCHROMA" nil '(:omchroma-max-cs-points))
(om::add-preference :libraries :omchroma-max-cs-points "Max points in Csound tables" 
                    (om::make-number-in-range :min 10 :max 100000 :decimals 0)
                    1000)

;================================================
; TOP-LEVEL CLASS
;================================================
(defclass cs-table ()
  ((id :initform nil :initarg :id :accessor id)))

(defmethod om::object-box-label ((self cs-table))
  (concatenate 'string 
               (string-upcase (type-of self)) " #"
               (format nil "~A" (or (id self) "?"))))

;================================================
; + SOME GENERAL DATA (size and stime)
;================================================
(defclass standard-cs-table (cs-table) 
   ((size :initform nil :initarg :size :type integer :accessor size)
    (stime :initform 0 :initarg :stime :type number :accessor stime)))

(defmethod initialize-instance :after ((self standard-cs-table) &rest args)
  (unless (size self)
    (setf (size self) (om::get-pref-value :externals :csound-table-size))))

;================================================
; JUST A STRING (FREE)
;================================================
(om::defclass! text-cs-table (cs-table)
  ((id :initform nil :initarg :id :accessor id)
   (csstring :initform nil :initarg :csstring :accessor csstring)))

(defmethod cs-table-string ((self text-cs-table)) (csstring self))

(defun get-table-id-from-str (string)
  (let ((rep (om::delete-spaces string)))
    (if (equal (elt rep 0) #\f)
      (setf rep (om::string-until-space (om::delete-spaces (subseq rep 1))))
      (error "Table definition strings must start by f"))
    (when rep
      (read-from-string rep))))

;;; old utility functions
(defun SCsT (string)
  (make-instance 'text-cs-table
                 :id (get-table-id-from-str string)
                 :csstring string))

(defun CsT (id params &key size (gennum 7))
  (make-instance 'text-cs-table 
                 :id id 
                 :csstring (format nil "f ~D 0 ~D ~D ~{~S ~}" 
                                   id (or size (om::get-pref-value :externals :csound-table-size)) 
                                   gennum params)))

;================================================
; GENRAL/TEXT-BASED FORMAT FOR GENs
;================================================
(om::defclass! gen-cs-table (standard-cs-table) 
  ((id :initform nil :initarg :id :accessor id)
   (size :initarg size :initform nil :type integer :accessor size)
   (stime :initarg stime :initform 0 :type number :accessor stime)
   (gen-num :initform 1 :initarg :gen-num :type integer :accessor gen-num)
   (param-list :initform nil :initarg :param-list :type list :accessor param-list))
  (:icon 605))

(defmethod cs-table-string ((self gen-cs-table))
  (format nil "f ~D ~D ~D ~D ~{ ~6F~}" 
          (round (get-table-id self)) (stime self) (size self) (gen-num self) (param-list self)))

;================================================
; BPF-CS-TABLE
;================================================

(defclass bpf-cs-table (standard-cs-table om::BPF) 
  ((om::x-points :initform '(0 256 512) :initarg :x-points)
   (om::y-points :initform '(0 100 0) :initarg :y-points)
   (om::decimals :initform 0 :initarg :decimals)
   (id :initform nil :initarg :id :accessor id)
   (size :initform nil :type integer :accessor size)
   (stime :initform 0 :type number :accessor stime)))

(defmethod om::additional-class-attributes ((self bpf-cs-table)) '(om::color om::name size stime))


;;; ID can be NIL
(defmethod om::set-property ((object bpf-cs-table) (prop-id (eql :id)) val)
  (if (om::number-or-nil-t-or-nil val)
      (setf (id object) (om::number-or-nil-number val))
    (setf (id object) NIL)))

(defmethod om::get-properties-list ((self bpf-cs-table))
  (om::add-properties 
   (om::hide-properties 
    (call-next-method) 
    '(:action :interpol))
   "" 
   `((:id "ID" ,(om::make-number-or-nil) id)
     (:size "size" :number size)
     (:stime "start-time" :number stime))))
     
  
(defmethod cs-bpf-points ((self bpf-cs-table))
  (let* ((size (size self))
         (resampled-table (if (> (length (om::point-list self)) 
                                 (om::get-pref-value :libraries :omchroma-max-cs-points))
                              (om::reduce-n-points self (om::get-pref-value :libraries :omchroma-max-cs-points))
                             self))
          (pointx (om::x-points resampled-table))
          (pointy (om::y-points resampled-table)))    
     (setf pointx (cdr (mapcar 'round (om::om-scale pointx 0 (- size 1)))))     
     (append (loop for y in pointy
                   for last = 0 then x
                   for x in pointx
                   append (list y (- x last))) (last pointy))))


(defmethod cs-table-string ((self bpf-cs-table))
  (let* ((points (cs-bpf-points self)))
    (format nil 
            (format nil "f ~~D ~~D ~~D ~~D ~~{ ~~,~DF~~}" (om::decimals self))
            (get-table-id self) (stime self) (size self) (gen-num self) points)))

;================================================
; GEN TABLES SUBCLASSES OF BPF-CS-TABLE
;================================================

(om::defclass! GEN07 (bpf-cs-table) 
  ((om::x-points :initform '(0 256 512) :initarg :x-points)
   (om::y-points :initform '(0 100 0) :initarg :y-points)
   (om::decimals :initform 0 :initarg :decimals)
   (id :initform nil :initarg :id :accessor id :documentation "a table ID, leave NIL to set automatically"))
  (:icon 605))
(defmethod gen-num ((self Gen07)) 7)

(om::defclass! GEN-07 (bpf-cs-table) 
  ((om::x-points :initform '(0 256 512) :initarg :x-points)
   (om::y-points :initform '(0 100 0) :initarg :y-points)
   (om::decimals :initform 0 :initarg :decimals)
   (id :initform nil :initarg :id :accessor id :documentation "a table ID, leave NIL to set automatically"))
  (:icon 605))
(defmethod gen-num ((self Gen-07)) -7)

(om::defclass! GEN05 (bpf-cs-table) 
  ((om::x-points :initform '(0 256 512) :initarg :x-points)
   (om::y-points :initform '(0 100 0) :initarg :y-points)
   (om::decimals :initform 0 :initarg :decimals)
   (id :initform nil :initarg :id :accessor id))
  (:icon 605))
(defmethod gen-num ((self Gen05)) 5)

(om::defclass! GEN-05 (bpf-cs-table) 
  ((om::x-points :initform '(0 256 512) :initarg :x-points)
   (om::y-points :initform '(0 100 0) :initarg :y-points)
   (om::decimals :initform 0 :initarg :decimals)
   (id :initform nil :initarg :id :accessor id))
  (:icon 605))
(defmethod gen-num ((self Gen-05)) -5)

(om::defclass! GEN02 (bpf-cs-table) 
  ((om::x-points :initform '(0 256 512) :initarg :x-points)
   (om::y-points :initform '(0 100 0) :initarg :y-points)
   (om::decimals :initform 0 :initarg :decimals)
   (id :initform nil :initarg :id :accessor id))
 (:icon 605))
(defmethod gen-num ((self Gen02)) 2)

(om::defclass! GEN-02 (bpf-cs-table)
  ((om::x-points :initform '(0 256 512) :initarg :x-points)
   (om::y-points :initform '(0 100 0) :initarg :y-points)
   (om::decimals :initform 0 :initarg :decimals)
   (id :initform nil :initarg :id :accessor id))
  (:icon 605))
(defmethod gen-num ((self Gen-02)) -2)


;================================================
; FILE-CS-TABLE: tables loaded from a file
;================================================
(om::defclass file-cs-table (cs-table)
  ((file :accessor file :initform "" :initarg :file)
   (skiptime :accessor skiptime :initform 0 :initarg :skiptime)))

(defmethod initialize-instance ((self file-cs-table) &rest initargs)
  (call-next-method)
  (when (and (file self) (subtypep (type-of (file self)) 'om::sound))
    (setf (file self) (om::file-pathname (file self)))))

(defmethod cs-table-string ((self file-cs-table))
  (format nil "f ~D ~D ~D ~D ~s ~D 0 0" 
          (get-table-id self) 
          (stime self) 
          (size self) 
          (gen-num self) 
          (namestring (file self)) 
          (skiptime self)))

;================================================
; GEN TABLES SUBCLASSES OF FILE-CS-TABLE
;================================================

(om::defclass! GEN01 (file-cs-table) 
  ((id :initarg :id :accessor id :initform nil)
   (size :initarg :size :accessor size :initform nil :type integer)
   (stime :initarg :stime :accessor stime :initform 0 :type number )
   (file :initarg :file :accessor file :initform "" )
   (skiptime :accessor skiptime :initform 0 :initarg :skiptime))
  (:icon 605))

(defmethod gen-num ((self Gen01)) 1)

(om::defclass! GEN-01 (file-cs-table) 
  ((id :initarg :id :accessor id :initform nil)
   (size :initarg :size :accessor size :initform nil :type integer)
   (stime :initarg :stime :accessor stime :initform 0 :type number )
   (file :initarg :file :accessor file :initform "" )
   (skiptime :accessor skiptime :initform 0 :initarg :skiptime))
  (:icon 605))

(defmethod gen-num ((self Gen-01)) -1)


;================================================
; MULTI-CS-TABLE 
; ad-hoc type defined to handle slots controlling several tables
; (typically for 2D 3D trajectories)
;================================================

(om::defclass! multi-cs-table () 
  ((tables :accessor tables :initform nil :initarg :tables)))

#|
(defvar *bpf-cs-coerce* 'gen07)

(defmethod get-array-data ((array cs-evt) (controlvalue bpf-lib) numcols &optional type)
  (if (subtypep type 'multi-cs-table)
      (call-next-method)
    (get-array-data array (bpf-list controlvalue) numcols type)))

(defmethod coerce-array-data ((array cs-evt) controlvalue defdata type)
  (cond ((subtypep type 'cs-table) 
         (coerce-to-cs-table controlvalue defdata type))
        ((subtypep type 'multi-cs-table)  
         (coerce-to-multi-cs-table controlvalue defdata type))
        (t (call-next-method))))

(defun coerce-to-cs-table (controlvalue defdata type)
  (if (bpf-p controlvalue) 
      (loop for i from 1 to (length defdata) collect (data-to-cstable controlvalue type))
    (mapcar #'(lambda (item) (data-to-cstable item type)) defdata)))

                
(defmethod data-to-cstable ((self t) &optional type) nil)
(defmethod data-to-cstable ((self integer) &optional type) self)
(defmethod data-to-cstable ((self string) &optional type) self)
(defmethod data-to-cstable ((self cs-table) &optional type) self)

(defmethod data-to-cstable ((self bpf) &optional type) 
  (let ((table (make-instance (or (valid-final-cs-type type) *bpf-cs-coerce*)
                  :point-list (point-list self)
                  :decimals (decimals self))))
     (setf (bpfcolor table) (bpfcolor self))
     table))


(defun coerce-to-multi-cs-table (controlvalue defdata type)  
  (cond ((or (bpf-p controlvalue) 
             (subtypep (type-of controlvalue) 'bpf-lib)
             ;(subtypep (type-of controlvalue) 'multi-cs-table)   ;;; multi-cs-table is a bpf-lib
             )
         (let ((mcst (data-to-multi-cs-table controlvalue)))
           (loop for i from 1 to (length defdata) collect mcst)))
        ((listp controlvalue)
         (cond ((numberp (car controlvalue))
                (make-list (length defdata) :initial-element controlvalue))
               ((bpf-p (car controlvalue))
                (make-list (length defdata) :initial-element (data-to-multi-cs-table (make-instance 'bpf-lib :bpf-list controlvalue))))
               (t (loop for item in defdata collect (data-to-multi-cs-table item)))))
        (t (mapcar #'(lambda (item) (data-to-cstable item type)) defdata))))

(defmethod data-to-multi-cs-table ((self bpf-lib))
  (make-instance 'multi-cs-table 
                 :bpf-list (loop for bpf in (bpf-list self) collect
                                    (data-to-cstable bpf 'gen-07))))

(defmethod data-to-multi-cs-table ((self bpf))
  (make-instance 'multi-cs-table 
                 :bpf-list (list (data-to-cstable self 'gen-07))))

(defmethod data-to-multi-cs-table ((self multi-cs-table)) self)

(defmethod data-to-multi-cs-table ((self number)) self)

(defmethod data-to-multi-cs-table ((self list)) 
  (make-instance 'multi-cs-table 
                 :bpf-list (loop for bpf in self collect
                                    (data-to-cstable bpf 'gen-07))))

(defmethod data-to-multi-cs-table ((self t)) nil)

|#

;===============================================================
; SIMPLY COLLECT TABLES FROM VARIOUS TYPES OF INPUT
; used for global tables written in the score header 
;===============================================================

(defmethod csound-load-tables ((self null)) nil)
(defmethod csound-load-tables ((self t)) nil)
(defmethod csound-load-tables ((self function)) (funcall self))
(defmethod csound-load-tables ((self pathname)) (load self))

(defmethod csound-load-tables ((self cs-table))
   (if (integerp (id self))
       self
     (om::om-beep-msg "Warning: 1 global Csound table could not be included in score (missing ID)") 
     ))

(defmethod csound-load-tables ((self list))
  (remove nil
          (om::flat 
           (loop for item in self collect (csound-load-tables item)))))


(defmethod csound-load-tables ((self string)) 
  (let ((str (om::delete-spaces self)))
    (cond
     ((equal (elt str 0) #\f) (SCsT str))
     ((probe-file str) (csound-load-tables (pathname str))))))

;;; also from OM objects
(defmethod csound-load-tables ((self om:textbuffer)) 
  (om::textbuffer-eval self))

(defmethod csound-load-tables ((self om::collection))
   (mapcar 'csound-load-tables (om::obj-list self)))


;===============================================================
; COLLECT TABLES FROM ARRAY DATA 
; used when the type of a slot is 'cs-table' 
;===============================================================

;; returns a flat list of all tables, eventually converted to the correct type
(defmethod get-array-tables ((array cs-array)) 
  (remove-duplicates 
   (remove nil 
           (om::flat 
            (loop for n from 0 to (1- (om::fields array)) 
                  append
                  (let ((type (om::get-field-type array n))
                        (def-val (om::get-field-default array n)))
                    (when (and type ;; if no type, the rest of the tests will be wrong
                               (or (subtypep type 'cs-table)
                                   (subtypep type 'multi-cs-table)))
                      (remove nil
                              (mapcar 
                               #'(lambda (table) (unless (integerp table) ;;; the case where just a table number is given here (e.g. associated with a global table)
                                                   (coerce-table-type table type def-val)))
                               (om::get-field array n))
                              )
                      ))
                  ))
           )))


(defun coerce-table-type (table type def)
  (unless (subtypep (type-of table) type)
    (initialize-instance (change-class table (class-of def))))
  table)

;;; called to write the Csound lines and the table strings in the the score
;;; id can be a list if it is a temporarily assigned id
(defmethod get-table-id ((self CS-table)) 
  (if (consp (id self)) (car (id self)) (id self)))

(defmethod get-table-id ((self t)) self) ;; mmh
(defmethod get-table-id ((self number)) self)
(defmethod get-table-id ((self string)) (get-table-id-from-str self))
(defmethod get-table-id ((self multi-cs-table)) (mapcar 'get-table-id (tables self)))


;=======================================
; Table ID generator
;=======================================
(defvar *auto-init-table-id* 1000)
(defvar *table-id-counter* 0)
(defun init-array-tables-id-counter () (setf *table-id-counter* 0))
(defun gen-new-table-id ()
  (prog1 (+ *auto-init-table-id* *table-id-counter*)
    (setf *table-id-counter* (1+ *table-id-counter*))))

;;; if table ID = NIL or "?" a temporary ID is assigned '(id)
;;; tables with ID = a list will be reinitialized to "?" after score is written
     
(defmethod prepare-table ((self CS-table))
  (when (or (null (id self))
           (and (stringp (id self)) (string-equal (id self) "?")))
    (let ((i (gen-new-table-id)))
      (setf (id self) (list i)))
    self))
       
(defmethod prepare-table ((self string)) self)

(defmethod prepare-table ((self t)) nil)

(defmethod prepare-table ((self list)) 
 (om::flat (loop for item in self collect (prepare-table item))))

(defmethod prepare-table ((self multi-cs-table))
  (loop for item in (tables self) collect (prepare-table item)))


(defmethod reset-temp-table-id ((self t)) t)

(defmethod reset-temp-table-id ((self list)) 
   (loop for item in self do (reset-temp-table-id item)))

(defmethod reset-temp-table-id ((self CS-table))
  (when (consp (id self)) 
    (setf (id self) nil)))

(defmethod reset-temp-table-id ((self multi-cs-table))
  (loop for item in (tables self) do (reset-temp-table-id item)))


;=======================================
; Insput tables in array box
;=======================================

(defmethod om::get-array-data-from-input ((input cs-table) n type)
  (make-list n :initial-element (om::om-copy input)))

(defmethod om::get-array-data-from-input ((input om::bpf) n (type (eql 'cs-table)))
  (make-list n :initial-element (om::om-copy input)))



