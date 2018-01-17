(in-package :om)

;============================================================
;CSOUND CLASSES
;============================================================

(defclass internCSEvt ()
  ((user-fun :initform nil :accessor user-fun)
   (numchan :initform 1  :initarg :numchan :accessor numchan)
   (precision :initform '(3) :type list :accessor precision)
   ))
   
;;; compat 
(defmethod (setf lprecision) (preci (self t)) nil)

;;; precision = une liste (un pour chaque field) ou une valeur (pour tt le monde)
(defmethod put-precision ((self internCSEvt) precision)
  (setf (precision self) precision))

(defmethod put-precision ((self t) precision) nil)


;;; get precision for field i
(defmethod get-precision ((self internCSEvt) &optional i)
  (if (numberp (precision self)) (precision self)
    (if (numberp i)
        (if (and (consp (precision self)) (< i (length (precision self))))
            (nth i (precision self))
          (car (precision self)))
      (car (precision self)))))

;=====================
; SUPERCLASS FOR ALL CSOUND CLASSES
;=====================

(defclass! CS-Evt (SynthesisEvt internCSEvt) 
   ((source-code :initform (load-buffer-textfile nil 'textfile "append") :allocation :class :type textfile :accessor source-code)
    (cs-inits :initform nil :allocation :class :type list :accessor cs-inits)
    (orc-header :initform nil :allocation :class :type list :accessor orc-header)
    (E-dels :initform 0 :initarg :E-dels :type number :accessor E-dels)
    (Durs :initform 1 :initarg :Durs :type number :accessor Durs)
    (InstID :initform 1 :allocation :class :accessor InstID)) 
   (:icon 601))

(defmethod special-keyword-index ((value (eql :precision)) (obj cs-evt)) :precision)

(defmethod get-parsing-fun ((self CS-EVT))
  (or (user-fun self) (call-next-method)))

(defmethod rep-editor ((self cs-evt) num)
  (cond ((equal num :precision) (precision self))
        (t (call-next-method))))

(defmethod omNG-copy ((self CS-Evt))
  `(let ((copy ,(call-next-method)))
     (put-precision copy ',(precision self))
     copy))

(defmethod omNG-save ((self CS-Evt) &optional (values? nil))
  `(let ((rep ,(call-next-method)))
     (put-precision rep ,(omng-save (precision self)))
     rep))

;=====================
;action-time
;=====================

(defmethod get-slot-in-out-names ((self CS-evt))
   (values '("self" "numcols" "action-time" "user-fun") '(nil 1 0 nil)
           '("Synthesis event" "Number of components [int]" "Event offset [s]" "A lambda function applied to each component")
           '(nil nil nil nil)))

(defmethod fixed-slots-list ((self CS-evt)) '(numcols action-time user-fun))

(defmethod get-obj-dur ((self CS-Evt)) 
   (let ((dura (loop for onset in (get-array-row self 0)
                     for dur in (get-array-row self 1)
                     ;;;minimize onset into start
                     maximize  (+ onset dur) into max-elt-dur
                     ;;finally (return (round (* 1000 (- dura start))))
                     finally (return max-elt-dur))))
     (round (* (+ (action-time self) dura) 1000))
     ))


;;;;==============================================================

(defmethod prepare-tables ((self CS-Evt))
  (let* ((initargs (get-all-initargs-of-class (type-of self))))
    (loop for slot? in initargs do
          (when (subtypep (thetype slot?) 'cs-table)
            (prepare-table (slot-value self (internp (name slot?) (slot-package slot?))))))))


(defmethod erase-temp-id-tables ((self CS-Evt))
  (let* ((initargs (get-all-initargs-of-class (type-of self))))
    (loop for slot? in initargs do
          (when (subtypep (thetype slot?) 'cs-table)
            (erase-temp-id-table (slot-value self (internp (name slot?) (slot-package slot?))))))))


(defmethod prepare-table ((self t)) nil)

(defmethod prepare-table ((self list)) 
  (mapcar 'prepare-table self))

(defmethod erase-temp-id-table ((self t)) t)

(defmethod erase-temp-id-table ((self list)) 
   (loop for item in self do
         (erase-temp-id-table item)))

        
;;;=======================================================
;CS TABLES
;;;=======================================================

(defvar *dynamic-cs-table-table* (make-hash-table :test 'equal))
(defvar *table-counter* 0)
(defvar *globals-csound-tables* nil)
(defvar *auto-init-table-id* 1000)
(defvar *bpf-cs-coerce* 'gen07)

;;;=======================================================

(defmethod get-table-id ((self CS-table)) 
   (car (list! (id self))))


;;; PREPARE TABLES:
;;; SET ID TO A LIST if "?"
;;; STORE IN THE TEMP HASH TABLE


(defmethod prepare-array-tables (array)
  (loop for r from 0 to (1- (num-array-slots array)) do
        (let ((row (get-array-row array r))
              (typ (row-type array r)))
          (prepare-array-slot-table array row typ))))

(defmethod prepare-array-slot-table ((array cs-evt) row type)
  (when (or (subtypep type 'cs-table)
            (subtypep type 'multi-cs-table))
    (mapc 'prepare-table row)))

;;; returns the ID (static or dynamically generated)
(defmethod prepare-table ((self CS-table))
  (cond 
   ((equal (id self) "?") 
    (let ((i (+ *auto-init-table-id* *table-counter*)))
      (setf *table-counter* (+ *table-counter* 1))
      (setf (id self) i)
      (setf (gethash i *dynamic-cs-table-table*) (cs-table-string self))
      (setf (id self) (list i))  ;;; tables with ID = a list will be reinitialized to "?" after score is written
      i))
   ((listp (id self)) 
    ;;; already prepared
    (id self))
   ((integerp (id self))
    (setf (gethash (id self) *dynamic-cs-table-table*) (cs-table-string self))
    (id self))))


;;; ERASE TABLES:
;;; RESET ID TO "?" when it was a list (= temp)
;;; STORE IN THE TEMP HASH TABLE

;(defmethod erase-temp-id-tables ((self CS-Evt))
;  (let* ((initargs (get-all-initargs-of-class (type-of self))))
;    (loop for slot? in initargs do
;          (when (or (subtypep (thetype slot?) 'cs-table)
;                    (subtypep (thetype slot?) 'multi-cs-table))
;            (erase-temp-id-table (slot-value self (internp (name slot?) (slot-package slot?))))))))

(defmethod erase-temp-id-tables ((self CS-Evt))
  (loop for r from 0 to (1- (num-array-slots self)) do
        (let ((row (get-array-row self r))
              (typ (row-type self r)))
          (when (or (subtypep typ 'cs-table)
                    (subtypep typ 'multi-cs-table))
            (erase-temp-id-table row)
            ))))
  
(defmethod erase-temp-id-table ((self t)) t)

(defmethod erase-temp-id-table ((self list)) 
   (loop for item in self do
         (erase-temp-id-table item)))

(defmethod erase-temp-id-table ((self CS-table))
   (when (listp (id self))
     (setf (id self) "?")))


;;;=============================================

(defmethod get-table-id ((self integer)) self)

;strings
(defmethod prepare-table ((self string))
  (let ((id (gen-table-id-from-str self)))
    (setf (gethash id *dynamic-cs-table-table*) self)
    id))

(defmethod get-table-id ((self string)) (gen-table-id-from-str self))

(defun gen-table-id-from-str (string)
  (let ((rep (delete-spaces string)))
    (if (equal (elt rep 0) #\f)
      (setf rep (string-until-space (delete-spaces (subseq rep 1))))
      (error "Table definition strings must start by f"))
    (when rep
      (read-from-string rep))))



;================================================
; CS-TABLE in class-array
;================================================

(defmethod get-array-data ((array cs-evt) (controlvalue bpf-lib) numcols &optional type)
  (if (subtypep type 'multi-cs-table)
      (call-next-method)
    (get-array-data array (bpf-list controlvalue) numcols type)))

(defmethod coerce-array-data ((array cs-evt) controlvalue defdata type)
  (cond ((subtypep type 'cs-table) 
         (corece-to-cs-table controlvalue defdata type))
        ((subtypep type 'multi-cs-table)  
         (corece-to-multi-cs-table controlvalue defdata type))
        (t (call-next-method))))

(defun corece-to-cs-table (controlvalue defdata type)
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


(defun corece-to-multi-cs-table (controlvalue defdata type)  
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


