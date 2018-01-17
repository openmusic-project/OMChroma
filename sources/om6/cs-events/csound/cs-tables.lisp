
(in-package :om)

;;; redefined in OM2Csound lib preferences
(defvar *cs-max-points* 1024)
(defvar *def-table-size* 4097)

(defclass! CS-table () 
   ((id :initform "?" :initarg :id :accessor id)
    (size :initform nil :initarg :size :type integer :accessor size)
    (stime :initform 0 :initarg :stime :type number :accessor stime))
   (:icon 605))

(defmethod initialize-instance :after ((self cs-table) &rest args)
  (unless (size self)
    (setf (size self) *def-table-size*)))

;================================================
(defclass! gen-cs-table (CS-table) 
  ((gen-num :initform 1 :initarg :gen-num :type integer :accessor gen-num)
   (param-list :initform nil :initarg :param-list :type list :accessor param-list))
  (:icon 605))

(defmethod cs-table-string ((self gen-cs-table))
  (format nil "f ~D ~D ~D ~D ~{ ~6F~}" (round (id self)) (stime self) (size self) (gen-num self) (param-list self)))

;================================================
(defclass file-cs-table ()
  ((id :initform nil :initarg :id :accessor id)
   (csstring :initform nil :initarg :vect :accessor csstring)))

(defmethod cs-table-string ((self file-cs-table)) (csstring self))

(defun SCsT (string)
  (let ((table (make-instance 'file-cs-table
                 :id (gen-table-id-from-str string))))
    (setf (csstring table) string)
    (push table *globals-csound-tables*)
    table))

(defun CsT (id vect &key (size *def-table-size*) (gennum 7))
  (let ((table (make-instance 'file-cs-table :id id)))
    (setf (csstring table) (format nil "f ~D 0 ~D ~D ~{~S ~}" id size gennum vect))
    (push table *globals-csound-tables*)
    table))

;================================================
(defclass! bpf-cs-table (CS-table bpf) ()
   (:icon 605)
   (:default-initargs 
     :x-points '(0 256 512)
     :y-points '(0 100 0)))

(defmethod get-initval ((self bpf-cs-table))
  (make-instance (class-of self) :point-list (list (om-make-point 0 0)  (om-make-point 256 1) (om-make-point 512 0))))

(defmethod make-one-instance ((self bpf-cs-table) &rest slots-vals)
   (let ((table (call-next-method)))
     (setf (id table) (fourth slots-vals)
           (size table) (or (fifth slots-vals) *def-table-size*)
           (stime table) (sixth slots-vals))
     table))


(defun make-cs-table (class x-points y-points &optional (decimals 0) (id "?") (size 256) (stime 0))
   (let ((table (simple-bpf-from-list x-points y-points class decimals)))
     (when table
       (setf (id table) id
             (size table) size
             (stime table) stime)
       table)))

(defmethod cs-bpf-points ((self bpf-cs-table))
   (let* ((size (size self))
          (resampled-table (if (> (length (point-list self)) *cs-max-points*)
                               (reduce-n-points self *cs-max-points*)
                             self))
          (pointx (x-points resampled-table))
          (pointy (y-points resampled-table)))    
     (setf pointx (cdr (mapcar 'round (om-scale pointx 0 (- size 1)))))     
     (append (loop for y in pointy
                   for last = 0 then x
                   for x in pointx
                   append (list y (- x last))) (last pointy))))

(defmethod cs-table-string ((self bpf-cs-table))
  (let* ((points (cs-bpf-points self)))
    (format nil "f ~D ~D ~D ~D ~{ ~6F~}" (id self) (stime self)  (size self) (tnum self) points)))

(defmethod omNG-copy ((self bpf-cs-table))
  `(let ((newtable ,(call-next-method)))
     (when newtable
       (setf (id newtable) ,(if (listp (id self)) (car (id self)) (id self)))
       (setf (size newtable) ,(size self))
       (setf (stime newtable) ,(stime self)))
     newtable))

;================================================
(defclass! Gen07 (bpf-cs-table) () (:icon 605))
(defmethod tnum ((self Gen07)) 7)

(defclass! Gen-07 (bpf-cs-table) () (:icon 605))
(defmethod tnum ((self Gen-07)) -7)

(defclass! Gen05 (bpf-cs-table) () (:icon 605))
(defmethod tnum ((self Gen05)) 5)

(defclass! Gen-05 (bpf-cs-table) () (:icon 605))
(defmethod tnum ((self Gen-05)) -5)

(defclass! Gen02 (bpf-cs-table) () (:icon 605))
(defmethod tnum ((self Gen02)) 2)

(defclass! Gen-02 (bpf-cs-table) () (:icon 605))
(defmethod tnum ((self Gen-02)) -2)

(defclass! Gen01 (CS-table)
  ((file :accessor file :initform "" :initarg :file)
   (skiptime :accessor skiptime :initform 0 :initarg :skiptime))
  (:icon 605))

;added, ms 1603
(defclass! Gen-01 (CS-table)
  ((file :accessor file :initform "" :initarg :file)
   (skiptime :accessor skiptime :initform 0 :initarg :skiptime))
  (:icon 605))

(defmethod initialize-instance ((self gen01) &rest initargs)
  (call-next-method)
  (when 
    (and (file self) (equal 'sound (type-of (file self))))
    (setf (file self) (om-sound-file-name (file self)))))

(defmethod tnum ((self Gen01)) 1)
(defmethod tnum ((self Gen-01)) -1)

(defmethod cs-table-string ((self gen01))
  (format nil "f ~D ~D ~D ~D ~s ~D 0 0" (id self) (stime self) (size self) (tnum self) (om-path2cmdpath (file self)) (skiptime self)))


;================================================
;Coerce
;================================================
(defmethod get-super-default-value ((type (eql 'CS-table)))
  (make-instance (or (valid-final-cs-type type) *bpf-cs-coerce*) 
                 :point-list (list (om-make-point 0 0)  (om-make-point 256 1) (om-make-point 512 0))))

(defun valid-final-cs-type (type)
  (find type '(gen01 gen07 gen05 gen-05 gen-07)))

(defmethod* Objfromobjs ((Self bpf) (Type CS-table))
            (Objfromobjs (Objfromobjs self (make-instance *bpf-cs-coerce*)) type))

(defmethod* Objfromobjs ((Self bpf-cs-table) (Type gen-CS-table))
   (let* ((points (cs-bpf-points self))
          (size (size self)))
     (make-instance (or (valid-final-cs-type (type-of type)) *bpf-cs-coerce*)
       :id (id self)
       :stime (stime self)
       :decimals (decimals self)
       :size (size self)
       :gen-num (tnum self)
       :param-list points)))

(defmethod* Objfromobjs ((Self bpf) (Type bpf-cs-table))
   (let ((table (make-instance (type-of type)
                  :point-list (point-list self)
                  :decimals (decimals self))))
     (when table 
       (setf (bpfcolor table) (bpfcolor self)))
     table))

(defmethod* Objfromobjs ((Self bpf-cs-table) (Type bpf-cs-table))
   (let ((table (call-next-method)))
     (when table 
       (setf (stime table) (stime self))
       (setf (id table) (id self))
       (setf (size table) (size self))
       table)))


;================================================
;;; MULTI-CS-TABLE 
;================================================

(defclass! multi-cs-table (bpf-lib) ())

(defmethod prepare-table ((self multi-cs-table))
  (mapcar 'prepare-table (bpf-list self)))

(defmethod csound-load-tables ((self multi-cs-table))
   (mapcar 'csound-load-tables (bpf-list self)))

(defmethod get-table-id ((self multi-cs-table)) 
   (mapcar 'get-table-id (bpf-list self)))

(defmethod erase-temp-id-table ((self multi-cs-table)) 
   (mapcar 'erase-temp-id-table (bpf-list self)))

