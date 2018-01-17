(in-package :cr)

(om::defclass! PTL 
  (FQL q3::om3Dobj)  
  ((duration :initform 0
             :initarg :duration
             :accessor duration
             :type float
             :documentation "Duration in seconds")
   (origtime :initform 0
             :initarg :origtime
             :accessor origtime
             :type float
             :documentation "Original position in seconds")
   (entry-delays :initform nil
                 :initarg :entry-delays
                 :accessor entry-delays
                 :type list
                 :documentation "List of entry-delays (in seconds)")
   (durs :initform nil
         :initarg :durs
         :accessor durs
         :type list
         :documentation "List of durs (in seconds)")
   (transp_funs :initform nil
                :initarg :transp_funs
                :accessor transp_funs
                :type list
                :documentation "List of transposition factor functions (non normalisŽ)")
   (amp_funs :initform nil
             :initarg :amp_funs
             :accessor amp_funs
             :type list
             :documentation "List of amplitude scaler functions (max ampl in the fql) (non normalisŽ)"))
  (:documentation "FQL EXTENSION" )
    (:icon 624)
  )

(om::defclass! model-partials 
  (model-cseq q3::om3Dobj)
  (
   (ptl-list :accessor ptl-list
             :writer put-ptl-list
             :initform ()
             :initarg :ptl-list 
             :documentation "list of PTL objects")
   )
  (:documentation "Partials Model (functions)")
  (:icon 624))

(in-package :om)

(defmethod get-bpf-factors ((self bpf) dur z val offset)
  (let ((x (x-points self))
        (y (y-points self)))
    (setf x (om-scale  x 0 dur (first x) (car (last x))))
    (loop for i in x
          for j in y collect (list (+ offset (float i)) (float (* val j)) (float z)))))

(defmethod get-bpf-factors1 (self dur z val offset)
  (setf self (cr::fun->bpf self 5))
  (let ((x (x-points self))
        (y (y-points self)))
    (setf x (om-scale  x 0 dur (first x) (car (last x))))
    (loop for i in x
          for j in y collect (list (* 100 (+ offset (float i))) (float (+ val (* val j))) (float z)))))

(in-package :q3)


(defvar *separation* 4.0)
(setf *separation* 5.0)

(defmethod initialize-instance :after ((self cr::ptl) &rest points)
  (declare (ignore points))
  (setf (modele self) (create-ptl-group  self (attributtes self)))
  )

(defmethod initialize-instance :after ((self cr::model-partials) &rest points)
  (declare (ignore points))
  (setf (modele self) (create-mp-group  self (attributtes self)))
  )

(defun ptl-group (self c1 c2)
  (loop for item in (om::om/ (cr::the-list self) 200.0)
        for amp in (cr::amplitudes self)
        for offset in (cr::entry-delays self)
        for dur in (cr::durs self)
        for freqfact in (cr::transp_funs self)
        for ampfact in (cr::amp_funs self)
        for i = 0.0 then (+ i *separation*) 
        
        append (let* ((onset (+ (cr::origtime self) offset))
                      (funfactors (om::get-bpf-factors1 freqfact  dur i item onset))
                      (ampfactors (om::get-bpf-factors1 ampfact  dur i amp onset))
                      mod1 mod2)
                 (setf mod1 (create-line  (* 100 onset)  item  i (* 100 (+ onset dur))  item i c1))
                 (setf mod2 (create-poly-line  funfactors c2))
                 (list mod1 mod2))))

(defun create-ptl-group (self &optional attribute-set)
  (apply 'create-display-group  (ptl-group self 
                                           (q3:create-attribute-set
                                            :diffuse-color 0 0 0)
                                           (q3:create-attribute-set
                                            :diffuse-color 0.7 1 0.2))))

(defun create-mp-group (self &optional attribute-set)
  (apply 'create-display-group  
         (loop for ptl in (cr::ptl-list self)
               for i = 0 then (mod (+ i 1) 15) 
               append (ptl-group ptl 
                                 (q3:create-attribute-set
                                  :diffuse-color 0 0 0)
                                 (let ((color (om::color2list (nth i om::*16-color-list*))))
                                   (q3:create-attribute-set
                                    :diffuse-color (first color) (second color) (third color)))))))
