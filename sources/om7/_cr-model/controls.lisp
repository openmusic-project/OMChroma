(in-package :om)


;;;===============================================================
;;; CR-CONTROL
;;; a structure for the specification of a matching between the CR-MODEL data with some classes of class-array

(defclass cr-ctrl (ompatchabs) 
  ((tmpmodeldata :accessor tmpmodeldata :initform nil :initarg :tmpmodeldata)))

(pushr 'cr-control *spec-new-boxes-types*)

(defmethod get-new-box-from-type ((type (eql 'cr-control)) position container)
  (let* ((name (mk-unique-name container "CR-CONTROL-PATCH"))
         (ctrl (make-instance 'cr-ctrl :name name :icon (list 655 (find-library "OMChroma")))))
    (omng-add-element ctrl (make-modeldatabox "MODEL DATA" (om-make-point 100 40)))
    (omNG-make-new-boxcall ctrl position name)))

(defmethod om-save ((self cr-ctrl) &optional (values? nil))
  `(let ((crctrl ,(call-next-method)))
        (change-class crctrl (find-class 'cr-ctrl))
        (setf (icon crctrl) (list 655 (find-library "OMChroma")))
        (setf (compiled? crctrl) nil)
        crctrl))


(defmethod compile-patch ((self cr-ctrl)) 
  "Generation of lisp code from the graphic boxes."
  (unless (compiled? self)
    ;; (print "compiling control")
    (let* ((boxes (boxes self))
           (out-slots (find-class-boxes boxes 'outslot))
           (in-boxes (append (find-class-boxes boxes 'modeldatabox) (sort (find-class-boxes boxes 'omin) '< :key 'indice)))
           (out-symb (code self))
           (oldletlist *let-list*) symbols body)
      (setf symbols (mapcar #'(lambda (thein) (setf (in-symbol thein) (gensym))) in-boxes))
        (setf *let-list* nil)
        (setf body `(values ,.(mapcar #'(lambda (theout)
                                          (gen-code theout 0)) out-slots)))
        (eval `(defun ,(intern (string out-symb) :om)  (,.symbols)
                  (let* ,*let-list* ,body)))
        
        (setf *let-list* oldletlist))
    (setf (compiled? self) t)
    ))




;;;==================================================================

(defclass! cr-control () 
           ((ctl :accessor ctl :initform nil :initarg :ctl)
            (args :accessor args :initform nil :initarg :args))
   (:icon 655))

;; compatibility with the former cr-control object
(defclass ctrl (cr-ctrl) ())

(defmethod upgrade-editor-box ((value cr-control) class name position)
  (when (ctl value) 
    (change-class (ctl value) (find-class 'cr-ctrl))
    (setf (icon (ctl value)) 655)
    (setf (name (ctl value)) name))
  (omNG-make-new-boxcall (or (ctl value) (make-instance 'cr-ctrl :name name :icon (list 655 (find-library "OMChroma")))) position name))

;;;==================================================================
;;; BOX

(defclass OMBoxCrCtrl (OMBoxAbsPatch) ())
(defmethod box-class ((self cr-ctrl)) 'OMBoxCrCtrl)

(defmethod make-outputs-of-frame ((self OMBoxCrCtrl) module)
  (let ((thenewout (om-make-view (get-out-class self)
                                 :position (om-make-point (- (round (w module) 2) 4)
                                                          (- (h module) 9))
                                 :size (om-make-point 8 8)
                                 :help-spec "connect to expand-model"
                                 :index 0)))  
    (setf (iconID thenewout) 227)
    (push thenewout (outframes module))
    (om-add-subviews module thenewout)))

(defmethod numouts ((self OMBoxCrCtrl)) 1)

(defmethod get-documentation ((self OMBoxCrCtrl)) (string+ "CR-CONTROL patch (use with cr-model/expand-model in OMChroma)"))
(defmethod get-object-insp-name ((self OMBoxCrCtrl)) (string+ "cr-control box"))

(defmethod allow-lock-button ((self OMBoxCrCtrl)) nil)

(defmethod omNG-box-value ((self OMBoxCrCtrl) &optional (num-out 0))
   (handler-bind ((error #'(lambda (c) 
                             (when *msg-error-label-on*
                               (om-message-dialog (string+ "Error while value the box " (string (name self)) " : " 
                                                        (om-report-condition c ))
                                               :size (om-make-point 300 200))
                               (clear-after-error self)
                               (om-abort)
                               ))))
     (compile-patch (reference self))
     (make-instance 'cr-control :ctl (reference self)
                    :args (mapcar #'(lambda (input) 
                                      (omNG-box-value input)) (inputs self)))))


(defmethod gen-code ((self OMBoxCrCtrl) numout)
  `(make-instance 'cr-control :ctl ,(reference self)
                  :args (list .,(decode self))))



;;;==================================================================
;;; EDITOR

(defclass crcontrolEditor (patchEditor)  ())
(defclass crcontrolPanel (patchPanel) ())

(defmethod get-editor-class ((self cr-ctrl)) 'crcontrolEditor)
(defmethod get-editor-panel-class ((self crcontrolEditor))  'crcontrolPanel)

(defmethod add-window-buttons  ((self crcontrolPanel))
  (call-next-method)
  (om-add-subviews self 
                   (om-make-view 'button-icon
                                            :iconID (list 665 (find-library "OMChroma"))
                                            :position (om-make-point 70 5)
                                            :size (om-make-point 24 24)
                                            :action
                                            #'(lambda(item) (declare (ignore item))
                                               (let* ((boxes (get-subframes self)) 
                                                      (i (length (list+ (find-class-boxes boxes 'tempOutFrame) 
                                                                        (find-class-boxes boxes 'outSlotFrame)
                                                                        (find-class-boxes boxes 'outFrame))))
                                                      (pos (om-make-point (+ 5 (* i 50)) 240)))
                                                 (omG-add-element self
                                                                  (make-frame-from-callobj 
                                                                   (make-new-output (mk-unique-name self "SLOT")
                                                                                    i pos
                                                                                    (list 665 (find-library "OMChroma"))
                                                                                    'outslot)))
                                                 (set-field-size self)
                                                 )))

    ))

(defmethod add-input-enabled ((self crcontrolPanel) type) (equal type 'in))
(defmethod add-output-enabled ((self crcontrolPanel) type) nil)

(defmethod box-allowed-p ((self omout) (panel crcontrolPanel)) 
  (om-beep-msg "No outputs in CR-CONTROL...")
  nil)

(defmethod box-allowed-p ((self selfTempIn) (panel crcontrolPanel)) 
  (om-beep-msg "No sepcial inputs in CR-CONTROL...")
  nil)


;;;==================================================================
;;; OUTSLOTS
(defclass outslot (omout) ())
(defclass outslotframe (outFrame) ())
(defmethod get-frame-class ((self outslot)) 'outslotframe)
(defmethod get-icon-box-class ((self outslot)) 'icon-box)

(defmethod omG-rename ((self outslotframe) new-name)
  (call-next-method)
  (omng-rename (object self) new-name))

(defmethod omNG-save ((self outslot) &optional (values? nil))
  (let* ((inputs (mapcar #'(lambda (input) (omNG-save input values?)) (inputs self))))
    `(om-load-slotout ,(name self) ,(indice self)  ,(om-save-point (frame-position self))
                      ',inputs ,(frame-name self) ,(om-save-point (frame-size self)))))

(defun om-load-slotout (name indice position inputs &optional fname fsize)
  (let ((newbox (make-new-slotout name indice (om-correct-point position))))
    (setf (frame-name newbox) fname)
    (setf (inputs newbox) (mapcar #'(lambda (input) (eval input)) inputs))
    (when fsize
      (setf (frame-size newbox) (om-correct-point fsize)))
    newbox))

(defun make-new-slotout (name indice posi)
  (let* ((icon (list 665 (find-library "OMChroma")))
         (theout (make-instance 'outslot
                   :name name
                   :icon icon
                   :reference nil
                   :indice indice)))
    (setf (frame-position theout) posi)
    (setf (inputs theout) (list (make-instance 'input-funbox
                                  :name "out"
                                  :value nil
                                  :doc-string "out")))
    theout))

(defmethod gen-code ((self outslot) numout)
  `(list ,(name self) ,(call-next-method)))
   
;;;==================================================================
;;; MODELDATA
;;; une boite vrtuelle pour recuperer les donnees du modele

(defclass! virtual-modeldata () 
  ((global-model :accessor global-model :initform nil)
   (local-time :accessor local-time :initarg :local-time :initform nil)
   (local-dur :accessor local-dur :initarg :local-dur :initform nil)
   (local-rank :accessor local-rank :initarg :local-rank :initform nil)
   (local-data :accessor local-data :initarg :local-data :initform nil))
  )

(defclass modeldatabox (OMTypedIn) 
  ((in-symbol :initform nil :initarg :in-symbol :accessor in-symbol)))

(defmethod OpenEditorframe ((self modeldatabox)) (not (dialog-message "This is a virtual model accessor. It has no real value.")))

(defclass modeldataframe (TypedInFrame) ())
(defmethod get-frame-class ((self modeldatabox)) 'modeldataframe)

(defmethod draw-typed-special ((self modeldataframe)) t)

(defun make-modeldatabox (name posi)
  (let ((rep (make-instance 'modeldatabox
                :name name
                :icon (list 667 (find-library "OMChroma"))
                :reference 'virtual-modeldata
                :indice 0)))
     (setf (frame-position rep) posi)
     (setf (defval rep) (make-instance 'virtual-modeldata))
     rep))

(defmethod omg-remove-element ((self crcontrolPanel) (box modeldataframe)) nil)
(defmethod allow-remove ((box modeldataframe) (panel crcontrolPanel)) nil)

(defmethod omNG-copy ((self modeldatabox))
  `(let* ((thein (make-instance ',(class-name (class-of self))
                   :name ,(name self)
                   :icon ,(copy-icon (icon self))
                   :reference 'virtual-modeldata
                   :indice 0)))
          (setf (frame-position thein) ,(om-copy-point (frame-position self)))
          (setf (frame-size thein) ,(om-copy-point (frame-size self)))
          (setf (defval thein) (make-instance 'virtual-modeldata))
     thein))

(defmethod omNG-save ((self modeldatabox) &optional (values? nil))
  (declare (ignore values?))
  `(om-load-modeldatabox ,(name self) ,(om-save-point (frame-position self)) ,(om-save-point (frame-size self))))

(defun om-load-modeldatabox (name  position  &optional fsize)
  (let ((newbox (make-modeldatabox name (om-correct-point position))))
    (when fsize
      (setf (frame-size newbox) (om-correct-point fsize)))
    (setf (defval newbox) (make-instance 'virtual-modeldata))
    newbox))


(defmethod gen-code ((self modeldatabox) numout)
   (if (= numout 0)
     `(global-model ,(in-symbol self))
     `(,(internp (nth numout (get-outs-name (defval self))) (symbol-package (reference self))) ,(in-symbol self))))


(defmethod omNG-box-value ((self modeldatabox) &optional (numout 0))
  (let ((md (tmpmodeldata (mycontainer self))))
    (when md (setf (defval self) md))
    (call-next-method)))

(defmethod rep-editor ((self virtual-modeldata) num)
   (let ((outs (get-outs-name self)))
     (if (= num 0)
       (global-model self)
       (eval `(,(internp (nth num outs) (symbol-package (type-of self))) ,self)))))




;;;==================================================================
;;; MAKE MATRICES
;;;==================================================================

(defmethod! expand-model ((my-model cr-model) ctl-model (target-class symbol))
  :icon 655
  (expand-model my-model ctl-model (make-instance target-class)))

(defmethod! expand-model ((my-model cr-model) (ctl-model cr-control) (target-class symbol))
  :icon 655
  (expand-model my-model ctl-model (make-instance target-class)))

(defmethod! expand-model ((my-model cr-model) (ctl-model cr-control) (target-class t))
  :icon 655
   (if (data my-model)
       (let ((n (length (elements (data my-model)))))
         (remove nil 
                 (loop for i from 0 to (- n 1)
                       collect (make-ctl-synthesis-event ctl-model target-class i my-model)
                       )))
     (om-beep-msg "Warning : CR-MODEL is not initialized!")
     ))

(defmethod! expand-model ((my-model cr-model) (ctl-model null) (target-class SynthesisEvt))
  :icon 655
  (if (data my-model)
      (let ((n (length (elements (data my-model)))))
        (remove nil 
                (loop for i from 0 to (- n 1)
                      collect (make-simple-synthesis-event target-class (car (nth i (time-struct my-model))) (nth i (elements (data my-model)))))))
    (om-beep-msg "Warning : CR-MODEL is not initialized!")))



(defmethod! expand-model ((my-model cr-model) (ctl-model textfile) (target-class SynthesisEvt))
  :icon 655
  (expand-model my-model (exp-list ctl-model) target-class))





;;; Chroma-style
(defmethod! expand-model ((my-model cr-model) (ctl-model list) (target-class SynthesisEvt))
  :icon 655
  (if (data my-model)
      (let ((n (length (elements (data my-model)))))
        (remove nil (cr::make-ctl2-list my-model ctl-model target-class)))
    (om-beep-msg "Warning : CR-MODEL is not initialized!")))





;;;;;========================================
;by serge 07082001
(defun mk-array (class numcols fixedargs  &rest argkeys) 
  (let* ((rep (make-instance class :numcols numcols))
         (initargs (get-all-initargs-of-class class))
         (precision 3) slot?)
    (loop for slot in (cdr (fixed-slots-list rep))
          for i = 0 then (+ i 1) do  
          (setf (slot-value rep slot) (nth i fixedargs)))
    (loop while argkeys do
          (cond
           ((setf slot? (car (member (string (car argkeys)) initargs :test 'string-equal :key 'name)))
            (pop argkeys)
            (setf (slot-value rep (internp (name slot?) (slot-package slot?))) (pop argkeys)))
           ;((string-equal (string (car argkeys)) "precision")
           ; (pop argkeys)
           ; (setf precision (pop argkeys)))
           ((array-special-keyword rep (car argkeys))
            (array-special-keyword-action rep (pop argkeys) (pop argkeys)))
           (t
            (pushr (list (pop argkeys) (pop argkeys)) (Lcontrols rep)))))
    ;(put-precision rep precision)
    (set-data rep)
    rep))
;;;;;========================================




(in-package chroma)

(defmethod make-ctl2-list ((my-model om::cr-model) ctl-model target-class
                           &key (outfile "yaka-out") (args-yaka-user nil)
                           (by-time nil))     
  (setf outfile  (get-cr-path :tmp :name outfile :type "ctl2"))
  (let ((n-fql (1- (om::model-nb-evts my-model)))      ; n-fql - 1 , nmarkers - 2
        resc-min resc-max gblamp-val )
    ; ? si outfile nil : stream pas vers fichier mais vers "load" ?
    (with-open-file  (outstream outfile :direction :output :if-exists :supersede)
      (format  outstream "(in-package :chroma)(defun cr::ctl2-result() (list ~%")
      (if by-time
        (setf resc-min (car (car (om::time-struct my-model))) resc-max (cadr (last-elem (om::time-struct my-model))))
        (setf resc-min 0 resc-max n-fql))
      ; FQL LOOP     
      (loop for i from 0 to n-fql
            do (cr::om-CTL2 target-class my-model (cr-interne-list ctl-model :chroma) i outstream args-yaka-user))
      (format outstream "))~%"))
    (load outfile))
  (cr::ctl2-result))

;; bof bof
(defmethod chroma::offset ((self om::cr-model)) 0)


(defun cr-interne-list (list package)
  (loop for elt in list 
        ;do (print elt)
        collect
        (cond ((listp elt) (cr-interne-list elt package))
              ((symbolp elt) (intern (symbol-name elt) package))
              (t elt))))

(defmethod om-CTL2 ((self om::SynthesisEvt) (my-model om::cr-model) ctl-model my-rank outstream 
                    args-yaka-user)
  (declare (special outstream my-model my-rank))
  ;(print ctl-model)
  (let*((my-time (car (nth my-rank (om::time-struct my-model))))
        (my-dur (- (cadr (nth (+ 1 my-rank) (om::time-struct my-model))) (car (nth (+ 1 my-rank) (om::time-struct my-model)))))
        (n-fql (1- (om::model-nb-evts my-model)))      ; n-fql - 1 , nmarkers - 2
        (my-fql (nth my-rank (om::elements (om::data my-model))))
        (my-ptl my-fql)
        (my-nev (length (om::get-vps-freqs my-fql)))
        string1)
    (declare (special n-fql my-time my-dur my-nev my-fql my-ptl))
    (when my-fql
      (when args-yaka-user (apply #'yaka-user args-yaka-user))
      (format outstream "(om::mk-array 'om::~a ~a ~%~%"
              (type-of self) my-nev) 
      (if (cr::get-gbl 'cr::ctl2-print) (format t "~a~%" my-time))
      ; A FAIRE : GLOBALSLOTS loop sur (first (multiple-value-list (om::get-slot-in-out-names self)))
      (cr::CTL2_global self ctl-model)
      ; KEYWORDS LOOP
      (cr::CTL2_keywords_loop self ctl-model)
      (format  outstream ") ~%~%")
      ))
  )


(in-package :om)

(defmethod make-simple-synthesis-event (target-class onset data)
  (when data 
    (let ((rep (make-instance (class-name (class-of target-class))
                 :numcols (length (get-vps-freqs data)))))
      (setf (action-time rep) onset)
      (setf (freq rep) (get-vps-freqs data))
      (setf (amp rep) (get-vps-amps data))
      rep)))


;;; text ctl: do nothing ??
(defmethod make-ctl-synthesis-event ((ctl list) target-class i model)
  (let* ((t1 (car (nth i (time-struct model))))
         (t2 (cadr (nth i (time-struct model))))
         (data (nth i (elements (data model))))
         rep)
    (when data 
      (setf rep (make-instance (class-name (class-of target-class))
                               :numcols (length (get-vps-freqs data))))
      (setf (action-time rep) t1))
    rep))

    
;;; visual ctl
(defmethod make-ctl-synthesis-event ((cr-ctrl cr-control) (target-class SynthesisEvt) i model)
  (let* ((rep nil)
         (t1 (car (nth i (time-struct model))))
         (t2 (cadr (nth i (time-struct model))))
         (dur (- t2 t1))
         (ncols nil)
         (fixedargs (make-list (- (length (fixed-slots-list target-class)) 1)))
         (data (nth i (elements (data model)))))
    (when data 
      (let ((modeldata (make-instance 'virtual-modeldata :local-time t1 
                                      :local-dur dur
                                      :local-data data
                                      :local-rank i))
            (realslots (mapcar 'name (get-all-initargs-of-class (type-of target-class))))
            slotslist
            argslist 
            (i -1))
        (setf (global-model modeldata) model)
        (setf slotslist (multiple-value-list (apply (intern (string (code (ctl cr-ctrl))) :om) (cons modeldata (args cr-ctrl)))))
        
        ;;; test
        (setf (tmpmodeldata (ctl cr-ctrl)) modeldata)
        
        (let ((pos (position "numcols" slotslist :key 'car :test 'string-equal)))
          (when pos 
            (setf ncols (cadr (nth pos slotslist)))
            (setf slotslist (append (first-n slotslist pos) (nthcdr (+ pos 1) slotslist)))))
        
        
        (loop for fsl in (fixed-slots-list target-class) do
              (unless (equal 'numcols fsl)
                (incf i)
                (let ((pos (position (string fsl) slotslist :key 'car :test 'string-equal)))
                  (if pos
                    (setf (nth i fixedargs) (cadr (nth pos slotslist))
                          slotslist (append (first-n slotslist pos) (nthcdr (+ pos 1) slotslist)))
                    ;; just for action time : set t1
                    (cond ((equal 'action-time fsl) (setf (nth i fixedargs) t1))
                          ((equal 'dur fsl) (setf (nth i fixedargs) 0))
                          (t nil))
                    ))))
              
        (loop for oneslot in slotslist do
              (when (member (car oneslot) realslots :test 'string-equal)
                (setf argslist (append argslist (list (string2initarg (car oneslot)) (cadr oneslot))))
            ;(setf (slot-value rep (intern (car oneslot) :om)) (cadr oneslot))
                ))
        (setf rep (apply 'mk-array (append 
                                      (list (class-name (class-of target-class)) 
                                          (or ncols (max 1 (length (get-vps-freqs data))))
                                          fixedargs)
                                      argslist)))
        rep
        ))))




(defmethod make-ctl-synthesis-event ((cr-ctrl cr-control) (target-class t) i model)
  (let* ((rep nil)
         (t1 (car (nth i (time-struct model))))
         (t2 (cadr (nth i (time-struct model))))
         (dur (- t2 t1))
         (data (nth i (elements (data model)))))
    (when data 
      (let ((modeldata (make-instance 'virtual-modeldata :local-time t1 
                                      :local-dur dur
                                      :local-data data
                                      :local-rank i))
            slotslist)
        (setf (global-model modeldata) model)
        (setf slotslist (multiple-value-list (apply (intern (string (code (ctl cr-ctrl))) :om) (cons modeldata (args cr-ctrl)))))
        
        ;;; test
        (setf (tmpmodeldata (ctl cr-ctrl)) modeldata)

        (when (and (find 'action-time (get-init-instance-slots-of-class (type-of target-class)) :key 'car :test 'equal)
                   (not (find "action-time" slotslist :test 'string-equal :key 'car)))
          (setf slotslist (cons (list "action-time" t1) slotslist)))
        (when (and (find 'dur (get-init-instance-slots-of-class (type-of target-class)) :key 'car :test 'equal)
                   (not (find "dur" slotslist :test 'string-equal :key 'car)))
          (setf slotslist (cons (list "dur" dur) slotslist)))

        (setf rep (make-instance (class-name (class-of target-class))))
        (loop for s in slotslist do
              (setf (slot-value rep (interne (car s))) (cadr s)))
        rep
        ))))



;;;;-------------------------------------------
; Une methode pour interpoler entre deux trucs

(defmethod! interpol-value ((list1 list) (list2 list) (nbsteps integer) i &optional profil)
  :icon 657
  (nth i (interpole-points list1 list2 nbsteps profil)))



;;;;------------------------------------------------------
; Compat chroma CTL2 : des methodes d'inspection du modele

(defmethod cr::get-norm-amp ((self cr-model) rang &optional (scaling-factor 1))
  (let ((thefql (nth rang (elements (data self)))))
    (if thefql
      (mapcar #'(lambda (y) (/ y (/ (model-max-amp self) scaling-factor ))) (get-vps-amps thefql))
      nil)))

(defmethod cr::begin-time ((self cr-model))
  (car (car (time-struct self))))

(defmethod cr::get-nth-time ((self cr-model) i)
  (car (nth i (time-struct self))))

