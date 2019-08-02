
(in-package :om)


(defmethod Class-has-editor-p  ((self cr::vps)) t)

(defmethod get-win-ed-pos ((self cr::vps)) (om-make-point 500 300))

(defmethod get-editor-class ((self cr::vps)) 'vpseditor)

(defclass vpseditor (EditorView object-editor) 
   ((maxfreq :initarg :maxfreq :accessor maxfreq :initform 4000.0)
    (vps-amp-unit :initarg :vps-amp-unit :accessor vps-amp-unit :initform :lin)
    (amp-unit :initarg :amp-unit :accessor amp-unit :initform :lin)
    (db-min :initarg :db-min :accessor db-min :initform -60)
    (env-w :initarg :env-w :accessor env-w :initform 60)
    (editable-p :initarg :editable-p :initform nil :accessor editable-p)))


(defmethod editable-editor ((self t)) nil)
(defmethod editable-editor ((self cr::vps)) nil)
(defmethod editable-editor ((self cr::fql)) t)


(defmethod get-panel-class ((self vpseditor)) 'vpspanel)
(defmethod get-env-view ((self vpseditor)) 'specenv-panel)

(defclass vpspanel (om-view view-with-ruler-xy om-view-drag) 
              ((envpanel :accessor envpanel)
               (selection? :initform nil :accessor selection?)))

(defmethod editor ((self vpspanel)) (om-view-container self))

(defclass specenv-panel (om-view view-with-ruler-y) ())

(defmethod selected-object ((self vpseditor)) (object self))

(defmethod initialize-instance :after ((self vpseditor) &rest L) 
  (declare (ignore l))
  (let* ((Panel (om-make-view (get-panel-class self) 
                              :position (om-make-point 90 *titlebars-h*) 
                              :size (om-make-point (- (w self) 90) (- (h self) (+ 50 *titlebars-h* (get-control-h self) 20)))
                              :scrollbars :v))
         (envpanel (om-make-view (get-env-view self)
                              :position (om-make-point 30 *titlebars-h*) 
                              :size (om-make-point 50 (- (h self) (+ *titlebars-h* (get-control-h self))))
                              :bg-color *om-light-gray-color*
                              )))
    (setf (rulery panel) (om-make-view 'ruler 
                                       :axe 'y :zoom 10
                                       :assoc-view panel
                                       :position (om-make-point 0 *titlebars-h*) 
                                       :size (om-make-point 30 (- (h self) *titlebars-h*))))
    
    (setf (rangey panel) (list 0 (maxfreq self)))
    (setf (rangex panel) (list 0 0))

    (setf (rangey envpanel) (list 0 (maxfreq self)))

    (setf (rulery envpanel) (rulery panel))
    
    (set-units-ruler panel (rulery panel))
    
    (setf (envpanel panel) envpanel)
    
    (when (object self)
      (setf (editable-p self) (editable-editor (object self))))
    
    (om-add-subviews self panel envpanel (rulery panel))
    (setf (panel self) panel)
    
    (om-set-bg-color self *om-light-gray-color*)
    ))

(defmethod update-view-of-ruler  ((self vpspanel))
   "Sometimes update drawing is hard, you can redefine this method."
  (setf (maxfreq (om-view-container self)) (second (rangey self)))
  (om-invalidate-view self)
  (om-invalidate-view (envpanel self)))

(defmethod get-titlebar-class ((self vpseditor)) 'vps-titlebar)
(defclass vps-titlebar (editor-titlebar) ())

(defmethod init-titlebar ((self vpseditor))
  (let ((name (string (class-name (class-of (object self))))))
    (om-add-subviews (title-bar self)
                     (om-make-dialog-item 'om-static-text (om-make-point 10 4) 
                                          (om-make-point 200 ;;(om-string-size name *controls-font*) 
                                                         20)
                                        name
                                        :bg-color *editor-bar-color*
                                        :font *om-default-font2b*
                                        ))))

(defmethod update-subviews ((self vpseditor))
  (when (title-bar self)
     (om-set-view-size  (title-bar self) (om-make-point (w self) *titlebars-h*)))
  (om-set-view-size (panel self) (om-make-point (- (w self) 30 (env-w self)) (- (h self) (if (title-bar self) *titlebars-h* 0))))
  (om-set-view-position (panel self) (om-make-point (+ 30 (env-w self)) (if (title-bar self) *titlebars-h* 0)))
  
  (om-set-view-size (envpanel (panel self)) (om-make-point (env-w self) (- (h self) (if (title-bar self) *titlebars-h* 0))))
  
  (om-set-view-size (rulery (panel self)) (om-make-point 30 (- (h self) (if (title-bar self) *titlebars-h* 0))))
  ;(om-set-view-size  (control self) (om-make-point (w self) (get-control-h self)))
  ;(om-set-view-position (control self) (om-make-point 0 (- (h self) (get-control-h self))))
  (om-invalidate-view self))


(defmethod editor-null-event-handler :after ((self vpseditor))
  (let ((pos (om-mouse-position self)))
    (let ((view (om-find-view-containing-point self pos))
           (p nil) freq amp)
     (if (equal view (panel self))
       (setf p (om-convert-coordinates pos self (panel self))
             freq t amp nil)
       (if (equal view (envpanel (panel self)))
           (setf p (om-convert-coordinates pos self (envpanel (panel self)))
                 freq t amp t)))
     (when p
       (update-titlebar-info self p :freq freq :amp amp) 
       ))))


(defun point2freq (vpseditor point)
  (/ (* (- (h (panel vpseditor)) point) (maxfreq vpseditor)) (h (panel vpseditor))))

(defun freq2point (vpseditor freq)
  (let ((panel (panel vpseditor)))
    (round (- (h panel) (* (h panel) (/ (- freq (car (rangey panel))) 
                                        (- (cadr (rangey panel)) (car (rangey panel)))))))))


;; amp = 0.0 -> 1.0
;; a concvertir en 0 -> 1 en passant par l'echelle dB ou pas
(defmethod draw-value-for-amp ((self vpseditor) values)
  (if (equal (amp-unit self) :db)
      (om-scale (lin->db values) 0.0 1.0 (db-min self) 0.0)
    values))

; point = 0.0 -> 1.0
; convertir en amplitude lineaire
(defmethod real-amp-for-point ((self vpseditor) values)
  (if (equal (amp-unit self) :db)
      (db->lin (om-scale values (db-min self) 0.0 0.0 1.0))
    values))

;; point = 0.0 -> 1.0
;; value = 0.0 -> 1.0 ou -60 dB -> 0 dB
(defun amp-value-for-point (vpseditor point)
    (if (equal (amp-unit vpseditor) :lin)
        point
      (om-scale point (db-min vpseditor) 0.0 0.0 1.0)))
  
(defun point2amp (vpseditor p)
  (amp-value-for-point vpseditor (/ p (w (envpanel (panel vpseditor))))))

(defmethod update-titlebar-info ((self vpseditor) point &key (freq t) (amp t))
  (om-with-focused-view (title-bar self)
    (om-with-fg-color (title-bar self) *editor-bar-color*
      (om-fill-rect 110 2 (- (w (title-bar self)) 102) 22))
    (when freq
      (om-draw-string (- (w self) 80) 21 
                      (format () "f = ~5f Hz" (point2freq self (om-point-v point)))))
    (when amp
      (om-draw-string 110 21 
                      (format () "amp = ~5f ~A" (point2amp self (om-point-h point)) (if (equal (amp-unit self) :lin) "(lin.)" "dB"))))
    ))
  

(defmethod add-partiel-to-pane ((self vpspanel) where)
  (when (editable-p (editor self))
    (let ((vps (selected-object (om-view-container self)))
          (f (point2freq (om-view-container self) (om-point-v where))))
      (add-partiel vps f)
      (setf (selection? self) (list (position f (get-vps-freqs (selected-object (om-view-container self))) :test '=)))
      (om-invalidate-view self t)
      (om-invalidate-view (envpanel self)))))


(defun add-partiel (vps f)
  (let* (freqs amps bws)
    (if (member f (get-vps-freqs vps) :test '=) (om-beep)
      (progn
      (if (cr::get-bw vps)
          (multiple-value-setq (freqs amps bws)
              (sort-partials (cons f (get-vps-freqs vps)) (cons 0.9 (get-vps-amps vps)) (cons 0 (cr::get-bw vps))))
        ;(initialize-instance vps :the-list freqs :amplitudes amps :bwl bws))
        (multiple-value-setq (freqs amps)
              (sort-partials (cons f (get-vps-freqs vps)) (cons 0.9 (get-vps-amps vps))))
        ;(initialize-instance vps :the-list freqs :amplitudes amps))
          )
      (initialize-instance vps :the-list freqs :amplitudes amps :bwl bws))
      )))

(defmethod delete-partiels-from-pane ((self vpsPanel))
  (when (editable-p (editor self))
    (let* ((vps (selected-object (om-view-container self)))
           (freqs (position-remove (selection? self) (get-vps-freqs vps)))
           (amps (position-remove (selection? self) (get-vps-amps vps))))
      
      (setf (cr::the-list (selected-object (om-view-container self))) freqs)
      (setf (cr::amplitudes (selected-object (om-view-container self))) amps)
      (initialize-instance (selected-object (om-view-container self)))
      
      (setf (selection? self) nil)
    (om-invalidate-view self t)
    (om-invalidate-view (envpanel self))
    ;(report-modifications (om-view-container self))
    )))



(defmethod omselect-with-shift ((self vpsPanel) n)
  (if (member n (selection? self) :test '=)
    (setf (selection? self) (remove n (selection? self) :test '=))
    (push n (selection? self))))

(defmethod handle-key-event ((self vpsPanel) char)
   (case char
     (#\d (setf (amp-unit (editor self))
                (if (equal (amp-unit (editor self)) :lin) :db :lin))
          (om-invalidate-view self)
          (om-invalidate-view (envpanel self))
          (editor-null-event-handler (editor self)))
     (:om-key-delete (when (selection? self) (delete-partiels-from-pane self)))
     (otherwise nil)))

(defmethod om-view-click-handler ((self vpspanel) where)
  (let* ((n (click-in-freq self where)))
    (cond 
     ((om-command-key-p) (add-partiel-to-pane self where))
     (n 
      (if (om-shift-key-p) (omselect-with-shift self n)
        (when (not (member n (selection? self) :test '=))
          (setf (selection? self) (list n)))))
     (t (setf (selection? self) nil)))
    (om-invalidate-view self t)))


(defmethod om-draw-contents ((self vpspanel))
  (let* ((vps (selected-object (om-view-container self))))
    (draw-vps vps self -1 0 0 (maxfreq (om-view-container self)))
    ))

(defmethod draw-vps ((self t) panel time-fact begin-time end-time maxfreq &optional (maxamp 1.0))
  nil)

(defmethod draw-vps ((self cr::vps) panel time-fact begin-time end-time maxfreq &optional (maxamp 1.0))
  (let* ((x1 (if (= time-fact -1) 0 (round (* time-fact begin-time))))
         (x2 (if (= time-fact -1) (w panel) (round (* time-fact end-time))))
         (amps (when (get-vps-amps self)
                (draw-value-for-amp (editor panel) (om/ (get-vps-amps self) maxamp))
                ))
         (normamps (if amps (om- 0.7 (om* 0.7 amps)) (make-list (length (get-vps-freqs self)) :initial-element 0.0)))
         (currselection? (equal self (selected-object (om-view-container panel)))))
    (om-with-focused-view panel
      (loop for f in (get-vps-freqs self) 
            for i = 0 then (+ i 1) do 
            (let ((fh (round (- (h panel) (* (/ f maxfreq) (h panel)))))
                  (c (nth i normamps)))
              (om-with-fg-color panel (if (and currselection? (member i (selection? panel))) *om-steel-blue-color* (om-make-color c c c))
                      (om-with-line-size 2
                        (om-draw-line x1 fh x2 fh))))
            ))
    ))


(defun click-in-freq (envpanel where &optional (approx 4))
  (when (and (>= (om-point-h where) 0) (< (om-point-h where) (w envpanel)))
  (let* ((vps (selected-object (om-view-container envpanel)))
         (p (om-point-v where))
         rep)
    (loop for f in (get-vps-freqs vps)
          for i = 0 then (+ i 1)
          while (not rep) do
          (let ((pix (freq2point (om-view-container envpanel) f)))
            (when (and (> p (- pix approx) ) (< p (+ pix approx)) )
              (setf rep i))))
    rep)))

(defmethod check-change-amp (envpanel pos)
  (when (editable-p (om-view-container envpanel))
    (let ((n (click-in-freq envpanel pos)))
      (when n 
        (setf (nth n (cr::amplitudes (selected-object (om-view-container envpanel))))
              (real-amp-for-point (om-view-container envpanel) (/ (om-point-h pos) (w envpanel))))
        (om-invalidate-view envpanel)
        (om-invalidate-view (panel (om-view-container envpanel)))
        (update-titlebar-info (editor (panel (om-view-container envpanel))) pos :freq t :amp t)
        ))))


(defmethod release-change-amp (envpanel pos)
  (check-change-amp envpanel pos))

;(defmethod om-view-click-handler ((self specenv-panel) where)
;  (om-init-motion-functions self 'check-change-amp 'release-change-amp))

(defmethod om-draw-contents ((self specenv-panel))
  (let* ((vps (selected-object (om-view-container self))))
    (when vps 
      (draw-envelope vps self (maxfreq (om-view-container self)))
      )))

;;; vps have no amps
(defmethod draw-envelope ((self cr::vps) panel maxfreq)
  nil)


(defmethod draw-envelope ((self cr::fql) panel maxfreq)
  (let ((pts (list (list 0 (h panel))))
        ;(maxf (maxfreq (om-view-container panel)))
        (last '(0 0)))
    (om-with-focused-view panel
      (loop for f in (get-vps-freqs self) 
            for a in (draw-value-for-amp (om-view-container panel) (get-vps-amps self))
           ; while (< f maxf)
            do
            (let ((fh (round (- (h panel) (* (/ f maxfreq) (h panel)))))
                  (amp-w (round (* a (w panel)))))
              (pushr (list amp-w fh) pts)
              (setf last (list 0 fh))))
      (pushr last pts)
      (om-with-fg-color panel *om-gray-color*
        (om-fill-polygon pts))
      (om-with-fg-color panel *om-red2-color*
        (om-with-line-size 2
        (loop for p in pts do (om-draw-line 0 (cadr p) (car p) (cadr p)))))
      (om-draw-view-outline panel)
      )))

(defmethod get-menubar ((self vpseditor)) 
  (list (make-om-menu 'minifile :editor self)
        (make-om-menu 'windows :editor self)
        (make-om-menu 'help :editor self :disable '())))

(defmethod get-help-list ((self vpseditor))
  (list '((alt+clic "Add Pitch")
          (del "Delete Selected Pitch(es)")
          (("d") "Switch Lin/dB Amp.")
          )))

;;;===============================================
;;; VPS LIST

(defclass! vps-list () 
           ((elements :accessor elements :initarg :elements :initform nil))
           (:icon 660))

(defmethod merge-vps ((self vps-list))
  (let ((freqs (loop for vps in (elements self) append (get-vps-freqs vps)))
        (amps (loop for vps in (elements self) append (get-vps-amps vps))))
    (multiple-value-setq (freqs amps) (multiple-value-bind (f a) (sort-partials freqs amps)
                                        (remove-unisson f a)))
(make-model-obj 'FQL :freq freqs :amp amps)))

(defun remove-unisson (freqs amps)
  (let ((flist nil)
        (alist nil))
    (loop for f in freqs
          for a in amps do
          (unless (member f flist :test '=)
            (push f flist)
            (push a alist)))
    (values (reverse flist) (reverse alist))))
             


(defmethod class-has-editor-p ((self vps-list)) t)
(defmethod get-editor-class ((self vps-list)) 'vpslisteditor)

(defclass vpslisteditor (vpseditor) 
  ((selectedvps :accessor selectedvps :initarg :selectedvps :initform :all)))

(defmethod initialize-instance :after ((self vpslisteditor) &rest args)
  (when (object self)
    (setf (selectedvps self) :all)))

(defmethod (setf selectedvps) (n (self vpslisteditor))
  (setf (slot-value self 'selectedvps) n)
  (setf (selection? (panel self)) nil)
  (when (title-bar self)
    (om-set-dialog-item-text 
     (car (om-subviews (title-bar self)))
     (string+ "Selection: " (if (integerp n)
                                (integer-to-string n) 
                              (if n "ALL" "NIL"))))))



(defmethod get-panel-class ((self vpslisteditor)) 'vpslistpanel)
(defclass vpslistpanel (vpspanel) ())


(defmethod selected-object ((self vpslisteditor))
  (when (selectedvps self)
    (if (and (integerp (selectedvps self)) 
             (< (selectedvps self) (length (elements (object self)))))
        (nth (selectedvps self) (elements (object self))))))


(defmethod get-or-make-selected-object ((self vpslisteditor))
  (when (selectedvps self)
    (or (selected-object self)
        (merge-vps (object self)))))


(defmethod om-draw-contents ((self vpslistpanel))
  (when (selectedvps (editor self))
    (let* ((vpslist (elements (object (om-view-container self)))))
      (if (integerp (selectedvps (editor self)))
          (draw-vps (nth (selectedvps (editor self)) vpslist) self -1 0 0 (maxfreq (om-view-container self)))
        (loop for vps in vpslist do
              (draw-vps vps self -1 0 0 (maxfreq (om-view-container self)))
              )))))

(defmethod handle-key-event ((self vpslistPanel) char)
  (case char
    (:om-key-tab (let ((n (selectedvps (editor self))))
                   (if (integerp n)
                       (if (< (+ 1 n) (length (elements (object (editor self)))))
                           (setf (selectedvps (editor self)) (+ 1 n))
                         (setf (selectedvps (editor self)) :all))
                     (setf (selectedvps (editor self)) 0))
                   ))
    (:om-key-delete (when (selection? self) (delete-partiels-from-pane self)))
    (#\d (setf (amp-unit (editor self))
                (if (equal (amp-unit (editor self)) :lin) :db :lin))
         (om-invalidate-view self)
         (om-invalidate-view (envpanel self))
         (editor-null-event-handler (editor self)))
    (otherwise nil))
  (om-invalidate-view self)
  (om-invalidate-view (envpanel self))
  )



(defmethod get-help-list ((self vpslisteditor))
  (list '((tab "Select next segment")      
          (("d") "Switch Lin/dB amp. display")
          )))

;;;=============================================
;;; VPS-seq

(defclass! vps-seq (vps-list) 
  ((onsets :accessor onsets :initarg :onsets :initform nil)
   (durs :accessor durs :initarg :durs :initform nil))
  (:icon 660))

;;; ERROR !!!
(defmethod initialize-instance :after ((self vps-seq) &rest args)
  (let ((os (onsets self)))
    (unless os (setf os (list 0)))
    (loop while (< (length os) (length (elements self))) do
          (setf os (append os (last os))))
    (setf (onsets self) os)
    (loop while (< (length (durs self)) (length (elements self))) do
          (setf (durs self) (append (durs self) '(1.0))))))  
    

(defmethod get-obj-dur ((self vps-seq))
  (sec->ms (list-max (om+ (onsets self) (durs self)))))

(defmethod get-obj-beg ((self vps-seq))
  (sec->ms (car (onsets self))))



(defmethod Class-has-editor-p  ((self vps-seq)) t)

(defmethod get-win-ed-pos ((self vps-seq)) (om-make-point 500 300))

(defmethod get-editor-class ((self vps-seq)) 'vpsseqeditor)

(defclass vpsseqeditor (vpslisteditor) ())

(defmethod get-panel-class ((Self vpsseqeditor)) 'vpsseqpanel)

(defmethod initialize-instance :after ((self vpsseqeditor) &rest L) 
  (declare (ignore l))
  (setf (rulerx (panel self)) 
        (om-make-view 'sound-ruler 
                      :axe 'x :zoom 10
                      :assoc-view (panel self)
                      :position (om-make-point 90 (- (h self) 20)) 
                      :size (om-make-point (w (panel self)) 20)))

  (setf (rangex (panel self)) (list (get-obj-beg (object self)) (get-obj-dur (object self))))
  (setf (bounds-x (panel self)) (list (get-obj-beg (object self)) (get-obj-dur (object self))))
  (set-units-ruler (panel self) (rulerx (panel self)))
  (om-add-subviews self (rulerx (panel self)))
 )

(defmethod update-editor-after-eval ((self vpsseqeditor) val)
  (call-next-method)
  (setf (rangex (panel self)) (list (get-obj-beg (object self)) (get-obj-dur (object self))))
  (setf (bounds-x (panel self)) (list (get-obj-beg (object self)) (get-obj-dur (object self))))
  (set-units-ruler (panel self) (rulerx (panel self)))
  (update-subviews self))

(defmethod update-subviews ((self vpsseqeditor))
  (when (title-bar self)
     (om-set-view-size  (title-bar self) (om-make-point (w self) *titlebars-h*)))
  (om-set-view-size (panel self) (om-make-point (- (w self) 30 (env-w self)) (- (h self) (if (title-bar self) *titlebars-h* 0) 20)))
  (om-set-view-position (panel self) (om-make-point (+ 30 (env-w self)) (if (title-bar self) *titlebars-h* 0)))
  
  (om-set-view-size (envpanel (panel self)) (om-make-point (env-w self) (- (h self) 20 (if (title-bar self) *titlebars-h* 0))))
  
  (om-set-view-size (rulery (panel self)) (om-make-point 30 (- (h self) 20 (if (title-bar self) *titlebars-h* 0))))
  (om-set-view-size (rulerx (panel self)) (om-make-point (- (w self) 30 (env-w self)) 20))
  (om-set-view-position (rulerx (panel self)) (om-make-point (+ 30 (env-w self)) (- (h self) 20)))
  (om-invalidate-view self))

(defclass vpsseqpanel (vpslistpanel) 
  ((bounds-x :initform '(0 1) :accessor bounds-x :initarg :bounds-x)
   (draw-markers :initform t :accessor draw-markers :initarg :draw-markers)))


(defmethod update-titlebar-info ((self vpsseqeditor) point &key (freq t) (amp t))
  (om-with-focused-view (title-bar self)
    (call-next-method self point :freq nil :amp amp)
    (when freq
      (om-draw-string (- (w self) 80) 11 (format () "t = ~4F s" (ms->sec (+ (car (rangex (panel self)))
                                                                   (/  (* (om-point-h point) (- (second (rangex (panel self))) (car (rangex (panel self)))))
                                                                       (w (panel self)))))))
      (om-draw-string (- (w self) 80) 22 (format () "f = ~5f Hz" (/  (* (- (h (panel self)) (om-point-v point)) (maxfreq self)) (h (panel self))))))
    ))
       
(defmethod get-string-nom  ((Self vpsseqpanel) Num (axe (eql 'x)))
   (declare (ignore axe))
   (format nil "~,2F" (ms->sec num)))
                        

(defmethod handle-key-event ((self vpsseqpanel) char)
  (case char 
    (#\i (init-view self))
    (#\m (setf (draw-markers self) (not (draw-markers self)))
         (om-invalidate-view self))
    (otherwise (call-next-method))))

(defmethod get-help-list ((self vpsseqeditor))
  (list '((tab "Select next segment")      
          (("d") "Switch Lin/dB amp. display")
          (("i") "Reinit. view")
          (("m") "Show/Hide time markers")
          )))


(defmethod init-view ((self vpsseqpanel))
  (setf (rangex self) (list (get-obj-beg (object (editor self))) (get-obj-dur (object (editor self)))))
  (set-units-ruler self (rulerx self))
  (om-invalidate-view (rulerx self))
  (update-subviews (editor self)))
 
(defmethod om-draw-contents ((self vpsseqpanel))
  (let* ((vpsseq (object (om-view-container self)))
         (vpslist (elements vpsseq))
         ;(tfact (/ (w self) (get-obj-dur vpsseq)))
         (tfact (/ (w self) (ms->sec (- (cadr (rangex self)) (car (rangex self))))))
         )
    (loop for vps in vpslist 
          for t1 in (onsets vpsseq)
          for t2 in (om+ (onsets vpsseq) (durs vpsseq)) 
          for i = 0 then (+ i 1) do
          (om-with-focused-view self
            (when (and (integerp (selectedvps (editor self))) (= i (selectedvps (editor self))))
              (om-with-fg-color self oa::*om-select-color-alpha*
              (om-fill-rect (round (* tfact t1)) 0 (round (* tfact (- t2 t1))) (h self))))
            (when (draw-markers self)
              (om-with-dashline
                  (om-with-fg-color self (om-make-color 0.3 0.3 1)
                    (om-draw-line (round (* tfact t1)) 0 (round (* tfact t1)) (h self)))
                (om-with-fg-color self (om-make-color 1 0.3 0.3) 
                  (om-draw-line (round (* tfact t2)) 0 (round (* tfact t2)) (h self)))
                )))
          (draw-vps vps self tfact t1 t2 (maxfreq (om-view-container self)))
          )))


