(in-package :om)

;;;============================
;;; CR-MODEL
;;; data = a list of instances of chroma data classes (standard slot names, e.g. FREQ, AMP, etc.)
;;; timestruct = a temporal segmentation (list of dates)
;;;============================

(defclass! cr-model ()
  ((modeltype :accessor modeltype :initarg :modeltype :initform 'FQL)
   (datatype :accessor datatype :initarg :datatype :initform '1TRC)
   (datasrc :accessor datasrc :initarg :datasrc :initform nil)
   (data :accessor data :initform nil)
   (embed-data :accessor embed-data :initform nil)
   (storemode :accessor storemode :initform nil)
   (time-struct :accessor time-struct :initarg :time-struct :initform nil))
  (:icon 654))


(defmethod get-slot-in-out-names ((self cr-model))
   (values '("self" "modeltype" "datatype" "datasrc" "time-struct")
           '(nil 'FQL '1TRC nil nil)
           '("object" "class name of the vertical structure" "SDIF analysis type" "source file" "temporal data")
           '(nil 
             ((1 (("Frequencies List" 'FQL)
                  ("Partials" 'PTL)
                  ("OM Chord" 'CHORD)
                  ))) 
             ((2 (("1TRC (additive analysis)" '1TRC) 
                  ("1MRK (chord-seq analysis)" '1MRK)
                  ("1HRM (harmonic analysis)" '1HRM)
                  ("1FOB (FOF analysis)" '1FOB)
                  ("1FQ0 (fundamental freq. analysis)" '1FQ0)) ))
             nil nil)
           ))

(defmethod cr-model-p ((self cr-model)) t)
(defmethod cr-model-p ((self t)) nil)

(defmethod convert-datasrc ((datasrc sdiffile))
  (filepathname datasrc))

(defmethod convert-datasrc ((datasrc t))
  datasrc)


(defmethod initialize-instance ((self cr-model) &rest initargs)
   (declare (ignore initargs)) 
   (call-next-method)
   ;(set-data self)
   ;(when (time-struct self)
   ;  (setf (time-struct self) (extract-time-struct (slot-value self 'time-struct))))
   )

(defmethod make-one-instance ((self cr-model) &rest slots-vals)
  (let ((rep (call-next-method)))
    (set-data rep)
    rep))

(defmethod omNG-save ((self cr-model) &optional (values? nil))
  (if (embed-data self)
    `(let ((model ,(call-next-method)))
       (setf (data model) ,(omng-save (data self)))
       (setf (embed-data model) t)
       model)
    (call-next-method)))
    
(defmethod omNG-copy ((self cr-model))
  (if (embed-data self)
    `(let ((model ,(call-next-method)))
       (setf (data model) ,(omng-copy (data self)))
       (setf (embed-data model) t)
       model)
    (call-next-method)))


(defmethod object-box-specific-menu ((self cr-model) box)
  (list (om-new-leafmenu (if (embed-data self) "Do Not Embed Model Data" "Embed Model Data")
                   #'(lambda () (setf (embed-data self) (not (embed-data self))))
                   nil t)))


;;;=======================
;;; GET MODEL DATA
;;; see Model Processing for tranformations
(defmethod! model-data ((self list) &optional modif-func args)
  :icon 654
  (let ((rep (loop for elt in self collect (if elt (clone elt) nil))))
    (when modif-func
      (loop for i = 0 then (+ i 1)
         while (< i (length rep))
         do (let ((data (nth i rep)))
              (when data 
                (if args
                  (when (nth i args)
                    (setf (nth i rep) (apply modif-func (list data (nth i args)))))
                  (setf (nth i rep) (apply modif-func (list data))))
                ))))
    rep))

(defmethod! model-data ((self cr-model) &optional modif-func args)
  :icon 654
  (when (data self) 
    (model-data (elements (data self)) modif-func args)))



;;;=========================
;;; SPECIALIZED INSPECT
;; ((t1 ((ind freq amp phi) ... (ind freq amp phi))) 
;;  (t2 ((ind freq amp phi) ... (ind freq amp phi)))
;;  ...)
(defmethod get-analysis-data ((datatype (eql '1TRC)) datasrc beg end)
  (multiple-value-setq (data times) (getsdifdata datasrc 0 "1TRC" "1TRC" nil nil nil beg end))
  (mat-trans (list times data)))

;; ((t1 ((ind freq amp phi) ... (ind freq amp phi))) 
;;  (t2 ((ind freq amp phi) ... (ind freq amp phi)))
;;  ...)
(defmethod get-analysis-data ((datatype (eql '1HRM)) datasrc beg end)
  (multiple-value-setq (data times) (getsdifdata datasrc 0 "1HRM" "1HRM" nil nil nil beg end))
  (remove nil (mat-trans (list times data)) :key 'cdr))

;;
(defmethod get-analysis-data ((datatype (eql '1MRK)) datasrc beg end)
  (multiple-value-setq (data times) (getsdifdata datasrc 0 "1MRK" "1TRC" nil nil nil beg end))
  (mat-trans (list times data)))

;; 
(defmethod get-analysis-data ((datatype (eql '1FOB)) datasrc beg end)
  (multiple-value-setq (data times) (getsdifdata datasrc 0 "1FOB" "1FOF" nil nil nil beg end))
  (mat-trans (list times data)))

;; ((t1 f1) (t2 f2) ... (tn fn)) 
(defmethod get-analysis-data ((datatype (eql '1FQ0)) datasrc beg end)
  (multiple-value-setq (data times) (getsdifdata datasrc 0 "1FQ0" "1FQ0" 0 nil nil beg end))
  (mat-trans (list times (flat data))))

(defmethod get-data-times ((self pathname) datatype)
  (get-data-frames (load-sdif-file self) datatype))

(defmethod get-data-times ((self sdiffile) (datatype (eql '1TRC)))
  (get-times self 0 "1TRC" "1TRC" nil nil))

(defmethod get-data-times ((self sdiffile) (datatype (eql '1HRM)))
  (get-times self 0 "1HRM" "1HRM" nil nil))

(defmethod get-data-times ((self sdiffile) (datatype (eql '1MRK)))
  (get-times self 0 "1MRK" "1TRC" nil nil))

(defmethod get-data-times ((self sdiffile) (datatype (eql '1FOB)))
  (get-times self 0 "1FOB" "1FOF" nil nil))

(defmethod get-data-times ((self sdiffile) (datatype (eql '1FQ0)))
  (get-times self 0 "1FQ0" "1FQ0" nil nil))

;;;====================

;;; returns a list of markers from input 
(defmethod extract-time-struct ((self sdiffile))
  (remove-if (lambda (x) (< x 0)) (get-times self 0 "1MRK" nil nil nil)))

(defmethod extract-time-struct ((self list)) self)



(defmethod set-model-markers (markers datasrc (datatype (eql '1MRK)))
  (or markers (extract-time-struct datasrc)))

(defmethod set-model-markers (markers datasrc datatype)
  (let* ((f (get-data-times datasrc datatype))
         result)
    (when f
      (setf f (subseq f 0 (- (length f) 1)))
      (loop for mrk in (onsetsmrk2seg markers)
            do (let* ((diff_l1 (mapcar #'(lambda (x) (abs (- x (car mrk)))) f))
                      (diff_l2 (mapcar #'(lambda (x) (abs (- x (cadr mrk)))) f))
                      (min1 (list-min diff_l1))
                      (min2 (list-min diff_l2))
                      (pos1 (position min1 diff_l1))
                      (pos2 (position min2 diff_l2)))
                 (push (list (nth pos1 f) (nth pos2 f)) result)))
      (setf result (remove-duplicates result :test #'eq))
      (sort result #'< :key 'car)
      )))

(defun onsetsmrk2seg (onsets)
  (if (listp (car onsets))
      onsets
    (loop for (a b) on onsets by #'cdr
          when b collect (list a b))))

;;; fills the data slot using file and datatype + time-struct
(defmethod set-data ((self cr-model))
  (setf (time-struct self) (extract-time-struct (time-struct self)))
  (cond ((consp (datasrc self))
         (when (> (length (datasrc self)) (length (time-struct self)))
           (om-print "Warning some data was ignored because they were over the time structure"))
         (setf (time-struct self) (onsetsmrk2seg (time-struct self)))
         (setf (data self) (make-instance 'vps-seq 
                                          :elements (first-n (datasrc self) (length (time-struct self)))
                                          :onsets (mapcar 'car (time-struct self))
                                          :durs (mapcar #'(lambda (seg) (- (cadr seg) (car seg))) 
                                                        (time-struct self))))
         (setf (modeltype self) (class-name (class-of (car (elements (data self))))))
         )
        ((datasrc self)    
         (let ((ttt (cond (;;; 1 on a une time-struct à ajuster avec les temps de datasrc
                           (time-struct self) 
                           (set-model-markers (time-struct self) (datasrc self) (datatype self)))
                           (;;; 2 on peut déduire timestruct a partir de datasrc
                            (equal (datatype self) '1MRK)
                            (set-model-markers (time-struct self) (datasrc self) (datatype self)))
                           (;;; on n'a rien: pas de timestruct
                            (setf tdata (get-data-times (datasrc self) (datatype self))) 
                            (list 0.0 (or (car (last tdata 2)) 1.0))))))
           (setf (time-struct self) (onsetsmrk2seg (or ttt (time-struct self))))
           (when (time-struct self)
             (setf (data self) (compute-model-data (datatype self) (modeltype self) (datasrc self) (time-struct self))))
           ))
        (t 
         (setf (time-struct self) (onsetsmrk2seg (time-struct self)))))
  (unless (time-struct self) (setf (time-struct self) '((0.0 1.0))))
  (unless (data self)
    (setf (data self) (make-instance 'vps-seq 
                                     :elements (repeat-n 
                                                (make-model-obj (modeltype self) :freq nil :amp nil)
                                                (length (time-struct self)))
                                     :onsets (mapcar 'car (time-struct self))
                                     :durs (mapcar #'(lambda (seg) (- (cadr seg) (car seg))) 
                                                   (time-struct self)))))
  ;(make-list (- (length (time-struct self)) 1)
  )


;;;============================
;;; COMPUTE MODEL
;;;============================
;;; renvoie le nombre max d'elements
(defun max-chunk (data)
  (apply #'max (mapcar #'length data)))

(defun find-duration (n datalist)
  (let ((rep nil))
    (loop for frame in datalist
          do (when (member n (cadr frame) :key 'car :test '=)
               (pushr (car frame) rep)))
    (if rep
      (- (list-max rep) (list-min rep))
      0.0)))
  
(defun get-add-data (data numero colonne)
  (loop for frame in data
        collect (nth colonne (find numero frame :key #'car))))

;;;===============
;;; GENERAL METHOD
;;; markers is already well formed (onset dur)
(defmethod compute-model-data (datatype modeltype datasrc markers) 
  (let ((data-list nil))
    (loop for seg in markers
          do (om-print (format nil "Segment : ~D -> ~D" (car seg) (cadr seg)))
          (let* ((analysisdata (get-analysis-data datatype datasrc (car seg) (cadr seg)))
                 modeldata)
            (when analysisdata
              (setf modeldata (get-model-data datatype modeltype analysisdata :durmin 0.1)))
            (push modeldata data-list)))
    (make-instance 'vps-seq :elements (reverse data-list)
                   :onsets (mapcar 'car markers)
                   :durs (mapcar #'(lambda (seg) (- (cadr seg) (car seg))) markers))))
       
;;;====================
;;; SPECIALIZED METHODS

(defmethod get-model-data ((datatype (eql '1TRC)) modeltype datalist &key (sort t) (weighed-avg t) (durmin 0.0))
  ;;; datalist = ((T1 (ind freq amp phi) ...) ... (Tn (ind freq amp phi) ...))
  (let (npartials freq_moyenne max_amp freq_fun amp_fun time_list triplets dur
                  (amps nil)
                  (freqs nil))
    (setf min-ind-partial (list-min (mapcar #'car (flat (mapcar #'cadr datalist) 1))))
    (setf max-ind-partial (list-max (mapcar #'car (flat (mapcar #'cadr datalist) 1))))
    (loop for i from min-ind-partial to max-ind-partial
            initially (setf amps nil freqs nil)
            do
            (setf freq_fun (get-add-data (mapcar #'cadr datalist) i 1))
            (setf amp_fun (get-add-data (mapcar #'cadr datalist) i 2))
            (setf dur (find-duration i datalist))
            ;(print (list freq_fun amp_fun dur))
            (when (> dur durmin)
              (progn
                (setf triplets (mapcar #'list amp_fun freq_fun))
                (setf triplets (remove nil triplets :key 'car) ) ; ou delete ??
                (when (> (length triplets) 1)
                  ;amplitude moyenne
                  (setf amp_fun (mapcar #'first triplets))
                  (setf max_amp (apply #'max amp_fun))
                  (setf amp_fun (mapcar #'(lambda (x) (/ x max_amp)) amp_fun))
                  ;frequence moyenne
                  (setf freq_fun  (mapcar #'second triplets))
                  (setf freq_moyenne (if weighed-avg 
                                       (cr::moyenne_ponderee freq_fun amp_fun)
                                       (cr::moyenne freq_fun)))
                  (push freq_moyenne freqs)
                  (push max_amp amps)
                  ))))
    (if (not (null freqs))
        (progn (if sort (multiple-value-setq (freqs amps) (om-sort-partiels freqs amps)))
          (make-model-obj modeltype :freq freqs :amp amps)
          ))))



(defmethod get-model-data ((datatype (eql '1TRC)) (modeltype (eql 'PTL)) datalist &key (sort t) (weighed-avg t) (durmin 0.0))
  (let (npartials freq_moyenne max_amp freq_fun edel dur amp_fun time_list triplets subdata
                  (amps nil)
                  (edels nil)
                  (durs nil)
                  (freqs nil)
                  (amp_funs nil)
                  (freq_funs nil)
                  (init-time (car (car datalist))))
    (setf min-ind-partial (list-min (mapcar #'car (flat (mapcar #'cadr datalist) 1))))
    (setf max-ind-partial (list-max (mapcar #'car (flat (mapcar #'cadr datalist) 1))))
    (loop for i from min-ind-partial to max-ind-partial
          initially (setf amps nil freqs nil amp_funs nil freq_funs nil edels nil durs nil)
          do
          (setf freq_fun (get-add-data (mapcar #'cadr datalist) i 1))
          (setf amp_fun (get-add-data (mapcar #'cadr datalist) i 2))
          ;temps (X des fun)
          (setf time_list (mapcar #'car datalist))
          (setf triplets  (mapcar #'list amp_fun time_list freq_fun))
          ;; delete breakpoints absents de l'analyse
          (setf triplets (delete nil triplets :key 'car))
          (when (> (length triplets) 1) ;nombre minimum de points dans les bpf (pourrait tre 2)
            (setf time_list (mapcar #'second triplets))
            (setf dur  (- (last-elem time_list) (first time_list)))
            (when (> dur durmin) ;nombre minimum de points dans les bpf
              (progn
                ;amplitude maximum
                (setf amp_fun (mapcar #'first triplets))
                (setf max_amp (apply #'max amp_fun))
                ;normalisation des fonctions d'amplitudes ??
                ;(setf amp_fun (mapcar #'(lambda (x) (/ x max_amp)) amp_fun))
                ;frequence moyenne
                (setf freq_fun  (mapcar #'third triplets))
                (setf freq_moyenne (if weighed-avg 
                                     (cr::moyenne_ponderee freq_fun amp_fun)
                                     (cr::moyenne freq_fun))) 
                (push freq_moyenne freqs)
                (push max_amp amps)
                (setf freq_fun (mapcar #'(lambda (x) (cr::ratio->semitones (/ x freq_moyenne))) freq_fun))
                ;entry-delays
                (setf edel (- (first time_list) init-time))
                (push edel edels)
                (push dur durs)
                ;normalisation des fonctions en temps
                (setf freq_fun (cr::make_fun (om::flat (mapcar #'(lambda (x y) (list x y)) freq_fun time_list))))
                (cr::X-resc_fun freq_fun 0 1)
                (setf amp_fun (cr::make_fun (om::flat (mapcar #'(lambda (x y) (list x y)) amp_fun time_list))))
                (cr::X-resc_fun amp_fun 0 1)
                (push freq_fun freq_funs)
                (push amp_fun amp_funs)
                ))))
    (if (not (null freqs))
      (progn (if sort (multiple-value-setq (freqs amps edels durs freq_funs amp_funs) 
                        (cr::sort-partiels-and-everything freqs amps edels durs freq_funs amp_funs)))
             (make-instance 'cr::ptl
               :the-list (nreverse freqs) 
               :amplitudes (nreverse amps)                     
               :duration (- (car (last-elem datalist)) init-time)  
               :origtime init-time
               :entry-delays (nreverse edels)
               :durs (nreverse durs)
               :transp_funs (nreverse freq_funs)
               :amp_funs (nreverse amp_funs))
             ))))



(defmethod get-model-data ((datatype (eql '1HRM)) modeltype datalist &key (sort t) (weighed-avg t) (durmin 0.0))
  (get-model-data '1TRC modeltype datalist :sort sort :weighed-avg weighed-avg :durmin durmin))

(defmethod get-model-data ((datatype (eql '1MRK)) modeltype datalist &key (sort t) (weighed-avg t) (durmin 0.0))
  (let ((data (cadr (car datalist)))
        (amps nil) (freqs nil))
    (loop for p in data 
          initially (setf amps nil freqs nil)
          do
          (push (cadr p) freqs)
          (push (caddr p) amps)
          )
    (if (not (null freqs))
      (progn (if sort (multiple-value-setq (freqs amps) (om-sort-partiels freqs amps)))
             (make-model-obj modeltype :freq freqs :amp amps)
             ))))

(defmethod get-model-data ((datatype (eql '1FQ0)) modeltype datalist &key (sort t) (weighed-avg t) (durmin 0.0))
  ;;; datalist = ((t1 f1) ... (tn fn))
  (make-model-obj modeltype :freq (list (cr::moyenne (mapcar #'cadr datalist))))
  )



;;;====================
;;; SPECIALIZED TARGETS

(defmethod make-model-obj ((modeltype t) &key freq amp &allow-other-keys)
  nil)

(defmethod make-model-obj ((modeltype (eql 'FQL)) &key freq amp &allow-other-keys)
  (make-instance 'cr::fql 
    :the-list freq 
    :amplitudes amp))

(defmethod make-model-obj ((modeltype (eql 'CHORD)) &key freq amp &allow-other-keys)
  (let ((rep (make-instance 'chord 
               :lmidic (f->mc freq))))
    (when amp (setf (lvel rep) (om-round (om-scale amp 0 127))))
    rep))
  

;;;=============================
;;; PERSISTANCE & SG DES MODELES

;;; ne pas recalculer data
(defmethod objfromobjs ((self cr-model) (type cr-model))
  (let ((new (make-instance 'cr-model 
               :datatype (datatype self)
               :modeltype (modeltype self)
               :time-struct (time-struct self))))
    (setf (data new) (data self))
    new))

(defmethod objfromobjs ((self cr-model) (type sdiffile))
  (let ((path (om-choose-new-file-dialog :prompt "Choose a file name for saving your model")))
    (save-model self path)))

 (defmethod objfromobjs ((self sdiffile) (type cr-model))
   (let ((new (make-instance 'cr-model))
         (tlist nil)
         (datalist nil)
         (ptrfile (dynamic-open self)))
       (sdif::SdifFReadGeneralHeader ptrfile)
       (sdif::SdifFReadAllASCIIChunks ptrfile)
       (loop for item in (framesdesc self) do
             (when (equal "1MRK" (car item))
               (push (nth 1 item) tlist)
               (sdif-set-pos ptrfile (nth 3 item))
               (setf mlist (nth 4 item))
               (loop for mat in mlist do
                     (cond ((equal "1BEG" (car mat)) (setf bmat mat))
                           ((equal "1END" (car mat)) (setf emat mat))
                           ((equal "1TRC" (car mat)) (setf pmat mat))
                           (t nil)))
               (when pmat 
                 (let ((flist nil) (alist nil))
                   ;;; a parameter matrix :
                   ;;; cherche les notes dans tmplist et set pitch et velocity
                   (sdif-read-headers ptrfile (nth 3 item) (fifth pmat))
                   (loop for i = 0 then (+ i 1) while (< i (second pmat)) do
                         (sdif::SdifFReadOneRow ptrfile)
                         (let* ((freq (sdif::SdifFCurrOneRowCol ptrfile 2))
                                (amp (sdif::SdifFCurrOneRowCol ptrfile 3)))
                           (pushr freq flist)
                           (pushr amp alist)
                           ))
                   (when flist (push (make-instance 'cr::fql :the-list flist :amplitudes alist) datalist))
                   ))
               (setf bmat nil)
               (setf emat nil)
               (setf pmat nil)
               )
             )
       (dynamic-close self ptrfile)
       (setf (time-struct new) (reverse tlist))
       (setf (data new) (reverse datalist))
       new))


(defmethod save-model ((self cr-model) filename)
  (let ((outfile (sdif-open-file (namestring (pathname filename)) 1)))
    (sdif-write-header outfile (list (make-instance 'sdiftype 
                                       :struct 'F :signature "1MRK" 
                                       :description '(("1TRC" "model_data")))))
    (loop for time in (time-struct self) 
          for i = 0 then (+ i 1) do
          (let ((frame (make-instance 'sdifframe 
                         :signature "1MRK"
                         :ftime time
                         :streamId 0))
                (data (nth i (data self)))
                (matrix (make-instance 'sdifmatrix :signature "1TRC" 
                                       :elts 0 :fields 4)))
            (when data
              (setf (data matrix) 
                    (flat (loop for f in (cr::fql data) 
                          for n = 0 then (+ n 1) collect 
                          (list n f (or (nth n (cr::amplitudes data)) 1.0) 0))))
              (setf (elts matrix) (length (cr::fql data))))
            (setf (lmatrix frame) (list matrix))
            (save-sdif frame outfile)
            ))
    filename))



;;;============================
;;; MODEL EDITOR
;;;============================

(defmethod Class-has-editor-p  ((self cr-model)) t )

(defmethod get-editor-class ((self cr-model)) 'modeleditor)

(defclass modeleditor (vpsseqeditor) ()) 

(defmethod get-panel-class ((Self modeleditor)) 'modelpanel)

(defclass modelpanel (vpsseqpanel) ())

(defmethod good-val-p? ((self cr-model))
  (or (data self) 
      (om-beep-msg "The CR-MODEL data has not been initialized.")))


(defmethod make-editor-window ((class (eql 'modeleditor)) object name ref &key 
                               winsize winpos (close-p t) (winshow t) (resize t) (retain-scroll nil)
                               (wintype nil))
  (if (data object)
      (call-next-method class (data object) name ref :winsize winsize :winpos winpos :resize resize 
                        :close-p close-p :winshow winshow :resize resize
                        :retain-scroll retain-scroll :wintype wintype)
    (progn (om-beep-msg "The CR-MODEL data has not been initialized. Cannot open the editor.")
      nil)))

(defmethod update-editor-after-eval ((self modeleditor) val)
  (setf (object self) (data val))
  (om-invalidate-view self t))

