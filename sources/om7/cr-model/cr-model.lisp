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
; (c) Ircam 2000 - 2019
;Authors: C. Agon, M. Stroppa, J. Bresson, S. Lemouton


(in-package :cr)

;;;============================
;;; CR-MODEL for OM7
;;; J. Bresson 2019
;;;============================


;;; A wrapper-class around Chroma's VPS system
(defclass! cr-vps (om::data-frame)
  (
;(vps :accessor vps :initarg :vps :initform nil :documentation "a object of any type of cr::VPS")
   (freqs :accessor freqs :initarg :freqs :initform nil :documentation "a list of frequencies")
   (amps :accessor amps :initarg :amps :initform nil :documentation "a list of amplitudes")
   (dur :accessor dur :initarg :dur :initform 1000 :documentation "duration (ms)"))
  (:documentation "A container for frequency/amplitude values coming from sound analysis.

It is used as element/data-frame for CR-MODELs.
"))



(defclass! cr-model (om::data-stream)
  ((data :accessor data :initarg :data :initform nil :documentation "model data: raw analysis data, or a list of CR-VPS instances")
   (time-struct :accessor time-struct :initarg :time-struct :initform nil :documentation "a list of time-markers or pairs of markers (begin/end)")
   (max-amp :accessor max-amp :initform 1.0) ;;; just useful for normalization 
   )
  (:default-initargs :default-frame-type 'cr-vps)
  (:documentation "A sequence of CR-VPS objects representing a sound model.")
  (:icon 654))

;;; once the model is initialized we don't need the data... 
;; (defmethod om::excluded-slots-from-copy ((self cr-model)) 'data)

(defmethod om::get-obj-dur ((self cr-model)) 
  (om::sec->ms (cadr (car (last (time-struct self))))))


;;;=======================
;;; FOR THE EDITOR
;;;=======================
(defmethod om::y-range-for-object ((self cr-model)) '(8000 0))


(defclass cr-model-editor (om::data-stream-editor) ())
(defmethod om::object-has-editor ((self cr-model)) t)
(defmethod om::get-editor-class ((self cr-model)) 'cr-model-editor)
(defmethod om::editor-with-timeline ((self cr-model-editor)) nil)


(defmethod om::data-frame-text-description ((self cr-vps))
  (list "MODEL VPS"
        (format nil "(~A elements)"(length (freqs self)))))


(defmethod om::draw-data-frame ((frame cr-vps) editor i &optional (active t))
  
  (let* ((panel (om::active-panel editor))
         (x1 (om::x-to-pix panel (om::date frame)))
         (x2 (- (om::x-to-pix panel (+ (om::date frame) (om::sec->ms (dur frame)))) 2))
         (max-amp (max-amp (om::object-value editor))))
    
    (oa::om-draw-line x1 0 x1 (om::h panel) :line 1 :style '(4 4) :color (oa::om-make-color 1. 0.3 0.3))
    (oa::om-draw-line x2 0 x2 (om::h panel) :line 1 :style '(4 4) :color (oa::om-make-color 0.3 0.3 1.0))
    
    (if (zerop max-amp) (setf max-amp 1.0))
    (loop for f in (freqs frame)
          for a in (amps frame) do 
          (let ((y (om::y-to-pix panel f))
                (col (* (- 1 (/ a max-amp)) .5)))
            (oa::om-draw-line x1 y x2 y :line 2 :color (oa::om-make-color col col col .5))
            ))
    ))


;;;=======================
;;; GET MODEL DATA IN
;;;=======================

(defmethod! model-data-from-sdif ((sdfiff t) sdiftype beg end)
  :indoc '("an SDIF File object or pathname" "type of SDIF data" "begin time" "end time")
  :initvals '(nil "1TRC" nil nil)
  :menuins '((1 (("1TRC" "1TRC") ("1HRM" "1HRM") ("1MRK" "1MRK") ("1FOB" "1FOB") ("1REB" "1REB") ("1FQ0" "1FQ0"))))
  :doc "
Returns formatted analysis data from <sdiftype> frames in <sdiff>.

Data format:
'((t1 ((ind freq amp) ... (ind freq amp))) 
  (t2 ((ind freq amp) ... (ind freq amp)))
  ...)
"
  (cond 

   ((string-equal sdiftype "1TRC")
    (multiple-value-bind (data times)  
        (om::getSDIFdata sdfiff 0 "1TRC" "1TRC" '(0 1 2) nil nil beg end)
      (om::mat-trans (list times data))))

   ((string-equal sdiftype "1HRM")
    (multiple-value-bind (data times)  
        (om::getSDIFdata sdfiff 0 "1HRM" "1HRM" '(0 1 2) nil nil beg end)
      (remove nil (om::mat-trans (list times data)) :key 'cdr)))
   
   ((string-equal sdiftype "1MRK")
    (multiple-value-bind (data times)  
        (om::getSDIFdata sdfiff 0 "1MRK" "1TRC" '(0 1 2) nil nil beg end)
      (om::mat-trans (list times data))))
   
   ((string-equal sdiftype "1FOB")
    (multiple-value-bind (data times)  
        (om::getSDIFdata sdfiff 0 "1FOB" "1FOF" '(0 1) nil nil beg end)
      (om::mat-trans (list times (loop for f in data 
                                       collect (loop for elements in f 
                                                     for i from 0 
                                                     collect (list i (car elements) (cadr elements))))))))
   
   ((string-equal sdiftype "1REB")
    (multiple-value-bind (data times)  
        (om::getSDIFdata sdfiff 0 "1REB" "1RES" '(0 1) nil nil beg end)
      (om::mat-trans (list times (loop for f in data 
                                       collect (loop for elements in f 
                                                     for i from 0 
                                                     collect (list i (car elements) (cadr elements))))))))

   ((string-equal sdiftype "1FQ0")
    (multiple-value-bind (data times)  
        (om::getSDIFdata sdfiff 0 "1FQ0" "1FQ0" 0 nil nil beg end)
      (om::mat-trans (list times (loop for f in data collect (list (list 1 (car f) 1.0)))))))
   ))


;;;===================================
;;; MAKE FQL FRAMES FROM ANALYSIS DATA
;;;===================================

;;; returns the list of (time freq amp) fpr partial #n
(defun get-partial (n datalist)
  (loop for frame in datalist
        for partial-frame = (find n (cadr frame) :key 'car :test '=)
        when partial-frame
        collect (list (car frame) (nth 1 partial-frame) (nth 2 partial-frame))))


;;; datalist = ((T1 (ind freq amp phi) ...) ... (Tn (ind freq amp phi) ...))
(defmethod make-cr-frame-from-data (datalist &key (sort t) (weighed-avg t) (durmin 0.0))
  
  (let* ((partial_nums (mapcar #'car (om::flat (mapcar #'cadr datalist) 1)))
         (min-ind-partial (om::list-min partial_nums))
         (max-ind-partial (om::list-max partial_nums))
         (amps nil)
         (freqs nil))
    
    (when min-ind-partial
      (loop for i from min-ind-partial to max-ind-partial
            for partial-frames = (get-partial i datalist)
            do 
            (when (print partial-frames) 
              
              (let* ((partial-times (mapcar #'car partial-frames))
                     (partial-dur (- (apply #'max partial-times) (apply #'min partial-times)))
                     (freq_fun (mapcar #'cadr partial-frames))
                     (amp_fun (mapcar #'caddr partial-frames))
                     (max_amp nil)
                     (mean_freq nil))
                
                (when (or (> partial-dur durmin) 
                        (= 1 (length partial-times)))  ;;; we are _probably_ with some MRK / cseq kind of data...
                  
                  (setf max_amp (apply #'max amp_fun))
                  (when (plusp max_amp) (setf amp_fun (mapcar #'(lambda (x) (/ x max_amp)) amp_fun)))
                  (setf mean_freq (if weighed-avg 
                                      (/ (apply #'+ freq_fun) (length freq_fun))
                                    (/ (apply #'+ (mapcar #'* freq_fun amp_fun)) (apply #'+ amp_fun))))

                  (push mean_freq freqs)
                  (push max_amp amps)
                  )))
            ))
    
    (when (and freqs sort)
      ;;; in principle this is not necessary
      (multiple-value-setq (freqs amps)
          (values-list (om::mat-trans (sort (om::mat-trans (list freqs amps)) #'< :key #'car)))))
    
    (make-instance 'cr-vps :freqs freqs :amps amps)))
   

;;;===================================
;;; GET/ADJUST TIME STRUCTURE
;;;===================================

(defmethod! model-time-from-sdif ((sdiff t) sdiftype)
  
  :indoc '("an SDIF File object or pathname" "type of SDIF data")
  :initvals '(nil "1TRC")
  :menuins '((1 (("1TRC" "1TRC") ("1HRM" "1HRM") ("1MRK" "1MRK") ("1FOB" "1FOB") ("1REB" "1REB") ("1FQ0" "1FQ0"))))
  :doc "Returns time list from <sdiftype> frames in <sdiff>."
  
  (cond 
   ((string-equal sdiftype "1TRC")
    (om::getSDIFtimes sdiff 0 "1TRC" "1TRC" 0.0 nil))

   ((string-equal sdiftype "1HRM")
    (om::getSDIFtimes sdiff 0 "1HRM" "1HRM" 0.0 nil))
   
   ((string-equal sdiftype "1MRK")
    (om::getSDIFtimes sdiff 0 "1MRK" "1TRC" 0.0 nil))
   
   ((string-equal sdiftype "1FOB")
    (om::getSDIFtimes sdiff 0 "1FOB" "1FOF" 0.0 nil))

   ((string-equal sdiftype "1REB")
    (om::getSDIFtimes sdiff 0 "1REB" "1RES" 0.0 nil))
   
   ((string-equal sdiftype "1FQ0")
    (om::getSDIFtimes sdiff 0 "1FQ0" "1FQ0" 0.0 nil))
   ))
  

(defun markers-to-segments (markers)
  (if (listp (car markers))
      markers ;; already formatted
    (loop for (a b) on markers by #'cdr
          when b collect (list a b))))


(defmethod adjust-model-segments (segments data)
  (let ((data-time-list (mapcar #'car data)))
    (if data-time-list
        (remove nil
                (loop for seg in segments
                      collect (let* ((diff_l1 (mapcar #'(lambda (x) (abs (- x (car seg)))) data-time-list))
                                     (diff_l2 (mapcar #'(lambda (x) (abs (- x (cadr seg)))) data-time-list))
                                     (min1 (om::list-min diff_l1))
                                     (min2 (om::list-min diff_l2))
                                     (pos1 (position min1 diff_l1))
                                     (pos2 (position min2 diff_l2))
                                     (t1 (nth pos1 data-time-list))
                                     (t2 (nth pos2 data-time-list)))
                                
                                (unless (= t1 t2) (list t1 t2))))
                ))
    ))


;;;===================================
;;; CR-MODEL INIT
;;;===================================

(defmethod om::om-init-instance ((self cr-model) &optional initargs)
  
  ;;; format list to list of segments (if needed)
  ;;; ... or sets a default time-struct
  (setf (time-struct self) (or (markers-to-segments (time-struct self)) 
                               (list (list 0.0 1.0)))) 
  
  (when (data self)
    ;;; adjust time-struct to the actual data
    (setf (time-struct self) (adjust-model-segments (time-struct self) (data self))))
  
  ;;; generate the actual sequence from data + time-struct
   (om::data-stream-set-frames 
       self
       
       (cond 
        
        ;;; (data self) is already a list of cr-vps
        ((and (consp (data self))
              (om::list-subtypep (data self) 'cr-vps)) 
         
         (when (> (length (data self)) (length (time-struct self)))
           (om::om-print "Warning some data was ignored because they were over the time structure" "cr-model"))
         
         (loop for seg in (time-struct self)
               for i from 0
               collect (let ((vps-frame (nth i (data self))))
                         (om::item-set-time vps-frame (om::sec->ms (car seg)))  
                         (setf (dur vps-frame) (- (cadr seg) (car seg)))
                         vps-frame))
         )
   
        ;;; (data self) is (in principle) a raw list of anamysis data
        ((data self)
         
         (loop for seg in (time-struct self)
               do (om::om-print (format nil "Segment : ~D -> ~D" (car seg) (cadr seg)))
               collect (let* ((frame-data (remove-if #'(lambda (element) (or (< (car element) (car seg))
                                                                            (> (car element) (cadr seg))))
                                                    (data self)))
                              (vps-frame (make-cr-frame-from-data frame-data :durmin 0.1)))
                         
                         (om::item-set-time vps-frame (om::sec->ms (car seg)))
                         (setf (dur vps-frame) (- (cadr seg) (car seg)))
                         vps-frame))
         )
         
        (t ;;; no data :(
           (loop for seg in (time-struct self) collect 
                 (make-instance 'cr-vps :date (car seg) :dur (- (cadr seg) (car seg))))
           )
        ))

   (setf (max-amp self)
         (loop for frame in (om::data-stream-get-frames self) maximize
               (apply #'max (amps frame))))
   (print (list "AMPLITUDE:" (max-amp self)))
  
  (call-next-method))
 



;;;=======================
;;; GET MODEL DATA OUT
;;; see Model Processing for tranformations
;;;=======================

(defmethod! model-data ((self list) &optional modif-func args)
  :icon 654
  (let ((rep (loop for elt in self collect (if elt (om::clone elt) nil))))
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
    (model-data (om::data-stream-get-frames self) modif-func args)))



;;;====================================
;;; SDIF EXPORT OF A CR-MODEL
;;;====================================

(defmethod objfromobjs ((self cr-model) (type om::sdiffile))
  (let ((path (om::om-choose-new-file-dialog :prompt "Choose an SDIF file name your model")))
    (when path (save-as-sdif self path))))
  

(defmethod save-as-sdif ((self cr-model) path)
  (let ((sdiffileptr (sdif::sdif-open-file (namestring path) sdif::ewritefile)))
    (if sdiffileptr
        (unwind-protect 
            (progn (sdif::SdifFWriteGeneralHeader sdiffileptr)
              (om::sdif-write (om::default-om-NVT) sdiffileptr)
              (om::sdif-write-types sdiffileptr 
                                (list (make-instance 'om::sdiftype :struct 'om::F :signature "1MRK" 
                                                     :description '(("1TRC" "model data")))))
              (sdif::SdifFWriteAllASCIIChunks sdiffileptr)
                  
              (let ((datalist (loop for time in (time-struct self)
                                    for frame in (om::data-stream-get-frames self)
                                    when (vps frame) 
                                    collect (list time (loop for f in (freqs frame)
                                                             for i from 0
                                                             collect (list i f (or (nth i (amps frame)) 1.0) 0))))))
                
                (loop for frame in (om::make-1MRK-frames datalist) do
                      (om::sdif-write frame sdiffileptr))
                ))
          
          (sdif::SDIFFClose sdiffileptr))
      (om::om-beep-msg "Could not open file for writing: ~A" path))
    path))

    


