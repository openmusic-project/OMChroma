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
;;; Equivalent of CR-MODEL in OM#
;;; J. Bresson 2019
;;;============================

;;; A super-simple alternative to the VPS system
(defclass! cr-partials ()
  ((freqs :accessor freqs :initarg :freqs :initform nil :documentation "a list of frequencies")
   (amps :accessor amps :initarg :amps :initform nil :documentation "a list of amplitudes"))
  (:documentation "A container for frequency/amplitude values coming from sound analysis.

It is used as element/data-frame for CR-MODELs.
"))

(defmethod cr-partials-freqs ((self t)) nil)
(defmethod cr-partials-amps ((self t)) nil)

(defmethod cr-partials-freqs ((self cr-partials)) (freqs self))
(defmethod cr-partials-amps ((self cr-partials)) (amps self))


;;; A wrapper-class around Chroma's VPS system
(defclass! cr-frame (om::data-frame)
  ((vps :accessor vps :initarg :vps :initform nil :documentation "a object of any type of cr::VPS")
   (dur :accessor dur :initarg :dur :initform 1000 :documentation "duration (ms)"))
  (:documentation "A container for frequency/amplitude values coming from sound analysis.
It is used as element/data-frame for CR-MODELs.
"))

;;; specialize type with eql-specializers
(defmethod make-cr-frame-contents ((type t) freqs amps)
  (declare (ignore type))
  (make-instance 'cr-partials :freqs freqs :amps amps))



(defclass! cr-model (om::data-stream)
  ((data :accessor data :initarg :data :initform nil :documentation "model data: raw analysis data, or a list of CR-VPS instances")
   (time-struct :accessor time-struct :initarg :time-struct :initform nil :documentation "a list of time-markers or pairs of markers (begin/end)")
   (data-type :accessor data-type :initform 'cr::fql)
   (max-amp :accessor max-amp :initform 1.0) ;;; just useful for normalization 
   )
  (:default-initargs :default-frame-type 'cr-vps)
  (:documentation "A sequence of CR-VPS objects representing a sound model.")
  (:icon 654))

;;; once the model is initialized we don't need the data... 
(defmethod om::excluded-slots-from-copy ((self cr-model)) (append (call-next-method) '(data)))

(defmethod om::additional-class-attributes ((self cr-model)) '(data-type))
(defmethod om::get-obj-dur ((self cr-model)) 
  (if (time-struct self)
      (om::sec->ms (cadr (car (last (time-struct self)))))
    1000))

;;; used in CR::ctl function... (?)
;;; (defmethod offset ((self cr-model)) 0.0)

(defmethod get-model-contents ((self cr-model))
  (om::data-stream-get-frames self))


;;;=======================
;;; GET MODEL DATA IN
;;;=======================

(defmethod! get-analysis-data ((sdfiff t) sdiftype beg end)
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
(defmethod make-frame-contents-from-data (datalist &key type (sort t) (weighed-avg t) (durmin 0.0))
  
  (let* ((partial_nums (mapcar #'car (om::flat (mapcar #'cadr datalist) 1)))
         (min-ind-partial (om::list-min partial_nums))
         (max-ind-partial (om::list-max partial_nums))
         (amps nil)
         (freqs nil))
    
    (when min-ind-partial
      (loop for i from min-ind-partial to max-ind-partial
            for partial-frames = (get-partial i datalist)
            do 
            (when partial-frames 
              
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
    
    (make-cr-frame-contents type freqs amps)))
   
  
;;;===================================
;;; GET/ADJUST TIME STRUCTURE
;;;===================================

(defun markers-to-segments (markers)
  (if (listp (car markers))
      markers ;; already formatted
    (loop for (a b) on markers by #'cdr
          when b collect (list a b))))

;;; never used
(defmethod adjust-model-segments (segments data)
  (let ((data-time-list (mapcar #'car data)))
    (if data-time-list
        (remove nil
                (loop for seg in segments
                      collect (let* 
                                  ((diff_l1 (mapcar #'(lambda (x) (abs (- x (car seg)))) data-time-list))
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
  
  (when initargs
  ;;; format list to list of segments (if needed)
  ;;; ... or sets a default time-struct
  (setf (time-struct self) (or (markers-to-segments (time-struct self)) 
                               (list (list 0.0 1.0)))) 

  ;;; generate the actual sequence from data + time-struct
   (om::data-stream-set-frames 
       self
       
       (cond 
        
        ;;; (data self) is already a list of cr-frame-partials (or other type)
        ((and (consp (data self))
              (or (om::list-subtypep (remove nil (data self)) (data-type self))
                  (om::list-subtypep (remove nil (data self)) 'cr::vps))) ;;; the default type
         
         (setf (data-type self) (type-of (car (data self))))
         
         (when (> (length (data self)) (length (time-struct self)))
           (om::om-print "Warning some data was ignored because they were over the time structure" "cr-model"))
         
         (loop for seg in (time-struct self)
               for cr-partials in (data self)
               collect (make-instance 'cr-frame 
                                      :date (om::sec->ms (car seg))
                                      :dur (- (cadr seg) (car seg))
                                      :vps cr-partials))
         )
   
        ;;; (data self) is (in principle) a raw list of anamysis data
        ((data self)
         
         ;;; adjust time-struct to the actual data
         ;;; (not sure we actually want to do this...)
         ; (setf (time-struct self) (adjust-model-segments (time-struct self) (data self)))
         
         (loop for seg in (time-struct self)
               do (om::om-print (format nil "Segment : ~D -> ~D" (car seg) (cadr seg)))
               collect (let* ((frame-data (remove-if #'(lambda (element) (or (< (car element) (car seg))
                                                                            (> (car element) (cadr seg))))
                                                    (data self))))
                         (make-instance 'cr-frame 
                                        :date (om::sec->ms (car seg))
                                        :dur (- (cadr seg) (car seg))
                                        :vps (make-frame-contents-from-data frame-data :type (data-type self) :durmin 0.1))
                         ))
         )
         
        (t ;;; no data :(
           (loop for seg in (time-struct self) collect 
                 (make-instance 'cr-frame :date (car seg) :dur (- (cadr seg) (car seg))
                                :vps nil))
           )
        ))
   )

   (setf (max-amp self)
         (loop for frame in (get-model-contents self) maximize
               (or (and frame (om::list-max (cr-partials-amps (vps frame)))) 0.0)))

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
  (model-data 
   (mapcar #'vps (get-model-contents self))
   modif-func args))



;;;==================
;;; MODEL INSPECT
;;;==================

;;; MAX AMP
(defmethod! model-max-amp ((self list))
  :icon 659
  (om::list-max (mapcar #'om::list-max (remove nil (mapcar #'cr-partials-amps (remove nil self))))))

(defmethod! model-max-amp ((self cr-model))
  (model-max-amp (mapcar #'vps (get-model-contents self))))

;;; MAX FREQ
(defmethod! model-max-freq ((self list))
  :icon 659
  (om::list-max (mapcar #'om::list-max (remove nil (mapcar #'cr-partials-freqs (remove nil self))))))

(defmethod! model-max-freq ((self cr-model))
  (model-max-freq (mapcar #'vps (get-model-contents self))))

;;; MIN FREQ
(defmethod! model-min-freq ((self list))
  :icon 659
  (om::list-min (mapcar #'list-min (remove nil (mapcar #'cr-partials-freqs (remove nil self))))))

(defmethod! model-min-freq ((self cr-model))
  :icon 659
  (model-min-freq (mapcar #'vps (get-model-contents self))))

;;; NB EVTS
(defmethod! model-nb-evts ((self cr-model))
  :icon 659
  (length (get-model-contents self)))

;;; NB EVTS
(defmethod! model-total-dur ((self cr-model))
  :icon 659
  (cadr (car (last (time-struct self)))))


;---------------------------------------------------------
; other inspection methods used in chroma CTL2

(defmethod get-norm-amp ((self cr-model) rang &optional (scaling-factor 1))
  (let ((thefql (nth rang (get-model-contents self))))
    (if thefql
      (mapcar #'(lambda (y) (/ y (/ (model-max-amp self) scaling-factor ))) (get-vps-amps thefql))
      nil)))

(defmethod begin-time ((self cr-model))
  (car (car (time-struct self))))

(defmethod get-nth-time ((self cr-model) i)
  (car (nth i (time-struct self))))

(defmethod get-nth-data-frame ((self cr-model) i)
  (vps (nth i (get-model-contents self))))


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
                                    for frame in (get-model-contents self)
                                    when (and frame (vps frame)) 
                                    collect (let ((vps (vps frame)))
                                              (list time (loop for f in (cr-partials-freqs  vps)
                                                               for i from 0
                                                               collect (list i f (or (nth i (cr-partials-amps vps)) 1.0) 0))))
                                    )))
                    
                (loop for frame in (om::make-1MRK-frames datalist) do
                      (om::sdif-write frame sdiffileptr))
                ))
          
          (sdif::SDIFFClose sdiffileptr))
      (om::om-beep-msg "Could not open file for writing: ~A" path))
    path))


;;;====================================
;;; CONNECTION TO CHROMA's FUNCTION
;;;====================================

(defmethod cr::gen-model-data ((self cr-model) fun-list arg-list &key 
                            (interpolmode) (markers) (test) (markermode 'delete) (timemode 'rel) (integeritp) (verbose))
  (cr::gen-model-data (get-model-contents self) fun-list arg-list
                      :interpolmode interpolmode :markers markers
                      :test test :markermode markermode
                      :timemode timemode :integeritp integeritp
                      :verbose verbose))
