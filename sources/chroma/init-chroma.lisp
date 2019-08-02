

(in-package :chroma)


(defvar *cr-root* (ensure-directories-exist (merge-pathnames "Documents/Chroma/" (user-homedir-pathname))) )

(defparameter *cr-out-dir* (ensure-directories-exist (merge-pathnames "cr-out/" *cr-root*)))
(defparameter *cr-csfun-dir* (ensure-directories-exist (merge-pathnames "cr-fun/" *cr-root*)))
(defparameter *cr-wt-dir* (ensure-directories-exist (merge-pathnames "cr-wt/" *cr-root*)))
(defparameter *cr-models-dir* (ensure-directories-exist (merge-pathnames "cr-models/" *cr-root*)))
(defparameter *cr-userfun-dir* (ensure-directories-exist (merge-pathnames "cr-userfun/" *cr-root*)))
(defparameter *cr-tmp-dir* (ensure-directories-exist (merge-pathnames "cr-tmp/" *cr-root*)))

(defun get-cr-path (dir &key name type subdirs)
  (let ((root (case dir 
                 (:out *cr-out-dir*)
                 (:csfun *cr-csfun-dir*)
                 (:wt *cr-wt-dir*)
                 (:models *cr-models-dir*)
                 (:userfun *cr-userfun-dir*)
                 (:tmp *cr-tmp-dir*)
                 (t nil))))
    (make-pathname :directory (append (pathname-directory root) subdirs)
                   :name name :type type)))
;(get-cr-path :wt)

;vps/pitch-conversions
(export '(fq->pch fq->midi fq->ratio fq->midic  fq->itvl fq->semitones
                  pch->fq pch->midi pch->midic pch->itvl pch->semitones pch->pch-class pch->ratio
                  midi->pch midi->semitones midi->pch-class midi->midic midi->fq midi->ratio midi->itvl
                  midic->midi midic->fq midic->pch midic->ratio midic->itvl midic->semitones midic->pch-class
                  ratio->fq ratio->itvl ratio->semitones ratio->midi ratio->midic ratio->pch
                  itvl->fq itvl->midi itvl->midic itvl->ratio itvl->pch itvl->semitones
                  semitones->ratio semitones->itvl semitones->fq semitones->midi semitones->midic 
                  pch-class->pch pch-class->midi pch-class->fq)
        :chroma)

;vps/vps
(export '(fql ptl ail spl cil crl arl rpl) :chroma)

