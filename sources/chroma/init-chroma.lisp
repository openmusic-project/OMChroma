

(in-package :chroma)

(defvar *cr-out-dir* nil)
(defvar *cr-csfun-dir* nil)
(defvar *cr-wt-dir* nil)
(defvar *cr-models-dir* nil)
(defvar *cr-userfun-dir* nil)
(defvar *cr-tmp-dir* nil)


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
