

(in-package :chroma)

(defvar *cr-out-dir* nil)
(defvar *cr-csfun-dir* nil)
(defvar *cr-wt-dir* nil)
(defvar *cr-models-dir* nil)
(defvar *cr-userfun-dir* nil)
(defvar *cr-tmp-dir* nil)

; (defparameter *cr-root* (make-pathname :directory (butlast (pathname-directory *load-pathname*) 2)))

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




;;; Definition of helper-functions with implementation-dependent or external dependencies:

(defun choose-file-dialog (&optional (message "Select a file...")) 
  #+lispworks
  (capi::prompt-for-file message)
  #-lispworks
  (error "Sorry I can not prompt for file...")
  )

(defun sound-file-get-info (filename)
  #+libsndfile
  (sf::sndfile-get-info filename)
  #-libsndfile
  (error "Sorry I can not read sound info...")
  )