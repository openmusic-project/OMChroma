(in-package :cr)


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



;;; Definition of helper-functions with implementation-dependent or external dependencies:

(defun choose-file-dialog (&optional (message "Select a file...")) 
  #+lispworks
  (capi::prompt-for-file message)
  #-lispworks
  (error "Sorry I can not prompt for file...")
  )


(defun cr-beep (&optional text)
  #+lispworks
  (capi::beep-pane nil)
  #-lispworks
  (print "BIP")
  
  (when text (print text))
)


(defun sound-file-get-info (filename)
  #+libsndfile
  (audio-io::om-get-sound-info (namestring filename))
  #-libsndfile
  (error "Sorry I can not read sound info..."))

