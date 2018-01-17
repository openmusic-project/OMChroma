(in-package :om)

(defun get-orc (name &optional (folder :local))
"
Return the a csound orchestra contained in the folder and subfolders
specified by the global variable cr::CSorc.
If a folder is passed as an argument, look for it in this folder and all the subfolders.
Automatically add the type orc to the file.
NB: csd files will not work.
"
(let ((result nil))
  (when (equal folder :local) 
    (setf folder (make-pathname :directory (pathname-directory *load-pathname*))))
  (when (null folder)
    (setf folder (get-cr-path :orc)))
  (loop for els in (om-directory folder :files t :directories t)
        while (not result) do
        (if (directoryp els)
            (setf result (get-orc name els))
          (when (and  name
                      (pathname-name els)
                      (pathname-type els)
                      (string-equal name (pathname-name els))
                      (string-equal (pathname-type els) "orc"))
            (setf result els))))
  result))


