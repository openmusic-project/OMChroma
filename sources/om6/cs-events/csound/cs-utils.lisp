(in-package :om)

(defvar *cs-orc-folder* nil)


(defun get-orc (name &optional (folder nil))
  "
Returns the a csound orchestra called <name>.orc if found in the <folder> and subfolders.
<folder> :local will look in the same folder where this function is called
<folder> NIL will look in *cs-orc-folder* 
"
  (let ((result nil)
        (real-folder 
         (cond ((equal folder :local) (make-pathname :directory (pathname-directory *load-pathname*)))
               ((null folder) *cs-orc-folder*)
               (t folder))))
    (when (probe-file real-folder)
      (loop for file in (om-directory real-folder :files t :directories nil)
            while (not result) do
            (print file)
            (when (and  name
                        (pathname-name file)
                        (pathname-type file)
                        (string-equal name (pathname-name file))
                        (string-equal (pathname-type file) "orc"))
              (setf result file)))
      (unless result 
        (loop for dir in (om-directory real-folder :files nil :directories t)
              while (not result) do
              (setf result (get-orc name dir)))
        ))
    result))


