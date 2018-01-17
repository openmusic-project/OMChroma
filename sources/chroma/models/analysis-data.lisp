;to deal with any kind of analysis

(in-package chroma)


(defclass analysis-data
  ()
  (   (data :type list
            :initform nil
            :accessor data
            :initarg :data
            :documentation "analysis data")
      (file :type pathname
            :initform nil
            :accessor file
            :initarg :file
            :documentation "analysis file pointer")
   )
  (:documentation "Analysis data in Ram"))

(defclass analysis-stream
  ()
  (   (file-accessor)
   )
  (:documentation "Streamed Sdif Analysis data (LATER)"))

