;to deal with data from cseq analysis (from audiosculpt)

(in-package chroma)


(defclass cseq-data
  (analysis-data)
  ()
  (:documentation "audiosculpt cseq data in Ram"))


(defmethod initialize-instance :after ((x cseq-data)  &rest initargs &key file)
  (declare (ignore initargs))
  (if (probe-file file)
    (if (om::sdif-check-file (om::om-path2cmdpath file))
      (load-cseq-sdif-file x file)
      (load-cseq-file x file))
    (om::om-beep-msg "this file does not exist"))
  )


(defmethod load-cseq-file ((x cseq-data)&optional file)
  (if(null file)(setf file (choose-file-dialog)))
  (format t "LOADING DATA FROM ~a~%" (file x))
  (with-open-file (in-stream file :direction :input)
         (setf (data x) (read in-stream)))'loaded
)

(defmethod load-cseq-sdif-file ((x cseq-data) &optional file)
  (if(null file)(setf file (choose-file-dialog)))
  (format t "LOADING DATA FROM ~a~%" (file x))
  (setf dataNewFormat (om::get-chordseq-data (om::load-sdif-file file)))
  (setf (data x) (convertToAsFormat dataNewFormat))
  )

;traverse l'analyse pour trouver l'amplitude max (normalisation ....)
(defmethod get-max-amp ((x cseq-data))
  (db->lin (apply #'max (mapcar #'fifth (cddr (data x))))))


(defun convertToAsFormat (data)
  (setf res (convertToAsFormat1 data))
  (setf res(sort res #'< :key #'fourth))
  (setf res(stable-sort res #'< :key #'third))
  (append (list "partials" (length res)) res)
  )

(defun convertToAsFormat1 (data)
  (loop for i in data
        do (setf fq  (first i) amp  (om::lin->db (fourth i)))
        collect (list "points" 2 (second i) fq amp (third i) fq amp)
        ))


;(converttoasformat '((1 2 3 0.5)(5 6 7 0.25) (5 2 3 0.25)))

