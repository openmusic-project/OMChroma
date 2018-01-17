;to deal with data from spectral envelope analysis
; see : http://www.ircam.fr/equipes/analyse-synthese/DOCUMENTATIONS/additive/estimate.html
;data format ((n SR/2 X)(T1 a1 a2 ..)(T2 a1 a2 ..)...)

(in-package chroma)


(defclass specenv-data
  (analysis-data)
  ()
  (:documentation "enveloppe spectrale calcule par estimate"))


(defmethod initialize-instance :after ((x specenv-data)  &rest initargs &key file)
  (declare (ignore initargs))
  (load-file x file)
  )


(defmethod load-file ((x specenv-data) &optional file)
"Load spectral envelope data from an estimate file"
  (if(null file)(setf file (choose-file-dialog)))
  (format t "LOADING DATA FROM ~a ~%" (file x))
  (with-open-file (in-stream file :direction :input)
    (let ((result nil)(ti t)(li t))
      (loop while (not (null li))
            do (setf li (read-line in-stream nil)
                     ti (read-from-string (format nil "(~a)" li)))
            do (if ti (push ti result))
            (when(and(listp ti)(get-gbl 'CTL2-PRINT)) (format t "~a~%" (first ti)) ))
      (setf (data x)(nreverse  (cdr result))
      ))))


