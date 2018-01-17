(in-package chroma)

(defclass model-specenv
  (chroma-model)
  ((timelist :accessor timelist
       :documentation "temps des enveloppes")
   (envlist :accessor envlist
       :documentation "liste de fun"))
  (:documentation "Spectral Surface Model"))



(defmethod initialize-instance :after ((x model-specenv) &rest initargs &key analysis)
  (declare (ignore initargs))
  (if analysis
    (progn (setf (timelist x) (mapcar #'first (cdr (data analysis))))
   (setf (envlist x) (mapcar #'envtofun (cdr (data analysis))))
   )))

(defun envtofun (l)
  (let((result nil)(l (cdr l)))
    (loop for i from 0 to (length l) 
          for e in l
          do (push (lin->db e) result)(push i result))
    (make_fun (nreverse result))))


;(envtofun '(1 2 3 4))

