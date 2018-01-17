(in-package chroma)

(defclass model-f0 
  (chroma-model)
  ((f0 :accessor f0
       :documentation "f0 : type fun"))
  (:documentation "Fundamental Frequency Model"))



(defmethod initialize-instance :after ((x model-f0) &rest initargs &key f0)
  (declare (ignore initargs))
    (setf (f0 x) (make_fun(xy-echange(data f0))))
   )



#|
(defmethod load-from-file ((x model-f0) &optional file)
  (if(null file)(setf file (choose-file-dialog)))
  (let (curr-list (truc nil) (funl nil))
    (with-open-file (in-stream file :direction :input)
      (loop while (not truc) 
            do (multiple-value-bind(s tr)(read-line in-stream nil)
                 (setf truc tr)
                 (setf curr-list
                       (read-from-string (concatenate 'string "(" s ")")))
                 (case (length curr-list)
                   (1(progn(push (car curr-list) funl))))))
    (setf (f0 x) (make_fun(xy-echange  funl)))
    (setf (total-duration x) (car(last funl)))
    )'loaded))
|#

(defun xy-echange (l)
  (let ((result nil))
    (loop for x in l 
          do (push (second x) result)
          do (push (first x)result)
          )
    (reverse result)
    ))