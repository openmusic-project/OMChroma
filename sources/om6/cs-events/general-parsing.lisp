(in-package :om)

;GENERAL_USER_FUN

#|
;test
a list of functions of 1 argument (component)
outputs
- a component (modified or the same) - the test is successful
- a string                           - the test fail, the string can explain the reason
- a list                             - If the car of the list is a component the test is successful else
                                     - the test fail. All elements of the list will be written in the scr file
- anything                           - the test fail

|#

#|
;sub-comp
a list of functions of 1 argument (component)
outputs
- a list of component or strings (the list can be empty NIL)
All elements of the list will be written in the scr file
|#

#|
;sub-test
a list of functions of 1 argument (component)
outputs
- a component (modified or the same) - the test is successful
- a string                           - the test fail, the string can explain the reason
- a list                             - If the car of the list is a component the test is successful else
                                     - the test fail. All elements of the list will be written in the scr file
- anything                           - the test fail

;remove-fields
a list of strings for fields that will not be written in the scr file

|#

(defmethod! gen-user-fun ((tests list) (sub-comp list) &key (sub-tests nil) (remove-fields nil))
   :initvals '(nil nil nil nil)
   :icon 602
   :documentation
"
;test
a list of functions of 1 argument (component)
outputs
- a component (modified or the same) - The test is successful
- a string                           - The test fails, the string can explain the reason
- a list                             - If the car of the list is a component the test is successful else
                                     -   the test fails. All the elements of the list will be written in the scr file
- anything                           - The test fails

;sub-comp
a list of functions of 1 argument (component)
outputs
- a list of component or strings (the list can be empty NIL)
All elements of the list will be written in the scr file

;sub-test
a list of functions of 1 argument (component)
outputs
- a component (modified or the same) - the test is successful
- a string                           - the test fail, the string can explain the reason
- a list                             - If the car of the list is a component the test is successful else
                                     - the test fails. All elements of the list will be written in the scr file
- anything                           - the test fails

;remove-fields
a list of strings for fields that will not be written in the scr file
"

   #'(lambda (matrix index)
; ***ADDED TO ALLOW TO ACCESS THIS VARIABLE FROM USER-FUNS, MARCO, 010914
       (declare (special index))
       (let* ((component (get-comp matrix index))
              (filtercomp (filtre-test component tests ))
              (current-comp (car filtercomp))
              (comp-list (second filtercomp)))
         (when current-comp
           (loop for item in sub-comp do
                 (let ((subcomp (funcall item current-comp))
                       subcomplist)
                   (setf subcomplist
                         (loop for subc in subcomp
                               append (if (component-p subc)
                                        (let* ((filtersubcomp (filtre-test subc sub-tests ))
                                               (succ-sub (first filtersubcomp))
                                               (subcomp-list (second filtersubcomp)))
                                          (if succ-sub
                                            (cons succ-sub subcomp-list)
                                            subcomp-list)
                                          )
                                        (list subc))))
                   (setf subcomplist (cons (format nil "Sub-component list using the function ~D ~%" item) subcomplist))
                   (setf comp-list (append comp-list subcomplist)))))
         (make-remove-fields (if current-comp (cons current-comp comp-list) comp-list) remove-fields))
       ))

(defun filtre-test (comp predicates)
  (let ((comp-list nil)
        (current-comp comp))
    (loop for item in predicates
          while (component-p current-comp) do
          (let ((temp-rep (cond ((or (symbolp item) (functionp item)) (funcall item current-comp))
                                ((consp item)
                                 (if (equal 'lambda (car item))
                                     (funcall (eval item) current-comp)
                                   (apply (car item) (cons current-comp (cdr item))))))))
            (cond
             ((null temp-rep)
              (setf current-comp nil))
             ((component-p temp-rep)
              (setf current-comp temp-rep))
             ((stringp temp-rep)
              (setf comp-list (cons temp-rep comp-list))
              (setf current-comp nil))
             ((listp  temp-rep)
              (if (component-p (car temp-rep))
                (progn
                  (setf current-comp (car temp-rep))
                  (setf comp-list (append comp-list (cdr temp-rep))))
                (progn
                  (setf current-comp nil)
                  (setf comp-list (append comp-list temp-rep)))
                ))
             (t (setf current-comp nil)))))
    (list current-comp comp-list)))

#|
(defun filtre-test (comp predicates )
   (let ((comp-list nil)
         (current-comp comp))
     (loop for item in predicates
           while (component-p current-comp) do
           (let ((temp-rep (funcall item  current-comp)))
             (cond
              ((null temp-rep)
               (setf current-comp nil))
              ((component-p temp-rep) 
               (setf current-comp temp-rep)
               (setf comp-list (list temp-rep)))
              ((stringp temp-rep)
; ***POUR FAIRE AFFICHER LES CHAINES DE CARACTERES DANS LE SCORE 
              (setf comp-list (cons temp-rep comp-list))
             ;  (setf comp-list (list temp-rep))
               (setf current-comp nil))
              ((listp  temp-rep)
               (setf current-comp (car temp-rep))
; ***POUR FAIRE AFFICHER LES CHAINES DE CARACTERES DANS LE SCORE 
               (setf comp-list (cdr temp-rep)))
              ; (setf comp-list (append comp-list (cdr temp-rep))))
              (t (setf current-comp nil)))))
     (list current-comp comp-list)))
|#


#|
REMPLACEE LORSQUE ON A FAIT LA USER-FUN
(defun filtre-test (comp predicates )
   (let ((comp-list nil)
         (current-comp comp))
     (loop for item in predicates
           while (component-p current-comp) do
           (let ((temp-rep (funcall item  current-comp)))
             (cond
              ((null temp-rep)
               (setf current-comp nil))
              ((component-p temp-rep) 
               (setf current-comp temp-rep))
              ((stringp temp-rep)
               (setf comp-list (cons temp-rep comp-list))
               (setf current-comp nil))
              ((listp  temp-rep)
               (setf current-comp (car temp-rep))
               (setf comp-list (append comp-list (cdr temp-rep))))
              (t (setf current-comp nil)))))
     (list current-comp comp-list)))
|#

(defun make-remove-fields (list  remove-fields)
   (if remove-fields
     (let (indexlist)
       (loop for item in list
             collect (if (component-p item)
                       (progn
                         (unless indexlist
                           (setf indexlist (loop for label in remove-fields
                                                 collect (label2index item label))))
                         (remove-index (val-list item) indexlist item))
                       item)))
     list))

(defun remove-index (list position comp)
   (setf (val-list comp)
         (loop for item in list
               for i = 0 then (+ i 1)
               when (not (member i position)) collect item))
   comp)



#|
;tests
(defvar *durmin* 0.1)
(defvar *nyquist* 22050)

(defun get-abs-dur (comp)
  (let ((curr-dur (* (comp-field comp "durtot") (comp-field comp "durs"))))
    (if (< curr-dur *durmin*)
      (format nil "error too short dur : ~5F ~%" curr-dur)
      comp)))

(defun get-abs-dur (comp)
  "Blindly compute the absolute duration"
    (comp-field comp "durs" (* (comp-field comp "durtot") (comp-field comp "durs"))))

(defun chk-durtot (comp)
  (let ((curr-dur (comp-field comp "durs"))
        (curr-ed (comp-field comp "e-dels"))
        (durtot (comp-field comp "durtot")))
    (if (<= (+ curr-dur curr-ed) durtot)
      comp
      (let ((new-dur (- durtot curr-ed)))
        (progn
          (comp-field comp "durs" new-dur)
          (list
           comp
           (format nil "-----> WARNING: Corrected dur, old : ~5F ~%" curr-dur)))))))

(defun chk-durmin (comp)
  (let ((curr-dur (comp-field comp "durs")))
    (if (< curr-dur *durmin*)
      (format nil "****=> ERROR: too short dur : ~5F ~%" curr-dur)
      comp)))

(defun get-abs-amp (comp adition)
  (comp-field comp "amp" (/ (comp-field comp "amp") adition ))
  comp)
;  (list comp "Changing amp "))

(defun check-nyquist (comp)
  (let ((curr-freq (comp-field comp "freq")))
    (if (<= curr-freq *nyquist*)
      comp
      (let ((new-freq (lower-fq curr-freq *nyquist*)))
       (comp-field comp "freq" new-freq)
       (list comp (format nil "Modified freq : old = ~7F new = ~7F  ~%" curr-freq new-freq))))))

(defun lower-fq (fq fqmax)
  (loop while (> fq fqmax) do
        (setf fq (/ fq 2)))
  fq)
        

(defun check-nyquist2 (comp)
  (let ((curr-freq (comp-field comp "freq")))
    (if (<= curr-freq *nyquist*)
      comp
      (format nil "error great freq : ~5F ~%" curr-freq))))
|#
