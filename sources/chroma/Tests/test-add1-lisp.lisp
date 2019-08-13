;*************************************
; TEST OF A LIST DEFINITION OF ADD-1 |
;*************************************

(in-package :cl-user)
(let ((lib (om:find-library "OMChroma")))
  (unless (om::loaded? lib)
    (om::load-om-lib lib)))

(let ((lib (om:find-library "marcolib")))
  (unless (om::loaded? lib)
    (om::load-om-lib lib)))

(in-package :om)
(defun get-syn-data (data x)
  (cadr (assoc data x)))

(defun get-global-data ()
      (list
       '(nev 10)
       '(action-time 0.0)
       '(user-fun)))


(defun get-slot-data ()
  (let ((add-1-global-data (get-global-data)))
    (list
     `(e-dels ,(mapcar #'(lambda (x) (let ((val (+ x (* x (cr::ran% 0.5))))) (if (< val 0.0) (* val -1.0) val)))
                       (cr::bkwd-lp (get-syn-data 'nev add-1-global-data) '(0 0.5 1.0 1.5))))
     `(durs ,(cr::lkp (get-syn-data 'nev add-1-global-data) (cr::make_fun '(1.0 0  5.0 2  2.0 3))))
     `(amp ,(cr::rept (get-syn-data 'nev add-1-global-data) 1.0))
     `(freq ,(cr::spsht 200.0 (get-syn-data 'nev add-1-global-data) 0.1 2.1 0.1))
     `(aenv ,(cr::fun->bpf (cr::make_fun '(0 0 1.0 10 0.5 30  0 50))))
     )))
; in this way, the list is evaluated each time it is called

(defparameter slot-data1
  (let ((add-1-globaldata (get-global-data)))
    (list
     `(e-dels ,#'(lambda (i) 
                   (mapcar #'(lambda (x) (let ((val (+ x (* x (cr::ran% 0.5))))) (if (< val 0.0) (* val -1.0) val)))
                           (cr::bkwd-lp (get-syn-data 'nev add-1-globaldata) '(0 0.5 1.0 1.5)))))
     `(durs ,(cr::lkp (get-syn-data 'nev add-1-globaldata) (cr::make_fun '(1.0 0  5.0 2  2.0 3))))
     `(amp ,(cr::rept (get-syn-data 'nev add-1-globaldata) 1.0))
     `(freq ,(cr::spsht 200.0 (get-syn-data 'nev add-1-globaldata) 0.1 2.1 0.1))
     `(aenv ,(cr::fun->bpf (cr::make_fun '(0 0 1.0 10 0.5 30  0 50))))
   )))
; in this way, the list is evaluated only when defined, and no longer



(let ((add-1-slots-data (get-slot-data))
      (add-1-global-data (get-global-data)))
; here: if add-1-slots-data is passed, random values will not change within the classes, but
;          they will change each time the file is evaluated
;       if (get-slot-data) is called, each time it is called, random values will change

  (setf tmp-l-classes (list 
                   (om-make-array 'add-1
; om-make-array synt-class numcols action-time user-fun [WITHOUT :!!]
;    :slot-name <vals>
                                  (get-syn-data 'nev (get-global-data)) ; numcols
                                  0.0 ; action-time
                                  nil ; user-fun
                                  :e-dels (get-syn-data 'e-dels (get-slot-data))
                                  :durs (get-syn-data 'durs (get-slot-data))
                                  :amp (get-syn-data 'amp (get-slot-data))
                                  :freq (get-syn-data 'freq (get-slot-data))
                                  :aenv (get-syn-data 'aenv (get-slot-data))
                                  )

                   (let ((my-class
                          (om-make-array 'add-1
;                                         (setf (numcols my-class) 3.0)
                                         (get-syn-data 'nev (get-global-data))
                                         3.0
                                         nil
                                         :e-dels (get-syn-data 'e-dels (get-slot-data))
                                         :durs (get-syn-data 'durs (get-slot-data))
                                         :amp (get-syn-data 'amp (get-slot-data))
                                         :freq (get-syn-data 'freq (get-slot-data))
                                         :aenv (get-syn-data 'aenv (get-slot-data))
                                         )))
                     (setf (action-time my-class) 3.0)
                     my-class)
                   )))


(let ((add-1-slots-data (get-slot-data))
      (add-1-global-data (get-global-data)))
; here: if add-1-slots-data is passed, random values will not change within the classes, but
;          they will change each time the file is evaluated
;       if (get-slot-data) is called, each time it is called, random values will change

  (setf tmp-l-classes1 (list 
                   (om-make-array 'add-1
                                 (get-syn-data 'nev (get-global-data))
                                  0.0
                                  #'(lambda (a i) (list (get-comp a i) (print (format () "This is the value of freq: ~a~%"  (comp-field (get-comp a i) "freq"))) ))
                                  :e-dels (get-syn-data 'e-dels (get-slot-data))
                                  :durs (get-syn-data 'durs (get-slot-data))
                                  :amp (get-syn-data 'amp (get-slot-data))
                                  :freq (get-syn-data 'freq (get-slot-data))
                                  :aenv (get-syn-data 'aenv (get-slot-data))
                                  )

                   (om-make-array 'add-1
                                  (get-syn-data 'nev (get-global-data))
                                  6.0
                                  (om::gen-user-fun '(ed-0?) ())
;                                         :e-dels (get-syn-data 'e-dels (get-slot-data))
                                  :e-dels '(0 1 2 -3 4 5 6 -7 8 9 10)
                                  :durs (get-syn-data 'durs (get-slot-data))
                                  :amp (get-syn-data 'amp (get-slot-data))
                                  :freq (get-syn-data 'freq (get-slot-data))
                                  :aenv (get-syn-data 'aenv (get-slot-data))
                                  )
                   )))


;(om-inspect tmp-l-classes)
;(om-inspect tmp-l-classes1)




; uses the resolution and SR specified in OM-Preferences
;(synthesize tmp-l-classes :name "test-add1" :rescale 0.5)
;(synthesize tmp-l-classes1 :name "test-add2" :rescale 0.5)
