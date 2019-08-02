(in-package :om)
;ms_1109
(defmethod! gen-model-data (lvals fun-list arg-list &key (interpolmode) (markers) (test) (markermode 'delete) (timemode 'rel) (integeritp) (verbose))
            :indoc '("Any list to be modified" "List of functions sequentially applied to lvals"
                                               "List of dynamic control structures (see below)"
                                               "Interpolation mode" "List of markers" "Test" "Markermode"
                                               "Timemode" "Integer Interpolation Flag" "Verbose Flag")
            :doc
"Process a list of values by sequentially applying all the functions in fun-list to each value,
with the arguments dynamically computed according to the rules specified in arg-list.

The first argument of each function is each element of lvals. The other arguments depend on the function.
NB: the function must return an element of the same type as the input argument.

arg-list: (larg1... largN)
largN: control list for the nth function, specifying the way the parameters are dynamically computed from the beginning to the end of the list.
Main structure:
'(
  (pos1 (val11... val1n))
  (posN (valN1... valNn)))

where:
posN: position of the corresponding arguments in the list of values
 if :timemode=relative
   if integer = absolute positions in the list (0 = beginning), that is, access through (nth posN list)
   if float = relative positions, scaled between the beginning and the end of the list (first number = beginning, last number = end)

 if :timemode=absolute
  posN refers to an absolute time with respect to the list of markers (which must be given)

val11...val1N: values of the arguments at the given position; the intermediate values will be computed via interpolation

:interpolmode = interpolation mode
  () = the values will be held until the next position, that is, no interpolation
  0.0 = linear interpolation
  <0.0 = logarithmic interpolation (going up), and exponential interpolation (going down), the argument is the alpha
  >0.0 = exponential interpolation (going up), and logarithmic interpolation (going down), the argument is the alpha
  '(exp2 [float]) = symetric interpolation (log/exp up and down,  >0.0 = exp, <0.0 = log)
  sin/sin1 = complete sinusoidal interpolation (from -pi/2 to pi/2)
  sin2 = 1/2 sin interpolation (from 0 to pi/2, slightly log up, exp down)
  sin3 = same as sin2, but totally symetric.

At the end of the process, a further test is applied to the computed list.
If the test is positive, the element will not be returned.
:test 'fun = apply fun to each element of the list.
:test '(fun arg1... argN) = apply fun with the following arguments: list-element, arg1... argN

:markermode
If the test results in eliminating some elements, AND there are corresponding markers, they will also be processed:
   delete = the marker correponding to the deleted element will also be deleted
   firstn = keep only the first n markers in the order of definition (hence discarding the remaining ones)
   () = do nothing, return all the markers

:integeritp
Flag, if t, the arguments computed by interpolation will be rounded to an integer.

:verbose
Flag, if t, print only the args passed to the modifying functions, NOT the result.
"
            :icon 654
            (let ((fun-list (list! fun-list)))
              (let ((num-vals (length lvals))
; if timemode is a list, keep only as many args as functions (if too little, repeat the last arg)
                    (timemode (prepare-arg (list! timemode) (length fun-list)))
                    (interpolmode (prepare-interpolmode (list! interpolmode) (length fun-list)))
                    (integeritp (prepare-arg (list! integeritp) (length fun-list)))
                    (res-arg-list))
                (loop for args in arg-list do
                      (let ((time-mode (nextl timemode))
                            (interpol-mode (nextl interpolmode))
                            (integer-itp (nextl integeritp)))
                        (cond ((and (equal time-mode 'rel) (null interpol-mode))
                             ; fixed list, no interpolation of arguments
                               (setf res-arg-list (cons (fixed-list args num-vals) res-arg-list)))
                              ((equal time-mode 'rel)
                               (setf res-arg-list
                                     (cons (interpolated-list
                                            args num-vals
                                            :itpmode interpol-mode
                                            :intitp integer-itp)
                                           res-arg-list)))
                              ((and (equal time-mode 'abs))
                               (setf res-arg-list
                                     (cons (interpolated-list-markers
                                            args markers
                                            :itpmode interpol-mode
                                            :intitp integer-itp)
                                           res-arg-list)))

                              (t (error "Unknown case, sir, timemode=~a, interpolmode=~a~%" timemode interpolmode)))))
                (if verbose
                    (nreverse res-arg-list)
;                 (print (list lvals fun-list (nreverse res-arg-list) :markers markers :test test :markermode markermode))))))
 
                 (final-model-data lvals fun-list (nreverse res-arg-list) :markers markers :test test :markermode markermode)))))

(defmethod! gen-model-data ((self cr-model) fun-list arg-list &key (interpolmode) (markers) (test) (markermode 'delete) (timemode 'rel) (integeritp) (verbose))
  :icon 654
  (gen-model-data (elements (data self)) fun-list arg-list
                  :interpolmode interpolmode :markers markers
                  :test test :markermode markermode
                  :timemode timemode :integeritp integeritp
                  :verbose verbose))


(defun prepare-arg (arg n)
  (cond
   ((null arg) (repeat-n arg n))
   ((listp arg) (cr::l-val n arg))
   (t (repeat-n arg n))))

;(prepare-arg '(1 2 3) 10)
;(prepare-arg '() 10)

(defun prepare-interpolmode (arg n)
  (cond
   ((null arg) (repeat-n arg n))
   ((and (listp arg) (symbolp (car arg)) (not (null (car arg)))) (repeat-n arg n))
   (t (cr::l-val n arg))))

;(prepare-interpolmode '(exp 1.0) 10)


(defun fixed-list (l length)
  (let* ((result)
         (curr-l (clone l))
         (el1 (nextl curr-l))
         (el2 (nextl curr-l)))
    (loop for i = 0 then (+ i 1)
          while (< i length) do
          (unless (< i (car el2)) ; update el to next ctl in list
            (setf el1 el2)
            (setf el2 (let ((tmpel (nextl curr-l))) (if (null tmpel) el2 tmpel)))) ; last el of list
          (setf result (cons (cadr el1) result)))
    (nreverse result)))

(defun interpolated-list (ctl-list length &key (intitp) (itpmode 0.0))
  (let ((result ; rearrange data so that they can build a FUN object (thorny!!!)
         (mapcar #'flat
                 (mat-trans
                  (loop for el in ctl-list ; el = (0 (10 15))
                        collect (om::mat-trans (list (cadr el) (repeat-n (car el) (length (cadr el))))))
                  ))))
        (mat-trans ; interpolate each argument alone, then mat-trans them
         (if intitp
              (loop for fun in result
                    collect 
                    (mapcar
                     #'round
                     (cr::y-list_fun (cr::sample_fun (cr::make_fun fun) length itpmode))))
           (loop for fun in result
                 collect (cr::y-list_fun (cr::sample_fun (cr::make_fun fun) length itpmode)))))

    ))

(defun interpolated-list-markers (ctl-list markers &key (intitp) (itpmode 0.0))
  (let ((result ; rearrange data so that they can build a FUN object (thorny!!!)
         (mapcar #'flat
                 (mat-trans
                  (loop for el in ctl-list ; el = (0 (10 15))
                        collect (om::mat-trans (list (cadr el) (repeat-n (car el) (length (cadr el))))))
                  ))))
        (mat-trans ; interpolate each argument alone, then mat-trans them
         (if intitp
              (loop for fun in result
                    collect 
                    (mapcar
                     #'round
                     (loop for marker in markers
                           collect (cr::y-val_fun (cr::make_fun fun) marker itpmode))))
           (loop for fun in result
                 collect 
                 (loop for marker in markers
                       collect (cr::y-val_fun (cr::make_fun fun) marker itpmode))))
         )))

;(interpolated-list-markers ctlmidimrk mrks)
;(interpolated-list-markers ctlmidimrk mrks :itpmode 1.0 :intitp t)
;(interpolated-list ctlmidi 9 :intitp t :itpmode 0.0)


(defmethod final-model-data (lvals fun-list arg-list &key (markers) (test) (markermode 'delete))
  ; complete data already prepared, just loop through them and apply the functions + the test

  (let ((rep lvals))
    (loop for fun in fun-list
          for args in arg-list do
 ;         (print (format () "Processing function with args ~a~%" args))
          (setf rep (loop for elem in rep
                                 for i = 0 then (+ i 1)
                                 collect
                                 (apply fun (append (list elem) (nth i args))))))
; deal with the test
    (cond ((and test markers)
           (if (equal markermode 'delete) ; delete corresponding marker in position
               (let ((res-val) (res-mrk))
                 (loop for val in rep
                       for mrk in markers do
                       (unless (if (and (listp test) (not (null test))) (apply (car test) (append (list val) (cdr test))) (apply test (list val)))
                         (setf res-val (cons val res-val))
                         (setf res-mrk (cons mrk res-mrk))))
                 (list (nreverse res-val) (nreverse res-mrk)))
             (let ((res-val))  ; otherwise compute result and...
               (loop for val in rep do
                     (unless (if (and (listp test) (not (null test))) (apply (car test) (append (list val) (cdr test))) (apply test (list val)))
                       (setf res-val (cons val res-val))))
               (if (equal markermode 'firstn)  ; ...either return first N markers,...
                   (list (reverse res-val) (firstn markers (length res-val)))
                 (list (nreverse res-val) markers))))) ; ...or return all the markers
          (test ; there are no markers
           (loop for val in rep
                 unless (if (and (listp test) (not (null test))) (apply (car test) (append (list val) (cdr test))) (apply test (list val)))
                   collect val))
          (t (if markers
                 (list rep markers)
               rep))) ; there is no test
    ))

;(defun mynth (l pos) (nth pos l))
;(defun my* (l) (om* l 10))
;(defun mytest (l) (null l))
;(defun mytest1 (l n) (print l) (print n) (print (equal l n)))
;(final-model-data '((1 2) (2 3) (3 4) (4 5) (5 6) (6 7) (7 8) (8 9) (10 11 12) (9 10)) '(my* mynth) '(() ((0) (1) (2) (0) (1) (2) (0) (1) (2) (0) (1) (2))))
;                  :test '(mytest) :markers ())
;:test '(mytest) :markers '(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 0.91 0.92) :markermode 'delete)


