;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package chroma)

;MODIFICATIONS D'UN MODELE :

;1.MODIFICATIONS TEMPORELLES

(defmethod modify-markers ((x chroma-model) start durlist)
"
Change a model's rhythm:
       x = chroma-model
   start = action time
 durlist = duration's list
"
  (let ((size (length (markers x )))
        (result (copy-instance x))
        (new-marker-list (durtotime start durlist)))
    (cond ((eq (length new-marker-list) size)  
           (setf (markers result) new-marker-list ))
           ((> (length new-marker-list) size)
           (format t "~%WARNING : too many markers in ~a ~%TRUNCATED~%"x)
           (setf (markers result) (butlast new-marker-list size)))
           (t (error "MISSING MARKERS. GIVEN: ~a, NEEDED AT LEAST: ~a."
                     (length new-marker-list) size)))result))

(defun durtotime (ref l)
  (if (null l)(list ref)  
      (cons ref (durtotime  (+ ref  (car l)) (cdr l)))))

;(durtotime 1.0 '(1 1 1 1))

(defmethod tempo-map ((model chroma-model) fun &key &allow-other-keys
;&optional random
)
"
Modify a model's tempo using a function.
   model = chroma model
     fun = object of type fun (1 = no change, 2 = twice as slow
"
(let ((fun2 (copy-list fun)) new-durs)
  (X-resc_fun fun2 (first (markers model)) (car(last (markers model))))
 (setf new-durs (mapcar
        (lambda (time dur)(* dur (y-val_fun fun2 time)))
        (markers model)(durlist  model)))
 (modify-markers model (first(markers model)) new-durs)))



;2.MODIFICATIONS DES FQLS

(defmethod modify ((x chroma-model) 
                         &key (fun 'insert-frequency)(args nil))
"
Modify a model:
   apply fun with arguments args to x (chroma-model)

     x = chroma-model
   fun = function called to modify the model
  args = 

Can be very easily extended by the user.
"
#|
(if (print(is-regions-partials? x))
  (call-next-method )

|#
  (let((model (copy-model x))
       result)
;    (print model)
    (loop for fql in (fql-list x)
          do (if (the-list fql) 
               (push (apply fun (append (list fql) args)) result)
               (push () result)))       ;to allow empty fql in models
    (setf (fql-list model)(reverse result))
    model))


(defmethod modify ((x model-partials) 
                         &key (fun 'insert-frequency)(args nil))
  (let((model (copy-model x))
       result)
    (loop for ptl in (ptl-list x)
          do (if (the-list ptl) 
               (push (apply fun (append (list ptl) args)) result)
               (push () result)))       ;to allow empty fql in models
   (setf (ptl-list model)(reverse result))
    model))


(defmethod modify-with-list ((x chroma-model) 
                         &key (fun 'transpose-fql) (args nil) (xp-mode ()) (oct 1))
"
Modify a model with a list of arguments:
   modify each fql according to different arguments contained in the args list.
Useful to modify each fql with a different argument.

The amount of elements in the args list must be the same as the amount of fqls in the fql-list.

    x = chroma model
  fun = function called
 args
  

"
  (let ((model (copy-model x))
       result)
    (unless (equal (length args)(length (fql-list x)))
      (error "LIST ARGS AND FQLS MUST HAVE THE SAME LENGTH~%   args : ~a / fqls : ~a"
             (length args)(length (fql-list x))))
    (loop for fql in (fql-list x)
          do (if (the-list fql) 
               (push (apply fun
                            (append
                             (list fql)
                             (list (car args) :xp-mode xp-mode :oct oct)))
                     result)
               (push () result))       ;to allow empty fqls in models
          do (setf args (cdr args)))
    (setf (fql-list model)(reverse result))
    model))

(defmethod modify-with-list ((x model-partials) 
                         &key (fun 'transpose-fql) (args nil) (xp-mode ()) (oct 1))
  (let ((model (copy-model x))
       result)
    (unless (equal (length args)(length (fql-list x)))
      (error "LIST ARGS AND FQLS MUST HAVE THE SAME LENGTH~%   args : ~a / fqls : ~a"
             (length args)(length (ptl-list x))))
    (loop for ptl in (ptl-list x)
          do (if (the-list ptl) 
               (push (apply fun
                            (append
                             (list ptl)
                             (list (car args) :xp-mode xp-mode :oct oct)))
                     result)
               (push () result))       ;to allow empty fqls in models
          do (setf args (cdr args)))
    (setf (ptl-list model)(reverse result))
    model))

(defmethod insert-frequency ((fql fql) &rest args)
"
Default function called by modify.
Insert equally-spaced new components between the original model's components.

The first inserted component is the first and second frequency.
"
  (declare (ignore args))
  (let ((result nil)(result-amp nil)
        (list (get-fql fql))
        (list-amp (get-amp fql)))
    (loop while list
          do (let((val (pop list)))
               (if(not(null result))
                  (push (/(+ val (car result))2) result))
               (push val result))
          do (let((val (pop list-amp)))
               (if  result-amp
                 (push (/(+ val (car result-amp))2) result-amp))
               (push val result-amp)))
    (make-instance 'fql :the-list(nreverse result)
                   :amplitudes (nreverse result-amp))))

(defmethod insert-frequency ((ptl ptl) &rest args)
"
Default function called by modify.
Insert equally-spaced new components between the original model's components.

The first inserted component is the first and second frequency.
The AMP and TRANSP funs are also INTERPOLATED.
"
  (declare (ignore args))
  (let ((result nil)(result-amp nil)(result-ed ())(result-d ())(result-tf ())(result-af ())
        (list (get-fql ptl))
        (list-amp (get-amp ptl))
        (list-ed (entry-delays ptl))
        (list-d (durs ptl))
        (list-tf (transp_funs ptl))
        (list-af (amp_funs ptl))
        )
    (loop while list
          do (let((val (pop list)))
               (when result
                 (push (/(+ val (car result))2) result))
               (push val result))
          do (let((val (pop list-amp)))
               (when result-amp
                 (push (/(+ val (car result-amp))2) result-amp))
               (push val result-amp))
          do (let((val (pop list-ed)))
                (when result-ed
                 (push (/(+ val (car result-ed))2) result-ed))
               (push val result-ed))
          do (let((val (pop list-d)))
                (when result-d
                 (push (/(+ val (car result-d))2) result-d))
               (push val result-d))
          do (let((val (pop list-tf)))
                (when result-tf
                 (push (interpol_fun val (car result-tf) 0.5) result-tf))
               (push val result-tf))
          do (let((val (pop list-af)))
               (when result-af
                 (push (interpol_fun val (car result-af) 0.5) result-af))
               (push val result-af)))
    (make-instance 'ptl :the-list(nreverse result)
                   :amplitudes (nreverse result-amp)
                   :entry-delays (nreverse result-ed)
                   :durs (nreverse result-d)
                   :transp_funs (nreverse result-tf)
                   :amp_funs (nreverse result-af))))

(defmethod low-pass-filter ((fql fql) &rest fq-c)
"
Cut all the frequencies above fq-c
"
  (let* ((list (copy-list (get-fql fql)))
         (pos (position-if #'(lambda (x) (> x (car fq-c))) list)))
    (if (null pos)(setf pos 0)(setf pos (- (length list)pos)))
    (let ((result (butlast list pos))
          (result-amp (butlast (get-amp fql) pos)))
      (if result
        (make-instance 'fql :the-list result
                     :amplitudes result-amp)
        ()))))
 
(defmethod low-pass-filter ((ptl ptl) &rest fq-c)
"
Cut all the frequencies above fq-c
"
  (let* ((list (copy-list (get-fql ptl)))
         (pos (position-if #'(lambda (x) (> x (car fq-c))) list)))
    (if (null pos)(setf pos 0)(setf pos (- (length list)pos)))
    (let ((result (butlast list pos))
          (result-amp (butlast (get-amp ptl) pos))
          (result-ed (butlast (entry-delays ptl) pos))
          (result-d (butlast (durs ptl) pos))
          (result-tf (butlast (transp_funs ptl) pos))
          (result-af (butlast (amp_funs ptl) pos)))
      (if result
        (make-instance 'ptl :the-list result
                       :amplitudes result-amp
                       :entry-delays result-ed
                       :durs result-d
                       :transp_funs result-tf
                       :amp_funs result-af)
        ()))))

(defmethod high-pass-filter ((fql fql) &rest fq-c)
"
Cut all the frequencies below fq-c
"
  (let* ((list (copy-list (get-fql fql)))
         (pos (position-if #'(lambda (x) (> x (car fq-c))) list)))
    (when (not(null pos))
      (let* ((pos (-(length list)pos))
             (result (last list pos))
             (result-amp (last (get-amp fql) pos)))
        (if result
          (make-instance 'fql :the-list result
                         :amplitudes result-amp)
          ())))))

(defmethod high-pass-filter ((ptl ptl) &rest fq-c)
"
Cut all the frequencies below fq-c
"
  (let* ((list (copy-list (get-fql ptl)))
         (pos (position-if #'(lambda (x) (> x (car fq-c))) list)))
    (when (not(null pos))
      (let* ((pos (-(length list)pos))
             (result (last list pos))
             (result-amp (last (get-amp ptl) pos))
             (result-ed (last (entry-delays ptl) pos))
             (result-d (last (durs ptl) pos))
             (result-tf (last (transp_funs ptl) pos))
             (result-af (last (amp_funs ptl) pos)))
        (if result
          (make-instance 'ptl :the-list result
                         :amplitudes result-amp
                         :entry-delays result-ed
                         :durs result-d
                         :transp_funs result-tf
                         :amp_funs result-af)
          ())))))


(defmethod band-pass-filter ((fql fql) &rest fq-c-list)
"
Cut all the frequencies outside fq-c-list
"
  (let* ((list (copy-list (get-fql fql)))
         (pos (position-if #'(lambda (x) (> x (apply #'max fq-c-list))) list)))
    (if (null pos)(setf pos 0)(setf pos (-(length list)pos)))
    (let* ((result (butlast list pos))
           (result-amp (butlast (get-amp fql) pos))
           (pos (position-if #'(lambda (x) (> x (apply #'min fq-c-list))) result)))
      (when (not(null pos))
        (let* ((pos (-(length result)pos))
               (result (last result pos))
               (result-amp (last result-amp pos)))
          (if result
            (make-instance 'fql :the-list result
                           :amplitudes result-amp)
            ()))))))

(defmethod band-pass-filter ((ptl ptl) &rest fq-c-list)
"
Cut all the frequencies outside fq-c-list
"
  (let* ((list (copy-list (get-fql ptl)))
         (pos (position-if #'(lambda (x) (> x (apply #'max fq-c-list))) list)))
    (if (null pos)(setf pos 0)(setf pos (-(length list)pos)))
    (let* ((result (butlast list pos))
           (result-amp (butlast (get-amp ptl) pos))
           (result-ed (butlast (entry-delays ptl) pos))
           (result-d (butlast (durs ptl) pos))
           (result-tf (butlast (transp_funs ptl) pos))
           (result-af (butlast (amp_funs ptl) pos))
           (pos (position-if #'(lambda (x) (> x (apply #'min fq-c-list))) result)))
      (when (not(null pos))
        (let* ((pos (-(length result)pos))
               (result (last result pos))
               (result-amp (last result-amp pos))
               (result-ed (last result-ed pos))
               (result-d (last result-d pos))
               (result-tf (last result-tf pos))
               (result-af (last result-af pos)))
          (if result
            (make-instance 'ptl :the-list result
                           :amplitudes result-amp
                           :entry-delays result-ed
                           :durs result-d
                           :transp_funs result-tf
                           :amp_funs result-af)
            ()))))))


(defmethod band-reject-filter ((fql fql) &rest fq-c-list)
"
Cut all the frequencies within fq-c-list
"
  (let* ((list1 (copy-list (get-fql fql)))
         (list2 (copy-list (get-fql fql)))
         (pos1 (position-if #'(lambda (x) (> x (apply #'min fq-c-list))) list1))
         (pos2 (position-if #'(lambda (x) (> x (apply #'max fq-c-list))) list2))
         (result1 ())(result-amp1 ())
         (result2 ())(result-amp2 ()))
    (if (null pos1)
      (setf pos1 0)
      (setf pos1 (-(length list1) pos1)))
    (setf result1 (butlast list1 pos1)
          result-amp1 (butlast (get-amp fql) pos1))
    (when pos2
      (setf pos2 (-(length list2) pos2)
            result2 (last list2 pos2)
            result-amp2 (last (get-amp fql) pos2)))
    (if (or result1 result2)
      (make-instance 'fql :the-list (append result1 result2)
                     :amplitudes (append  result-amp1 result-amp2))
      ())))


(defmethod band-reject-filter ((ptl ptl) &rest fq-c-list)
"
Cut all the frequencies within fq-c-list
"
  (let* ((list1 (copy-list (get-fql ptl)))
         (list2 (copy-list (get-fql ptl)))
         (pos1 (position-if #'(lambda (x) (> x (apply #'min fq-c-list))) list1))
         (pos2 (position-if #'(lambda (x) (> x (apply #'max fq-c-list))) list2))
         (result1 ())(result-amp1 ())(result-ed1 ())(result-d1 ())(result-tf1 ())(result-af1 ())
         (result2 ())(result-amp2 ())(result-ed2 ())(result-d2 ())(result-tf2 ())(result-af2 ()))
    (if (null pos1)
      (setf pos1 0)
      (setf pos1 (-(length list1) pos1)))
    (setf result1 (butlast list1 pos1)
          result-amp1 (butlast (get-amp ptl) pos1)
          result-ed1 (butlast (entry-delays ptl) pos1)
          result-d1 (butlast (durs ptl) pos1)
          result-tf1 (butlast (transp_funs ptl) pos1)
          result-af1 (butlast (amp_funs ptl) pos1)
          )
    (when pos2
      (setf pos2 (-(length list2) pos2)
            result2 (last list2 pos2)
            result-amp2 (last (get-amp ptl) pos2)
            result-amp2 (last (get-amp ptl) pos2)
            result-ed2 (last (entry-delays ptl) pos2)
            result-d2 (last (durs ptl) pos2)
            result-tf2 (last (transp_funs ptl) pos2)
            result-af2 (last (amp_funs ptl) pos2)
            ))
    (if (or result1 result2)
      (make-instance 'ptl :the-list (append result1 result2)
                     :amplitudes (append  result-amp1 result-amp2)
                     :entry-delays (append result-ed1 result-ed2)
                     :durs (append result-d1 result-d2)
                     :transp_funs (append result-tf1 result-tf2)
                     :amp_funs (append result-af1 result-af2)
                     )
      ())))


(defmethod change-resolution ((fql fql) &key (approx nil))
"
Change the frequency resolution of an fql. If some freqs are doubled,
eliminate then and keep only the one with the highest amplitude.
If approx is nil, use semitones. 50 = quarter tone.
ms_110902
"
    (let ((appxl (approx-freqs (the-list fql) approx))
          (amps (amplitudes fql))
          (bws (bwl fql)))
      (make_fql (remove-duplicate-freqs (list appxl amps bws)))))

(defmethod change-resolution (l &key (approx nil))
"
When l is not recognized, simply do nothing and pass it over
ms_1109
"
   l)

(defmethod change-resolution ((ptl ptl) &key (approx nil))
"
Change the frequency resolution of an fql. If some freqs are doubled,
eliminate then and keep only the one with the highest amplitude.
If approx is nil, use semitones. 50 = quarter tone.
ms_110902
"
  (let ((appxl (approx-freqs (the-list ptl) approx))
        (amps (amplitudes ptl))
        (durs (durs ptl))
        (edels (entry-delays ptl))
        (tfuns (transp_funs ptl))
        (afuns (amp_funs ptl)))
    (let ((all (remove-duplicate-freqs (list appxl amps durs edels tfuns afuns))))
      (make-instance 'ptl :the-list (first all)
                     :amplitudes (second all)
                     :entry-delays (fourth all)
                     :durs (third all)
                     :transp_funs (fifth all)
                     :amp_funs (sixth all)
                     )
      )))

(defmethod approx-freqs (lfreqs approx)
  "Recomputes the frequencies with a given approximation [cents], nil = do nothing, 50 = quarter tones"
  (pch->fq (fq->pch lfreqs approx)))

(defmethod remove-duplicate-freqs (lfreqs)
"Lfreqs must be a list of lfreqs [lamps lbws]"
  (let ((freqs (car lfreqs))
        (amps (cadr lfreqs))
        (bwl (caddr lfreqs)))
    (cond ((null amps) ; only freqs
           (list (remove-duplicates freqs :test #'=)))
          (t ; only amps and freqs
           (let* ((all (om::mat-trans lfreqs)) ;place fqs and amps together
                  (sortedamps (sort all #'< :key #'cadr))) ; sort by amps, so that the last duplicate is the loudest
             (om::mat-trans (sort (remove-duplicates sortedamps :test #'= :key #'car) #'< :key #'car))))))) ; remove and rebuild the original structure


#|

;ms_1109
(defmethod add-randomize ((x fql) range xpitvls)
"
"
  (let ((freqs (the-list fql))
        (amps (amplitudes fql))
        (bws (bwl fql)))
    (make_fql (remove-duplicate-freqs (list appxl amps bws)))))

(model (copy-model x))
        result)
;    (print model)
    (loop for fql in (fql-list x)
          do (if (the-list fql) 
               (push (apply fun (append (list fql) args)) result)
               (push () result)))       ;to allow empty fql in models
    (setf (fql-list model)(reverse result))
    model))
|#
 
