;=====================================================
; CHROMA 
;=====================================================
; part of the OMChroma library
; -> High-level control of sound synthesis in OM
;=====================================================
;
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;=====================================================

;---------------------------------------------------------------;
;****** USER-DEFINED FUNCTIONS FOR MODIFYING MODELS' ***********;
;****** TEMPORAL DATA ******************************************;
;---------------------------------------------------------------;

(in-package :cr)

; AVAILABLE TIME FUNCTIONS (alphabetical list)
; durs->onsets
; durs->segs
; get-onsets
; get-second
; min-dur
; onsets->durs
; onsets->ioi
; onsets->timestruct
; onsets+ovlp
; remove-segments
; segs->durs
; temporal-pivot
; time-map-fun
; time-shift
; time-sht
; time-stretch
; time-sth

; AVAILABLE AUXILIARY FUNCTIONS (alphabetical list)
; check_dur
; modify_durs
; within? / within-list? / within-times?

;---------------------------------------------------------------;
; TIME FUNCTIONS
(defmethod remove-segments ((lsegs list) (ltimes list) epsilon &optional lvps)
  ;:indoc '("List of time segments" "List of time values" "Minimum time resolution" "Optional list")
  ;:initvals '('((0 1) (1 1.4) (2 2.1)) '(1.2 2) 0.001 '(a b c)) 
  ;:numouts 2
  ;:doc "Remove the time segment(s) that correspond to the given time values.
;If the segments are single markers, they are converted into adjacent time segments,
;before applying remove-segments.
;If a list of vps's is also given, remove the corresponding vps as well.
;The match is done within the precision of an epsilon [0.001sec] of the given value."
  ;:icon 2002

            (let ((lsegs (if (listp (car lsegs)) lsegs (onsets->timestruct lsegs))))
              (if lvps
                  (loop for seg in lsegs for vps in lvps
                        unless (within-times? seg ltimes epsilon) collect seg into time-segs and collect vps into vps-segs
                        end
                        finally return (values time-segs vps-segs))
                
                (loop for seg in lsegs
                      unless (within-times? seg ltimes epsilon) collect seg into time-segs
                      end
                      finally return time-segs)
                )))
            
;(remove-segments '(0 1.1 2 3.3 4 5.5 6 7.7) '(2.3 4.5 6.0) 0.01)
;(remove-segments '(0 1.1 2 3.3 4 5.5 6 7.7) '(4.5 2.3 6.0 3.2) 0.001)
;(remove-segments '((0 0.3) (1.1 1.3) (2 2.1) (3.3 4.0) (4.0 5.0) (5.5 6.0) (6.0 7.5) (7.7 8.0)) '(2.3 4.5 6.0) 0.001)
;(remove-segments '((0 0.3) (1.1 1.3) (2 2.1) (3.3 4.0) (4.0 5.0) (5.5 6.0) (6.0 7.5) (7.7 8.0)) '(2.3 4.5 6.0) 0.001 '(a b c d e f g h i j k l))
;(remove-segments '(0 1.1 2 3.3 4 5.5 6 7.7) '(2.3 4.5 6.0) 0.01 '(a b c d e f g h i j k l))


(defun within? (val segment epsilon)
"If val is contained within the time segment ± espilon, return the segment"
  (when (and val (and (>= val (- (car segment) epsilon)) (<= val (+ (cadr segment) epsilon)))) segment))

; (within? () '(1.1 2.0) 0.31)
; (within? 2.3'(1.1 2.0) 0.01)

(defun within-list? (val lsegs epsilon)
"If val is contained within the list of time segment ± espilon, return the corresponding segment"
  (loop for seg in lsegs
        for within = (within? val seg epsilon)
        until within
        finally return within))

; (within-list? 4.1 '((0.0 1.0) (1.1 2.0) (2.2 3.0)) 0.3)
; (within-list? 1.3 '((0.0 1.0) (1.1 2.0) (2.2 3.0)) 0.01)

(defun within-times? (seg ltimes epsilon)
"If a time of list of time values ± espilon is contained within the time segment, return it"
  (loop for tim in ltimes
        for within = (within? tim seg epsilon)
        until within
        finally return within))

; (within-times? '(0.0 1.0) '(1.1 2.2 3.3) 0.01)
; (within-times? '(0.0 1.0) '(1.1 2.2 3.3) 0.2)
; (within-times? '(4.0 5.0) '(1.1 2.2 3.3) 0.01)
; (within-times? '(1.1 2.0) '(2.3 3.3) 0.03)

;---------------------------------------------------------------;
; MINIMUM DURATION
(defmethod min-dur ((ltimes list) mindur &optional lvps)
  ;:indoc '("List of durations or time segments" "Minimum duration (use chroma's global value as default)" "List of vps")
  ;:initvals '('(0 1 1.05) 0.1 '(a b c)) 
  ;:numouts 2
  ;:doc "Remove the duration(s) or time segment(s) that are shorter than the minimum duration.
  ;If a list of vps's is also given, remove the corresponding vps as well. If mindur is nil, do nothing."
  :icon 2002
            (if mindur
              (let ((ltim ltimes))
                (if lvps
                    (loop for seg in ltim for vps in lvps
                          for dur = (if (listp seg) (- (cadr seg) (car seg)) seg)
                          when (>= dur mindur) collect seg into time-segs and collect vps into vps-segs
                          end
                          finally return (values time-segs vps-segs))
                
                  (loop for seg in ltim
                        for dur = (if (listp seg) (- (cadr seg) (car seg)) seg)
                        when (>= dur mindur) collect seg into time-segs
                        end
                        finally return time-segs)
                 ))
            (values ltimes lvps)))

;(min-dur '((0 1.1) (1 2.2) (3 4.4) (5 6.6)) 1.2)
;(min-dur '(0.3 1.4 1.5 1.1 0.5) 1.3 '(a b c d e))
;(min-dur '(0.3 1.4 1.5 1.1 0.5) 1.)
;(min-dur '(0.3 1.4) () '(a b c))

;---------------------------------------------------------------;
;ms_1205
(defmethod temporal-pivot ((ltimes list) (nth number) (tp number))
  ;:indoc '("List of times or time segments" "Nth el in list" "Temporal pivot for that element")
  ;:initvals '('((0 1) (1 1.4) (2 2.1)) 2 10.0) 
  ;:numouts 1
  ;:doc "Place the nth element of a list of times to tp Place the other vals around."
  ;:icon 2002
  (let* ((ref-time (nth nth ltimes))
         (time (if (listp ref-time) (car ref-time) ref-time))
         (tp (- tp time)))
    (om+ tp ltimes)))

;(temporal-pivot '((0 0.4) (1.1 1.8) (2.2 2.9) (3.3 3.9) (4.5 5.5)) 2 10.0)
;(temporal-pivot '(0 1.1 2.2 3.3 4.5) 3 10.0)

;---------------------------------------------------------------;
;ms_1205 (new)
(defmethod time-shift ((ltimes list) (offset t) &key (sortflag t))
  ;:indoc '("List of times or time segments" "Time offset" ":sortflag Sort flag [t]")
  ;:initvals '('((0 1) (1 1.4) (2 2.1)) '(1.2 2)) 
  ;:numouts 1
  ;:doc "Add the value of offset to all the times or time segments.
;If offset is nil, set the first time to 0.0.
;If offset is a list, use each value of the list as an offset, repeating the last one, if needed.
;If during the stretch times are not increasing, and sortflag is t, reorder the list.
;Negative times are eliminated (with warning message)."
  ;:icon 2002

  (let* ((l (length ltimes))
         (loffsets (cond
                    ((numberp offset) (cr::rept l offset))
                    ((null offset) (if (listp (car ltimes)) (cr::rept l (- (caar ltimes))) (cr::rept l (- (car ltimes)))))
                    ((listp offset) (cr::l-val l offset))
                    (t (om-beep-msg (format () "I cannot understand what you mean with ~a" offset)))))
         (result (loop for time in ltimes for offset in loffsets
                       collect (om+ offset time))))
    (if sortflag (sort. result '< (when (listp (car result)) 'car)) result)))


;(time-shift '(1 2.2 3.3 4.4) 0)
;(time-shift '(1 2.2 3.3 4.4) 10.0)
;(time-shift '((1.1 2) (2.2 3.0) (3.3 4.) (4.4 10.0)) 0.0)
;(time-shift '((1.1 2) (2.2 3.0) (3.3 4.) (4.4 10.0)) nil)
;(time-shift '((1.1 2) (2.2 3.0) (3.3 4.) (4.4 10.0)) 10)
;(time-shift '(1 2.2 3.3 4.4) '(1 2 3))
;(time-shift '(1 2.2 3.3 4.4) '(1 2 3 0.1))
;(time-shift '(1 2.2 3.3 4.4) '(1 2 3 0.1) :sortflag nil)
;(time-shift '((1.1 2) (2.2 3.0) (3.3 4.) (4.4 10.0)) '(2 3))

;---------------------------------------------------------------;
;ms_1205 (new)
(defmethod time-stretch ((ltimes list) stretch &key (exp 1.0) (sortflag t))
  ;:indoc '("List of times or time segments" "Stretching value" ":exp Exponent [1]" ":sortflag [t]")
  ;:initvals '(((0 1) (1 1.4) (2 2.1)) (1.2 2) 2.0 1.0) 
  ;:numouts 1
  ;:doc "Multiply all the times or time segments by stretch raised to the exp'th power [default exp = 1.0, no power].
;If stretch is a list, use each value of the list as an offset, repeating the last one, if needed.
;If during the stretch times are not increasing, and sortflag is t, reorder the list.
;Negative times are eliminated (with warning message)."
  ;:icon 2002

  (let* ((l (length ltimes))
         (lstretches (cond
                      ((numberp stretch) (cr::rept l stretch))
                      ((listp stretch) (cr::l-val l stretch))
                      (t (om-beep-msg (format () "I cannot understand what you mean with ~a" stretch)))))
         (result (loop for time in ltimes for stretch in lstretches
                       collect (om* (expt stretch exp) time))))
    (if sortflag (sort. result '< (when (listp (car result)) 'car)) result)))

;(time-stretch '(1 2.2 3.3 4.4) 2.0)
;(time-stretch '(1 2.2 3.3 4.4) 2.0 :exp 0.8)
;(time-stretch '((1.1 2) (2.2 3.0) (3.3 4.) (4.4 10.0)) 2.0)
;(time-stretch '((1.1 2) (2.2 3.0) (3.3 4.) (4.4 10.0)) 2.0 :exp 1.1)
;(time-stretch '((1.1 2) (2.2 3.0) (3.3 4.) (4.4 10.0)) '(1.0 0.5 0.3 0.1) :sortflag nil)
;(time-stretch '((1.1 2) (2.2 3.0) (3.3 4.) (4.4 10.0)) '(1.0 0.5 0.3 0.1) :sortflag t)

;---------------------------------------------------------------;
;ms_1205 (new)
(defmethod time-sht ((ltimes list) offset stretch &key (exp 1.0) (sortflag t))
  ;:indoc '("List of times or time segments" "Time offset" "Stretching value" "Exponent")
  ;:initvals '(((0 1) (1 1.4) (2 2.1)) 0.0 1.0 1.0) 
  ;:numouts 1
  ;:doc "Apply time-shift to time-stretched values."
  ;:icon 2002  
  (time-shift (print (time-stretch ltimes stretch :exp exp :sortflag sortflag)) offset :sortflag sortflag))

(defmethod time-sth ((ltimes list) offset stretch &key (exp 1.0) (sortflag t))
  ;:indoc '("List of times or time segments" "Time offset" "Stretching value" "Exponent")
  ;:initvals '(((0 1) (1 1.4) (2 2.1)) 0.0 1.0 1.0) 
  ;:numouts 1
  ;:doc "Apply time-stretch to time-shifted values."
  ;:icon 2002
  (time-stretch (time-shift ltimes offset :sortflag sortflag) stretch :exp exp :sortflag sortflag))

;(time-shift (time-stretch '(0 1 2) 2.0 :exp 1.0) 1.1)
;(time-stretch (time-shift '(0 1 2) 1.1) 2.0 :exp 1.0)
;(time-sht '(1 2.2 3.3 4.4) 0.0 1.0)
;(time-sth '(1 2.2 3.3 4.4) 0.0 1.0)
; NB: different vals only if both args are not neutral
;(time-sht '(1 2.2 3.3 4.4) 1.1 1.0)
;(time-sth '(1 2.2 3.3 4.4) 1.1 1.0)

;NB: if sortflag=(), result may be different than same function call, with sortflag=t
;(time-sht '(1 2.2 3.3 4.4) '(0.0 1.1 3.3 -1.0) '(2.0 1.0 0.9 0.3) :exp 1.0 :sortflag ())
;(time-sht '(1 2.2 3.3 4.4) '(0.0 1.1 3.3 -1.0) '(2.0 1.0 0.9 0.3) :exp 1.0 :sortflag t)
;(time-shift '(2.0 2.2 2.9699998 1.32) '(0.0 1.1 3.3 -1.0) :sortflag ())
;(time-shift '(1.32 2.0 2.2 2.9699998) '(0.0 1.1 3.3 -1.0) :sortflag t)
;(time-sth '(1 2.2 3.3 4.4) 0.0 2.0)

;(time-sht '(1 2.2 3.3 4.4) 1.1 2.0)
;(time-sth '(1 2.2 3.3 4.4) 1.1 2.0)

;(time-sht '((1.1 2) (2.2 3.0) (3.3 4.) (4.4 10.0)) 2.0 1.1)
;(time-sht '((1.1 2) (2.2 3.0) (3.3 4.) (4.4 10.0)) 2.0 1.2)

;---------------------------------------------------------------;
;OBSOLETE???
(defmethod time-map-fun ((markers list) fun &optional (exp 1.0))
  ;:indoc '("List of times or time segments" "Stretching bpf or chroma fun" "Exponent")
  ;:initvals '(((0 1) (1 1.4) (2 2.1)) 2.0 (simple-bpf-from-list (0 1 2) (0.0 1.0 10.0) bpf 5) 1.0)
  ;:numouts 1
  ;:doc "Multiply all the times or time segments by the value coming from sampling the table from the beginning to the end."
  ;:icon 658
  (let ((fun2 (copy-list (if (cr::is_fun fun) fun (bpf->fun fun))))
        (markers2 (copy-list markers))
        new-durs)
    (cr::x-resc_fun fun2 (first markers2) (car (last markers2)))
    (setf new-durs (mapcar
                    (lambda (time dur) (* dur (expt (cr::y-val_fun fun2 time) exp)))
                    markers2 (cr::markerstodur markers2)))
    (cr::durtotime (first markers2) new-durs)))


;ms_1205 (new)
(defmethod time-map ((segs list) timefun &key (exp 0.0) (durmin (cr::get-gbl 'cr::durmin)) (durmode t) (sortflag ()) (mapmode 'rel) (apply 'all))
  ;:indoc '("List of onsets or time segments [(<beg-onset i> <end-onset i>)]"
  ;         "Time map [bpf or chroma fun]"
  ;         "Exponent [0.0=linear]" "Minimum allowed duration [0.01]" "Behaviour if too small dur" )
  ;:initvals '(((0 0.5) (1 1.5) (2 2.5) (3 3.5) (4 4.5)) (simple-bpf-from-list '(0 1) '(1.0 2.0) 'bpf 5))
  ;:numouts 1
  ;:doc "Multiply all the times or time segments by the value coming from sampling the timefun
;from the beginning to the end (mapmode=rel), or absolutely (not yet implemented).
;Apply the timemap depending on the value of :apply: 'all (onsets and durations), 'dur (only durations), 'at (only onsets).
;After the mapping, if the duration is too little:
;   if durmode=(), eliminate the segment, and send a warning,
;      otherwise set the segment's dur to durmin and send a warning."
  ;:icon 658
  (let ((fun2 (copy-list (if (cr::is_fun timefun) timefun (bpf->fun timefun))))
        (segs2 (copy-list (segs->durs segs)))
        (result))
    (cond
     ((eq apply 'all)
      (setf result
            (tmap-dur (tmap-at segs2 fun2 exp) fun2 exp)))
     ((eq apply 'dur)
      (setf result (tmap-dur segs2 fun2 exp)))
     ((eq apply 'at)
      (setf result (tmap-at segs2 fun2 exp)))
     (t (om-beep-msg (format () "Illegal apply mode Restrict your imagination to all, at or dur: ~a" apply))))

    (let ((final_result (check_dur result durmin durmode)))
      (if (null final_result)
          (om-beep-msg
           (format () "Empty list of segments. Check your durmin (~f), please, Sir ~a!!" durmin (cr::get-gbl 'cr::USER)))
        (durs->segs final_result)))))

;(time-map '(0 1 2 3) (cr::make_fun '(1.0 0 2.0 2)))
;(time-map '((0 0.5) (1.1 1.5) (2 2.5) (3.3 3.5)) (cr::make_fun '(1 0 2.0 2)))
;(time-map '((0 0.5) (1.1 1.5) (2 2.5) (3.3 3.5)) (cr::make_fun '(2 0 2.0 2)))
;(time-map '((0 0.5) (1.1 1.5) (2 2.5) (3.3 3.5)) (cr::make_fun '(2 0 2.0 2)) :apply 'at)
;(time-map '((0 0.5) (1.1 1.5) (2 2.5) (3.3 3.5)) (cr::make_fun '(2 0 2.0 2)) :apply 'dur :durmin 0.5)
;(time-map '((0 0.5) (1.1 1.5) (2 2.5) (3.3 3.5)) (cr::make_fun '(2 0 2.0 2)) :apply 'dur :durmin 0.5 :durmode t)
;(time-map '((0 0.5) (1.1 1.5) (2 2.5) (3.3 3.5)) (cr::make_fun '(2 0 2.0 2)) :apply 'dur :durmin 1.5)

;(time-map '(0 1 2 3) (cr::make_fun '(0 0 10.0 2)) 1.1)
;(time-map '(0 1 2 3) (simple-bpf-from-list '(0 1 2) '(0 1.0 10.0) 'bpf 3))
;(time-map '(0 1 2 3) (simple-bpf-from-list '(0 1 2) '(0 1.0 10.0) 'bpf 3) 2.2)


(defun tmap-dur (segs fun &optional (exp 0.0))
  (let ((onsets (get-onsets segs))
        (durs (get-second segs)))
   (mat-trans (list onsets (om* durs (cr::y-list_fun (cr::sample_fun fun (length onsets) exp)))))))
;(tmap-dur '((0 0.5) (1 0.5) (2 0.5) (3 0.5) (4 0.5)) (cr::make_fun '(0 0 10.0 1)))


(defun tmap-at (segs fun &optional (exp 0.0))
  (let ((onsets (get-onsets segs))
        (durs (get-second segs)))
   (mat-trans (list (om* onsets (cr::y-list_fun (cr::sample_fun fun (length segs) exp))) durs))))
;(tmap-at '((0 0.5) (1 1.5) (2 2.0) (3 1.5) (4 1.0)) (cr::make_fun '(0.0 0 10.0 1)))

;---------------------------------------------------------------;
;ms_1205
(defmethod onsets->timestruct (lonsets) 
 ; :icon '(141)
 ; :indoc '("A list of onsets or time segments [(<beg-onset i> <end-onset i>)]")
 ; :initvals '('(0 1 2 3))
;  :doc "Generate a list of time segments (<beg-onset i> <end-onset i>)."
  (onsetsmrk2seg lonsets))
;(onsets->timestruct '(0 1 2 3))
;(onsets->timestruct '((0 1) (2 3) (4 5)))
 

(defmethod onsets+ovlp (lonsets lovlps &key (ovlpmin 0.05) (ovlpmode ()))
  :icon '(141)
  :indoc '("a list of onsets or time segments [(<beg-onset i> <end-onset i>)]"
           "one or more overlapping factors [% of dur or absolute duration, ovlp if > 1.0]"
           "min allowed dur for overlap [sec]"
           "ovlpmode [t=absolute duration, ()=relative duration)]")
  :initvals '((0 1 2 3) (0.5 1.0 1.5))
  :doc "Generate a list of overlapping time segments (<beg-onset i> <end-onset i>).
If a time structure is given, the value of end[i] will be replaced by the computation of the overlap.
If the duration of the overlap is below <ovlpmin> set it to <ovlpmin>.
"
  (let* ((lonsets (get-onsets lonsets))
         (timestruct (onsets->timestruct lonsets))
         (tmstr_ovlp (compute-overlaps timestruct (cr::l-val (length timestruct) (list! lovlps)) ovlpmode))
         (verovlp (verify_overlaps (get-overlaps tmstr_ovlp) ovlpmin)))
(make_timestruct_from_overlap tmstr_ovlp verovlp)))
 
;(onsets+ovlp '(0 1 2 3 10) 0.8)
;(onsets+ovlp '(0 1 2 3 4) 1.01)
;(onsets+ovlp '(0 1 2 3) 0.1 :ovlpmode t :ovlpmin 0.2)
;(onsets+ovlp '((0.0 0.155) (0.155 0.31) (0.31 0.465) (0.465 0.62) (0.62 0.775) (0.775 0.92999995) (0.92999995 1.0849999) (1.0849999 1.2399999) (1.2399999 1.3949999) (1.3949999 1.5499998) (1.5499998 1.7049998) (1.7049998 1.8599998) (1.8599998 2.0149999) (2.0149999 3.0149999)) 1.0)

;(onsets+ovlp '(0 1 2 3 4) '(0.6 0.1 0.9) :ovlpmin 0.5)
;(onsets+ovlp '(0 1 2 3 4) '(1.6 1.1 1.9) :ovlpmin 0.5)
;(onsets+ovlp '(0 1 2 3) 1.1 :ovlpmin 0.2)
;(onsets+ovlp '((0 1) (2 3.5) (4 5.1)) 1.0)
;(onsets+ovlp '((0 1) (2 3.5) (4 5.1)) 1.01)
;(onsets+ovlp '((0 1) (2 3.5) (4 5.1)) 1.01 :ovlpmode t)

; LOCAL FUNS
(defun check_ovlpdur (lsegs minovlp)
  (let ((loverlap (get-overlaps lsegs)))
        (make-timestruc lsegs (verify_overlap loverlap minovlp))))

(defun compute-overlaps (lsegs lovlp &optional (ovlpmode ()))
  (if ovlpmode ; absolute overlaps
      (x-append
       (loop for seg1 in lsegs for seg2 in (cdr lsegs) for ovlp in lovlp 
             collect
             (let* ((t1 (car seg1))
                    (t2 (car seg2))
                    (localdur (- t2 t1)))
               (list t1 (+ t1 (+ localdur ovlp)))))
       (last lsegs))
    (x-append
     (loop for seg1 in lsegs for seg2 in (cdr lsegs) for ovlp in lovlp 
           collect
           (let* ((t1 (car seg1))
                  (t2 (car seg2))
                  (localdur (- t2 t1)))
             (list t1 (+ t1 (* localdur ovlp)))))
     (last lsegs))))
;(compute-overlaps '((0 1) (1 3) (3 4) (4 10)) '(1.0 0.5 2.0))
;(compute-overlaps '((0 1) (1 3) (3 4) (4 10)) '(0.1 0.05 0.2) t)
;(compute-overlaps '((0 1.3) (1.2 2.5) (2 3.5) (4.0 4.5) (5.0 5.9) (5.5 6.0)) '(0.1 1.0 1.5 1.1 0.8))
;(compute-overlaps '((0 1.3) (1.2 2.5) (2 3.5) (4.0 4.5) (5.0 5.9) (5.5 6.0)) '(0.1 1.0 1.5 1.1 0.8) t)

(defun get-overlaps (lsegs)
  (loop for seg1 in lsegs for seg2 in (cdr lsegs)
        collect (- (cadr seg1) (car seg2))))
;(get-overlaps '((0 1.3) (1.2 2.5) (2 3.5) (4.0 4.5) (5.0 5.9) (5.5 6.0)))

(defun verify_overlaps (lovlp min)
  (loop for ovlp in lovlp
        collect (if (and (> ovlp 0.0) (< ovlp min)) min ovlp)))
;(verify_overlaps '(-1 0.0 0.01 0.2) 1.0)

(defun make_timestruct_from_overlap (lsegs loverlap)
  (x-append
   (loop for seg1 in lsegs for seg2 in (cdr lsegs) for ovlp in loverlap
         collect (list (car seg1) (+ (car seg2) ovlp)))
   (last lsegs)))
;(make_timestruct_from_overlap '((0 1.3) (1.2 2.5) (2 3.5) (3.3 5.0)) '(0.5 0.5 0.5 0.5))
;(make_timestruct_from_overlap '((0 1.3) (1.2 2.5) (2 3.5) (3.3 5.0)) '(0.5 0.5 0.5 0.5))


(defmethod durs->timestruct (ldurs &key (begtime 0.0))
  :icon '(141)
  :indoc '("a list of durs or segments [(<beg-onset i> <dur i>)]")
  :initvals '('(0 1 2 3))
  :doc "Convert a list of durations or time segments (<beg-onset i> <dur i>)
into a list of time segments (<onset-beg i> <onset-end i>)."

  (let ((durs (durs->durstruct ldurs :begtime begtime))
        (onsets (durs->onsets ldurs :begtime begtime)))
    (mapcar #'(lambda (x y) (list x (+ x (cadr y)))) onsets durs)))

;     (mat-trans (list (butlast (dx->x begtime ldurs)) ldurs)))))

;(durs->timestruct '(1 2 3 10))
;(durs->timestruct '(1 1 1 1 1 1 1 1 1 1 1 1 5))
;(durs->timestruct '((0 1) (2 1) (4 1)))

(defmethod durs->durstruct (ldurs &key (begtime 0.0))
  :icon '(141)
  :indoc '("a list of durs")
  :initvals '('(0 1 2 3))
  :doc "Convert a list of durations into a list of time segments (<beg-onset i> <dur i>)"
  (if (listp (car ldurs)) ldurs
    (let ((onsets (durs->onsets ldurs :begtime begtime)))
      (mapcar #'(Lambda (x y) (list x y)) (butlast onsets) ldurs))))

;(durs->durstruct '(1 2 3 10))


(defmethod onsets->durstruct (lonsets) 
            :icon '(141)
            :indoc '("a list of onsets or time segments [(<beg-onset i> <end-onset i>)]")
            :initvals '('(0 1 2 3))
            :doc "Convert a list of time segments (<beg-onset i> <end-onset i>)into a list of time segments (<onset i> <dur i>)."
  (let ((time-str (onsets->timestruct lonsets)))
    (mapcar #'(lambda (x) (list (car x) (- (cadr x) (car x)))) time-str)))

;(onsets->durstruct '(0 1 2 3 5))
;(onsets->durstruct '((0 1) (2 5) (6 10)))

(defmethod durs->onsets (ldurs &key (begtime 0.0)) 
            :icon '(141)
            :indoc '("a list of durations or time segments [(<time i> <dur i>)]" "starting time")
            :initvals '('(0 1 2 3))
            :doc "Convert a list of time segments (<time i> <dur i>)into a list of time segments (<beg-onset i> <end-onset i>)."
  (let ((endtimes (if (listp (car ldurs)) (mapcar #'car ldurs) (dx->x begtime ldurs))))
    endtimes))

;(durs->onsets '(1 1 1 5))
;(durs->onsets '(1 1 1 1) :begtime 0.1)
;(durs->onsets '((0 1) (2 1) (4 1)))

(defmethod onsets->ioi (time-struct)
            :icon '(141)
            :indoc '("lonsets or time structure")
            :initvals '('(0 1 2 3))
            :doc "Return the IOI of a time structure"
            (if (listp (car time-struct))
                (x->dx (mapcar 'car time-struct))
              (x->dx time-struct)))

;(onsets->ioi '(0 1 2 3))
;(onsets->ioi '((0 1) (2 1) (4 1)))
;(onsets->ioi '((0 0.5) (1.1 1.5) (2.0 2.5) (3.3 3.5)))

(defmethod get-onsets (time-struct)
            :icon '(141)
            :indoc '("lonsets or time structure")
            :initvals '('(0 1 2 3))
            :doc "Return the onsets of a time structure, including the last one."
            (if (listp (car time-struct))
                (x-append (mapcar 'car time-struct) (cdr (last-elem time-struct)))
              time-struct))

;(get-onsets '(0 1 2 3))
;(get-onsets '((0 0.5) (1.1 1.5) (2.2 2.5) (3.3 3.5)))

(defmethod get-second (time-struct)
            :icon '(141)
            :indoc '("lonsets or time structure")
            :initvals '('(0 1 2 3))
            :doc "Return the second value (endtime or dur) of a time structure"
            (if (listp (car time-struct))
                (mapcar 'cadr time-struct)
              (get-second (onsets->timestruct time-struct))))

;(get-second '(0 1 2 3))
;(get-second '((0 0.5) (1.1 1.5) (2.2 2.5) (3.3 3.5)))


;---------------------------------------------------------------;
;ms_1205
(defun modify_durs (durs fact op)
  (if (listp fact)
      (moddurs durs fact op)
    (mapcar #'(lambda (dur) (funcall op fact dur)) durs)))

(defun moddurs (durs lfact op)
  (let ((facts (cr::l-val (length durs) lfact)))
    (loop for fact in facts for dur in durs
        collect (funcall op fact dur))))
;(moddurs '(1 2 3 4 5) '(0.1 1.0 2.1) '+)
;(moddurs '(1 2 3 4 5) '(0.1 1.0 2.0) '*)

;(modify_durs '(0 1 2 3) 1.0 '+)
;(modify_durs '(0 1 2 3) 2.0 '*)
;(modify_durs '(0 1 2 3) '(0.1 1.0 2.1) '+)
;(modify_durs '(0 1 2 3) '(0.0 1.0 2.0) '*)

;---------------------------------------------------------------;
;ms_1205
(defmethod durs->segs (ldurs  &key (begtime 0.0) (fact ()) (op '+) (durmin (cr::get-gbl 'cr::durmin)) (durmode t))
  :icon '(141)
  :indoc '("a list of durations or time segments [(ti duri)]" "initial time (only for ldurs)"
           "factor [0.0]" "operator [+]" "minimum allowed duration [0.01]" "behaviour if too small dur")
  :initvals '('(0 1 2 3) 0.0)
  :doc "Convert a list of time segments (<onset i> <dur i>) into a list of time segments (<beg-onset i> <end-onset i>).
Before the conversion, apply a factor to the duration, according to the operator.
Factor can be a number or a list. In the latter case, apply each value to each dur, repeating the last one, if needed.
If the duration is too little:
   if durmode=(), eliminate the segment, and send a warning,
      otherwise set the segment's dur to durmin and send a warning.
Negative times are eliminated (with warning message). *** STILL TO DO!"

  (let* ((segs (durs->timestruct ldurs begtime))
         (vals (values (mat-trans segs)))
         (onsets (first vals))
         (durs (second vals))
         (durs (if fact (modify_durs durs fact op) durs))
         (result (mat-trans (list onsets durs)))
         (final-result (check_dur result durmin durmode)))

    (if (null final-result)
        (om-beep-msg
         (format () "Empty list of segments. Check your durmin (~f), please, Sir ~a!!" durmin (cr::get-gbl 'cr::USER)))
          (loop for seg in final-result
               collect (list (car seg) (+ (car seg) (cadr seg)))))
    ))

;(durs->segs '(1 1 1 1 1) 0.0)
;(durs->segs '(1 1 1 1 1) 0.0 :fact 0.1)
;(durs->segs '(1 1 1 1 1) 0.0 :fact 0.1 :op '*)
;(durs->segs '(1 1 1 1 1) 0.0 :fact 0.01 :op '* :durmin 0.1)

;(durs->segs '((0 1) (2 1)  (3 1) (4 1) (5 1)))
;(durs->segs '((0 1) (2 1)  (3 1) (4 1) (5 1)) 0.0 :fact '(-0.1 0 0.1))
;(durs->segs '(1 1 1 1 1) 0.0 :fact '(0.1 0.2 0.3) :op '*)
;(durs->segs '(1 1 1 1 1) 0.0 :fact 0.01 :op '* :durmin 0.1)


(defmethod segs->durs (lonsets &key (fact ()) (op '+) (durmin (cr::get-gbl 'cr::durmin)) (durmode t))
  :icon '(141)
  :indoc '("a list of onsets or time segments [(<beg-onset i> <end-onset i>)]"
           "factor [0.0]" "operator [+]" "minimum allowed duration" "behaviour if too small dur")
  :initvals '('(0 1 2 3))
  :doc "Convert a list of time segments (<beg-onset i> <end-onset i>) into a list of time segments (<onset i> <dur i>).
After the conversion, apply a factor to the duration, according to the operator.
Factor can be a number or a list. In the latter case, apply each value to each dur, repeating the last one, if needed.
If the duration is too little:
   if durmode=(), eliminate the segment, and send a warning,
      otherwise set the segment's dur to durmin and send a warning.
Negative times are eliminated (with warning message)."

  (let* ((segs (onsets->timestruct lonsets))
         (vals (values (mat-trans segs)))
         (onsets (first vals))
         (durs (onsets->durs lonsets))
         (durs (if fact (modify_durs durs fact op) durs))
         (result (mat-trans (list onsets durs)))
         (final_result (check_dur result durmin durmode)))
    (if (null final_result) (om-beep-msg
                             (format () "Empty list of segments. Check your durmin (~f), please, Sir ~a!!" durmin (cr::get-gbl 'cr::USER)))
         final_result)))

;(segs->durs '(0 1 2 3 4))
;(segs->durs '(0 1 2 3 4) :fact 0.1)
;(segs->durs '(0 1 2 3 4) :fact 0.1 :op '*)
;(segs->durs '(0 1 2 3 4) :fact '(0.1 0.2 0.3))
;(segs->durs '(0 1 2 3 4) :fact '(0.1 0.2 0.3) :op '*)
;(segs->durs '(0 1 2 3 4) :fact '(0.1 0.2 0.3) :durmin 0.2)
;(segs->durs '(0 1 2 3 4) :fact '(0.1 0.2 0.3) :op '* :durmin 0.2)
;(segs->durs '(0 1 2 3 4) :fact '(0.1 0.2 0.3) :op '* :durmin 0.2 :durmode t)

;(segs->durs '((0 1) (2 3) (5 6)) :fact 0.1)
;(segs->durs '((0 1) (2 3) (5 6)) :fact 0.1)
;(segs->durs '((0 1) (2 3) (5 6)) :fact 0.1 :op '*)
;(segs->durs '((0 1) (2 3) (5 6)) :fact 0.1 :op '*)
;(segs->durs '((0 1) (2 3) (5 6)) :fact '(0.1 0.2 0.3) :durmin 1.11)
;(segs->durs '((0 1) (2 3) (5 6)) :fact '(0.1 0.2 0.3) :durmin 1.11 :durmode t)
;(segs->durs '((0 1) (2 3) (5 6)) :fact 0.1 :op '* :durmin 0.2)
;(segs->durs '((0 1) (2 3) (5 6)) :fact 0.1 :op '* :durmin 0.2 :durmode t)


(defun check_dur (lsegs mindur mode)
  (let ((result
         (loop for seg in lsegs
               collect (list (car seg)
                             (if (>= (cadr seg) mindur) (cadr seg)
                               (if mode (progn (om-beep-msg (format () "Too small dur: ~f. Replaced with mindur (~f)" (cadr seg) mindur))
                                          mindur)
                                 (progn (om-beep-msg (format () "Too small dur: ~f (mindur=~f). Segment eliminated" (cadr seg) mindur))
                                   ())))))))
    (remove () result :key 'cadr)))

;(check_dur '((0 0.1) (1 0.2) (3 0.3) (4 0.4)) 0.01 ())
;(check_dur '((0 0.1) (1 0.2) (3 0.3) (4 0.4)) 0.2 ())
;(check_dur '((0 0.1) (1 0.2) (3 0.3) (4 0.4)) 0.2 t)
;***************************************************************;
