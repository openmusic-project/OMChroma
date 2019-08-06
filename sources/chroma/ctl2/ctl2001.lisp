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
; File author: M. Stroppa, S. Lemouton
;=====================================================

(in-package :cr)


;;; a version of CTL2 independent from OM classes and structures.
;;; returns a list containing :
;;; 1) the number of elements
;;; 2) a list of global arguments as specified by <global-args> names
;;; (note: <global-args> can already be a simple list of a list of pairs (value default)
;;; 2) a list of synthesis parameters :keys and values as specified by <synth-args> e.g. (:freq (freq-list) :amp (amps-list) ...)

(defmethod CTL2 ((my-model cr::chroma-model)
                 global-args synth-args
                 rules
                 &key (outfile "yaka-out") (args-yaka-user nil)
                 (by-time nil))     
  "Generate a synthesis object by combining a control and an analysis model.
Write in the Intermediate-files folder by default."
  
  (let ((out-path (get-cr-path :out :name outfile :type "ctl2"))
        (n-fql (1- (nev my-model)))
        ;; resc-min resc-max gblamp-val   ;; <= not used
        )
    
    (with-open-file (outstream outfile :direction :output :if-exists :supersede)

      ;;; outstream will be used by CTL functions
      (declare (special outstream))
      
      (format outstream "(defun cr::ctl2-result() ~%")
      (format outstream "(list ~%")

      ;;; not used anywhere... ?
      ;(if by-time
      ;  (setf resc-min (begin-time my-model) resc-max (get-nth-time my-model n-fql))
      ;  (setf resc-min 0 resc-max n-fql))
      
      (loop for i from 0 to n-fql
            do (CTL2_compute_event my-model global-args synth-args 
                                   rules i args-yaka-user))
      
      (format outstream "))~%"))
    
    (load out-path))
  
  (cr::ctl2-result))


(defmethod CTL2_compute_event ((model cr::model-partials) 
                               global-args synth-args 
                               rules my-rank 
                               args-yaka-user)
  "ctl2 subroutine, process 1 ptl/fql"
  (declare (special outstream my-model my-rank))
  
  (let* ((my-model model)
         (my-time (get-nth-time my-model my-rank))
         (my-dur (get-nth-dur my-model my-rank))
         (n-fql (1- (nev my-model)))      ; n-fql - 1 , nmarkers - 2
         (my-ptl (nth my-rank (ptl-list my-model)))
         (my-fql my-ptl)
         (my-nev (length (the-list my-ptl))))
    
    (declare (special my-model n-fql my-time my-dur my-nev my-fql my-ptl))
    
    (when my-ptl         ;to allow empty fql in models
      
      (when args-yaka-user (apply 'yaka-user args-yaka-user))
      
      (when (get-gbl 'ctl2-print) (format t "~a~%" my-time))

      (format outstream "(list ~a ~%" my-nev)

      (let ((global-args-names (loop for arg in global-args 
                                     collect (if (listp arg) (car arg) arg)))
            (global-args-defvals (loop for arg in global-args 
                                       collect (if (listp arg) (cadr arg) NIL))))
        
        
        (format outstream" (list ")
        (CTL2_global rules global-args-names global-args-defvals)
        (format outstream" )~%"))
      
      (format outstream" (list ")
      (CTL2_keywords_loop rules synth-args)
      (format outstream" )~%")
      
      (format  outstream ") ~%~%")

      ))
  )


;;; writes in <outstream> the time of the current model
;;; followed by chorma-processed values for a number of <keys>
;;; according to a given <ctl-model> (a list of list-formatted rules)
;;; and using the corresponding <defvals> as default values if needed

(defun CTL2_global (ctl-model keys defvals)
  
  (declare (special outstream my-model my-rank n-fql my-time my-dur my-nev my-fql my-ptl))

  (format outstream " ~a " (+ (offset my-model) my-time))

  (loop for k in keys  
        for def in defvals
       ; do (print k) 
        do (let* ((key k) (ex (assoc key ctl-model)))
             (declare (special key ex))
             (if ex 
               (case (length ex)
                 (2 (if(or(numberp (cadr ex))(symbolp (cadr ex)))(ctl2G-k)
                       (case (caadr ex)
                         (call (ctl2G-function))
                         (omfun (ctl2G-omfun))
                         (otherwise (ctl2G-kb))
                         )))
                 (4 (ctl2G-variable-constant-b))
                 (3 (ctl2G-variable-constant-a))
                 (0 nil)
                 (otherwise (error "UNKNOWN CTL2 (G-)STRUCTURE : ~a~%"ex)))
               (ctl2G-default-value def)))
        ))


;;; writes in <outstream> a sequence of keys and chroma-processed values for <keys>
;;; according to a given <ctl-model> (a list of list-formatted rules)
(defun CTL2_keywords_loop (ctl-model keys)

  (declare (special outstream my-model my-rank n-fql my-time my-dur my-nev my-fql my-ptl))
  
  (loop for k in keys  
        do (let* ((key k) 
                  ;;MODIF SLM mars 2005 
                  (ex (assoc key ctl-model :test #'(lambda (x y) (equal (symbol-name x) (symbol-name y))))))
             
             (declare (special key ex))
            
             (case (length ex)
               (2 (if (or (numberp (cadr ex)) (symbolp (cadr ex)))
                      (ctl2N-k)
                    (cond ;(print (caadr ex))
                     ((equal 'MIN (caadr ex)) (ctl2N-constant-variable))
                     ((equal 'CALL (caadr ex)) (ctl2N-function))
                     (t (ctl2N-kb))
                     )))
               (4 (if (member 'MIN (second ex) :test 'equal)
                      (if (member 'MIN-FUN (fourth ex) :test 'equal)
                          (ctl2N-variable-variable-b)
                        (ctl2N-variable-variable-ab)) 
                    (ctl2N-variable-constant-b)))
               (3 (if (member 'MIN (second ex) :test 'equal)
                      (ctl2N-variable-variable-aa)
                    (ctl2N-variable-constant-a)))
               (0 nil)
               (otherwise (error "UNKNOWN CTL2 STRUCTURE : ~a~%"ex))))
        ))


(defun ctl2N-k ()                        ; CONSTANT
  (declare (special key outstream ex my-nev))
  (format outstream ":~a '(~a )~%" key (eval (eval (cadr ex)))))

(defun ctl2G-k ()                        ; CONSTANT
  (declare (special outstream ex))
  (format outstream " ~a " (eval (eval (cadr ex)))))

(defun ctl2G-default-value (def)                        ; VALEUR PAR DEFAUT
  (declare (special key outstream ex))
  (format outstream " ~a " def))

(defun ctl2N-kb ()                        ; FONCTION EVALUATION RETARDEE
  (declare (special key outstream ex my-nev))
  (let ((f (eval (cadr ex))))
    (if (numberp f)
      (format outstream ":~a '(~a )~%" key f)
      (format outstream ":~a #'~a ~%" key  f))
    ))

(defun ctl2G-kb ()                        ; FONCTION EVALUATION RETARDEE
  (declare (special outstream ex my-nev))
  (let ((f (eval(cadr ex))))
    (if(numberp f)
      (format outstream " ~a " f)
      (format outstream " ~a " (eval f)))))



(defun ctl2N-function ()                 ; HACK POUR TRAITER LES BPF, A REVOIR?
  (declare (special key outstream ex))
  (let ((result (eval (cdadr ex))))
    (if (numberp (car result)) 
      (format outstream ":~a '~a ~%" key result)
      (format outstream ":~a ~a ~%" key result))))
      

#|
(defun ctl2N-function ()                 ; USER-DEFINED-FUNCTION
  (declare (special key outstream ex))
  (format outstream ":~a '~a ~%" key (eval(cdadr ex))))
|#
(defun ctl2G-function ()                 ; USER-DEFINED-FUNCTION
  (declare (special outstream ex))
  (format outstream " ~a ~%"  (eval(cdadr ex)))
  (format t " ~a ~%"  (eval(cdadr ex))))

(defun ctl2G-omfun ()                 ; USER-fun global
  (declare (special outstream ex))
  (format outstream " ~a ~%"  (eval(cdadr ex)))
  (format t " ~s ~%"  (cdadr ex)))

(defun ctl2N-constant-variable ()                 
  (declare (special key outstream ex my-nev))
  (multiple-value-bind (min max fun expt) (ctl2-get-min-max (cadr ex))
    (format outstream ":~a '~a~%" key (lkpr my-nev fun (eval min) (eval max) expt))))

(defun ctl2N-variable-constant-a ()        ; CAS 3A
  (declare (special key outstream ex my-nev n-fql my-rank))
  (let((from (eval(second(second ex))))
       (to (eval (second(third ex))))
       (fun (copy-list ctl2-def-fun1))
       val)
    (resc_fun fun from to 0  n-fql)
    (setf val (apply 'y-val_fun (list fun my-rank)) )
    (format outstream ":~a '(~a)~%" key (eval val))))

(defun ctl2N-variable-constant-b ()        ; CAS 3B
  (declare (special key outstream ex my-nev n-fql my-rank my-model my-time))
  (let((from (eval(second(second ex))))
       (to (eval(second (third ex))))
       (fun (copy-list (eval (second (fourth ex)))))
       (by-time (member 'time (fourth ex)))
       (expnt (if (numberp(car(last(fourth ex))))(car(last(fourth ex))) 0))
       val)
    (if by-time
      (progn(resc_fun fun from to (begin-time my-model)(get-nth-time my-model n-fql))
            (setf val (apply 'y-val_fun (list fun my-time expnt))))
      (progn (resc_fun fun from to 0  n-fql)
             (setf val (apply 'y-val_fun (list fun my-rank expnt)))))
    (format outstream ":~a '(~a)~%" key (eval val))))

(defun ctl2G-variable-constant-a ()        ; CAS 3A
  (declare (special outstream ex my-nev n-fql my-rank))
  (let((from (eval(second(second ex))))
       (to (eval (second(third ex))))
       (fun (copy-list ctl2-def-fun1))
       val)
    (resc_fun fun from to 0  n-fql)
    (setf val (apply 'y-val_fun (list fun my-rank)) )
    (format outstream " ~a " (eval val))))

(defun ctl2G-variable-constant-b ()        ; CAS 3B
  (declare (special outstream ex my-nev n-fql my-rank my-model my-time))
  (let((from (eval(second(second ex))))
       (to (eval(second (third ex))))
       (fun (copy-list (eval (second (fourth ex)))))
       (by-time (member 'time (fourth ex)))
       (expnt (if (numberp(car(last(fourth ex))))(car(last(fourth ex))) 0))
       val)
    (if by-time
      (progn(resc_fun fun from to (begin-time my-model)(get-nth-time my-model n-fql))
            (setf val (apply 'y-val_fun (list fun my-time expnt))))
      (progn (resc_fun fun from to 0  n-fql)
             (setf val (apply 'y-val_fun (list fun my-rank expnt)))))
    (format outstream " ~a "  (eval val))))

(defun ctl2N-variable-variable-aa ()        ; CAS 4A
  (declare (special key outstream ex my-nev my-rank n-fql))
  (multiple-value-bind (from-min from-max from-fun from-expnt) (ctl2-get-min-max (second ex))
    (multiple-value-bind (to-min to-max to-fun to-expnt) (ctl2-get-min-max (third ex))
      (let* ((fun (copy-list ctl2-def-fun1))
             (from-list (lkpr my-nev from-fun from-min from-max from-expnt))
             (to-list (lkpr my-nev to-fun to-min to-max to-expnt)))
        (X-resc_fun fun 0  n-fql)
        (let ((val-list
              (mapcar (lambda (from to)
                        (let ((tmp-fun (copy-list fun)) ) 
                          (Y-resc_fun tmp-fun from to)
                          (y-val_fun tmp-fun my-rank)) )
                      from-list
                      to-list) ))           
        (format outstream ":~a '~a~%" key val-list))))))

(defun ctl2N-variable-variable-ab ()        ; CAS 4A
  (declare (special key outstream ex my-nev my-rank n-fql my-model my-time))
  (multiple-value-bind (from-min from-max from-fun from-expnt) (ctl2-get-min-max (second ex))
    (multiple-value-bind (to-min to-max to-fun to-expnt) (ctl2-get-min-max (third ex))
      (let* ((by-time (member 'time (fourth ex)))
             (fun (copy-list (eval (second (fourth ex)))))
             (expnt (if(numberp(car (last (fourth ex))))
                      (car (last (fourth ex)))
                      0))
             (from-list (lkpr my-nev from-fun (eval from-min) (eval from-max) from-expnt))
             (to-list (lkpr my-nev to-fun  (eval to-min)  (eval to-max) to-expnt)))
        (if by-time
          (progn (X-resc_fun fun (begin-time my-model)(get-nth-time my-model n-fql))
                 (let (( val-list (mapcar (lambda (from to)
                                            (let ((tmp-fun (copy-list fun)) ) 
                                              (Y-resc_fun tmp-fun from to)
                                              (y-val_fun tmp-fun my-time expnt)) )
                                          from-list
                                          to-list) ))
                   (format outstream ":~a ~a~%" key val-list)))
          (progn (X-resc_fun fun 0 n-fql)
                 (let (( val-list (mapcar (lambda (from to)
                                            (let ((tmp-fun (copy-list fun)) ) 
                                              (Y-resc_fun tmp-fun from to)
                                              (y-val_fun tmp-fun my-rank expnt)) )
                                          from-list
                                          to-list)))           
                   (format outstream ":~a '~a~%" key val-list))
                 ))))))


(defun ctl2N-variable-variable-b ()        ; CAS 4B
  (declare (special key outstream ex my-nev my-rank n-fql my-model my-time))
  (multiple-value-bind (from-min from-max from-fun from-expnt) (ctl2-get-min-max (second ex))
    (multiple-value-bind (to-min to-max) (ctl2-get-min-max (third ex))
      (multiple-value-bind (min-fun max-fun expnt-min expnt-max) (ctl2-get-min-max-fun (fourth ex))
        (let ((by-time (member 'time (fourth ex))))
          (if by-time
            (progn (resc_fun min-fun (eval from-min) (eval to-min) (begin-time my-model)(get-nth-time my-model n-fql))
                   (resc_fun max-fun (eval from-max) (eval to-max) (begin-time my-model)(get-nth-time my-model n-fql))
                   (let((val-min-l (apply 'y-val_fun (list min-fun my-time expnt-min)) )
                        (val-max-l (apply 'y-val_fun (list max-fun my-time expnt-max)) ))
                     (format outstream ":~a '~a~%" key (lkpr my-nev from-fun val-min-l val-max-l from-expnt))))
            (progn (resc_fun min-fun (eval from-min) (eval to-min) 0 n-fql)
                   (resc_fun max-fun (eval from-max) (eval to-max) 0 n-fql)
                   (let((val-min-l (apply 'y-val_fun (list min-fun my-rank expnt-min)) )
                        (val-max-l (apply 'y-val_fun (list max-fun my-rank expnt-max)) ))
                     (format outstream ":~a '~a~%" key (lkpr my-nev from-fun val-min-l val-max-l from-expnt))))
            ))))))
      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ctl2-get-min-max (ex)
  (let ((min (eval(second (member 'min ex))))
        (max (eval (second (member 'max ex))))
        (fun (eval (second (member 'fun ex))))
        (expt 0))
    (if(null fun)
      (setf fun (eval 'ctl2-def-fun1))
      (if (> (length (member 'fun ex)) 2)
        (setf expt (third (member 'fun ex)))))
    (if(not(and (numberp (eval min)) (numberp (eval max))))
      (error "UNKNOWN CTL2 STRUCTURE : ~a~%"ex)
      (values-list (list min max (copy-list fun) expt)))))

(defun ctl2-get-min-max-fun (ex) ;FOR CAS 4B
  (let* ((min-l (member 'min-fun ex))
         (min-fun (eval (second min-l)))
         (max-l (member 'max-fun ex))
         (max-fun (eval(second max-l)))
         (min-expt (third min-l))
         (max-expt 0))
    (if(not(numberp min-expt))(setf min-expt 0))
    (if (and (> (length max-l) 2) (numberp (third max-l)))
           (setf max-expt (third max-l)))
      (values-list (list (copy-list min-fun) (copy-list max-fun) min-expt max-expt))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;tests : 
#|
;Cf tests/Part-doc.lisp

(setf test2 (om::mk-array 'om::ad1m 13 '(50)
:durtot 1.522
:e-dels (lambda(x)0.008175375886588126)
:durs '(1.0 0.9416666666666667 0.8833333333333333 0.825 0.7666666666666667 0.7083333333333333 0.65 0.5916666666666667 0.5333333333333334 0.4750000000000001 0.41666666666666663 0.3583333333333334 0.30000000000000004)
:amp (cr::ran-from 0.0 1.0) ;???
:fq '(351.4393285303638 672.218 701.810942322059 988.0353671149467 1054.3424856362344 1320.1187320443166 1405.7238950969859 1756.597109530485 1760.445263444686 1972.8244152210725 2108.7279260581845 2457.191962153317 2464.827398139473) 
:bal '(1.0 0.9166666666666666 0.8333333333333334 0.75 0.6666666666666667 0.5833333333333333 0.5 0.41666666666666663 0.33333333333333337 0.25 0.16666666666666663 0.08333333333333337 0.0)
:jta #'(lambda (x) (cr::ran-from 0.0 1.0))
:tra #'(lambda (x) 0.050098247863535304)
;:trf ,(eval(lambda(x)6.659354169218529))
;:atk ,(eval(lambda(x)0.02252982837619228))
;:dec ,(eval(lambda(x)0.1717755443822137))
;:aenv ,(eval(lambda(x)(s_ve (nth (1- indp) '((fun 0.11771993130586163 0.0 0.1388513159212296 0.007884362680683314 0.16323000728189957 0.01576872536136663 0.19204367139433529 0.023653088042049936 0.22480191163625982 0.030223390275952694 0.2613063718792316 0.038107752956636 0.30126591590158497 0.04599211563731932 0.3442706503894442 0.05387647831800263 0.38855276158937685 0.06176084099868595 0.43186698028524195 0.0683311432325887 0.4701105526734307 0.07621550591327202 0.4984252094042599 0.08409986859395532 0.5154659707832392 0.09198423127463864 0.5235401583277138 0.09986859395532197 0.526865789697371 0.10643889618922468 0.528080340777724 0.114323258869908 0.5304567742721819 0.12220762155059132 0.5361669005949892 0.13009198423127463 0.546323648426584 0.13797634691195795 0.560660551647698 0.14586070959264127 0.5759038740540257 0.15243101182654403 0.5891148903295413 0.16031537450722733 0.6033231640745117 0.16819973718791065 0.6230171474290643 0.17608409986859397 0.6479626904362453 0.1839684625492773 0.6733641643115663 0.19053876478318005 0.6958249547006055 0.19842312746386334 0.7139207420333349 0.20630749014454663 0.7292895657917232 0.21419185282522996 0.7413102413009175 0.22207621550591328 0.7486003054796229 0.22864651773981606 0.7467925899308834))))))
;:jtv ,(eval(lambda(x)0.10167589607029388))
;:vfq ,(eval(lambda(x)0.06867764968178729))
;:fdev ,(eval(lambda(x)6.874006763797266))
;:fenv ,(eval(lambda(x)6.18371069558656))
:pamp (eval(lambda(x)(list 1.2197197937953903 0.5645609333926193 0.51986576417222 0.3443032664049853 0.471122439503023 0.5235254215707046 0.17518707772629866 1.359231305899402 0.4222197236695785 0.14321365955948692 0.09296944149446013 0.015332076858687491 0.0)))
:penv (list (cr::fun->bpf (list 'cr::fun 0.0 0 1 1 0.0 4))(cr::fun->bpf(list 'cr::fun 1.1 0 0.2 1 1.24 4 0.24 5 1.24 6)))
:pdur '( 1.522 1.522 1.522 1.522 1.522 1.522 1.522 1.522 1.522 1.522 1.522 1.522 1.522)
:npart (eval(lambda(x)1))
:dur2 '(1.0 0.925 0.85 0.775 0.7 0.625 0.55 0.475 0.4 0.32499999999999996 0.25 0.17500000000000004 0.09999999999999998)
:amp2 '(0.39296149737315234 0.5615649583575097 0.9187556016660626 0.09271499943479923 0.7802791460505627 0.021362397897274264 0.12339569261396191 0.10695473105661603 0.10720128717525924 0.02941033693747308 0.1694727981062989 0.10502676773407099 0.08985660378539741) 
:ston (eval(lambda(x)0.06418527702649177))
:pflag '(0 ))
)
|#