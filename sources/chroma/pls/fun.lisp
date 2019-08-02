;*****************************************************************************
;-------| PULS SYSTEM
;-------| This file is: $LLpls/fun.ll
;-------| Version V1.0: Feb 6, 1990, rev. 2.0 (IRCAM, May-August 2000)
;-------| Rev. 3.0, IRCAM, Feb 2010
;-------| By Marco Stroppa
;-------| Copyright 1990 IRCAM
;*****************************************************************************
(in-package :cr)

; PACKAGE TO DEAL WITH BREAK-POINT FUNCTIONS
;                 ASSOCIATED TYPE NAME: FUN


; DESCRIPTION OF THE DATA STRUCTURE:
;    The structure consists of a breakpoint function defined between two
;       intervals.  Out-of-range values will bear the value of the closest
;	point defined in the function.
;    Note: values are contained into a list of couples, Yn Xn;
;    	   X values should always be different and in ascending order
;	   the type of interpolation used depends on the optional parameter
;	      given to y-val_fun.  Possible values are:
;	      	    () or nothing : first-order interpolation
;		    2  	  	  : second-order interpolation
;		    'EXP k	  : exponential interpolation (e**kt)

; CURRENT IMPLEMENTATION:
;    The structure of type FUN is nothing but a list with type prefix.


; AVAILABLE FUNCTIONS:

;	CONSTRUCTORS: make_fun
;	              sample_fun
;	SELECTORS:    x-beg_fun
;		      y-beg_fun
;		      x-end_fun
;		      y-end_fun
;		      y-val_fun
;		      y-min_fun
;		      y-max_fun
;		      y-list_fun
;		      x-list_fun
;		      reduce_fun
;		      reduce2_fun
;		      interpol_fun
;		      interpol-sample_fun
;	MODIFIERS:    resc_fun
;		      x-resc_fun
;		      y-resc_fun
;	PREDICATES:   is_fun
;	INFO:	      print_fun
;		      short-print_fun



; DESCRIPTION OF THE PACKAGE:

;	NAME:		make_fun  (CONSTRUCTOR)
;	TYPE:		Expr with 2*N arguments
;	CALL:		(make_fun '(y1 x1 ... yN xN))
;	FUNCTION:	define and initialize a structure of type FUN
;		        arguments must be in even number (couples)
;			see above for more info
;	VALUE:		the new structure
;	SOURCE:		$LLpls/fun.ll

(defun make_fun (all)
"
;	NAME:		make_fun  (CONSTRUCTOR)
;	TYPE:		Expr with 2*N arguments
;	CALL:		(make_fun '(y1 x1 ... yN xN))
;	FUNCTION:	define and initialize a structure of type FUN
;		        arguments must be in even number (couples)
;			see above for more info
;	VALUE:		the new structure
;	SOURCE:		$LLpls/fun.ll
"
  (unless (and (>= (length all) 2)
               (evenp (length all)))
    (error "We're sorry, the number of arguments must be even and >= 2   ~a" all))
  (mapc #'(lambda (num)
            (unless (numberp num)
              (error "Arguments are not all numbers, sorry ~a" num)))
        all)
  ; IF THERE ARE ONLY TWO VALUES, CREATE A CONSTANT FUNCTION OF 4 VALUES
  ; (0 1) -> (0 1 0 2)
  (when (= (length all) 2)
    (setf all (list (car all) (cadr all) (car all) (+ (cadr all) 1))))

  ; CHECK ASCENDING ORDER OF X VALUES
  (let ((ll all))
    (loop while (cddr ll)
          do (let ((first-x (cadr ll))
                   (second-x (cadddr ll)) )
               (unless (< first-x second-x)
                 (error "X values are not all in ascending order, sir ~a"
                        (list first-x second-x)))
               (nextl ll)
               (nextl ll))) )
  
  (attach-type 'FUN all) )

;ADDED BY MARCO, 000328 WHEN CREATING DYNAMIC FUNCTIONS

;	NAME:		sample_fun  (CONSTRUCTOR)
;	TYPE:		Expr with 2 arguments
;	CALL:		(sample_fun number-of-points fun)
;	FUNCTION:	sample a structure of type FUN "number-of-points" times
;	VALUE:		the new structure (always of type FUN)
;	SOURCE:		$LLpls/fun.lisp

(defun sample_fun (fun points &optional (exp 0.0))
"
;	NAME:		sample_fun  (CONSTRUCTOR)
;	TYPE:		Expr with 2 arguments
;	CALL:		(sample_fun number-of-points fun)
;	FUNCTION:	sample a structure of type FUN number-of-points times
;                       exp: see y-val_fun
;	VALUE:		the new structure (always of type FUN)
;	SOURCE:		$LLpls/fun.lisp

"
; backward compatibility with the previous order of args
  (let ((fun (if (numberp fun) points fun))
        (points (if (numberp fun) fun points)))
    (let ((sampled-y (lkp points fun exp)))
      (make_fun
       (loop for i from 0 to (1- points)
             append (list (nth i sampled-y) i))))))

;(sample_fun 101 (make_fun '(0 0 200 1)) -0.1)

;	NAME:		x-beg_/x-end_/y-beg_/y-end_fun  (SELECTORS)
;	TYPE:		Expr with 1 argument
;	CALL:		(x-beg_fun fun)
;			(x-end_fun fun)
;			(y-beg_fun fun)
;			(y-end_fun fun)
;	FUNCTION:	return respectively the initial and the final values
;			   for the X and Y axes of a structure of type FUN
;	VALUE:		the values described above
;	SOURCE:		$LLpls/fun.ll

(defun x-beg_fun (fun)
    (pls-check-type 'FUN fun 'x-beg_fun)
    (cadr (contents fun)))

(defun x-end_fun (fun)
    (pls-check-type 'FUN fun 'x-end_fun)
    (car (last (contents fun))))

(defun y-beg_fun (fun)
    (pls-check-type 'FUN fun 'y-beg_fun)
    (car (contents fun)))

(defun y-end_fun (fun)
    (pls-check-type 'FUN fun 'y-end_fun)
    (let ((cts (contents fun)))
       (car (nthcdr (- (length cts) 2) cts))))


;	NAME:		y-min_/y-max_fun  (SELECTORS)
;	TYPE:		Expr with 1 argument
;	CALL:		(y-min_fun fun)
;			(y-max_fun fun)
;	FUNCTION:	return respectively the minimum and maximum value
;			   for the Y axis of a structure of type FUN
;	VALUE:		the values described above
;	SOURCE:		$LLpls/fun.ll

(defun y-min_fun (fun)
    (pls-check-type 'FUN fun 'y-min_fun)
    (let* ((cts (contents fun))
	   (Ymin (car cts)))
	(let ((cts cts))
	    (loop while cts
		do (let ((Ycurr (nextl cts)) )
		    (when (< Ycurr Ymin)
			(setq Ymin Ycurr)) )
		(nextl cts) ))
    Ymin) )

(defun y-max_fun (fun)
    (pls-check-type 'FUN fun 'y-max_fun)
    (let* ((cts (contents fun))
	   (Ymax (car cts)))
	(let ((cts cts))
	    (loop while cts
		do (let ((Ycurr (nextl cts)) )
		    (when (> Ycurr Ymax)
			(setq Ymax Ycurr)) )
		(nextl cts) ))
    Ymax) )

;modified, ms1002, ms_1109 (replaced exp interpolation function)
;	NAME:		y-val_fun  (SELECTOR)
;	TYPE:		Expr with 2 or more arguments
;	CALL:		(y-val_fun fun x-val [optional paramters])
;	FUNCTION:	return the Y value corresponding to the given X value
;			   x-val
;			optional parameters are needed so as to have an
;			   interpolation other than a first-order [CAN BE EXPANDED]
;			correct calls:
;                          (y-val_fun fun X) -> linear interpolation
;                          (y-val_fun fun X 0.5) -> exponential interpolation (log-shaped
;                               going up, exp-shaped coming down if 0.0<exp<1.0, the other way
;                               if exp>1.0; exp=1.0 -> linear interpolation)
;                          (y-val_fun fun X '(exp 0.5)) -> same as above
;                          (y-val_fun fun X '(exp2 0.5)) -> symetric exponential/log curves
;                               (exp-shaped going up and down if exp > 1.0; log-shaped both ways
;                               if 0.0<exp<1.0)
;                          (y-val_fun fun X '(sin)) -> same as (y-val_fun fun X 'sin)
;                             complete sinusoidal shape (from -pi/2 to pi/2)
;                          (y-val_fun fun X '(sin2)) -> same as (y-val_fun fun X 'sin2)
;                             use only 1/2 sin, from 0 to pi/2, slightly log up, exp down
;                          (y-val_fun fun X '(sin3)) -> same as (y-val_fun fun X 'sin3)
;                             use only 1/2 sin, from 0 to pi/2, symetric shapes
;	VALUE:		the value of which above
;	SOURCE:		$LLpls/fun.lisp

;(y-val_fun (make_fun '(200 0 100.0 100)) 50 -0.5)
;(y-val_fun (make_fun '(0 0 100.0 100 0 200)) 150 '(exp 0.5))
;(y-val_fun (make_fun '(0 0 100.0 100 0 200)) 150 '(exp1 -0.5))
;(y-val_fun (make_fun '(0 0 100.0 100 0 200)) 150 '(exp2 1.0))
;(y-val_fun (make_fun '(0 0 100.0 100 0 200)) 150 'sin1)
;(y-val_fun (make_fun '(0 0 100.0 100 0 200)) 150 'sin2)
;(y-val_fun (make_fun '(0 0 100.0 100 0 200)) 150 'sin3)

(defun y-val_fun (fun x-val &optional exp)
"
;	NAME:		y-val_fun  (SELECTOR)
;	TYPE:		Expr with 2 or more arguments
;	CALL:		(y-val_fun fun x-val [optional paramters])
;	FUNCTION:	return the Y value corresponding to the given X value x-val
;				optional parameters are needed so as to have an
;			   	interpolation other than a first-order [CAN BE EXPANDED]
;				correct calls:
;                          (y-val_fun fun X) -> linear interpolation (exp = 0.0)
;                          (y-val_fun fun X 1.0) -> exponential interpolation
;								(exp-shaped going up, log-shaped coming down)
;                          (y-val_fun fun X -1.0) -> exponential interpolation
;								(log-shaped going up, exp-shaped coming down)
;                          (y-val_fun fun X '(exp2 1.0)) -> symetric exp curves (up and down)
;                          (y-val_fun fun X '(exp2 -1.0)) -> symetric log curves (up and down)
;                          (y-val_fun fun X 'sin) -> complete sinusoidal shape (from -pi/2 to pi/2), symetric
;                          (y-val_fun fun X 'sin1) -> same as sin
;                          (y-val_fun fun X 'sin2) -> use only 1/2 sin, from 0 to pi/2, slightly log up, exp down
;                          (y-val_fun fun X 'sin3) -> use only 1/2 sin, from 0 to pi/2, symetric shapes
;	VALUE:		the value of which above
"
    (pls-check-type 'FUN fun 'y-val_fun)
    (cond
       ((<= x-val (x-beg_fun fun))
	(y-beg_fun fun))
       ((>= x-val (x-end_fun fun))
	(y-end_fun fun))
       (t	   		; VALUE IS SOMEWHERE IN THE MIDDLE
	(cond
	   ((null exp)                               ; DEFAULT -> LINEAR
	    (linear-y-val-fun (contents fun) x-val))      ;     (y-val_fun fun x)

; DEFAULT CASE OF EXP INTERPOLATION (JUST A NUMBER <> 0.0 FOLLOWING ARGUMENT X)
;adapted to handle symbols correctly, ms_1109
	   ((numberp exp)                     ; (y-val_fun fun x 1.0) -> EXP
            (if (= exp 0.0)
                (linear-y-val-fun (contents fun) x-val)
              (exp-y-val-fun (contents fun) x-val exp)))

           ((symbolp exp)
            (let ((s (intern (string exp) :cr)))
              (cond
               ((eq s 'sin)                       ; (y-val_fun fun x 'sin)
                (sin-y-val-fun (contents fun) x-val))
               ((eq s 'sin1)                       ; (y-val_fun fun x 'sin1)
                (sin-y-val-fun (contents fun) x-val))
               ((eq s 'sin2)                       ; (y-val_fun fun x 'sin2)
                (sin2-y-val-fun (contents fun) x-val))

;               ((equal exp  "sin2")                       ; (y-val_fun fun x 'sin2)
;            (sin2-y-val-fun (contents fun) x-val))

;           ((eq exp  (intern "SIN2" :om))                       ; (y-val_fun fun x 'sin2)
;            (sin2-y-val-fun (contents fun) x-val))

               ((eq s 'sin3)                       ; (y-val_fun fun x 'sin3)
                (sin3-y-val-fun (contents fun) x-val)))))

           ((listp exp)
            (let ((s (intern (string (car exp)) :cr))
                  (val (cadr exp)))
              (cond
               ((or (eq s 'exp) (eq s 'exp1))
;            (case (car optional)
;                   ('exp (if (= (cadr optional) 0.0)        ; (y-val_fun fun x '(exp 2.0))
                (if (= val 0.0)        ; (y-val_fun fun x '(exp 2.0))
                    (linear-y-val-fun (contents fun) x-val)
                  (exp-y-val-fun (contents fun) x-val val)))

               ((eq s 'exp2)
                  (if (= val 0.0)        ; (y-val_fun fun x '(exp2 0.5))
                      (linear-y-val-fun (contents fun) x-val)
                    (exp2-y-val-fun (contents fun) x-val val)))
               ((or (eq s 'sin) (eq s 'sin1))
                (sin-y-val-fun (contents fun) x-val)) ; (y-val_fun fun x '(sin))
               ((eq s 'sin2)
                (sin2-y-val-fun (contents fun) x-val)) ; (y-val_fun fun x '(sin2))
               ((eq s 'sin3)
                (sin3-y-val-fun (contents fun) x-val)) ; (y-val_fun fun x '(sin3))
               ((eq exp 'binom) (error "Binomial interpolation still to come~%"))
               ((eq exp 'cubic) (error "Cubic interpolation not yet  ~%"))
               (t
                (error "Unavailable optional parameter, sir, review your notes, please!  ~a~%"
                       exp)))))
           (t
	    (error "Illegal optional parameter! Read the doc, ~a!  ~a~%"
	      (get-gbl 'USER) exp))))))

; NO OPTIONAL PARAMETER: LINEAR INTERPOLATION
(defun linear-y-val-fun (cnts-fun Xcurr)
  (let ((cts cnts-fun))
    (catch 'eofwhile
      (loop while (cddr cts)
            do (let ((Xend (cadddr cts)))
                 (if (> Xcurr Xend)
                   (progn (nextl cts) (nextl cts))
                   (throw 'eofwhile
                          (if (= Xcurr Xend)
                            (caddr cts)	; CORRESPONDING Y VAL
                            (let ((Xin (cadr cts))
                                  (Yin (car cts))
                                  (Yend (caddr cts)) )
                              (+  Yin
                                  (* (- Yend Yin)
                                     (/ (- Xcurr Xin)
                                        (- Xend Xin)))))) )
                   ))) )) )


;;; similar to om::number-interpolation
(defun number-interpolation (n1 n2 n curve)
  (+ n1 (* (- n2 n1) (expt n (exp curve)))))


; WITH OPTIONAL PARAMETER: EXP INTERPOLATION (from om::abc-interpolation, now obsolete), asymetric
;ms_1002
; replaced by number-interpolations (in "kernels.lisp")
;ms_1109
(defun exp-y-val-fun (cnts-fun Xcurr alpha)
"Asymetric curves going up/down, exp-log or log-exp"
  (let ((cts cnts-fun))
    (catch 'eofwhile
      (loop while (cddr cts)
            do (let ((Xend (fourth cts)))
                 (if (> Xcurr Xend)
                   (progn (nextl cts) (nextl cts))
                   (throw 'eofwhile
                          (if (= Xcurr Xend)
                            (third cts)	; CORRESPONDING Y VAL
                            (let ((Xin (cadr cts))
                                  (Yin  (car cts))
                                  (Yend  (third cts)))
                             (number-interpolation Yin Yend (/ (- Xcurr Xin) (- Xend Xin)) alpha))))))))))



; WITH OPTIONAL PARAMETER: NEW SYMETRIC INTERPOLATION
;ms, 1002
(defun exp2-y-val-fun (cnts-fun Xcurr alpha)
"Symetric curves going up/down, exp or log"
    (let ((cts cnts-fun))
    (catch 'eofwhile
      (loop while (cddr cts)
            do (let ((Xend (fourth cts)))
                 (if (> Xcurr Xend)
                   (progn (nextl cts) (nextl cts))
                   (throw 'eofwhile
                          (if (= Xcurr Xend)
                            (third cts)	; CORRESPONDING Y VAL
                            (let ((Xin (cadr cts))
                                  (Yin  (car cts))
                                  (Yend  (third cts)))
                              (if (> Yend Yin) ; case as exp-y-val-fun
                                  (number-interpolation Yin Yend (/ (- Xcurr Xin) (- Xend Xin)) alpha)
                                (number-interpolation Yend Yin (- 1 (/ (- Xcurr Xin) (- Xend Xin))) alpha)))))))))))


; OPTIONAL PARAMETER: SINUSOIDAL INTERPOLATIONS
(defun sin-y-val-fun (cnts-fun Xcurr)
"Uses a whole sine, from -pi/2 to pi/2 to compute the interpolation"
  (let ((cts cnts-fun))
    (catch 'eofwhile
      (loop while (cddr cts)
            do (let ((Xend (cadddr cts)))
                 (if (> Xcurr Xend)
                   (progn (nextl cts) (nextl cts))
                   (throw 'eofwhile
                          (if (= Xcurr Xend)
                            (caddr cts)	; CORRESPONDING Y VAL
                            (let* ((Xin (cadr cts))
                                   (Yin (car cts))
                                   (Yend (caddr cts))
                                   (Xmult (/ (- Xcurr Xin)
                                             (- Xend Xin)))
                                   (pi-2 (* pi 0.5)) )
                              (+  Yin
                                  (* (- Yend Yin)
                                     (* (+ (sin (- (* Xmult pi) pi-2)) 1.0) 0.5) )))))))))))
; formula corrected, ms1002


(defun sin2-y-val-fun (cnts-fun Xcurr)
"Uses a 1/2 sine, from 0 to pi/2 to compute the interpolation;
 slightly log going up, exp coming down"
  (let ((cts cnts-fun))
    (catch 'eofwhile
      (loop while (cddr cts)
            do (let ((Xend (cadddr cts)))
                 (if (> Xcurr Xend)
                   (progn (nextl cts) (nextl cts))
                   (throw 'eofwhile
                          (if (= Xcurr Xend)
                            (caddr cts)	; CORRESPONDING Y VAL
                            (let* ((Xin (cadr cts))
                                   (Yin (car cts))
                                   (Yend (caddr cts))
                                   (Xmult (/ (- Xcurr Xin)
                                             (- Xend Xin)))
                                   (pi-2 (* pi 0.5)) )
                              (+  Yin
                                  (* (- Yend Yin)
                                     (sin (* Xmult pi-2)))))
                            )))) )
            )))

;new, ms_1002
(defun sin3-y-val-fun (cnts-fun Xcurr)
"Uses a 1/2 sine, from 0 to pi/2 to compute the interpolation;
 slightly log going up AND coming down = symetric curves"
  (let ((cts cnts-fun))
    (catch 'eofwhile
      (loop while (cddr cts)
            do (let ((Xend (cadddr cts)))
                 (if (> Xcurr Xend)
                   (progn (nextl cts) (nextl cts))
                   (throw 'eofwhile
                          (if (= Xcurr Xend)
                            (caddr cts)	; CORRESPONDING Y VAL
                            (let* ((Xin (cadr cts))
                                   (Yin (car cts))
                                   (Yend (caddr cts))
                                   (Xmult (/ (- Xcurr Xin)
                                             (- Xend Xin)))
                                   (pi-2 (* pi 0.5)) )
;                              (print Xin)
;                              (print Xend)
;                              (print Yin)
;                              (print Yend)
;                              (print Xmult)
                              (if (> Yend Yin) ; going up
                                  (+  Yin
                                      (* (- Yend Yin)
                                         (sin (* Xmult pi-2))))
                                (+  Yin  ; going down
                                    (* (- Yend Yin)
                                       (- 1.0 (sin (+ (* Xmult pi-2) pi-2)))))))
                            )))) )
            )))


; MIXED USEFUL FUNCTIONS FOR THE INTERPOLATION PROCEDURES
;(ascending-x-fun? '(0 0  10 1  5 2  20 3  30 4) 1.5)
;(defun ascending-x-fun? (cnts-fun Xcurr)
;"Return t if the Y-values containing the current X-val are ascending,
;   nil if they are descending"
;  (let* ((both-X (both-X-fun cnts-fun Xcurr))
;         (previous-y (car both-X))
;         (next-y (cdr both-X)))
;    (< 0.0 (- next-y previous-y))
;    ))

;(defun both-X-fun (cnts-fun Xcurr)
;"Return the 2 Y-points between which lies Xcurr.
; X is supposed to lie within the boundaries of the function and not to coincide
;    with an already defined X-point;
; if it is the case, it returns the values which correspond to the previous and the current X-point"
;  (let ((cts cnts-fun))
;    (catch 'eofwhile
;      (loop while (cddr cts)
;            do (let ( ; UNUSED????? (Xprevious (cadr cts))
;                     (Xnext (cadddr cts))
;                     (Yprevious (car cts))
;                     (Ynext (caddr cts)))
;                 (if (> Xcurr Xnext)
;                   (progn (nextl cts) (nextl cts))
;                   (throw 'eofwhile
;                          (cons Yprevious Ynext))
;                   ))) )
;    ) )


;	NAME:		y-list_/x-list_fun  (SELECTORS)
;	TYPE:		Expr with 1 argument
;	CALL:		(y-list_fun fun)
;			(x-list_fun fun)
;	FUNCTION:	return respectively a list containing all the
;			   Y and X values of a structure of type FUN
;	VALUE:		the values described above
;	SOURCE:		$LLpls/fun.ll

(defun y-list_fun (fun)
"Return a list containing all the Y values of a FUN"
    (pls-check-type 'FUN fun 'y-list_fun)
    (let* ((cts (contents fun))
	   ; UNUSED??? (Y-list nil)
           )
	(let ((cts cts))
	    (loop while (cdr cts)
		when (evenp (length cts))
                            collect (car cts)
		do (nextl cts)
                do (nextl cts)))))

(defun x-list_fun (fun)
"Return a list containing all the X values of a FUN"
    (pls-check-type 'FUN fun 'y-list_fun)
    (let* ((cts (contents fun))
	   ; UNUSED???? (Y-list nil)
           )
	(let ((cts cts))
	    (loop while (cdr cts)
		when (evenp (length cts))
                            collect (cadr cts)
		do (nextl cts)
                do (nextl cts)))))


;	NAME:		resc_/x-resc_/y-resc_fun  (MODIFIER)
;	TYPE:		Expr with 3 or 5 arguments
;	CALL:		(resc_fun fun Y-min Y-max X-begin X-end)
;			(x-resc_fun fun X-begin X-end)
;			(y-resc_fun fun Y-min Y-max)
;	FUNCTION:	physically resc all the values (or respectively
;			   only the X or Y values) of the break points of a
;			   function of type FUN
;	VALUE:		the string 'ok
;	SOURCE:		$LLpls/fun.ll

(defun resc_fun (fun Ymin Ymax Xbeg Xend)
"
;	NAME:		resc_/x-resc_/y-resc_fun  (MODIFIER)
;	TYPE:		Expr with 3 or 5 arguments
;	CALL:		(resc_fun fun Y-min Y-max X-begin X-end)
;			(x-resc_fun fun X-begin X-end)
;			(y-resc_fun fun Y-min Y-max)
;	FUNCTION:	physically resc all the values (or respectively
;			   only the X or Y values) of the break points of a
;			   function of type FUN
;	VALUE:		the string 'ok

"
    (x-resc_fun fun Xbeg Xend)
    (y-resc_fun fun Ymin Ymax)
    fun)
							  
(defun x-resc_fun (fun newXbeg newXend)
  (pls-check-type 'FUN fun 'x-resc_fun)
  (unless (and (numberp newXbeg)
               (numberp newXend)
               (> newXend newXbeg))
    (error "No resc. Args are not all numbers in ascending order ~a"
           (list newXbeg newXend)))
  (let* ((oldXbeg (x-beg_fun fun))
         (oldXend (x-end_fun fun))
         (newXend-Xbeg (- newXend newXbeg))
         (oldXend-Xbeg (- oldXend oldXbeg))
         (cts (contents fun)))
    (let ((cts cts))
      (loop while cts
            do (let* ((oldXcurr (cadr cts))
		      (newXcurr (+ newXbeg (* newXend-Xbeg
					      (/ (- oldXcurr oldXbeg)
					 	 oldXend-Xbeg)))) )
                 (rplaca (cdr cts) newXcurr) )
            (nextl cts)
            (nextl cts)) ))
  fun)

(defun y-resc_fun (fun newYmin newYmax &optional (exp 0.0))
  (pls-check-type 'FUN fun 'y-resc_fun)
  (unless (and (numberp newYmin)
               (numberp newYmax))
    (error "No resc. Args are not all numbers ~a" (list newYmin newYmax)))

;ADDED 000525, Marco: allow to rescale constant functions
;                   : if a constant rescaling is detected, return simply
;                       a constant FUN placed in the middle of the rescaling
;                       Ex: (y-resc_fun (make_fun '(10 1 10 2 10 3)) 10 20)
;                           -> (FUN 15 1 15 2 15 3)

  (let* ((oldYmin (y-min_fun fun))
         (oldYmax (y-max_fun fun))
         (newYmax-Ymin (- newYmax newYmin))
         (oldYmax-Ymin (- oldYmax oldYmin))
         (cts (contents fun))
         (ff (make_fun `(,newYmin 0 ,newYmax 1))) )
    (let ((cts cts))
      (loop while cts
        do (let* ((oldYcurr (car cts))
               (newYcurr
                (if (= oldYmin oldYmax)
                  (if (= newYmin newYmax)
                    newYmin

; ADDED, return the value between the two Y vals
                    (y-val_fun ff 0.5 exp))
                  (+ newYmin (* newYmax-Ymin
                                (/ (- oldYcurr oldYmin)
                                   oldYmax-Ymin))) )) )
              (rplaca cts newYcurr)
              (nextl cts)
              (nextl cts))) ))
  fun)


(defun reduce_fun (fun factor)
"
;	NAME:		reduce_fun  (SELECTOR)
;	TYPE:		Expr with 2 arguments
;	CALL:		factor = 1 <-> maximum reduction
;			factor = 0 <-> reduction without loss
;			Reduces <points> by removing all points closer than [<factor> * the amplitude range of the function] to the corresponding interpolated values.
;                       <factor> = 1 means the maximum reduction (all intermediate points are removed)
;                       <factor> = 0 means reduction without loss (only exact matching points are removed)
;	FUNCTION:	returns a reduced fun 
By Serge Lemouton, July 2001
"
  (pls-check-type 'FUN fun 'reduce_fun)
  (let ((before (list (first fun)(second fun)(third fun)))
        (after (cdddr fun))
        (amplitude (-(y-max_fun fun)(y-min_fun fun))))
    (if (= 0. amplitude)
      (setf before (make_fun (cdr before)))
      (loop for i on after by #'cddr 
            while (third i) 
            do (let* ((x_val (second i))
                      (y_val (first i))
                      (temp_fun (make_fun (list (car(last(butlast before)))(car(last before))(third i)(fourth i))))
                      (interpolated_y (y-val_fun temp_fun x_val))
                      (error (/(abs(- interpolated_y y_val)) amplitude) ))
                 (if (>  error factor)
                   (setf before (append before (list y_val x_val)))))
            do (setf after (cddr after))
            finally (setf before (append before (last(butlast fun))(last fun)))))
  before))
  
;(setf testfun (make_fun '(0 0 10 1 15 2 20 3 0 4)))
;(setf testfun (make_fun '(1 0 1 4 1 10 1 20)))
;(setf testfun'(fun 0.5 0 0.5 1 0.5 2) )
;(reduce_fun testfun 0.0)

(defun reduce2_fun (fun npoints &optional (precision 10))
"
;	NAME:		reduce2_fun  (SELECTOR)
;	TYPE:		Expr with 2 or 3 arguments
;	CALL:		npoints : MAXIMUM number of points of the reduced function
;                       precision : amount of recursive steps
;			
;	FUNCTION:	 reduces the number of break points of a
;
			   function of type FUN
By Serge Lemouton, July 2001
"
  (let ((borneMax t)  ; si npoints borne superieure
        (min_factor 0)
        (max_factor 1)
        (curr_factor 0)
        (n (1+(* 2 npoints)))
        result)
    (loop for i from 0 to precision
          do (setf result(reduce_fun fun curr_factor))
          while (not(eq (length result) n))
          do  (if (>(length result) n)
                (setf min_factor curr_factor)
                (setf max_factor curr_factor))
          (setf curr_factor (/ (+ min_factor max_factor) 2)))
    (if (and borneMax (not(eq (length result) n)) ) (setf result  (reduce_fun fun max_factor))) 
    (when (get-gbl 'prnflg)
      (format t "reduce ~D -> ~D (~,2F %)~%"
              (floor (length (X-list_fun fun)))
              (floor(* 0.5 (1- (length result))))(* 100 curr_factor)))
    result
    ))

;(set-gbl 'prnflg t)
;(setf testfun (make_fun '(0.5 0 10 1 15 2 20 3 0 4)))
;(reduce2_fun testfun 3)

(defun interpol_fun (fun1 fun2 factor)
  "
;	NAME:		interpol_fun  (SELECTOR)
;	TYPE:		Expr with 3 args
;	CALL:		factor = 1 -> fun2
;			factor = 0 -> fun1
;	FUNCTION:	returns a fun lying between fun1 and fun2
;	NB: since the x-values of each fun are merged, a function
;	      with a smaller size will keep its last value until the end
By Serge Lemouton, July 2001
  "
  (case  factor
    (0 fun1)
    (1 fun2)
    (otherwise 
     (let ((result nil)
           (x_list (remove-duplicates (sort (append (x-list_fun fun1) (x-list_fun fun2)) #'< ))))
       (loop for x in x_list 
             do (push (+(*(- 1 factor) (y-val_fun fun1 x))
                        (* factor (y-val_fun fun2 x)))
                      result)
             do (push x result))
       (make_fun (nreverse result))))))

;(interpol_fun '(fun 0 0 1 1) '(fun 1 0 0 1 2 2 3 4.5) 0.5)


(defun interpol-sample_fun (fun1 fun2 factor points &optional itp)
  "
;	NAME:		interpol-sample_fun  (SELECTOR)
;	TYPE:		Expr with 4-5 args
;	CALL:		factor = 1 -> fun2
;			factor = 0 -> fun1
;	FUNCTION:	returns an interpolated fun between fun1 and fun2
;	NB: both funs are sampled before being interpolated
; ms_1002
  "
  (case  factor
    (0 fun1)
    (1 fun2)
    (otherwise
     (if (equal fun1 fun2) fun1
       (let* ((result nil)
             (itp2 (ifn itp 0.0 itp))
             (fun1 (sample_fun fun1 points itp2))
             (fun2 (sample_fun fun2 points itp2)))
         (let ((x_list (x-list_fun fun1))
               (y1_list (y-list_fun fun1))
               (y2_list (y-list_fun fun2)))
           (loop for x in x_list
                 for y1 in y1_list
                 for y2 in y2_list
                 do (push (+ (* (- 1 factor) y1)
                             (* factor y2))
                          result)
                 do (push x result)))
         (make_fun (nreverse result)))))))
 
;(interpol-sample_fun (make_fun '(0 0 1 1)) (make_fun '(1 0 0 1)) 0.4 100 :sin1)
;(interpol-sample_fun (make_fun '(0 0 1 1)) (make_fun '(1 0 0 1)) 0.4 100)


;	NAME:		is_fun  (PREDICATE)
;	TYPE:		Expr with 1 argument
;	CALL:		(is_fun fun)
;	FUNCTION:	test whether the argument is a structure of type FUN
;	VALUE:		t or nil according to the test
;	SOURCE:		$LLpls/fun.ll

(defun is_fun (fun)
  (when (is-tagged fun)
    (eq (pls-type fun) 'FUN)))


;	NAME:		print_/short-print_fun  (INFO)
;	TYPE:		Expr with 1 argument
;	CALL:		(print_fun fun)
;			(short-print_fun fun)
;	FUNCTION:	nicely or shortely print a structure of type FUN
;	VALUE:		the string 'done
;	SOURCE:		$LLpls/fun.ll

(defun print_fun (fun)
  (print (format () "Structure of type: ~a" (pls-type fun)))
  (let ((cts (contents fun))
        (counter 0))
    (loop while cts
          do (print (format () "   Y~d = ~a, X~d = ~a"  (incf counter) (nextl cts) counter (nextl cts))))
    'done) )

(defun short-print_fun (fun)
   (print (pls-type fun))
;   (let ((cts (contents fun)) (newline 3) (counter 0) )
   (let ((cts (contents fun)) (counter 0) )
	(loop while cts
		do
                (print (format () "~a / ~a, ~a" (incf counter) (nextl cts) (nextl cts) ))
;		(when (cdr cts) (print (format () "  -  "))
;		(when (= (mod counter newline) 0) (terpri)) )
;	(unless (= counter newline) (terpri))
        )
    'done))
