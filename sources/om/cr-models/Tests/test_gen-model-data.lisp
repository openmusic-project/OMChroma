(in-package :om)
;ms_1109
"Process a list of values by sequentially applying all the functions in fun-list to each value,
with the arguments dynamically computed according to the rules specified in arg-list.

The first argument of each function is each element of the list. The other arguments depend on the function.
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

val11...val1N: values of the arguments at the given position; the intermediate values will be computer via interpolation

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
If the test results in eliminating some elements, AND there are correspondng markers, they will also be processed:
   delete = the marker correponding to the deleted element will also be deleted
   firstn = keep only the first n markers in the order of definition (hence discarding the remaining ones)
   () = do nothing, return all the markers

:integeritp
Flag, if t, the arguments computed by interpolation will be rounded to an integer.
"
; tests interpolation
(setf mytest-list (list
                 (arithm-ser 0 100 10)
                 (arithm-ser 0 20 1)
                 (arithm-ser 0 20 2)
                 (arithm-ser -10 10 1)
                 (arithm-ser 0 50 2)
                 (arithm-ser 40 50 1)
                 (arithm-ser 10 20 1)
                 ))

(setf mytest-list1 (list
                 (arithm-ser 5000 7000 100)
                 (arithm-ser 3000 7000 100)
                 (arithm-ser 4000 5000 100)
                 (arithm-ser 6000 9000 100)
                 (arithm-ser 6000 9000 100)
                 (arithm-ser 5000 9000 100)
                 (arithm-ser 3000 9000 100)
                 (arithm-ser 2000 9000 100)
                 (arithm-ser 8000 9000 100)
                 ))

; keep only the elements between min/max
(defun myfilter (l min max)
  (loop for el in l
        when (and (>= el min) (<= el max)) collect el))

; transpose eles by xp
(defun myxp (l xp)
  (loop for el in l do
        collect (+ el xp)))

; test, eliminate null results
(defun mytest (l) (null l))


(setf ctlmidi
      '( (0 (6000 7000))
         (3 (4000 7000))
         (8 (5000 6000))
         ))

(setf ctlmidimrk
      '( (1.0 (6000 7000))
         (3.0 (4000 7000))
         (7.0 (5000 6000))
         ))

(setf ctlmidimrk1
      '( (1.0 (6000 7000))
         (3.0 (11000 17000))
         (7.0 (5000 6000))
         ))

(setf ctlmidi
      '( (0 (6000 7000))
         (3 (4000 7000))
         (8 (5000 6000))
         ))

(setf ctlfirst
      '( (0 (3))
         (2 (10))
         (5 (2))
         ))

(setf ctlxp
      '( (0 (1000))
         (3 (-1000))
         (8 (0))
         ))

(setf ctlxp0
      '( (0 (0))
         (3 (0))
         (8 (0))
         ))

(setf ctlxpmrk
      '( (1.0 (1000))
         (3.0 (-1000))
         (7.0 (0))
         ))

(setf mrks '(0 1 2 3 4 5 6 7 8 9))

;(interpolated-list-markers ctlmidimrk mrks)
;(interpolated-list-markers ctlmidimrk mrks :itpmode 1.0 :intitp t)
;(interpolated-list ctlmidi 9 :intitp t :itpmode 0.0)

;(gen-model-data mytest-list1 '(myfilter firstn) (list ctlmidi ctlfirst))
;(gen-model-data mytest-list1 '(myfilter) (list ctlmidi))

;(gen-model-data mytest-list1 '(myfilter) (list ctlmidi) :interpolmode 0.0 :integeritp t :verbose t)
;(gen-model-data mytest-list1 '(myfilter) (list ctlmidi) :interpolmode 0.0)
;(gen-model-data mytest-list1 '(myfilter) (list ctlmidi) :interpolmode 0.0 :integeritp t)
;(gen-model-data mytest-list1 '(myfilter) (list ctlmidi) :interpolmode 0.0 :integeritp t :test 'mytest)
;(gen-model-data mytest-list1 '(myfilter myxp) (list ctlmidi ctlxp0) :interpolmode '(0.0 nil) :integeritp t)
;(gen-model-data mytest-list1 '(myfilter myxp) (list ctlmidi ctlxp) :interpolmode '(0.0 nil) :integeritp t)
;(gen-model-data mytest-list1 '(myfilter myxp) (list ctlmidi ctlxp) :interpolmode '(0.0) :integeritp t)

;(gen-model-data mytest-list1 '(firstn) (list ctlfirst) :interpolmode () :integeritp t)
;(gen-model-data mytest-list1 '(firstn) (list ctlfirst) :interpolmode 0.0 :integeritp t) ;:verbose t)

;(gen-model-data mytest-list1 '(myfilter) (list ctlmidimrk) :interpolmode 0.0 :timemode 'abs :markers mrks :integeritp t :verbose t)
;(gen-model-data mytest-list1 '(myfilter) (list ctlmidimrk) :interpolmode 0.0 :timemode 'abs :markers mrks :integeritp t)
;(gen-model-data mytest-list1 '(myfilter) (list ctlmidimrk1) :interpolmode 0.0 :timemode 'abs :markers mrks :integeritp t)
;(gen-model-data mytest-list1 '(myfilter) (list ctlmidimrk1) :interpolmode 0.0 :timemode 'abs :markers mrks :integeritp t :test 'mytest)
;(gen-model-data mytest-list1 '(myfilter) (list ctlmidimrk1) :interpolmode 0.0 :timemode 'abs :markers mrks :integeritp t :test 'mytest :markermode 'firstn)
;(gen-model-data mytest-list1 '(myfilter) (list ctlmidimrk) :interpolmode 0.0 :timemode 'abs :markers mrks :integeritp t :test 'mytest :markermode ())

