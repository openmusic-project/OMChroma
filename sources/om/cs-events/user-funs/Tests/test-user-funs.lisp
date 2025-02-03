;******************************************************************
(in-package :om)
;------------------------------------------------------------------

; THIS FILE CONTAINS SOME USEFUL METHODS USED
;    TO TEST THE GENERALIZED USER FUN

;******************************************************************

#|
BEHAVIOUR OF GEN-USER-FUN
;test
a list of functions of 1 argument (component)
outputs
- a component (modified or the same) - the test is successful
- a string                           - the test fails, the string can explains the reason
- a list                             - If the car of the list is a component the test is successful else
                                     - the test fails. All elements of the list will be written in the scr file.
- anything                           - the test fails

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
                                     - the test fail. All elements of the list will be written in the scr file
- anything                           - the test fail

;remove-fields
a list of strings for fields that will not be written in the scr file
|#


;------------------------------------------------------------------
; LOCAL SLOTS / ONLY FOR TESTING
;------------------------------------------------------------------

(defmethod test-dur ((c component))
  "
FOR TESTING PURPOSES ONLY!!
Correct the component if its duration is beyond durtot.
(e-dels[i]+durs[i] > durtot[e]	durs[i] => durtot[e] - e-dels[i])

Return a list whose first element is NOT a component.
All the elements of the list are written, including the component
as a second element.
  "
  (let ((curr-dur (comp-field c "durs"))
        (curr-ed (comp-field c "e-dels"))
        (durtot (durtot (event c))))
    (if (<= (+ curr-ed curr-dur) durtot)
      c
      (list 
         (format () ";---> WARNING / Reduced DUR: old-dur = ~a, new-dur = ~a~% "
              curr-dur (- durtot curr-ed))
         (comp-field c "durs" (- durtot curr-ed))
         ))))


(defmethod test-dur2 ((c component))
  "
FOR TESTING PURPOSES ONLY!!
Correct the component if its duration is beyond durtot.
(e-dels[i]+durs[i] > durtot[e]	durs[i] => durtot[e] - e-dels[i])

Return a symbol. Nothing is written in the score
"
  (let ((curr-dur (comp-field c "durs"))
        (curr-ed (comp-field c "e-dels"))
        (durtot (durtot (event c))))
    (if (<= (+ curr-ed curr-dur) durtot)
      c
      'hello-world
       )))


(defmethod fq-sr1? ((c component))
"
FOR TESTING PURPOSES ONLY
fq[i] > SR/2 => discard component
"
(declare (special cr::sr/2))
   (let ((sr2 (if (find-package 'cr)
                  (cr::get-gbl 'cr::sr/2)
                  22050)))
   (if (> (comp-field c "fq") sr2)
     (list
      (format () ";*****ERROR: FQ > Nyquist: ~a~%;    Component n. ~a discarded~%"
              (comp-field c "fq") (index c))
      (progn (comp-field c "fq" (* -1.0 (comp-field c "fq")))
             "j'ai change")) ; TEST FOR TESTING: RETURN AN ERROR AND THE NEGATIVE FREQUENCY
       c)))

(defmethod fq-sr2? ((c component))
"
FOR TESTING PURPOSES ONLY
fq[i] > SR/2 => discard component
"
(declare (special cr::sr/2))
   (let ((sr2 (if (find-package 'cr)
                  (cr::get-gbl 'cr::sr/2)
                  22050)))
   (if (> (comp-field c "fq") sr2)
     (list
      (comp-field c "fq" (* -1.0 (comp-field c "fq")))
      (format () ";*****ERROR: FQ > Nyquist: ~a~%;    Component n. ~a discarded~%"
              (comp-field c "fq") (index c))
      ) ; TEST FOR TESTING: RETURN THE NEGATIVE FREQUENCY AND AN ERROR
       c)))


(defmethod fq-sr3? ((c component))
"
FOR TESTING PURPOSES ONLY
fq[i] > SR/2 => discard component
"
(declare (special cr::sr/2 index)
         )		; modified in the gen-user-fun  
   (let ((index (index c))
         (sr2 (if (find-package 'cr)
                  (cr::get-gbl 'cr::sr/2)
                  22050)))
   (if (> (comp-field c "fq") sr2)
     'HELLO-WORLD ; TEST FOR TESTING: RETURN A SYMBOL (ANYTHING)
       c)))

#|
not (0 <= bal[i] <= 1)	set to 0 or 1 / DONE WITHIN THE ORCHESTRA FILE
	
atk[i] / dec[i] < 0	set to 0 / DONE WITHIN THE ORCHESTRA FILE
	
not (0 <= jtv[i] <= 1)	set to 0 or 1 / DONE WITHIN THE ORCHESTRA FILE
	
nosc[i] < 0	nosc[i] = 0 / NOT NEEDED
|#

;******************************************************************
