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

(in-package :chroma)

(defclass regions
  (chroma-model)
  ((region-list :accessor region-list
             :initform ()
             :initarg :region-list
             :documentation "list of regions")))

;;;;;;;;;
;CLASS METHODS

(defmethod startlist ((self regions))
"List of the regions start times"
 (mapcar #'(lambda (x)(+ (offset x) (first(second x)))) (region-list self)))

(defmethod endlist ((self regions))
"List of the regions end times"
 (mapcar #'(lambda (x)(+ (offset x) (second(second x)))) (region-list self)))

(defmethod namelist ((self regions))
"List of the regions names"
 (mapcar #'first (region-list self)))

(defmethod durlist ((self regions))
"List of the regions durations"
 (mapcar #'(lambda (x)(-(second (second x))(first(second x)))) (region-list self)))

(defmethod begin-time ((x regions))
  "get beginning-time of model"
  (let*((debut-fin (mapcar #' second  (region-list x)))
        (debuts (mapcar #' first debut-fin)))
    (+ (offset x) (apply #'min debuts))))

(defmethod end-time ((x regions))
  "get end-time of model"
  (let*((debut-fin (mapcar #' second  (region-list x)))
        (fins (mapcar #' second debut-fin)))
    (+ (offset x) (apply #'max fins))))

(defmethod get-nth-time ((x regions) rank)
  "get the starting time of the nth region"
  (+ (offset x) (first (second (nth rank (region-list x))))))

(defmethod get-nth-dur ((x regions) rank)
  "get nth duration"
  (+ (offset x) (- (second (second (nth rank (region-list x)))) (get-nth-time x rank))))

(defmethod regions-from-durs (at dur name)
  "create a list of regions when giving separate start times, duration and name lists"
  (mapcar #'(lambda (a d n) (list n (list a (+ a d)))) at dur name))

(defmethod regions-from-end-times (at end name)
  "create a list of regions when giving separate start times, duration and name lists"
  (mapcar #'(lambda (a e n) (list n (list a e n))) at end name))


(defmethod get-nth-dur ((x regions) rank)
  "get nth duration"
  (- (second (second (nth rank (region-list x)))) (get-nth-time x rank)))

(defmethod get-fql-from-time ((x regions) time)
  "invalid method for region-models"
  (declare (ignore time))
  nil)

(defmethod get-regions ((x regions) &key (name ""))
  (let ((result nil))
    (loop for region in (region-list x)
          do (if (search name (car region))
               (push region result)))
    (mapcar #'butlast (nreverse result)
            )))

;;;;;;;;;
;PRIVATE METHODS

(defmethod initialize-instance :before ((x regions) &rest initargs &key markers)
  (declare (ignore initargs))
  (make-regions x :markers markers)
  )

(defmethod initialize-instance :after ((x regions) &rest initargs &key markers)
  (declare (ignore initargs markers))
  (check-regions x)
  )

(defmethod make-regions ((x regions) &key markers)
  (let ((res nil)(index 0)(comment nil))
    (loop for (i j) on markers 
          do (setf comment (format () "region~a"(setf index (1+ index))))
          do (cond 
              ((numberp i) (cond
                            ((numberp j)
                             (push (list comment (list i j)() ) res))
                            ((null j) nil)
                            ((listp j)
                             (if (listp (cdr j))
                               (push (list comment (list i (car (second j)))() ) res)
                               (push (list comment (list i (car j))() ) res)))
                            (t nil)))
              ((>  (length i) 1)
               (if (listp (second i))
                 (push (append i '(nil)) res)
                 (push (list comment i ()) res))
               (if(numberp j)
                 (push (list comment (list (second (second i)) j)() ) res))))
          finally (setf (region-list x)(reverse res))
    )))

(defmethod check-regions ((self regions))
  ;1.la fin de chaque region est elle bien avant le début ?
  (loop for region in (region-list self) do
        (let((region (second region))) ; debut-fin
          (if (> (first region)(second region)) (error "region end before start !~a"region))))
  ;2.virer les régions trop courtes
  (setf (region-list self)(remove-if #'(lambda (x) (eq (first (second x))(second (second x)))) (region-list self)))
)              


;(region-list(make-instance 'regions :markers  '(("premiere note" (0 1))(1 1) 1.4 1.5 (2 2) 4 (5 5)(5 6))))
;'((0 1) (2 3) 4)

;'(("premiere note" (0 1))( "xx" (0.5 3))(4 6))
; '((0 1) 1.5 (2 3) 4)))
;'((0 1) (2 3) 4)
                       
;;;;;;;;;
;MISC FUNCTIONS

(defun build-region-list (names starts ends)
"build a region list from name starting_time and end-times lists"
  (mapcar 'list names (mapcar 'list starts ends) (make-list (length names))))
