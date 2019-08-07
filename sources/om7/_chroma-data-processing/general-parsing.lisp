; OMChroma
; High-level control of sound synthesis in OM
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
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307,10 USA.
;
; (c) Ircam 2000 - 2017
;Authors: C. Agon, M. Stroppa, J. Bresson, S. Lemouton


(in-package :cr)


(defun make-remove-fields (list remove-fields)
  
  (if remove-fields
      
      (flet ((remove-index (list position comp)
               (setf (val-list comp)
                     (loop for item in list
                           for i = 0 then (+ i 1)
                           when (not (member i position)) collect item))
               comp))
          
        (let (indexlist)
          (loop for item in list
                collect (if (component-p item)
                            (progn
                              (unless indexlist
                                (setf indexlist (loop for label in remove-fields
                                                      collect (label2index item label))))
                              (remove-index (val-list item) indexlist item))
                          item))))
    list))


(defun filtre-test (comp predicates)
  (let ((comp-list nil)
        (current-comp comp))
    (loop for item in predicates
          while (component-p current-comp) do
          (let ((temp-rep 
                 (cond ((or (symbolp item) (functionp item)) (funcall item current-comp))
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








