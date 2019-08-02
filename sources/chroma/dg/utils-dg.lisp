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
; File author: M. Stroppa
;=====================================================


;******************************************************************
;-------| CTL1 SYSTEM
;-------| This file is: $LLdg/utils.ll
;-------| Version V1.0: Jan 22, 1990
;-------| By Marco Stroppa
;-------| Copyright 1990 IRCAM
;******************************************************************

; THIS FILE CONTAINS SOME USEFUL GENERAL-PURPOSE FUNCTIONS FOR DG SYSTEM

(in-package :cr)

;-----------------------------------------------------------------------------
; AVAILABLE FUNCTIONS
;	advance
;	build-cwt
;	build-fqwt
;	firstn
;	load-db / load-ve
;-----------------------------------------------------------------------------
;firstn
(defun firstn (n l)
  (cond ((null l) ())
        ((<= n 0) ())
        (t (cons (car l) (firstn (1- n) (cdr l))))))
;-----------------------------------------------------------------------------

; (advance n l)
(defun advance (n l)
"Return the list l without the first n els when n = 0, return the same list
"
  (loop while (>= (decf n) 0)
	do (nextl l))
  l)

;-----------------------------------------------------------------------------
; load-db / load-ve
; (load-db name)

(defun load-db (name)
"Load a given data base from $LLdb
The extension of the file to be loaded is : \"name\"_db.lisp"
  (cond
   ((null name) 'done)
   ((symbolp name)
    (let* ((name (format () "~a_db.lisp" name))
           (path (merge-pathnames
                  (make-pathname
                   :directory (append
                               (pathname-directory (getenv 'LLdb))
                               ))
                  name)))
      (format t "Loading ~a~%" path)
      (load path)) )
   ((listp name)
    (load-db (car name))
    (load-db (cdr name)))
   (t (error "ILLEGAL ARGUMENT: ~a" name))))

  
;-----------------------------------------------------------------------------
(defun load-ve (name)
"Load-ve loads a data base of virtual envelopes that is un LLdb/ve
"
  (cond
   ((null name) 'done)
   ((symbolp name)
    (let* ((name (format () "~a_db.lisp" name))
           (path (merge-pathnames
                  (make-pathname
                   :directory (append
                               (pathname-directory (getenv 'LLdb))
                               (list "ve")))
                  name)))
      (format t "Loading ~a~%" path)
      (load path)) )
   ((listp name)
    (load-ve (car name))
    (load-ve (cdr name)))
   (t (error "ILLEGAL ARGUMENT: ~a" name))))


