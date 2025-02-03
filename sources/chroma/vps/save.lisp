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
; File author: M. Stroppa, K. Haddad
;=====================================================
(in-package :om)


(defmethod omNG-save ((self cr::spectrum) &optional (values? nil)) 
  "Cons a Lisp expression that retuns a copy of self when it is evaluated."
  `(when (find-class ',(type-of self) nil)
     (let ((rep (make-instance ',(type-of self))))
       (setf (cr::the-list rep) ',(cr::the-list self))
       (setf (cr::fql rep) ',(cr::fql self))
       (setf (cr::amplitudes rep) ',(cr::amplitudes self))
       (setf (cr::bwl rep) ',(cr::bwl self))
       rep
       )))

(defmethod omNG-copy ((self cr::spectrum))
  "Cons a Lisp expression that return a copy of self when it is valuated."
  `(let ((rep (make-instance ',(type-of self))))
     (setf (cr::the-list rep) ',(cr::the-list self))
     (setf (cr::fql rep) ',(cr::fql self))
     (setf (cr::amplitudes rep) ',(cr::amplitudes self))
     (setf (cr::bwl rep) ',(cr::bwl self))
     rep
     ))


