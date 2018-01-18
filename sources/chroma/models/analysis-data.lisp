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


;to deal with any kind of analysis

(in-package :cr)


(defclass analysis-data
  ()
  (   (data :type list
            :initform nil
            :accessor data
            :initarg :data
            :documentation "analysis data")
      (file :type pathname
            :initform nil
            :accessor file
            :initarg :file
            :documentation "analysis file pointer")
   )
  (:documentation "Analysis data in Ram"))

(defclass analysis-stream
  ()
  ((file-accessor))
  (:documentation "Streamed Sdif Analysis data (LATER)"))

