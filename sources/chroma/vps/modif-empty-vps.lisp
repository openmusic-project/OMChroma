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


; To allow modification of fql-lists.
; fql-list = list of {fql | nil}

(in-package :cr)

;---------------------------------------------------------------------------------
(defmethod revert ((x list) &optional start &key (end nil))
(declare (ignore start end))
  (if (null x) nil))

(defmethod transpose ((x list) (interval number))
(declare (ignore interval))
  (if (null x) nil))

(defmethod mirror ((x list) &optional start &key (end nil))
(declare (ignore start end))
  (if (null x) nil))

(defmethod merge_vps ((x list) (y list) &optional (tolerance 1))
(declare (ignore tolerance))
  (if (and (null x)(null y) )nil))

(defmethod remove-octaves ((x list) &key (tolerance 1) (from-bottom nil))
(declare (ignore tolerance from-bottom))
  (if (null x) nil))

(defmethod octave-p_vps ((x list) &optional (seuil 0.05))
(declare (ignore start seuil))
  (if (null x) nil))

(defmethod itvl-mod-p_vps ((x list) itvl &optional (seuil 0.05))
(declare (ignore itvl seuil))
  (if (null x) nil))

(defmethod itvl-p_vps ((x list) itvl-list &optional (seuil 0.05 ))
(declare (ignore itvl-list seuil))
  (if (null x) nil))

(defmethod itvl-cil-p_vps ((x list) itvl &optional (seuil 0.05 ))
(declare (ignore itvl seuil))
  (if (null x) nil))

(defmethod stretch_vps ((x list) &key (reference ())
                           (offset 0) (stretching 2) (random 0))
(declare (ignore reference offset stretching random))
  (if (null x) nil))

(defmethod main-partials ((x list) &key reference diapason
                             (max-nn *MAX-NN*)(threshold nil)&allow-other-keys)
(declare (ignore reference diapason max-nn threshold))
  (if (null x) nil))
