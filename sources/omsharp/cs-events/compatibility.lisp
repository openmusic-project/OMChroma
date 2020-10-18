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


;=======================================
; Compat to load from OM6
;=======================================

(in-package :cr)


(defmethod om::put-precision ((self cs-array) precision) (setf (precision self) precision))
(defmethod (setf om::precision) (val (self cs-array)) (setf (precision self) val))

(defmethod (setf om::action-time) (val (obj cs-evt)) (setf (action-time obj) val))

(import '(text-cs-table gen-cs-table gen01 gen01 cr::gen02 gen-02 gen05 gen-05 gen07 gen-07) :om)

(defmethod (setf om::id) (val (obj cs-table)) (setf (id obj) val))

(defmethod (setf om::stime) (val (obj standard-cs-table)) (setf (stime obj) val))
(defmethod (setf om::size) (val (obj standard-cs-table)) (setf (size obj) val))



