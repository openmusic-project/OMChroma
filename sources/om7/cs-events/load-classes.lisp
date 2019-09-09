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

(let ((cs-dir (om::om-relative-path '("Csound") nil (om::mypathname (om::find-library "OMChroma"))))) 
    
    ;;; => DEFINE CLASSES FROM .ORC FILES 
    (def-all-cs-classes cs-dir :name "Classes")
    
    ;;; => LOAD ALL LISP FILES IN 'advanced-classes' 
    (mapcar 'load (om-api:om-directory (merge-pathnames "advanced-cs-classes/" *load-pathname*) :type "lisp" :recursive t))

    )

