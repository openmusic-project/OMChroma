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
; (c) Ircam 2000 - 2019
;Authors: M. Stroppa, C. Agon, J. Bresson, S. Lemouton

;;; Initialization of CHROMA variables

(in-package :om)


(cr::set-gbl 'cr::SR om::*audio-sr*)		 ; SAMPLING RATE
(cr::set-gbl 'cr::DIAPASON 'om::*diapason-freq*) ; CURRENT DIAPASON
(cr::set-gbl 'cr::USER 'om::*composer-name*)	 ; USER'S NAME FOR PERSONALIZED MESSAGES
(cr::set-gbl '*chroma-output* om-lisp::*om-stream*) 

