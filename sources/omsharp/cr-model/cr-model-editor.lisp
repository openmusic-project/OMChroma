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
;Authors: C. Agon, M. Stroppa, J. Bresson, S. Lemouton


;;;=======================
;;; CR-MODEL EDITOR
;;;=======================
; J. Bresson, 2019


(in-package :cr)


(defclass cr-model-editor (om::data-track-editor) ())
(defmethod om::object-has-editor ((self cr-model)) t)
(defmethod om::get-editor-class ((self cr-model)) 'cr-model-editor)
(defmethod om::editor-with-timeline ((self cr-model-editor)) nil)

(defmethod om::object-default-edition-params ((self cr-model))
  '((:grid t)
    (:x1 0) (:x2 nil)
    (:y1 0) (:y2 8000)))

(defmethod om::frame-display-modes-for-object ((self cr-model-editor) (object cr-model)) '(:vps))

(defmethod om::make-left-panel-for-object ((editor cr-model-editor) (object cr-model) view)
  (oa::om-make-view 'om::y-ruler-view :size (oa::omp 30 nil)
                    :related-views (list view)
                    :vmin 0 :vmax 22000
                    :y1 (om::editor-get-edit-param editor :y1)
                    :y2 (om::editor-get-edit-param editor :y2)))

(defmethod om::data-frame-text-description ((self cr-frame))
  (list "MODEL VPS"
        (format nil "(~A elements)" (length (cr-partials-freqs (vps self))))))


(defmethod om::draw-data-frame ((frame cr-frame) editor i &optional (active t))
  
  (let* ((panel (om::active-panel editor))
         (x1 (om::x-to-pix panel (om::date frame)))
         (x2 (- (om::x-to-pix panel (+ (om::date frame) (om::sec->ms (dur frame)))) 2))
         (max-amp (max-amp (om::object-value editor))))
    
    (oa::om-draw-line x1 0 x1 (om::h panel) :line 1 :style '(4 4) :color (oa::om-make-color 1. 0.3 0.3))
    (oa::om-draw-line x2 0 x2 (om::h panel) :line 1 :style '(4 4) :color (oa::om-make-color 0.3 0.3 1.0))
    
    (if (zerop max-amp) (setf max-amp 1.0))
    (when (vps frame)
      (loop for f in (cr-partials-freqs (vps frame))
            for a in (cr-partials-freqs (vps frame)) do 
            (let ((y (om::y-to-pix panel f))
                  (col (* (- 1 (/ a max-amp)) .5)))
              (oa::om-draw-line x1 y x2 y :line 2 :color (oa::om-make-color col col col .5))
              ))
      )))
