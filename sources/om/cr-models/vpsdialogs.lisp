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


(defmethod set-fql-data ((self vpsPanel) n ffreq famp fbw)
  (let* ((obj (object (editor self)))
         (dec 10)
         (xsize (max 50 (* 10 dec)))
         (mydialog (om-make-window 'om-dialog
                                   :size (om-make-point (max 180 (+ 100 (* 12 dec))) 100)
                                   :window-title ""
                                   :position (om-add-points (om-view-position (window self)) (om-mouse-position self))))
         (freq (om-make-dialog-item 'om-editable-text (om-make-point 30 5) (om-make-point xsize 10)
                                    (format () "~D" ffreq)
                                   ))
         (amp (om-make-dialog-item 'om-editable-text (om-make-point 30 35) (om-make-point xsize 10)
                                   (format () "~D" famp)
                                   ))
         (bw (om-make-dialog-item 'om-editable-text (om-make-point 30 65) (om-make-point xsize 10)
                                   (format () "~D" fbw)
                                   )))
    (om-add-subviews mydialog 
                     (om-make-dialog-item 'om-static-text (om-make-point 5 9) (om-make-point 20 20) "Freq"
                                          :font *om-default-font3b*
                                          :bg-color *om-window-def-color*)
                     (om-make-dialog-item 'om-static-text (om-make-point 5 39)  (om-make-point 20 20) "Amps"
                                          :font *om-default-font3b*
                                          :bg-color *om-window-def-color*)
                     (om-make-dialog-item 'om-static-text (om-make-point 5 69)  (om-make-point 20 20) "Bdw"
                                          :font *om-default-font3b*
                                          :bg-color *om-window-def-color*)
                     freq amp bw
                     (om-make-dialog-item 'om-button (om-make-point (- (w mydialog) 80) 5) (om-make-point 70 20) "Cancel"
                                          :di-action (om-dialog-item-act item 
                                                       (declare (ignore item))
                                                       (om-return-from-modal-dialog mydialog ()))
                                          :default-button nil)
                     (om-make-dialog-item 'om-button (om-make-point (- (w mydialog) 80) 36) (om-make-point 70 20) "OK"
                                          :di-action  (om-dialog-item-act item 
                                                        (declare (ignore item))
                                                        ;ici il faut faire aussi comme en bpf
                                                        ;c-a-d limit < next freq > previous freq
                                                        ; donc faire une methode
                                                        (let ((newf (read-from-string (om-dialog-item-text freq)))
                                                              (newamp (read-from-string (om-dialog-item-text amp)))
                                                              (newbw (read-from-string (om-dialog-item-text bw))))
                                                          (setf (nth n (cr::fql obj)) newf)
                                                          (when (cr::amplitudes obj)
                                                            (setf (nth n (cr::amplitudes obj)) newamp))
                                                          (when (cr::bwl obj)
                                                          (setf (nth n (cr::bwl obj)) newbw))
                                                          (om-return-from-modal-dialog mydialog ())
                                                          ;(om-invalidate-view self t) ;ne marche pas...
                                                          (update-subviews (editor self))
                                                          ))
                                          :default-button t))
                     (om-modal-dialog mydialog)))


;peut-etre deplacer dans vps.lisp
(defmethod om-view-doubleclick-handler ((Self vpspanel) Where) 
  (let ((n (click-in-freq self where))
        (obj (object (editor self))))
    (when n
      (let ((freq (nth n (cr::fql obj)))
            (amp (nth n (cr::amplitudes obj)))
            (bdw (nth n (cr::bwl obj))))
      (set-fql-data self n freq amp bdw)
      (om-invalidate-view self t)))
  (call-next-method)))



