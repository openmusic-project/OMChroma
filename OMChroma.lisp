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

;;;================================
;;; Lib loader for OM 6
;;;================================

(in-package :om)


(load (merge-pathnames "sources/package" *load-pathname*))
(load (merge-pathnames "sources/chroma/load" *load-pathname*))

(mapc 

 #'(lambda (file)
     (compile&load (merge-pathnames file *load-pathname*)))
 
 '("sources/om6/chroma-init"
   "sources/om6/chroma-fun"

   "sources/om6/cs-events/general-parsing"

   "sources/om6/cs-events/csound/cs-utils"
   "sources/om6/cs-events/csound/cs-tables"
   "sources/om6/cs-events/csound/csound-evt"
   "sources/om6/cs-events/csound/csound-tools"
   "sources/om6/cs-events/csound/csound-parsing"

   "sources/om6/cs-events/user-funs/user-funs"
   "sources/om6/cs-events/user-funs/methods"

   "sources/om6/cs-events/chromaspat"

   "sources/om6/chroma-reference"
   "sources/om6/doc-chroma"

   "sources/om6/cr-models/vps-tools"
   "sources/om6/cr-models/vpseditor"
   "sources/om6/cr-models/cr-model"
   "sources/om6/cr-models/processing"
   "sources/om6/cr-models/expand-model"
   "sources/om6/cr-models/ctl"
   ))


(om::fill-library '((nil nil nil (synthesize chroma-prisma) nil)
                    ("Csound" 
                     (("Csound Tables" nil (cs-table gen-cs-table gen01 gen02 gen-02 gen05 gen07 gen-07) nil nil)
                      ("Basic Classes" nil nil nil nil)
                      ("Advanced Classes" nil nil nil nil)
                      ("Multi Channels" nil nil nil nil))
                     nil nil nil)
                    ("Vertical Pitch Structures" 
                     (("Inspect" nil nil (get-vps-freqs get-vps-freq-ratios get-vps-intervals 
                                                        get-vps-amps get-vps-maxamp get-vps-minamp
                                                        ;;; Lisp functions:
                                                        cr::get-homogeneity cr::get-harmonicity cr::get-surface 
                                                        cr::get-density cr::get-sd cr::get-cs cr::get-virt-fund cr::match) nil)
                      ("Processing" nil nil (low-pass-filter high-pass-filter band-pass-filter
                                                             main-partials n-main-partials stretch-vps pitch-transpose
                                                             xpose-begin xpose-end
                                                             revert-vps mirror-vps) nil)
                      ("Conversions" nil nil (pch->fq fq->pch fq->ratio) nil))
                     nil
                     nil nil)
                    ("Models" 
                     (("Inspect" nil nil (model-max-amp model-max-freq model-min-freq model-nb-evts) nil)
                      ("Tools" nil nil (make-cr-fun time-map-fun add-random) nil))
                     (cr-model) (model-data expand-model) nil)
                    )
                  (find-library "OMChroma"))


(defun load-chroma-classes (dir &optional pack)
  (loop for item in (om-directory dir :files t :directories t) do
        (if (directoryp item) 
            (let ((thepackage 
                   (or (and pack (find (car (last (pathname-directory item)))
                                       (subpackages pack) :key 'name :test 'string-equal))
                       pack)))
               (load-chroma-classes item thepackage))    
        (when (string-equal (pathname-type item) "lisp")
          (load item)
          (let ((classname (intern (string-upcase (pathname-name item)))))
            (when (and pack (find-class classname nil))
              (addclass2pack classname pack)))))))


(let* ((chromapack (or *current-lib* (find-library "OMChroma")))
       (cspack (find "Csound" (subpackages chromapack) :key 'name :test 'string-equal))
       (basicpack (find "Basic Classes" (subpackages cspack) :key 'name :test 'string-equal))
       (advpack (find "Advanced Classes" (subpackages cspack) :key 'name :test 'string-equal))
       (spacepack (find "Multi Channels" (subpackages cspack) :key 'name :test 'string-equal)))

  
  (setq *cs-orc-folder* (om-relative-path '("Csound") nil))
  
  (load-chroma-classes (om-relative-path '("sources" "om6" "cs-events" "csound" "classes" "Basic") nil) basicpack)
  (load-chroma-classes (om-relative-path '("sources" "om6" "cs-events" "csound" "classes" "Advanced") nil) advpack)
  (load-chroma-classes (om-relative-path '("sources" "om6" "cs-events" "csound" "classes" "Panning") nil) spacepack)
  )


(unless (fboundp 'om::set-lib-release) (defmethod om::set-lib-release (version &optional lib) nil))

(om::set-lib-release 5.02 (find-library "OMChroma"))

;(cl-user::clean-sources (make-pathname :directory (append (pathname-directory *load-pathname*) '("sources"))))


(print "
 ==============================
 OMChroma
 ==============================
 High-level control of sound synthesis in OM
 (c) Ircam 2000-2018
 C. Agon, M. Stroppa, J. Bresson, S. Lemouton
 ==============================
")


; (om::omchroma-reference)
