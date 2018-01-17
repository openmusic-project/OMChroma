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
; (c) Ircam 2000 - 2010
;Authors: C. Agon, M. Stroppa, J. Bresson, S. Lemouton

(in-package :cl-user)
  
;;; requires compile&load defined in OM
(mapc #'(lambda (filename) (compile&load (decode-local-path filename))) 
      '(
        ;SYS
        "init-chroma"
        "globals"
        "utils-cr"
  
        ;PLS
        "pls/utils-pls"
        "pls/all-purp"
        "pls/type"
        "pls/tbl"
        "pls/fun"
        "pls/ctl"
        "pls/raw"
        "pls/ve"
        "pls/wt"
        "pls/wt-frz"
                        
        ;DG
        "dg/utils-dg"
        "dg/df"
        "dg/dv"
        "dg/spectrum"
                        
        ; VPS
        "vps/globals-vps"
        "vps/symbolic-pitch"
        "vps/pitch-conversions"
        "vps/utils-vps"
        "vps/vps"
        "vps/chord"
        "vps/spectrum"
        "vps/user-vps"
        "vps/modif-vps"
        "vps/modif-empty-vps"
        "vps/match"
     
        ; MODELS
        "models/analysis-data"
        "models/f0-data"
        "models/cseq-data"
        "models/additive-data"
        "models/formant-data"
        "models/specenv-data"
        "models/model"
        "models/regions"
        "models/f0-model"
        "models/specenv-model"
        "models/cseq-model"                     
        "models/cseq-regions"
        "models/partials-model"
        "models/partials-regions"
        "models/modif-models"
        "models/modif-regions"
        "models/user-model"
        "models/time-processing"
        "models/utils-model"
  
        ; CTL2
        "ctl2/globals-ctl2"
        "ctl2/ctl2001"
        "ctl2/utils-ctl2001"
        )
      )
     
    






