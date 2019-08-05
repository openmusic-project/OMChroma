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


; CR-MODEL connection with Chroma's VPS system
; J. Bresson, 2019 / M. Stroppa

(in-package :cr)


;;;=========================================
;;; CONNECTION WITH THE CR-MODEL OBJECT
;;;=========================================

;;; specialize CR-MODEL frame-maker with eql-specializers
(defmethod make-cr-frame-contents ((type (eql 'cr::fql)) freqs amps)
  (make_fql (list freqs amps)))

(defmethod cr-partials-freqs ((self cr::fql)) (get-vps-freqs self))
(defmethod cr-partials-amps ((self cr::fql)) (get-vps-amps self))


;;;=========================================
;;; TOOLS TO MANIPULATE VPS IN OM
;;;=========================================

;;;===================
;;; VPS CONSTRUCTORS
;;;===================

(defmethod! make-vps ((vps list) &optional reference)
  :indoc '("VPS or any type, except PTL" "Reference, needed only by AIL and ARL")
  :doc "Automatically instanciate a VPS"
  :icon 2002
  (cr::make_vps vps reference))


;(setf aspl (make-vps 'DOd6)) ; error
;(setf aspl (make-vps '(DO4 LAb4 RE5 SOL5 DOd6)))
;(setf aspl1 (make-vps '((DO4 . 33) (LAb4 . -10) (RE5 . 45) (SOL5 . -45) (DOd6 . 35))))
;(setf arpl (make-vps '(DO LAb RE1 SOL1 DOd2)))
;(setf acil (make-vps '(6- 4+ 4 4+)))x
;(setf aail (make-vps '(-6+ -2- 4 7- (3+ 1)) 'LA4))
;(setf afql (make-vps '(261 415 587 784 1108)))
;(setf bfql (make-vps '((261 415 587 784 1108) (0.0 -3.0 -6.0 -10.0 -13.0) (3.0 15.0 34.0 45.0 55.0))))
;(setf cfql (make-vps '((261 415 587 784 1108) (0.0 -3.0 -6.0 -10.0 -13.0) )))

(defmethod! make-spl ((vps list))
  :indoc '("Symbolic Pitch List")
  :doc "Automatically instanciate a SPL"
  :icon 2002
  (cr::make_spl vps))

(defmethod! make-rpl ((vps list))
  :indoc '("Relative Pitch List")
  :doc "Automatically instanciate a RPL"
  :icon 2002
  (cr::make_rpl vps))

(defmethod! make-cil ((vps list))
  :indoc '("Contiguous Interval List")
  :doc "Automatically instanciate a CIL"
  :icon 2002
  (cr::make_cil vps))

(defmethod! make-ail ((vps list) reference)
  :indoc '("Anchored Interval List" "Anchor [symbolic pitch]")
  :doc "Automatically instanciate an AIL"
  :icon 2002
  (cr::make_ail vps reference))

(defmethod! make-fql ((vps list))
  :indoc '("Frequency List")
  :doc "Automatically instanciate a FQL"
  :icon 2002
  (cr::make_fql vps))

(defmethod! make-crl ((vps list))
  :indoc '("Contiguous Ratios List")
  :doc "Automatically instanciate a CRL"
  :icon 2002
  (cr::make_crl vps))

(defmethod! make-arl ((vps list) reference)
  :indoc '("Anchored Ratios List [Spectrum]" "Anchor [Hz]")
  :doc "Automatically instanciate an ARL"
  :icon 2002
  (cr::make_arl vps reference))


;;;=================
;;; GENERATIVE
;;;=================

(defmethod! gen-spsht (f0 np sh st &optional (ran 0.0))
  :icon 656
  :indoc '("f0" "n partials" "shift" "stretch" "detune")
  :initvals '(100 10 0.0 2.0 0.0)
  (cr::spsht_vps f0 np sh st ran))


(defmethod! add-random ((self list) rate)
  :icon 656
  :indoc '("a list of values" "a random rate (0.0->1.0)")
  :initvals '(nil 0.1)
  (mapcar (lambda (x) (cr::ran x (* x rate))) self))



;;;==================
;;; UTILS CR-FUN (FOR TIME PROCESSING)
;;;==================
(defmethod! make-cr-fun ((self list))
  :icon 657
  :initvals '((0 0 1 1))
  (cr::make_fun self))

(defmethod! make-cr-fun ((self om::bpf))
  :icon 657
  (let ((list (loop for x in (om::x-points self)
                    for y in (om::y-points self)
                    append (list y x))))
    (cr::make_fun list)))




;;;=========================================
;;; SELECTORS WITH POSSIBLE VPS CONVERSION
;;;=========================================

(defmethod! spl-vps (vps &rest pars)
  :indoc '("Any VPS" "[Reference, only when needed] [Approximation, in cents]")
  :doc "Return the SPL of a VPS, in the original format, or with a given approximation.
Pars: Approximation [cents], or Reference [pitch, octave or Hz], Approximation [cents]
"
  :icon 2003
  (cond
   ((null pars) (cr::spl_vps vps))
   ((null (cdr pars)) (cr::spl_vps vps (car pars)))
   (t (cr::spl_vps vps (car pars) (cadr pars)))))

;(spl-vps aspl)
;(spl-vps arpl 4)
;(spl-vps aspl1 50)


(defmethod! get-vps-nn (vps)
  :indoc '("Any VPS")
  :doc "Return the Number of Notes of a VPS]
"
  :icon 2003
  (cr::nn_vps vps))

(defmethod! get-vps-gil (vps)
  :indoc '("Any VPS")
  :doc "Return the Global Interval List of a VPS in a symbolic notation
"
  :icon 2003
  (cr::gil_vps vps))

(defmethod! get-vps-gils (vps)
  :indoc '("Any VPS")
  :doc "Return the Global Interval List of a VPS in semitones
"
  :icon 2003
  (cr::gils_vps vps))


(defmethod! get-vps-surf (vps)
  :indoc '("Any VPS")
  :doc "Return the Surface (interval between the first and last note) of a VPS in a symbolic notation
"
  :icon 2003
  (cr::surf_vps vps))

(defmethod! get-vps-surf-s (vps)
  :indoc '("Any VPS")
  :doc "Return the Surface (interval between the first and last note) of a VPS in semitones
"
  :icon 2003
  (cr::surf-s_vps vps))

(defmethod! get-vps-dens (vps)
  :indoc '("Any VPS")
  :doc "Return the Density of a VPS.
A density of 1.0 corresponds to a cluster of semitones.
A density of 2.0 corresponds to a cluster of quarter tones.
"
  :icon 2003
  (cr::dens_vps vps))

(defmethod! get-vps-hom (vps)
  :indoc '("Any VPS")
  :doc "Return the Coeficient of Homogeneity of a VPS as a single symbolic interval.
"
  :icon 2003
  (cr::hom_vps vps))

(defmethod! get-vps-hom-s (vps)
  :indoc '("Any VPS")
  :doc "Return the Coeficient of Homogeneity of a VPS as a single interval in semitones.
"
  :icon 2003
  (cr::hom-s_vps vps))

(defmethod! get-vps-hom-e (vps)
  :indoc '("Any VPS")
  :doc "Return the extended Coeficient of Homogeneity of a VPS (two symbolic intervals).
"
  :icon 2003
  (cr::hom-e_vps vps))

(defmethod! get-vps-hom-es (vps)
  :indoc '("Any VPS")
  :doc "Return the extended Coeficient of Homogeneity of a VPS (two intervals in semitones).
"
  :icon 2003
  (cr::hom-es_vps vps))

(defmethod! get-vps-sd (vps)
  :indoc '("Any VPS")
  :doc "Return the Standard Deviation of a VPS (need at least 4 items in the VPS).
"
  :icon 2003
  (cr::sd_vps vps))

(defmethod! get-vps-cs (vps &optional ss)
  :indoc '("Any VPS" "Optional Stability Space, default: cr::*STABILITY-SPACE*")
  :doc "Return the Coefficient of Stability a VPS
using the default weights (cr::*STABILITY-SPACE*)
or a user-defined space.
"
  :icon 2003
  (cr::cs_vps vps ss))

(defmethod! get-vps-harm (vps)
  :indoc '("SPL, AIL, FQL, ARL")
  :doc "Return the coefficient of Harmonicity of a VPS.
NB: RPL, CIL and CRL must be previously converted.
"
  :icon 2003
  (cr::harm_vps vps))

(defmethod! get-vps-harm-e (vps)
  :indoc '("SPL, AIL, FQL, ARL")
  :doc "Return the coefficient of Harmonicity of a VPS in an extenden format.
NB: RPL, CIL and CRL must be previously converted.
"
  :icon 2003
  (cr::harm-e_vps vps))

(defmethod! get-vps-vf0 (vps)
  :indoc '("SPL, AIL, FQL, ARL")
  :doc "Return the Virtual Fundamental of a VPS.
NB: RPL, CIL and CRL must be previously converted.
"
  :icon 2003
  (cr::vf0_vps vps))

(defmethod! get-vps-max-fq (vps)
  :indoc '("SPL, AIL, FQL, ARL")
  :doc "Return the Maximum Frequence of a VPS.
NB: RPL, CIL and CRL must be previously converted.
"
  :icon 2003
  (cr::max-fq_vps vps))

(defmethod! get-vps-min-fq (vps)
  :indoc '("SPL, AIL, FQL, ARL")
  :doc "Return the Maximum Frequence of a VPS.
NB: RPL, CIL and CRL must be previously converted.
"
  :icon 2003
  (cr::min-fq_vps vps))

(defmethod! get-vps-max-amp (vps)
  :indoc '("FQL")
  :doc "Return the Maximum Amplitude of a FQL.
"
  :icon 2003
  (cr::max-amp_vps vps))

(defmethod! get-vps-min-amp (vps)
  :indoc '("FQL")
  :doc "Return the Minimum Amplitude of a FQL.
"
  :icon 2003
  (cr::min-amp_vps vps))

(defmethod! get-vps-max-bw (vps)
  :indoc '("FQL")
  :doc "Return the Maximum Bandwidth of a FQL.
"
  :icon 2003
  (cr::max-bw_vps vps))

(defmethod! get-vps-min-bw (vps)
  :indoc '("FQL")
  :doc "Return the Minimum Amplitude of a FQL.
"
  :icon 2003
  (cr::min-bw_vps vps))


;;;===============
;;; VPS GET DATA
;;;===============

(defmethod! get-vps-freqs ((self t) &optional reference)
  :icon 2001
  :indoc '("VPS" "Reference, if needed")
  nil)

(defmethod! get-vps-freqs ((self cr::vps) &optional reference)
  (if reference 
      (cr::get-fql self :reference reference)
    (cr::get-fql self)))

(defmethod! get-vps-freqs ((self cr::fql) &optional reference)
  (cr::fql self))

(defmethod! get-vps-freqs ((self cr::rpl) &optional reference)
  (cr::get-fql self :octave reference))

(defmethod! get-vps-freqs ((self om::chord) &optional reference)
  (om::mc->f (om::lmidic self)))


(defmethod! get-vps-amps ((self t))
  :icon 2001
  :indoc '("FQL or PTL")
  nil)

(defmethod! get-vps-amps ((self cr::fql))
  (cr::amplitudes self))

(defmethod! get-vps-amps ((self om::chord))
  (om::om-scale (om::lvel self) 0.0 1.0))

(defmethod! get-vps-maxamp ((x t))
  :icon 659
  (om::list-max (get-vps-amps x)))

(defmethod! get-vps-minamp ((x t))
  :icon 659
  (om::list-max (get-vps-amps x)))


(defmethod! get-vps-freq-ratios ((self cr::vps) ref)
  :icon 659
  (cr::arl_vps self ref))

(defmethod! get-vps-intervals ((self cr::vps) ref)
  :icon 659
  (cr::ail_vps self ref))


(defmethod! get-vps-bws ((self t))
  :icon 2001
  :indoc '("FQL or PTL")
  nil)

(defmethod! get-vps-bws ((self cr::fql))
  (cr::bwl self))


;;;===============================================================
;;; CONVERT OBJECTS
;;;===============================================================

;;; TO FQL
(defmethod om::objfromobjs ((self cr::fql) (type cr::fql))
  (om::clone self))
  
(defmethod om::objfromobjs ((self cr::vps) (type cr::fql))
  (cr::make_fql (cr::fql_vps self)))
  
;;; TO ARL
(defmethod om::objfromobjs ((self cr::vps) (type cr::arl))
  (let ((ref (car (cr::get-fql self))))
    (cr::make_arl (cr::arl_vps self ref) (cr::pch->fq ref))))

(defmethod om::objfromobjs ((self cr::anchored-vps) (type cr::arl))
  (let ((ref (cr::reference self)))
    (cr::make_arl (cr::arl_vps self ref) (cr::pch->fq ref))))

;;; TO CRL
(defmethod om::objfromobjs ((self cr::vps) (type cr::crl))
  (cr::make_crl (cr::crl_vps self)))


;;; TO SPL
(defmethod om::objfromobjs ((self cr::vps) (type cr::spl))
  (cr::make_spl (cr::spl_vps self 100 1)))

;;; TO AIL
(defmethod om::objfromobjs ((self cr::anchored-vps) (type cr::ail))
  (let ((ref (cr::reference self)))
    (cr::make_ail (cr::ail_vps self ref) (cr::fq->pch ref 1))))

(defmethod om::objfromobjs ((self cr::vps) (type cr::ail))
  (let ((ref (car (cr::get-fql self))))
    (cr::make_ail (cr::ail_vps self ref) (cr::fq->pch ref 1))))

(defmethod om::objfromobjs ((self cr::fql) (type cr::ail))
  (let ((ref (car (cr::get-fql self))))
    (cr::make_ail (cr::ail_vps self ref 100) (cr::fq->pch ref 1))))

;;; TO CIL
(defmethod om::objfromobjs ((self cr::vps) (type cr::cil))
  (cr::make_cil (cr::cil_vps self)))

(defmethod om::objfromobjs ((self cr::fql) (type cr::cil))
  (cr::make_cil (cr::cil_vps self 100)))


;;;===============================================================
;;; VPS PROCESSING
;;;===============================================================

(defmethod! vps-high-pass ((self cr::vps) freq)
  :icon 656
  :indoc '("vps" "filter freq (Hz)")
  :initvals '(nil 800.0)
  (cr::high-pass-filter self freq))

;ms_1109
; Unrecognized data, do nothing and pass it over
(defmethod! vps-high-pass (l freq) l)


(defmethod! vps-low-pass ((self cr::vps) freq)
  :icon 656
  :indoc '("vps" "filter freq (Hz)")
  :initvals '(nil 800.0)
  (cr::low-pass-filter self freq))

;ms_1109
; Unrecognized data, do nothing and pass it over
(defmethod! vps-low-pass (l freq) l)


(defmethod! vps-band-pass ((self cr::vps) freqlist)
  :icon 656
  :indoc '("vps" "list of freqs (Hz)")
  :initvals '(nil (400.0 800.0))
  (cr::band-pass-filter self (om::list-min freqlist) (om::list-max freqlist)))

;ms_1109
; Unrecognized data, do nothing and pass it over
(defmethod! vps-band-pass (l freqlist) l)
  

(defmethod! vps-main-partials ((self cr::vps) threshold)
  :icon 656
  :indoc '("vps" "threshold amp")
  :initvals '(nil 0.01)
  (cr::main-partials self :threshold threshold))
  
;ms_1109
; Unrecognized data, do nothing and pass it over
(defmethod! vps-main-partials (l threshold) l)
  

(defmethod! vps-n-main-partials ((self cr::vps) n)
  :icon 656
  :indoc '("vps" "number of partials")
  :initvals '(nil 20)
  (cr::main-partials self :max-nn n))

;ms_1109
; Unrecognized data, do nothing and pass it over
(defmethod! vps-n-main-partials (l n) l)


(defmethod! vps-stretch ((self cr::vps) fact &key (offset 0) (random 0))
  :icon 656
  :indoc '("vps" "stretching factor")
  :initvals '(nil 2)
  (cr::stretch_vps self :stretching fact :offset offset :random random))

;ms_1109
; Unrecognized data, do nothing and pass it over
(defmethod! vps-stretch (l fact &key offset random)
  (declare (ignore random offset))
  l)


(defmethod! vps-transpose ((self cr::vps) ratio)
  :icon 656
  :indoc '("vps" "stretching factor")
  :initvals '(nil 1)
  (cr::transpose self ratio))

;ms_1109
; Unrecognized data, do nothing and pass it over
(defmethod! vps-transpose (l ratio) l)
 

(defmethod! vps-revert ((self cr::vps))
  :icon 656
  :indoc '("symbolic pitch list")
  :initvals '(nil)
  (cr::revert self))

;ms_1109
; Unrecognized data, do nothing and pass it over
(defmethod! vps-revert (l) l)
  

(defmethod! vps-mirror ((self cr::vps))
  :icon 656
  :indoc '("symbolic pitch list")
  :initvals '(nil)
  (cr::mirror self))

;ms_1109
; Unrecognized data, do nothing and pass it over
(defmethod! vps-mirror (l) l)


(defmethod! vps-xpose-begin ((self cr::vps) begin)
  :icon 656
  :indoc '("symbolic pitch list" "begin pitch")
  :initvals '(nil)
  (cr::xpose_vps self begin))

;ms_1109
; Unrecognized data, do nothing and pass it over
(defmethod! vps-xpose-begin (l begin) l)
  

(defmethod! vps-xpose-end ((self cr::vps) end)
  :icon 656
  :indoc '("symbolic pitch list" "end pitch")
  :initvals '(nil)
  (cr::xpose-end_vps self end))

;ms_1109
; Unrecognized data, do nothing and pass it over
(defmethod! vps-xpose-end (l end) l)


;new, ms_110902
(defmethod! vps-approximate-freqs ((self cr::vps) &optional (appx))
  :icon 656
  :indoc '("fql/ptl" "approximation [cents, nil=semitones)]")
  :initvals '(nil nil)
  (cr::change-resolution self :approx appx))

;ms_1109
; Unrecognized data, do nothing and pass it over  
(defmethod! vps-approximate-freqs (l &optional (appx)) l)



;ms_111221, modified
(defmethod! vps-add-randomize ((self list) range xpitvls &optional (remove-dup t))
  :icon 656
  :indoc '("list of vals or number or fql" "range for processing [min/max vals]" "list of processing data" "remove-dup")
  :initvals '(nil nil nil)
  :doc
"Transpose each frequency in <range> by aleatorically selecting one interval in the list.
The transposition is ADDED to the original list, hence it works better with midic.
if sort=t, if the process generates some duplicated notes, remove them.
Ex: (add-randomize '(5000 6000 7000 8000) '(6000 7000) '(-1200 0 1200))
    (add-randomize '(5000 6300 6400 6500 6600 8000) '(6000 7000) '(-100 200 100 -200))
    (add-randomize '(5000 6300 6400 6500 6600 8000) '(6000 7000) '(-100 200 100 -200) :remove-dup t)
WARNING: AT THE MOMENT IT DOES NOT TAKE AMPS and BWS INTO ACCOUNT"
  (let ((result
         (loop for el in self 
               collect (vps-add-randomize el range xpitvls :remove-dup))))
    (if (numberp (car result)) ; if it is not a list of fqls, but a list of numbers
      (if (null remove-dup)
          (om::sort-list result)
        (let ((sorted (om::sort-list result)))
          (om::remove-dup sorted #'= 1)))
      result)))


(defmethod! vps-add-randomize ((self number) range xpitvls &optional (remove-dup t))
  (if (cr::within-p self range) (+ self (om::nth-random xpitvls)) self))

(defmethod! vps-add-randomize ((self cr::fql) range xpitvls &optional (remove-dup t))
  (let ((result
         (loop for freq in (get-vps-freqs self)
               collect (if (cr::within-p freq range) (+ freq (om::nth-random xpitvls)) freq))))
    (if (null remove-dup)
        (cr::make_fql (om::sort-list result))
      (let ((sorted (om::sort-list result)))
        (cr::make_fql (om::remove-dup sorted #'= 1))))))

;(get-vps-freqs (add-randomize (cr::make_fql '(100 200 300 400 500)) '(200 400) '(0.9 1.0 1.1)))
;(mapcar #'get-vps-freqs (add-randomize (list (cr::make_fql '(100 200 300 400 500)) (cr::make_fql '(200 250 300 350 400))) '(200 400) '(0.9 1.0 1.1)))
;(add-randomize 300 '(200 400) '(-100 0 100))
;(add-randomize '(100 200 300 400 500) '(200 400) '(-100 0 100))
;(add-randomize '(100 200 300 400 500) '(200 400) '(-100 0 100) ())


;new, ms_110902
(defmethod! vps-mul-randomize ((self list) range xpitvls &optional (remove-dup))
  :icon 656
  :indoc '("list of vals or number or fql" "range for processing [min/max vals]" "list of processing data" "remove-dup")
  :initvals '(nil nil nil)
  :doc
"Transpose each frequency in <range> by aleatorically selecting one interval [cents] in the list.
The transposition is MULTIPLIED to the original list, hence it works better with freqs.
If remove-dup=t, if the process generates some duplicated notes, remove them.
If remove-dup=number, if the process generates some duplicated notes ± <number>, remove them.
Ex: (mul-randomize '(100 200 300 400 500 600) '(200 500) '(100 0 -100))
WARNING: AT THE MOMENT IT DOES NOT TAKE AMPS and BWS INTO ACCOUNT"
  (let ((result
         (loop for el in self 
               collect (vps-mul-randomize el range xpitvls remove-dup))))
    (if (numberp (car result))
        (let ((sorted (om::sort-list result)))
          (cond
           ((null remove-dup) sorted)
           ((numberp remove-dup) (remove-dup-with-range sorted remove-dup))
           (t (om::remove-dup sorted #'= 1))))
      result)))

;(mul-randomize '(100 200 300 400 500) '(200 400) '(100 0 -100))
;(mul-randomize '(100 200 300 400 500) '(200 400) '(1200 0 -1200))
;(mul-randomize '(100 200 300 400 500) '(200 400) '(1200 0 -1200) t)
;(mul-randomize '(100 200 300 400 500) '(100 500) '(1200 0 -1200) 100)
;(mul-randomize '(100 200 300 400 500) '(200 400) '(0.9 1.0 1.1))


(defmethod! vps-mul-randomize ((self number) range xpitvls &optional (remove-dup))
  (if (within-p self range) (* self (om::nth-random xpitvls)) self))

;(mul-randomize 100.0 '(50 200) '(-100 0 100))
;(mul-randomize 100 '(150 200) '(100 0 100 -100 0 100))

(defmethod! vps-mul-randomize ((self cr::fql) range xpitvls &optional (remove-dup))
  (let ((result
          (loop for freq in (get-vps-freqs self)
                  collect (vps-mul-randomize freq range xpitvls))))
        (let ((sorted (om::sort-list result)))
          (cond
           ((null remove-dup) (cr::make_fql sorted))
           ((numberp remove-dup) (cr::make_fql (remove-dup-with-range sorted remove-dup)))
           (t (cr::make_fql (om::remove-dup sorted #'= 1)))))))

;(get-vps-freqs (mul-randomize (cr::make_fql '(100 200 300 400 500)) '(200 400) '(0.9 1.0 1.1)) )
;(mapcar #'get-vps-freqs (add-randomize (list (cr::make_fql '(100 200 300 400 500)) (cr::make_fql '(200 250 300 350 400))) '(200 400) '(0.9 1.0 1.1)))
;(get-vps-freqs (mul-randomize (cr::make_fql '(100 200 300 400 500)) '(200 400) '(-1200 0 1200) t) )
;(get-vps-freqs (mul-randomize (cr::make_fql '(100 200 300 400 500)) '(200 400) '(-1200 0 1200) 100) )

(defun remove-dup-with-range (list range)
"Remove duplicated values within a given range"
   (let* ((result
           (loop for el1 in (cdr list) for el2 in (om::x->dx list)
                 when (> el2 range)
                 collect el1)))
     (if result
       (if (< (abs (- (car result) (car list))) range)
           (om::x-append (car list) (cdr result))
         (om::x-append (car list) result))
       (list (car list)))))

;(remove-dup-with-range '(0 0.1 0.3 0.31 0.32 0.4 0.5) 0.09)
;(remove-dup-with-range '(0 0.01 0.03 0.031 0.032 0.04 0.05) 0.09)

;;;===============================================================
;;; VPS TO CHORD
;;; STILL TO DO...
;;;===============================================================

