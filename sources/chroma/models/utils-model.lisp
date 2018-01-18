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

(in-package :chroma)

;-----------------------------------------------------------------------------
(defun max- (l)
  "max- ignore nil"
(let(( ll(remove nil l)))
     (when ll (apply #'max ll))))
  
(defun min- (l)
  "min- ignore nil"
(let(( ll(remove nil l)))
     (when ll (apply #'min ll))))

;--------------------
;Fonctions 
;--------------------

(defun markerstodur (l)
   (cond((null (second l))())
       (t( cons (-(second l)(first l))(markerstodur(cdr l))))))
;(markerstodur '(0 1.1 1.3 1.5))
;(om::x->dx '(0 1.1 1.3 1.5))

(defun nombre_de_partiels (subdata)
  (apply #'min (mapcar #'car (mapcar #'car subdata))))
;le nombre de partiel est défini comme le minimum de tous les nombres de partiels entre deux segments

(defun nombre_de_partiels_max (subdata)
  (apply #'max (mapcar #'car (mapcar #'car subdata))))
;le nombre de partiel est défini comme le maximum de tous les nombres de partiels entre deux segments

(defun numero_de_partiels_max (subdata)
  (apply #'max(mapcar (lambda (x) (apply #'max  (mapcar #'car x))) (mapcar #'cdr subdata))))
;le numero de partiel max est défini comme le maximum de tous les index de partiels entre deux segments

(defun get_add_fun (data numero colonne)
  (loop for frame in data
        collect (nth colonne (find numero (cdr frame) :key #'car))))

(defun get_freq_fun (data numero)
  (get_add_fun data numero 1))

(defun get_amp_fun (data numero )
  (get_add_fun data numero 2))

(defun get_bw_fun (data numero )
  (get_add_fun data numero 3))

(defun moyenne (fun)
  (/(apply #'+ fun)(length fun)))

(defun moyenne_ponderee (freq amp)
  (/(apply #'+ (mapcar #'* freq amp))(apply #'+ amp)))

(defun seuillage_add (list seuil)
  (remove-if #'(lambda (y)(< y (db->lin seuil))) list :key #'car))

;(seuillage_add '((1 2 3)(4 5 6)(1 2 3)(100 100 100)) (lin->db 101))
(defun seuillage_add2 (list seuil)
  (if(> (apply #'max (mapcar #'car list))  (db->lin seuil))
    list
    nil))
;(seuillage_add '((1 2 3)(4 5 6)(1 2 3)(100 100 100)) (lin->db 101))
(defun seuillage_cseq (list seuil)
  (remove-if #'(lambda (y)(< y (db->lin seuil))) list :key #'car))


(defun fragmente_add (data debut fin)
  (let ((i_debut  (position debut data :test #'<= :key #'cadar))
        (i_fin (position fin data :test #'<= :key #'cadar)))
    (if (null i_fin)(setf i_fin (1- (length data))))
    (if (eq i_debut i_fin)
      (if (eq i_debut (length data))
        (setf i_fin (1- (length data)) i_debut (- 2 (length data)))
        (setf i_fin (1+ i_debut))))
     (subseq data i_debut i_fin)))

(defun sort-partiels (freqs amps)
  (let((result (sort (mapcar #'list freqs amps) #'> :key #'first)))
       (values (mapcar #'first result)
               (mapcar #'second result))
       ))

(defun sort-partiels-and-funs (freqs amps fun1 fun2)
  (let((result (sort (mapcar #'list freqs amps fun1 fun2) #'> :key #'first)))
       (values(mapcar #'first result)
              (mapcar #'second result)
              (mapcar #'third result)
              (mapcar #'fourth result))
       ))

(defun sort-partiels-and-everything (freqs amps fun1 fun2 fun3 fun4)
  (let((result (sort (mapcar #'list freqs amps fun1 fun2 fun3 fun4) #'> :key #'first)))
       (values(mapcar #'first result)
              (mapcar #'second result)
              (mapcar #'third result)
              (mapcar #'fourth result)
              (mapcar #'fifth result)
              (mapcar #'sixth result)
              )))
