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

(in-package :cr)

;--------------------
;CLASSE
;--------------------

(defclass chroma-model ()
  ((sfname)
   (sfduration)
   (offset :type float
           :initform 0.0
           :initarg :offset
           :accessor offset
           :reader get-offset
           :writer set-offset
           )
   (markers :type list
            :initform '(0.0)
            :accessor markers
            :initarg :markers
            :documentation "default markers")
   )
  (:documentation "Chroma Model"))

;--------------------
;METHODES
;--------------------
(defmethod durlist ((x chroma-model))
"List of the durations between markers in seconds"
  (markerstodur (markers x)))

(defmethod begin-time ((x chroma-model))
  "get beginning-time of model"
  (+ (offset x) (first (markers x))))

(defmethod end-time ((x chroma-model))
  "get end-time of model"
  (+ (offset x)(nth (nev x) (markers x))))

(defmethod get-nth-time ((x chroma-model) rank)
  "get nth beginning-time of model"
  (+ (offset x)(nth rank (markers x))))

(defmethod get-nth-dur ((x chroma-model) rank)
  "get nth duration"
  (- (get-nth-time x (1+ rank)) (get-nth-time x rank)))

(defmethod nev ((x chroma-model))
"Number of events (1 event = primitive CTL1 event)"
  (length (fql-list x)))

(defmethod total-duration ((x chroma-model))
  "Total duration of the whole model"
  (- (end-time x)(begin-time x)))

(defmethod durtot ((x chroma-model))
  (error "use total-duration instead of DURTOT"))

(defun load-markers-file (&optional file)
  (if(null file)(setf file (choose-file-dialog)))
  (if (probe-file file)
      (load-markers-file-text file)
    (progn 
      (cr-beep) (print "this file does not exist"))))
  
;;SLM 01-2005
(defun load-markers-file-text (&optional file)
  (if(null file)(setf file (choose-file-dialog)))
  (format t "LOADING MARKERS FROM FILE ~a~%" file)
  (with-open-file (in-stream file :direction :input)
    (cddr (read in-stream nil))
    ))

#|
; With OM the markers can also bve loaded from and SDIF file:

 (defun load-markers-file-sdif (&optional file)
   (unless file (setf file (choose-file-dialog)))
   (format t "LOADING MARKERS FROM FILE ~a~%" file)
   (mapcar #'second (om::framesdesc (om::load-sdif-file (namestring file)))))

;(load-markers-file-sdif)
|#


#|
;;; MARKERS FROM AIFF (DEPRECATED) :

(defun get-aiff-markers ()
  (let ((name (choose-file-dialog)))
    (when name
      (peak-regions (load-aiff-markers name)))))


(defun load-aiff-markers (name)
   (let ((result nil)
         (thesound (open name :element-type 'unsigned-byte))
         id pos str)
     (change-class thesound 'cromasound)
     (look-for-chunck thesound "MARK")
     (let((nummarkers (read-short thesound))); nummarkers
        (loop for i from 1 to nummarkers do
             (setf id (read-short thesound)); markerId
             (setf pos (read-long thesound));position
             (let ((numchars  (read-byte thesound)))
               (setf str (aiff-read-string thesound numchars)))
             (push (list id str pos) result))
     (nreverse result))))

(defun get-peak-regions (name)
   (let ((markers (load-aiff-markers name))
         (result nil)
         (thesound (open name :element-type 'unsigned-byte)))
     (change-class thesound 'cromasound)
     (look-for-chunck thesound "APPL")
     (if (string-equal "auFM" (read-ostype thesound))
       (let((numregions (read-short thesound)))
         (loop for i from 1 to numregions do
               (let* ((n (read-short thesound))
                 (numchars (read-byte thesound))
                 (str (aiff-read-string thesound (1- numchars)))
                 (start-id (read-short thesound)); markerId
                 (end-id (read-short thesound)); markerId
                 (timestamp1 (read-long thesound));not used
                 (timestamp2 (read-long thesound));not used
                 (start-time (third (assoc start-id markers)))
                 (end-time (third (assoc end-id markers))))
              (push (list n str  start-time  end-time) result)))))
         (nreverse result)))


(defun peak-regions0 (l)
  (let ((result nil)
        (nummarkers (car l))
        (l (cdr l)))
    (loop for i from 1 to nummarkers by 2 
          do (print i)
          do (let ((mark1 (assoc i l))
                   (mark2 (assoc (1+ i) l)))
               (when (and mark1 mark2)
                 (let ((name (second mark1))
                       (debut (float(/(third mark1) 44100.)))
                       (fin (float(/(third mark2)44100.))))
                   (push (list name (list debut fin)) result)))))
    (nreverse result)))

;New version :
(defun peak-regions (l)
  (let* ((result nil)
         (l (mapcar #'cdr (cdr l)))
         (rl (nreverse (copy-seq l)))
         (regions-names (mapcar #'car l))
         (regions-names (remove-duplicates regions-names :test 'equalp))
         (regions-names (sort regions-names 'string<)))
    (print l)
    (loop for i in regions-names
          do (print i)
          do (let ((mark1 (assoc i l :test #'equalp))
                   (mark2 (assoc i rl :test #'equalp)))
               (when (and mark1 mark2)
                 (let ((debut (float(/(second mark1) 44100.)))
                       (fin (float(/(second mark2)44100.))))
                   (push (list i (list debut fin)) result)))))
    (nreverse result)))

;(get-aiff-markers)

(defmethod aiff-read-string ((s om::sound) n)
  (let((result nil))
    (loop for i from 0 to n 
          do (push (code-char (read-byte s)) result))
    (coerce (nreverse result) 'string)))

|#

#|
Peak writes regions into application specific chunks with the ID
'auFM'.

The data follows the application specific chunk, in the following
format:

=========================================
ApplicationSpecificChunkID (unsigned long)
Chunk Length (long)
App Signature (unsigned long 'auFM')

Total number of regions (short)

Region #1 Name (pascal style, not zero padded at end)
Region #1 Start marker ID (short)
Region #1 End marker ID (short)
Region #1 timestamp1 (long, 0 (not used))
Region #1 timestamp2 (long, 0 (not used))

Region #2 ""
.
.

Region #3 ""
.
.
.

Extra byte 0x00 if total length of chunk is odd.
=========================================

There needs to be two corresponding markers in the AIFF file for the
start
and end IDs for each region.

Let me know if you need any more assistance.

Best regards,
Steve Berkley

|#
