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

(in-package :om)

(require-library "om-csound")

(in-package :cr)


;;; here data can still be of any type but tehere is no more reference to the containing array
(defmethod format-csound-line (id offset data field-precision-list)
  (let ((str (format nil "i~D " id)))
    (setf (car data) (+ (car data) offset))
    (loop for item in data for i = 0 then (+ i 1) do
          (setf str 
                (concatenate 
                 'string str 
                 (cond ((integerp item) 
                        (format nil "~D " item))
                       ((numberp item) 
                        (format nil (format nil "~~,~DF " (nth i field-precision-list)) item))
                       ((or (subtypep (type-of item) 'cs-table)
                            (subtypep (type-of item) 'multi-cs-table))
                        (format nil "~D " (get-table-id item)))
                       ((pathnamep item) 
                        (format nil "~s " (namestring item)))
                       ((subtypep (type-of item) 'om::sound)
                        (format nil "~s " (namestring (om::get-sound-file item))))
                       (t (format nil "~A " item))))))
    str))



(defun normalize-output (path normalize format res)
  (let ((s (make-instance 'om::sound :n-samples :n-channels))
        (level (if (numberp normalize) normalize 0)))
    (setf (om::file-pathname s) path)
    (om::set-sound-info s path)
    (om::om-print-format "Normalizing output to ~D" (list level))
    (om::save-sound 
     (om:sound-normalize s :level level)
     path :format format :resolution res)))
  

;;;===============================
;;; SYNTHESIZE CS-ARRAY
;;;===============================

(defmethod cs-synthesize ((self cs-array) &key (name "my-synt") (run t) 
                          format resolution 
                          (normalize nil normalize-supplied-p)
                          inits tables
                          sr kr)
  
  (declare (ignore inits sr kr)) ;;; this is already in the csound instrument

  (unwind-protect 
  
      (let* ((out-format (string (or format (om::get-pref-value :audio :format))))
             (out-res resolution) ;;; no resolution will keep the output in float format (or resolution (om::get-pref-value :audio :resolution))
             (out-normalize (if normalize-supplied-p normalize (om::get-pref-value :audio :normalize)))
             
             (path-aiff (if (equal :rt name) name
                          (if (pathnamep name) name 
                            (om::outfile name :type out-format))))
             (cs-basename (if (equal :rt name) "cs_temp" (pathname-name path-aiff)))
             (path-orc (if (pathnamep (source-code self)) (source-code self)
                         (om::handle-new-file-exists (om::tmpfile cs-basename :type "orc"))))
             (path-sco (om::handle-new-file-exists (om::tmpfile cs-basename :type "sco")))
             (global-tables (csound-load-tables tables)))
        
        (unless (source-code self) (error "CS-ARRAY has no source code!"))

         ;;; Write the .ORC (if needed)
        (unless (pathnamep (source-code self))
          
          (with-open-file (f path-orc :direction :output)
            (write-string (source-code self) f))
          
          (when run (om::add-tmp-file path-orc))
          )

        ;;; Write the .SCo from tables and matrix data
        (with-open-file (out path-sco :direction :output)
      
         (format out "; Score header defined in orchestra:~%")
         ;;;(loop for table in (orc-gens self) do (write-line table out))
         (loop for table in (cs-description-gens (cs-instr self)) do (write-line table out))
         
         
          (format out "~%; global tables:~%")
          (loop for item in (om::list! global-tables) do (format out "~A~%" (cs-table-string item)))
      
          (init-array-tables-id-counter)
      
          (let ((local-tables (get-array-tables self)))
            
            (when local-tables
              (format out "~%;------ local tables in array ------~%~%")
                (loop for table in local-tables do 
                      (prepare-table table)
                      (write-line (cs-table-string table) out)
                      )
                )
            
            (format out "~%;------ lines for event ------~%~%")
            (let ((id (instr-num self))
                  (field-precision-list (mapcar 'om::array-field-decimals (om::data self))))
              
              (loop for i from 0 to (- (om::elts self) 1) collect
                    (let ((element (loop for field in (om::data self)
                                         for pn from 2 to (num-csound-fields self) ;;; prevents collecting the additional control fields
                                         collect (nth i (om::array-field-data field)))))
                      (write-line 
                       (format-csound-line id (om::ms->sec (om::onset self)) element field-precision-list) 
                       out)
                      )
                    ))

            (reset-temp-table-id local-tables)
            ))

        (when run (om::add-tmp-file path-sco))
        
        (if run   
            
            (let ((out-final (om::csound-synth path-orc path-sco :out path-aiff :format out-format)))
              (if out-normalize
                  (normalize-output out-final out-normalize out-format out-res)
                out-final))
                  
          (probe-file path-sco))
        )
    
  ;;; unwind-protect cleanup form(s)
  (om::maybe-clean-tmp-files))
  )


;;;===============================
;;; SYNTHESIZE CS-EVTs
;;;===============================

(defun get-unique-instrument-list (list)
  (remove-duplicates list :test 'equal :key 'type-of))

(defmethod get-channels-in-instrument ((self cs-evt))
  (when (cs-instr self)
    (cs-description-channels (cs-instr self))))

(defmethod get-inits-in-instrument ((self cs-evt))
  (when (cs-instr self)
    (append 
     (loop for glob in (cs-description-global-vars (cs-instr self)) collect
           (make-instance 'CS-INIT :init-type :global :name (car glob) :value (cadr glob)))
     (loop for mac in (cs-description-macros (cs-instr self)) collect
           (make-instance 'CS-INIT :init-type :macro :name (car mac) :params (cadr mac) :value (caddr mac)))
     (loop for oc in (cs-description-opcodes (cs-instr self)) collect
           (make-instance 'CS-INIT :init-type :opcode :name (car oc) :params (cadr oc) :value (caddr oc)))
     )))

;;; Dealing with nchannels:
;;; Priority order = 1) cs-synthesis keyword, 2) max of provided CS-INITs, 3) value found in Csound instruments  

(defun get-num-channels-from-inits (inits)
  (let ((chan-inits (get-inits-of-type (om::list! inits) :channels)))
    (and chan-inits (reduce 'max (mapcar 'value chan-inits) :initial-value 1))))

(defun get-num-channels-from-inst-list (list)
  (let ((chan (get-channels-in-instrument (car list)))
        (rep t))
  (loop for item in list 
        when rep do
        (setf rep (= chan (get-channels-in-instrument item))))
  (if rep chan)))

;;; THE GENRAL CASE WITH CS-SYNTHESIZE IS TO SYNTHESIZE A LIST OF 1 OR MORE EVENTS,
;;; POSSIBLY FOR DIFFERENT SUBCLASSES OF CS-EVT

;;; Note on user-fun(s):
;;; - Events and their contents are DEEP-COPIED because the parsing might modify them and be applied in sequence (if several functions)
;;; - Components operations must therefore modify the event in place in order to be propagated
;;; - (also for tables to be collected correctly)
(defmethod cs-synthesize ((self list) &key (name "my-synt") (run t) 
                          format resolution 
                          (normalize nil normalize-supplied-p)
                          inits tables
                          sr kr)
  
  (if (not (om::list-subtypep self '(cs-evt cs-string)))
      (om::om-beep-msg "CS-SYNTHESIZE requires only subtypes of CR:CS-EVT")
   
    (unwind-protect 
  
        (let* ((out-format (string (or format (om::get-pref-value :audio :format))))
               (out-res resolution) ;;; no resolution will keep the output in float format (or resolution (om::get-pref-value :audio :resolution))
               (out-normalize (if normalize-supplied-p normalize (om::get-pref-value :audio :normalize)))
               (format-str (string-downcase out-format))
               (path-aiff (if (equal :rt name) name
                            (if (pathnamep name) name 
                              (om::outfile name :type format-str))))

               (cs-basename (if (equal :rt name) "cs_temp" (pathname-name path-aiff)))
               (path-csd (om::handle-new-file-exists (om::tmpfile cs-basename :type "csd")))
               (csound-events (om::om-copy self))
               (real-csound-events (remove 'cs-string csound-events :key 'type-of))
               (instlist (copy-list (get-unique-instrument-list real-csound-events))) ;;; 1 instance (copied) of each different class
               (global-tables (csound-load-tables tables))
               
               (synth-sr (or sr (om::get-pref-value :audio :samplerate) 44100))
               (synth-kr (or kr synth-sr))
               
               (all-inits (prepare-inits (om::flat (om::list! inits)) real-csound-events))
               
               (nch (or (get-num-channels-from-inits all-inits)
                        (get-num-channels-from-inst-list instlist))))
        
        
          ;;; assign temporary IDs to the classes through class-allocated instr-num
          (loop for item in instlist
                for i = 1 then (+ i 1) do
                (setf (instr-num item) i))
         
          (assert nch nil "Error: All instruments must have the same number of channels !")
          
          ;; => GO
          (with-open-file (out path-csd :direction :output)
                
            (format out "<CsoundSynthesizer>~%")

            (format out "<CsOptions>~%")
                
            (format out "~A ~A ~A~%" 
                    (om::get-pref-value :externals :csound-flags)
                    (concatenate 'string "--format=" format-str)
                    (case out-res (16 "-s") (24 "-3") (32 "-l") (otherwise ""))
                    )
                
            (format out "</CsOptions>~%")

            ;;;===========================================
            ;;; Write the .ORC from all different classes
            ;;;===========================================
             
            (format out "<CsInstruments>~%")

            (format out "; CSOUND ORCHESTRA GENERATED BY OMCHROMA~%")
            (format out "; ~A~%" (om::om-get-date))
               
            (format out "~%; == ORC HEADER: ==~%~%")
            (format out "sr = ~D~%" synth-sr)
            (format out "kr = ~D~%" synth-kr)
            (format out "ksmps = ~D~%" (/ synth-sr synth-kr))
            (format out "nchnls = ~D~%" nch)
                
            (format out "~%" nch)

            (write-cs-inits out all-inits)

            (format out "~%; == INSTRUMENTS: ==~%~%")
            (loop for inst in instlist 
                  do
                  (format out ";====================~%")
                  (format out "instr ~D~%" (instr-num inst))
                  (loop for orc-line in (cs-description-body-lines (cs-instr inst)) 
                        do (write-line orc-line out))
                  (format out "endin~%~%"))
               
            (format out "</CsInstruments>~%")

            ;;;===========================================
            ;;; Write the .SCO from tables and matrix data
            ;;;===========================================
                
            (format out "<CsScore>~%")
                
            (format out "; CSOUND SCORE GENERATED BY OMCHROMA~%")
            (format out "; ~A~%~%" (om::om-get-date))
               
            (format out "; Score header defined in orchestra:~%")
            (loop for inst in instlist do
                  (format out "; - from ~A:~%" (type-of inst))
                  (loop for table in (cs-description-gens (cs-instr inst)) do 
                        (write-line table out)))
      
            (format out "~%; global tables:~%")
            (loop for item in (om::list! global-tables) do (format out "~A~%" (cs-table-string item)))
      
            (init-array-tables-id-counter)
                
            (loop for evt in csound-events
                  for e = 1 then (+ e 1) do
                  
                  (if (typep evt 'cs-string)
                      (format out "~A~%" (str evt))
                  
                 (progn 
                   
                   (setf (om::attached-components evt) nil)
   
                  (dotimes (i (om::elts evt)) 
                    (om::get-comp evt i) ;;; this will "attach" a component to the array 
                    (when (user-fun evt)
                      (loop for fun in (om::list! (user-fun evt)) do (funcall fun evt i)))
                    )
                      
                  (let ((local-tables (get-array-tables evt)))
                        
                    (format out "~%;------ EVENT ~D ------" e)
                      
                    (when local-tables
                      (format out "~%;------ local tables in array ------~%")
                      (loop for table in local-tables do 
                            (prepare-table table)
                            (write-line (cs-table-string table) out)))
                        
                    ;;; tables have been assigned an ID and can still be referenced inside the array components
                    (format out "~%;------ lines for event ------~%")
                        
                    (let ((offset (+ (om::ms->sec (om::onset evt)) (action-time evt)))
                          (id (instr-num evt))
                          (n-fields-minus-1 (1- (num-csound-fields evt)))
                          (field-precision-list (mapcar 'om::array-field-decimals (om::data evt))))
                          
                      (loop for c in (om::attached-components evt) do
                            (write-line 
                             (format-csound-line id offset (om::first-n (om::component-vals c) n-fields-minus-1) field-precision-list)
                             out)
                            )
                      )
                        
                    (reset-temp-table-id local-tables)
                    (setf (om::attached-components evt) nil)
                    ))
                 ))
                
            (format out "</CsScore>~%")
                
            (format out "</CsoundSynthesizer>~%")

            )

          (when run (om::add-tmp-file path-csd))
              
          ;;;===========================================
          ;;; RUN (or not...)
          ;;;===========================================
              
          (if run   
              
              (let ((out-final (om::csd-synth path-csd :out path-aiff)))
                (if out-normalize
                    (normalize-output out-final out-normalize out-format out-res)
                  out-final))
            
            (probe-file path-csd))
          
          )
    
      ;;; unwind-protect cleanup form(s)
      (om::maybe-clean-tmp-files))
    ))



(defmethod cs-synthesize ((self cs-evt) &key (name "my-synt") (run t) 
                          format resolution 
                          (normalize nil normalize-supplied-p) 
                          inits tables sr kr)

  (cs-synthesize (list self) :name name :run run 
                 :format format 
                 :resolution resolution 
                 :normalize (if normalize-supplied-p normalize (om::get-pref-value :audio :normalize))
                 :inits inits  :tables tables 
                 :sr sr :kr kr))





;;; .. to make it work with the generic OM 'synthesize' method
(defmethod om::synthesize-method ((self cs-evt)) 'cs-synthesize)


;;;===============================================
;;; allow simple-strings to include in CSound score (e.g. tempo, or comments) 

(defclass! cs-string () 
  ((str :accessor str :initarg :str :initform "" :type string)))

;;; short-hand
(defun cs-str (string) (make-instance 'cs-string :str string))

(defmethod om::synthesize-method ((self cs-string)) 'cs-synthesize)



