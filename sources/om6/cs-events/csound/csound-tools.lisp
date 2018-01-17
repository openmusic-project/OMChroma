(in-package :om)

(defvar *perf-is-running* nil)


;===============================================================
;PARSE AN ORC FILE AND DEFINE A CLASS FOR EACH CSOUND INSTRUMENT
;===============================================================

(defvar *last-orc-directory* nil)

(defmethod! get-instrument (&optional pathname) 
   :initvals nil :indoc nil :icon 602
   :doc "For the time being parse only csound.orc files."
   (let ((name (or pathname (om-choose-file-dialog :directory *last-orc-directory*))))
     (when (and name (probe-file name))
       (setf *last-orc-directory* (make-pathname :directory (pathname-directory name)))
       (load-instruments name))))

;;;===================================================

(defmethod! make-instrument ((input t)) 
   :initvals nil :indoc nil :icon 602
   :doc "For the time being parse only csound.orc files"
   nil)

(defmethod! make-instrument ((input pathname)) 
  (if (probe-file input)
    (load-instruments input)
    (format nil "~s not found" (namestring input))))

(defmethod! make-instrument ((input string)) 
  (make-instrument (pathname input)))


;;;===================================================

(defun load-instrument-from-head (file name)
  (let* ((rep (parse-csound-head file))
         (j (car rep))
         (globals (second rep))
         (chans (third rep))
         (macros (fourth rep)))
    (loop while (not (stream-eofp file)) do
    (when (integerp j)
      (let ((oldpos (file-position file))
            (numins (get-orc-numins file)))
        (def-csound-inst (pathname-name name) j (get-csound-source file oldpos) numins globals chans macros)
        (setf j (car (parse-csound-head file)))
        )))))

;;; !!!! file type !!!
(defun load-instruments (name)
   (cond
    ((string-equal (pathname-type name) "orc")
     (with-open-file (file name)
       (load-instrument-from-head file name)
       ))
    ((string-equal (pathname-type name) "csd")
     (with-open-file (file name)
       (look-for-line file "<CsInstruments>")
       (load-instrument-from-head file name)
       ))
    (t  (om-beep-msg "Sorry only .ORC or .CSD files can be parsed."))))

                    
(defun def-csound-inst (name num lines numins globals chans macros)
   (setf chans (or chans 1))
   (let* ((newclassname (interne (format nil "~D-~D" name num)))
          (exist? (find-class newclassname nil))  
          (define t) 
          (historique (format nil ";Instrument ~D from file ~D" num name))
          newins )
     (setf lines (cons historique lines))
     (when exist? 
       (setf define (om-y-or-n-dialog (format nil "The class ~D already exists, do you want to redefine it ?" newclassname))))
     (when define
       (setf newins (eval  `(defclass* ,newclassname (CS-Evt)
                               ((source-code :initform nil :allocation :class :type textfile :accessor source-code)
                                (globals-list :initform nil :allocation :class :type list :accessor globals-list)
                                (macros-list :initform nil :allocation :class :type list :accessor macros-list)
                                (numchan :initform ,chans :allocation :class  :accessor numchan)
                                (InstID :initform 1 :allocation :class :accessor InstID)
                                ,.(loop for i from 4 to numins
                                        collect (list (interne (format nil "P~D" i))
                                                      :initform 0 :initarg (string2initarg  (format nil "P~D" i))
                                                      :type 'number
                                                      :accessor (interne (format nil "P~D" i))))
                                )
                               (:icon 601))))
       (when newins
         (let ((defval (get-super-default-value (class-name newins))))
           (setf (source-code defval) (load-buffer-textfile lines 'textfile "append"))
           (setf (globals-list defval) globals)
           (setf (macros-list defval) macros)
           )
         
           (resave-class newins)
           )
       (if newins
         (unless exist?
           (unless *chroma-user-pack* (def-cr-userpack))
           (AddClass2Pack (class-name newins) *chroma-user-pack* :protect nil)
           (om-message-dialog (format nil "A new class ~A has been defined." (string-upcase newclassname)) :window-title "Chroma Message"))
         (om-message-dialog (format nil "Error defining class ~A." (string-upcase newclassname)) :window-title "Chroma Message")))))



(defvar *chroma-user-pack* nil)

(defun def-cr-userpack ()
  (if (member "Chroma User" (subpackages *package-user*) :test 'string-equal :key 'name)
      (setf *chroma-user-pack* (find "Chroma User" (subpackages *package-user*) :test 'string-equal :key 'name))
    (progn 
      (setf *chroma-user-pack* (make-instance 'OMPackage :name "Chroma User" :icon 159))
      (addpackage2pack *chroma-user-pack* *package-user* :protect nil))))




;===============================================================
;Tables
;===============================================================

(defmethod csound-load-tables ((self null)) nil)
(defmethod csound-load-tables ((self t)) nil)

(defmethod csound-load-tables ((self textfile)) 
  (eval-textfile self))

(defmethod csound-load-tables ((self function)) 
  (funcall self))

(defmethod csound-load-tables ((self function)) 
  (funcall self))

(defmethod csound-load-tables ((self list))
   (loop for item in self do 
         (csound-load-tables item)))

(defmethod csound-load-tables ((self string)) 
   (let ((rep (delete-spaces self)))
    (cond
     ((equal (elt rep 0) #\f) (SCsT self))
     (t (load (truename self))))))

(defmethod csound-load-tables ((self cs-table))
   (when (integerp (id self))
     (push self *globals-csound-tables*)))

(defmethod csound-load-tables ((self pathname)) (load self))

;===============================================================
;Building an orc file from an array
;===============================================================

(defmethod add-n-slots ((self SynthesisEvt)) 0)

(defmethod format-table-slots (array)
  (loop for n from 0 to (num-array-slots array) do
     (format-one-table-slot array n)))

(defmethod format-one-table-slot (array n)
  (when (or (subtypep (row-type array n) 'cs-table)
            (subtypep (row-type array n) 'multi-cs-table))
    (setf (nth n (data array))
          (mapcar 'get-table-id (nth n (data array))))
    ))


;(defmethod csound-lines ((self SynthesisEvt))
;  (let* ((offset (+ (action-time self) (/ (offset self) 1000.0)))
;         (nslots (+ (num-array-slots self) (add-n-slots self)))
;         (id (instid self))
;         (array (clone self))
;         rep)
;    (format-table-slots array)
;    (setf rep
;          (loop for i from 0 to (- (numcols array) 1) append
;                (let ((arraycol (if (functionp (Parsing-fun array))
;                                    (list! (funcall (Parsing-fun array) array i))
;                                  (list (get-array-col array i)))))
;                  (loop for item in arraycol collect
;                        (let* ((itemdata (if (component-p item) (comp-list item) item))
;                               (dataline (cs-params-list (if (listp itemdata)
;                                                             (first-n itemdata nslots)
;                                                           itemdata))))
;                          (real-csound-line dataline id offset array)))
;                  )))
;    rep))


(defmethod csound-lines ((self SynthesisEvt))
  (let* ((offset (+ (action-time self) (/ (offset self) 1000.0)))
         (nslots (+ (num-array-slots self) (add-n-slots self)))
         (id (instid self))
         (array (clone self))
         rep)
    (format-table-slots array)
    (setf rep
          (loop for i from 0 to (- (numcols array) 1) append
                (let ((arraycol nil))
                  (if (user-fun array)
                      (loop for f in (list! (user-fun array)) do
                            (setf arraycol (list! (funcall f array i))))
                    (setf arraycol (list (get-array-col array i))))
                  (loop for item in arraycol collect
                        (let* ((itemdata (if (component-p item) (comp-list item) item))
                               (dataline (cs-params-list (if (listp itemdata)
                                                             (first-n itemdata nslots)
                                                           itemdata))))
                          (real-csound-line dataline id offset array)))
                  )))
    rep))

     

(defun str-format (num)
  (format nil "i~~D ~~{~~,~DF ~~}" 3))

(defmethod cs-params-list ((self t)) self)
(defmethod cs-params-list ((self list)) (flat self))
(defmethod cs-params-list ((self component)) (flat (val-list self)))


(defmethod real-csound-line ((self list) id offset array)
  (let ((str (format nil "i~D " id)))
    (setf (nth 0 self) (+ (nth 0 self) offset))
    (loop for item in self for i = 0 then (+ i 1) do
          (setf str (concatenate 'string str 
                                 (if (numberp item) (format nil (format nil "~~,~DF " (get-precision array i)) item)
                                   (format nil "~s " (if (pathnamep item) (namestring item) 
                                                       (if (subtypep (type-of item) 'sound)
                                                           (namestring (om-sound-file-name item))
                                                         item))))))
          )
    str))

(defmethod real-csound-line ((self string) id offset array) 
   (declare (ignore id offset array))
   (format nil ";~D" self))




;======================================


(defun get-event-list (list)
  (remove-duplicates 
   (remove-if 'stringp list) 
    :test 'equal :key 'type-of))

;;;; NOT USED from OM 6.3
(defun dialogue-dup (globlist type)
  (let* ((typename (cond ((equal type :global) "Global Variable")
                         ((equal type :macro) "Macro")
                         (t "??")))
                         
         (thedialog (om-make-window 'om-dialog  
                                    :window-title (string+ typename " conflicts")
                                    :size (om-make-point 500 450) 
                                    :close nil :resizable nil :maximize nil))
         (scrol (om-make-view 'om-scroller
                  :owner thedialog
                  :size (om-make-point 480 300)
                  :scrollbars :v
                  :field-size (om-make-point 270 (* 70 (length globlist)))
                  :position (om-make-point 10 80))) 
         tablelist)
    (loop for item in globlist
          for i = 0 then (+ 1 i) do
          (let ((table (om-make-dialog-item 'om-single-item-list 
                                            (om-make-point 120 (+ 6 (* 65 i))) 
                                            (om-make-point 300 60) ""
                                         :font (om-make-font "Courier" 12)
                                         ;:container thedialog
                                         :scrollbars :v
                                         :range (loop for cr in item collect (nth (cond ((equal type :global) 1)
                                                                                        ((equal type :macro) 2)
                                                                                        (t "??" 0)) cr)))))
            (push table tablelist)
            (om-select-item-index table 0)
            (om-add-subviews scrol (om-make-dialog-item 'om-static-text
                                                        (om-make-point 5 (+ 18 (* 65 i))) (om-make-point 70 20)
                                                        (caar item)
                                                        :font (om-make-font "Courier" 13 :style '(:bold)))
                             table)
          ))
    
    
    (om-add-subviews thedialog
                     (om-make-dialog-item 'om-static-text
                                          (om-make-point 10 10) (om-make-point 300 60)
                                          (string+ "Some conflicting " typename
                                                   "s were detected between the different classes. Please choose one for each declared "
                                                   typename ".")
                                          :font *controls-font*
                                          )
      (om-make-dialog-item 'om-button (om-make-point 250 390) (om-make-point  80 24) "Cancel" 
                        :di-action (om-dialog-item-act item
                                     (declare (ignore item)) 
                                     (om-return-from-modal-dialog thedialog nil)))
     (om-make-dialog-item 'om-button (om-make-point 350 390) (om-make-point  80 24) "OK" 
                        :di-action (om-dialog-item-act item
                                    (declare (ignore item)) 
                                     (om-return-from-modal-dialog thedialog (loop for item in globlist
                                                                                  for cel in tablelist
                                                                                  collect (nth (om-get-selected-item-index cel) item)))) 
                        :default-button t :focus t))
    
    (om-modal-dialog thedialog)
    ))

;(resoudre-duplicatas testlist :macro)

;(setf testlist '(("a" "" "gfh ; test comment")
;                 ("a" "" "  azer")
;                 ("a" "" " 	=	360000")))
;                 ("a" "" "envore ;kghjkgh")
;                 ("a" "" "   init 32767 				; 16 bits")
;                 ("a" "" "gfh ; test comment")
;                 ("a" "" "  azer")
;                 ("a" "" "  	=	360000")
;                 
;                 ("b" "" "envore ;kghjkgh")
;                 ("b" "" "   init 32767 				; 16 bits")
;                 ("b" "" "gfh ; test comment")
;                 ("b" "" "  azer")
;                 ("b" "" "  	=	360000")
;                 ("b" "" "envore ;kghjkgh")
;                 ("c" "" "   init 32767 				; 16 bits")
;                 ("c" "" "gfh ; test comment")
;                 ("c" "" "  azer")
;                 ("c" "" "  	=	360000")
;                 ("c" "" "envore ;kghjkgh")
;                 ))




(defun string-simplify (str)
  (let ((lineend (or (search ";" str)
                     (length str))))
    (remove-line-spaces (remove-tabs (subseq str 0 lineend)))))

(defun remove-tabs (str)
  (let ((pos (position #\Tab str)))
    (when pos 
      (setf (elt str pos) #\Space)
      (setf str (remove-tabs str)))
    str))


(defun remove-line-spaces (str &optional (from 0))   
  (let ((spacepos (search " " str :start2 from)))
    (cond ((null spacepos)  str)
          ((= spacepos 0)  (remove-line-spaces (subseq str 1)))
          ((= spacepos (- (length str) 1))  (remove-line-spaces (subseq str 0 spacepos)))
          ((string-equal " " (elt str (+ spacepos 1)))         
           (remove-line-spaces (concatenate 'string (subseq str 0 spacepos) (subseq str (+ spacepos 1)))))
          (t  (remove-line-spaces str (+ spacepos 1))))))
    


;;;; NOT USED from OM 6.3          
(defun dup-simplify (a &optional (type :gbl))
  (cond 
   ((equal type :macro) (list (car a) (cadr a) (string-simplify (caddr a))))
   ((equal type :global) (list (car a) (string-simplify (cadr a))))
   (t a)))

;;;; NOT USED from OM 6.3
(defun dup-equal (a b &optional (type :global))
  (cond 
   ((equal type :macro) 
    (and (string-equal (string-simplify (second a)) (string-simplify (second b)))
         (string-equal (string-simplify (third a)) (string-simplify (third b)))))
   ((equal type :global) 
    (string-equal (string-simplify (second a)) (string-simplify (second b))))
   (t nil)))   

;;;; NOT USED from OM 6.3
(defun dup-fatal-error (a b &optional (type :global))
  (cond 
   ((equal type :macro)
    (if (and (string-equal (car a) (car b))
             (not (string-equal (string-simplify (second a)) (string-simplify (second b)))))
        (progn 
          (om-message-dialog (string+ "Error: Macro " (car a) " is defined in various classes with incompatible arguments !"))
          t)
      nil))
   (t nil)))

; (dialogue-dup '(("a = b") ("a = 5")) :gbl)

;;;; NOT USED from OM 6.3
(defun resoudre-duplicatas (globlist &optional (type :global))
  (let (original)
    (setf globlist (remove-duplicates globlist :test 'equal))
    (setf original (copy-list globlist))
    (let (duplicatas autosolve)
      (loop while globlist do
            (let ((first (pop globlist)) dup)
              (loop for item in globlist do
                    (when (string-equal (car first) (car item))
                       (cond ((dup-equal first item type)
                              (push (dup-simplify item type) autosolve)
                              (setf globlist (remove item globlist)))
                             ((dup-fatal-error first item type)
                              (abort))
                             (t 
                              (push (dup-simplify item type) dup)
                              (setf globlist (remove item globlist))))))
              (when dup 
                (push (dup-simplify first type) dup)
                (push dup duplicatas))))
      (when duplicatas
        (setf duplicatas (dialogue-dup (reverse duplicatas) type)))
      (let ((finalista (append autosolve duplicatas)))
        (loop for item in finalista do
              (setf original (remove (car item) original :key 'car :test 'string-equal)))
        (setf original (append original finalista)))
      original)))


;;;; NOT USED from OM 6.3
(defun overwrite-globals (list newgbls)
  (let ((l1 ())
        (l2 (mapcar #'(lambda (item) (list (string-simplify (car item)) (cadr item))) newgbls)))
    (loop for item in list do 
          (let ((redefined (find (string-simplify (car item)) l2 :test 'string-equal :key 'car)))
            (unless redefined (push (list (string-simplify (car item)) (cadr item)) l1))))
    (append (reverse l1) l2)))


;;;; NOT USED from OM 6.3
(defun write-globals-variables (file instlist &optional forced-gbls)
  (let (globallist)
    (unless (listp (car forced-gbls)) (setf forced-gbls (list forced-gbls)))
    (setf globallist (loop for item in instlist append (globals-list item)))
    (setf globallist (overwrite-globals globallist forced-gbls))
    (setf globallist (resoudre-duplicatas (copy-list globallist) :global))
    (format file "; GLOBAL VARIABLES~%~%")
    (loop for gvar in globallist do
          (format file "~D  ~D~%" (car gvar) (second gvar)))
    (when globallist (format file "~%~%"))
    ))

;;;; NOT USED from OM 6.3
(defun write-macros (file instlist)
  (let ((maclist (loop for item in instlist append (macros-list item))))
    (setf maclist (resoudre-duplicatas (copy-list maclist) :macro))
    (format file "; MACROS~%~%")
    (loop for mac in maclist do
          (format file "#define ~D~D #~D#~%" (car mac) (or (cadr mac) "") (caddr mac)))
    (when maclist (format file "~%~%"))
    ))

;;;; NOT USED from OM 6.3
(defun write-opcodes (file strlist)
    (when strlist
      (format file "; USER-DEFINED OPCODES~%~%")
      (loop for oc in (list! strlist) do
            (format file oc))
      (format file "~%~%")))


;;;=================================


(defun init-equal (init1 init2)
  (and (equal (init-type init1) (init-type init2))
       (string-equal (string-simplify (name init1)) (string-simplify (name init2)))
       (equal (mapcar 'string-simplify (params init1)) (mapcar 'string-simplify (params init2)))
       (string-equal (string-simplify (value init1)) (string-simplify (value init2)))))

(defun init-dup-fatal-error (a b)
  (let ((t1 (init-type a))
        (t2 (init-type b)))
    (if (equal t1 t2) 
        (cond 
         ((equal t1 :macro)
          (if (and (string-equal (string-simplify (name a)) (string-simplify (name b)))
                   (not (equal (mapcar 'string-simplify (params a)) (mapcar 'string-simplify (params b)))))
              (progn 
                (om-message-dialog (string+ "Error: Macro " (name a) " is defined in various classes with incompatible arguments !"))
                t)
            nil))
         (t nil))
      nil)))

(defun solve-init-duplicates (init-list &optional type)
  (let* ((original (remove-duplicates init-list :test 'init-equal))
        (copy (copy-list original)))
    (let (duplicatas autosolve)
      (loop while copy do
            (let ((first (pop copy)) dup)
              (loop for item in copy do
                    (when (string-equal (string-simplify (name first)) (string-simplify (name item)))
                       (cond ((init-equal first item)
                              (push item autosolve)
                              (setf copy (remove item copy)))
                             ((init-dup-fatal-error first item)
                              (abort))
                             (t 
                              (push item dup)
                              (setf copy (remove item copy))))))
              (when dup 
                (push first dup)
                (push dup duplicatas))))
      (when duplicatas
        (or (setf duplicatas (init-duplicates-dialog (reverse duplicatas) type))
            (om-abort)))
        
      (let ((finalista (append autosolve duplicatas)))
        (loop for item in finalista do
              (setf original (remove (name item) original :key 'name :test 'string-equal)))
        (setf original (append original finalista)))
      original)))


(defun write-cs-globals (file global-inits)
  (when global-inits
    (let ((globallist (solve-init-duplicates global-inits :global)))
      (when globallist
        (format file "; GLOBAL VARIABLES~%~%")
        (loop for gvar in globallist do
              (format file "~D  ~D~%" (name gvar) (value gvar)))
        (format file "~%~%"))
      )))

(defun write-cs-macros (file mac-inits)
  (when mac-inits
    (let ((maclist (solve-init-duplicates mac-inits :macro)))
      (when maclist 
        (format file "; MACROS~%~%")
        (loop for mac in maclist do
              (format file "#define ~D~D #~D#~%" (name mac) (or (params mac) "") (value mac)))
        (format file "~%~%"))
      )))

(defun write-cs-opcodes (file opcode-inits)
  (when opcode-inits
    (let ((oclist (solve-init-duplicates opcode-inits :opcodes)))
      (when oclist
        (format file "; USER-DEFINED OPCODES~%~%")    
        (loop for oc in oclist do
              (format file "opcode ~A, ~A, ~A~% ~A~%endop" (name oc)  (car (params oc)) (cadr (params oc)) (value oc))
              (format file "~%~%"))
        ))))

   
(defun overwrite-cs-inits (originals news)
  (mapcar #'(lambda (new) (setf (name new) (string-simplify (name new)))) news)
  (let ((not-redefined nil))
    (loop for item in originals do 
          (let ((redefined (find item news 
                                 :test #'(lambda (a b) 
                                                 (and (equal (init-type a) (init-type b))
                                                      (string-equal (string-simplify (name a)) (string-simplify (name b))))))))
            (unless redefined (push item not-redefined))))
    (append (reverse not-redefined) news)))

(defun get-all-inits (instlist &optional type)
  (loop for item in instlist append 
        (if type (get-inits-type (cs-inits item) type)
          (cs-inits item))))

(defun write-cs-inits (out instlist inits)
  (let ((new-list (overwrite-cs-inits (get-all-inits instlist) inits)))
    (write-cs-globals out (get-inits-type new-list :global))
    (write-cs-macros out (get-inits-type new-list :macro))
    (write-cs-opcodes out (get-inits-type new-list :opcode))))


(defun init-duplicates-dialog (initdup-list type)
  (let* ((typename (cond ((equal type :global) "Global Variable")
                         ((equal type :macro) "Macro")
                         ((equal type :opcode) "UDO")
                         (t "??")))
                         
         (thedialog (om-make-window 'om-dialog  
                                    :window-title (string+ typename " conflicts")
                                    :size (om-make-point 500 450) 
                                    :close nil :resizable nil :maximize nil))
         (scrol (om-make-view 'om-scroller
                  :owner thedialog
                  :size (om-make-point 480 300)
                  :scrollbars :v
                  :field-size (om-make-point 270 (* 70 (length initdup-list)))
                  :position (om-make-point 10 80))) 
         tablelist)
    (loop for duplicates in initdup-list
          for i = 0 then (+ 1 i) do
          (let ((table (om-make-dialog-item 'om-single-item-list 
                                            (om-make-point 120 (+ 6 (* 65 i))) 
                                            (om-make-point 300 60) ""
                                         :font (om-make-font "Courier" 12)
                                         ;:container thedialog
                                         :scrollbars :v
                                         :range (loop for init in duplicates collect (flat-string (value init))))))
            (push table tablelist)
            (om-select-item-index table 0)
            (om-add-subviews scrol (om-make-dialog-item 'om-static-text
                                                        (om-make-point 5 (+ 18 (* 65 i))) (om-make-point 70 20)
                                                        (name (car duplicates))
                                                        :font (om-make-font "Courier" 13 :style '(:bold)))
                             table)
          ))
    
    
    (om-add-subviews thedialog
                     (om-make-dialog-item 'om-static-text
                                          (om-make-point 10 10) (om-make-point 400 60)
                                          (string+ "Some conflicting " typename
                                                   "s were detected between the different classes. Please choose one for each declared "
                                                   typename ".")
                                          :font *controls-font*
                                          )
      (om-make-dialog-item 'om-button (om-make-point 250 390) (om-make-point  80 24) "Cancel" 
                        :di-action (om-dialog-item-act item
                                     (declare (ignore item)) 
                                     (om-return-from-modal-dialog thedialog nil)))
     (om-make-dialog-item 'om-button (om-make-point 350 390) (om-make-point  80 24) "OK" 
                        :di-action (om-dialog-item-act item
                                    (declare (ignore item)) 
                                     (om-return-from-modal-dialog thedialog (loop for item in initdup-list
                                                                                  for cel in tablelist
                                                                                  collect (nth (om-get-selected-item-index cel) item)))) 
                        :default-button t :focus t))
    
    (om-modal-dialog thedialog)
    ))


(defun nchannels-init? (inits)
  (let ((chan-inits (get-inits-type (list! inits) :channels)))
    (and chan-inits (reduce 'max (mapcar 'value chan-inits) :initial-value 1))))


;==============


(defun all-channels-equal? (list)
  (let ((chan (numchan (car list)))
        (rep t))
  (loop for item in list 
        when rep do
        (setf rep (= chan (numchan item))))
  (if rep chan)))

(defmethod filtre-events-list ((ev string) funlist) t)

(defmethod filtre-events-list (ev funlist)
  (let ((rep t))
    (loop for fun in funlist
          while rep do
          (setf rep (funcall fun ev)))
    rep))

(defun filtre-events (list funlist) 
  (loop for item in list
        when (filtre-events-list item (list! funlist)) collect item))

;; attention aux conflits !!
(defun write-instrument-headers (file instlist)
  (loop for item in instlist do
        (loop for line in (orc-header item) do
              (write-line  line file))))

(defvar *verbose-matrix*  1)


(defmethod format-inits (inits cs-events)
  (flat 
   (mapcar #'(lambda (init) 
               (format-init init cs-events))
           inits)))

;;; SOME SPECIFIC CLASS OF INITS CAN BE RE FORMATTED ACCORDING TO THE CLASS(ES) SYNTHESIZED
;;; SEE FOR INSTANCE OMPRISMA PRISMA-INIT
;;; => MUST RETURN ONE OR LIST OF CS-INITS
(defmethod format-init (inits cs-events) inits)
  
;===================================
; CS-SYNTHESIZE

(defmethod collect-synthesis-events ((self t)) nil)
(defmethod collect-synthesis-events ((self list)) 
   (remove-if 'null (loop for object in self collect (collect-synthesis-events object))))

(defmethod collect-synthesis-events ((self container)) 
   (remove-if 'null (loop for object in (inside self) collect (collect-synthesis-events object))))
                          
(defmethod collect-synthesis-events ((self SynthesisEvt)) self)

(defmethod collect-synthesis-events ((self string)) self)


(defmethod cs-Synthesize ((csound-events t) &key  
                            tables inits
                            (name "my-synt") 
                            (sr nil) (kr  nil) 
                            (nchnls 1) (rescale nil) (run t) (evt-test) (resolution nil)
                            opcodes
                            &allow-other-keys)
  
  (unless (maq-obj-p csound-events)
    (setf csound-events (list! csound-events)))
  
  (let* ((synth-sr (or sr *audio-sr* 44100))
         (synth-kr (or kr synth-sr))
         (elements (filtre-events (collect-synthesis-events csound-events) evt-test))
         (ksmps (/ synth-sr synth-kr))
         (normalize rescale)
         (instlist (get-event-list (copy-list elements)))
         (path-aiff (if (equal :rt name) name
                      (if (pathnamep name) name 
                        (corrige-sound-filename (string+ name ".aiff") *om-outfiles-folder*))))
         (cs-basename (if (equal :rt name) "cs_temp" (pathname-name path-aiff)))
         (path-orc (handle-new-file-exists (tmpfile (string+ cs-basename ".orc"))))
         (path-sco (handle-new-file-exists (tmpfile (string+ cs-basename ".sco"))))  
         (nch nil)
         rep)
  
      (when run 
        (add-tmp-file path-orc)
        (add-tmp-file path-sco))
      
      (unless elements (om-beep-msg "No synthesis events were found."))
      (when elements
        (when *verbose-matrix*
          (print "Start synthesis"))
        (setf *globals-csound-tables* nil)
        (csound-load-tables (list! tables))
        
        (setf inits (format-inits (flat (list! inits)) csound-events))

        (setf nch (or nchnls 
                      (nchannels-init? inits)
                      (all-channels-equal? instlist)))
        
        (if (null nch)
            (progn
              (om-message-dialog "Error: All instruments must have the same number of channels !")
              (setf rep nil))
          (progn
            (loop for item in instlist
                  for i = 1 then (+ i 1) do
                  (setf (instID  item) i)) 
            
             ;(loop for item in instlist do
             ;      (print (list item (instid item))))
             
             (WITH-OPEN-FILE (out path-orc :direction :output  :if-does-not-exist :create :if-exists :supersede)
               (format out "; HEADER ~%~%sr = ~D~%kr = ~D~%ksmps = ~D~%nchnls = ~D~%~%" synth-sr synth-kr ksmps nch)
               
               (write-cs-inits out instlist (list! inits))

               (format out "~%~%;INSTRUMENTS~%~%")
               
               ;(loop for item in instlist
               ;      for i = 1 then (+ i 1) do
               ;      (setf (instID  item) i))

               (loop for item in instlist
                     for i = 1 then (+ i 1) do
                     (format out ";==============~%instr ~D~%;==============~%~%" i)
                     ;;; !!!!
                     (loop for lin in (list-of-lines (buffer-text (source-code item))) do
                           (write-line lin out))
                     ;;; !!!!
                     (format out "endin~%~%")))

             (WITH-OPEN-FILE (out path-sco :direction :output :if-does-not-exist :create :if-exists :supersede)
               (format out ";This synthesis process called ~D started on " name)
               (printdate out)
               (format out "~%~%")
               (format out "; Global Variables: sr = ~D, kr = ~D, ksmps = ~D, nchnls = ~D~%~%" synth-sr synth-kr ksmps nch)
               (format out "; Defined by chroma classes:~%")
               (write-instrument-headers out instlist)
               (format out "~%; Loaded tables:~%")
               (loop for item in *globals-csound-tables* do
                     (format out "~D~%" (cs-table-string item)))
               (format out "~%; Generated tables:~%~%")
               
               (setf *table-counter* 0)
               (loop for item in elements
                     for k = 1 then (+ k 1) do
                     (if (stringp item)
                       (write-line item out)
                       (progn
                         (when (and *verbose-matrix* (>= *verbose-matrix* 1)
                                    (zerop (second (multiple-value-list (floor k *verbose-matrix* )))))
                           (print (format nil "------ Matrix n. ~D --------" k)))
                         
                         (clrhash *dynamic-cs-table-table*)
                         (prepare-array-tables item)
                         
                         (format out "~%;------ Lines for event n. ~D --------~%~%" k)
                         
                         (let ((cs-lines (csound-lines item)))
                         
                           (maphash #'(lambda (key val) 
                                        (declare (ignore key))
                                        (write-line val out)) 
                                    *dynamic-cs-table-table*)
                     
                           (loop for lin in cs-lines
                                 for l = 1 then (+ l 1) do
                                 (when (and *verbose-matrix* (<= *verbose-matrix* -1)
                                            (zerop (second (multiple-value-list (floor l *verbose-matrix* )))))
                                   (print (format nil "------ Line n. ~D --------" l)))
                                 (write-line lin out)))
                     
                         (erase-temp-id-tables item)
                         (clrhash *dynamic-cs-table-table*)
                         )))
               (format out "~%~%"))
             
             (if run   
               (if (require-library "OM2Csound")
                   (setf rep (csound-synth path-sco path-orc path-aiff normalize resolution))
                 (om-beep-msg "Library OM2Csound not found!!!"))
               (setf rep (list (probe-file path-orc) (probe-file path-sco))))
             
             (when *delete-inter-file* (clean-tmp-files))
             )
          )
        rep)))


(defmethod! synthesize ((elements cs-evt) &key (name "my_synt") sr (rescale nil) (run t) (evt-test  nil) resolution 
                        kr tables (nchnls nil) inits ;For Csound
                        patch duration ;For Chant
                        )        
            (cs-synthesize elements :name name :sr sr :kr kr :tables tables 
                           :inits inits :nchnls nchnls 
                           :rescale rescale :run run :evt-test evt-test :resolution resolution))


(defmethod synthesize-method ((self cs-evt)) 'cs-Synthesize)
(defmethod synthesize-method ((self string)) 'cs-Synthesize)

(defun to-cs-synth (evt)
  (or (subtypep (type-of evt) 'cs-evt)
      (stringp evt)))


(pushr 'to-cs-synth *synthesis-element-tests*)




;======================
(defmethod component-p ((self component)) t)
(defmethod component-p ((self t)) nil)


(defmethod! general-parsing ((self component) (predicates list) (modifiers list) &key (remove-duplicatas t) (remove-fields nil))
   :initvals '(nil nil nil t)
   (let ((comp-list nil)
         (current-comp self))
     (loop for item in predicates
           while (component-p current-comp) do
           (let ((temp-rep (funcall item current-comp)))
             (cond
              ((component-p temp-rep) 
               (setf current-comp temp-rep))
              ((stringp temp-rep)
               (setf comp-list (append comp-list (list temp-rep)))
               (setf current-comp nil))
              ((listp  temp-rep)
               (setf current-comp (car temp-rep))
               (setf comp-list (append comp-list (cdr temp-rep))))
              (t (setf current-comp nil)))))
     (when current-comp
       (loop for item in modifiers do
             (setf comp-list (append comp-list (list! (funcall item current-comp))))))
     (when remove-duplicatas
       (setf comp-list (remove-duplicates comp-list :test 'equal)))
     (make-remove-fields comp-list remove-fields)
     comp-list))


;=======================
