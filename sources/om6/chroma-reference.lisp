
(in-package :om)

(defvar *chroma-ref-directory* nil)

(defun init-chroma-ref-dir ()
  (let ((crlib (find-library "OMChroma")))
    (when crlib
      (setf *chroma-ref-directory* 
            (lib-ref-path (find-library "OMChroma"))))))



(defun omchroma-reference ()
  (init-chroma-ref-dir)
  (gen-chroma-reference (gen-chroma-entries (find-library "OMChroma")
                                         :exclude-packages '("Basic Classes" "Advanced Classes" "Classes")) 
                        *chroma-classes-entries* 
                        *chroma-ref-directory* 
                        :title "OMChroma" 
                        :general-doc *chroma-general-doc* 
                        :doc-base *chroma-slots-doc* 
                        :appendix *chroma-appendix*)
  )

; (omchroma-reference)
; (gen-chroma-entries (find-library "OMChroma") :exclude-packages '("Basic Classes" "Advanced Classes" "Classes"))


(defmethod special-ref-location ((self internSynthEvt)) *chroma-ref-directory*)


(defvar *chroma-slots-doc* nil)

(defun chroma-doc (slotname &optional base)
  (cadr (find slotname (or base *chroma-slots-doc*) :key 'car :test 'string-equal)))     

(defvar *chroma-classes-entries* nil)

(defvar *chroma-appendix* nil)

(defvar *chroma-general-doc* nil)  

(defvar *credit-line* nil)

(defun gen-chroma-entries (lib &key exclude-packages)
  (cons
   (list "" (list (list "" (remove nil (append  (mapcar 'class-name (classes lib)) (mapcar 'function-name (functions lib)))))))
   (loop for pack in (subpackages lib) 
         when (not (find (name pack) exclude-packages :test 'string-equal))
         collect (list (name pack) (gen-subpack-entries pack :exclude-packages exclude-packages)))))

(defun gen-chroma-reference (ref-entries classes-entries dir &key title general-doc doc-base appendix)

  (let ((pagetitle (string+ (or title "") " Reference")))
    
    (when (probe-file dir) (om-delete-directory dir))
    (om-create-directory dir)
    
    ;;; copy OMChroma logo in ref-pages
    (let ((logopict (make-pathname :directory (pathname-directory (lib-resources-folder (find-library "OMChroma"))) :name "logo" :type "png")))
      (when (probe-file logopict)
        (om-copy-file logopict (make-pathname :directory (pathname-directory dir) :name "logo" :type "png"))))
    
    (setf *credit-line* (concatenate 'string "<center><font size=-2>" "OMChroma auto-doc generation for OpenMusic " *version-string* "</font></center>"))
    
    (let ((indexpath (make-pathname :directory (pathname-directory dir)
                                    :name "index" :type "html"))
          (alphaindexpath (make-pathname :directory (pathname-directory dir)
                                         :name "ind-alpha" :type "html"))
          (allclasses (remove nil (loop for pack in classes-entries append
                                        (loop for group in (cadr pack) append (cadr group)))))
          (allsymbols (remove nil (loop for pack in ref-entries append
                                        (loop for group in (cadr pack) append (cadr group))))))
      
      (with-open-file (index indexpath :direction :output)
        (write-line "<html>" index)
        (write-line (concatenate 'string "<head><title>" pagetitle "</title>") index)
        (write-line *om-ref-css* index)
        (write-line "</head>" index)
        (write-line "<body bgcolor=BBBBBB>" index)
        (write-line "<table align=center bgcolor=FFFFFF width=87%><tr>" index)
        (write-line "<td>" index)
        (write-line (concatenate 'string "<H1>" pagetitle "</H1>") index)
        (write-line "<p style=\"text-align: right;\"><a href=ind-alpha.html>Alphabetical Index</a></p>" index)
        (write-line "</td>" index)
        (write-line "<td><img src=./logo.png width=100 align=right></td></tr>" index)

        (write-line "<tr><td colspan=2>" index)
        (write-line "<hr>" index)
        (loop for lll in (om-text-to-lines general-doc) do (write-line (string+ lll "<br>") index))
        (write-line "</td></tr>" index)
      
        (write-line "<tr><td colspan=2>" index)
        (write-line "<hr><h2>Synthesis Classes<h2>" index)
        (loop for pack in classes-entries do
              (when (car pack)
                (write-line (concatenate 'string "<h3>" (string (car pack)) "</h3>") index))
              (loop for group in (cadr pack) do
                    (when (car group)
                      (write-line (concatenate 'string "<h4>" (string (car group)) "</h4>") index))
                    (loop for item in (cadr group) do
                          (write-line (concatenate 'string "<a href=" 
                                                   (special-path-check
                                                    (string-downcase (string item))) ".html>" 
                                                   (special-html-check (string item)) "</a>") index)
                          ))
              (write-line "<br><br>" index)
              )
      
        (write-line "</td></tr><tr><td colspan=2>" index)
        (write-line "<hr><h2>Other Objects and Functions<h2>" index)

        (loop for pack in ref-entries do
              (let ((name (if (consp (car pack)) (first (car pack)) (car pack)))
                    (doc (if (consp (car pack)) (second (car pack)) nil)))
                (when name
                  (write-line (concatenate 'string "<h3>" (string-upcase (string name)) "</h3>") index))
                (when doc
                  (write-line (concatenate 'string "<p>" doc "</p>") index))
                (loop for group in (cadr pack) do
                      (let ((n (if (consp (car group)) (first (car group)) (car group)))
                            (d (if (consp (car group)) (second (car group)) nil)))
                        (when n
                          (write-line (concatenate 'string "<h4>" (string n) "</h4>") index))
                        (when d
                          (write-line (concatenate 'string "<p>" (string d) "</p>") index))
                        (loop for item in (cadr group) do
                              (write-line (concatenate 'string "<a href=" 
                                                       (special-path-check
                                                        (string-downcase (string item))) ".html>" 
                                                       (special-html-check (string item)) "</a> ") index)
                              )
                        ))
                (write-line "<br><br>" index)
                ))

        (write-line "</td></tr><tr>" index)
        (write-line "<tr><td colspan=2 class=center><br><br><br>" index)
        (write-line *credit-line* index)
        (write-line "</td></tr>" index)
        (write-line "</table>" index)
        (write-line "</body></html>" index))

      (with-open-file (index alphaindexpath :direction :output)
        (write-line "<html>" index)
        (write-line (concatenate 'string "<head><title>" title "</title>") index)
        (write-line *om-ref-css* index)
        (write-line "</head>" index)
        (write-line "<body bgcolor=BBBBBB>" index)
        (write-line "<table align=center bgcolor=FFFFFF width=87%><tr>" index)
        (write-line "<td>" index)
        (write-line (concatenate 'string "<H3>Alphabetical Index</H3>") index)
        (write-line "<p style=\"text-align: right;\"><a href=index.html>Back to main Reference</a></p>" index) 
        (write-line "</td>" index)
        (write-line "<td><img src=./logo.png width=100 align=right></td></tr>" index)
      
        (write-line "<tr><td colspan=2>" index)
        (write-line "<hr>" index)
        (loop for lll in (om-text-to-lines (or general-doc "")) do (write-line (string+ lll "<br>") index))
        (write-line "</td></tr>" index)
      
        (write-line "<tr><td colspan=2><hr><br>" index)
        (mapcar #'(lambda (item) (write-line (concatenate 'string "<b><a href=" (special-path-check
                                                                                 (string-downcase (string item))) ".html>" 
                                                          (string item) "</a></b><br>") index))
                (sort (copy-list (append allsymbols allclasses)) 'string<)
                )
        (write-line "</td></tr>" index)
        (write-line "<tr><td colspan=2 class=center><br><br><br>" index)
        (write-line *credit-line* index)
        (write-line "</td></tr>" index)
        (write-line "</table>" index)
        (write-line "</body></html>" index))  
    
      (setf *chroma-slots-list* nil)
      
      (mapcar #'(lambda (symb) (make-chroma-ref-page symb dir pagetitle doc-base appendix)) allclasses)
      (mapcar #'(lambda (symb) (make-ref-page symb dir title)) allsymbols)

      (make-slots-ref-page dir title)
      (when appendix (make-cstables-ref-page dir title appendix))
    
      indexpath)))

(defmethod get-additional-slots ((self t)) nil)

(defmethod get-additional-slots ((self synthesisevt)) 
  (get-all-initargs-of-class (type-of self)))


(defun make-chroma-ref-page (symbol dir &optional title doc-base appendix)
  (let* ((title (or title "Class Reference"))
         (pagepath (make-pathname :directory (pathname-directory dir)
                                  :name (special-path-check
                                         (string-downcase (string symbol)))
                                  :type "html"))
         (dummy-instance (make-instance symbol))
         (doc (om-get-documentation symbol))
         
         (classicon (and (find-class symbol nil) (omclass-p (find-class symbol nil)) (icon (find-class symbol nil))))
         (icon (if (consp classicon) (car classicon) classicon))
         (iconfile (and icon (probe-file (make-pathname :directory (pathname-directory (lib-icons-folder (find-library "OMChroma")))
                                                       :name (integer-to-string icon)
                                                       :type "png"))))   ;;; !!! currently all icons are TIF
         
         (graph-pict (probe-file (make-pathname :directory (append (pathname-directory (lib-resources-folder (find-library "OMChroma"))) '("doc-picts"))
                                                :name (string symbol)
                                                :type "gif"))))
    
    (with-open-file (index pagepath :direction :output)
      (write-line "<html>" index)
      (write-line (concatenate 'string "<head><title>" (special-html-check (string symbol)) " - " title "</title>") index)
      (write-line *om-ref-css* index)
      (write-line "</head>" index)
      (write-line "<body bgcolor=BBBBBB>" index)
      (write-line "<br>" index)
      (write-line "<table align=center bgcolor=FFFFFF width=87% border=0><tr>" index)
      (write-line "<td>" index)
      (write-line (concatenate 'string "<a href=index.html>" title "</a>") index)
      (write-line (concatenate 'string "<h2>" (special-html-check (string symbol)) "</h2>") index)
          
      (write-line "</td><td width=130>" index)
      
      (if iconfile 
          (write-line (concatenate 'string "<img src=\""  (namestring iconfile) "\" type=\"image/tiff\" width=64>") index)
        (write-line "<img src=./logo.png width=64>" index))
    
      (write-line "</td></tr>" index)
      (write-line "<tr><td colspan=2>" index)
      (write-line "<table width=100% border=0>" index)
      (write-line "<tr><td colspan=3><b><font color=882222>CLASS SLOTS:</font></b></td></tr>" index)
      
      (when (find-class symbol nil)
      (multiple-value-bind (args initvals docs menus)
          (get-slot-in-out-names dummy-instance)
        (loop for slot in args
              for val in initvals
              for doc in docs do
              (unless (string-equal slot "self") (add-slot-ref slot symbol doc-base))
              (write-line "<tr>" index)
              (write-line (concatenate 'string "<td>- <b><a href=class-slots.html#" (string slot) ">" (string slot) "</a></b></td>") index)
              (write-line (concatenate 'string "<td>" doc "</td>") index)  ;;; (or (chroma-doc slot doc-base) doc)
              (write-line (concatenate 'string "<td>[default = " (special-html-check (format nil "~s" (eval val))) "]</td>") index)
              (write-line "</tr>" index)
              )))
      
      (let ((matrixslots (get-additional-slots dummy-instance)))
        (when matrixslots
          (write-line "<tr><td colspan=3><br><b><font color=882222>ARRAY SLOTS (KEYWORDS):<br></font></b></td></tr>" index)
      
          (loop for slot in matrixslots ; (nth 2 doc) 
            for i = 0 then (+ i 1) do
            (add-slot-ref (name slot) symbol doc-base)
            (write-line "<tr>" index)
            (write-line (concatenate 'string "<td class=top>- <b><a href=class-slots.html#" 
                                     (string (name slot)) ">" (string (name slot)) "</a></b></td>") index)
            (write-line "<td class=top>" index) 
            (write-line (or (chroma-doc (string (name slot)) doc-base) "") index)
            (let ((spec-doc (cadr (find (string (name slot)) (slot-docs (find-class symbol nil)) :key 'car :test 'string-equal))))
                  ; (cadr slot)))
              (when spec-doc
                (write-line "<br>" index) 
                (write-line (concatenate 'string "<i>" spec-doc "</i>") index)))
            (write-line "</td>" index)
            (let* (;(slt (find (string (name slot)) (get-all-initargs-of-class symbol) :key 'name :test 'string-equal))
                   (initform (theinitform slot));  (initform (and slt (theinitform slt)))
                   (initval (eval initform))
                   (initprint (cond ((pathnamep initval) (string+ "File: " (pathname-name initval)))
                                    ((omclass-p (class-of initval)) (string+ "Instance: " (string (type-of initval))))
                                    (t (format nil "~s" initval)))))
                                    
              (write-line (concatenate 'string "<td class=top>[default = " (special-html-check initprint) "]</td>") index))
            (write-line "</tr>" index))
          ))

      (write-line "</table>" index)
      (write-line "</td></tr>" index)
   
      (write-line "<tr><td colspan=2>" index)
      (when doc
        (write-line "<br><b><font color=882222>Description:</font></b><br>" index)
        (loop for str in (om-text-to-lines (nth 3 doc)) do
              (write-line (concatenate 'string "" (special-html-check str) "<br>") index))
        )
      
      (write-line "</td></tr>" index)

      (when graph-pict
          (write-line "<tr><td colspan=2 class=center>" index)
          (write-line (concatenate 'string "<img src=\"" (namestring graph-pict) "\" align=center width=600>") index)
          (write-line "</td></tr>" index)
          )
      
      (when appendix
        (write-line (concatenate 'string "<tr><td colspan=2 class=center><hr><br>see <a href=appendix.html>" (car appendix) "</a><br><br><br></td></tr>") index))
        

      (write-line "<tr><td colspan=2 class=center>" index)
      (write-line *credit-line* index)
      (write-line "</td></tr>" index)
      (write-line "</table>" index)
      
      (write-line "</body></html>" index))
    pagepath))
      

(defvar *chroma-slots-list* nil)
(defun add-slot-ref (slotname class &optional base)
  (let ((pos (position slotname *chroma-slots-list* :key 'car :test 'equal)))
    (if pos
      (pushr class (nth 2 (nth pos *chroma-slots-list*)))
      (push (list slotname (chroma-doc (string slotname) base) (list class)) *chroma-slots-list*))))


(defun make-slots-ref-page (dir &optional (title ""))
  (let ((slotpath (make-pathname :directory (pathname-directory dir)
                                 :name "class-slots" :type "html")))
    (with-open-file (index slotpath :direction :output)
      (write-line "<html>" index)
      (write-line (concatenate 'string "<head><title>" title "</title>") index)
      (write-line *om-ref-css* index)
      (write-line "</head>" index)
      (write-line "<body>" index)
      (write-line "<table bgcolor=FFFFFF align=center width=87% border=0><tr>" index)
      (write-line "<td colspan=2 align=left>" index)
      (write-line (concatenate 'string "<H3>Class Slots</H3>") index)
      (write-line "<a href=index.html>Back to main Reference</a>" index)
      (write-line "</td>" index)
      (write-line "<td><img src=./logo.png width=100 align=right></td></tr>" index)
      (write-line "<tr><td colspan=3><hr><br></td></tr>" index)
      
      (loop for item in (sort *chroma-slots-list* 'string< :key 'car) do
            (write-line "<tr>" index)
            (write-line (concatenate 'string "<td><a name=" (string (car item)) "><b>" (string (car item)) "</b></td>") index)
            (write-line (concatenate 'string "<td>" (if (second item) (string (second item)) "") "</td>") index)
            (write-line (concatenate 'string "<td width=25%><i>used in classes:</i><br>" 
                                     (subseq (reduce #'(lambda (str str2)
                                                         (concatenate 'string str ", " "<a href=" (string-downcase (string str2)) ".html>" (string str2) "</a>"))
                                                     (third item) :initial-value "")
                                      1)
                                     "<br><br></td>") index)
            (write-line "</tr>" index))
      (write-line "<tr><td colspan=3 class=center><br><br><br>" index)
      (write-line *credit-line* index)
      (write-line "</td></tr>" index)
      (write-line "</table>" index)
      
      (write-line "</body></html>" index))

    slotpath))


(defun make-cstables-ref-page (dir &optional (title "") appendix)
  (let ((slotpath (make-pathname :directory (pathname-directory dir)
                                 :name "appendix" :type "html")))
    (with-open-file (index slotpath :direction :output)
      (write-line "<html>" index)
      (write-line (concatenate 'string "<head><title>" title "</title>") index)
      (write-line *om-ref-css* index)
      (write-line "</head>" index)
      (write-line "<body>" index)
      (write-line "<table bgcolor=FFFFFF align=center width=87% border=0><tr>" index)
      (write-line "<td align=left>" index)
      (write-line (concatenate 'string "<H3>" (car appendix) "</H3>") index)
      (write-line "<a href=index.html>Back to main Reference</a>" index)
      (write-line "</td>" index)
      (write-line "<td><img src=./logo.png width=100 align=right></td></tr>" index)
      (write-line "<tr><td colspan=2><hr><br></td></tr>" index)
      
      (write-line "<tr><td colspan=2><table align=center>" index)
      (loop for item in (cadr appendix) do
            (write-line "<tr>" index)
            (if (consp item)
                (write-line (concatenate 'string 
                                         "<td><b>" (integer-to-string (car item)) "</b></td>" 
                                         "<td>" (if (second item) (string (second item)) "") "</td>") index)
              (write-line (concatenate 'string "<td colspan=2>" (if (or (null item) (and (stringp item) (string-equal item ""))) "&nbsp;" (string item)) "</td>") index))
            (write-line "</tr>" index))

      (write-line "</table></td></tr>" index)
      (write-line "<tr><td colspan=2 class=center><br><br><br>" index)
      (write-line *credit-line* index)
      (write-line "</td></tr>" index)
      (write-line "</table>" index)
      
      
      (write-line "</body></html>" index))

    slotpath))


; (init-chroma-ref-dir)
#|
(gen-chroma-reference *chroma-ref-entries* *chroma-ref-directory* 
                      :title "OMChroma Class Reference" 
                      :general-doc *chroma-general-doc* 
                      :doc-base *chroma-slots-doc* 
                      :appendix *chroma-appendix*)
|#

   








