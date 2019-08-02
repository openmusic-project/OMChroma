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
; Authors: C. Agon, M. Stroppa, J. Bresson, S. Lemouton

; Automatic doc generation 
; File author: J. Bresson

(in-package :cr)

;;; EVALUATE THIS TO GENERATE THE REFERENCE PAGES
; (gen-omchroma-reference)
; (om::gen-package-entries (om::find-library "OMChroma"))

;;;==========================================
;;; GENERAL, HARD-CODED DOC INFO
;;;==========================================

;;;   Last update: April 29, 2009, ms
(defparameter *chroma-general-doc* 
  "<b>Welcome to the Auto-generated documebtation of OMChroma</b>

 <b>Naming conventions for the values of OMChroma class slots:</b>
 - % is always expressed between 0.0 and 1.0 (not 0 and 100)
 - int = integer number
 - flt = floating point number
 - mlt = multiplier of a reference value
 - GEN = csound GEN function object or a pointer (int) to it
")
  

(defparameter *chroma-appendix* 
  '("Csound tables conventions"
    ((1 "AUDIO (default size: 65537 points)")
     (2 "VIBRATO OR MODULATING (default size: 65537 points)")
     (3 "TREMOLO (default size: 65537 points)")
     (4 "LARGE NON-INTERPOLATING SINE (default size: 16777216 points)")
     (5 "LARGE COSINE (BUZZ, default size: 65537 points)")
     (6 "ASCENDING LINEAR SEGMENT FOR GLISSANDOS (0->1)")
     (7 "TRIANGLE FUNCTION")
     (8 "STRAIGHT LINE = 1")
     (10 "TRANSFER FUNCTION FOR WAVESHAPING")
     (11 "noise-modulated sine wave")
     (12 "sine wave with only one high partial (10th)")
     (13 "pseudo-inharmonic spectrum made of high partials")
     (19 "LARGE SIGMOID RISE/DECAY (1/2 COSINE, 65536 points)")
     (20 "INTERPOLATING ASCENDING SIGMOID RISE (1/2 COSINE, 65537 points)")
     (21 "INTERPOLATING DESCENDING SIGMOID RISE (1/2 COSINE, 65537 points)")
     (22 "SINE-BASED BELL SHAPE (SIN FROM -90 TO 270, 65537 points)")
     (23 "slowly descending exponential envelope")
     (24 "rapidly descending exponential envelope")
     (31 "AUDIO FILE (granular synthesis)")
     (32 "SHORT AUDIO FILE (granular synthesis)")
     )))


;;;=========================
;;; main function: 
;;; generates the OMChroma reference pages
;;; will be called automatically instead of the standard 
;;; gen-lib-reference thanks to the naming convention
;;;=========================
(defun om::gen-omchroma-reference ()
  (let ((lib (om::find-library "OMChroma")))
    (gen-chroma-reference-pages (om::gen-package-entries lib :exclude-packages '("classes" "Test"))
                                (om::gen-package-entries (om::get-subpackage lib "classes"))
                                (om::get-lib-reference-pages-folder lib)
                                :title (format nil "OMChroma ~A" (om::version lib))  
                                :general-doc *chroma-general-doc* 
                                ;:slots-doc-base *chroma-slots-doc* 
                                :appendix *chroma-appendix*
                                :logofile (probe-file (merge-pathnames (make-pathname :name "logo" :type "png")
                                                                       (om::lib-resources-folder lib)))
                                )
    ))

; (om::gen-omchroma-reference)

;;;=========================
;;; HTML pages generation
;;;=========================

(defun gen-chroma-reference-pages (ref-entries classes-entries dir &key title general-doc slots-doc-base appendix logofile)
    
  (when (probe-file dir) (om::om-delete-directory dir))
  (om::om-create-directory dir)
  
  (let ((indexpath (om::om-make-pathname :directory dir :name "index" :type "html"))
        (alphaindexpath (om::om-make-pathname :directory dir :name "ind-alpha" :type "html"))
        
        (allclasses (remove nil 
                             (append 
                              (om::find-value-in-kv-list (cdr classes-entries) :entries)
                             (loop for section in (om::find-value-in-kv-list (cdr classes-entries) :sections) append
                                   (append (om::find-value-in-kv-list (cdr section) :entries)
                                           (loop for group in (om::find-value-in-kv-list (cdr section) :groups) append 
                                                 (om::find-value-in-kv-list (cdr group) :entries)))))))
        (allsymbols (remove nil 
                             (append 
                              (om::find-value-in-kv-list (cdr ref-entries) :entries)
                             (loop for section in (om::find-value-in-kv-list (cdr ref-entries) :sections) append
                                   (append (om::find-value-in-kv-list (cdr section) :entries)
                                           (loop for group in (om::find-value-in-kv-list (cdr section) :groups) append 
                                                 (om::find-value-in-kv-list (cdr group) :entries)))))))
        )

    (when (probe-file logofile)
      (om::om-copy-file logofile (make-pathname :directory (pathname-directory dir) :name "logo" :type "png")))

    ;;; MAIN REFERENCE PAGE
    (with-open-file (index indexpath :direction :output)
      ;;; HEADER
      (write-line "<html>" index)
      (write-line "<head>" index)
      (write-line (concatenate 'string "<title>" title " Reference</title>") index)
      (write-line om::*om-ref-css* index)
      (write-line "</head>" index)
      
      (write-line "<body>" index)
      (write-line "<table width=100% cellpadding=20>" index)
      
      ;;; TITLE BAR
      (write-line  "<tr>" index)
      (write-line "<th width=10%>&nbsp;</th>" index)
      (write-line "<th>" index)
      (write-line (concatenate 'string "<H1><font size=+3>" title "</font> <br>Reference pages</H1>") index)
      (write-line "<p style=\"text-align: right;\">Main Index | <a class=\"dark\" href=ind-alpha.html>Alphabetical Index</a></p>" index)
      (write-line "</th>" index)
      (write-line "<th width=10%>&nbsp;</th>" index)
      (write-line "</tr>" index)
      
      (write-line "<tr>" index) 
      ;;; margin left
      (write-line "<td width=10%>&nbsp;</td>" index)
      
      (write-line "<td>" index)

      ;;; MAIN BODY AT CENTER
      
      (loop for lll in (om::om-text-to-lines general-doc) do 
            (write-line (concatenate 'string lll "<br>") index))

      (when appendix
        (write-line (concatenate 'string "<i>See also: <a href=appendix.html>" (car appendix) "</a></i>") index))
                 
      (write-line "<br><br><h2><center>SYNTHESIS CLASSES</center></h2>" index)
      
      (when (om::find-value-in-kv-list (cdr classes-entries) :entries)
        ;;; top level items (not in a section)
        (loop for item in (om::find-value-in-kv-list data :entries) do
              (write-line (concatenate 'string "<a href=" (om::special-path-check (string-downcase (string item))) ".html>" 
                                       (om::special-html-check (string item)) "</a> ") index))
        )

      (loop for section in (om::find-value-in-kv-list (cdr classes-entries) :sections) do
            
            (let ((name (om::find-value-in-kv-list (cdr section) :name))
                  (doc (om::find-value-in-kv-list (cdr section) :doc)))
              
              (write-line (concatenate 'string "<h4>" (string name) "</h4>") index)
              (when doc (write-line (concatenate 'string "<p>" doc "</p>") index))
              
              ;;; items in section (not in a sub-group)
              (loop for item in (om::find-value-in-kv-list (cdr section) :entries) do
                    (write-line (concatenate 'string "<a href=" (om::special-path-check (string-downcase (string item))) ".html>" 
                                             (om::special-html-check (string item)) "</a> ") index))
              
              (loop for group in (om::find-value-in-kv-list (cdr section) :groups) do
                    
                    (let ((n (om::find-value-in-kv-list (cdr group) :name))
                          (d (om::find-value-in-kv-list (cdr group) :doc)))
                      (when n (write-line (concatenate 'string "<h4>" (string n) "</h4>") index))
                      (when d (write-line (concatenate 'string "<p>" (string d) "</p>") index))
                      
                      (loop for item in (om::find-value-in-kv-list (cdr group) :entries) do
                            (write-line (concatenate 'string "<a href=" (om::special-path-check (string-downcase (string item))) ".html>" 
                                                     (om::special-html-check (string item)) "</a> ") index))
                      )
                    (write-line "<br><br>" index)
                    )))
      
      (write-line "<br><br>" index)
      (write-line "<hr><h2><center>OTHER OBJECTS AND FUNCTIONS<center></h2>" index)

      (when (om::find-value-in-kv-list (cdr ref-entries) :entries)
        ;;; top level items (not in a section)
        (loop for item in (om::find-value-in-kv-list data :entries) do
              (write-line (concatenate 'string "<a href=" (om::special-path-check (string-downcase (string item))) ".html>" 
                                       (om::special-html-check (string item)) "</a> ") index))
        )
      
      (loop for section in (om::find-value-in-kv-list (cdr ref-entries) :sections) do
               
            (let ((name (om::find-value-in-kv-list (cdr section) :name))
                  (doc (om::find-value-in-kv-list (cdr section) :doc)))
              
              (when name (write-line (concatenate 'string "<a name=" (string name) ">" "<h3>" (string-upcase (string name)) "</h3></a>") index))
              (when doc (write-line (concatenate 'string "<p>" doc "</p>") index))
             
              ;;; items in section (not in a sub-group)
              (loop for item in (om::find-value-in-kv-list (cdr section) :entries) do
                    (write-line (concatenate 'string "<a href=" (om::special-path-check (string-downcase (string item))) ".html>" 
                                             (om::special-html-check (string item)) "</a> ") index))
              
              (loop for group in (om::find-value-in-kv-list (cdr section) :groups) do
                    
                    (let ((n (om::find-value-in-kv-list (cdr group) :name))
                          (d (om::find-value-in-kv-list (cdr group) :doc)))
                      (when n (write-line (concatenate 'string "<h4>" (string n) "</h4>") index))
                      (when d (write-line (concatenate 'string "<p>" (string d) "</p>") index))
                      
                      (loop for item in (om::find-value-in-kv-list (cdr group) :entries) do
                            (write-line (concatenate 'string "<a href=" (om::special-path-check (string-downcase (string item))) ".html>" 
                                                     (om::special-html-check (string item)) "</a> ") index))
                      ))
              ))

      (write-line "<br><br><br>" index)
      (write-line (concatenate 'string "<center>" (om::credits-line) "</center>") index)
      
      (write-line "</td>" index)
      
      ;;; margin right
      (write-line "<td width=10%>&nbsp;</td>" index)

      (write-line "</tr></table>" index)
      (write-line "</body></html>" index))

    
    ;;; ALPHABETICAL INDEX PAGE
    
    (with-open-file (index alphaindexpath :direction :output)
      ;;; HEADER
      (write-line "<html>" index)
      (write-line "<head>" index)
      (write-line (concatenate 'string "<title>" title " Reference</title>") index)
      (write-line om::*om-ref-css* index)
      (write-line "</head>" index)
      
      (write-line "<body>" index)
      (write-line "<table width=100% cellpadding=20>" index)
      
      ;;; TITLE BAR
      (write-line  "<tr>" index)
      (write-line "<th width=10%>&nbsp;</th>" index)
      (write-line "<th>" index)
      (write-line (concatenate 'string "<H1><font size=+3>" title "</font> <br>Reference pages</H1>") index)
      (write-line "<p style=\"text-align: right;\"><a class=\"dark\" href=index.html>Main Index</a> | Alphabetical Index</p>" index)
      (write-line "</th>" index)
      (write-line "<th width=10%>&nbsp;</th>" index)
      (write-line "</tr>" index)
      
      (write-line "<tr>" index) 
      ;;; margin left
      (write-line "<td width=10%>&nbsp;</td>" index)
      
      (write-line "<td>" index)

      (mapcar #'(lambda (item) 
                  (write-line (concatenate 'string "<a href=" (om::special-path-check
                                                                  (string-downcase (string item))) ".html>" 
                                           (string item) "</a><br>") index))
              (sort (copy-list (append allsymbols allclasses)) 'string<)
              )
      (write-line "<br><br>" index)
      (write-line (concatenate 'string "<center>" (om::credits-line) "</center>") index)
      
      (write-line "</td></tr>" index)
      (write-line "</table>" index)
      (write-line "</body></html>" index))
  
    
    (setf *chroma-slots-list* nil)
      
    ;;; ENTRIES AND CLASSES PAGES
    (mapcar #'(lambda (symb) (make-chroma-ref-page symb dir title slots-doc-base appendix)) allclasses)
    (mapcar #'(lambda (symb) (om::make-ref-page symb dir title)) allsymbols)

    ;;; OTHER PAGES...
    (when appendix (make-cstables-ref-page dir title appendix))
    
    indexpath))

; (om::gen-omchroma-reference)



;;; GENERATES A PAGE FOR A CHROMA CLASS..
(defun make-chroma-ref-page (symbol dir &optional title slots-doc-base appendix)
  (let* ((title (or title "Class Reference"))
         (pagepath (om::make-pathname :directory (pathname-directory dir)
                                      :name (om::special-path-check
                                             (string-downcase (string symbol)))
                                      :type "html"))
         (class (find-class symbol nil))
         (lib (om::find-library (om::library class)))
         (dummy-instance (om::om-init-instance (make-instance symbol)))
         (doc (om::get-documentation-info symbol)))
    
    (with-open-file (index pagepath :direction :output)
      ;;; HEADER
      (write-line "<html>" index)
      (write-line (concatenate 'string "<head><title>" title " Reference: " (string symbol) "</title>") index)
      (write-line om::*om-ref-css* index)
      (write-line "</head>" index)
      
      (write-line "<body>" index)

      (write-line "<table align=center width=100% cellpadding=20>" index)
       
      (write-line "<th width=10%>&nbsp;</th>" index)

      (write-line "<th>" index)
      (write-line (concatenate 'string "<H1><font size=+3>" title "</font> <br>Reference pages</H1>") index)
      (write-line "<p style=\"text-align: right;\"><a class=\"dark\" href=index.html>Main Index</a> | <a class=\"dark\" href=ind-alpha.html>Alphabetical Index</a></p>" index)
      (write-line "</th>" index)
      (write-line "<th width=10%>&nbsp;</th>" index)

      (write-line  "</tr>" index)
      
      
      (write-line "<tr>" index)
    
      (write-line "<td width=10%>&nbsp;</td>" index)

      (write-line "<td>" index)

      ;;;
      (write-line "<table width=100%><tr>" index)

      ;;; HEADING SECTION
      (write-line "<tr>" index)
      (write-line "<td>" index)
      (write-line (concatenate 'string "<h2>" (om::special-html-check (string symbol)) "</h2>") index)
      (write-line "</td>" index)
          
      (write-line "<td width=60 align=right>" index)    
      (let* ((classicon (and (find-class symbol nil) (om::omclass-p class)
                             (om::icon class)))
             (iconfile (om::om-relative-path '("icons") (format nil "~A.png" classicon) (om::lib-resources-folder lib))))
        (if (om::file-exist-p iconfile)
            (setf iconfile (namestring iconfile))
          (setf iconfile "./logo.png"))
        (write-line (concatenate 'string "<p class=right><img src=" iconfile " width=60></p>") index)
        )
      (write-line "</td></tr>" index)
          
      (write-line "<tr><td colspan=2>" index)
          
      (write-line "<table width=100% border=0>" index)
         
      (write-line "<tr><td colspan=3><b><font color=882222>CLASS SLOTS:</font></b></td></tr>" index)
          
      (if (null (nth 2 doc))
          (write-line "None<br><br>" index)
        (progn
          (write-line "<table width=100% border=0>" index)
          (loop for slot in (nth 2 doc) do
                (write-line "<tr>" index)
                (cond ((consp slot)
                       (write-line (concatenate 'string "<td width=100> - <font color=333366><b>" 
                                                (string-downcase (car slot)) "</b></font></td>") index)
                       (write-line (concatenate 'string "<td>" (if (nth 2 slot) (om::special-html-check (nth 2 slot)) "") "</td>") index)
                       (write-line (concatenate 'string "<td>" (om::special-html-check (format nil "[default = ~A]" (nth 1 slot))) "</td>") index)
                       )
                      ;;; special items (&optional, etc.)
                      (t (write-line (concatenate 'string "<td colspan=3><br><i><b>" slot "</b></i></td>") index))
                      )
                (write-line "</tr>" index)
                )
          (write-line "</table>" index))
        )
          
   
      (let ((cs-instr-params (cs-description-params (cs-instr dummy-instance))))

        (when cs-instr-params
          (write-line "<tr><td colspan=3><br><b><font color=882222>CSOUND PARAMS (KEYWORDS):<br></font></b></td></tr>" index)
              
          (write-line "<table width=100% border=0>" index)
              
          (loop for param in cs-instr-params  
                for i = 0 then (+ i 1) do
                (let ((name (cs-param-description-name param)))
                  
                  (write-line "<tr>" index)
                  (write-line (concatenate 'string "<td class=top>- <b>" (string name) "</b></td>") index)
                  (write-line "<td class=top>" index) 
                  (write-line (om::special-html-check (cs-param-description-doc param)) index)
                  (write-line "</td>" index)
                    
                  (let* ((initval (cs-param-description-defval param))
                         (initprint (cond ((pathnamep initval) (concatenate 'string "File: " (pathname-name initval)))
                                          ((om::omclass-p (class-of initval)) (concatenate 'string "Instance: " (string (type-of initval))))
                                          (t (format nil "~s" initval)))))
                                    
                    (write-line (concatenate 'string "<td class=top>[default = " (om::special-html-check initprint) "]</td>") index))
                  (write-line "</tr>" index))
                )
          (write-line "</table>" index)
          ))

      ;;; DESCRIPTION
      (when (cs-description-doc-lines (cs-instr dummy-instance))
        (write-line "<br><font color=882222><b>DESCRIPTION:</b></font>" index)
        (write-line "<p>" index)
        ;;; (loop for str in (om::om-text-to-lines (nth 3 doc)) do
        (loop for str in (cs-description-doc-lines (cs-instr dummy-instance)) do
              (write-line (concatenate 'string "" (om::special-html-check str) "<br>") index))
        (write-line "</p>" index)
        )

      (when (cs-description-gens (cs-instr dummy-instance))
        (write-line "<font color=000000><b>PREDEFINED TABLES:</b></font>" index)
        (write-line "<p>" index)
        ;;; (loop for str in (om::om-text-to-lines (nth 3 doc)) do
        (loop for str in (cs-description-gens (cs-instr dummy-instance)) do
              (write-line (concatenate 'string "" (om::special-html-check str) "<br>") index))
        (write-line "</p>" index)
        )

      ;;; the pict...
      (let ((graph (probe-file (merge-pathnames (make-pathname :directory '(:relative "doc-picts")
                                                               :name (string symbol)
                                                               :type "gif")
                                                (om::lib-resources-folder lib)))))
        (when graph
          (write-line (concatenate 'string "<br><center><img src=../doc-picts/" (string-downcase (string symbol)) ".gif align=center width=500></center>") index)
          ))
      
      (write-line (concatenate 'string "<br><br><br><center>" (om::credits-line) "</center>") index)
      
      (write-line "</td>" index)

      ;;; margin right
      (write-line "<td width=10%>&nbsp;</td>" index)
      
      (write-line "</tr></table>" index)
      
      (write-line "</body></html>" index))
    pagepath))
      
; (om::gen-omchroma-reference)

(defun make-cstables-ref-page (dir &optional (title "") appendix)
  (let ((slotpath (make-pathname :directory (pathname-directory dir)
                                 :name "appendix" :type "html")))
    (with-open-file (index slotpath :direction :output)
      ;;; HEADER
      (write-line "<html>" index)
      (write-line "<head>" index)
      (write-line (concatenate 'string "<title>" title " Reference</title>") index)
      (write-line om::*om-ref-css* index)
      (write-line "</head>" index)
      
      (write-line "<body>" index)
      (write-line "<table width=100% cellpadding=20>" index)
      
      ;;; TITLE BAR
      (write-line  "<tr>" index)
      (write-line "<th width=10%>&nbsp;</th>" index)
      (write-line "<th>" index)
      (write-line (concatenate 'string "<H1><font size=+3>" title "</font> <br>Reference pages</H1>") index)
      (write-line "<p style=\"text-align: right;\"><a class=\"dark\" href=index.html>Main Index</a> | <a class=\"dark\" href=ind-alpha.html>Alphabetical Index</a></p>" index)
      (write-line "</th>" index)
      (write-line "<th width=10%>&nbsp;</th>" index)
      (write-line "</tr>" index)
      
      (write-line "<tr>" index) 
      ;;; margin left
      (write-line "<td width=10%>&nbsp;</td>" index)

      (write-line "<td>" index)
      (write-line (concatenate 'string "<H2>" (car appendix) "</H2>") index)
      
      (write-line "<table align=center cellpadding=2>" index)
      (loop for item in (cadr appendix) do
            (write-line "<tr>" index)
            (if (consp item)
                (write-line (concatenate 'string 
                                         "<td width=20><b>" (format nil "~D" (car item)) "</b></td>" 
                                         "<td>" (if (second item) (string (second item)) "") "</td>") index)
              (write-line (concatenate 'string "<td colspan=2>" (if (or (null item) (and (stringp item) (string-equal item ""))) "&nbsp;" (string item)) "</td>") index))
            (write-line "</tr>" index))
      (write-line "</table>" index)

      (write-line (concatenate 'string "<br><br><br><center>" (om::credits-line) "</center>") index)
      
      (write-line "</td>" index)
      ;;; margin right
      (write-line "<td width=10%>&nbsp;</td>" index)
      (write-line "<tr></table>" index)
    
      (write-line "</body></html>" index))

    slotpath))


   








