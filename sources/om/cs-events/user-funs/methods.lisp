;******************************************************************
(in-package :om)
;------------------------------------------------------------------

; THIS FILE CONTAINS SOME USEFUL METHODS NEEDED IN OMCHROMA
;   (USUALLY LISP CODE OF OMCHROMA PATCHES)

;******************************************************************
; FUNCTIONS:
; funs-from-db
;------------------------------------------------------------------
; METHODS:
; 
;------------------------------------------------------------------
; TESTS FOR GENERALIZED-PARSING-FUN:
; check-durmin
; test-durmin
;******************************************************************

;------------------------------------------------------------------
; FUNCTIONS
;------------------------------------------------------------------

(defmethod! funs-from-db ((fun-list t))
   :icon 615
   :doc 
"Return a list of files containing Csound functions from a data base
   specified by CSfun to be passed as an argument to synthesize.
The files should be strings without extension. A default extension .lisp is
   automatically loaded"
  (let ((dir (cr::get-cr-path :csfun)))
    (if dir 
        (append (list (make-pathname :directory (pathname-directory dir)
                                     :name "DEF-funs" :type "lisp"))
                (loop for fun in fun-list
                      collect (let ((file
                                     (make-pathname
                                      :directory (pathname-directory dir)
                                      :name (string fun) :type "lisp")))
                                (if (probe-file file)
                                    file
                                  (error "Hello! Would you, please, tell me, sir ~a, what I should do with an inexistant function file?~%       ~a~%"
                                         (cr::get-gbl 'cr::USER) file)))))
      (print (format () "funs-from-db - WARNING: THE DIRECTORY WHERE TO LOOK FOR FILE DOES NOT EXIST")))))

;------------------------------------------------------------------
; METHODS
;------------------------------------------------------------------
;------------------------------------------------------------------
; TESTS FOR GENERALIZED-PARSING-FUN
;------------------------------------------------------------------
#|
(defmethod check-durmin ((evt-list t) durmin)
"Eliminate the event (do not collect it),
   if its duration is shorter than the allowed minimum duration (durmin)"
   (let ((evt-list (list! evt-list)))
        (loop for evt in evt-list do
              (when (test-durmin evt durmin)
                collect evt))))

(defmethod test-durmin ((evt Cs-evt) durmin)
"Return t if the Cs-evt's total duration (durtot) is not shorter than durmin"
  (when (< durmin (durtot evt) )))

; (durtot (make-instance 'simplesinus+))

|#
;******************************************************************
