;TO CREATE DIPHONE SCRIPT FROM AudioSculpt Markers File
;lemouton@ircam.fr

(defvar trans-beg 1/3)
(defvar trans-end 2/3)

(defun write-script (stream markers title)
  (let ((cnt 0)(ls (append '(0) (cddr markers))))
    (loop while (cddr ls)
          initially (let* ((center (second ls))
                           (distance-post (-(third ls) center)))
                      (format stream "Segment ~A-debut  beg ~A ctr ~A  end ~A ~%"
                              title center center (+ center (* distance-post trans-end)))
                      (pop ls))
          do (let* ((center (second ls))
                    (distance-pre (- center (first ls)))
                    (distance-post (-(third ls) center)))
               (format stream "Segment ~A-~A  beg ~A ctr ~A  end ~A ~%"
                       title  cnt (- center (* distance-pre (- 1 trans-beg))) center (+ center (* distance-post trans-end))))
          do  (pop ls)
          do  (incf cnt)
          finally  (let* ((center (second ls))
                          (distance-pre (- center (first ls))))
                     (format stream "Segment ~A-fin  beg ~A ctr ~A  end ~A ~%"
                             title   (- center (* distance-pre (- 1 trans-beg))) center center )))))

(let* ((file (choose-file-dialog))
       (prefixe (string-right-trim ".mark" (pathname-name file)))
       (markers (with-open-file  (stream file) (read stream)))
       (file-out (make-pathname :directory (pathname-directory file) :name (format () "~a.script"prefixe ))))
  (with-open-file  (outstream file-out :direction :output :if-exists :supersede)
  (write-script outstream markers prefixe)))
