#|

THOROUGH TESTS OF THE ELET SYSTEM
 

|#


;ATTENTION: MUST EVALUATE in-package AT THE BEGINNING!!!

(in-package chroma)


;;;;;; MAIN SYSTEM ;;;;;;;;;;;;;;;;;;;;

#|

;;;;;; ELET
; (elet list choices)
; Main function performing an exhaustive search of the first argument
; Inspired by some old code from Jacques Duthen
; First arg : list of numbers that must be found as the first element of each
;                element of the second argument
; Second arg: list of all the choices. The function will produce all the combinations
;                of elements substituting to each number all the elements following
;                the same number in the second argument, taken one by one
; Return: the list containing all the possibile combinations.

;;;;;; Meaning for the piece "Žlet...fogytiglan"
; Main global variable (evaluated in elet-sys.lisp)
(defvar *ELET-INT-FIELD* '(
		(2 1 2)
		(3 3 4)
		(4 5 6)
		(5 6 7)
		(6 8 9)
		(7 10 11)

		(9 13 14)
		(10 15 16)
		(11 17 18)
		(12 18 19)
		(13 20 21)
		(14 22 23)))
; 2 = interval of second (minor or major => 1 or 2 semitones)
; 3 = interval of third (minor or major => 3 or 4 semitones)
; etc. until the range of two octaves (with exclusion of unison/octave)

|#

(elet '(1 2 3) '((1 1 2) (2 3 4) (3 5 6)))
(elet '(3 2 1 3 3 2 1 2) '((1 1 2) (2 3 4) (3 5 6)))
(elet '(1 2 5) '((1 1 2) (2 3 4) (3 5 6)))
; "5" is not taken into account

(elet '(2 5 3) *ELET-INT-FIELD*)


;-----------------------------------------------------------------------------
;;;;;; GENERATE-ELET
; (generate-elet input-is beg-pch [itvl-field])

; input-is      : input list of numbers (major/minor intervals in ELET)
; beg-pch       : starting pitch for the results
; itvl-field    : field containing the choices (default: *ELET-INT-FIELD*)

; Return a list of SPL's starting at <beg-pch> without octaves

(generate-elet '(2 3 4 2 5 2 6) 'DO4 *ELET-INT-FIELD*)
(generate-elet '(2 3 4 2 5 2 6) 'DO4)
(generate-elet '(2 3 4 2 5 2 6) 'DO4
               '((2 1 2) (3 3) (4 4) (5 5) (6 6)))

;-----------------------------------------------------------------------------
;;;;;; SHOW-ELET
;;;;;; ANAL-ELET
; (show-elet vps-list pitch [:file <name>] [:approx <cents>])
; (anal-elet vps-list pitch [:file <name>] [:approx <cents>])

; vps-list      : list of VPS's usually produced by "generate-elet"
; :file <name>  : output file name, if missing, write on the Listener
; :approx <name>: pitch approximation in cents (default: t = no approximation)

; Show-elet prints the SPL's list starting at <beg-pch> without octaves
;   either in the Listener or in the file, when specified (the file will be found
;   in the directory "ELET" (see env.lisp).
; Anal-elet does the same including some analytical data

(show-elet (generate-elet '(2 3 4 2 5 2 6) 'DO4))
(show-elet (generate-elet '(2 3 4 2 5 2 6) 'DO4) :approx 50.0)
;(show-elet (generate-elet '(2 3 4 2 5 2 6) 'DO4) :file "pippo.vps")
;(show-elet (generate-elet '(2 3 4 2 5 2 6) 'DO4)
;           :file "pippo1.vps" :approx 50.0)

(anal-elet (generate-elet '(2 3 4 2 5 2 6) 'DO4))
(anal-elet (generate-elet '(2 3 4 2 5 2 6) 'DO4) :approx 50.0)
;(anal-elet (generate-elet '(2 3 4 2 5 2 6) 'DO4) :file "pippo.vps")
;(anal-elet (generate-elet '(2 3 4 2 5 2 6) 'DO4)
;           :file "pippo1.vps" :approx 50.0)

;-----------------------------------------------------------------------------
;;;;;; RUN-SHOW-ELET
;;;;;; RUN-ANAL-ELET
; (run-show-elet input-is-list pitch [:itvl-field *ELET-INT-FIELD*]
;                                    [:approx <cents>])
; (run-anal-elet input-is-list pitch [:itvl-field *ELET-INT-FIELD*]
;                                    [:approx <cents>])

; input-is-list : list of lists numbers (itvls) to be passed to "generate-elet"
; itvl-field    :  field containing the choices (default: *ELET-INT-FIELD*)
; approx <cents>: pitch approximation in cents (default: t = no approximation)

; Generate a data base consisting in a set of files in the default directory
;    "ELET" (see env.lisp). Each file contains all the solutions (with or
;    without analysis data) corresponding to calling "show-elet" with each
;    <input-is>. If the format of the input-is is compatible with the one
;    used in the piece Žlet...fogytiglan, the filename will be in the form:
;    <main-itvl>_<secondary-itvls>_<pitch>.vps (Ex. 2_3462-DO4.vps),
;    otherwise it will be of the form <itvls>-<pitch>.vps (Ex. 234-DO4.vps)
;    where <itvls> contain a maximum of 7 elements of the list. If the
;    elements are > 7, the name is <itvls>+<pitch>.vps (itvls = first 7 els).

(setf input-is-list '((2 3 4 2 5 2 6)
                      (2 2 3 2 3 2 5)
                      (2 2 3 2 5 2 4)))
(setf input-is-list1 '((2 3 4 2 5 2 6)
                      (1 2 3)
                      (1 2 3 4 5 6 7 8)))

#|
(run-show-elet input-is-list 'DO4)
(run-show-elet input-is-list 'DO4 :approx 50.0)
(run-show-elet input-is-list 'DO4
               :itvl-field '((2 3 4) (3 4 5) (4 5 6) (5 6 7) (6 7 8)))

(run-anal-elet input-is-list 'DO4)
(run-anal-elet input-is-list 'DO4 :approx 50.0)
(run-anal-elet input-is-list 'DO4
               :itvl-field '((2 3 4) (3 4 5) (4 5 6) (5 6 7) (6 7 8)))

(run-show-elet input-is-list1 'DO4)
(run-show-elet input-is-list1 'DO4 :approx 50.0)
(run-show-elet input-is-list1 'DO4
               :itvl-field '((2 3 4) (3 4 5) (4 5 6) (5 6 7) (6 7 8)))

(run-anal-elet input-is-list1 'DO4)
(run-anal-elet input-is-list1 'DO4 :approx 50.0)
(run-anal-elet input-is-list1 'DO4
               :itvl-field '((2 3 4) (3 4 5) (4 5 6) (5 6 7) (6 7 8)))

|#
;-----------------------------------------------------------------------------
