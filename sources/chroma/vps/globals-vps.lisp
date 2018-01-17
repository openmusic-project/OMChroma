(in-package chroma)


; maximum number of notes
(defvar *MAX-NN* 20)

(defvar *SPITCH-ALIST* '((DO . -9) 
                             (RE . -7) 
                             (MI . -5)
                             (FA . -4)
                             (SOL . -2)
                             (LA . 0)
                             (SI . 2)
                             (UT . -9)
                             (D0 . -9)
;AMERICAN SPELLING ADDED 8/97 (serge)
                             (C . -9) 
                             (D . -7) 
                             (E . -5)
                             (F . -4)
                             (G . -2)
                             (A . 0)
                             (B . 2)
                             ))
;for frequency to pitch conversions:
(defvar *SCPITCH-ALIST* '((0 . "DO")
                               (12 . "DO")
                               (1 . "DOd")
                               (2 . "RE") 
                               (3 . "MIb") 
                               (4 . "MI")
                               (5 . "FA")
                               (6 . "FAd")
                               (7 . "SOL")
                               (8 . "SOLd")
                               (9 . "LA")
                               (10 . "SIb")
                               (11 . "SI")))

(defvar *SPITCHLIST* '("DO" "RE" "MI" "FA" "SOL" "LA" "SI" "UT" "D0" 
                            "C" "D" "E" "F" "G" "A" "B"))

(defvar *ALTERATIONS* '("b" "d" "B" "D" "f" "s" "F" "S"))
(defvar *ALTERATIONS-ALIST* '((b . -1) (d . 1)(f . -1) (s . 1)))

;deviations en cents :
(defvar *DEVIATIONS-ALIST* '((-q . -50) (q . 50)))
(defvar *DEVIATIONS*  (mapcar #'car *DEVIATIONS-ALIST*))

(defvar *INTERVALLES-ALIST* '((1 . 0)
                                   (2- . 1)
                                   (2+ . 2)  
                                   (3- . 3)  
                                   (3+ . 4)(4- . 4)  
                                   (4 . 5)  
                                   (4+ . 6)(5- . 6)  
                                   (5 . 7)  
                                   (6- . 8)(5+ . 8)  
                                   (6+ . 9)  
                                   (7- . 10)  
                                   (7+ . 11) 
                                   (-2- . -1)
                                   (-2+ . -2)  
                                   (-3- . -3)  
                                   (-3+ . -4)(-4- . -4)  
                                   (-4 . -5)  
                                   (-4+ . -6)(-5- . -6)  
                                   (-5 . -7)  
                                   (-6- . -8)(-5+ . -8) 
                                   (-6+ . -9)  
                                   (-7- . -10)  
                                   (-7+ . -11) ))
(defvar *INTERVALLES* (mapcar #'car *INTERVALLES-ALIST*))

(defvar *WEIGHTS* '( ;INTERVALLIC WEIGHTS WITHIN AN OCTAVE
		(0 . 0.0)
		(1 . 10.0)
		(2 . 7.5)
		(3 . 1.5)
		(4 . 1.0)
		(5 . 3.5)
		(6 . 4.0)
		(7 . 2.0)
		(8 . 2.5)
		(9 . 3.0)
		(10 . 7.0)
		(11 . 9.5)))

(defvar *OCT-SCALERS* '( ;OCTAVE SCALERS
	(0 . 1.0)
	(1 . 0.85)
	(2 . 0.7)
	(3 . 0.45)
	(4 . 0.25)
	(5 . 0.1)
	(6 . 0.0)))

(defvar *STABILITY-SPACE* (cons *WEIGHTS* *OCT-SCALERS*)) ;REPRESENTATION NEEDED FOR THE STABILITY SPACE
;----------------------------------------------------------------------------

;AAA

(defun memberp (i list)
  (when (symbolp i)
    (setf i (internc i)))
  (member i list :test 'equal))

(defun internc (s)
  (intern (string-upcase (string s)) :cr)) 
