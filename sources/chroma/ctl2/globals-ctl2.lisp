;;CTL2 GLOBALS
(in-package chroma)

(defvar ctl2-def-fun1 (make_fun '(0 0 1 1)))
(defvar ctl2-def-fun2 (make_fun '(1 0 0 1)))
(defvar ctl2-def-fun3 (make_fun '(0 0 1 .5 0 1)))
#|
(defconstant *ctl2-keywords* '((ADD .(ED DUR AMP FQ BAL CAENV CAMOD CFMOD CFPRT NOSC ED2 DUR2 AMP2 STON))
                        (FM .(ED DUR AMP FQ BAL IEV IMIN IMAX STON CAENV CAMOD CFMOD CFPRT NFRM ED2 DUR2 AMP2 IEV2 IMIN2 IMAX2 STON2))
                        (WT .(ED DUR AMP CWT FQWT BAL CAENV CAMOD CFMOD CFPRT NOSC ED2 DUR2 AMP2 STON))))
|#

(defvar *default-ctl-model* 
  '(
    (ED     0.0)    
    (DUR 1)    
    (AMP (call get-model-norm-amp))    
    (BAL  0.5)
    (FREQ  (call get-model-fq))

    (CWT  (call get-model-cwt))
    (FQWT  (call get-model-fqwt))
    
    (IEV 10)                            ; amplitude envelope 10 = linear 1 -> 0   
    (IMIN 0.0)
    (IMAX (call get-model-bw-to-index))

    (CAENV ''(0.01 0.1 6))               ; amplitude envelope 6 = constant 1    
    (CAMOD ''(0.0 0.0 6.0))            
    (CFMOD ''(0.5 6.0 0.0 9))            ; amplitude envelope 6 = constant 1
    (CFPRT ''(0.0 7 0.1 0))              ; amplitude envelope 7 = constant 0   
    
    (NOSC 0)
    (NFRM 0)
    
    (ED2 0.0)    
    (DUR2  1.0)
    (AMP2  (call get-model-norm-amp))

    (IEV2 10)                           ; amplitude envelope 10 = linear 1 -> 0   
    (IMIN2 0.0)
    (IMAX2 1.5)

    (STON 0.0)
    (STON2 0.0)
  ))

;(enable-print-ctl2)

(set-gbl '*closest-wt* nil)
