;DEFINICIONES INTERESANTES

(define (hoja? elem)
  (not (list? elem)))

(define (plana? elem)
  (for-all? hoja? elem))

(define (estructurada? lista)
  (not (plana? lista)))


(define (num-hojas lista)
  (cond ((null? lista)0)
        ((hoja? lista) 1)
        (else (+ (num-hojas (first lista))
                 (num-hojas (rest lista))))))
     
         

(num-hojas '((1 2) (3 4 (5) 6) (7))) ; ⇒ 7

(define (aplana lista)
  (cond ((null? lista)
        '())
        ((hoja? lista)
         (list lista))
        (else (append (aplana (first lista))
                      (aplana (rest lista))))))
  
       


(aplana '(1 2 (3 (4 (5))) (((6)))))
; ⇒ (1 2 3 4 5 6)
     

(define (pertenece? dato lista)
  (cond ((null? lista) #f)
        ((hoja? lista) (equal? dato lista))
        (else (or (pertenece? dato (first lista))
                  (pertenece? dato (rest lista))))))
         
;(pertenece? 'a '(b c (d (a)))) ; ⇒ #t
;(pertenece? 'a '(b c (d e (f)) g)) ; ⇒ #f


(define (altura lista)
  (cond ((null? lista) 0)
        ((hoja? lista) 0)
        (else (max (+ 1 (altura (first lista)))
                 (altura (rest lista))))))

(altura '(1 (2 3) 4)) ; ⇒ 2
(altura '(1 (2 (3)) 3)) ; ⇒ 3

