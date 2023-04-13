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




;EJERCICIO 1 
(define lista-a '((a b) d (c (e) (f g) h)))
(check-equal? (fourth (third lista-a)) 'h)

;EJERCICIO 2
;A CONCATENA
(define (concatena lista)
  (cond ((null? lista) "")
        ((hoja? lista) (symbol->string lista))
        (else (string-append (concatena (first lista))
                             (concatena (rest lista))))))

(concatena '(a b (c) d)) ; ⇒ "abcd"
(concatena '(a (((b)) (c (d (e f (g))) h)) i)) ; ⇒ "abcdefghi"


;TODOS POSITIVOS
;B
(define (todos-positivos? lista)
  (cond
    ((null? lista)#t)
    ((hoja? lista) (< 0 lista))
    (else (and (todos-positivos? (first lista))
             (todos-positivos? (rest lista))))))

;Recuerda que cuando utilizas FOS no es necesario comprobar que
;la lista ha llegado a su fin
(define(todos-positivos-fos? lista)
        (if(hoja? lista)
           (< 0 lista)
           (for-all? (lambda (elem)
                          (todos-positivos-fos? elem))lista)))
                      

(todos-positivos? '(1 (2 (3 (-3))) 4)) ; ⇒ #f
(todos-positivos-fos? '(1 (2 (3 (3))) 4)) ; ⇒ #t
