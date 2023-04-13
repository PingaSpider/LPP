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

;CUMPLEN PREDICADO
;EN ESTE EJERCICIO UTILICE APLANA PQ NO DI CON UNA FORMA DE COMPROBAR HOJA Y PREDICADO JUNTOS PARA CONSTRUIR LA LISTA
;SIN TENER FALLOS.
;POR ESO EN PRIMER LUGAR APLANO LA LISTA Y LUEGO UTILIZO RECURSION NORMAL PARA CREAR LA LISTA FILTRADA
(define (cumplen-predicado pred lista)
 (aux pred (aplana lista)))

(define (aux pred lista)
  (cond ((null? lista) '())
        ((pred (first lista)) (append (list (first lista)) (aux pred (rest lista))))
        (else (aux pred (rest lista)))))
        
(define (cumplen-predicado-fos pred lista)
  (filter pred (aplana lista)))
  
  
(define (busca-mayores n lista)
  (cumple-predicado-fos (lambda (x)
                      (> x n)) lista))

(busca-mayores 10 '(-1 (20 (10 12) (30 (25 (15)))))) ; ⇒ (20 12 30 25 15)

(define (empieza-por char lista-pal)
  (cumple-predicado-fos (lambda (x)
                          (equal? char (string-ref (symbol->string x) 0))) lista-pal))

(empieza-por #\m '((hace (mucho tiempo)) (en) (una galaxia ((muy  muy) lejana))))
; ⇒ (mucho muy muy)


(define (sustituye-elem elem-old elem-new lista)
  (cond ((null? lista) '())
        ((hoja? lista) (if(equal? lista elem-old) elem-new lista))
        (else
         (cons (sustituye-elem elem-old elem-new (first lista))
               (sustituye-elem elem-old elem-new (rest lista))))))

(sustituye-elem 'c 'h '(a b (c d (e c)) c (f (c) g)))
; ⇒ (a b (h d (e h)) h (f (h) g))


;OPCION FOS
(define (sustituye-elem-fos elem-old elem-new lista)
  (cond ((hoja? lista) (if(equal? elem-old lista) elem-new lista))
        (else
        ;SI SOBRE MAP APLICAS LA LLAMADA SIN HACER LAMBDA ESTARA MAL
         (map (lambda (x)
                (sustituye-elem-fos elem-old elem-new x)) lista))))
                
                
;ESTE ME COSTO BASTANTE
(define (intersecta lista1 lista2)
(cond ((or(null? lista1)(null? lista2))
       '())
      ((if(hoja? (first lista1))
         (if(hoja? (first lista2))
            (cons (cons (first lista1)(first lista2)) (intersecta (rest lista1)(rest lista2)))
            (intersecta (rest lista1)(rest lista2)))
         (if(plana? (first lista1))
            (if(hoja? (first lista2))
               (intersecta (rest lista1)(rest lista2))
               (cons (cons (first (first lista1)) (first (first lista2))) (intersecta (rest lista1)(rest lista2))))
            (intersecta (rest lista1)(rest lista2)))))
      (else (intersecta (rest lista1)(rest lista2)))))

            
