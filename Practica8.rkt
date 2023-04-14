;a.1) Escribe la sentencia en Scheme que define el siguiente árbol genérico y escribe utilizando las funciones de la 
;barrera de abstracción de árboles una expresión que devuelva el número 10.

(define arbol '(15 (14 (2) (3))
                  (8 (6))
                  (12 (9) (10) (11))))


(dato-arbol (second(hijos-arbol (third (hijos-arbol arbol)))))

;1b (suma-datos-arbol (hijos-arbol arbol) devuelve la suma de todos los arboles hijos

(+ 4 2 3 8 6 12 9 10 11)
(suma-datos-bosque (hijos-arbol arbol))


;b1
;Usando abstraccion
(construye-arbolb 10
                                (construye-arbolb 23
                                                  (construye-arbolb 5 arbolb-vacio arbolb-vacio)
                                                  (construye-arbolb 32
                                                                    (construye-arbolb 29 arbolb-vacio arbolb-vacio)
                                                                    (arbolb-vacio)))
                                (construye-arbolb 45
                                                  (arbolb-vacio)
                                                  (construye-arbolb 56 arbolb-vacio arbolb-vacio)))
;sin abstraccion
(define arbolb '(40 (23
                     (5 () ())
                     (32
                      (29 () ())
                      ()))
                    (45 ()
                        (56 () ()))))
                        
                        
;sacar 29
(dato-arbolb (hijo-izq-arbolb(hijo-der-arbolb(hijo-izq-arbolb arbolb))))



;2A
(define (to-string-arbol arbol)
  (string-append (symbol->string (dato-arbol arbol))
                 (to-string-bosque (hijos-arbol arbol))))
(define (to-string-bosque bosque)
  (if(null? bosque)
     ""
     (string-append (to-string-arbol (first bosque))
                   (to-string-bosque (rest bosque)))))

(define(to-string-arbol-fos arbol)
  (string-append (symbol->string (dato-arbol arbol))
                 (foldr string-append "" (map to-string-arbol-fos (hijos-arbol arbol)))))

(define arbol2 '(a (b (c (d)) (e)) (f)))
(to-string-arbol arbol2) ; ⇒ "abcdef"
(to-string-arbol-fos arbol2) ; ⇒ "abcdef"


;2B VECES ARBOL
(define (veces-arbol dato arbol)
  (if(equal? (dato-arbol arbol) dato)
     (+ 1 (veces-bosque dato (hijos-arbol arbol)))
     (+ 0 (veces-bosque dato (hijos-arbol arbol)))))

(define (veces-bosque dato bosque)
  (if(null? bosque)
     0
  (+ (veces-arbol dato (first bosque))
     (veces-bosque dato (rest bosque)))))
     
;VECES ARBOL FOS
;IMPORTANTE RECORDAR QUE MAP APLICA UNA FUNCION POR LO QUE SI TENEMOS FUNCIONES DE 
;DOS ARGUMENTOS DEBEMOS USAR UN LAMBDA
(define (igual? dato n)
  (if(equal? dato n)
     1
     0))
     
(define (veces-arbol-fos dato arbol)
  (+ (igual? dato (dato-arbol arbol))
                ;AQUI USAMOS LAMBDA PARA PODER LLAMAR A LA FUNCION CON RECURSION
     (foldr + 0 (map (lambda(x)
                       (veces-arbol-fos dato x)) (hijos-arbol arbol)))))
                       
                       
(veces-arbol 'b '(a (b (c) (d)) (b (b) (f)))) ; ⇒ 3
(veces-arbol 'g '(a (b (c) (d)) (b (b) (f)))) ; ⇒ 0
(veces-arbol-fos 'b '(a (b (c) (d)) (b (b) (f)))) ; ⇒ 3
(veces-arbol-fos 'g '(a (b (c) (d)) (b (b) (f)))) ; ⇒ 0


;CUMPLEN PREDICADO
(define (hojas-cumplen-bosque pred bosque)
  (if(null? bosque)
     '()
     (append (hojas-cumplen pred (first bosque))
             (hojas-cumplen-bosque pred (rest bosque)))))

(define (hojas-cumplen pred arbol)
  (if(and (hoja-arbol? arbol)(pred (dato-arbol arbol)))
     (list (dato-arbol arbol))
     (hojas-cumplen-bosque pred (hijos-arbol arbol))))

(define arbol1 '(10 (2) (12 (4) (2)) (10 (5))))
(define arbol2 '(10 (2) (12 (4) (2)) (10 (6))))
(hojas-cumplen even? arbol1) ; ⇒ '(2 4 2)
(hojas-cumplen even? arbol2) ; ⇒ '(2 4 2 6)


;EJERCICIO 4
;a)
(define (suma-raices-hijos arbol)
  (foldr + 0 (map (lambda(x)
                    (dato-arbol x)) (hijos-arbol arbol))))
 

(define arbol3 '(20 (2) (8 (4) (2)) (9 (5))))
(suma-raices-hijos arbol3) ; ⇒ 19
(suma-raices-hijos (second (hijos-arbol arbol3))) ; ⇒ 6


;RAICES MAYORES ARBOL
define (raices-mayores-arbol? arbol)
  (if( > (dato-arbol arbol) (suma-raices-hijos arbol))
     (raices-mayores-bosque? (hijos-arbol arbol))
     #f))

(define (raices-mayores-bosque? bosque)
  (if(null? bosque)
     #t
     (and (raices-mayores-arbol? (first bosque))
          (raices-mayores-bosque? (rest bosque)))))

(raices-mayores-arbol? arbol3) ; ⇒ #t
(raices-mayores-arbol? '(20 (2) (8 (4) (5)) (9 (5)))) ; ⇒ #f

;VERSION FOS
(define (raices-mayores-arbol-fos arbol)
  (for-all? (lambda(arbol)
              (> (dato-arbol arbol)
                 (suma-raices-hijos arbol))) (hijos-arbol arbol)))
                 
                 
;COMPRUEBA RAICES
(define (comprueba-raices-arbol arbol)
  (if( > (dato-arbol arbol) (suma-raices-hijos arbol))
     (cons  1 (comprueba-raices-bosque (hijos-arbol arbol)))
     (cons  0 (comprueba-raices-bosque (hijos-arbol arbol)))))

(define (comprueba-raices-bosque bosque)
  (if(null? bosque)
     '()
     (cons(comprueba-raices-arbol (first bosque))
          (comprueba-raices-bosque (rest bosque)))))

  

(comprueba-raices-arbol arbol3) ; ⇒ (1 (1) (1 (1) (1)) (1 (1)))
(comprueba-raices-arbol '(20 (2) (8 (4) (5)) (9 (5)))) 
; ⇒ (1 (1) (0 (1) (1)) (1 (1)))


;ESCAMINO
(define (es-camino? lista arbol)
  (if(equal? (first lista)(dato-arbol arbol))
     (es-camino-bosque? (rest lista)(hijos-arbol arbol))
     (es-camino-bosque? lista (hijos-arbol arbol))))

(define (es-camino-bosque? lista bosque)
  (cond ((and (null? lista)(null? bosque)) #t)
        ((null? lista)#f)
        ((null? bosque) #f)
        (else
         (es-camino? lista (first bosque))
         (es-camino-bosque? (rest lista)(rest bosque)))))

(es-camino? '(a b a) arbol) ; ⇒ #t
(es-camino? '(a b) arbol) ; ⇒ #f
(es-camino? '(a b a b) arbol) ; ⇒ #f
        
 
;NODOS NIVEL
(define (nodos-nivel nivel arbol)
  (if(= nivel 0)
     (list (dato-arbol arbol))
     (novo-nivel-bosque (- nivel 1) (hijos-arbol arbol))))


(define(novo-nivel-bosque nivel bosque)
  (if(or(null? bosque)(< nivel 0))
     '()
     (append (nodos-nivel nivel (first bosque))
             (novo-nivel-bosque nivel (rest bosque)))))

(nodos-nivel 0 arbol) ; ⇒ '(1)
(nodos-nivel 1 arbol) ; ⇒ '(2 6)
(nodos-nivel 2 arbol) ; ⇒ '(3 5 7)
(nodos-nivel 3 arbol) ; ⇒ '(4 2)


(define arbol3 '(20 (2) (8 (4) (2)) (9 (5))))
;(suma-raices-hijos arbol3) ; ⇒ 19
;(suma-raices-hijos (second (hijos-arbol arbol3))) ; ⇒ 6

(define arbolb1 '(20 (13 (2 () ())
                         (18 () ()))
                     (40 (25 () () )
                         (43 () ()))))
(define arbolb2 '(20 (13 (2 () ())
                         (22 () ()))
                     (40 (25 () () )
                         (43 () ()))))

;EJERCICO 6
(define(ordenado-entre? arbolb min max)
  (cond ((vacio-arbolb? arbolb) #t)
        ((> (dato-arbolb arbolb) max) #f)
        ((< (dato-arbolb arbolb) min) #f)
        (else
         (and (ordenado-entre? (hijo-izq-arbolb arbolb) min (dato-arbolb arbolb) )
              (ordenado-entre? (hijo-der-arbolb arbolb) (dato-arbolb arbolb) max )))))


;(ordenado-entre? arbolb1 0 50) ; ⇒ #t
;(ordenado-entre? arbolb2 0 50) ; ⇒ #f
;(ordenado-entre? arbolb1 0 30) ; ⇒ #f

(define (menor-arbol arbol)
  (if (vacio-arbolb? (hijo-izq-arbolb arbol))
      (dato-arbolb arbol)
      (menor-arbol (hijo-izq-arbolb arbol))))

(define (mayor-arbol arbol)
  (if (vacio-arbolb? (hijo-der-arbolb arbol))
      (dato-arbolb arbol)
      (mayor-arbol (hijo-der-arbolb arbol))))


(define (ordenado-menor? arbol max)
  (ordenado-entre? arbol (menor-arbol arbol) max ))

(define (ordenado-mayor? arbol min)
  (ordenado-entre? arbol min  (mayor-arbol arbol)))
      

(ordenado-menor? arbolb1 50) ; ⇒ #t
(ordenado-menor? arbolb1 40) ; ⇒ #f
(ordenado-menor? arbolb2 50) ; ⇒ #f
(ordenado-mayor? arbolb1 0)  ; ⇒ #t
(ordenado-mayor? arbolb1 20) ; ⇒ #f
(ordenado-mayor? arbolb2 0) ; ⇒ #f
        


(define (ordenado? arbol)
  (and (ordenado-menor? arbol (mayor-arbol arbol))
       (ordenado-mayor? arbol (menor-arbol arbol))))

(ordenado? arbolb1) ; ⇒ #t
(ordenado? arbolb2) ; ⇒ #f


;EJERCICIO 7
(define arbolb '(9
                (5 (3
                    (1 () ())
                    (4 () ()))
                   (7 () ()))
                (15 (13
                     (10 () ())
                     (14 () ()))
                    (29 ()
                        (23 () ())))))

(define (camino-arbolb arbol camino)
  (cond ((null? camino)
         '())
        ((equal? (first camino) '<)
         (camino-arbolb (hijo-izq-arbolb arbol) (rest camino)))
        ((equal? (first camino) '>)
         (camino-arbolb (hijo-der-arbolb arbol) (rest camino)))
        (else
         (cons (dato-arbolb arbol) (camino-arbolb arbol (rest camino))))))
         
         


