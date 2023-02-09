;ejericio 1
(define (cuadrado x)
  (* x x))
(define (binario-a-decimal b3 b2 b1 b0)
  (+ ( * (cuadrado 2) 2 b3)
     (*  (cuadrado 2) b2)
     (* 2 b1)
     (* 1 b0)))

(check-equal? (binario-a-decimal 1 1 1 1) 15)
(check-equal? (binario-a-decimal 0 1 1 1) 7)
(check-equal? (binario-a-decimal 1 0 0 1) 9)


;ejercicio 1.2
(define (binario-a-hexadecimal b3 b2 b1 b0)
  (if (> (binario-a-decimal b3 b2 b1 b0) 9)
      (integer->char (+ 55 (binario-a-decimal b3 b2 b1 b0)))
      (binario-a-decimal b3 b2 b1 b0)))

;ejericio 2
(define (menor x y)
  (if (< x y)
      x
      y))
(define (menor-de-tres x y z)
  (menor (menor x y) z))



;ejercicio 3
(define (f x)
    (cons x 2))

(define (g x y)
    (cons x y))
;(g (f (+ 1 2)) (+ 1 1))
; -> llamamos a F despues de sumar 2+1 = 3;
;->F devuelve (3 . 2)
;-> llamamos a G = (cons (3 . 2) (+ 1 1) = (cons (3 . 2) 2)
;-> resultado ((3 . 2 ) . 2)

;ejercicio 3.1
;orden aplicativo
; (func-2 0 (func-1 10))
; entramos por func-1, cuando entramos la operacion es (/ 10 0)
;aqui se produciria un error por division entre 0
;ORDEN NORMAL
;en este caso entramos directamente en el if como el primer valor de la
;funcion es 0 evalua como verdadero y directamente devuelve el valor 0
; sin entrar a evaluar la otra parte.


;ejerciio 4
(define (mas-larga x y)
  (if (>= (string-length x) (string-length y))
      x
      y))
(define (cadenas-mayores lista1 lista2)
        (list(mas-larga (first lista1) ( first lista2))
             (mas-larga (second lista1) (second lista2))
             (mas-larga (third lista1) (third lista2))))
             
           
#lang racket
;EJERCIO CARTAS

;Definimos los diferentes palos
(define pica '♠)
(define corazon '♥)
(define diamante '♦)
(define trebol   '♣)

(define (obten-palo palo)
  (cond ((equal? palo pica)
         (string->symbol "Picas"))
        ((equal? palo corazon)
         (string->symbol "Corazones"))
        ((equal? palo diamante)
         (string->symbol "Diamantes"))
        (else string->symbol "Treboles")))

;Cuando defini la funcion, no puedo devolver un numero, me exige un procedimiento
;tengo duda respecto a esto, lo resolvi con una suma pero quiero que me lo aclare en clase porfa
(define (obten-valor valor)
  (cond ((equal? valor #\J)
        (+ 10 0))
        ((equal? valor #\Q)
        (+ 11 0))
        ((equal? valor #\K)
        (+ 12 0))
        ((equal? valor #\A)
        (+ 1 0))
        ;en caso de no ser ninguno sera alguno numero normal de la baraja
        (else(- (char->integer valor) 48))))

;usamo symbol-string para convertir en string las cartas que recibamos y poder trabajar sobre sus caracteres
(define (carta lacarta)
  (cons (obten-valor(string-ref (symbol->string lacarta) 0))
        (obten-palo (string-ref (symbol->string lacarta) 1))))

;con esta funcion aclaramos un poco del codigo de jugada-mano
;asi no se hace tan pesado de leer
(define (number-char carta1)
  (number->string(car (carta carta1))))

;comparamos si las cartas son iguales y hacemos el string
;la igualdad debe ser solo numerica no de palos
(define (jugada-mano carta1 carta2 carta3)
  (cond ((= (car (carta carta1)) (car (carta carta2)) (car (carta carta3)))
        (string-append "trio de " (number-char carta1)))
        ((= (car (carta carta1)) (car (carta carta2)))
        (string-append "pareja de " ((number-char carta1))))
        ((= (car (carta carta1)) (car (carta carta3)))
        (string-append "pareja de " (number-char carta1)))
        ((= (car (carta carta3)) (car (carta carta2)))
        (string-append  "pareja de " ((number-char carta2))))
        (else "nada")))
           

