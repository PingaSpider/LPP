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
