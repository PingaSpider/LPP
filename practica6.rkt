(define (fib n)
   (fib-iter 1 0 n))

(define (fib-iter a b count)
   (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

    

;EJERCICIO 1
(define (concat lista)
  (concat-iter lista ""))

;definimos las operaciones
; si la lista esta vacia devolvemos el resultado
;caso contrario llamada recursiva con el resto de  la lista + string-append del resultado
;con el primer elemento de la lista.
(define (concat-iter lista result)
  (if(null? lista)
     result
     (concat-iter (rest lista) (string-append result (first lista)))))


(concat  '("hola" "y" "adiós")) ; ⇒ "holayadiós"


;EJERCICIO 1.b
(define (min-max lista)
  (min-max-iter lista (cons 0 0)))

;comparamos el primer elemento de la lista con la pareja inicial (0.0)
;si es menor llamada recurisva con pareja formada por el primer elemento de la lista y cdr de result
;si es mayor llamada recurisva con pareja formada por el car de result y primer elemento de la lista
;sino llamada recursiva con el result acumulado
(define (min-max-iter lista result)
  (cond ((null? lista) result)
        ((< (first lista)(car result)) (min-max-iter (rest lista) (cons (first lista) (cdr result))))
        ((> (first lista) (cdr result)) (min-max-iter (rest lista) (cons (car result)(first lista))))
        (else (min-max-iter (rest lista) result))))

(min-max '(2 5 9 12 5 0 4)) ; ⇒ (0 . 12)
(min-max '(3 2 -8 4 10 0)) ; ⇒ (-8 . 10)
;(min-max-iter '(5 9 12 -2 5 0 4) (cons 2 2)) ; ⇒ (-2 . 12)
