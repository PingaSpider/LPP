#lang racket
(require rackunit)
(require "lpp.rkt")

(define (es-prefijo? pal1 pal2)
  (if(equal? pal1 (substring pal2 0 (string-length pal1)))
     #t
     #f))

(define (contiene-prefijo prefijo lista-pal)
  (if(null? lista-pal)
     '()
     (if(es-prefijo? prefijo (first lista-pal))
        (cons #t (contiene-prefijo prefijo (rest lista-pal)))
        (cons #f (contiene-prefijo prefijo (rest lista-pal))))))

;(contiene-prefijo "ante" '("anterior" "antígona" "antena" "anatema"))


(define (mayor cadena1 cadena2)
  (if (>= (string-length cadena1) (string-length cadena2))
      cadena1
      cadena2))

(define (cadenas-mayores lista1 lista2)
  (if (null? lista1)
      lista2
      (if(null? lista2)
          lista1
          (cons (mayor (first lista1) (first lista2)) (cadenas-mayores (rest lista1) (rest lista2))))))
          
          
(define (inserta-ordenada dato lista)
  (if(null? lista)
     (list dato)
     (if(> dato (first lista))
        (cons (first lista) (inserta-ordenada dato (rest lista)))
        (cons dato lista))))


(define (mueve-al-principio lista dato)
  (if (equal? dato (first lista))
      (rest lista)
      (cons (first lista) (mueve-al-principio (rest lista) (dato)))))


;LISTA_SIMBOLOS
(define (comprueba simbol num)
  (if(=(string-length (symbol->string simbol)) num)
     #t
     #f))


 (define (comprueba-simbolos lista-simbolos lista-num)
   (if(null? lista-simbolos)
      '()
      
      (if(comprueba (first lista-simbolos) (first lista-num))
         (cons (cons (first lista-simbolos) (first lista-num)) (comprueba-simbolos (rest lista-simbolos)
                                                                                (rest lista-num)))
         (comprueba-simbolos (rest lista-simbolos) (rest lista-num)))))

(comprueba-simbolos '(este es un ejercicio de examen) '(2 1 2 9 1 6))



(define (expande-pareja pareja)
  (if(= 0 (cdr pareja))
        '()
        (cons (car pareja) (expande-pareja (cons (car pareja)
                                                 (- (cdr pareja) 1))))))

(expande-pareja '(hola . 3)) ; ⇒ (hola hola hola)
(expande-pareja '(#t . 5)) ; ⇒ (#t #t #t #t #t)


;EJERCICIO 2

(define (expande-pareja pareja)
  (expande-pareja-iter pareja '()))

(define (expande-pareja-iter pareja result)
  (if(= 0 (cdr pareja))
     result
     (expande-pareja-iter (cons (car pareja)(- (cdr pareja) 1)) (append result (list (car pareja))))))




(expande-pareja (cons 'a 4)) ; ⇒ (a a a a)
;(expande-parejas '(#t . 3) '("LPP" . 2) '(b . 4))
; ⇒ (#t #t #t "LPP" "LPP" b b b b)


;2B
; IMPORTANTE: append debe recibir siempre dos lista pq sino hace una pareja
(define (rotar k lista)
  (if (= 0 k)
      lista
      (rotar (- k 1) (append (rest lista) (list(first lista))))))

(rotar 4 '(a b c d e f g)) ; ⇒ (e f g a b c d)
(rotar 5 '(a b c d e f g)) ; ⇒ (e f g a b c d)

;mi-foldl
(define (mi-foldl funt inicio lista)
  (mi-foldl-iter funt inicio lista ))

(define (mi-foldl-iter funt inicio lista)
  (if (null? lista)
      inicio
      (mi-foldl-iter funt (funt (first lista) inicio) (rest lista))))

(mi-foldl string-append "****" '("hola" "que" "tal")) ;⇒ "talquehola****"
(mi-foldl cons '() '(1 2 3 4)) ; ⇒ (4 3 2 1)


;EJERCICIO 3
(define (binario-a-decimal binario)
  (binario-a-decimal-iter binario 0))

(define (binario-a-decimal-iter lista result)
        (if (null? lista)
            result
            (binario-a-decimal-iter (rest lista) (+(* 2 result) (first lista)))))
  
(binario-a-decimal '(1 1 1 1)) ; ⇒ 15
(binario-a-decimal '(1 1 0)) ; ⇒ 6
(binario-a-decimal '(1 0)) ; ⇒ 2
