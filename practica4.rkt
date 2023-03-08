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





