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



(map (lambda (x)
         (cond 
            ((symbol? x) (symbol->string x))
            ((number? x) (number->string x))
            ((boolean? x) (if x "#t" "#f"))
            (else "desconocido"))) '(1 #t hola #f (1 . 2))) ; ⇒ ?

(filter (lambda (x) 
            (equal? (string-ref (symbol->string x) 1) #\a)) 
    '(alicante barcelona madrid almería)) ; ⇒ ?

(foldr (lambda (dato resultado)
          (string-append dato "*" resultado)) "" 
          '("Hola" "que" "tal")) ; ⇒ ?

(foldr append '() '((1 2) (3 4 5) (6 7) (8))) ; ⇒ ?

(foldl (lambda (dato resultado)
         (string-append
          (symbol->string (car dato))
          (symbol->string (cdr dato))
          resultado)) "" '((a . b) (hola . adios) (una . pareja))) ; ⇒ ?

(foldr (lambda (dato resultado)
           (cons (+ (car resultado) dato)
                 (+ (cdr resultado) 1))) '(0 . 0) '(1 1 2 2 3 3)) ; ⇒ ?

(apply + (map cdr '((1 . 3) (2 . 8) (2 . 4)))) ; ⇒ ?

(apply min (map car (filter (lambda (p)
                                  (> (car p) (cdr p))) 
                                  '((3 . 1) (1 . 20) (5 . 2))))) ; ⇒ ?
    

; Los siguientes ejercicios utilizan esta definición de lista

(define lista '((2 . 7) (3 . 5) (10 . 4) (5 . 5)))


; Queremos obtener una lista donde cada número es la suma de las
; parejas que son pares

(filter even?
        (map (lambda (x) (+ (car x)
                                 (cdr x)))
               lista))
; ⇒ (8 14 10)

; Queremos obtener una lista de parejas invertidas donde la "nueva"
; parte izquierda es mayor que la derecha.

(filter (lambda (x) (>(car x) (cdr x)))
        (map (lambda(x) (cons (cdr x) (car x))) lista))
; ⇒ ((7 . 2) (5 . 3))


(define (f1 x) (lambda (y z) (string-append y z x)))
(define g1 (f1 "a"))
(check-equal? (g1 "clase" "lpp") "claselppa")



; Queremos obtener una lista cuyos elementos son las partes izquierda
; de aquellas parejas cuya suma sea par.
(foldr cons '()
        (map car (filter (lambda (x) (even? (+ (car x) (cdr x)))) lista)))
; ⇒ (3 10 5)




(define (f2 x) (lambda (y z) (list y x z)))
(define g2 (f2 "lpp"))
(check-equal? (g2 "hola" "clase") (list "hola" "lpp" "clase"))


(define (f3 g3) (lambda(z x) (g3 z x)))
(check-equal? ((f3 cons) 3 4) '(3 . 4))

