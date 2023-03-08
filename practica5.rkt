(define (aplica-veces f1 f2 n x)
  (if(= 0 n)
     x
     (f1(f2(aplica-veces f1 f2 (- n 1) x)))))

(aplica-veces (lambda (x) (+ x 1)) (lambda (x) (+ x 2)) 2 10) ; ⇒ 16
(aplica-veces (lambda (x) (* x x)) (lambda (x) (+ x 1)) 4 3) ; ⇒ 7072978201


(define (comprueba pred lista1 lista2)
  (if(null? lista1)
     '()
     (if(pred (first lista1) (first lista2))
        (cons (cons (first lista1) (first lista2))
              (comprueba pred (rest lista1) (rest lista2)))
        (comprueba pred (rest lista1) (rest lista2)))))

(comprueba (lambda (x y)
             (= (string-length (symbol->string x)) y))
           '(este es un ejercicio de examen) 
           '(2 1 2 9 1 6))
; ⇒ ((un . 2) (ejercicio . 9) (examen . 6))

(comprueba (lambda (x y)
              (= (string-length x) (string-length y)))
             '("aui" "a" "ae" "c" "aeiou")
             '("hola" "b" "es" "que" "cinco"))
; ⇒ (("a" . "b") ("ae" . "es") ("aeiou" . "cinco"))


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


;EJERCICIO 4

(define (contar-iguales lista)
  (foldr + 0 (map (lambda (x)
                  (if(equal? (car x) (cdr x))
                     1
                     0)) lista)))

(contar-iguales 
   '((2 . 3) ("hola" . "hola") (\#a . \#a) (true . false))) 
; ⇒ 2
(contar-iguales
   '((2 . "hola") ("hola" . 3) (\#a . true) (\#b . false))) 
; ⇒ 0


(define (suma-n-izq n lista)
  (map (lambda (x)
         (cons (+ (car x) n) (cdr x))) lista))

(suma-n-izq 10 '((1 . 3) (0 . 9) (5 . 8) (4 . 1)))
; ⇒ ((11 . 3) (10 . 9) (15 . 8) (14 . 1))


(define (aplica-2 func lista-parejas)
  (map (lambda (x)
         (func (car x) (cdr x))) lista-parejas))

(aplica-2 + '((2 . 3) (1 . -1) (5 . 4)))
; ⇒ (5 0 9)
(aplica-2 (lambda (x y)
             (if (even? x)
                 y
                 (* y -1))) '((2 . 3) (1 . 3) (5 . 4) (8 . 10)))
; ⇒ (3 -3 -4 10)
