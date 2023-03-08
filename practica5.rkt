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
