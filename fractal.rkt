;KOCH
(define (koch nivel long)
  (if(= nivel 0)
     (line long 0 "black")
     (beside/align "bottom"
                   (koch (- nivel 1 ) long)
                   (rotate 60 (koch (- nivel 1) long))
                   (rotate -60 (koch (- nivel 1) long))
                   (koch (- nivel 1) long))))
                   
;Sierpinski
(define (hipotenusa x)
  (* x (sqrt 2)))

(define (sierpinski-elem base)
  (isosceles-triangle (hipotenusa (/ base 2)) 90 "outline" "black"))

(define (componer-sierpinski figura)
    (above figura
           (beside figura figura)))

(define (sierpinski ancho)
  (if (< ancho 10)
      (sierpinski-elem ancho)
      (componer-sierpinski (sierpinski (/ ancho 2)))))


;(sierpinski 300)



;COPO DE NIEVE
(define (copo_nieve nivel trazo)
  (above
    (beside/align "bottom"
                (rotate 60 (koch nivel trazo))
                (rotate -60 (koch nivel trazo)))
    (rotate -180 (koch nivel trazo))))
                                  

;(copo_nieve 3 5)


;ALFOMBRA DE SIERPINSKI 

(define (circulo-base tam)
  (circle (/ tam 2) "outline" "blue"))

(define (componer figura tam)
  (above (beside figura figura figura)
         (beside figura
                 (circle tam "solid" "blue")
                 figura)
         (beside figura figura figura)))





(define (carpet tam)
  (if(< tam 20)
     (circle tam "outline" "blue")
     (componer (carpet (/ tam 3)) (/ tam 3))))

(carpet 80 )

