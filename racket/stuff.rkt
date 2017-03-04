#lang racket

(define (make_func n)
  (let* ([x n]
         [y 7]
        [f (lambda(v) 
            (if (equal? 3 v) "good" (+ v x y)))])
    f))
       

(define z (make_func 5))

(z 9)

#|  a bunch of experiments about streams
(define (pows2)
  (define (helpme x)
     (lambda () (cons x (helpme (* x 2)))))
  (helpme 1))

(write "pows2       ") (writeln pows2)
(write "(pows2)     ") (writeln (pows2))
(write "((pows2))   ") (writeln ((pows2)))
(write "(car ((pows2)))   ")  (writeln (car ((pows2))))
(write "(cdr ((pows2)))   ")  (writeln (cdr ((pows2))))

(car ((cdr ((pows2)))))  ;2 

(car ((cdr ((cdr ((pows2))))))) ;4

(define ei ((cdr ((cdr ((pows2))))))) ; (4 (lambda ()) )

(define x 3)
(define ei2 ((cdr ((cdr ((cdr ((pows2))))))))) ; (8 (lambda ()) )
ei2
x
  a bunch of experiments about streams |#