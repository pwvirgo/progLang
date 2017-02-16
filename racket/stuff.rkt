#lang racket

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