#lang racket
;; Programming Languages Homework4 Simple Test
;; Save this file to the same directory as your homework file
;; These are basic tests. Passing these tests does not guarantee that your code will pass the actual homework grader

;; Be sure to put your homework file in the same folder as this test file.
;; Uncomment the line below and change HOMEWORK_FILE to the name of your homework file.
(require "hw4.rkt")

(require rackunit)

;; Helper functions
(define ones (lambda () (cons 1 ones)))
   (define (pows2)
     (define (helpme x)
       (lambda () (cons x (helpme (* x 2)))))
     (helpme 1))

(define a 2)

(define tests
  (test-suite
   "Sample tests for Assignment 4"
   
   ; sequence test
   (check-equal? (sequence 0 5 1) (list 0 1 2 3 4 5) "Sequence test 0 5 1")
   (check-equal? (sequence 0 5 3) (list 0 3) "Sequence test 0 3")
   (check-equal? (sequence -7 5 4) (list -7 -3 1 5) "Sequence test -7 5 4")
   (check-equal? (sequence 3 2 1) (list) "Sequence test 3 2 1")

   ; string-append-map test
   (check-equal? (string-append-map 
       (list "dan" "dog" "curry" "dog2") ".jpg")
           '("dan.jpg" "dog.jpg" "curry.jpg" "dog2.jpg") "string-append-map test 1")
   
   (check-equal? (string-append-map 
       (list) ".jpg") '() "string-append-map test 2")

   ; list-nth-mod test
   (check-equal? (list-nth-mod (list 0 1 2 3 4) 2) 2 "list-nth-mod test 1")
   (check-equal? (list-nth-mod (list 0 1 2 3 4) 4) 4 "list-nth-mod test 2")
   (check-equal? (list-nth-mod (list 0 1 2 3 4) 5) 0 "list-nth-mod test 3")


   
   ; stream-for-n-steps test - problem 4
   (check-equal? (stream-for-n-steps ones 2) (list 1 1) "stream-for-n-steps test 1")
   (check-equal? (stream-for-n-steps (pows2) 4) (list 1 2 4 8) "stream-for-n-steps test 2")
   
   ; funny-number-stream test - problem 5
   (check-equal? (stream-for-n-steps funny-number-stream 16) (list 1 2 3 4 -5 6 7 8 9 -10 11 12 13 14 -15 16) "funny-number-stream test")


   ; dan-then-dog test  question 6
   (check-equal? (stream-for-n-steps dan-then-dog 4)
      (list "dan.jpg" "dog.jpg" "dan.jpg" "dog.jpg") "dan-then-dog test 1")
   (check-equal? (stream-for-n-steps dan-then-dog 0)
      (list) "dan-then-dog test 2")


   ; stream-add-zero test
   (check-equal? (stream-for-n-steps (stream-add-zero ones) 1) (list (cons 0 1))
                 "stream-add-zero t1")
   (check-equal? (stream-for-n-steps (stream-add-zero funny-number-stream) 5)
                 '((0 . 1) (0 . 2) (0 . 3) (0 . 4) (0 . -5)) "stream-add_zero t2")

  
   ; cycle-lists test
   (check-equal? (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 3) (list (cons 1 "a") (cons 2 "b") (cons 3 "a")) 
                 "cycle-lists test")

   (check-equal? (stream-for-n-steps (cycle-lists '(a b c) '(13 12)) 7)
                 '((a . 13) (b . 12) (c . 13) (a . 12) (b . 13) (c . 12) (a . 13)))
   

   ; vector-assoc test
   (check-equal? (vector-assoc 4 (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1))) (cons 4 1) "vector-assoc test")

  
   ; cached-assoc tests
   (check-equal? ((cached-assoc (list (cons 1 2) (cons 3 4)) 3) 3) (cons 3 4)
                 "cached-assoc test")

#|   
   ; while-less test
   (check-equal? (while-less 7 do (begin (set! a (+ a 1)) a)) #t "while-less test")

|#
   ))

(require rackunit/text-ui)

;; runs the test
(run-tests tests)
