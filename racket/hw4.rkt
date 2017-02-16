
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file


#| 1. function sequence takes 3 arguments low, high, and stride, all
assumed to be numbers. Assumes stride is positive.  Produces a list of
numbers from low to high (including low and possibly high) separated by
stride and in sorted order. |#

(define (sequence low high stride)
    (if (> low high)
        '()
        (cons low (sequence (+ low stride) high stride ))))

#| 2. function string-append-map takes a list of strings xs and a string
suffix and returns a list of strings. Each element of the output should
be the corresponding element of the input appended with suffix (with no
extra space between the element and suffix). You must use Racket-library
functions map and string-append. Sample solution: 2 lines. |#

(define (string-append-map xs suffix)
   (map (lambda (xs) (string-append xs suffix)) xs))


#| 3. function list-nth-mod takes a list xs and a number n. If the
number is negative, terminates the computation with (error
"list-nth-mod: negative number"). Else if the list is empty, terminate
the computation with (error "list-nth-mod: empty list"). Else return the
ith element of the list where we count from zero and i is the remainder
produced when dividing n by the list’s length. Library functions length,
remainder, car, and list-tail are all useful – see the Racket
documentation. Sample solution is 6 lines. |#

(define (list-nth-mod xs n)
  (cond [(< n 0)      (error "list-nth-mod: negative number")]
        [(empty? xs)  (error "list-nth-mod: empty list")]
        [#t           (car (list-tail xs (remainder n (length xs))))]))

#| 4. function stream-for-n-steps takes a stream s and a number n. It
returns a list holding the first n values produced by s in order. Assume
n is non-negative. Sample solution: 5 lines. Note: You can test your
streams with this function instead of the graphics code. |#
(define (stream-for-n-steps s n)
  (if (= 0 n)
      '()
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))
   
#| 5. funny-number-stream is like the stream of natural
numbers (i.e., 1, 2, 3, ...) except numbers divisble by 5 are negated
(i.e., 1, 2, 3, 4, -5, 6, 7, 8, 9, -10, 11, ...). Remember a stream is a
thunk that when called produces a pair. Here the car of the pair will be
a number and the cdr will be another stream. |#

(define (funny-number-stream)
  (define (f x)
    (lambda () (cons (if (= 0 (remainder x 5)) (- x) x) (f (+ 1 x)))))
  ((f 1)))

;(stream-for-n-steps funny-number-stream 16)

#| 6. dan-then-dog: the elements of the stream alternate between the
strings "dan.jpg" and "dog.jpg" (starting with "dan.jpg"). More
specifically, dan-then-dog should be a thunk that when called produces a
pair of "dan.jpg" and a thunk that when called produces a pair of
"dog.jpg" and a thunk that when called... etc. |#

(define (dan-then-dog)
  (define (f jpg)
     (lambda () (cons jpg (f (if (string=? jpg "dan.jpg") "dog.jpg" "dan.jpg")))))
  ((f "dan.jpg")))

;(stream-for-n-steps dan-then-dog 5)

#| ---------------------------------------------------------------
7. stream-add-zero takes a stream s and returns another stream. If s
would produce v for its ith element, then (stream-add-zero s) would
produce the pair (0 . v) for its ith element.
 ----------------------------------------------------------------|#

(define (stream-add-zero s)
  (define (f s)
    (cons (cons 0 (car (s))) (lambda () (f (cdr (s))))))
    (lambda () (f s)))

; (stream-for-n-steps (stream-add-zero funny-number-stream) 5)

#| ---------------------------------------------------------------------
8. cycle-lists takes two lists xs and ys and returns a stream. The lists
may or may not be the same length, but assume they are both non-empty.
The elements produced by the stream are pairs where the first part is
from xs and the second part is from ys. The stream cycles forever
through the lists. For example, if xs is ’(1 2 3) and ys is ’("a" "b"),
then the stream would produce, (1 . "a"), (2 . "b"), (3 . "a"), (1 .
"b"), (2 . "a"), (3 . "b"), (1 . "a"), (2 . "b"), etc.

Sample solution is 6 lines and is more complicated than the previous
stream problems. Hints: Use one of the functions you wrote earlier. Use
a recursive helper function that takes a number n and calls itself with
(+ n 1) inside a thunk.
   ------------------------------------------------------------------ |#
