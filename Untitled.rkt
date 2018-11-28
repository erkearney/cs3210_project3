#lang racket

(define (insertAtFront n list)
  (cond
    [(null? list) '()]
    [ (cons (cons n (car list)) (insertAtFront n (cdr list)))]
  )
  )

(define (revTour x)
  (if (= 1 x)
      '()
     (cons x (revTour(- x 1)))))

(define (reverse x)
  (if(null? x)
    '()
  (append (reverse (cdr x)) (list (car x)))))

(define (genTour x)
  (reverse (revTour x))
  )

(define (genTours n)
  (insertAtFront '1 (permutations (genTour n)))
 )