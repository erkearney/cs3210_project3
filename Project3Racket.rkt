#lang racket
 
(define (genTours n)
  (cond
    [ (equal? n 1) 1]
    [#t (addToList (list n) (genTours (- n 1)))]
    )
)


(define (addToList list n)
  (append list n)
  )

(define (revlist list)
    (if (null? list)
        null
        (myappend (car list) (revlist (cdr list)))
        )
  )

(define (myappend x list)
  (write list)
  (if (null? list)
      (cons x '())
      (cons (car list) (myappend x (cdr list)))
      )
  )