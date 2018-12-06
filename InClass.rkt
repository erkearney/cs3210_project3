#lang racket

; return a list consisting of all the guys in items that are bigger than lower
(define (find-bigger lower items)
  (if (equal? items '() )
      '()
      (if (> (car items) lower)
          (cons (car items) (find-bigger lower (cdr items)))
          (find-bigger lower (cdr items))
  )
      ))

(define (sort-neighbors items)
  (if (equal? items '())
      '()
      (if (< (car items) (car (cdr items))))
         (cons (car items) (cons (car (cdr items))
               (sort-neighbors (cdr (cdr items))))
         (cons (second items) (cons (car items)
                                 (sort-neighbors (cddr items))))
  )
  )
  