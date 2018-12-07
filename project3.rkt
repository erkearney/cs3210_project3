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

(define (firstScore points)
  (cond
    [ (equal? (cdr points) '()) 0 ]
    [ (+(calcDistance (car points) (car(cdr points))) (firstScore (cdr points))) ]
  )
  )

(define (score points path)
  (pathScore (arrangePath points path))
  )
  

  (define (arrangePath points path)
    (cond
      [ (equal? path '()) '() ]
      [ (cons (list-ref points (- (car path) 1)) (arrangePath points (cdr path)))]
      ))


(define (pathScore points)
  (+ (firstScore points) (lastDist points))
  )

(define (lastDist points)
      (+ (calcDistance (last points) (car points))))

(define (calcDistance pointa pointb)
  (cond
    [ (null? pointa) 0 ]
    [ (null? pointb) 0 ]
    [ (sqrt (+ (expt (-  (car pointb) (car pointa)) 2) (expt (- (car (cdr pointb)) (car (cdr pointa)) ) 2))) ]
  )
  )

(define (etsp points)
  (findOptimal points (genTours (length points)))
)

(define (findOptimal points tours)
  (cond
    [ (equal? (cdr(cdr tours)) '()) '() ]
    [ (findSmallest points (findSmallest points (car tours) (car (cdr tours))) (findOptimal points (cdr (cdr tours))))  ]
    ))
  
(define (findSmallest points tour1 tour2)
  (cond
    [ (equal? tour2 '()) tour1 ]
    [ (if (< (score points tour1) (score points tour2))
         tour1
         tour2
         )
      ]
      )

  )

