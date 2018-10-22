#lang racket/base

(require racket/vector
	 racket/list)

(define (vector-norm vec)
  (sqrt
   (for/fold ([sum 0]) ([x (vector->list vec)])
     (+ sum (* x x)))))

(define (normalize vec)
  (let [(norm (vector-norm vec))]
    (vector-map (lambda (x) (/ x norm)) vec)))

(define (random-gradient dim)
  (let [(vec (for/vector [(_ (in-range dim))] (random)))]
    (normalize vec)))

(define (gradient-grid dim n)
  (for/vector [(_ (in-range n))]
    (for/vector [(_ (in-range n))]
      (random-gradient dim))))

(define (dot u v)
  (apply + (vector->list (vector-map * u v))))

(define (neighbours point)
  (let [(n (vector-length point))
	(hypercube (vector->list
		    (vector-map
		     (lambda (x) (inexact->exact (floor x))) point)))]
    (remove-duplicates
     (for*/list [(i (in-range (+ n 1)))
		 (l (in-permutations
		     (build-list n (lambda (j) (if (< j i) 1 0)))))]
       (map + hypercube l)))))

(define (perlin-noise point grads)
  (for/list [(n (neighbours point))]
    (dot (vector-map - point n) (vector-ref (vector-ref grads (cadr n)) (car n)))))
