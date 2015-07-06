#lang racket

;; This needs to be defined here, since it
;; will be used a lot.
(define atom?
  (lambda (l)
    (not (list? l))))
      

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))


(define member?
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((or (eq? a (car l))
           (member? a (cdr l)))))))

(member? 'tea '(coffee tea or milk))