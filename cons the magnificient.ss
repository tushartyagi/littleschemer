#lang racket

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

; (rember 'a '())
; (rember 'b '(a a a))

(define firsts
  (lambda (l) ;; l is a list of lists (which may be empty)
    (cond
      ((null? l) (quote ()))
      (else (cons (car (car l))
                  (firsts (cdr l)))))))

;(firsts '((a b c) (1 2 3) (4 5 6)))
;(firsts '())
;(firsts '((a b c) ((no) 2 3) (4 (more) 6)))

(define insertR
  (lambda (new old lat) ;; inserts new to the right of the first occurance of old
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons (car lat) (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))
    