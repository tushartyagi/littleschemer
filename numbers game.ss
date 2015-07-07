#lang racket

(define o+
  (lambda (m n)
    (cond
      ((zero? m) n)
      (else (add1 (o+ (sub1 m) n))))))


(define o-
  (lambda (m n)
    (cond
      ((zero? n) m)
      (else (sub1 (o- m (sub1 n)))))))