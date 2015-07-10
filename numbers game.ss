#lang racket

(define atom?
  (lambda (a)
  (not (list? a))))

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

(define o*
  (lambda (m n)
    (cond
      ((zero? n) 0)
      (else (o+ m (o* m (sub1 n)))))))

(define addtup
  (lambda (lat)
    (cond
      ((null? lat) 0)
      ((o+ (car lat) (addtup (cdr lat)))))))

; (addtup '(1 2 3 4 5))

(define tup+  ;; adds the individual tuples and return a tuple
  (lambda (t1 t2)
    (cond
      ((and (null? t1) (null? t2)) '())
      ((null? t1) t2)
      ((null? t2) t1)
      (else (cons (+ (car t1) (car t2))
                  (tup+ (cdr t1) (cdr t2)))))))

; (tup+ '(1 2 3 4 5) '(1 2 3 4 5))
; (tup+ '(1 2 3) '(1 2 3 4 5))
; (tup+ '(1 2 3 4 5) '(1 2 3))

(define o>
  (lambda (m n)
    (cond
      ; since we consider only positive numbers,
      ; if the first number reaches zero then it's
      ; either less than or equal to the first number
      ; not greater.
      ((and (zero? m)) #f)  
      ((and (zero? n)) #t)
      (else (o> (sub1 m) (sub1 n))))))

; (o> 2 3)
; (o> 3 3)
; (o> 3 2)

(define o=
  (lambda (m n)
    (cond
      ((and (zero? m) (zero? n)) #t)
      ((or (and (zero? m) (not (zero? n)))
           (and (zero? n) (not (zero? m)))) #f)
      (else (o= (sub1 m) (sub1 n))))))

; (o= 1 1)
; (o= 1 2)
; (o= 2 1)

(define o<
  (lambda (m n)
    (and (not (o> m n))
         (not (o= m n)))))

; (o< 2 3)
; (o< 3 3)
; (o< 3 2)
      
(define pow
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (o* n (pow n (sub1 m)))))))

; (pow 2 3)
; (pow 2 8)
; (pow 2 0)

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

; (length '())
; (length '(1 2 3 4 5))
; (length '(1 2 (3 4) 5))

(define pick
  (lambda (n lat)
    (cond
      ((eq? n 1) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

; (pick 4 '(a b c d e))

; remove the element at n
(define rempick
  (lambda (n lat)
    (cond
      ((eq? n 1) (cdr lat))
      (else (cons (car lat)
                  (rempick (sub1 n) (cdr lat)))))))

; (rempick 1 '(a b c d e))
; (rempick 4 '(a b c d e))
; (rempick 5 '(a b c d e))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat)
                  (no-nums (cdr lat)))))))

; (no-nums '(a 1 b 1 c 1 d 1 e))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat))
       (cons (car lat)
             (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

; (all-nums '(a 1 b 2 c 3 d 4 e))

(define eqan?
  (lambda (a1 a2)
    (or (and (number? a1)
             (number? a2)
             (= a1 a2))
        (and (atom? a1)
             (atom? a2)
             (eq? a1 a2)))))

; (eqan? 1 2)
; (eqan? 1 1)
; (eqan? 1 'a)
; (eqan? 'a 1)
; (eqan? 'a 'a)

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eq? (car lat) a) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

(occur  'chuck '(how much wood
                 could
                 a wood chuck
                 chuck
                 if a wood chuck 
                 could chuck wood))
