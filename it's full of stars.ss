#lang racket

; ((coffee) cup ((tea) cup) (and (hick)) cup)

(define atom?
  (lambda (a)
  (not (list? a)))) 

(define rember*
  (lambda (a lat)
    (cond
      ; the car can be either an atom or a list
      ((null? lat) (quote ()))
      ((atom? (car lat))
       (cond
         ((eq? a (car lat))
          (rember* a (cdr lat)))
         (else (cons (car lat)
                     (rember* a (cdr lat))))))
      (else (cons (rember* a (car lat))
                  (rember* a (cdr lat)))))))

; (rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))

(define insertR*
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((atom? (car lat))
       (cond
         ((eq? (car lat) old)
          (cons old 
                (cons new
                      (insertR* new old (cdr lat)))))
         ; Made a mistake here by not using cons,
         ; I was simply using insertR* like this:
         ; (else (insertR* new  old (cdr lat)))))
         (else (cons (car lat)
                     (insertR* new old (cdr lat))))))
      (else (cons (insertR* new old (car lat))
                  (insertR* new old (cdr lat)))))))

(insertR* 'roast 'chuck '((how much (wood))
                          could
                          ((a(wood)chuck) )
                          (((chuck)))
                          (if(a)((wood chuck)))
                          could chuck wood))