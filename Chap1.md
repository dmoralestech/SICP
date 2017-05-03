```
(define identity (lambda (x) x))


(define (square x)
  (* x x))
      

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (cond ((zero? n) identity)
        (else (compose f (repeated f (- n 1))))))

((repeated square 3) 2)
```
