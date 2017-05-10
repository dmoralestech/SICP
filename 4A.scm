```
(define deriv-rules
  '(
    ( (dd (?c c) (? v))              0                                 )
    ( (dd (?v v) (? v))              1                                 )
    ( (dd (?v u) (? v))              0                                 )
    ( (dd (+ (? x1) (? x2)) (? v))   (+ (dd (: x1) (: v))
                                        (dd (: x2) (: v)))             )
    ( (dd (* (? x1) (? x2)) (? v))   (+ (* (: x1) (dd (: x2) (: v)))
                                        (* (dd (: x1) (: v)) (: x2)))  )
    ( (dd (** (? x) (?c n)) (? v))   (* (* (: n) (+ (: x) (: (- n 1))))
                                        (dd (: x) (: v)))              )
    ))


(define algebra-rules
’(
( ((? op) (?c e1) (?c e2)) (: (op e1 e2)) )
( ((? op) (? e1) (?c e2)) ((: op) (: e2) (: e1)) )
( (+ 0 (? e)) (: e) )
( (* 1 (? e)) (: e) )
( (* 0 (? e)) 0 )
( (* (?c e1) (* (?c e2) (? e3))) (* (: (* e1 e2)) (: e3)) )
( (* (? e1) (* (?c e2) (? e3))) (* (: e2) (* (: e1) (: e3))) )
( (* (* (? e1) (? e2)) (? e3)) (* (: e1) (* (: e2) (: e3))) )
( (+ (?c e1) (+ (?c e2) (? e3))) (+ (: (+ e1 e2)) (: e3)) )
( (+ (? e1) (+ (?c e2) (? e3))) (+ (: e2) (+ (: e1) (: e3))) )
( (+ (+ (? e1) (? e2)) (? e3)) (+ (: e1) (+ (: e2) (: e3))) )
( (+ (* (?c c) (? a)) (* (?c d) (? a)))
(* (: (+ c d)) (: a)) )
( (* (? c) (+ (? d) (? e))) (+ (* (: c) (: d))
(* (: c) (: e))) )
))

(? x) ;;acts like a variable that can be bound to any expression;
(?c x) ;;can similarly be bound to any numeric constant. Then
(: x) ;;is the expression that was bound to x, and
(: (op x y)) ;;calls the Scheme evaluator (underneath!) with the expressions substituted in for op, x, y.

```
