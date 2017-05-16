2.2
```
(list <a1> <a2> ... <an>) is same as (cons <a1> (cons <a2> (cons ... (cons <an> nil) ...)))

(define one-through-four (list 1 2 3 4))

one-through-four
;;(1 2 3 4)

(car one-through-four)
;;1

(cdr one-through-four)
;;(2 3 4)

(car (cdr one-through-four))
;;2

(cons 10 one-through-four)
;;(10 1 2 3 4)

(cons 5 one-through-four)
;;(5 1 2 3 4)

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
      
(define squares (list 1 4 9 16 25))

(list-ref squares 3)
;;16

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
      
(define odds (list 1 3 5 7))

(length odds)
;;4

(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))
  
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;;Ex 2.17
(define (last-pair items) 
 (let ((rest (cdr items))) 
   (if (null? rest) 
       items 
       (last-pair rest)))) 
       
;;2.18
(define (reverse items) 
 (define (iter items result) 
   (if (null? items) 
       result 
       (iter (cdr items) (cons (car items) result)))) 

 (iter items nil)) 

(define nil '()) 
;; less efficient
(define (reverse items) 
 (if (null? (cdr items)) 
     items 
     (append (reverse (cdr items)) 
             (cons (car items) nil)))) 
  
;;2.19
 (define (first-denomination denominations) (car denominations)) 
 (define (except-first-denom denominations) (cdr denominations)) 
 (define (no-more? denominations) (null? denominations)) 
  
 (define (cc amount denominations) 
   (cond  

    ((= amount 0) 1) 
     
    ((or (< amount 0) (no-more? denominations)) 0) 
     
    (else 
     (+ (cc amount (except-first-denom denominations)) 
        (cc (- amount  
               (first-denomination denominations))  
            denominations))))) 
```

In general, the underlying idea of data abstraction is to identify for each type of data object a basic set of operations in terms of which all manipulations of data objects of that type will be expressed, and then to use only those operations in manipulating the data.
```
 (define zero (lambda (f) (lambda (x) x))) 
 
 (define (add-1 n) 
   (lambda (f) (lambda (x) (f ((n f) x))))) 

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
      
(define odds (list 1 3 5 7))


(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))


(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;; 2.3.2      
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))
         
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum-old a1 a2) (list '+ a1 a2))

(define (make-product-old m1 m2) (list '* m1 m2))

(define (=number? exp num)
  (and (number? exp) (= exp num)))


(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))    

;; 2.3.3 Representing Sets
;;(define (element-of-set? x set)
;;  (cond ((null? set) false)
;;        ((equal? x (car set)) true)
;;        (else (element-of-set? x (cdr set)))))
        
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))
        
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))
      
;;(define (intersection-set set1 set2)¥
;; (cond ((or (null? set1) (null? set2)) '())
;;        ((element-of-set? (car set1) set2)        
;;         (cons (car set1)
;;               (intersection-set (cdr set1) set2)))
;;        (else (intersection-set (cdr set1) set2))))
;;        

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()    
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))
 
;; binary tree
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
  
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))
                    
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))
 
 ;; Ex 2.64
 (define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))
                      
 (define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))
        
;; Huffman encoding
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
  
(define (leaf? object)
  (eq? (car object) 'leaf))
  
(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))
```
