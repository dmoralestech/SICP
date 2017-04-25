In general, the underlying idea of data abstraction is to identify for each type of data object a basic set of operations in terms of which all manipulations of data objects of that type will be expressed, and then to use only those operations in manipulating the data.

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
      
