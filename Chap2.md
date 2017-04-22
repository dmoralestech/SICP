In general, the underlying idea of data abstraction is to identify for each type of data object a basic set of operations in terms of which all manipulations of data objects of that type will be expressed, and then to use only those operations in manipulating the data.

 (define zero (lambda (f) (lambda (x) x))) 
 
 (define (add-1 n) 
   (lambda (f) (lambda (x) (f ((n f) x))))) 
