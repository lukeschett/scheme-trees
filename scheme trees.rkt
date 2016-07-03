(define (make-tree value left right) (list value left right))
(define (value tree) (car tree))
(define (left tree) (cadr tree))
(define (right tree) (caddr tree))

;Check if trees are equal
(define (tree-equal? T1 T2)
  (if (or (null? T1) (null? T2))
      (if (and (null? T1) (null? T2))
          #t
          #f)
      (if (= (value T1) (value T2))
          (if (and (tree-equal? (left T1) (left T2))
                   (tree-equal? (right T1) (right T2)))
              #t
              #f)
          #f)))

;Insert a bnumber into a tree
(define (insert tree number)
  (if (null? tree)
      (make-tree number '() '())
      (cond ((< number (value tree)) (list (value tree) (insert (left tree) number) (right tree)))
            ((> number (value tree)) (list (value tree) (left tree) (insert (right tree) number)))
            ((= number (value tree)) tree))))

;Insert a list into a tree
(define (insert-list tree L)
  (if (null? L)
      tree
      (insert-list (insert tree (car L)) (cdr L))))
(define (extract-sorted tree)
  (if (null? tree)
      '()
      (append (extract-sorted (left tree))
              (list (value tree))
              (extract-sorted (right tree)))))
(define (tree-sort L)
  (extract-sorted (insert-list '() L)))
 
;Delete a node from a tree
(define (delete-node tree v)
  (if (null? tree)
      '()
      (if (= (value tree) v)
          (cond ((and (null? (left tree)) (null? (right tree))) '())
                ((null? (left tree)) (right tree))
                ((null? (right tree)) (left tree))
                ((not (and (null? (left tree)) (null? (right tree))))
                 (make-tree (value (right tree))
                                 (left tree)
                                 (delete-node (right tree) (value (right tree))))))
          (if (< v (value tree))
              (make-tree (value tree) (delete-node (left tree) v) (right tree))
              (make-tree (value tree) (left tree) (delete-node (right tree) v))))))
  

;Parse tree using operations
(define (parse-tree tree)
  (cond ((eq? (value tree) #\+) (+ (parse-tree (left tree))
                                   (parse-tree (right tree))))
        ((eq? (value tree) #\*) (* (parse-tree (left tree))
                                   (parse-tree (right tree))))
        ((eq? (value tree) #\/) (/ 1 (parse-tree (left tree))))
        ((eq? (value tree) #\-) (* -1 (parse-tree (left tree))))
        ((and (null? (left tree)) (null? (right tree))) (value tree))))

;Parse Polish tree
(define (prepare x)
  (cond ((number? x) (number->string x))
        ((char? x) (string x))))
(define (parse-polish tree)
  (if (null? tree)
      ""
      (string-append (prepare (value tree))
                     (parse-polish (left tree))
                     (parse-polish (right tree)))))

;Parse RPN tree
(define (parse-rpn tree)
  (if (null? tree)
      ""
      (string-append (parse-rpn (left tree))
                     (parse-rpn (right tree))
                     (prepare (value tree)))))