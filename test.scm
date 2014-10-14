(use srfi-1)

(define (check-num x)
  (cond [ (= (modulo x 15) 0) 'FizzBuzz]
	[ (= (modulo x 5) 0) 'Buzz]
	[ (= (modulo x 3) 0) 'Fizz]
	[else x]))

(define (fizz-buzz check max)
  (map check (iota max 1)))

(define (fold proc init lis)
  (if (null? lis)
      init
      (fold proc #?=(proc (car lis) init) #?=(cdr lis))))

(define (last-pair lis)
  (if (pair? (cdr lis))
      (last-pair (cdr lis))
      lis))

(define (copy-list lis)
  (if (pair? lis)
      (cons (car lis) 
	    (copy-list (cdr lis)))
      lis))

(define (deep-copy-list lis)
  (if (pair? lis)
      (cons
       (if (pair? (car lis))
	   (deep-copy-list (car lis))
	   (car lis))
       (deep-copy-list (cdr lis)))
      lis))

(define (append2 a b)
  (if (pair? a)
      (cons (car a) (append2 (cdr a) b))
      b))

(define (reverse2 lis)
  (define (reverse-rec lis ret)
    (if (null? lis)
	ret
	(reverse-rec (cdr lis) (cons (car lis) ret))))
  (reverse-rec lis '()))

(define (find pred lis)
  (cond
   [(null? lis) #f]
   [(pred (car lis)) (car lis)]
   [else (find pred (cdr lis))]))

(define (length2 lis)
  (fold (lambda (a b) (+ b 1)) 0 lis))

(define (filter2 pred lis)
  (cond 
   ((null? lis) '())
   ((pred (car lis)) (cons (car lis)
			   (filter2 pred (cdr lis))))
   (else (filter2 pred (cdr lis)))))

(define (max-number lis)
  (if (null? lis)
      (error "max-number needs at least one number")
      (fold (lambda (a b) (if (> a b) a b)) (car lis) (cdr lis))))

(define (print-elements lis)
  (fold (lambda (a b) (print a)) #f lis))

(define (tree-walk walker proc tree)
  (walker (lambda (elt)
	    (if (list? elt)
		(tree-walk walker proc elt)
		(proc elt)))
	  tree))

;;(define (reverse-for-each proc lis)
;;  (for-each proc (reverse lis)))

;;(define (reverse-map proc lis)
;;  (map proc (reverse lis)))

(define (reversed walker)
  (lambda (proc lis)
    (walker proc (reverse lis))))

(define reverse-for-each (reversed for-each))

(define reverse-map (reversed map))

(define (for-each-numbers proc lis)
  (for-each proc 
	    (filter number? lis)))

(define (map-numbers proc lis)
  (map proc
       (filter number? lis)))

(define (numbers-only walker)
  (lambda (proc lis)
    (walker proc (filter number? lis))))

(define (filter-tree proc lis)
  (cond ((null? lis)
	 '())
	((pair? (car lis))
	 (cons (filter-tree proc (car lis)) (filter-tree proc (cdr lis))))
	((proc (car lis)) (cons (car lis) (filter-tree proc (cdr lis))))
	(else (filter-tree proc (cdr lis)))))

(define (numbers-only-for-tree walker)
  (lambda (proc lis)
    (walker proc (filter-tree number? lis))))
