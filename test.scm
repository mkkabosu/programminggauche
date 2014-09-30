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
  (define (length-rec lis n)
    (if (null? lis)
	n
	(length-rec (cdr lis) (+ n 1))))
  (length-rec lis 0))

(define (filter2 pred lis)
  (cond 
   ((null? lis) '())
   ((pred (car lis)) (cons (car lis)
			   (filter2 pred (cdr lis))))
   (else (filter2 pred (cdr lis)))))
