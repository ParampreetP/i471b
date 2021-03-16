;;-*- mode: scheme; -*-
;; :set filetype=scheme


;;Return the list resulting by multiplying each element of `list` by `x`.
(define (mul-list list x)
	(if (null? list)
	  '()
	(cons
	  (* x (car list)) (mul-list (cdr list) x))
	)
)

;;Given a proper-list list of proper-lists, return the sum of the
;;lengths of all the top-level contained lists.
(define (sum-lengths list)
  (if (null? list)
    0
    (+ (length(car list)) (sum-lengths (cdr list)))  
  )
)
;;Evaluate polynomial with list of coefficients coeffs (highest-order
;;first) at x.  The computation should reflect the traditional
;;representation of the polynomial.
(define (poly-eval coeffs x)
  (if (null? coeffs)
    0
    (let ([n (- (length coeffs) 1)] [c (car coeffs)])
      (+ (* c (expt x n)) (poly-eval (cdr coeffs) x))
    )  
  )
)

;;Evaluate polynomial with list of coefficients coeffs (highest-order
;;first) at x using Horner's method.
(define (poly-eval-horner coeffs x)
  (if (null? coeffs)
	0
  (letrec ([eval (lambda (intialvalue list)
                    (if (null? list)
                      intialvalue
                      (eval (+ (* intialvalue x) (car list)) (cdr list))))])
    (eval (car coeffs) (cdr coeffs))))
)

;;Return count of occurrences equal? to x in exp
(define (count-occurrences exp x)
 (if (equal? exp x)
    1
    (if (pair? exp)
      (+ (count-occurrences (car exp) x) (count-occurrences (cdr exp) x))
      0
    )
  )
)

;;Return result of evaluating arith expression over Scheme numbers
;;with fully parenthesized prefix binary operators 'add, 'sub, 'mul
;;and 'div.
(define (arith-eval exp)
  '())  ;TODO

;;Given a proper-list list of proper-lists, return sum of lengths of
;;all the contained lists.  Must be tail-recursive.
(define (sum-lengths-tr list)
  '())  ;TODO

;;Evaluate polynomial with list of coefficients coeffs (highest-order
;;first) at x.  Must be tail-recursive.
(define (poly-eval-horner-tr coeffs x)
  '())  ;TODO

;;Return the list resulting by multiplying each element of `list` by `x`.
;;Cannot use recursion, can use one or more of `map`, `foldl`, or `foldr`.
(define (mul-list-2 list x)
	(map (lambda (n)
		(* n x)) list))

;;Given a proper-list list of proper-lists, return the sum of the
;;lengths of all the contained lists.  Cannot use recursion, can use
;;one or more of `map`, `foldl`, or `foldr`.
(define (sum-lengths-2 list)
  	(foldl (lambda (a b) (+ b (length a))) 0 list))


		     
