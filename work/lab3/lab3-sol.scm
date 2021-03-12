(define (qr a b c) (list 
			(/ (+ (- 0 b) (sqrt(- (expt b 2) (* 4 a c))))(* 2 a))
			(/ (- (- 0 b) (sqrt(- (expt b 2) (* 4 a c)))) (* 2 a)) 
			))
(define quadratic-roots
	(lambda (a b c (sqrt-fn sqrt))
	(if (= a 0)
		'error
	(let ([x (sqrt-fn (- ( * b b) (* 4 a c)))])
		(list (/ (+ (- 0 b) x) (* 2 a))
		(/ (- (- b) x) (* 2 a)))))))

(define my-sqrt
	(lambda (n (x 1.0))
		(if (< (abs (/ (- (* x x) n) n)) 0.0001)
		x
		(my-sqrt n (/ (+ x (/ n x)) 2)))))



(define greater-than
	(lambda (ls (n 0))
	(if (null? ls) 
	'()
	(if (< (car ls) n) 
		(append '("#f") (greater-than (cdr ls) n))
		(append '("#t") (greater-than (cdr ls) n))
))))


(define less-than
	(lambda (ls (n 0))
	(if (null? ls) 
	'()
	(if (> (car ls) n) 
		(append '("#f") (less-than (cdr ls) n))
		(append '("#t") (less-than (cdr ls) n))
))))

(define get-greater-than
	(lambda (ls (n 0))
		(if (null? ls)
		'()
		(append
		  (list
		    (when (> (car ls) n)
			(car ls)
		    )
		)
		(get-greater-than (cdr ls) n)
		)
	)
	)
)

(define get-less-than
	(lambda (ls (n 0))
		(if (null? ls)
		'()
		(append
		  (list
		    (when (< (car ls) n)
			(car ls)
		    )
		)
		(get-less-than (cdr ls) n)
		)
	)
	)
)

