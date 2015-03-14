(define (reduce-right op init seq)
	(if (null? seq)
		init
		(op (car seq)
			(reduce-right op init (cdr seq)))))

;Part 1
(define (map p seq)
	(reduce-right (lambda (x y)
				(cons (p x) y))
		'()
		seq))

(define (append seq1 seq2)
	(reduce-right cons seq2 seq1))

(define (length seq)
	(reduce-right (lambda (x y)
				(+ 1 y))
		0
		seq))

;Part 2
(define (reduce-left op init seq)
	((reduce-right (lambda (x fn)
				(lambda (a)
		(fn (op a x))))
		(lambda (x) x)
		seq)
	init))

;Part 3
(define (reduce-operator fn init flag)
	(let [(reduce (if (eq? flag 'right) reduce-right reduce-left))]
		(lambda x
			(reduce fn init x))))

(define list (reduce-operator cons '() 'right))
