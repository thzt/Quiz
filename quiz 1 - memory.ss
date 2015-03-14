(define (ack m n)
  (cond
    [(= m 0) (+ n 1)]
    [(and (> m 0) (= n 0)) (ack (- m 1) 1)]
    [(and (> m 0) (> n 0) (ack (- m 1) (ack m (- n 1))))]
    [else (error "invalid arguments with")]))

(define (add parameter-list new-value env)
	(cons (cons parameter-list new-value) env))
	
(define (find parameter-list env)
    (if (null? env)
        (cons #f #f)
		(let* [(item (car env))
			   (item-parameter-list (car item))
			   (item-old-value (cdr item))]
			  (if (match parameter-list item-parameter-list)
				  (cons #t item-old-value)
				  (find parameter-list (cdr env))))))

(define (match list-a list-b)
    (cond
        [(and (null? list-a) (null? list-b)) #t]
        [(not (= (length list-a) (length list-b))) #f]
        [(not (= (car list-a) (car list-b))) #f]
        [else (match (cdr list-a)(cdr list-b))]))
		
(define (memory fn)
	(let [(env '())]
		(lambda parameter-list
			(let* [(result (find parameter-list env))
				   (exist? (car result))
				   (old-value (cdr result))]
				(if exist?
					old-value
					(let [(new-value (apply fn parameter-list))]
						(set! env (add parameter-list new-value env))
						new-value)))
			)))

(time (ack 3 9))

(set! ack (memory ack))

(time (ack 3 9))

