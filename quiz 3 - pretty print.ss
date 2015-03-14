(define-record-type E
	(fields
		w h	c v))
	
	
(define (display-v v)
	(if (not (null? v))
		(begin (display (car v))
			(display "\n")
			(display-v (cdr v)))))
	
	
(define (display-e e)
	(display-v (E-v e)))
	
	
(define (extend-x e w)
	(let* [(w0 (E-w e))
		(h0 (E-h e))
		(c0 (E-c e))
		(v0 (E-v e))
		(a (/ (- w w0) 2))
		(s (make-string a #\ ))]
		(make-E w h0 c0
			(map (lambda (x)
				(string-append s x s))
				v0))))

				
(define (extend-y e h c)
	(let* [(w0 (E-w e))
		(h0 (E-h e))
		(c0 (E-c e))
		(v0 (E-v e))
		(a (- c c0))
		(b (- h h0
			(- c c0)))
		(z (make-string w0 #\ ))
		(x (make-list a z))
		(y (make-list b z))]
		(make-E w0 h c
			(append x v0 y))))
		
		
(define (integrate-x e1 e2 ch)
	(let* [(w1 (E-w e1))
		(h1 (E-h e1))
		(c1 (E-c e1))
		(v1 (E-v e1))
		(w2 (E-w e2))
		(h2 (E-h e2))
		(c2 (E-c e2))
		(v2 (E-v e2))
		(vec (make-vector h1 
			(make-string 3 #\ )))]
		
		(vector-set! vec 
			(- c1 1) 
			(string-append " " ch " "))
			
		(let [(v (vector->list vec))]
			(make-E (+ w1 w2 3) h1 c1
				(do ([cur-v1 v1 (cdr cur-v1)]
					[cur-v v (cdr cur-v)]
					[cur-v2 v2 (cdr cur-v2)]
					[r '() (append r 
						(list (string-append (car cur-v1)
							(car cur-v)
							(car cur-v2))))])
					((null? cur-v1) r))))))
	
	
(define (integrate-y e1 e2)
	(let* [(w1 (E-w e1))
		(h1 (E-h e1))
		(c1 (E-c e1))
		(v1 (E-v e1))
		(w2 (E-w e2))
		(h2 (E-h e2))
		(c2 (E-c e2))
		(v2 (E-v e2))
		(row (make-string w1 #\-))]
		(make-E w1 (+ h1 h2 1) (+ h1 1)
			(append v1 (list row) v2))))
		
	
(define (combine-x e1 e2 ch)
	(let* [(w1 (E-w e1))
		(h1 (E-h e1))
		(c1 (E-c e1))
		(v1 (E-v e1))
		(w2 (E-w e2))
		(h2 (E-h e2))
		(c2 (E-c e2))
		(v2 (E-v e2))
		(ext-h (+ (max c1 c2) (max (- h1 c1) (- h2 c2))))
		(ext-c (max c1 c2))
		(ext-e1 (extend-y e1 ext-h ext-c))
		(ext-e2 (extend-y e2 ext-h ext-c))]
		
		(integrate-x ext-e1 ext-e2 ch)))
	
	
(define (combine-y e1 e2)
	(let* [(w1 (E-w e1))
		(h1 (E-h e1))
		(c1 (E-c e1))
		(v1 (E-v e1))
		(w2 (E-w e2))
		(h2 (E-h e2))
		(c2 (E-c e2))
		(v2 (E-v e2))
		(ext-w (max w1 w2))
		(ext-e1 (extend-x e1 ext-w))
		(ext-e2 (extend-x e2 ext-w))]
		
		(integrate-y ext-e1 ext-e2)))
		
		
(define (combine-ys es)
	(let* [(a (car es))
		(bs (cdr es))]
		(if (null? bs)
			a
			(combine-y a (combine-ys bs)))))
	
	
(define (combine-xs es ch)
	(let* [(a (car es))
		(bs (cdr es))]
		
		(if (null? bs)
			a
			(combine-x a (combine-xs bs ch) ch))))
		

(define (s->e s)
	(if (atom? s)
		(make-E 1 1 1 (list (number->string s)))
		(let* [(ch (car s))
			(ss (cdr s))]
			
			(if (eq? ch '/)
				(combine-ys (map s->e ss))
				(combine-xs (map s->e ss) (symbol->string ch))))))

(define p '(+ 1
               (/ 1
                  (* (* 2 (/ (/ 3 3) (+ (+ 1 (/ 4 (+ (+ 1 5) 8))) (- (/ (/ 1 2) 3) (/ 1 (/ 2 3)))))) (+ (/ 4
                        (+ 5 (/ 9
                              (+ 7
                                 (/ 6
                                    (* (+ 8 5) 3))))))
                     (/ 9 1))))))
			
(display-e (s->e p))

;todo -- add parentheses to hold the priority of computation.
;todo -- support symbol in input s-exp, support multi-digit number or symbol
;todo -- the fraction line should be wider.

