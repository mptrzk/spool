(define tokens '("[" "~" "~" "~" "]"))
(car tokens)

(define (parse tokens)
	(if (eqv? (car tokens) "[")
			(parse-list (cdr tokens))
			(if (atom? token)

(parse tokens)
