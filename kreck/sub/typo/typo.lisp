(defmacro fn (args &body body)
  `(lambda ,args ,@body))

(defun test-1 (expr) 
  `(if ,expr 
       t 
       (progn (format t "failed: ~a~%" (quote ,expr))
              nil)))

(defmacro test (name &body exprs) 
  `(if (and ,@(mapcar #'test-1 exprs)) 
       (progn (format t "~a tests passed!~%" ,name)
              t)))

(defun andf (&rest args) 
  (and args))

(defun all (lst) 
  (reduce (fn (x y) (and x y)) lst))



(defun wff-check (expr) nil)

(defun type-check (expr) 
  (if (symbolp expr)
      expr 
      (let ((op (car expr)) 
            (args (cdr expr))) 
        (if (and (listp op)
                 (eq (car op) '->)) 
            (if (all (mapcar #'andf (cdr op) args)) 
                (if (symbolp (cadr op))
                    (cdar op)
                    'not-wff)) 
            'not-wff))))

(test "all"
  (test "wff-check" 
    (eq (wff-check 'a) t) 
    (eq (wff-check '(-> b)) t) 
    (eq (wff-check '(a b)) nil)) ;TODO tree backtrace
  (test "type-check" 
    (eq (type-check 'a) 'a) 
    (eq (type-check '(-> a b)) '(-> a b)) 
    (eq (type-check '(&& a b)) '(&& a b))
    (eq (type-check '(|| a b)) '(|| a b)))) 


