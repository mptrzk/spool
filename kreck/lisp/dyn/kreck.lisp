(defparameter dbg nil)

(defmacro fn (args &body body) 
  `(lambda ,args ,@body))

(defun thread-sub (a b) 
  (subst a '$$ b))

(defmacro $$-> (input &body exprs) 
  (reduce #'thread-sub (cons input exprs)))

(defun test-1 (expr) 
  `(if ,expr 
       t 
       (progn (format t "failed: ~s~%" (quote ,expr))
              nil)))

(defmacro test (name &body exprs) 
  `(if (and ,@(mapcar #'test-1 exprs)) 
       (progn (if ,name
                  (format t "~a tests passed!~%" ,name))
              t)))

(test nil
  (= ($$-> '(1 2 3) 
       (mapcar (fn (x) (+ 1 x)) $$) 
       (reduce #'+ $$)
       (- $$ 4))
     5))


(defun kreck-search (subj key)
  (cond ((atom subj) nil)
        ((equal (car subj) key) subj)
        (t (or (kreck-search (car subj) key)
               (kreck-search (cdr subj) key)))))
(test "kreck-search"
  (equal (kreck-search '() "a") nil)
  (equal (kreck-search '("a" 1 2 3) "a") '("a" 1 2 3)) 
  (equal (kreck-search '(("a" 1 2) 3) "a") '(("a" 1 2) 3))) 

(defun kreck-expand (subj key)
  (let ((res (kreck-search subj key)))
        (if res
            (cdr res)
            (format t "key ~s not found in ~s~%"
                    key ;^^dump to error log
                    subj))))

(defun kreck-evlis (subj forms)
  (mapcar (fn (f) (kreck-eval subj f))
          forms))


(defun kreck-eval (subj form)
  (if (atom form)
      (kreck-expand subj form)
      (let ((op (kreck-expand subj (car form)))
            (args (cdr form)))
        (if (atom op)
            (funcall op subj args)
            (let ((eform (kreck-evlis subj form))) 
              (kreck-eval (cons (cdr eform)
                                (cdar eform))
                          (caar eform)))))))


(defun quot-op (subj args) 
  (car args))

(defun car-op (subj args)
  (car (kreck-eval subj (car args))))

(defun cdr-op (subj args)
  (cdr (kreck-eval subj (car args))))

(defun cons-op (subj args)
  (cons (kreck-eval subj (car args))
        (kreck-eval subj (cadr args))))

(defun eval-op (subj args)
  (kreck-eval (kreck-eval subj (car args))
              (kreck-eval subj (cadr args))))

(defun if-op (subj args) 
  (if (kreck-eval subj (car args))
      (kreck-eval subj (cadr args))  
      (kreck-eval subj (caddr args))))

(defun list-op (subj args) 
  (kreck-evlis subj args)) 

(defparameter *defs* `("subj"
                       ("q" . ,#'quot-op)  
                       ("<" . ,#'car-op)  
                       (">" . ,#'cdr-op)  
                       ("c" . ,#'cons-op)  
                       ("*" . ,#'eval-op)
                       ("?" . ,#'if-op)
                       ("l" . ,#'list-op)))

(defun ploo (expr)
  (cond ((null expr) nil)
        ((symbolp expr) (string-downcase expr))
        ((consp expr) (cons (ploo (car expr))
                            (ploo (cdr expr))))
        (t expr)))


(defun kreck (subj form)
  (kreck-eval (cons (ploo subj) *defs*) (ploo form)))

(kreck '(1 2 3) '(< subj))


