(defparameter dbg nil)

(defmacro fn (args &body body) 
  `(lambda ,args ,@body))

(defun thread-sub (a b) 
  (subst a '$$ b))

(defmacro $$-> (input &body exprs) 
  (reduce #'thread-sub (cons input exprs)))


;macrolet & stuff?
(defun test-1 (expr) 
  `(if ,expr 
       t 
       (progn ,(unless (member (car expr) *test-macros*)
                   `(format t "failed: ~s~%" (quote ,expr))) 
              nil)))

(defmacro test (name &body exprs) 
  `(and ,@(mapcar #'test-1 exprs)))

(defmacro test-equal (name &body exprs)
  `(test ,name
     ,@(mapcar (fn (e)
                 (cons 'equal e)) 
               exprs)))


(test nil
  (= ($$-> '(1 2 3) 
       (mapcar (fn (x) (+ 1 x)) $$) 
       (reduce #'+ $$)
       (- $$ 4))
     5))



;query

(defun entryp (expr)
  (and (consp expr)
       (consp (cdr expr))))

(defun recordp (expr)
  (and (entryp expr) (cadr expr)))

(defun tablep (expr)
  (and (entryp expr) (not (cadr expr))))
;assume validated?

;where validate?
(defun kreck-query (entry key)
  (cond ((not (entryp entry)) nil) ;error?
        ((equal (car entry) key)
         entry)
        ((tablep entry)
         (mapcar #'kreck-query (cddr entry))))) ;not mapcar!
;query-table?
 

(defun kreck-query-val (subj key)
  (if (atom key)
      (let ((res (kreck-query subj key)))
        (if res
            (cdr res)
            (format t "key ~s not found in ~s~%"
                    key ;^^dump to error log
                    subj)))
      key))

;kreck
(defun kreck-evlis (subj forms)
  (mapcar (fn (f) (kreck-eval subj f))
          forms))

(defun kreck-apply (subj op args)
  (if (atom op)
      (funcall op subj args)
      (let ((clos (kreck-eval subj op))) 
        (kreck-eval (cons args
                          (cdr clos))
                    (car clos)))))

(defun kreck-eval (subj form)
  (if (atom form)
      (kreck-query-val subj form)
      (kreck-apply subj
                   (kreck-eval subj (car form))
                   (cdr form))))


;exops

(defun subj-op (subj args) 
  subj)

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

(defun query-op (subj args) 
  (kreck-query subj (kreck-eval subj (car args))))


;defs

(defparameter *defs* `("defs"
                       ("$" . ,#'subj-op)
                       ("q" . ,#'quot-op)  
                       ("<" . ,#'car-op)  
                       (">" . ,#'cdr-op)  
                       ("c" . ,#'cons-op)  
                       ("*" . ,#'eval-op)
                       ("?" . ,#'if-op)
                       ("l" . ,#'list-op)
                       ("@" . ,#'query-op)))

(defun ploo (expr)
  (cond ((null expr) nil)
        ((symbolp expr) (string-downcase expr))
        ((consp expr) (cons (ploo (car expr))
                            (ploo (cdr expr))))
        (t expr)))


(defun kreck (subj form)
  (kreck-eval (cons (ploo subj) *defs*) (ploo form)))


;tests

(test "all" ;no "in all"?
  (test "entry-predicates"
    (not (entryp "a"))
    (not (entryp '("a")))
    (entryp '("a" ()))
    (entryp '("a" () "foo"))
    (tablep '("a" () "foo"))
    (not (tablep '("a" "t" "foo")))
    (not (recordp '("a" () "foo")))
    (recordp '("a" "t" "foo"))
    )
  (test-equal "kreck-query"
    ((kreck-query "t") )))

