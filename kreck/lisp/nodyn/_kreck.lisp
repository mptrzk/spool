(defmacro fn (args &body body) 
  `(lambda ,args ,@body))


(defun thread-sub (a b) 
  (subst a '$$ b))

(defmacro $$-> (input &body exprs) 
  (reduce #'thread-sub (cons input exprs)))

($$-> '(1 2 3) 
  (mapcar (fn (x) (+ 1 x)) $$) 
  (reduce #'+ $$)
  (- $$ 4))

(defun maptree (fun tree &optional (apred #'atom)) 
  (if (funcall apred tree) 
      (funcall fun tree) 
      (cons (maptree fun (car tree) apred) 
            (maptree fun (cdr tree) apred))))

(defun pseudo-parse (expr)
  (maptree (fn (a)
             (if (and a (symbolp a))
                 (cons (string-downcase a) nil) 
                 (cons nil a))) 
           expr))

(pseudo-parse '((aa "bb") 12 e))

(defun symp (a) 
  (and (consp a) (atom (car a))))

(symp '(nil . dd))

(defun pseudo-unparse (expr) ;refactor
  (maptree (fn (m) 
             (if (car m) 
                 (let ((name (read-from-string (car m)))) 
                   (if (cdr m) 
                       (cons name
                             (pseudo-unparse (cdr m)))
                       name)) 
                 (cdr m)))
           expr
           #'symp))

(pseudo-unparse (pseudo-parse '((aa "bb") 12 e)))
(pseudo-parse '((aa "bb") 12 e))
 

(defun def-get (deflist str) 
  (if deflist
      (if (equal str (caar deflist)) 
          (car deflist)
          (def-get (cdr deflist) str))))
(def-get '(("a" . 1) ("b" . 2) ("c" . 3)) "b")


(defun sym-expand (sym deflist) 
  (if (car sym)
      (cons (car sym) 
            (cdr (def-get deflist (car sym))))
      sym))

(sym-expand '("b") '(("a" . 1) ("b" . 2) ("c" . 3)))

(defun sexp-expand (expr deflist) 
  (maptree (fn (s) (sym-expand s deflist))
           expr
           #'symp))

(pseudo-parse '((a "b") 12 . d))

(sexp-expand (pseudo-parse '((a "b") 12 . d)) 
             '(("a" . 1) ("b" . 2) ("c" . 3) ("d" . 4)))
;assoc miss?

(defun def-add (deflist name expr)
  (cons (cons name
              (sexp-expand (pseudo-parse expr)
                           deflist)) 
        deflist))


(pseudo-unparse '("a" nil . 1))


(defun strip (expr) 
  (maptree (fn (s)
             (if (car s)
                 (strip (cdr s))
                 (cdr s)))
           expr
           #'symp)) 

($$-> '()
  (def-add $$ "a" 1) 
  (def-add $$ "b" '(a a)) 
  (def-add $$ "c" '(a b b)) 
  (def-get $$ "c") 
  (strip $$))

(defun kreck-evlis (subj forms)
  (mapcar (fn (f) (kreck-eval subj f))
          forms))

(defun kreck-eval (subj form)
  (let ((op (car form)) 
        (args (cdr form)))
    (if (atom op)
        (funcall op subj args)
        (let ((eform (kreck-evlis subj form))) 
          (kreck-eval (cons (cdr eform)
                            (cdar eform))
                      (car eform))))))

(defun subj-op (subj args) 
  subj)

(defun quot-op (subj args) 
  (car args))

(defun car-op (subj args)
  (car (kreck-eval subj (car args))))

(defun cdr-op (subj args)
  (car (kreck-eval subj (car args))))

(defun cons-op (subj args)
  (cons (kreck-eval subj (car args))
        (kreck-eval subj (cadr args))))

(defun eval-op (subj args)
  (kreck-eval (kreck-eval subj (car args))
              (kreck-eval subj (cadr args))))

(defun cond-op (subj args) 
  (if (kreck-eval subj (car args))
      (kreck-eval subj (cadr args))  
      (kreck-eval subj (caddr args))))

(defun list-op (subj args) 
  (kreck-evlis subj args)) 

($$-> '()
  (def-add $$ "$" #'subj-op) 
  (def-add $$ "q" #'quot-op)
  (strip $$))

;parse - read & bind
;expand -> bind
;pseudo-parse -> read

;indenting when (arg1 arg2 &body body)

;TCO 



