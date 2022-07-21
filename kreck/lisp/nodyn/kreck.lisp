(defparameter dbg nil)

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

(defun kreck-evlis (subj forms)
  (mapcar (fn (f) (kreck-eval subj f))
          forms))

(defun kreck-eval (subj form)
  (if dbg (format t "subj: ~s~%form: ~s~%~%" subj form))
  (let ((op (car form)) 
        (args (cdr form)))
    (if (atom op)
        (funcall op subj args)
        (let ((eform (kreck-evlis subj form))) 
          (kreck-eval (cons (cdr eform)
                            (cdar eform))
                      (caar eform))))))

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

(defun cond-op (subj args) 
  (if (kreck-eval subj (car args))
      (kreck-eval subj (cadr args))  
      (kreck-eval subj (caddr args))))

(defun list-op (subj args) 
  (kreck-evlis subj args)) 

(defun add2-op (subj args) 
  (+ (kreck-eval subj (car args))
     (kreck-eval subj (cadr args))))

(defun kreck (subj form deflist) 
  (kreck-eval (parse subj deflist) (parse form deflist)))



(defun maptree (fun tree &optional (apred #'atom)) 
  (if (funcall apred tree) 
      (funcall fun tree) 
      (cons (maptree fun (car tree) apred) 
            (maptree fun (cdr tree) apred))))

(defun sexp-read (expr)
  (maptree (fn (a)
             (if (and a (symbolp a))
                 (cons (string-downcase a) nil) 
                 (cons nil a))) 
           expr))

(sexp-read '((aa "bb") 12 e))

(defun symp (a) 
  (and (consp a) (atom (car a))))

(symp '(nil . dd))

(defun sexp-unread (expr) ;refactor
  (maptree (fn (m) 
             (if (car m) 
                 (let ((name (read-from-string (car m)))) 
                   (if (cdr m) 
                       (cons name
                             (sexp-unread (cdr m)))
                       name)) 
                 (cdr m)))
           expr
           #'symp))

(sexp-unread (sexp-read '((aa "bb") 12 e)))
(sexp-read '((aa "bb") 12 e))
 

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

(sexp-read '((a "b") 12 . d))

(sexp-expand (sexp-read '((a "b") 12 . d)) 
             '(("a" . 1) ("b" . 2) ("c" . 3) ("d" . 4)))
;assoc miss?

(defun def-add (deflist name expr)
  (cons (cons name
              (sexp-expand (sexp-read expr)
                           deflist)) 
        deflist))


(sexp-unread '("a" nil . 1))


(defun strip (expr) 
  (maptree (fn (s)
             (if (car s)
                 (strip (cdr s))
                 (cdr s)))
           expr
           #'symp)) 

(defun parse (expr deflist) 
  (strip (sexp-expand (sexp-read expr) deflist)))

($$-> '()
  (def-add $$ "a" 1) 
  (def-add $$ "b" '(a a)) 
  (def-add $$ "c" '(a b b)) 
  (def-get $$ "c") 
  (strip $$))



(let ((defs
        ($$-> '()
          (def-add $$ "$" #'subj-op) 
          (def-add $$ "q" #'quot-op) 
          (def-add $$ "<" #'car-op)
          (def-add $$ ">" #'cdr-op)
          (def-add $$ "c" #'cons-op)
          (def-add $$ "*" #'eval-op)
          (def-add $$ "?" #'cond-op)
          (def-add $$ "l" #'list-op)
          (def-add $$ "+2" #'add2-op)
          (def-add $$ "args" '(< ($)))
          (def-add $$ "arg1" '(< args))
          (def-add $$ "env" '(> ($)))
          (def-add $$ "env1" '(< env))
          (def-add $$ "rec" '(l env1 env1))
          (def-add $$ "~" nil)
          (def-add $$ "clop" '(q ((? arg1
                                     (c (c (< arg1) (< arg1)) 
                                        (rec (> arg1)))
                                     (q ~))
                                  (? arg1
                                     (c (c (< arg1) (< arg1)) 
                                        (rec (> arg1)))
                                     (q ~))))))))
  (kreck '(1 2 3) '(clop ($)) defs))
;problems with lack of implicit imperative semantics
;name for parse without stripping?
;making maptree not fall into infinite loop?
  ;throw exception
;clos?


;TODO stack overflow when a def doesn't exist
;TODO doing it all sanely

#|
($$-> '()
  (def-add $$ "$" #'subj-op) 
  (def-add $$ "q" #'quot-op)
  (strip $$) 
 |#

;reading - text to sexp
;

;indenting when (arg1 arg2 &body body)

;TCO 



