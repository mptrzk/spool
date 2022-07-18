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
 

(defun def-get (str alist) 
  (if alist
      (if (equal str (caar alist)) 
          (car alist)
          (def-get str (cdr alist)))))
(def-get "b" '(("a" . 1) ("b" . 2) ("c" . 3)))


(defun sym-expand (sym deflist) 
  (if (car sym)
      (cons (car sym) 
            (cdr (def-get (car sym) deflist)))
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
  (def-get "c" $$) 
  (strip $$))


