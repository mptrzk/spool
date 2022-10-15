(defparameter dbg nil)

(defmacro fn (args &body body) 
  `(lambda ,args ,@body))

(defun thread-sub (a b) 
  (subst a '$$ b))

(defmacro $$-> (input &body exprs) 
  (reduce #'thread-sub (cons input exprs)))


(defvar *test-macros* '(test test-equal))

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

(defmacro dump (expr)
  `(progn (format t "~s:~%  ~s~%" (quote ,expr) ,expr)
          ,expr))


;kreck

(defun kreck-eval (subj form)
  (if (atom form)
      form
      (let ((op (kreck-eval subj (car form)))) 
        (if (atom op) 
            (funcall op subj (cdr form))
            (kreck-eval (cons (cdr form)
                              (cons subj op))
                        (car op))))))


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

(defun defget-op (subj args)
  (let* ((key (car args))
         (res (assoc key (cadddr subj) :test #'equal)))
    (if res
        (cdr res)
        (error (format nil "def ~s not found~%" key)))))

(defun defev-op (subj args)
  (kreck-eval subj (defget-op subj args)))

(defun debug-op (subj args)
  (format t "evaluating ~s:~%" (cons '!d (unparse args)))
  (let ((res (kreck-eval subj (car args))))
    (format t "  ~s~%" (unparse res))
    res))

;defs



(defun parse (expr)
  (cond ((null expr) nil)
        ((symbolp expr) (list #'defev-op
                              (string-downcase expr)))
        ((consp expr) (cons (parse (car expr))
                            (parse (cdr expr))))
        (t expr)))

(defun unparse (expr)
  (cond ((atom expr) expr)
        ((and (eq (car expr) #'defev-op)
              (stringp (cadr expr)))
         (let ((s (read-from-string (cadr expr)))) 
           s))
        (t (cons (unparse (car expr))
                 (unparse (cdr expr))))))

(defun def-make (name &rest expr)
  (cons name (parse (car expr))))

(defmacro kreck (args &body form)
  `(unparse (kreck-eval (parse (quote (,args nil nil ,*defs*)))
                        (parse (quote ,(car form))))))


(defparameter *defs*
  `(("~" . nil)
    ("$" . ,#'subj-op)
    ("q" . ,#'quot-op)  
    ("<" . ,#'car-op)  
    (">" . ,#'cdr-op)  
    ("c" . ,#'cons-op)  
    ("*" . ,#'eval-op)
    ("?" . ,#'if-op)
    ("$@" . ,#'defget-op)
    ("$@*" . ,#'defev-op)
    ("!d" . ,#'debug-op)
    ("args" . ,(parse '(< ($)))) 
    ("ctx" . ,(parse '(> ($)))) 
    ("calr" . ,(parse '(< (> ($))))) 
    ("this" . ,(parse '(> (> ($))))) 
    ("arg1" . ,(parse '(< args)))
    ("arg2" . ,(parse '(< (> args))))
    ("code" . ,(parse '(< this))) 
    ("defs" . ,(parse '(< (> this)))) 
    ("clos" . ,(parse '(> (> this)))) 
    ("l" . ,(parse '(c (q (? args
                                 (c (* calr (< args))
                                    (* (c (> args) ctx)
                                       code))
                                 ~))
                           (c defs ~))))
    ("eargs" . ,(parse '(* calr (c (q l) args))))
    ("fn-snip" . ,(parse '(c eargs (c ~ this)))) 
    ("$->" . ,(parse '(l (q (? args 
                               (* (c (> args)
                                     (c (* calr (< args))
                                        this)) 
                                  code)
                               calr))
                         defs)))
    ("$@c" . ,(parse '(l (q (l (* calr (q args))
                               (* calr (q calr))
                               (* calr (q code))
                               (c (c (< (> arg1))
                                     (* calr arg2))
                                  (* calr (q defs)))))
                         defs)))  
    ))


(kreck (1 2 3) (l (> args) (> args)))
(kreck (1 2 3) ((l (q ($-> fn-snip args)) defs) (> args)))
(kreck (1 2 3)
  ($-> ($@c a (l (q q)
                 (l (q ($-> fn-snip (c arg1 arg1)))
                    defs)))
       ($@c q* (l (q q)
                  (l (q (l (q q)
                           (* calr arg1)))
                     defs)))
       ($@c inop (q* (l (q (l (* calr arg1)
                              (* calr (q defs)))) 
                        defs)))
       ($@c fn (q* (inop (q (inop (l (q $->)
                                     (q fn-snip)
                                     (* calr arg1)))))))
       ($@c b (q* (inop (q ($-> fn-snip (c arg1 arg1))))))
       ($@c x (q* (inop (q (c arg1 arg1)))))
       ((fn (q (c arg1 arg1))) (l arg1 arg2))
       ))
