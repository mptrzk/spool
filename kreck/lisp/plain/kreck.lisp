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
            (kreck-eval (cons subj
                              (cons op (cdr form))) (car op))))))


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
         (res (assoc key (cddadr subj) :test #'equal)))
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
  `(unparse (kreck-eval (quote (nil (nil nil ,@*defs*) ,@args))
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
    ("calr" . ,(parse '(< ($)))) 
    ("this" . ,(parse '(< (> ($))))) 
    ("args" . ,(parse '(> (> ($))))) 
    ("arg1" . ,(parse '(< args)))
    ("arg2" . ,(parse '(< (> args))))
    ("code" . ,(parse '(< this))) 
    ("clos" . ,(parse '(< (> this)))) 
    ("defs" . ,(parse '(> (> this)))) 
    ("l" . ,(parse '(c (q (? args
                                 (c (* calr (< args))
                                    (* (c calr
                                          (c this 
                                             (> args)))
                                       code))
                                 ~))
                           (c ~ defs))))
    ("eargs" . ,(parse '(* calr (c (q l) args))))
    ("snip" . ,(parse '(c ~ (c this eargs)))) 
    ("$->" . ,(parse '(c (q (? args 
                               (* (c (* calr (< args))
                                     (c this
                                        (> args)))
                                  code)
                               calr))
                         (c ~ defs))))
    ("foo" . ,(parse '(c (q ($-> snip ;TODO remove
                                 (? arg1
                                    (c (c (< arg1) (< arg1))
                                       (this (> arg1)))
                                    ~)))
                         (c ~ defs))))
    ("$@c" . ,(parse '(c (q (c (* calr (q calr))
                               (c (c (* calr (q code))
                                     (c ~
                                        (c (c (< (> arg1))
                                              (* calr arg2))
                                           (* calr (q defs)))))
                                  (* calr (q args)))))
                         (c ~ defs))))))

;TODO inops with quote?
;nah, the expr would contain symbol "defs"

;todo split snip and eargs
;rewrite eargs as subj modifying

;(untrace kreck-eval)
;(kreck (1 2 3) foo) << INCORRECT USAGE
;(kreck (1 2 3) (l (> args)))
;(kreck (1 2 3) (foo (> args)))
;(kreck (1 2 3) ($-> snip)) << impossible, no calr
;(kreck (1 2 3) ($@c foo (q ($))))
;(kreck (1 2 3) ($-> ($@c a 1) defs))
(kreck (1 2 3)
  ($-> ($@c a (l (q q)
                 (c (q ($-> snip (c arg1 arg1)))
                    (c ~ defs))))
       ($@c q* (l (q q)
                  (c (q (l (q q)
                           (* calr arg1)))
                     (c ~ defs))))
       ($@c inop (q* (c (q (c (* calr arg1)
                              (c ~ defs)))
                        (c ~ defs))))
       ;(a (l 1 2))
       ($@c fn (q* (inop (inop (l (q $->) (q snip) (* calr arg1))))))
       ($@c b (q* (inop (q ($-> snip (c arg1 arg1))))))
       ;(b (l 1 2))
       ($@c x (q* (inop (q (c arg1 arg1)))))
       (< (fn))
       ;((fn (q (c arg1 arg1))) (l (> args)))
       ))

