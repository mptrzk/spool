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
      (let ((op (kreck-eval subj (car form)))
            (args (cdr form)))
        (if (atom op)
            (funcall op subj args)
            (kreck-eval (cons args (cdr op))
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
         (res (assoc key (cddr subj) :test #'equal)))
    (if res
        (kreck-eval subj (cdr res))
        (error (format nil "def ~a not found~%" key)))))

(defun debug-op (subj args)
  (format t "evaluating ~a:~%" (cons '!d (unparse args)))
  (let ((res (kreck-eval subj (car args))))
    (format t "  ~a~%" (unparse res))
    res))

;defs



(defun parse (expr)
  (cond ((null expr) nil)
        ((symbolp expr) (list #'defget-op
                              (string-downcase expr)))
        ((consp expr) (cons (parse (car expr))
                            (parse (cdr expr))))
        (t expr)))

(defun unparse (expr)
  (cond ((atom expr) expr)
        ((and (eq (car expr) #'defget-op)
              (stringp (cadr expr)))
         (let ((s (read-from-string (cadr expr)))) 
           s))
        (t (cons (unparse (car expr))
                 (unparse (cdr expr))))))

(defun def-make (name &rest expr)
  (cons name (parse (car expr))))

(defun kreck (args form)
  (unparse (kreck-eval (cons (parse args)
                             (cons nil *defs*))
                       (parse form))))


(defparameter *defs*
  `(("~" . nil)
    ("$" . ,#'subj-op)
    ("q" . ,#'quot-op)  
    ("<" . ,#'car-op)  
    (">" . ,#'cdr-op)  
    ("c" . ,#'cons-op)  
    ("*" . ,#'eval-op)
    ("?" . ,#'if-op)
    ("@" . ,#'defget-op)
    ("!d" . ,#'debug-op)
    ("args" . ,(parse '(< ($))))
    ("arg1" . ,(parse '(< args)))
    ("arg2" . ,(parse '(< (> args))))
    ("arg3" . ,(parse '(< (> (> args)))))
    ("ctx" . ,(parse '(> ($))))
    ("locs" . ,(parse '(< (> ($)))))
    ("defs" . ,(parse '(> (> ($)))))
    ("this" . ,(parse '(< locs)))
    ("calr" . ,(parse '(< (> locs))))
    ("non-rec-fexpr-ctx" . ,(parse '(c (c ~
                                          (c ($)
                                             ~))
                                       defs)))
    ("*^" . ,(parse '(c (q (* (* calr (q calr))
                              (* calr arg1)))
                        non-rec-fexpr-ctx)))

    ("w" . ,(parse '(c (q (c (*^ arg1) ~))
                       non-rec-fexpr-ctx)))

     
    ("l" . ,(parse '(c (q (? args
                             (c (*^ arg1)
                                (*^ (c (q l) (> args))))))
                       non-rec-fexpr-ctx)))
    ("fex-gate" . ,(parse '(c (q (l (q c)
                                  (l (q q) arg1)
                                  (l (q c)
                                     (l (q l)
                                        (l (q q) arg1)
                                        (q ($)))
                                     (q defs))))
                            non-rec-fexpr-ctx)))
    
    ))                ;^^context capturing problem?
;evaluating in caller context - deflist ambiguity
;is non-deflist context capture a problem?
;is it desirable?

; recursion with context change macro
;^* - eval in caller scope
;*^^ in fn definitions?


(kreck '(1 2 3) '($))
;(kreck '(1 2 3) '(w (> args)))
(kreck '(1 2 3) '(l (> args) 1))
(kreck '(1 2 3) '(fex-gate (c arg1 arg2)))
(kreck '(1 2 3) '(* ($) (fex-gate (c arg1 arg2))))
(kreck '(1 2 3) '((* ($) (fex-gate (c arg1 arg1))) (> arg1)))
(kreck '((1 2) 3 4)
       '((* ($) (fex-gate (l arg1 (*^ arg1)))) (> arg1)))

;^nope
