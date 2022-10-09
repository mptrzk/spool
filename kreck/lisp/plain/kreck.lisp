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
        (error (format nil "def ~a not found~%" key)))))

(defun defev-op (subj args)
  (kreck-eval subj (defget-op subj args)))

(defun debug-op (subj args)
  (format t "evaluating ~a:~%" (cons '!d (unparse args)))
  (let ((res (kreck-eval subj (car args))))
    (format t "  ~a~%" (unparse res))
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
    ("code" . ,(parse '(< this))) 
    ("clos" . ,(parse '(< (> this)))) 
    ("defs" . ,(parse '(> (> this)))) 
    ("l-dyn" . ,(parse '(c (q eargs)
                           (c ~ defs))))
    ("eargs" . ,(parse '(? args
                                 (c (* calr (< args))
                                    (* (c calr
                                          (c this 
                                             (> args)))
                                       code))
                                 ~)))))



;(untrace kreck-eval)
;(kreck (1 2 3) foo) << INCORRECT USAGE
(kreck (1 2 3) (l-dyn (> args) (> args)))
;(kreck (1 2 3) (l (> args) 1))
;(kreck (1 2 3) ($@c foo (q ($))))
;(kreck ((1 2) 3) ($-> 1))

