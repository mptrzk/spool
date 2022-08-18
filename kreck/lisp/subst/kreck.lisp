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
  (if (or (atom form)
          (null (car form)))
      form
      (kreck-apply subj
                   (kreck-eval subj (car form))
                   (cdr form))))

(defun kreck-apply (subj op args)
  (if (atom op)
      (funcall op subj args)
      (let ((op-res (kreck-eval subj op)))
        (kreck-eval (cons args (cdr op-res))
                    (car op-res)))))

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

(defun getdef-op (subj args)
  (let* ((key (car args))
         (res (assoc key (cddr subj) :test #'equal)))
    (if res
        (cdr res)
        (error (format nil "def ~a not found~%" key)))))

(defun evdef-op (subj args)
  (kreck-eval subj (getdef-op subj args)))

(defun debug-op (subj args)
  (format t "evaluating ~a:~%" (cons '!d (unparse args)))
  (let ((res (kreck-eval subj (car args))))
    (format t "  ~a~%" (unparse res))
    res))

;defs



(defun parse (expr)
  (cond ((null expr) nil)
        ((symbolp expr) (list #'evdef-op
                              (string-downcase expr)))
        ((consp expr) (cons (parse (car expr))
                            (parse (cdr expr))))
        (t expr)))

(defun unparse (expr)
  (cond ((atom expr) expr)
        ((and (eq (car expr) #'evdef-op)
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
    ("$@" . ,#'getdef-op)
    ("$@*" . ,#'evdef-op)
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
    ("^*" . ,(parse '(q (c (q (* (* calr (q calr))
                              (* calr arg1)))
                        non-rec-fexpr-ctx))))

    ("w" . ,(parse '(q (c (q (c (^* arg1) ~))
                       non-rec-fexpr-ctx))))

    ("l" . ,(parse '(q (c (q (* ($) this))
                       (c (c (q (? args
                                   (c (* calr arg1)
                                      (* (c (> args) ctx) this))
                                   ~))
                             (c ($) ~))
                          defs)))))
    ("fex-gate" . ,(parse '(q (c (q (l (q c)
                                       (l (q q) arg2)
                                       (l (q c)
                                          (l (q l)
                                             (l (q q) arg2)
                                             (q ($)))
                                          (l (q q)
                                             (c (c (< (> arg1))
                                                   (q (c this ctx)))
                                                defs)))))
                                 non-rec-fexpr-ctx))))
    ("!fex-gate" . ,(parse '(q (c (q (* calr 
                                     (* calr
                                        (c (q fex-gate) args))))
                               non-rec-fexpr-ctx))))

    ("$@c" . ,(parse '(q (c (q (c (^* (q args))
                               (c (^* (q locs))
                                  (c (c (< (> arg1))
                                        (^* arg2))
                                     (^* (q defs))))))
                         non-rec-fexpr-ctx))))

    ("$->" . ,(parse '(q (!fex-gate $->
                                 (? args 
                                    (* (c (> args)
                                          (c (l this
                                                (^* arg1))
                                             defs))
                                       this)
                                    calr)))))
    ))

;fex-gate context capturing problem?
;evaluating in caller context - deflist ambiguity
;is non-deflist context capture a problem?
;is it desirable?

; recursion with context change macro
;^* - eval in caller scope
;^*^ in fn definitions?


(kreck '(1 2 3) '($))
;(kreck '(1 2 3) '(w (> args)))
(kreck '(1 2 3) '(l (> args) 1))
(kreck '(1 2 3) '(fex-gate dump (l arg1 (^* arg1))))
(kreck '((1 2) 3 4)
       '((fex-gate dump (l arg1 (^* arg1))) (> arg1)))
(kreck '((1 2) 3 4)
       '((!fex-gate dump (l arg1 (^* arg1))) (> arg1)))
(kreck '(1 2 3) '($@c foo (q ($))))
(kreck '((1 2) 2 3)
       '($-> ($@c foo (q ($)))
             ($@c bar (q (> ($))))
             ($@c baz (q (c (q (< (^* arg1)))
                            non-rec-fexpr-ctx)))
             (l (baz args)
                (baz (> args)))))
;(kreck '((1 2) 3 4)
;       '(((fex-gate dump (l arg1 (^* arg1)))) (> arg1)))
;^^ why not that
;explain confusion over fex-gate eval
;and the kerfuffle with (< arg1)

;^nope


