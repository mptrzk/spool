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



;kreck

(defun kreck-apply (subj op args)
  (if (atom op)
      (funcall op subj args)
      (let ((clos (kreck-eval subj op))) 
        (kreck-eval (acons "args" 
                           args
                           ;(kreck-evlis subj args)
                           (cdr clos))
                    (car clos)))))

(defun kreck-eval (subj form)
  (if (atom form)
      form
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
  (mapcar (fn (f) (kreck-eval subj f))
          args))

(defun evlis-op (subj args) 
  (let ((esubj (kreck-eval subj (car args))))
    (mapcar (fn (f) (kreck-eval subj f))
          args)))

(defun assoc-op (subj args)
  (assoc (kreck-eval subj (car args))
         (kreck-eval subj (cadr args))
         :test #'equal))

(defun getvar-op (subj args)
  (let* ((key (kreck-eval subj (car args)))
         (res (assoc key subj :test #'equal)))
    (if res
        (cdr res)
        (error (format nil "var ~a not found~%" key)))))


;defs



(defun parse (expr)
  (cond ((null expr) nil)
        ((symbolp expr) (list #'getvar-op
                              (string-downcase expr)))
        ((consp expr) (cons (parse (car expr))
                            (parse (cdr expr))))
        (t expr)))

(defun def-make (name &rest expr)
  (cons name (parse (car expr))))

(defun kreck (args form)
  (kreck-eval (acons "args" (parse args) *defs*)
              (parse form)))

(defparameter *defs*
  `(("$" . ,#'subj-op)
    ("q" . ,#'quot-op)  
    ("<" . ,#'car-op)  
    (">" . ,#'cdr-op)  
    ("c" . ,#'cons-op)  
    ("*" . ,#'eval-op)
    ("?" . ,#'if-op)
    ("l" . ,#'list-op)
    ("@" . ,#'assoc-op)
    ("@$" . ,#'getvar-op)
    ,(def-make "*l"
               '(c (q (* (* (> ($))
                            (< args))
                         (c (q l)
                            (* (> ($))
                               (< (> args))))))
                   ($)))
    ,(def-make "apply" ;flipping subjs?
               '(c (q (* (> ($))
                         (c (* (> ($))
                               (< args))
                            (* (> ($))
                               (c (q l)
                                  (* (> ($))
                                     (< (> args))))))))
                   ($)))

    ,(def-make "dup"
               '(c (q (apply (q (c (q (c (> args)
                                         (> args)))
                                   ($)))
                             args))
                   ($)))
    ))

;exop quote trap problem
;*$
;!*$ ?
;%*$ ?
;fexpr-apply?
;sign-letter runes


;tests
;default aura

(test "all" ;no "in all"?
  (test-equal "kreck"
    ((kreck '(1 2 3) '(> (< ($)))) '(1 2 3))
    ;((kreck '(1 2 3) '(ooo (q 1) 2 3)) '(1 2 3))
    ))
(kreck '(1 2 3) '(*l ($) (q (4 (> args) 6))))
