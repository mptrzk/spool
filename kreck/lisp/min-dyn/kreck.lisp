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

(defun debug-op (subj args)
  (format t "evaluating ~a:~%" (cons '!d (unparse args)))
  (let ((res (kreck-eval subj (car args))))
    (format t "  ~a~%" (unparse res))
    res))

;defs



(defun parse (expr)
  (cond ((null expr) nil)
        ((symbolp expr) (list #'getvar-op
                              (string-downcase expr)))
        ((consp expr) (cons (parse (car expr))
                            (parse (cdr expr))))
        (t expr)))

(defun unparse (expr)
  (cond ((atom expr) expr)
        ((and (eq (car expr) #'getvar-op)
              (stringp (cadr expr)))
         (let ((s (read-from-string (cadr expr)))) 
           s))
        (t (cons (unparse (car expr))
                 (unparse (cdr expr))))))

(defun def-make (name &rest expr)
  (cons name (parse (car expr))))

(defun kreck (args form)
  (kreck-eval (acons "args" (parse args) *defs*)
              (parse form)))

(defparameter *defs*
  `(("~" . nil)
    ("$" . ,#'subj-op)
    ("q" . ,#'quot-op)  
    ("<" . ,#'car-op)  
    (">" . ,#'cdr-op)  
    ("c" . ,#'cons-op)  
    ("*" . ,#'eval-op)
    ("?" . ,#'if-op)
    ("#" . ,#'getvar-op)
    ("!d" . ,#'debug-op)
    ("l" . ,(parse '(c (q (? args
                             (c (* old
                                   (< args))
                                (* old
                                   (c (c code ($))
                                      (> args))))
                             ~))
                       (c (c "code"
                             (q (? args
                                   (c (* old
                                         (< args))
                                      (* old 
                                         (c (c code ($))
                                            (> args))))
                                   ;^evals 
                                   ~)))
                          (c (c "old"
                                ($))
                             ($))))))
    ("l" . ,(parse '(c (q (? args
                             (c (* (> ($))
                                   (< args))
                                (* (> ($))
                                   (c (q l)
                                      (> args))))
                             ~))
                       ($))))
    ))





(kreck '(1 2 3) '(l (> args) 1))
;bad approach
;write code by hand first
;^this is the closuwee
; no, that's an inop!
;evalating gate ctx arg?
