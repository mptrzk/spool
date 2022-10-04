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

(defun defet-op (subj args)
  (let* ((key (car args))
         (res (assoc key (cddr subj) :test #'equal)))
    (if res
        (cdr res)
        (error (format nil "def ~a not found~%" key)))))

(defun defev-op (subj args)
  (kreck-eval subj (defet-op subj args)))

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
  `(unparse (kreck-eval (cons (parse (quote ,args))
                              (cons nil *defs*))
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
    ("$@" . ,#'defet-op)
    ("$@*" . ,#'defev-op)
    ("!d" . ,#'debug-op)
    ("args" . ,(parse '(< ($))))
    ("arg1" . ,(parse '(< args)))
    ("arg2" . ,(parse '(< (> args))))
    ("arg3" . ,(parse '(< (> (> args)))))
    ("ctx" . ,(parse '(> ($))))
    ("locs" . ,(parse '(< ctx)))
    ("defs" . ,(parse '(> ctx)))
    ("this" . ,(parse '(< locs)))
    ("calr" . ,(parse '(< (> locs))))

    ("non-rec-inop-ctx" . ,(parse '(c (c ~
                                         (c ($)
                                            ~))
                                      defs)))

    ("^*" . ,(parse '(q (c (q (* (* calr (q calr))
                                 (* calr arg1)))
                           non-rec-inop-ctx))))

    ("w" . ,(parse '(q (c (q (c (^* arg1) ~))
                          non-rec-inop-ctx))))

    ("l" . ,(parse '(q (c (q (* ($) this))
                          (c (c (q (? args
                                      (c (* calr arg1)
                                         (* (c (> args) ctx) this))
                                      ~))
                                (c ($) ~))
                             defs)))))


    ("$@c" . ,(parse '(q (c (q (c (^* (q args))
                                  (c (^* (q locs))
                                     (c (c (< (> arg1))
                                           (^* arg2))
                                        (^* (q defs))))))
                            non-rec-inop-ctx))))

    ("$->" . ,(parse '(q (c (q (* ($) this))
                            (c (l (q (? args 
                                        (* (c (> args)
                                              (c (l this
                                                    (^* arg1))
                                                 defs))
                                           this)
                                        calr))
                                  ($))
                               defs)))))))



;gate context capturing problem?
;evaluating in caller context - deflist ambiguity
;is non-deflist context capture a problem?
;is it desirable?

; recursion with context change macro
;^* - eval in caller scope
;^*^ in fn definitions?



(kreck (1 2 3) ($))
(kreck (1 2 3) (l (> args) 1))
(kreck (1 2 3) ($@c foo (q ($))))
(kreck ((1 2) 3) ($-> 1))
(kreck ((1 2) 2 3)
  ($-> ($@c q* (q (q (c (q (l (q q) (^* arg1)))
                        non-rec-inop-ctx))))
       ($@c r-op-loader (q (q (* ($) this))))
       ($@c e-op-loader (q (q (* (c (* calr
                                       (c (q l) args))
                                    ctx)
                                 this))))
       ($@c gate
            (q (q (c e-op-loader
                     (c (l (q (l (q c)
                                 (l (q q) arg1)
                                 (l (q c)
                                    (l (q l)
                                       (l (q q) arg3)
                                       (q ($))
                                       (l (q q) calr))
                                    (l (q q)
                                       (c (c (q "rec")
                                             (q (q (c this ctx))))
                                          (? arg2
                                             arg2
                                             defs))))))
                           ($))
                        defs)))))

       ($@c makr (q (< (> (> locs)))))
       ($@c dump
            (q* (gate r-op-loader ~
                      (q (l arg1 (^* arg1))))))
       (l (dump (> (q (1 2 3))))
          r-op-loader)))
;use cases for $@c without q?
;q in gate definition or loader definition?
  ;gate! look at the gate's own loader
;many ways
;gates defined with deflist arg?
;is there still a need for the last arg of "gate" to be evaluated?
;^does that matter? - wrappers
;easy scope gate - finding "makr" in the deflist
; requires symbolic defs
;all defs scoped?
;if deflist empty, search makr's deflist
;outr, calr, makr
