(defparameter *code* '((defparameter *dbg* nil)

(defmacro dump (expr &optional (fun '(lambda (x) x))) 
  `(progn (if *dbg*
              (format t "~a => ~a~%" 
                      (quote ,expr) 
                      (,fun ,expr))) 
          ,expr))

(defmacro with-dbg (expr) 
  (let ((expr* (gensym))) 
    `(progn (setf *dbg* t) 
            (let ((,expr* ,expr))
              (setf *dbg* nil)
              ,expr*))))


(defmacro mbind (vars value-form &body body) 
  `(multiple-value-bind ,vars ,value-form ,@body))

(defmacro letfoo (bindings body) 
  (let ((loop-name (gensym "loop"))
        (args (mapcar #'car bindings)) 
        (inits (mapcar #'cadr bindings)))
    `(labels ((,loop-name ,args 
                ,(subst loop-name 'recur body))) 
       (,loop-name ,@inits))))


(defparameter *defs*
  (make-hash-table))

(defun def-add (name expr) 
  (setf (gethash name *defs*) (make-noun expr)))

(defun def-get (name) 
  (mbind (val exists) 
    (gethash name *defs*) 
    (if exists 
        val 
        (progn (format t "invalid def - ~a~%" name)
               'err))))

(defparameter *auras*
  (make-hash-table))

(defun aura-add (name expr) 
  (setf (gethash name *auras*) expr))

(defun aura-get (name) 
  (mbind (val exists) 
    (gethash name *auras*) 
    (if exists 
        val 
        (progn (format t "invalid aura - ~a~%" name)
               'err))))

(defun uintp (x) (and (integerp x) (>= x 0)))

(defun noun-uint (expr)
  (reverse (letfoo ((n expr) 
                    (noun nil))
             (if (> n 0)
                 (mbind (q r) (floor n 2)
                   (recur q (cons (if (= r 1)
                                      (def-get 'T))
                                  noun)))
                 noun))))


(defun make-noun (noun) 
  (cond ((listp noun) 
         (mapcar #'make-noun noun))
        ((uintp noun)
         (noun-uint noun))  
        ((symbolp noun)
         (def-get noun))))

(defun noun-equal (a b) 
  (equal (make-noun a) (make-noun b)))

(defun inc (noun) 
  (cond ((equal noun (def-get '~)) 
         (make-noun '(T)))
        ((equal (car noun) (def-get '~)) 
         (cons (def-get 'T)
               (cdr noun)))
        ((equal (car noun) (def-get 'T)) 
         (cons (def-get '~)
               (inc (cdr noun))))))

(def-add '~ '())
(def-add 'T '(~))
(letfoo ((symlist '($ q l h < > = c * ? ! uq uqs))
         (ctr (make-noun '(~)))) 
  (if (null symlist) 
      t 
      (progn (def-add (car symlist) (list nil ctr)) 
             (recur (cdr symlist) (inc ctr)))))

(defmacro case-noun (expr &body clauses) 
  (let ((val-sym (gensym "val"))) 
    `(let ((,val-sym ,expr)) 
       (cond ,@(mapcar (lambda (c) 
                         (if (eq (car c) 'else)
                             `(T ,(cadr c))
                             `((noun-equal ,val-sym ,(car c)) 
                               ,(cadr c)))) 
                       clauses)))))



(defun assoc-err (item alist) 
  (if (null alist)
      (progn (format t "item ~a not found in the alist~%" item)
             (cons item 'err)) 
      (if (equal item (caar alist)) 
          (car alist) 
          (assoc-err item (cdr alist)))))

(defun aura-def (noun symlist) 
  (let ((deflist (mapcar (lambda (sym) 
                       (cons (def-get sym) sym)) 
                     symlist))) 
    (cdr (assoc-err noun deflist)))) 



(defun aura-list (noun auras) 
  (if noun 
      (cons (aura-apply (car noun) (car auras)) 
            (aura-list (cdr noun) 
                       (if (cdr auras)
                           (cdr auras)
                           auras)))
      nil))

(defun aura-if (noun args) 
  (mbind (predicate aura-true aura-false)
         (values-list args)
         (if (funcall predicate noun) 
             (aura-apply noun aura-true) 
             (aura-apply noun aura-false))))

(defun aura-uint (noun) 
  (letfoo ((noun* noun) 
           (weight 1) 
           (res 0)) 
    (if noun*
        (recur (cdr noun*)
               (* weight 2)
               (+ res (* weight
                         (if (car noun*) 1 0))))
        res)))


(defun aura-apply (noun aura) 
  (if (symbolp aura)
      (case aura 
        (uint (aura-uint noun)) 
        (otherwise (aura-apply noun (aura-get aura))))
      (case (car aura) 
        (def (aura-def noun (cdr aura))) 
        (list (aura-list noun (cdr aura))) 
        (if (aura-if noun (cdr aura))) 
        (otherwise (format t "aura - undefined operator: ~a~%"
                           (car aura))))))

(aura-add 'opcode '(def ! $ q < > c * ? l h))
(aura-add 'bool '(def T ~))
(aura-add 'code '(list opcode code))

(aura-apply (make-noun '?) '(def ? $))


(aura-apply (make-noun '(c (> ($)) ($))) 'code)


(defun corep (noun) (if (car noun) T nil))

(aura-add 'code
          '(list (if corep
                     code
                     opcode)
                 code))

(aura-add 'tree 
          '(if consp
               (list tree)
               (def ~)))

(aura-if (make-noun '(c c c)) '(corep (list opcode) opcode))
(aura-if (make-noun 'c) '(corep (list opcode) opcode))
(aura-apply (make-noun '((~) (c c) c (c c c)))
            '(list bool (if corep (list opcode) opcode)))


(defun pock-evlist (subj args) 
  (mapcar (lambda (arg)
            (pock-eval subj arg)) 
          args))


(defun pock-apply (subj fun args) 
  (let* ((fun* (pock-eval subj fun)) 
         (code (car fun*)) 
         (env (cdr fun*)))
    (pock-eval (cons args env) code)))

(defun noun-iden (a b) 
  (if (equal a b)
      (def-get 'T) 
      (def-get '~)))

(defun pock-eval (subj form) 
  (if (caar form)
      (pock-apply subj
                  (car form)
                  (pock-evlist subj (cdr form)))
      (mbind (op arg1 arg2 arg3) 
             (values-list form)
             (case-noun op 
               ('$ subj) 
               ('q arg1) 
               ('< (car (pock-eval subj arg1)))
               ('> (cdr (pock-eval subj arg1))) 
               ('c (cons (pock-eval subj arg1) 
                         (pock-eval subj arg2)))
               ('= (noun-iden (pock-eval subj arg1) 
                              (pock-eval subj arg2)))
               ('* (pock-eval (pock-eval subj arg1) 
                              (pock-eval subj arg2)))
               ('? (if (pock-eval subj arg1) 
                       (pock-eval subj arg2)
                       (pock-eval subj arg3))) 
               ('h (pock-eval subj arg2))
               ('l (pock-evlist subj (cdr form)))
               ('! (pock-eval subj
                              (pock-apply subj
                                          (cadr form)
                                          (cddr form))))
               (else (format t "invalid opcode - ~a~%" op))))))


(defun pock (subj form &optional (aura 'tree)) 
  (aura-apply (pock-eval (make-noun subj) (make-noun form)) aura))


(defun make-gate (code &rest env) 
  (let ((code* (make-noun code)))
    (list (def-get 'q) 
          (append (list code* code*)
            (mapcar (lambda (e) 
                      (make-noun e))
                    env)))))



(def-add 'args '(< ($)))
(def-add 'arg1 '(< (< ($))))
(def-add 'arg2 '(< (> (< ($)))))
(def-add 'arg3 '(< (> (> (< ($))))))
(def-add 'funs '(< (> ($))))
(def-add 'fun1 '(< funs))
(def-add 'locs '(< (> (> ($)))))
(def-add 'loc1 '(< locs))
(def-add 'loc2 '(< (> locs)))
(def-add 'loc3 '(< (> (> locs))))
(def-add 'olds '(< (> (> (> ($))))))

(def-add 'rec1 '(l fun1 (l fun1)))

(pock '(T T T) '(l (> ($)) ($)))
(aura-apply (make-gate 'args) 'code)

(def-add 'door '(q ((l (q q) (l arg1)))))
(pock '() 
      '(door (q args))
      'code)


(def-add 'gate '(q ((l (q q)
                       (l arg2
                          (l arg2)
                          arg1)))))

(def-add 'gate '(! door
                   (l (q q)
                       (l arg2
                          (l arg2)
                          arg1))))


(equal (pock '()
             '(gate (q ~) (q args))
             'code)
       (aura-apply (make-gate 'args) 'code))


(pock '(T T T)
      '((q (funs (T ~ T))) ($) (> ($))) 
      '(list bool))

(def-add 'foo '(! gate ($) locs))
(pock '(T T T)
      '(foo ($) (> ($))) 
      'code)

(def-add 'inc 
         '(! gate ()
             (? arg1 
                (? (< arg1) 
                   (c (q ~)
                      (rec1 (> arg1))) 
                   (c (q T) (> arg1)))
                (q (T)))))

(pock '(T T ~)
      '(inc ($) (> ($))) 
      '(list bool))

(def-add 'len 
         '(! gate (~)
             (? arg1
                ((l fun1 (l fun1) (inc locs)) (> arg1)) 
                locs)))



(pock '(T T (T T) ~ T ~)
      '(len ($))
      'uint)

(def-add 'map '(! gate ()
                  (? arg2
                     (c (arg1 (< arg2))
                        (rec1 arg1 (> arg2))) 
                     (q ~)))) 


(def-add 'foldl '(! gate ()
                    (? arg3 
                       (rec1 arg1
                             (arg1 arg2 (< arg3)) 
                             (> arg3))
                       arg2)))




(def-add 'apply '(q ((* (c arg2 (> arg1)) (< arg1)))))
(pock '() '(apply inc (q ((T)))))





(def-add 'any '(! gate ()
                  (? arg1 
                     (? (< arg1)
                        (q T) 
                        (rec1 (> arg1))) 
                     (q ~))))

(def-add 'all '(! gate ()
                  (? arg1 
                     (? (< arg1)
                        (rec1 (> arg1))
                        (q ~)) 
                     (q T))))

(def-add 'or '(! door (any args)))
(def-add 'and '(! door (all args)))
(def-add 'not '(q ((? arg1 (q ~) (q T)))))
(def-add 'xor2 '(q ((and (or arg1 arg2)
                         (not (and arg1 arg2))))))

(def-add 'add '(! gate ()
                  (? arg1 
                     (? arg2
                        (c (xor2 (< arg1) (< arg2)) 
                           (? (and (< arg1) (< arg2))
                              (rec1 (inc (> arg1)) (> arg2)) 
                              (rec1 (> arg1) (> arg2))))
                        arg1)
                     arg2)))

(pock '(T T T)
      '(xor2 (q T) (q T)) 
      'bool)

(pock '(T T T)
      '(add (q 19999) (q 5999)) 
      'uint)


(pock '(T T T)
      '(= (< ($)) (c (q T) (> ($))))
      'bool)

(pock '((T (T T)) (~ T) (~ T T))
      '(map (! gate () (inc (len arg1))) ($)) 
      '(list uint)) 

(def-add 'rec-loc 
         '(! door 
             (l (q l)
                   (q fun1)
                   (q (l fun1))
                   (c (q l) args))))





(def-add 'count 
         '(! gate ((~))
             (? arg2 
                (? (arg1 (< arg2)) 
                   ((! rec-loc (inc loc1)) arg1
                                             (> arg2)) 
                   ((! rec-loc loc1) arg1 (> arg2)))
                loc1)))

(def-add 'append
         '(! gate ()
             (? arg1 
                (c (< arg1) 
                   (rec1 (> arg1) arg2))
                arg2)))
(pock '(T T T) 
      '(append ($) ($)))



(def-add 'let 
         '(! door 
             (c (l (q l)
                   (l (q q) arg2)
                   (l (q q) (l arg2)) 
                   (l (q q) (l))
                   (q ($)))
                arg1)))

(pock '(T T T) 
      '(let (q ((> ($)) ($)))
         (q (h arg1))) 
      'code)


(pock '(T T T)
      '(! let ((q T) (q T))
          ($)))

(pock '(T T T) 
      '(! let ((add (q (T T)) (q (T T))) (q (T T)))
        (add arg1 arg2)))

(pock '(T ~ ~ T T ~ T T) 
      '(count (! gate () (? arg1 (q T) (q ~))) ($))
      'uint)

(def-add 'dec
         '(! gate ()
             (? arg1 
                (? (< arg1) 
                   (c (q ~)
                      (> arg1)) 
                   (! let ((rec1 (> arg1))) 
                          (? arg1
                         (c (q T)
                            arg1)
                         (q ~))))
                (q ~))))


(def-add 'dec
         '(! gate ()
             (? arg1 
                (? (< arg1) 
                   (? (> arg1)
                      (c (q ~)
                         (> arg1))
                      (q ~))
                   (c (q T)
                      (rec1 (> arg1))))
                (q ~))))
(pock '9 
      '(dec (dec (dec (dec ($))))) 
      'uint)



(def-add 'subst '(? (= )))



(def-add 'qq 
         '(! gate () 
             (? arg1 
                (? (= (< arg1) (q uq))
                   (< (> arg1))
                   (? (= (< (< arg1)) (q uqs)) 
                      (l (q append)
                         (< (> (< arg1))) 
                         (rec1 (> arg1)))
                      (l (q c)
                         (rec1 (< arg1)) 
                         (rec1 (> arg1))))) 
                (q (q ~)))))

(pock '(T T T)
      '(! qq (~ ((uqs ($))) ~))
      )))

(defun gen-def (def)
  (let ((name (cadr def)) 
        (value (caddr def)))
    (format nil "def_add(\"~(~a~)\", rread(\"~(~a~)\"));"
            (cadr name)
            (cadr value))))

(defun convert-defs (code)
  (let ((defs (remove-if (lambda (x) 
                           (not (eq (car x) 'def-add)))
                         code))) 
    (format nil "~{~a~^~%~}"
            (mapcar #'gen-def defs))))

(convert-defs *code*)



(with-open-file (file "dump" 
                      :direction :output 
                      :if-exists :supersede
                      :if-does-not-exist :create) 
  (format file "~a" (convert-defs *code*)))
