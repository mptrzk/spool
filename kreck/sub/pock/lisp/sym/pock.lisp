
; (qq (a (b c) (uq d))) => (list 'a (list 'b 'c) d)
; (qq (a b . (uq c) => (cons 'a (cons 'b c))
; nil => nil?






#|
(defun qq-f (tree) 
  (if tree
      (if (consp tree) 
          (if (eq (car tree) 'uq) 
              (cadr tree) 
              (if (and (consp (car tree)) 
                       (eq (caar tree) 'uqs))
                  (append (cadar tree)
                          (qq-f (cdr tree)))
                  (cons (qq-f (car tree))
                        (qq-f (cdr tree))))) 
          (list 'quote tree))))
(defmacro qq (tree) 
  ())
|#

(defun append-1 (l1 l2) 
  (if l1 
      (cons (car l1) 
            (append-1 (cdr l1) l2)) 
      l2))

(append-1 '(1 2 3) '(4 5 6))

;how could it be made faster?
;the evil eval?
;  naa, env problem
; reverse list and cons recursively
(defun list-1-f (args) 
  (if args
      (cons 'cons
            (cons (car args)
                  (cons (list-1-f (cdr args))
                        nil)))
      nil))

(defun list-1-f (args) 
  (if args
      (cons 'cons
            (cons (car args)
                  (cons (list-1-f (cdr args))
                        nil)))
      nil))

(defmacro list-1 (&rest args) 
  (list-1-f args))

(defun reverse-rec (lst res) 
  (if lst 
      (reverse-rec (cdr lst) 
                   (cons (car lst)
                         res))
      res))

(defun reverse-1 (lst) 
  (reverse-rec lst nil))
(reverse-1 '(1 (2 3) 4))

;(defun list-2-f (lst)
;  ()) ;nonsense

(defun qq-f (tree) 
  (if tree
      (if (consp tree) 
          (if (eq (car tree) 'uq) 
              (cadr tree) 
              (if (and (consp (car tree)) 
                       (eq (caar tree) 'uqs))
                  (list 'append (cadar tree)
                          (qq-f (cdr tree)))
                  (list 'cons (qq-f (car tree))
                        (qq-f (cdr tree))))) 
          (list 'quote tree))))


(defmacro qq (tree) 
  (qq-f tree))

(qq (1 (uqs (list (+ 1 4) 2)) 3))

(qq-f '(1 ((foo) (uqs (+ 2 3))) 3))



(defun qq-f (tree) 
  (if tree
      (if (consp tree) 
          (cond ((eq (car tree) 'uq) 
                 (cadr tree)) 
                (t (cons (qq-f (car tree))
                         (qq-f (cdr tree))))) 
          (list 'quote tree))))
(qq-f '(1 ((uq foo) (uq (+ 2 3))) 3))
(qq-f '(uq (list 1 2 3)))


`(1 ,@1)
(qq-f '(1 (uq 1)))
(qq-f '(uqs (list 1 2 3))) ;TODO thinq why it doesn't quite work

; TODO compare with scheme's qq macros




(defun m-a-s (expr testp fun) 
  (if (funcall testp expr) 
      (funcall fun expr) 
      (mapcar (lambda (ex)
                (m-a-s ex testp fun)) 
              expr)))

(m-a-s '(a (b c) d) 
       #'symbolp 
       (lambda (sym) `(1 1)))

(defmacro dump (&rest exps) 
  `(progn ,@(mapcar (lambda (ex) 
                      `(format t "~a => ~a~%" ',ex ,ex)) 
                    exps)))
(let ((subj 2)) (dump (+ 1 subj)))

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



(defun make-noun (noun) ;TODO proof that it's idempotent
  (if (listp noun) 
      (mapcar #'make-noun noun)
      (def-get noun)))

(defun noun-equal (a b) 
  (equal (make-noun a) (make-noun b)))

(defun lus (noun) 
  (cond ((equal noun (def-get '~)) 
         (make-noun '(T)))
        ((equal (car noun) (def-get '~)) 
         (cons (def-get 'T)
               (cdr noun)))
        ((equal (car noun) (def-get 'T)) 
         (cons (def-get '~)
               (lus (cdr noun))))))

(def-add '~ '())
(def-add 'T '(~))
(noun-equal 'T '(~))
(letfoo ((symlist '($ \' < > \: * ? @ l))
         (ctr (make-noun '(~)))) 
  (if (null symlist) 
      t 
      (progn (def-add (car symlist) ctr) 
             (recur (cdr symlist) (lus ctr)))))

(defmacro case-noun (expr &rest clauses) 
  (let ((val-sym (gensym "val"))) 
    `(let ((,val-sym ,expr)) 
       (cond ,@(mapcar (lambda (c) 
                         (if (eq (car c) 'else)
                             `(T ,(cadr c))
                             `((noun-equal ,val-sym ,(car c)) 
                               ,(cadr c)))) 
                       clauses)))))
;TODO :debug keyword
;"meta" opcode
;noun-gensym?
; error
;memoizing opcodes



(defun pock-eval (subj form) 
  (mbind (op arg1 arg2 arg3)
         (values-list form)
    (case-noun op 
      ('$ subj) 
      ('\' arg1) 
      ('< (car (pock-eval subj arg1)))
      ('> (cdr (pock-eval subj arg1))) 
      ('\: (cons (pock-eval subj arg1) 
                 (pock-eval subj arg2)))
      ('* (pock-eval (pock-eval subj arg1) 
                     (pock-eval subj arg2)))
      ('? (if (pock-eval subj arg1) 
              (pock-eval subj arg2)
              (pock-eval subj arg3))) 
      ;iffy stuff
      ('@ (let ((code (pock-eval subj arg1))
                (data (pock-eval subj arg2))) 
            (pock-eval (list data code) code))) 
      ('l (mapcar (lambda (arg)
                          (pock-eval subj arg)) 
                        (cdr form))) 
      ('h (pock-eval subj arg2))
      (else (format t "invalid opcode - ~a~%" op)))))
;symbols, quotes, macros
(defun pock (subj form) 
  (pock-eval (make-noun subj) (make-noun form)))



(def-add 'q '\')
(def-add 'c '\:)
(def-add 'core-code '(< (> ($))))
(def-add 'core-data '(< ($)))

(def-add 'load-core 
         '(* ($)
             core-code))


(defun assoc-err (item alist) 
  (if (null alist)
      (progn (format t "~a not found in the def list~%" item)
             (cons item 'err)) ;change to dump-error or an exception or something
      (if (equal item (caar alist)) 
          (car alist) 
          (assoc-err item (cdr alist)))))

(defun aura-def (noun symlist) ;error handling !! (let* n' stuff)
  (let ((deflist (mapcar (lambda (sym) 
                       (cons (def-get sym) sym)) 
                     symlist))) 
    (cdr (assoc-err noun deflist)))) ;assoc with error


;TODO naming kerfuffle
(defun aura-list (noun auras) 
  (if noun 
      (cons (aura-apply (car noun) (car auras)) 
            (aura-list (cdr noun) 
                       (if (cdr auras)
                           (cdr auras)
                           auras)))
      nil))


(defun aura-apply (noun aura) 
  (let ((aura-expr (if (symbolp aura) 
                       (aura-get aura) 
                       aura))) 
    (case (car aura-expr) 
      (def (aura-def noun (cdr aura-expr))) 
      (list (aura-list noun (cdr aura-expr))))))

(aura-add 'opcode '(def $ q < > c * ?))
(aura-add 'bool '(def T ~))
(aura-add 'code '(list opcode code))
(aura-def (make-noun '(~)) '(? $))
(aura-apply (make-noun '?) '(def ? $))
(aura-apply (make-noun '(~ (~) ~)) '(list (def ~ T)))
(aura-apply (make-noun '((~) (~) ~)) '(list opcode (def ~ T)))
(aura-apply (make-noun '(c (> ($)) ($))) 'code)
(aura-list (make-noun '(T * T)) '(opcode))

(def-add 'iden
         '(? (< core-data) 
             (? (< (> core-data)) 
                (? (* (c (c (< (< core-data)) 
                            (c (< (< (> core-data)))
                               (q ~)))
                         (c core-code
                            (q ~)))
                      core-code)
                   (? (* (c (c (> (< core-data)) 
                               (c (> (< (> core-data))) 
                                  (q ~)))
                            (c core-code 
                               (q ~)))
                         core-code)
                      (q T) 
                      (q ~))
                   (q ~))
                (q ~))
             (? (< (> core-data))
                (q ~) 
                (q T))))

(def-add 'iden
         '(? (< core-data) 
             (? (< (> core-data)) 
                (? (@ core-code
                      (l (< (< core-data))
                         (< (< (> core-data)))))
                   (? (@ core-code 
                         (l (> (< core-data)) 
                            (> (< (> core-data)))))
                      (q T) 
                      (q ~))
                   (q ~))
                (q ~))
             (? (< (> core-data))
                (q ~) 
                (q T))))



(pock '(((? (*)) (? (*)))
        iden)
      'load-core)


(pock '((~ T (T T)) (< (> core-data)))
      'load-core) 

            
;bootstrapping with external macros and quasiquote?

; opcode table as global?
; using that pattern matching library?
; writing my own patttern matching macros?

;each symbol-aura defines a bijection between inner and
;outer symbols



#|
(defun code-validate (noun) 
  (let ((op (car noun))) 
    (cond (and (= (length noun) 1) 
               (equal op )))));todo finish
|#


