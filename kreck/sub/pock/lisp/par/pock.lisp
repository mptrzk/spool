(ql:quickload "cl-interpol")
(cl-interpol:enable-interpol-syntax)





(defparameter *defs* (make-hash-table :test #'equal))



(defmacro λ (args expr) 
  `(lambda ,args ,expr))


(defmacro lit-coerce (a ty)
  `'(,@(coerce a ty)))


(defmacro letfoo (bindings body) 
  (let ((loopname (gensym "loop")) 
        (names (mapcar #'car bindings)) 
        (inits (mapcar #'cadr bindings))) 
    `(labels ((,loopname ,names
                ,(subst loopname 'recur body))) 
       (,loopname ,@inits))))

(defmacro meval (expr) 
  `',(eval expr))


(defun funcall-times (fn n arg) 
  (if (> n 0) 
      (funcall-times  fn (- n 1) (funcall fn arg))
      arg))


(defun dump (x)
  (format t "~s~%" x)
  x)


(defun format-noun (s a)
  (if (atom a) 
      (format s "~a" "~")
      (format s "(~{~a~^ ~})"
              (mapcar (λ (a) 
                         (format-noun nil a)) 
                      a))))


(defun whitep (c)
  (member c
          '(#\tab #\space #\return #\newline)
          :test #'equal))


(defun def-add (key def) 
  (setf (gethash key *defs*) def))


(defun init-defs () ;think about error handling for this
  (def-add "~" (read-noun "()"))
  (def-add "T" (read-noun "(~ ~)"))
  (def-add "T" (read-noun "(~ ~)"))
  (letfoo ((opcodes (mapcar #'string
                            (coerce "$'<>:*?!" 'list)))
           (ctr (read-noun "(~)")))
    (if opcodes 
        (progn (def-add (car opcodes) ctr) 
          (recur (cdr opcodes) (lus ctr)))
        t))
  t)



(defun get-def (key) ;gethash but with error handling
  (multiple-value-bind (val pres) 
    (gethash key *defs*) 
    (if pres 
        val 
        (progn (format t "wrong key:~a~%" key) 
               'err))))

(defun special-char-p (c) 
  (if (member c (lit-coerce "~!@#$%^&*_+=;:'\\|,<.>/?" list)) 
      t 
      nil))

;(defun keyword-start-char-p (c) 
;  (or (alpha-char-p c) (special-char-p c)))
;separate condition for 1 char keywords?

(defun parse-keyword (str)
  (letfoo ((i 1)) 
    (let ((sub (subseq str i))) 
      (if (or (equal sub "") 
              (not (alphanumericp (char sub 0)))) 
          `(,(get-def (subseq str 0 i)) ; why not (- i 1)?
             ,(subseq str i)) 
          (recur (+ i 1))))))


(defun parse-list (str) 
  (let ((hs (char str 0)) 
        (ts (subseq str 1))) 
    (cond ((whitep hs) (parse-list ts)) 
          ((equal hs #\)) `(nil ,ts)) 
          (t (let* ((hpr (parse-noun str)) 
                    (hl (car hpr)) 
                    (tpr (parse-list (cadr hpr))) 
                    (tl (car tpr))
                    (tts (cadr tpr))) 
               `((,hl . ,tl) ,tts))))))

(defun parse-noun (str) 
  (let ((hs (char str 0)) 
        (ts (subseq str 1))) 
    (cond  
      ((equal hs #\() (parse-list ts)) 
      ((special-char-p hs)
       `(,(get-def (string hs)) ,ts))
      ((alpha-char-p hs) (parse-keyword str))
      (t (format t "invalid expression: ~a" str)))))


(defun read-noun (str) 
  (car (parse-noun str)));;TODO junk cond


(defun load-src (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents))) ;TODO understand

(defun lus (noun) 
  (cond ((equal noun nil)
         '((nil nil)))
        ((equal (car noun) nil) 
         (cons '(nil nil) (cdr noun)))
        ((equal (car noun) '(nil nil)) 
         (cons nil (lus (cdr noun)))))) 

(defun pock-eval (subj form) 
  (let ((op (car form))
        (args (cdr form))) 
    (cond ((equal op (read-noun "$"))
           subj)
          ((equal op (read-noun "'"))
           (car args))
          ((equal op (read-noun "<"))
           (car (pock-eval subj (car args))))
          ((equal op (read-noun ">"))
           (cdr (pock-eval subj (car args))))
          ((equal op (read-noun ":"))
           (cons (pock-eval subj (car args))
                 (pock-eval subj (cadr args))))
          ((equal op (read-noun "*"))
           (pock-eval (pock-eval subj (car args))
                 (pock-eval subj (cadr args))))
          ((equal op (read-noun "?")) 
           (if (not (null (pock-eval subj (car args)))) 
               (pock-eval subj (cadr args))
               (pock-eval subj (caddr args)))))))

;(defun pock (noun) 
;  (let ((subj  (car noun))
;        (form  (cadr noun))
;        (op  (caadr noun))
;        (args  (cdadr noun))) 
;    (cond ((equal op (read-noun "$"))
;           subj)
;          ((equal op (read-noun "'"))
;           (car args))
;          ((equal op (read-noun "<"))
;           (car (pock `(,subj ,(car args)))))
;          ((equal op (read-noun ">"))
;           (cdr (pock `(,subj ,(car args)))))
;          ((equal op (read-noun ":"))
;           (cons (pock `(,subj ,(car args)))
;                 (pock `(,subj ,(cadr args)))))
;          ((equal op (read-noun "*"))
;           (pock `(,(pock `(,subj ,(car args)))
;                  ,(pock `(,subj ,(cadr args))))))
;          ((equal op (read-noun "?")) subj))))
(defun pock-eval-1 (noun) (pock-eval (car noun) (cadr noun)))

(defun check-pock (src ref) 
  (let ((res (pock-eval-1 (read-noun src))) 
        (refval (read-noun ref))) 
    (if (equal res refval) 
        t 
        (format t "pock(~a) = ~a != ~a"
                src 
                (format-noun nil res)
                ref))))

(init-defs)

(and (check-pock "((T T T) ($))" "(T T T)")
     (check-pock "((T T T) (' ($)))" "($)") 
     (check-pock "((T T T) (< ($)))" "T") 
     (check-pock "((T T T) (> ($)))" "(T T)") 
     (check-pock "((T T T) (: ('($)) (> ($))))" "(($) T T)") ;test '?'
     (check-pock "((T T T) (? ($) (< ($)) (> ($))))" "T")
     (check-pock "((T T T) (? (' ~) (< ($)) (> ($))))" "(T T)")
     (check-pock "(((T (T T) T) (< (> ($))))
                  (* (< ($))
                     (< (> ($)))))" 
                 "(T T)"))

(defun noun-expand (noun) 
  (if (symbolp noun) 
      (get-def (string-downcase noun))
      (mapcar #'noun-expand noun)))

(defmacro pock (subj form) 
  `(pock-eval ',(noun-expand subj)
         ',(noun-expand form)))


(def-add  "run-core" (read-noun "(* (< ($)) 
                                    (< (> ($))))")) ;symbols FTW?
(def-add "t" (get-def "T"))
(pock ((T T T) (< ($))) 
          run-core)
;dafuq is symbol interning?



;(lit-coerce #?"abc" list)
;(macroexpand '(meval (read-noun "(T T)")))


;(letfoo ((s 1) 
;         (n 6)) 
;  (if (<= n 1) 
;      s
;      (recur (* s n) (- n 1))))

;(macroexpand '(letfoo ((s 1) 
;         (n 6)) 
;  (if (<= n 1) 
;      s
;      (recur (* s n) (- n 1)))))


;funcall-times?
;why ho-fun, not macro
;  generating code with repeated fn application is dumb
;  could generate macro with let n' stuff, but it would also be dumb
;    TODO check if i've done that somewhere by accident

;macro - letdump

;declarations

;pock -> pock-eval
;pock as an evlauatin macro
; what with upper case symbols?
