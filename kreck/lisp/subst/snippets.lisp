
;pretty-printing
(defun my-format (stm str &rest args)
  (let ((res (apply #'format nil str args)))
    (format stm "~a" res)
    (length res)))
(my-format t "~a" '(1 2 3))



(defun pretty-print-list (expr horz indent)
  (let ((indent-res (pretty-print (car expr)
                               indent)))
    (if (consp (cdr expr))
        (if (> horz 0)
            (progn
              (format t " ")
              (pretty-print-list (cdr expr)
                                 (- horz 1)
                                 (+ indent-res 1)))
            (progn
              (format t "~%")
              (dotimes (_ indent) (format t " "))
              (pretty-print-list (cdr expr)
                                 0
                                 indent)))
        (if (cdr expr)
            ($$-> indent-res
              (+ $$ (my-format t " . "))
              (pretty-print (cdr expr) $$))
            indent-res))))


(defun pretty-print (expr &optional (indent 0))
  (if (atom expr)
      (+ indent (my-format t "~s" expr))
      ($$-> indent
        (+ $$ (my-format t "(")) 
        (pretty-print-list expr 1 $$) ;version with empty lists?
        (+ $$ (my-format t ")")))))

;(pretty-print '((1 2 3) 3 4))
;(pretty-print '(1 . 1))
(pretty-print (kreck '() '(fex-gate dump (l arg1 (*^ arg1)))))



;rplc
(defun rplc (path val)
  (let ((op (car path))
        (arg (cadr path)))
    (cond ((equal op '$) val)
          (t (rplc arg
                   (cond ((eq op '<)
                          (list 'c val (list '> arg)))
                         ((eq op '>)
                          (list 'c (list '< arg) val))
                         (t (error "foo"))))))))

(kreck '(1 2 3) (rplc '(< ($)) 777))

;incoherent list
("l" . ,(parse '(c (q (? args
                         (c (^* arg1)
                            (^* (c (q l) (> args))))))
                   non-rec-fexpr-ctx)))
