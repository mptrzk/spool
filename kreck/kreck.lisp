(defun my-pairlis (keys vals alist)
  (if keys
      (my-pairlis (cdr keys)
                  (cdr vals)
                  (acons (car keys)
                         (car vals)
                         alist))
      alist))

