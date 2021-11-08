(defmacro let1 (var val &body body)
  `(let ((,var ,val)) ,@body))

(defun add (a b)
  (let1 x (+ a b)
    (format t "The sum is ~a" x) x))

(defmacro split (val yes no)
  (let1 g (gensym) ;マクロ展開時に実行される
    `(let1 ,g ,val
      (if ,g
        (let 
          ((head (car ,g))
           (tail (cdr ,g)))
          ,yes)
        ,no))))

(defun pairs (lst)
  (labels 
    ((f (lst acc)
      (split lst
        (if tail
          (f (cdr tail) (cons (cons head (car tail)) acc))
          (reverse acc))
        (reverse acc))))
    (f lst nil)))

(defmacro recurse (vars &body body)
  (let1 p (pairs vars)
    `(labels
      ((self ,(mapcar #'car p) ,@body)) 
      (self ,@(mapcar #'cdr p)))))

(defun my-length (lst)
  (recurse (lst lst acc 0)
    (split lst
      (self tail (1+ acc))
      acc)))
